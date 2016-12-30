/*
 * Leto db server functions
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 *
 * modification and additions 2015-16 Rolf 'elch' Beckmann
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "srvleto.h"

#if defined( USE_LZ4 )
   #include "lz4.h"
#endif

#define PARSE_MAXDEEP            5   /* used in leto_ParseFilter() */
#define SHIFT_FOR_LEN            3

typedef void ( *CMDSET )( PUSERSTRU, const char * );
static CMDSET s_cmdSet[ LETOCMD_SETLEN ] = { NULL };

static const char * szOk = "++++";
static const char * szErr1 = "-001";
static const char * szErr2 = "-002";
static const char * szErr3 = "-003";
static const char * szErr4 = "-004";
static const char * szErr101 = "-101";
static const char * szErrAcc = "-ACC";
static const char * szErrLck = "-LCK";

static char        s_szDirBase[ HB_PATH_MAX ] = "";  /* log files path, defaults to location executable */
static DATABASE *  s_pDB = NULL;
static AVAILAREAID s_AvailIDS = { NULL, 0, 0, 0 };

static PUSERSTRU  s_users = NULL;         /* the users array, one entry for each thread2/ threadx */
static HB_USHORT  s_uiUsersAlloc = 99;    /* Number of allocated user structures */
static HB_USHORT  s_uiUsersMax = 0;       /* Higher index of user structure, which was busy -- now static */
static HB_USHORT  s_uiUsersFree = 0;      /* first free connection structure */
static HB_USHORT  s_uiUsersCurr = 0;      /* Current number of users  -- now static */
static PTABLESTRU s_tables = NULL;        /* now static */
static HB_UINT    s_uiTablesAlloc = 999;  /* Number of allocated table structures */
static HB_UINT    s_uiTablesMax = 0;      /* Highest index of table structure, which was busy */
static HB_UINT    s_uiTablesCurr = 0;     /* Current number of opened tables */
static HB_UINT    s_uiTablesFree = 0;     /* first free table structure */
static PGLOBESTRU s_globes = NULL;        /* global values for s_tables, n to 1 relation for s_bNoSaveWA */
static HB_UINT    s_uiGlobesCurr = 0;     /* used with s_bNoSaveWA: current number of active globes */
static HB_UINT    s_uiIndexMax = 0;       /* Higher index of index structure, which was busy */
static HB_UINT    s_uiIndexCurr = 0;      /* Current number of opened indexes */
static HB_ULONG   s_ulTimeOut = HB_THREAD_INFINITE_WAIT;
static HB_ULONG   s_ulStartDateSec;       /* server startup time */
static HB_ULONG   s_ulTransAll = 0;       /* executed LetoDBf TRANSACTIONs */
static HB_ULONG   s_ulTransOK = 0;        /* valid LetoDBf TRANSACTIONs of above */

static PHB_ITEM * s_pThxArray = NULL;
static HB_COND_NEW( s_ThxCond );
static HB_CRITICAL_NEW( s_ThxMtx );

/* ToDo: make a static struct over all one time set config settings, instead each as single static */
static int       s_iDebugMode = 1;
static char *    s_szServerAddr = NULL;
static int       s_iServerPort = 2812;
static char *    s_pDataPath = NULL;
static HB_USHORT s_uiDataPathLen = 0;
static HB_USHORT s_uiDriverDef = 0;
static HB_BOOL   s_bShareTables = HB_FALSE;
static HB_BOOL   s_bNoSaveWA = HB_FALSE;
static HB_BOOL   s_bFileFunc = HB_FALSE;
static HB_BOOL   s_bAnyExt = HB_FALSE;
static HB_USHORT s_uiLockExtended = HB_FALSE;  /* default versus extended mode ( DBFLOCK_CLIPPER2, DBFLOCK_HB32 ) */
static HB_BOOL   s_bUdfEnabled = HB_FALSE;
static HB_USHORT s_uiCacheRecords = 10;
static HB_BOOL   s_bOptimize = HB_TRUE;
static HB_BOOL   s_bForceOpt = HB_FALSE;
static int       s_iAutOrder = 0;
static HB_USHORT s_uiMemoType = 0;
static HB_USHORT s_uiMemoBlocksize = 0;
static int       s_iUserLock = -1;
static HB_BOOL   s_bLowerPath = HB_FALSE;
static HB_BOOL   s_bSetTrigger = HB_FALSE;
static char *    s_pTrigger = NULL;
static char *    s_pPendTrigger = NULL;
static HB_BOOL   s_bHardCommit = HB_FALSE;
static HB_BOOL   s_bPass4L = HB_FALSE;        //  Pass needs: Login,Manage,Datamodify
static HB_BOOL   s_bPass4M = HB_FALSE;
static HB_BOOL   s_bPass4D = HB_FALSE;


/* LOG files quick mutex -- also used by s_pDB */
#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   static HB_SPINLOCK_T s_fileMtx = HB_SPINLOCK_INIT;
   #define HB_GC_LOCKF()    HB_SPINLOCK_ACQUIRE( &s_fileMtx )
   #define HB_GC_UNLOCKF()  HB_SPINLOCK_RELEASE( &s_fileMtx )
#else
   static HB_CRITICAL_NEW( s_fileMtx );
   #define HB_GC_LOCKF()    hb_threadEnterCriticalSection( &s_fileMtx )
   #define HB_GC_UNLOCKF()  hb_threadLeaveCriticalSection( &s_fileMtx )
#endif

/* SKIP/ seek mutex only used in mode s_bNoSaveWA for specific WA */
#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   #define HB_GC_LOCKA()    HB_SPINLOCK_ACQUIRE( &pAStru->pTStru->pGlobe->pMutex )
   #define HB_GC_UNLOCKA()  HB_SPINLOCK_RELEASE( &pAStru->pTStru->pGlobe->pMutex )
#else
   #define HB_GC_LOCKA()    hb_threadEnterCriticalSection( &pAStru->pTStru->pGlobe->pMutex )
   #define HB_GC_UNLOCKA()  hb_threadLeaveCriticalSection( &pAStru->pTStru->pGlobe->pMutex )
#endif

/* transaction statistic quick mutex */
#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   static HB_SPINLOCK_T s_TransMtx = HB_SPINLOCK_INIT;
   #define HB_GC_LOCKTRAN()    HB_SPINLOCK_ACQUIRE( &s_TransMtx )
   #define HB_GC_UNLOCKTRAN()  HB_SPINLOCK_RELEASE( &s_TransMtx )
#else
   static HB_CRITICAL_NEW( s_TransMtx );
   #define HB_GC_LOCKTRAN()    hb_threadEnterCriticalSection( &s_TransMtx )
   #define HB_GC_UNLOCKTRAN()  hb_threadLeaveCriticalSection( &s_TransMtx )
#endif

/* table struct: no spinlock mutex as it may last longer */
static HB_CRITICAL_NEW( s_TStruMtx );    /* also used for s_uiIndexCurr */
#define HB_GC_LOCKT()       hb_threadEnterCriticalSection( &s_TStruMtx )
#define HB_GC_UNLOCKT()     hb_threadLeaveCriticalSection( &s_TStruMtx )

/* user struct quick mutex */
#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   static HB_SPINLOCK_T s_UStruMtx = HB_SPINLOCK_INIT;
   #define HB_GC_LOCKU()    HB_SPINLOCK_ACQUIRE( &s_UStruMtx )
   #define HB_GC_UNLOCKU()  HB_SPINLOCK_RELEASE( &s_UStruMtx )
#else
   static HB_CRITICAL_NEW( s_UStruMtx );    /* also used for s_bLockLock */
   #define HB_GC_LOCKU()       hb_threadEnterCriticalSection( &s_UStruMtx )
   #define HB_GC_UNLOCKU()     hb_threadLeaveCriticalSection( &s_UStruMtx )
#endif

// #define LETO_SAVEMODE ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO )

/* two helper functions for statics from leto_2.c */
extern HB_U64 leto_Statistics( int iEntry );
extern int leto_ExitGlobal( HB_BOOL fExit );
extern void leto_SrvShutDown( unsigned int uiWait );

extern char * leto_memoread( const char * szFilename, HB_ULONG * pulLen );
extern HB_BOOL leto_fileread( const char * szFilename, char * pBuffer, const HB_ULONG ulStart, HB_ULONG * ulLen );
extern HB_BOOL leto_filewrite( const char * szFilename, const char * pBuffer, const HB_ULONG ulStart, HB_ULONG ulLen );
extern HB_BOOL leto_memowrite( const char * szFilename, const char * pBuffer, HB_ULONG ulLen );

extern void leto_acc_release( void );
extern HB_BOOL leto_acc_find( const char * szUser, const char * szPass, char ** pAcc );
extern void leto_acc_setPath( const char * szPath );
extern void leto_ToggleZip( PUSERSTRU pUStru, const char * szData );

extern void leto_SendError( PUSERSTRU pUStru, const char * szData, HB_ULONG ulLen );
extern void leto_SendAnswer( PUSERSTRU pUStru, const char * szData, HB_ULONG ulLen );
extern void leto_SendAnswer2( PUSERSTRU pUStru, const char * szData, HB_ULONG ulLen, HB_BOOL bAllFine, int iError );
extern HB_BOOL leto_AskAnswer( HB_SOCKET hSocket );
extern void leto_Admin( PUSERSTRU pUStru, const char * szData );

extern void leto_Variables( PUSERSTRU pUStru, const char * szData );
extern void leto_setVarsMax( HB_ULONG ulMaxVars, HB_USHORT uiMaxVarLen );
extern void leto_varsown_release( PUSERSTRU pUStru );
extern void leto_vars_release( void );

extern void letoListInit( PLETO_LIST pList, HB_ULONG ulSize );
extern void letoListFree( PLETO_LIST pList );
extern HB_BOOL letoListEmptyTS( PLETO_LIST pList );
extern void * letoAddToList( PLETO_LIST pList );
extern void * letoGetListItem( PLETO_LIST pList, HB_USHORT ulNum );
extern void letoDelFromList( PLETO_LIST pList, HB_USHORT ulNum );
extern void letoDelItemList( PLETO_LIST pList, PLETO_LIST_ITEM pDelItem );
extern HB_BOOL letoIsRecInList( PLETO_LIST pLockList, HB_ULONG ulRecNo );
extern HB_BOOL letoIsRecInListTS( PLETO_LIST pList, HB_ULONG ulRecNo );
extern HB_BOOL letoAddRecToListTS( PLETO_LIST pList, HB_ULONG ulRecNo, HB_BOOL bAdd );
extern HB_BOOL letoAddRecToList( PLETO_LIST pLockList, HB_ULONG ulRecNo, HB_BOOL bAdd );
extern HB_BOOL letoDelRecFromList( PLETO_LIST pLockList, HB_ULONG ulRecNo );
extern HB_BOOL letoDelRecFromListTS( PLETO_LIST pList, HB_ULONG ulRecNo );
extern void letoClearList( PLETO_LIST pList );
extern void letoListLock( PLETO_LIST pList );
extern void letoListUnlock( PLETO_LIST pList );

#if defined( __HARBOUR30__ )
extern HB_BOOL leto_filesize( const char * szFilename, HB_ULONG * pulLen );
#endif

int iDebugMode( void )
{
   return s_iDebugMode;
}

void leto_setTimeout( HB_ULONG ulTimeOut )
{
   s_ulTimeOut = ulTimeOut;
}

/* for thread local pUStru TLS for leto_udf() */
static HB_TSD_NEW( s_TSDustru, sizeof( PUSERSTRU * ), NULL, NULL );

static PUSERSTRU letoGetUStru( void )
{
   return *( PUSERSTRU * ) hb_stackGetTSD( &s_TSDustru );
}

/* export function, maybe make above non-static */
PUSERSTRU letoGetsUStru( void )
{
   return letoGetUStru();
}

static PUSERSTRU letoSetUStru( PUSERSTRU pUStru )
{
   PUSERSTRU * pUStruTSD = hb_stackGetTSD( &s_TSDustru );

   *pUStruTSD = pUStru;
   return *( pUStruTSD );
}

/* leto_udf() */
HB_FUNC( LETO_GETUSTRUID )
{
   PUSERSTRU pUStruTSD = letoGetUStru();

   if( pUStruTSD )
      hb_retni( pUStruTSD->iUserStru );
   else
      hb_retni( -1 );
}

/* leto_udf() */
HB_FUNC( LETO_UDFMUSTQUIT )
{
   PUSERSTRU pUStruTSD = letoGetUStru();

   if( pUStruTSD )
      hb_retl( pUStruTSD->bCloseConnection );
   else
      hb_retl( HB_FALSE );
}

const char * leto_sDirBase( void )
{
   return ( const char * ) s_szDirBase;
}

/* only called after err_internal, no MT lock */
void s_TablesFree( void )
{
   hb_xfree( s_tables );
   s_tables = NULL;
}

HB_USHORT leto_ActiveUser( void )
{
   HB_USHORT uRet;

   HB_GC_LOCKU();
   uRet = s_uiUsersCurr;
   HB_GC_UNLOCKU();

   return uRet;
}

HB_USHORT leto_MaxUsers( void )
{
   return s_uiUsersAlloc;
}

HB_BOOL leto_CheckPass( int iType )
{
   if( iType == 1 )
      return ( s_bPass4L || s_bPass4M || s_bPass4D );
   else if( iType == 2 )
      return ( ! s_bPass4L && ! s_bPass4M && ! s_bPass4D );
   else
      return HB_FALSE;
}

void leto_writelog( const char * sFile, int n, const char * s, ... )
{
   HB_FHANDLE handle;
   char       sFileDef[ HB_PATH_MAX ];
   int        iMsgLen;

   if( n != 0 )
      iMsgLen = n;
   else
      iMsgLen = ( unsigned int ) strlen( s );

   HB_GC_LOCKF();

   if( ! sFile )
   {
      strcpy( sFileDef, s_szDirBase );
      strcpy( sFileDef + strlen( s_szDirBase ), "letodbf.log" );
   }

   if( iMsgLen > 0 )
   {
      if( iMsgLen > 1 && s[ iMsgLen - 2 ] == '\r' && s[ iMsgLen - 1 ] == '\n' )
         iMsgLen -= 2;
      else if( s[ iMsgLen - 1 ] == '\n' )
         iMsgLen -= 1;
   }

   if( hb_fsFile( ( sFile ) ? sFile : sFileDef ) )
      handle = hb_fsOpen( ( sFile ) ? sFile : sFileDef, FO_WRITE | FO_EXCLUSIVE );
   else
      handle = hb_fsCreate( ( sFile ) ? sFile : sFileDef, 0 );

   if( handle != FS_ERROR )
   {
      int  iYear, iMonth, iDay;
      char szDateTime[ 21 ];
      char szTime[ 9 ];

      hb_dateToday( &iYear, &iMonth, &iDay );
      hb_dateTimeStr( szTime );
      sprintf( szDateTime, "%02d.%02d.%04d %s ", iMonth, iDay, iYear, szTime );

      hb_fsSeek( handle, 0, SEEK_END );
      hb_fsWrite( handle, szDateTime, 20 );
      if( iMsgLen >= 0 )
         hb_fsWrite( handle, s, ( HB_USHORT ) iMsgLen );
      else
      {
         char    message[ 1024 ];
         va_list ap;

         va_start( ap, s );
         hb_vsnprintf( message, sizeof( message ) - 1, s, ap );
         message[ 1023 ] = '\0';
         va_end( ap );
         hb_fsWrite( handle, message, ( HB_USHORT ) strlen( message ) );
      }

#if defined( HB_OS_UNIX ) && ! defined( HB_EOL_CRLF )
      hb_fsWrite( handle, "\n", 1 );
#else
      hb_fsWrite( handle, "\r\n", 2 );
#endif
      hb_fsClose( handle );
   }

   HB_GC_UNLOCKF();
}

/* determine needed strlen for numeric value, to avoid mathematic (int) log10() + 1 */
static HB_UINT leto_log10Len( HB_U64 ulValue )
{
   char buf[ 21 ];

   return( ultostr( ulValue, buf ) );
}

char * leto_readServerLog( int iUser, unsigned int nRows, HB_SIZE * nLen )
{
   HB_FHANDLE handle;
   char       sFileDef[ HB_PATH_MAX ];
   char *     szContent = ( char * ) hb_xgrab( 2 );

   if( iUser < 0 )  /* main server log file */
   {
      strcpy( sFileDef, s_szDirBase );
      strcpy( sFileDef + strlen( s_szDirBase ), "letodbf.log" );
   }
   else
   {
      strcpy( sFileDef, s_szDirBase );
      hb_snprintf( sFileDef + strlen( s_szDirBase ), HB_PATH_MAX - 1 - strlen( s_szDirBase ),
                   "letodbf_%0*d.log", leto_log10Len( s_uiUsersAlloc ), iUser );
   }
   HB_GC_LOCKF();

   handle = hb_fsOpen( sFileDef, FO_READ );
   if( handle != FS_ERROR )
   {
      HB_FOFFSET size = hb_fsSeekLarge( handle, 0, FS_END );
      HB_SIZE    nRead;

      szContent[ 0 ] = '+';
      szContent[ 1 ] = '\0';
      *nLen = 1;

      if( ! nRows && size )  /* full log */
      {
         hb_fsSeekLarge( handle, 0, FS_SET );
         szContent = ( char * ) hb_xrealloc( szContent, ( HB_SIZE ) size + 2 );
         nRead = hb_fsReadLarge( handle, szContent + 1, ( HB_SIZE ) size );
         szContent[ nRead + 1 ] = '\0';
         *nLen = ( HB_SIZE ) nRead + 1;
      }
      else if( size )  /* last nRow lines up to max 4K */
      {
         char pBuffer[ 4096 ];

         hb_fsSeekLarge( handle, -1 * HB_MIN( size - 1, 4096 ), FS_END );
         nRead = hb_fsReadLarge( handle, pBuffer, 4096 );
         if( nRead )
         {
            HB_SIZE      n = nRows + 1, nChars;
            const char * ptr = pBuffer + nRead - 1;

            while( n && ptr > pBuffer)
            {
               if( *ptr == '\n' && --n == 0 )
                  break;
               ptr--;
            }
            if( ptr > pBuffer )
               ptr++;
            nChars = pBuffer + nRead - ptr;
            szContent = ( char * ) hb_xrealloc( szContent, nChars + 2 );
            memcpy( szContent + 1, ptr, nChars );
            szContent[ nChars + 1 ] = '\0';
            *nLen = nChars + 1;
         }
         else
            szContent[ 0 ] = '-';
      }
      hb_fsClose( handle );
   }
   else
   {
      szContent[ 0 ] = '-';
      szContent[ 1 ] = '\0';
      *nLen = 1;
   }

   HB_GC_UNLOCKF();
   return szContent;
}

PUSERSTRU leto_FindUserStru( HB_THREAD_ID hThreadID )
{
   PUSERSTRU pUStruRet = NULL;

   HB_GC_LOCKU();
   if( hThreadID )
   {
      PUSERSTRU pUStru = s_users;
      HB_USHORT uiCount = 0;
      HB_USHORT ui;

      for( ui = 0; ui < s_uiUsersMax; ui++, pUStru++ )
      {
         if( pUStru->iUserStru )
         {
            if( HB_THREAD_EQUAL( pUStru->hThreadID, hThreadID ) )
            {
               pUStruRet = pUStru;
               break;
            }
            if( ++uiCount >= s_uiUsersCurr )
               break;
         }
      }
   }
   HB_GC_UNLOCKU();

   return pUStruRet;
}

/* remove log file with intro for new connection if exists */
static HB_BOOL leto_wUsLogDelete( PUSERSTRU pUStru )
{
   char    szName[ HB_PATH_MAX ];
   HB_BOOL bErased = HB_FALSE;

   if( pUStru )
   {
      strcpy( szName, s_szDirBase );
      hb_snprintf( szName + strlen( s_szDirBase ), HB_PATH_MAX - 1 - strlen( s_szDirBase ),
                   "letodbf_%0*d.log", leto_log10Len( s_uiUsersAlloc ), pUStru->iUserStru - 1 );
      if( hb_fsFile( szName ) )
      {
         bErased = hb_fsDelete( szName );
      }
   }

   return bErased;
}

void leto_wUsLog( PUSERSTRU pUStru, int iMsgLen, const char * s, ... )
{
   HB_FHANDLE   handle;
   char         szName[ HB_PATH_MAX ];

   if( ! s )
      return;

   if( ! pUStru )  /* don't do -- a HB_GC_LOCKU() there */
      pUStru = leto_FindUserStru( HB_THREAD_SELF() );

   if( pUStru )
   {
      HB_GC_LOCKF();

      strcpy( szName, s_szDirBase );
      hb_snprintf( szName + strlen( s_szDirBase ), HB_PATH_MAX - 1 - strlen( s_szDirBase ),
                   "letodbf_%0*d.log", leto_log10Len( s_uiUsersAlloc ), pUStru->iUserStru - 1 );

      if( iMsgLen == 0 )
         iMsgLen = strlen( s );

      if( hb_fsFile( szName ) )
      {
         handle = hb_fsOpen( szName, FO_WRITE );
         if( handle != FS_ERROR )
            hb_fsSeek( handle, 0, SEEK_END );
      }
      else
         handle = hb_fsCreate( szName, 0 );

      if( handle >= 0 )
      {
         int  iYear, iMonth, iDay;
         char szDateTime[ 30 ];
         char szTime[ 9 ];

         hb_dateToday( &iYear, &iMonth, &iDay );
         hb_dateTimeStr( szTime );
         sprintf( szDateTime, "%02d.%02d %s.%03d ", iMonth, iDay, szTime, ( int ) ( hb_dateMilliSeconds() % 1000 ) );
         hb_fsWrite( handle, szDateTime, ( HB_USHORT ) strlen( szDateTime ) );

         if( iMsgLen > 1 && s[ iMsgLen - 2 ] == '\r' && s[ iMsgLen - 1 ] == '\n' )
            iMsgLen -= 2;
         else if( iMsgLen > 0 && s[ iMsgLen - 1 ] == '\n' )
            iMsgLen -= 1;

         if( iMsgLen >= 0 )
            hb_fsWrite( handle, s, ( HB_USHORT ) iMsgLen );
         else
         {
            char    message[ 1024 ];
            va_list ap;

            va_start( ap, s );
            hb_vsnprintf( message, sizeof( message ), s, ap );
            va_end( ap );
            hb_fsWrite( handle, message, ( HB_USHORT ) strlen( message ) );
         }
#if defined( HB_OS_UNIX ) && ! defined( HB_EOL_CRLF )
         hb_fsWrite( handle, "\n", 1 );
#else
         hb_fsWrite( handle, "\r\n", 2 );
#endif
         hb_fsClose( handle );
      }

      HB_GC_UNLOCKF();
   }
}

HB_FUNC( LETO_WUSLOG )
{
   leto_wUsLog( NULL, hb_parclen( 1 ), hb_parc( 1 ) );
}

HB_FUNC( LETO_WRLOG )
{
   leto_writelog( NULL, hb_parclen( 1 ), hb_parc( 1 ) );
}

HB_FUNC( LETO_SETHRBERROR )
{
   PUSERSTRU pUStru = leto_FindUserStru( HB_THREAD_SELF() );

   /* not found may happen at server start ( no user logged in ) and then we are here after leto_hrbLoad() failed */
   if( pUStru )
   {
      int          iOsCode = hb_parni( 3 );
      const char * szFileDesc = hb_parc( 6 );  /* filename or error description */

      if( pUStru->szHbError )
         hb_xfree( pUStru->szHbError );
      pUStru->szHbError = ( char * ) hb_xgrab( 42 + hb_parclen( 6 ) );
      sprintf( pUStru->szHbError, "    :%d-%d-%d-%d", hb_parni( 1 ), hb_parni( 2 ), iOsCode, hb_parni( 4 ) );
      if( hb_parl( 5 ) )
         sprintf( pUStru->szHbError + strlen( pUStru->szHbError ), "%c%s", '\t', szFileDesc );
      else
         sprintf( pUStru->szHbError + strlen( pUStru->szHbError ), "%c%s", '!', szFileDesc );
      if( ! iOsCode )
         iOsCode = 1;
      pUStru->iHbError = iOsCode;
   }
}

static const char * leto_Driver( HB_USHORT uiDriver )
{
#ifdef __BM
   return uiDriver == LETO_NTX ? "BMDBFNTX" : "BMDBFCDX";
#else
   return uiDriver == LETO_NTX ? "DBFNTX" : "DBFCDX";
#endif
}

HB_FUNC( LETO_DEFAULTDRIVER )
{
   hb_retc( leto_Driver( s_uiDriverDef ) );
}

static const char * leto_memoDefaultExt( int iType )
{
   switch( iType )
   {
      case DB_MEMO_DBT:
         return ".dbt";
      case DB_MEMO_FPT:
         return ".fpt";
      case DB_MEMO_SMT:
         return ".smt";
   }
   return NULL;
}

static int leto_lockScheme( int iIndexType )
{
   int iLockScheme;

   if( s_uiLockExtended )
   {
      if( iIndexType == LETO_NTX )  /* '1' */
#ifdef __HARBOUR30__
         iLockScheme = DB_DBFLOCK_HB32;
#else
         iLockScheme = DB_DBFLOCK_CLIPPER2;
#endif
      else if( s_uiLockExtended != DB_DBFLOCK_COMIX )
         iLockScheme = DB_DBFLOCK_HB32;
      else
         iLockScheme = DB_DBFLOCK_COMIX;
   }
   else
   {
      if( iIndexType == LETO_CDX )
         iLockScheme = DB_DBFLOCK_VFP;
      else
         iLockScheme = DB_DBFLOCK_CLIPPER;  /* DBFNTX + DBFNSX */
   }
   return iLockScheme;
}

static void leto_setSetDeleted( HB_BOOL fDeleted )
{
   HB_BOOL fSetDeleted = hb_setGetDeleted();

   if( fDeleted != fSetDeleted )
   {
      PHB_ITEM pItem = hb_itemNew( NULL );

      hb_itemPutL( pItem, fDeleted );
      hb_setSetItem( HB_SET_DELETED, pItem );
      hb_itemRelease( pItem );
   }
}

void leto_initSet( void )
{
   PHB_ITEM  pItem = hb_itemNew( NULL );
   LPRDDNODE pRDDNode;
   HB_USHORT uiRddID;

   hb_rddDefaultDrv( leto_Driver( s_uiDriverDef ) );
   hb_itemPutL( pItem, s_bHardCommit ? 1 : 0 );
   hb_setSetItem( HB_SET_HARDCOMMIT, pItem );
#ifdef __BM
   hb_itemPutL( pItem, HB_TRUE );
   hb_setSetItem( HB_SET_OPTIMIZE, pItem );
   hb_itemPutL( pItem, s_bForceOpt );
   hb_setSetItem( HB_SET_FORCEOPT, pItem );
#endif

   if( s_iAutOrder < 0 )  /* change Harbour default _SET_AUTOPEN -- is SET IN letodb.ini */
   {
      hb_itemPutL( pItem, HB_FALSE );
      hb_setSetItem( HB_SET_AUTOPEN, pItem );
   }
   else if( s_iAutOrder >= 0 )
   {
      hb_itemPutL( pItem, HB_TRUE );
      hb_setSetItem( HB_SET_AUTOPEN, pItem );

      if( s_iAutOrder > 0 )
      {
         hb_itemPutNI( pItem, s_iAutOrder );
         hb_setSetItem( HB_SET_AUTORDER, pItem );
      }
   }

   leto_setSetDeleted( HB_FALSE );  /* Harbour default */

   pRDDNode = hb_rddFindNode( leto_Driver( s_uiDriverDef ), &uiRddID );
   if( ! pRDDNode )
   {
      if( pItem )
         hb_itemRelease( pItem );
      return;
   }

   /* all below need pRDDNode */

   if( s_uiMemoType )
   {
      hb_itemPutNI( pItem, s_uiMemoType );
      SELF_RDDINFO( pRDDNode, RDDI_MEMOTYPE, 0, pItem );

      hb_itemPutC( pItem, leto_memoDefaultExt( s_uiMemoType ) );
      SELF_RDDINFO( pRDDNode, RDDI_MEMOEXT, 0, pItem );
   }
   if( s_uiMemoBlocksize )
   {
      hb_itemPutNI( pItem, s_uiMemoBlocksize );
      SELF_RDDINFO( pRDDNode, RDDI_MEMOBLOCKSIZE, 0, pItem );
   }
   if( s_uiLockExtended )
   {
      hb_itemPutNI( pItem, leto_lockScheme( s_uiDriverDef ) );
      SELF_RDDINFO( pRDDNode, RDDI_LOCKSCHEME, 0, pItem );
   }
   if( s_pTrigger )
   {
      hb_itemPutC( pItem, s_pTrigger );
      SELF_RDDINFO( pRDDNode, RDDI_TRIGGER, 0, pItem );
   }
   if( s_pPendTrigger )
   {
      hb_itemPutC( pItem, s_pPendTrigger );
      SELF_RDDINFO( pRDDNode, RDDI_PENDINGTRIGGER, 0, pItem );
   }
   if( pItem )
      hb_itemRelease( pItem );
}

HB_FUNC( LETO_INITSET )  /* during server startup */
{
   PUSERSTRU pUStru = letoGetUStru();

   if( ! pUStru )
      leto_initSet();
}

HB_FUNC( LETO_SETDIRBASE )  /* during server startup */
{
   PUSERSTRU pUStru = letoGetUStru();

   if( ! pUStru )
      strcpy( s_szDirBase, hb_parc( 1 ) );
}

/* the shorter the faster, formerly it was "LETOxxxxx" --> "Exxxxx" */
static _HB_INLINE_ void leto_MakeAlias( HB_ULONG ulAreaID, char * szAlias )
{
   szAlias[ 0 ] = 'E';
   ultostr( ulAreaID, ++szAlias );  /* sure with '\0' terminated */
}

static AREAP leto_RequestArea( HB_ULONG ulAreaID, HB_BOOL bMemIO, const char * szRealAlias )
{
   AREAP pArea = NULL;

   if( s_bNoSaveWA && ! bMemIO )
   {
      if( ulAreaID > 0 )  /* same as client */
         pArea = ( AREAP ) hb_rddGetWorkAreaPointer( ( int ) ulAreaID );
   }
   else
      pArea = hb_rddRequestArea( szRealAlias, NULL, HB_TRUE, s_ulTimeOut );

   if( pArea )
   {
      hb_rddSelectWorkAreaNumber( pArea->uiArea );
#if 0  /* removed, only one unique cdPage per Connection */
      hb_cdpSelect( pArea->cdPage );
#endif
   }

   return pArea;
}

static HB_BOOL leto_DetachArea( PAREASTRU pAStru )
{
   HB_ULONG ulAreaID = pAStru->ulAreaID;
   HB_BOOL  bRetVal;

   if( ( ! s_bNoSaveWA || pAStru->pTStru->bMemIO ) && pAStru->bNotDetached && ulAreaID )
   {
      AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

      /* first check if pAStru is the current WA, much faster as to search for */
      if( pArea != NULL )
      {
         if( ! pArea->atomAlias )
            pArea = NULL;
         else if( leto_stricmp( hb_dynsymName( (PHB_DYNS ) pArea->atomAlias ), pAStru->pTStru->szLetoAlias ) )
            pArea = NULL;
      }

      if( pArea == NULL )  /* not the active one, must search for */
      {
         PHB_DYNS pSymAlias;

         /* faster replace for hb_rddGetAliasNumber( szAlias, &ulAreaID ) as szAlias is well prepared */
         pSymAlias = hb_dynsymFind( pAStru->pTStru->szLetoAlias );
         ulAreaID = pSymAlias ? ( HB_ULONG ) hb_dynsymAreaHandle( pSymAlias ) : 0;
         if( ulAreaID )
            pArea = ( AREAP ) hb_rddGetWorkAreaPointer( ( int ) ulAreaID );
         else
            leto_writelog( NULL, -1, "ERROR leto_DetachArea! hb_rddGetAliasNumber! %s[&s:%lu]",
                           pAStru->szAlias, pAStru->pTStru->szLetoAlias, ulAreaID );
      }

      if( pArea != NULL )
      {
         bRetVal = ( hb_rddDetachArea( pArea, NULL ) == HB_SUCCESS );
         if( bRetVal )
            pAStru->bNotDetached = HB_FALSE;
      }
      else
      {
         bRetVal = HB_FALSE;
         leto_writelog( NULL, -1, "ERROR leto_DetachArea! no pArea for %s [%s:%lu]",
                        pAStru->szAlias, pAStru->pTStru->szLetoAlias, ulAreaID );
      }
   }
   else
      bRetVal = ulAreaID ? HB_TRUE : HB_FALSE;

   return bRetVal;
}

/* search for the server side workarea ID */
static PAREASTRU leto_FindArea( PUSERSTRU pUStru, HB_ULONG ulAreaID )
{
   if( ulAreaID )
   {
      PLETO_LIST_ITEM pListItem = pUStru->AreasList.pItem;
      PAREASTRU pAStru;

      while( pListItem )
      {
         pAStru = ( PAREASTRU ) ( pListItem + 1 );
         if( pAStru->ulAreaID == ulAreaID )
            return pAStru;
         pListItem = pListItem->pNext;
      }
   }

   return NULL;
}

/* search for the from client view workarea ID */
static PAREASTRU leto_FindUdfArea( PUSERSTRU pUStru, HB_ULONG ulAreaID )
{
   if( ulAreaID )
   {
      PLETO_LIST_ITEM pListItem = pUStru->AreasList.pItem;
      PAREASTRU pAStru;

      while( pListItem )
      {
         pAStru = ( PAREASTRU ) ( pListItem + 1 );
         if( pAStru->ulSelectID == ulAreaID )
            return pAStru;
         pListItem = pListItem->pNext;
      }
   }

   return NULL;
}

static PAREASTRU leto_FindAlias( PUSERSTRU pUStru, const char * szAlias )
{
   PAREASTRU       pAStru;
   PLETO_LIST_ITEM pListItem;

   if( ! szAlias || ! *szAlias )  /* active area this case */
   {
      HB_ULONG ulAreaID = hb_rddGetCurrentWorkAreaNumber();

      pListItem = pUStru->AreasList.pItem;
      while( pListItem )
      {
         pAStru = ( PAREASTRU ) ( pListItem + 1 );
         if( pAStru->ulAreaID == ulAreaID )
            return pAStru;
         pListItem = pListItem->pNext;
      }
      return NULL;
   }
   else
   {
      char szUpperAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
      HB_U32 uiCrc;

      hb_strncpyUpper( szUpperAlias, szAlias, HB_RDD_MAX_ALIAS_LEN );
      uiCrc = leto_hash( szUpperAlias, strlen( szUpperAlias ) );

      pListItem = pUStru->AreasList.pItem;
      while( pListItem )
      {
         pAStru = ( PAREASTRU ) ( pListItem + 1 );
         if( uiCrc == pAStru->uiCrc && ! leto_stricmp( pAStru->szAlias, szUpperAlias ) )
            return pAStru;
         pListItem = pListItem->pNext;
      }
   }

   return NULL;
}

/* pUStru->pCurAStru != NULL ensured by caller */
void leto_FreeCurrArea( PUSERSTRU pUStru )
{
   if( ! s_bNoSaveWA || pUStru->pCurAStru->pTStru->bMemIO )
   {
      if( pUStru->pCurAStru->ulAreaID && pUStru->pCurAStru->bNotDetached )
         leto_DetachArea( pUStru->pCurAStru );
   }
}

/* ulAreaID == 0 means all areas */
static void leto_FreeArea( PUSERSTRU pUStru, HB_ULONG ulAreaID, HB_BOOL bAllOther )
{
   PLETO_LIST_ITEM pListItem = pUStru->AreasList.pItem;
   PAREASTRU       pAStru;

   while( pListItem )
   {
      pAStru = ( PAREASTRU ) ( pListItem + 1 );
      if( ! s_bNoSaveWA || ( pAStru->pTStru && pAStru->pTStru->bMemIO ) )
      {
         if( pAStru->bNotDetached && ( ulAreaID == 0 ? HB_TRUE :
             ( ! bAllOther ? pAStru->ulAreaID == ulAreaID : pAStru->ulAreaID != ulAreaID ) ) )
         {
            if( leto_DetachArea( pAStru ) )
            {
               if( ! bAllOther && ulAreaID )
                  break;
            }
         }
      }
      pListItem = pListItem->pNext;
   }
}

static AREAP leto_SelectArea( PUSERSTRU pUStru, HB_ULONG ulAreaID )
{
   AREAP pArea = NULL;

   /* is wanted WA not the same as last used ? */
   if( ulAreaID != pUStru->ulCurAreaID || ! pUStru->pCurAStru )
   {
      PAREASTRU pAStru;

      if( ( pAStru = leto_FindArea( pUStru, ulAreaID ) ) != NULL )
      {
         if( pAStru->bNotDetached )
         {
            int      iArea;

            if( s_bNoSaveWA && ! pAStru->pTStru->bMemIO )  // ToDo memio alias
               iArea = ( int ) ulAreaID;  /*same as client */
            else
            {
               PHB_DYNS pSymAlias;

               /* faster replace for hb_rddGetAliasNumber( szAlias, &iArea ); */
               pSymAlias = hb_dynsymFind( pAStru->pTStru->szLetoAlias );
               iArea = pSymAlias ? hb_dynsymAreaHandle( pSymAlias ) : 0;
            }

            if( iArea > 0 )
            {
               pArea = ( AREAP ) hb_rddGetWorkAreaPointer( iArea );
               if( pArea != NULL )
               {
                  hb_rddSelectWorkAreaNumber( pArea->uiArea );
                  pUStru->ulCurAreaID = ulAreaID;
                  pUStru->pCurAStru = pAStru;
               }
            }
            else
               leto_wUsLog( pUStru, -1, "ERROR leto_selectArea() not detached - no Area %s",
                            pAStru->pTStru->szLetoAlias );
         }
         else
         {
            if( ( pArea = leto_RequestArea( ulAreaID, pAStru->pTStru->bMemIO, pAStru->pTStru->szLetoAlias ) ) != NULL )
            {
               pUStru->ulCurAreaID = ulAreaID;
               pUStru->pCurAStru = pAStru;
               pAStru->bNotDetached = HB_TRUE;
            }
            else
               leto_wUsLog( pUStru, -1, "ERROR leto_selectArea() detached - no Area %s", pAStru->pTStru->szLetoAlias );
         }
      }
      else
         leto_wUsLog( pUStru, -1, "ERROR leto_SelectArea(%lu)! find Area failed", ulAreaID );
   }
   else  /* same WA as lastly used */
   {
      if( pUStru->pCurAStru->bNotDetached )
      {
         pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
#if 0
         if( s_iDebugMode >= 8 )
            leto_wUsLog( pUStru, -1, "DEBUG leto_SelectArea() selected non-detached Area( %lu )", ulAreaID );
#endif
      }
      else
      {
         if( ( pArea = leto_RequestArea( pUStru->ulCurAreaID, pUStru->pCurAStru->pTStru->bMemIO,
                                         pUStru->pCurAStru->pTStru->szLetoAlias ) ) != NULL )
         {
            pUStru->pCurAStru->bNotDetached = HB_TRUE;
#if 0
            if( s_iDebugMode >= 8 )
               leto_wUsLog( pUStru, -1, "DEBUG leto_SelectArea() requested again Area( %lu )", ulAreaID );
#endif
         }
         else
            leto_wUsLog( pUStru, -1, "ERROR leto_SelectArea()! failed to again get detached area %d", ulAreaID );
      }
   }

   if( pArea == NULL )
   {
      pUStru->ulCurAreaID = 0;
      pUStru->pCurAStru = NULL;
   }

   return pArea;
}

/* leto_udf() */
HB_FUNC( LETO_SELECTAREA )
{
   PUSERSTRU pUStru = letoGetUStru();
   HB_ULONG  ulAreaID = hb_parnl( 1 );

   //HB_GC_LOCKT();

   if( pUStru->pCurAStru && leto_SelectArea( pUStru, ulAreaID ) )
      hb_retl( HB_TRUE );
   else
      hb_retl( HB_FALSE );

   //HB_GC_UNLOCKT();
}

/* HB_GC_LOCKT() must be ensured by caller */
static HB_ULONG leto_CreateAreaID( void )
{
   HB_ULONG ulRetVal;

   if( s_AvailIDS.iCurIndex <= 0 )
   {
      ++s_AvailIDS.ulNextID;
      ulRetVal = s_AvailIDS.ulNextID;
   }
   else
   {
      --s_AvailIDS.iCurIndex;
      ulRetVal = s_AvailIDS.pulAreaID[ s_AvailIDS.iCurIndex ];
   }

   return ulRetVal;
}

/* HB_GC_LOCKT() must be ensured by caller */
static void leto_DelAreaID( HB_ULONG ulAreaID )
{
   if( ! ulAreaID )
   {
      leto_writelog( NULL, 0, "ERROR leto_DelAreaID! param, inform server developer" );
      return;
   }

   if( ulAreaID > s_AvailIDS.ulNextID )
   {
      leto_writelog( NULL, 0, "ERROR leto_DelAreaID! nAreaID > s_AvailIDS.ulNextID, inform server developer" );
      return;
   }

   if( ! s_AvailIDS.pulAreaID )
   {
      s_AvailIDS.pulAreaID = ( HB_ULONG * ) hb_xgrab( sizeof( HB_ULONG ) * 100 );
      s_AvailIDS.iAllocIndex = 100;
      s_AvailIDS.iCurIndex = 0;
   }
   else if( s_AvailIDS.iCurIndex >= s_AvailIDS.iAllocIndex )
   {
      s_AvailIDS.iAllocIndex += 100;
      s_AvailIDS.pulAreaID = ( HB_ULONG * ) hb_xrealloc( s_AvailIDS.pulAreaID, sizeof( HB_ULONG ) * s_AvailIDS.iAllocIndex );
   }

   s_AvailIDS.pulAreaID[ s_AvailIDS.iCurIndex ] = ulAreaID;
   ++s_AvailIDS.iCurIndex;
}

static char leto_ExprGetType( PUSERSTRU pUStru, const char * pExpr, int iLen )
{
   PHB_ITEM     pItem = hb_itemPutCL( NULL, pExpr, iLen );
   const char * szType;
   char         cRet;

   /* note: removed hb_xvmSeqBegin() for hb_macroGetType() */
   szType = hb_macroGetType( pItem );
   if( szType[ 0 ] == 'U' && szType[ 1 ] == 'I' )
   {
      PHB_MACRO pMacro;
      PHB_ITEM  pValue;

      /* note: removed hb_strndup( pExpr, iLen ) */
      hb_xvmSeqBegin();
      pMacro = hb_macroCompile( hb_itemGetCPtr( pItem ) );
      if( pMacro )
      {
         pItem = hb_itemPutPtr( pItem, ( void * ) pMacro );
         pValue = hb_vmEvalBlockOrMacro( pItem );
         hb_vmDestroyBlockOrMacro( pItem );
         pItem = NULL;
      }
      else
         pValue = NULL;
      hb_xvmSeqEnd();
      if( pItem )
         hb_itemRelease( pItem );
      if( pUStru->iHbError || ! pValue )
         cRet = 'U';
      else
         cRet = hb_itemTypeStr( pValue )[ 0 ];
   }
   else
   {
      hb_itemRelease( pItem );
      cRet = szType[ 0 ];
   }

   return cRet;
}

static HB_BOOL leto_ParseFilter( PUSERSTRU pUStru, const char * pNew, HB_ULONG ulFltLen )
{
   int          iMode = 0;
   HB_BOOL      bLastCond = 0;
   int          iDeep = 0;
   int          piDeep[ PARSE_MAXDEEP ];
   const char * ppDeep[ PARSE_MAXDEEP ];
   const char * ptr, * ptr1;
   char         c, cQuo = ' ';

   for( ptr = ptr1 = pNew; ulFltLen; ptr++, ulFltLen-- )
   {
      c = *ptr;
      if( iMode == 0 )
      {
         if( c == '\"' || c == '\'' )
         {
            iMode = 1;        /* mode 1 - a string */
            cQuo = c;
         }
         else if( HB_ISFIRSTIDCHAR( c ) )
         {
            iMode = 2;        /* mode 2 - an identifier */
            ppDeep[ iDeep ] = ptr;
         }
         else if( HB_ISDIGIT( c ) )
         {
            iMode = 3;        /* mode 3 - a number */
         }
         else if( c == '(' )  /* parenthesis, but not of a function */
         {
            piDeep[ iDeep ] = 1;
            ppDeep[ iDeep ] = ptr + 1;
            if( ++iDeep >= PARSE_MAXDEEP )
               return HB_FALSE;

            piDeep[ iDeep ] = 0;
            ppDeep[ iDeep ] = NULL;
         }
         else if( ( c == '>' ) && ( ptr > pNew ) && ( *( ptr - 1 ) == '-' ) )
         {
            return HB_FALSE;
         }
      }
      else if( iMode == 1 )   /* inside a string */
      {
         if( c == cQuo )
            iMode = 0;
      }
      else if( iMode == 2 )   /* inside an identifier */
      {
         if( c == '(' )
         {
            piDeep[ iDeep ] = 2;
            if( ++iDeep >= PARSE_MAXDEEP )
               return HB_FALSE;

            piDeep[ iDeep ] = 0;
            ppDeep[ iDeep ] = NULL;
            iMode = 0;
         }
         else if( ( c == '-' ) && ( ulFltLen > 1 ) && ( *( ptr + 1 ) == '>' ) )     /* Check "FIELD->" */
         {
            if( iDeep >= 0 && ppDeep[ iDeep ] && ( ptr - ppDeep[ iDeep ] == 5 ) )
            {
               char pBuf[ 6 ];

               memcpy( pBuf, ppDeep[ iDeep ], 5 );
               pBuf[ 5 ] = '\0';
               hb_strLower( pBuf, 5 );
               if( strcmp( pBuf, "field" ) == 0 )
               {
                  --ulFltLen;
                  ++ptr;
               }
               else
                  return HB_FALSE;
            }
            else
               return HB_FALSE;
         }
         else if( ! ( HB_ISNEXTIDCHAR( c ) || c == ' ' ) )
         {
            iMode = 0;
         }
      }
      else if( iMode == 3 )   /* We are inside a number */
      {
         if( ! ( HB_ISDIGIT( c ) || ( c == '.' && ulFltLen > 1 && HB_ISDIGIT( *( ptr + 1 ) ) ) ) )
            iMode = 0;
      }

      if( iMode != 1 )
      {
         if( c == ')' )
         {
            if( --iDeep < 0 )
               return HB_FALSE;

            if( bLastCond && ptr1 > ppDeep[ iDeep ] )
            {
               if( leto_ExprGetType( pUStru, ptr1, ptr - ptr1 ) != 'L' )
                  return HB_FALSE;

               ptr1 = ptr + 1;
               bLastCond = 0;
            }
         }
         else if( iMode != 3 && c == '.' && (
                     ( ulFltLen >= 5 && *( ptr + 4 ) == '.' && ( *( ptr + 1 ) == 'A' || *( ptr + 1 ) == 'a' ) && ( *( ptr + 2 ) == 'N' || *( ptr + 2 ) == 'n' ) && ( *( ptr + 3 ) == 'D' || *( ptr + 3 ) == 'd' ) )
                     || ( ulFltLen >= 4 && *( ptr + 3 ) == '.' && ( *( ptr + 1 ) == 'O' || *( ptr + 1 ) == 'o' ) && ( *( ptr + 2 ) == 'R' || *( ptr + 2 ) == 'r' ) ) ) )
         {
            /* ptr stays at a beginning of .and. or .or. */
            if( iDeep && ( ppDeep[ iDeep - 1 ] > ptr1 ) )
            {
               ptr1 = ppDeep[ iDeep - 1 ];
               bLastCond = 1;
            }
            if( bLastCond )
            {
               if( iDeep && piDeep[ iDeep - 1 ] == 2 )
                  while( *ptr1 != '(' )
                     ptr1++;
               if( *ptr1 == '(' )
                  ptr1++;

               if( ( ptr > ( ptr1 + 1 ) ) && leto_ExprGetType( pUStru, ptr1, ptr - ptr1 ) != 'L' )
                  return HB_FALSE;
            }
            ptr += 3;
            ulFltLen -= 3;
            if( ulFltLen > 0 && *ptr != '.' )
            {
               ptr++;
               ulFltLen--;
            }
            ptr1 = ptr + 1;
            bLastCond = 1;
         }
      }
   }
   ptr--;
   if( ( ptr1 > pNew ) && ( ( ptr - ptr1 ) > 2 ) && leto_ExprGetType( pUStru, ptr1, ptr - ptr1 + 1 ) != 'L' )
      return HB_FALSE;

   return HB_TRUE;
}

static void leto_StrTran( char * szText, char cSearch, char cReplace, HB_SIZE nLen )
{
   HB_SIZE u;

   for( u = 0; u < nLen; u++ )
   {
      if( szText[ u ] == cSearch )
         szText[ u ] = cReplace;
   }
}

HB_FUNC( LETO_SETAPPOPTIONS )  /* during server startup */
{
   PUSERSTRU pUStru = letoGetUStru();
   HB_USHORT uiLen;

   if( HB_ISCHAR( 1 ) && ! pUStru )
   {
      s_pDataPath = ( char * ) hb_xgrab( hb_parclen( 1 ) + 2 );
      if( hb_parclen( 1 ) )
      {
         strcpy( s_pDataPath, hb_parc( 1 ) );
         s_uiDataPathLen = ( HB_USHORT ) hb_parclen( 1 );
         leto_StrTran( s_pDataPath, DEF_CH_SEP, DEF_SEP, s_uiDataPathLen );
      }
      else  /* ToDo watch and verify if not better throw a warning */
      {
         s_pDataPath[ 0 ] = DEF_SEP;
         s_pDataPath[ 1 ] = '\0';
         s_uiDataPathLen = 1;
      }
   }
   else   /* security exit cause of wrong param */
   {
      leto_writelog( NULL, 0, "DEBUG No valid call of leto_SetAppOptions()" );
      return;
   }

   if( HB_ISNUM( 2 ) )
      s_uiDriverDef = ( HB_USHORT ) hb_parni( 2 );
   if( HB_ISLOG( 3 ) )
      s_bFileFunc = hb_parl( 3 );
   if( HB_ISLOG( 4 ) )
      s_bAnyExt = hb_parl( 4 );
   if( HB_ISLOG( 5 ) )
      s_bPass4L = hb_parl( 5 );
   if( HB_ISLOG( 6 ) )
      s_bPass4M = hb_parl( 6 );
   if( HB_ISLOG( 7 ) )
      s_bPass4D = hb_parl( 7 );
   if( HB_ISCHAR( 8 ) && hb_parclen( 8 ) > 0 )
      leto_acc_setPath( hb_parc( 8 ) );

   if( HB_ISLOG( 10 ) )
      s_bShareTables = hb_parl( 10 );
   if( HB_ISLOG( 11 ) )
      s_bNoSaveWA = hb_parl( 11 );

   if( HB_ISNUM( 12 ) && HB_ISNUM( 13 ) )
      leto_setVarsMax( hb_parnl( 12 ), ( HB_USHORT ) hb_parni( 13 ) );

   if( HB_ISNUM( 14 ) )  // ToDo: maximum value
   {
      s_uiCacheRecords = ( HB_USHORT ) hb_parni( 14 );
      if( s_uiCacheRecords < 1 )
         s_uiCacheRecords = 10;
   }
   if( HB_ISNUM( 15 ) )
   {
      s_uiTablesAlloc = ( HB_UINT ) hb_parnl( 15 );
      if( s_uiTablesAlloc < 10 )
         s_uiTablesAlloc = 10;
      if( s_uiTablesAlloc > 1000000 )
         s_uiTablesAlloc = 1000000;
   }
   if( HB_ISNUM( 16 ) )
   {
      s_uiUsersAlloc = ( HB_USHORT ) hb_parnl( 16 );
      if( s_uiUsersAlloc > 65534 )
         s_uiUsersAlloc = 65534;
   }
   if( HB_ISNUM( 17 ) )
   {
      s_iDebugMode = hb_parnl( 17 );
      if( s_iDebugMode < 0 )
         s_iDebugMode = 0;
   }
   if( HB_ISLOG( 18 ) )
      s_bOptimize = hb_parl( 18 );
   if( HB_ISNUM( 19 ) )
      s_iAutOrder = hb_parni( 19 );
   if( HB_ISNUM( 20 ) )
      s_uiMemoType = ( HB_USHORT ) hb_parni( 20 );
   else
   {
      if( s_uiDriverDef )
         s_uiMemoType = DB_MEMO_DBT;
      else
         s_uiMemoType = DB_MEMO_FPT;
   }

   if( HB_ISLOG( 21 ) )
      s_bForceOpt = hb_parl( 21 );
   if( HB_ISNUM( 22 ) )
      s_uiLockExtended = ( HB_USHORT ) hb_parni( 22 );
   if( HB_ISLOG( 23 ) )
      s_bUdfEnabled = hb_parl( 23 );
   if( HB_ISNUM( 24 ) && hb_parni( 24 ) > 0 && hb_parni( 24 ) <= 0xFFFF && hb_parni( 24 ) % 32 == 0 )
      s_uiMemoBlocksize = ( HB_USHORT ) hb_parni( 24 );
   else
   {
      if( s_uiMemoType == DB_MEMO_DBT )
         s_uiMemoBlocksize = 512;
      else if( s_uiMemoType == DB_MEMO_FPT )
         s_uiMemoBlocksize = 64;
      else if( s_uiMemoType == DB_MEMO_SMT )
         s_uiMemoBlocksize = 32;
   }
   if( HB_ISLOG( 25 ) )
   {
      s_bLowerPath = hb_parl( 25 );
      if( s_bLowerPath )
         hb_strLower( s_pDataPath, s_uiDataPathLen );
   }
   if( HB_ISCHAR( 26 ) )
   {
      uiLen = ( HB_USHORT ) hb_parclen( 26 );
      s_pTrigger = ( char * ) hb_xgrab( uiLen + 1 );
      if( uiLen > 0 )
         memcpy( s_pTrigger, hb_parc( 26 ), uiLen );
      s_pTrigger[ uiLen ] = '\0';
   }
   if( HB_ISCHAR( 27 ) )
   {
      uiLen = ( HB_USHORT ) hb_parclen( 27 );
      s_pPendTrigger = ( char * ) hb_xgrab( uiLen + 1 );
      if( uiLen > 0 )
         memcpy( s_pPendTrigger, hb_parc( 27 ), uiLen );
      s_pPendTrigger[ uiLen ] = '\0';
   }
   if( HB_ISLOG( 28 ) )
      s_bSetTrigger = hb_parl( 28 );
   if( HB_ISLOG( 29 ) )
      s_bHardCommit = hb_parl( 29 );
}

/* leto_udf() */
HB_FUNC( LETO_GETAPPOPTIONS )
{
   HB_USHORT uiNum;

   if( HB_ISNUM( 1 ) )
      uiNum = ( HB_USHORT ) hb_parni( 1 );
   else
      uiNum = 0;

   switch( uiNum )
   {
      case LETOOPT_cDataPath:
         hb_retc( s_pDataPath );
         break;
      case LETOOPT_nIndexType:
         hb_retni( s_uiDriverDef );
         break;
      case LETOOPT_lAnyExt:
         hb_retl( s_bAnyExt );
         break;
      case LETOOPT_lShare:
         hb_retl( s_bShareTables );
         break;
      case LETOOPT_lNoSaveWA:
         hb_retl( s_bNoSaveWA );
         break;
      case LETOOPT_lOptimize:
         hb_retl( s_bOptimize );
         break;
      case LETOOPT_nAutoOrder:
         hb_retni( s_iAutOrder );
         break;
      case LETOOPT_nMemotype:
         hb_retni( s_uiMemoType );
         break;
      case LETOOPT_nLockCheme:
         hb_retni( leto_lockScheme( s_uiDriverDef ) );  // TO Fix: actual driver or only if extended lock ?
         break;
      case LETOOPT_lUdfEnable:
         hb_retl( s_bUdfEnabled );
         break;
      case LETOOPT_nMemoBSize:
         hb_retni( s_uiMemoBlocksize );
         break;
      case LETOOPT_bLowerPath:
         hb_retl( s_bLowerPath );
         break;
      case LETOOPT_pTrigger:
         hb_retc( s_pTrigger );
         break;
      case LETOOPT_pPendTrigger:
         hb_retc( s_pPendTrigger );
         break;
      case LETOOPT_bHardCommit:
         hb_retl( s_bHardCommit );
         break;
      case 42:  /* what else is the answer to all !? */
      {
#if 0  /* intentional forced crash test for debug test internal crash log */
         char * pCrash = NULL;

         hb_xfree( pCrash );
         hb_retni( 1 );
#else
         leto_writelog( NULL, -1, "INFO: USERSTRU %d, AREASTRU %d, TABLESTRU %d, GLOBESTRU %d INDEXSTRU %d ",
                        sizeof( USERSTRU ), sizeof( AREASTRU ), sizeof( TABLESTRU ), sizeof( GLOBESTRU ), sizeof( INDEXSTRU )  );
#endif
         break;
      }

      default:
         hb_retc( "please want what ?" );
         break;
   }
}

/* leto_udf() */
static PAREASTRU leto_Select( PUSERSTRU pUStru, HB_ULONG ulAreaID, const char * szAlias, HB_BOOL bUdf )
{
   PAREASTRU pAStru = NULL;

   if( ! ulAreaID && szAlias )
   {
      if( bUdf )
         pAStru = leto_FindAlias( pUStru, szAlias );
      else
      {
         PHB_DYNS pSymAlias = hb_dynsymFind( szAlias );

         /* replace for hb_rddGetAliasNumber( szAlias, ( int * ) &ulAreaID ); */
         ulAreaID = pSymAlias ? ( HB_ULONG ) hb_dynsymAreaHandle( pSymAlias ) : 0;
         if( ulAreaID )
            pAStru = leto_FindArea( pUStru, ulAreaID );
      }

      if( pAStru && leto_SelectArea( pUStru, pAStru->ulSelectID ) )
      {
         AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

         if( ! pArea )
            pArea = NULL;
#if 1
         if( pArea && s_iDebugMode > 20 )
            leto_wUsLog( pUStru, -1,"DEBUG leto_Select( %s ) found in %lu ", pArea->atomAlias, pAStru->ulAreaID );
#endif
      }
      else
         pAStru = NULL;

      return pAStru;
   }
   else if( ! ulAreaID )
   {
      ulAreaID = hb_rddGetCurrentWorkAreaNumber();
      bUdf = HB_FALSE;  /* non ! UDF mode */
   }

   if( ulAreaID )
   {
      if( bUdf )
         pAStru = leto_FindUdfArea( pUStru, ulAreaID );
      else
         pAStru = leto_FindArea( pUStru, ulAreaID );
      if( pAStru && leto_SelectArea( pUStru, pAStru->ulSelectID ) )
      {
#if 1
         if( s_iDebugMode > 20 )
            leto_wUsLog( pUStru, -1,"DEBUG leto_Select( %s ) pAStru area %lu (%lu) ", szAlias, pAStru->ulAreaID, pAStru->ulSelectID );
#endif
      }
      else
         pAStru = NULL;
   }

   return pAStru;
}

/* leto_udf() */
HB_FUNC( LETO_SELECT )  /* set ! WA by number or ALIAS */
{
   PUSERSTRU    pUStru = letoGetUStru();
   HB_ULONG     ulAreaID = 0;
   const char * szAlias = NULL;
   PAREASTRU    pAStru;

   if( HB_ISNUM( 1 ) && hb_parni( 1 ) > 0 )
      ulAreaID = ( HB_ULONG ) hb_parni( 1 );
   else if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) )
      szAlias = hb_parc( 1 );

   pAStru = leto_Select( pUStru, ulAreaID, szAlias, HB_TRUE );
   if( pAStru )
      hb_retni( pAStru->ulAreaID );
   else
      hb_retni( 0 );
}

/* leto_udf() */
HB_FUNC( LETO_ALIAS )
{
   char * szAlias = NULL;

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) )
   {
      PUSERSTRU pUStru = letoGetUStru();
      PAREASTRU pAStru = leto_FindAlias( pUStru, hb_parc( 1 ) );

      if( pAStru )
      {
         if( pAStru != pUStru->pCurAStru )
         {
            if( ! leto_SelectArea( pUStru, pAStru->ulAreaID ) )
               pAStru = NULL;
         }

         if( pAStru )
         {
            if( ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) || pAStru->ulUdf )  // ToDo memio alias
               szAlias = pAStru->szAlias;
            else
               szAlias = pAStru->pTStru->szLetoAlias;
         }
      }
   }

   hb_retc( szAlias );
}

/* leto_udf() */
HB_FUNC( LETO_AREAID )
{
   PUSERSTRU pUStru = letoGetUStru();
   PAREASTRU pAStru = leto_FindAlias( pUStru, hb_parc( 1 ) );

   if( pAStru )
   {
      hb_retnl( pAStru->ulAreaID );
#if 0
      if( pAStru->ulUdf )
         hb_retnl( pAStru->ulAreaID );
      else
         hb_retnl( pAStru->ulSelectID );
#endif
   }
   else
      hb_retni( 0 );
}

#if 0   /* don't remove - ready for later again use ;-) */
static HB_BOOL leto_CheckClientVer( PUSERSTRU pUStru, HB_USHORT uiVer )
{
   return pUStru->uiMajorVer * 100 + pUStru->uiMinorVer >= uiVer;
}
#endif

static long leto_LastUpdate( AREAP pArea )
{
   long lJulian = 0;

   if( pArea )
   {
      PHB_ITEM pItem = hb_itemNew( NULL );

      SELF_INFO( pArea, DBI_LASTUPDATE, pItem );
      lJulian = hb_itemGetDL( pItem );
      hb_itemRelease( pItem );
   }
   return lJulian;
}

static HB_ULONG leto_MemoInfo( AREAP pArea, char * szTemp )
{
   PHB_ITEM     pItem = hb_itemNew( NULL );
   int          iMemoType, iMemoVer, iMemoBlocksize, iLockScheme;
   DBORDERINFO  pOrderInfo;
   const char * szBagExt = NULL;
   HB_ULONG     ulLen;

   iMemoType = iMemoVer = iMemoBlocksize = iLockScheme = 0;
   pItem = hb_itemPutNI( pItem, 0 );
   if( SELF_INFO( pArea, DBI_MEMOTYPE, pItem ) == HB_SUCCESS )
      iMemoType = hb_itemGetNI( pItem );

   pItem = hb_itemPutNI( pItem, 0 );
   if( SELF_INFO( pArea, DBI_MEMOVERSION, pItem ) == HB_SUCCESS )
      iMemoVer = hb_itemGetNI( pItem );

   pItem = hb_itemPutNI( pItem, 0 );
   if( SELF_INFO( pArea, DBI_MEMOBLOCKSIZE, pItem ) == HB_SUCCESS )
      iMemoBlocksize = hb_itemGetNI( pItem );

   pItem = hb_itemPutNI( pItem, 0 );
   if( SELF_INFO( pArea, DBI_LOCKSCHEME, pItem ) == HB_SUCCESS )
   {
      iLockScheme = hb_itemGetNI( pItem );
      if( iLockScheme <= 0 )
         iLockScheme = DB_DBFLOCK_CLIPPER;
   }

   memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
   pOrderInfo.itmResult = hb_itemPutC( NULL, NULL );
   if( SELF_ORDINFO( pArea, DBOI_BAGEXT, &pOrderInfo ) == HB_SUCCESS )
      szBagExt = hb_itemGetCPtr( pOrderInfo.itmResult );

   pItem = hb_itemPutC( pItem, NULL );
   if( SELF_INFO( pArea, DBI_MEMOEXT, pItem ) == HB_SUCCESS )
      ulLen = sprintf( szTemp, "%s;%s;%d;%d;%d;%d;", szBagExt ? szBagExt : "",
                       hb_itemGetCPtr( pItem ), iMemoType, iMemoVer, iMemoBlocksize, iLockScheme );
   else
      ulLen = sprintf( szTemp, "bad;%d;%d;%d;%d;",
                       iMemoType, iMemoVer, iMemoBlocksize, iLockScheme );

   hb_itemRelease( pItem );
   if( pOrderInfo.itmResult )
      hb_itemRelease( pOrderInfo.itmResult );

   return ulLen;
}

static HB_BOOL leto_FindTag( PAREASTRU pAStru, const char * szOrder )
{
   HB_BOOL bTagFound = HB_FALSE;

   if( ! ( pAStru->pTagCurrent && ! leto_stricmp( pAStru->pTagCurrent->szTagName, szOrder ) ) )
   {
      LETOTAG * pTag = pAStru->pTag;

      while( pTag )
      {
         if( ! leto_stricmp( pTag->szTagName, szOrder ) )
         {
            pAStru->pTagCurrent = pTag;
            bTagFound = HB_TRUE;
            break;
         }
         else
            pTag = pTag->pNext;
      }
      if( ! bTagFound )
         pAStru->pTagCurrent = NULL;
   }

   return bTagFound;
}

static void leto_AddTag( PAREASTRU pAStru, PINDEXSTRU pIStru, const char * szOrder )
{
   HB_ULONG  ulLen = strlen( szOrder );
   LETOTAG * pTag = ( LETOTAG * ) hb_xgrabz( sizeof( LETOTAG ) );

   if( ulLen > LETO_MAX_TAGNAME )
      ulLen = LETO_MAX_TAGNAME;

   pTag->pIStru = pIStru;
   memcpy( pTag->szTagName, szOrder, ulLen );
   pTag->szTagName[ ulLen ] = '\0';
   hb_strUpper( pTag->szTagName, ulLen );

   if( pAStru->pTag )
   {
      LETOTAG * pTagNext = pAStru->pTag;

      while( pTagNext->pNext )
         pTagNext = pTagNext->pNext;
      pTagNext->pNext = pTag;
   }
   else
      pAStru->pTag = pTag;
}

static void leto_FreeTag( LETOTAG * pTag )
{
   if( pTag->pTopScope )
      hb_itemRelease( pTag->pTopScope );
   if( pTag->pBottomScope )
      hb_itemRelease( pTag->pBottomScope );
   hb_xfree( pTag );
}

HB_FUNC( LETO_N2B )
{
   HB_U32   n = ( HB_U32 ) hb_parnl( 1 );
   char     s[ 9 ];
   HB_UCHAR uLen = leto_n2b( s, n );

   hb_retclen( s, uLen );
}

HB_FUNC( LETO_B2N )
{
   HB_ULONG  n = leto_b2n( hb_parc( 1 ), ( HB_UCHAR ) hb_parclen( 1 ) );

   hb_retnl( n );
}

int leto_GetParam( const char * szData, const char ** pp2, const char ** pp3, const char ** pp4, const char ** pp5 )
{
   char * ptr;
   int    iRes = 0;

   if( ( ptr = strchr( szData, ';' ) ) != NULL )
   {
      iRes++;
      *ptr = '\0';
      if( pp2 )
      {
         *pp2 = ++ptr;
         if( ( ptr = strchr( *pp2, ';' ) ) != NULL )
         {
            iRes++;
            *ptr = '\0';
            if( pp3 )
            {
               *pp3 = ++ptr;
               if( ( ptr = strchr( *pp3, ';' ) ) != NULL )
               {
                  iRes++;
                  *ptr = '\0';
                  if( pp4 )
                  {
                     *pp4 = ++ptr;
                     if( ( ptr = strchr( *pp4, ';' ) ) != NULL )
                     {
                        iRes++;
                        *ptr = '\0';
                        if( pp5 )
                        {
                           *pp5 = ++ptr;
                           if( ( ptr = strchr( *pp5, ';' ) ) != NULL )
                           {
                              iRes++;
                              *ptr = '\0';
                              ptr++;
                              if( ( ptr = strchr( ptr, ';' ) ) != NULL )
                              {
                                 iRes++;
                                 *ptr = '\0';
                              }
                           }
                        }
                     }
                  }
               }
            }
         }
      }
   }

   return iRes;
}

static HB_USHORT leto_DataPath( const char * szFilename, char * szBuffer )
{
   const char * ptr = szFilename;
   int          iLen = 0;

   szBuffer[ 0 ] = '\0';
   if( szFilename[ 0 ] )
   {
      HB_BOOL bLeadSep = ( szFilename[ 0 ] == '/' || szFilename[ 0 ] == '\\' );
      HB_BOOL bMemIO;

      if( ! strncmp( szFilename + ( bLeadSep ? 1 : 0 ), "mem:", 4 ) )  /* e.g. "/mem:"  */
         bMemIO = HB_TRUE;
      else
         bMemIO = HB_FALSE;

      if( ! bMemIO && s_pDataPath )
      {
         iLen = ( int ) s_uiDataPathLen;
         strcpy( szBuffer, s_pDataPath );
         if( iLen > 0 && szBuffer[ iLen - 1 ] != '/' && szBuffer[ iLen - 1 ] != '\\' )
         {
            szBuffer[ iLen++ ] = DEF_SEP;
            szBuffer[ iLen ] = '\0';
         }
      }

      if( strlen( szFilename ) > 2 && ptr[ 1 ] == ':' && s_pDataPath ) // example: "c:\path"
         ptr += 2;
      while( *ptr == '/' || *ptr == '\\' )
         ++ptr;

      strcpy( szBuffer + iLen, ptr );
      if( s_bLowerPath )
         hb_strLower( szBuffer + iLen, strlen( szBuffer + iLen ) );
      iLen += strlen( ptr );
      leto_StrTran( szBuffer, DEF_CH_SEP, DEF_SEP, iLen );
   }

   return ( HB_USHORT ) iLen;
}

static HB_BOOL leto_ProdIndex( const char * szTable, const char * szBagName )
{
   const char * p1, * p1end, * p2, * p2end;
   char *       ptr;

   if( ! szTable || ! * szTable || ! szBagName || ! * szBagName )
      return HB_FALSE;

   if( strlen( szTable ) >= 2 && szTable[ 1 ] == ':' )
      p1 = szTable + 2;
   else
      p1 = szTable;
   if( ( ptr = strrchr( szTable, '.' ) ) == NULL )
      p1end = szTable + strlen( szTable );
   else
      p1end = ptr;

   if( strlen( szBagName ) >= 2 && szBagName[ 1 ] == ':' )
      p2 = szBagName + 2;
   else
      p2 = szBagName;
   if( ( ptr = strrchr( szBagName, '.' ) ) == NULL )
      p2end = szBagName + strlen( szBagName );
   else
      p2end = ptr;

   if( *p1 && *p2 && ( p2end - p2 ) == ( p1end - p1 ) &&
       ( ( ! s_bLowerPath ) ? ! strncmp( p1, p2, p1end - p1 ) :
                              ! hb_strnicmp( p1, p2, p1end - p1 ) ) )
      return HB_TRUE;
   else
      return HB_FALSE;
}

/* physical record length including delete flag */
static HB_USHORT leto_RecordLen( AREAP pArea )
{
   PHB_ITEM  pItem = hb_itemNew( NULL );
   HB_USHORT uiRecordLen;

   SELF_INFO( pArea, DBI_GETRECSIZE, pItem );
   uiRecordLen = ( HB_USHORT ) hb_itemGetNI( pItem );
   hb_itemRelease( pItem );

   return uiRecordLen;
}

static HB_ULONG leto_GetOrdInfoNL( AREAP pArea, HB_USHORT uiCommand )
{
   DBORDERINFO pInfo;
   HB_ULONG    ulResult;

   memset( &pInfo, 0, sizeof( DBORDERINFO ) );
   pInfo.itmResult = hb_itemPutNL( NULL, 0 );
   SELF_ORDINFO( pArea, uiCommand, &pInfo );
   ulResult = hb_itemGetNL( pInfo.itmResult );
   hb_itemRelease( pInfo.itmResult );

   return ulResult;
}

static HB_I64 leto_timeCounter( void )
{
#if defined( HB_OS_UNIX ) && ( defined( CLOCK_MONOTONIC_RAW ) | defined( CLOCK_MONOTONIC ) )
   struct timespec ts;

   #if defined( CLOCK_MONOTONIC_RAW )   /* without NTP changes, _RAW kernel >= 2.6.28 */
      clock_gettime( CLOCK_MONOTONIC_RAW, &ts );
   #elif defined( CLOCK_MONOTONIC )       /* only forwarding clock */
      clock_gettime( CLOCK_MONOTONIC, &ts );
   #endif

   return ( HB_I64 ) ( ( ts.tv_sec * 1000000000 ) + ts.tv_nsec );
#elif defined( HB_OS_WIN )
   LARGE_INTEGER llNanoSec;

   QueryPerformanceCounter( &llNanoSec );
   return ( HB_I64 ) ( llNanoSec.QuadPart );
#endif
}

/* in shared mode foreign software may change/ add records,
 * only in non shared mode we can buffer record data */
static _HB_INLINE_ HB_BOOL leto_Updated( PAREASTRU pAStru )
{
   return ( ! s_bShareTables || ! pAStru->pTStru->bShared ) ?
          ( HB_I64 ) ( pAStru->pTStru->pGlobe->llNanoSecWrite - pAStru->llNanoSecRead ) > 0 : HB_TRUE;
}

/*
 * removed: 1 if exclusive / file locked [ ! pAStru->pTStru->bShared || pAStru->bLocked ]
 * 0 if the record isn't locked by mysel
 * 1 if is locked by myself in given area
 * removed: -1 if is locked by other area [ letoIsRecInListTS( &pAStru->pTStru->LocksList, ulRecNo ) ]
 */
static int leto_IsRecLocked( PAREASTRU pAStru, HB_ULONG ulRecNo )
{
   if( s_bNoSaveWA && ! pAStru->pTStru->bMemIO )
   {
      /* this works as pTStru is thread local workarea in this mode */
      if( letoIsRecInListTS( &pAStru->pTStru->LocksList, ulRecNo ) )
         return 1;
   }
   else
   {
      if( letoIsRecInList( &pAStru->LocksList, ulRecNo ) )
         return 1;
   }

   return 0;
}

static void leto_IsRecLockedUS( PUSERSTRU pUStru, const char * szData )
{
   HB_ULONG ulRecNo = strtoul( szData, NULL, 10 );

   if( ulRecNo > 0 )
   {
      if( leto_IsRecLocked( pUStru->pCurAStru, ulRecNo ) == 1 )
         leto_SendAnswer( pUStru, "+T;", 3 );
      else
         leto_SendAnswer( pUStru, "+F;", 3 );
   }
   else
      leto_SendAnswer( pUStru, szErr2, 4 );
}

/* return the possible theoretic maximum length of send record data */
static _HB_INLINE_ HB_ULONG leto_recLen( PTABLESTRU pTStru )
{
/*
 *   3 -- HB_UINT24 len of data
 *   1 -- record flags byte, e.g. EOF, deleted etc
 *   5 -- HB_UINT32 RecNo + ';'
 *
 *  if iZipRecord >= 0
 *     pTStru->uiRecordlen
 *  else  // note: bigger as uiRecordlen with all full filled strings or 'OTHER' fields
 *     for each field:
 *        if eof()
 *           1 byte = '\0'
 *        else
 *           HB_FT_STRING:   if( FieldLen > 255 ? 2 : 1 ) bytes + fieldlen
 *           HB_FT_LOGICAL:  1 byte
 *           HB_FT_MEMO:     1 byte '\0' or '!' -- or fieldlen for binary Memos [ 4 ]
 *           binary fields:  fieldlen
 *           OTHER:          1 byte + field
 *  1 -- after field-data a ';'
 *
 *  5 -- HB_UINT32 reccount + ';'
 *  5 -- HB_UINT32 DBOI_POSITION + ';'
 *  5 -- HB_UINT32 DBOI_KEYCOUNT + ';'
 *
 * ==> pTStru->uiRecordlen + 24 + 1 elch reserve + ( pTStru->uiFields * 3 )
 */
   return pTStru->uiRecordLen + 25 + ( pTStru->uiFields * 3 );
}

static HB_ULONG leto_rec( PUSERSTRU pUStru, PAREASTRU pAStru, AREAP pArea, char * szData, HB_ULONG * ulRelPos )
{
   char *    pData = szData + SHIFT_FOR_LEN;
   HB_USHORT uiFieldCount = pArea->uiFieldCount;
   HB_ULONG  ulRealLen;

   if( uiFieldCount )
   {
      HB_BYTE * pRecord;
      HB_ULONG  ulRecNo;
      HB_ULONG  ulRecCount;

      SELF_GETREC( pArea, &pRecord );
      ulRecNo = ( ( DBFAREAP ) pArea )->ulRecNo;
      // SELF_RECNO( pArea, &ulRecNo );
      if( ( ! ulRelPos || ! *ulRelPos ) && s_bShareTables && pAStru->pTStru->bShared )
         SELF_RECCOUNT( pArea, &ulRecCount );
      else
         ulRecCount = ( ( DBFAREAP ) pArea )->ulRecCount;

      /* set time for hotbuffer BEFORE we read data, to be very sure to don't miss a change */
      if( s_bShareTables )
         pAStru->llNanoSecRead = leto_timeCounter();

      *pData = 0x40;
      if( pArea->fBof )
         *pData |= LETO_FLG_BOF;
      if( ! pArea->fEof )
      {
         if( leto_IsRecLocked( pAStru, ulRecNo ) == 1 )
            *pData |= LETO_FLG_LOCKED;
         if( pRecord[ 0 ] == '*' )
            *pData |= LETO_FLG_DEL;
         if( pArea->fFound )
            *pData |= LETO_FLG_FOUND;
      }
      else
         *pData |= LETO_FLG_EOF;

      HB_PUT_LE_UINT32( ( HB_BYTE * ) ++pData, ulRecNo );
      pData += 4;
      *pData++ = ';';

      if( pUStru->iZipRecord >= 0 )
      {
         HB_USHORT uiRecordLen = pAStru->pTStru->uiRecordLen;

         memcpy( pData, pRecord, uiRecordLen );  /* includes delete flag */
         pData += uiRecordLen;
      }
      else if( pArea->fEof )
      {
         memset( pData, '\0', uiFieldCount );
         pData += uiFieldCount;
      }
      else
      {
         const char * pRecordField = ( char * ) pRecord + 1;  /* first byte is delete flag */
         const char * ptr, * ptrEnd;
         LPFIELD      pField = NULL;
         HB_USHORT    ui, uiLen;

         for( ui = 0; ui < uiFieldCount; ui++ )
         {
            if( ui > 0 && pField )
               pRecordField += pField->uiLen;  /* pField from last loop */
            ptr = pRecordField;
            pField = pArea->lpFields + ui;

            switch( pField->uiType )
            {
               case HB_FT_STRING:
                  /* ToDo: HB_FF_COMPRESSED || HB_FF_ENCRYPTED => binary content */
                  ptrEnd = ptr + pField->uiLen - 1;
                  while( ptrEnd > ptr && *ptrEnd == ' ' )
                     ptrEnd--;
                  ulRealLen = ptrEnd - ptr + 1;
                  if( ulRealLen == 1 && *ptr == ' ' )
                     ulRealLen = 0;
                  /* Trimmed field length */
                  if( pField->uiLen < 256 )
                  {
                     pData[ 0 ] = ( HB_BYTE ) ulRealLen & 0xFF;
                     uiLen = 1;
                  }
                  else
                  {
                     uiLen = leto_n2b( pData + 1, ulRealLen );
                     pData[ 0 ] = ( HB_BYTE ) uiLen & 0xFF;
                     uiLen++;
                  }
                  pData += uiLen;
                  if( ulRealLen > 0 )
                     memcpy( pData, ptr, ulRealLen );
                  pData += ulRealLen;
                  break;

               case HB_FT_LONG:
               case HB_FT_FLOAT:
                  ptrEnd = ptr + pField->uiLen - 1;
                  while( ptrEnd > ptr && *ptr == ' ' )
                     ptr++;
                  ulRealLen = ptrEnd - ptr + 1;
                  while( ptrEnd > ptr && ( *ptrEnd == '0' || *ptrEnd == '.' ) )
                     ptrEnd--;
                  if( *ptrEnd == '0' || *ptrEnd == '.' )
                     *pData++ = '\0';
                  else
                  {
                     *pData++ = ( HB_BYTE ) ulRealLen & 0xFF;
                     memcpy( pData, ptr, ulRealLen );
                     pData += ulRealLen;
                  }
                  break;

               case HB_FT_DATE:
                  if( *ptr <= ' ' && pField->uiLen == 8 )
                     *pData++ = '\0';
                  else
                  {
                     memcpy( pData, ptr, pField->uiLen );
                     pData += pField->uiLen;
                  }
                  break;

               case HB_FT_LOGICAL:
                  *pData++ = *ptr;
                  break;

               case HB_FT_MEMO:
               case HB_FT_BLOB:
               case HB_FT_PICTURE:
               case HB_FT_OLE:
                  if( pField->uiLen == 4 )
                  {
                     memcpy( pData, ptr, pField->uiLen );
                     pData += pField->uiLen;
                  }
                  else  /* length 10 */
                  {
                     if( *( ptr + pField->uiLen - 1 ) == ' ' )
                        *pData++ = '\0';
                     else
                        *pData++ = '1';  /* was '!'; */
                  }
                  break;

               /* binary fields */
               case HB_FT_INTEGER:
               case HB_FT_CURRENCY:
               case HB_FT_DOUBLE:
               case HB_FT_CURDOUBLE:
               case HB_FT_TIME:
               case HB_FT_MODTIME:
               case HB_FT_TIMESTAMP:
               case HB_FT_AUTOINC:
               case HB_FT_ROWVER:
                  memcpy( pData, ptr, pField->uiLen );
                  pData += pField->uiLen;
                  break;

               case HB_FT_ANY:
                  if( pField->uiLen == 3 || pField->uiLen == 4 )
                  {
                     memcpy( pData, ptr, pField->uiLen );
                     pData += pField->uiLen;
                  }
                  else
                  {
                     PHB_ITEM pItem = hb_itemNew( NULL );

                     SELF_GETVALUE( pArea, ui + 1, pItem );
                     if( HB_IS_LOGICAL( pItem ) )
                     {
                        *pData++ = 'L';
                        *pData++ = ( hb_itemGetL( pItem ) ? 'T' : 'F' );
                     }
                     else if( HB_IS_DATE( pItem ) )
                     {
                        *pData++ = 'D';
                        hb_itemGetDS( pItem, ( char * ) pData );
                        pData += 8;
                     }
                     else if( HB_IS_STRING( pItem ) )
                     {
                        uiLen = ( HB_USHORT ) hb_itemGetCLen( pItem );
                        if( uiLen <= pField->uiLen - 3 )
                        {
                           *pData++ = 'C';
                           *pData++ = ( HB_BYTE ) uiLen & 0xFF;
                           *pData++ = ( HB_BYTE ) ( uiLen >> 8 ) & 0xFF;
                           memcpy( pData, hb_itemGetCPtr( pItem ), uiLen );
                           pData += uiLen;
                        }
                        else
                           *pData++ = '!';
                     }
                     else if( HB_IS_NUMERIC( pItem ) )
                     {
                        char * szString = hb_itemStr( pItem, NULL, NULL );
                        char * szTemp;

                        *pData++ = 'N';
                        if( szString )
                        {
                           szTemp = szString;
                           while( HB_ISSPACE( *szTemp ) )
                              szTemp++;
                           uiLen = ( HB_USHORT ) strlen( szTemp );
                           *pData++ = ( HB_BYTE ) uiLen & 0xFF;
                           memcpy( pData, szTemp, uiLen );
                           pData += uiLen;
                           hb_xfree( szString );
                        }
                        *pData++ = '\0';
                     }
                     else
                        *pData++ = 'U';

                     hb_itemRelease( pItem );
                  }
                  break;
            }
         }
      }

      *pData++ = ';';
      HB_PUT_LE_UINT32( ( HB_BYTE * ) pData, ulRecCount );
      pData += 4;

      /* !!! attention: the end of pData record-info is flexible !!! */
      if( pUStru->bBufKeyNo || pUStru->bBufKeyCount )
      {
         DBORDERINFO pOrderInfo;

         memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
         if( pUStru->bBufKeyNo )
         {
            *pData++ = '%';
            if( ulRelPos && *ulRelPos )
               HB_PUT_LE_UINT32( ( HB_BYTE * ) pData, *ulRelPos );
            else
            {
               pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
               SELF_ORDINFO( pArea, DBOI_POSITION, &pOrderInfo );
               HB_PUT_LE_UINT32( ( HB_BYTE * ) pData, hb_itemGetNL( pOrderInfo.itmResult ) );
               if( ulRelPos )
                  *ulRelPos = hb_itemGetNL( pOrderInfo.itmResult );
            }
            pData += 4;
         }
         if( pUStru->bBufKeyCount )
         {
            *pData++ = '$';
            pOrderInfo.itmResult = hb_itemPutNL( pOrderInfo.itmResult, 0 );
            SELF_ORDINFO( pArea, DBOI_KEYCOUNT, &pOrderInfo );
            HB_PUT_LE_UINT32( ( HB_BYTE * ) pData, hb_itemGetNL( pOrderInfo.itmResult ) );
            pData += 4;
         }

         hb_itemRelease( pOrderInfo.itmResult );
      }
      else
         *pData++ = '#';

      ulRealLen = pData - szData - SHIFT_FOR_LEN;
      HB_PUT_LE_UINT24( szData, ( HB_U32 ) ulRealLen );
      ulRealLen += SHIFT_FOR_LEN;
   }
   else
      ulRealLen = 0;

   return ulRealLen;
}

static char leto_OrdKeyType( AREAP pArea, const char * szTagName )
{
   DBORDERINFO pOrderInfo;
   char        cKeyType;

   memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
   if( szTagName )
      pOrderInfo.itmOrder = hb_itemPutC( NULL, szTagName );
   pOrderInfo.itmResult = hb_itemPutC( NULL, NULL );
   SELF_ORDINFO( pArea, DBOI_KEYTYPE, &pOrderInfo );
   cKeyType = *( hb_itemGetCPtr( pOrderInfo.itmResult ) );
   if( szTagName )
      hb_itemRelease( pOrderInfo.itmOrder );
   hb_itemRelease( pOrderInfo.itmResult );

   return cKeyType;
}

/* return HB_TRUE if szBagName have multiple tags,
 * if szBagName == NULL, return HB_TRUE if RDD can do so */
static HB_BOOL leto_OrdCanCompound( AREAP pArea, const char * szTagName )
{
   DBORDERINFO pOrderInfo;
   HB_BOOL     bCanCompound;

   memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
   if( szTagName )
   {
      pOrderInfo.itmOrder = hb_itemPutC( NULL, szTagName );
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_INDEXTYPE, &pOrderInfo );  /* this reflect if more than one TAG is in */
      bCanCompound = ( hb_itemGetNI( pOrderInfo.itmResult ) == DBOI_TYPE_COMPOUND );
      hb_itemRelease( pOrderInfo.itmOrder );
   }
   else
   {
      SELF_ORDINFO( pArea, DBOI_ISMULTITAG, &pOrderInfo );  /* this reflect if RDD can multi-TAG */
      bCanCompound = hb_itemGetL( pOrderInfo.itmResult );
   }
   hb_itemRelease( pOrderInfo.itmResult );

   return bCanCompound;
}

/* with szBagName == NULL count all orders, else per BAGname */
static HB_USHORT leto_OrdCount( AREAP pArea, const char * szBagName )
{
   DBORDERINFO pOrderInfo;
   HB_USHORT   uiOrdCount;

   memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
   if( szBagName && *szBagName )
      pOrderInfo.atomBagName = hb_itemPutC( NULL, szBagName );
   pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
   SELF_ORDINFO( pArea, DBOI_ORDERCOUNT, &pOrderInfo );
   uiOrdCount = ( HB_USHORT ) hb_itemGetNI( pOrderInfo.itmResult );
   if( pOrderInfo.atomBagName )
      hb_itemRelease( pOrderInfo.atomBagName );
   hb_itemRelease( pOrderInfo.itmResult );

   return uiOrdCount;
}

static void leto_SetFocusQuick( AREAP pArea, const char * szOrder )
{
   DBORDERINFO pInfo;

   memset( &pInfo, 0, sizeof( DBORDERINFO ) );
   pInfo.itmOrder = hb_itemPutC( NULL, szOrder );
   /* pInfo.itmResult = hb_itemPutC( NULL, NULL ); */
   SELF_ORDLSTFOCUS( pArea, &pInfo );
   hb_itemRelease( pInfo.itmOrder );
   if( pInfo.itmResult )
      hb_itemRelease( pInfo.itmResult );
}

static int leto_SetFocus( AREAP pArea, const char * szOrder )
{
   DBORDERINFO pInfo;
   int         iOrder;

   memset( &pInfo, 0, sizeof( DBORDERINFO ) );
   if( szOrder[ 0 ] == '\0' )
   {
      iOrder = 0;
      /* elch -- this following block was complete missing ! */
      pInfo.itmOrder = hb_itemPutNI( NULL, iOrder );
      pInfo.itmResult = hb_itemPutC( NULL, NULL );
      SELF_ORDLSTFOCUS( pArea, &pInfo );
      hb_itemRelease( pInfo.itmOrder );
      hb_itemRelease( pInfo.itmResult );
   }
   else
   {
      pInfo.itmOrder = hb_itemPutC( NULL, szOrder );
      pInfo.itmResult = hb_itemPutC( NULL, NULL );
      SELF_ORDLSTFOCUS( pArea, &pInfo );
      hb_itemRelease( pInfo.itmOrder );
      pInfo.itmOrder = NULL;

      /* only here and now check if new order was succesful set */
      hb_itemPutNI( pInfo.itmResult, 0 );
      SELF_ORDINFO( pArea, DBOI_NUMBER, &pInfo );
      iOrder = hb_itemGetNI( pInfo.itmResult );

      hb_itemRelease( pInfo.itmResult );
   }

   return iOrder;
}

/* return "sBagname;sTagname;sFor;cKeytype;iKeysize;bUnique;bCustom;"  ToDo no bTemporary ? */
static int leto_IndexInfo( AREAP pArea, char * szRet, HB_USHORT uiNumber, const char * szKey, const char * szTagName,
                           const char * szBagName, HB_BOOL bProduction )
{
   DBORDERINFO pOrderInfo;
   int         iLen = 0;

   szRet[ 0 ] = bProduction ? '*' : ' ';
   strcpy( szRet + 1, szBagName );
   iLen += strlen( szBagName ) + 1;
   szRet[ iLen++ ] = ';';

   strcpy( szRet + iLen, szTagName );
   iLen += strlen( szTagName );
   szRet[ iLen++ ] = ';';

   strcpy( szRet + iLen, szKey );
   iLen += strlen( szKey );
   szRet[ iLen++ ] = ';';

   memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
   pOrderInfo.itmOrder = hb_itemPutNI( NULL, uiNumber );
   pOrderInfo.itmResult = hb_itemPutC( NULL, NULL );
   SELF_ORDINFO( pArea, DBOI_CONDITION, &pOrderInfo );
   strcpy( szRet + iLen, hb_itemGetCPtr( pOrderInfo.itmResult ) );
   iLen += hb_itemGetCLen( pOrderInfo.itmResult );
   szRet[ iLen++ ] = ';';

   hb_itemPutC( pOrderInfo.itmResult, NULL );
   SELF_ORDINFO( pArea, DBOI_KEYTYPE, &pOrderInfo );
   szRet[ iLen++ ] = *( hb_itemGetCPtr( pOrderInfo.itmResult ) );
   szRet[ iLen++ ] = ';';

   hb_itemPutNI( pOrderInfo.itmResult, 0 );
   SELF_ORDINFO( pArea, DBOI_KEYSIZE, &pOrderInfo );
   iLen += ultostr( hb_itemGetNL( pOrderInfo.itmResult ), szRet + iLen );
   szRet[ iLen++ ] = ';';

   hb_itemPutL( pOrderInfo.itmResult, HB_FALSE );
   SELF_ORDINFO( pArea, DBOI_ISDESC, &pOrderInfo );
   szRet[ iLen++ ] = hb_itemGetL( pOrderInfo.itmResult ) ? 'T' : 'F';
   szRet[ iLen++ ] = ';';

   hb_itemPutL( pOrderInfo.itmResult, HB_FALSE );
   SELF_ORDINFO( pArea, DBOI_UNIQUE, &pOrderInfo );
   szRet[ iLen++ ] = hb_itemGetL( pOrderInfo.itmResult ) ? 'T' : 'F';
   szRet[ iLen++ ] = ';';

   hb_itemPutL( pOrderInfo.itmResult, HB_FALSE );
   SELF_ORDINFO( pArea, DBOI_CUSTOM, &pOrderInfo );
   szRet[ iLen++ ] = hb_itemGetL( pOrderInfo.itmResult ) ? 'T' : 'F';
   szRet[ iLen++ ] = ';';
   szRet[ iLen ] = '\0';

   hb_itemRelease( pOrderInfo.itmOrder );
   hb_itemRelease( pOrderInfo.itmResult );

   return iLen;
}

/* Note: LetoDB will *not* re-open a once opened index with same TAGname, the first opened counts */
static PINDEXSTRU leto_InitIndex( PUSERSTRU pUStru, const char * szTagName, const char * szFileName,
                                  AREAP pArea, const char * szOrdKey, HB_USHORT uiOrder )
{
   PAREASTRU   pAStru = pUStru->pCurAStru;
   PTABLESTRU  pTStru = pAStru->pTStru;
   PINDEXSTRU  pIStru = NULL;
   HB_USHORT   ui = 0;
   HB_BOOL     bRegistered = HB_FALSE, bIndexed = HB_FALSE;

   HB_GC_LOCKT();  /* to not interfere with leto_Mgmt() et al .. */

   /* check if already known to table */
   while( ui < pTStru->uiIndexCount && ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStru->IndexList, ui ) ) != NULL )
   {
      if( ! leto_stricmp( szTagName, pIStru->szTagName ) )  /* TagName ever uppercase, ready opened for table */
      {
         LETOTAG * pTag = pAStru->pTag;

         bIndexed = HB_TRUE;
         while( pTag )
         {
            if( pTag->pIStru == pIStru )  /* already registered to this user */
            {
               bRegistered = HB_TRUE;
               break;
            }
            pTag = pTag->pNext;
         }

         break;
      }
      ui++;
   }

   if( ! bIndexed )  /* create pIStru for new index */
   {
      DBORDERINFO pOrderInfo;
      char        szTable[ HB_PATH_MAX ];
      char *      ptr;
      HB_BOOL     bProduction, bShared;

      pTStru->uiIndexCount++;
      pIStru = ( PINDEXSTRU ) letoAddToList( &pTStru->IndexList );
      pIStru->uiAreas = 0;

      s_uiIndexCurr++;
      if( s_uiIndexCurr > s_uiIndexMax )
         s_uiIndexMax = s_uiIndexCurr;

      leto_DataPath( ( const char * ) pUStru->pCurAStru->pTStru->szTable, szTable );

      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
      pOrderInfo.itmOrder = hb_itemPutNI( NULL, uiOrder );

      pOrderInfo.itmResult = hb_itemPutL( pOrderInfo.itmResult, HB_FALSE );
      SELF_ORDINFO( pArea, DBOI_SHARED, &pOrderInfo );
      bShared = hb_itemGetL( pOrderInfo.itmResult );

      pOrderInfo.itmResult = hb_itemPutC( pOrderInfo.itmResult, NULL );
      SELF_ORDINFO( pArea, DBOI_FULLPATH, &pOrderInfo );

      pIStru->szFullPath = ( char * ) hb_xgrab( hb_itemGetCLen( pOrderInfo.itmResult ) + 1 );
      strcpy( pIStru->szFullPath, hb_itemGetCPtr( pOrderInfo.itmResult ) );
      /* ToDo bShared versus bTemporary -- HArbour misses real filename etc. */
      bProduction = leto_ProdIndex( szTable, pIStru->szFullPath );

      ui = s_uiDataPathLen;
      if( ! strncmp( pIStru->szFullPath, "mem:", 4 ) )  /* important to check this first */
         ptr = pIStru->szFullPath;
      else if( ! ui )
         ptr = pIStru->szFullPath + 1;  /* remove leading [back]slash */
      else if( ! strncmp( pIStru->szFullPath, s_pDataPath, ui ) )
         ptr = pIStru->szFullPath + ui + 1;
      else
         ptr = pIStru->szFullPath + 1;  /* Houston ! */
      pIStru->szBagName = ( char * ) hb_xgrab( strlen( ptr ) + 1 );
      strcpy( pIStru->szBagName, ptr );

      hb_itemRelease( pOrderInfo.itmOrder );
      hb_itemRelease( pOrderInfo.itmResult );

      pIStru->bProduction = bProduction;
      pIStru->bShared = bShared;
      /* Note Harbour can also make compound multi-tag index for NTX */
      pIStru->bCompound = leto_OrdCanCompound( pArea, szTagName );
      pIStru->cKeyType = leto_OrdKeyType( pArea, szTagName );
      pIStru->szTagName = ( char * ) hb_xgrab( strlen( szTagName ) + 1 );
      strcpy( pIStru->szTagName, szTagName );
      pIStru->szOrdKey = ( char * ) hb_xgrab( strlen( szOrdKey ) + 1 );
      strcpy( pIStru->szOrdKey, szOrdKey );
   }

   if( ! bRegistered && pIStru &&
       ( ( pIStru->bProduction && ! strstr( pAStru->pTStru->szDriver, "NTX" ) ) || *szFileName == '*' ||
         ( ( ! s_bLowerPath ) ? ! strcmp( szFileName, pIStru->szBagName ) : ! leto_stricmp( szFileName, pIStru->szBagName ) ) ) )
   {
         pIStru->uiAreas++;
         pIStru->bCompound = leto_OrdCanCompound( pArea, szTagName );
         leto_AddTag( pAStru, pIStru, szTagName );
         bRegistered = HB_TRUE;
   }

   HB_GC_UNLOCKT();

   if( bRegistered )
      return pIStru;
   else
      return NULL;
}

/* caller must free result */
static char * leto_IndexesInfo( PUSERSTRU pUStru, const char * szFullName, AREAP pArea )
{
   HB_USHORT uiOrder = 1, uiRegistered = 0, uiOrderActive = 0;
   HB_USHORT uiOrdMax = leto_OrdCount( pArea, NULL );
   char *    pIdxInfo = uiOrdMax ? ( char * ) hb_xgrab( ( LETO_IDXINFOBLOCK * uiOrdMax ) + 6 ) :
                                   ( char * ) hb_xgrab( 16 );
   int       iIdxInfoStart = eprintf( pIdxInfo, "%d;", uiOrdMax );

   if( uiOrdMax )
   {
      DBORDERINFO  pOrdkeyInfo, pTagInfo;
      char *       pDouble, * szTagDoubles = ( char * ) hb_xgrab( ( LETO_MAX_TAGNAME * uiOrdMax ) + uiOrdMax + 2 );
      int          iTagDoubles = 1;
      const char * szTagName;
      PINDEXSTRU   pIStru;

      *szTagDoubles = ';';
      *( szTagDoubles + 1 ) = '\0';
      memset( &pOrdkeyInfo, 0, sizeof( DBORDERINFO ) );
      pOrdkeyInfo.itmResult = hb_itemPutNI( pOrdkeyInfo.itmResult, 0 );
      SELF_ORDINFO( pArea, DBOI_NUMBER, &pOrdkeyInfo );
      uiOrderActive = ( HB_USHORT ) hb_itemGetNI( pOrdkeyInfo.itmResult );
      iIdxInfoStart += eprintf( pIdxInfo + iIdxInfoStart, "%d;", uiOrderActive );
      pUStru->pCurAStru->pTagCurrent = NULL;  /* for later check if NULL and uiOrderActive > 0  */

      memset( &pTagInfo, 0, sizeof( DBORDERINFO ) );
      while( uiOrder <= uiOrdMax )
      {
         pOrdkeyInfo.itmOrder = hb_itemPutNI( pOrdkeyInfo.itmOrder, uiOrder );
         pOrdkeyInfo.itmResult = hb_itemPutC( pOrdkeyInfo.itmResult, NULL );
         SELF_ORDINFO( pArea, DBOI_EXPRESSION, &pOrdkeyInfo );
         if( ! hb_itemGetCLen( pOrdkeyInfo.itmResult ) )
            break;  /* ? no more orders ? */

         pTagInfo.itmOrder = hb_itemPutNI( pTagInfo.itmOrder, uiOrder );
         pTagInfo.itmResult = hb_itemPutC( pTagInfo.itmResult, NULL );
         SELF_ORDINFO( pArea, DBOI_NAME, &pTagInfo );  /* == tagname */

         /* TAGname a must, for NTX created from BagName aka first 10 char of filename must be unique */
         if( hb_itemGetCLen( pTagInfo.itmResult ) )
         {
            szTagName = hb_itemGetCPtr( pTagInfo.itmResult );
            /* ignore orders with doubled TagNames */
            if( ( pDouble = strstr( szTagDoubles, szTagName ) ) != NULL &&
                *( pDouble - 1 ) == ';' && *( pDouble + hb_itemGetCLen( pTagInfo.itmResult ) ) == ';' )
            {
               leto_wUsLog( pUStru, -1, "ERROR leto_IndexesInfo() double TAGname %s used in %s",
                            szTagName, hb_itemGetCPtr( pTagInfo.itmResult ) );
               uiOrder++;
               continue;
            }
            else
               iTagDoubles += sprintf( szTagDoubles + iTagDoubles, "%s;", szTagName );
            if( ( pIStru = leto_InitIndex( pUStru, szTagName, szFullName, pArea,
                                           hb_itemGetCPtr( pOrdkeyInfo.itmResult ), uiOrder ) ) != NULL )
            {
               iIdxInfoStart += leto_IndexInfo( pArea, pIdxInfo + iIdxInfoStart, uiOrder,
                                                hb_itemGetCPtr( pOrdkeyInfo.itmResult ),
                                                szTagName,
                                                pIStru->szBagName,
                                                pIStru->bProduction );
               if( uiOrder == uiOrderActive )
                  leto_FindTag( pUStru->pCurAStru, szTagName );
               uiRegistered++;
            }
         }
         else  /* should never happen */
         {
            leto_wUsLog( pUStru, -1, "ERROR leto_IndexesInfo() TAGname is missing for %s", szFullName );
            break;
         }

         uiOrder++;
      }

      hb_itemRelease( pOrdkeyInfo.itmResult );
      if( pOrdkeyInfo.itmOrder )
         hb_itemRelease( pOrdkeyInfo.itmOrder );
      if( pTagInfo.itmResult )
         hb_itemRelease( pTagInfo.itmResult );
      if( pTagInfo.itmOrder )
         hb_itemRelease( pTagInfo.itmOrder );
      hb_xfree( szTagDoubles );
   }
   if( uiRegistered < uiOrdMax || uiOrderActive > uiRegistered || ( uiOrderActive && ! pUStru->pCurAStru->pTagCurrent ) )
   {
      int  iLenLen = strchr( pIdxInfo, ';' ) - pIdxInfo + 1, iLen;

      if( uiRegistered < uiOrdMax )
      {
         char szCount[ 8 ];

         if( s_iDebugMode > 10 )
            leto_wUsLog( pUStru, -1, "DEBUG leto_IndexesInfo() global ordcount %d versus %d", uiOrdMax, uiRegistered );
         iLen = eprintf( szCount, "%d;", uiRegistered );
         memcpy( pIdxInfo, szCount, iLen );
         if( iLenLen - iLen )
            memmove( pIdxInfo + iLen, pIdxInfo + iLenLen, iIdxInfoStart + 1 - iLenLen );  /* move including '\0' */
         iLenLen -= ( iLenLen - iLen );
      }
      /* catch an ignored possible double TagName by user */
      if( uiOrderActive > uiRegistered || ( uiOrderActive && ! pUStru->pCurAStru->pTagCurrent ) )
      {
         leto_wUsLog( pUStru, -1, "ERROR leto_IndexesInfo() corrected active order %d to 0", uiOrderActive );
         iLen = iLenLen;
         iLenLen = strchr( pIdxInfo + iLen, ';' ) - pIdxInfo + 1;
         *( pIdxInfo + iLen ) = '0';
         *( pIdxInfo + iLen + 1 ) = ';';
         if( iLenLen - iLen + 2 )
            memmove( pIdxInfo + iLen + 2, pIdxInfo + iLenLen, iIdxInfoStart + 1 - iLenLen );  /* move including '\0' */
         leto_SetFocus( pArea, "" );
         pUStru->pCurAStru->pTagCurrent = NULL;
      }
   }

   return pIdxInfo;
}

/* HB_GC_LOCKT() must be ensured by caller */
static void leto_CloseIndex( PINDEXSTRU pIStru )
{
   if( pIStru->szTagName )
   {
      hb_xfree( pIStru->szTagName );
      pIStru->szTagName = NULL;
   }
   if( pIStru->szBagName )
   {
      hb_xfree( pIStru->szBagName );
      pIStru->szBagName = NULL;
   }
   if( pIStru->szFullPath )
   {
      hb_xfree( pIStru->szFullPath );
      pIStru->szFullPath = NULL;
   }
   if( pIStru->szOrdKey )
   {
      hb_xfree( pIStru->szOrdKey );
      pIStru->szOrdKey = NULL;
   }
   pIStru->uiAreas = 0;
   s_uiIndexCurr--;
}


/* HB_GC_LOCKT() must be ensured by caller */
static int leto_FindTable( const char * szTable, HB_ULONG * ulAreaID )
{
   PTABLESTRU pTStru = s_tables;
   HB_UINT    uiCurr = 0, ui;
   int        iRet = -1;
   HB_UINT    uiCrc = leto_hash( szTable, strlen( szTable ) );

   if( ! s_uiTablesCurr )
      return -1;

   for( ui = 0; ui < s_uiTablesAlloc; pTStru++, ui++ )
   {
      if( pTStru->szTable )
      {
         if( pTStru->uiCrc == uiCrc &&  /* hash value pre-check */
             ! strcmp( ( const char * ) pTStru->szTable, szTable ) )
         {
            if( ulAreaID != NULL )
               *ulAreaID = pTStru->ulAreaID;
            iRet = ui;
            break;
         }
         if( ++uiCurr >= s_uiTablesCurr )
            break;
      }
   }

   return iRet;
}

static void leto_CloseGlobe( PGLOBESTRU pGStru )
{
   if( s_bNoSaveWA )
   {
      pGStru->uiAreas--;
      if( pGStru->uiAreas )
         return;

      s_uiGlobesCurr--;
   }
   else
      pGStru->uiAreas = 0;

   if( pGStru->szTable )
   {
      hb_xfree( pGStru->szTable );
      pGStru->szTable = NULL;
   }
   if( pGStru->szCdp )
   {
      hb_xfree( pGStru->szCdp );
      pGStru->szCdp = NULL;
   }
   pGStru->uiCrc = 0;
}

/* HB_GC_LOCKT() must be ensured by caller */
static void leto_CloseTable( PTABLESTRU pTStru )
{
   /* HB_GC_LOCKT(); */
   if( ! s_bNoSaveWA || pTStru->bMemIO )
      leto_DelAreaID( pTStru->ulAreaID );

   if( pTStru->szTable )
   {
      hb_xfree( pTStru->szTable );
      pTStru->szTable = NULL;
   }
   pTStru->uiCrc = 0;
   if( pTStru->szDriver )
   {
      hb_xfree( pTStru->szDriver );
      pTStru->szDriver = NULL;
   }
   letoListFree( &pTStru->LocksList );
   if( pTStru->uiIndexCount )
   {
      HB_USHORT  ui = 0;
      PINDEXSTRU pIStru;

      while( ui < pTStru->uiIndexCount && ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStru->IndexList, ui ) ) != NULL )
      {
         if( pIStru->szTagName )
            leto_CloseIndex( pIStru );
         ui++;
      }
      pTStru->uiIndexCount = 0;
   }
   letoListFree( &pTStru->IndexList );

   pTStru->ulAreaID = pTStru->uiAreas = 0;
   pTStru->pGlobe->bLocked = HB_FALSE;
   leto_CloseGlobe( pTStru->pGlobe );

   s_uiTablesCurr--;
   if( ( pTStru - s_tables ) / sizeof( TABLESTRU ) < s_uiTablesFree )
      s_uiTablesFree = ( pTStru - s_tables ) / sizeof( TABLESTRU );

   /* HB_GC_UNLOCKT(); */
}

static PGLOBESTRU leto_InitGlobe( const char * szTable, HB_U32 uiCrc, HB_UINT uiTable, HB_USHORT uiLen, const char * szCdp )
{
   PGLOBESTRU pGStru;

   if( s_bNoSaveWA )
   {
      /* search for existing globe */
      HB_UINT uiGlobe = 0;

      pGStru = s_globes;
      while( uiGlobe < s_uiTablesAlloc )
      {
         if( uiGlobe >= s_uiGlobesCurr )  /* new one */
         {
            s_uiGlobesCurr++;
            pGStru->uiAreas = 1;
            break;
         }
         else if( pGStru->uiCrc == uiCrc && ! strcmp( ( const char * ) pGStru->szTable, szTable ) )
         {
            pGStru->uiAreas++;
            break;
         }
         pGStru++;
         uiGlobe++;
      }
   }
   else
   {
      pGStru = s_globes + uiTable;
      pGStru->uiAreas = 1;
   }

   if( pGStru->uiAreas == 1 )
   {
      pGStru->szTable = ( HB_BYTE * ) hb_xgrab( uiLen + 1 );
      memcpy( pGStru->szTable, szTable, uiLen );
      pGStru->szTable[ uiLen ] = '\0';
      pGStru->uiCrc = uiCrc;
      if( ! szCdp )
      {
         szCdp = hb_cdpID();
         if( szCdp )
         {
            pGStru->szCdp = ( char * ) hb_xgrab( strlen( szCdp ) + 1 );
            strcpy( pGStru->szCdp, szCdp );
         }
         else
            pGStru->szCdp = NULL;
      }
      else
      {
         pGStru->szCdp = ( char * ) hb_xgrab( strlen( szCdp ) + 1 );
         strcpy( pGStru->szCdp, szCdp );
      }
   }

   if( s_bShareTables )
      pGStru->llNanoSecWrite = leto_timeCounter();
   else
      pGStru->llNanoSecWrite = 2;
   pGStru->ulRecCount = 0;
   pGStru->bLocked = HB_FALSE;

#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   pGStru->pMutex = HB_SPINLOCK_INIT;
#endif

   return pGStru;
}

static _HB_INLINE_ HB_BOOL leto_InitGlobeValidCP( int iTableStru, const char * szCdp )
{
   PTABLESTRU pTStru = s_tables + iTableStru;

   if( ( s_bNoSaveWA && ! pTStru->bMemIO ) ? pTStru->pGlobe->uiAreas > 1 : HB_TRUE )
   {
      if( pTStru->pGlobe->szCdp )
      {
         if( ! szCdp )
            szCdp = hb_cdpID();
         if( ! szCdp )
            return HB_FALSE;
         else
            return ! strcmp( pTStru->pGlobe->szCdp, szCdp );
      }
      else
         return ( ! hb_cdpID() );
   }
   else
      return HB_TRUE;
}

/* HB_GC_LOCKT() must be ensured by caller ! */
static int leto_InitTable( HB_ULONG ulAreaID, const char * szName, const char * szDriver, HB_BOOL bShared, const char * szLetoAlias, HB_BOOL bReadonly, const char * szCdp )
{
   PTABLESTRU pTStru = s_tables + s_uiTablesFree;
   HB_UINT    uiTable = s_uiTablesFree;
   HB_USHORT  uiLen;
   AREAP      pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   while( uiTable < s_uiTablesAlloc && pTStru->szTable )
   {
      pTStru++;
      uiTable++;
   }

   /* Note: we cannot realloc s_tables, because pAStru->pTStru will then point to old mem area */
   if( uiTable >= s_uiTablesAlloc )
      return -1;
   else
      s_uiTablesFree = uiTable + 1;
#if 0  /* clean before re-use */
   memset( pTStru, 0, sizeof( TABLESTRU ) );
#endif

   pTStru->ulAreaID = ulAreaID;
   strcpy( pTStru->szLetoAlias, szLetoAlias );

   uiLen = ( HB_USHORT ) strlen( szName );
   pTStru->szTable = ( HB_BYTE * ) hb_xgrab( uiLen + 1 );
   memcpy( pTStru->szTable, szName, uiLen );
   pTStru->szTable[ uiLen ] = '\0';
   pTStru->uiCrc = leto_hash( szName, ( int ) uiLen );

   pTStru->szDriver = ( char * ) hb_xgrab( strlen( szDriver ) + 1 );
   strcpy( pTStru->szDriver, szDriver );
   pTStru->bMemIO = ! strncmp( szName, "mem:", 4 );
   pTStru->bShared = bShared;
   pTStru->bReadonly = bReadonly;
   letoListInit( &pTStru->LocksList, sizeof( HB_ULONG ) );
   letoListInit( &pTStru->IndexList, sizeof( INDEXSTRU ) );
   pTStru->uiRecordLen = leto_RecordLen( pArea );
   SELF_FIELDCOUNT( pArea, &pTStru->uiFields );

   pTStru->ulFlags = 0;
   pTStru->uiIndexCount = 0;
   pTStru->uiAreas = 1;

   if( uiTable >= s_uiTablesMax )
      s_uiTablesMax = uiTable + 1;
   s_uiTablesCurr++;

   pTStru->pGlobe = leto_InitGlobe( szName, pTStru->uiCrc, uiTable, uiLen, szCdp );

   return uiTable;
}

static HB_BOOL leto_TableLock( PAREASTRU pAStru, int iTimeOut )
{
   HB_BOOL bRet;

   /* is table is exclusive opened */
   if( ( s_bShareTables || s_bNoSaveWA ) && ! pAStru->pTStru->bShared )
      bRet = HB_TRUE;
   else if( pAStru->bLocked )  /* already F-locked by myself */
      bRet = HB_TRUE;
   else if( ! letoListEmptyTS( &pAStru->pTStru->LocksList ) )  /* existing record locks */
      bRet = HB_FALSE;
   else if( ( ! s_bShareTables && ! s_bNoSaveWA ) || pAStru->pTStru->bMemIO )  /* only logical lock */
   {
      HB_GC_LOCKT();
      bRet = ! pAStru->pTStru->pGlobe->bLocked;
      if( bRet )
         pAStru->pTStru->pGlobe->bLocked = pAStru->bLocked = HB_TRUE;
      HB_GC_UNLOCKT();
   }
   else  /* try physical lock the file with the standard RDD method */
   {
      /* ! s_bNoSaveWA == we solely have in this moment the requested area,
       *  so can pre-check if already locked, to avoid blocking other from unlock */
      if( ! s_bNoSaveWA )
         bRet = pAStru->pTStru->pGlobe->bLocked;  /* file locked not by myself */
      else
         bRet = HB_FALSE;

      if( ! bRet )
      {
         AREAP      pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
         DBLOCKINFO dbLockInfo;

         dbLockInfo.itmRecID = NULL;
         dbLockInfo.uiMethod = DBLM_FILE;
         do
         {
            dbLockInfo.fResult = HB_FALSE;
            SELF_LOCK( pArea, &dbLockInfo );
            bRet = dbLockInfo.fResult ? HB_TRUE : HB_FALSE;
            if( bRet )
               break;
            else if( ! pAStru->pTStru->pGlobe->bLocked && iTimeOut > 21 )
            {
               /*  it's a 3rd party lock */
               hb_threadReleaseCPU();
               hb_threadReleaseCPU();
               iTimeOut -= 42;
               continue;
            }
            else
               break;
         }
         while( iTimeOut > 0 );

         if( bRet )
            pAStru->pTStru->pGlobe->bLocked = pAStru->bLocked = HB_TRUE;
      }
   }

   return bRet;
}

/* unlock all record locks of *one* connection and also maybe a file lock */
static HB_BOOL leto_TableUnlock( PAREASTRU pAStru, HB_BOOL bOnlyRecLocks, AREAP pArea )
{
   if( pArea == NULL )
   {
      pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      if( ! pArea )  /* should never happen */
      {
         leto_writelog( NULL, -1, "ERROR leto_TableUnlock() bSaveNoWA missing area! %s", pAStru->szAlias );
         return HB_FALSE;
      }
   }

   if( pAStru->pTStru->bShared )
   {
      if( s_bNoSaveWA && ! pAStru->pTStru->bMemIO )
      {
         /* as the pAStru->pTStru is local to thread, this removes all record locks of one thread */
         if( ! bOnlyRecLocks )
         {
            SELF_UNLOCK( pArea, NULL );
            pAStru->pTStru->pGlobe->bLocked = pAStru->bLocked = HB_FALSE;
            if( pAStru->pTStru->LocksList.pItem )
            {
               letoListLock( &pAStru->pTStru->LocksList );
               letoClearList( &pAStru->pTStru->LocksList );
               letoListUnlock( &pAStru->pTStru->LocksList );
            }
         }
         else if( pAStru->pTStru->LocksList.pItem )
         {
            PLETO_LOCK_ITEM pLockA;
            PHB_ITEM        pLock = NULL;

            letoListLock( &pAStru->pTStru->LocksList );

            /* note: ever the first in list is removed, until emty */
            do
            {
               if( ( pLockA = ( PLETO_LOCK_ITEM ) pAStru->pTStru->LocksList.pItem ) != NULL )
               {
                  pLock = hb_itemPutNL( pLock, pLockA->ulRecNo );
                  SELF_UNLOCK( pArea, pLock );
                  letoDelRecFromList( &pAStru->pTStru->LocksList, pLockA->ulRecNo );
               }
            }
            while( pAStru->pTStru->LocksList.pItem );

            letoListUnlock( &pAStru->pTStru->LocksList );
            if( pLock )
               hb_itemRelease( pLock );
         }
      }
      else if( pAStru->bLocked || pAStru->LocksList.pItem )
      {
         HB_BOOL bRealLock = s_bShareTables && pAStru->pTStru->bShared && ! pAStru->pTStru->bMemIO;

         if( pAStru->LocksList.pItem )
         {
            PLETO_LOCK_ITEM pLockA = ( PLETO_LOCK_ITEM ) pAStru->LocksList.pItem;
            PHB_ITEM        pLock = NULL;

            letoListLock( &pAStru->pTStru->LocksList );
            while( pLockA )
            {
               if( bRealLock )
               {
                  pLock = hb_itemPutNL( pLock, pLockA->ulRecNo );
                  SELF_UNLOCK( pArea, pLock );
               }
               letoDelRecFromList( &pAStru->pTStru->LocksList, pLockA->ulRecNo );
               pLockA = pLockA->pNext;
            }
            letoListUnlock( &pAStru->pTStru->LocksList );

            if( pLock )
               hb_itemRelease( pLock );
            letoClearList( &pAStru->LocksList );
         }

         if( ! bOnlyRecLocks && pAStru->bLocked )
         {
            if( bRealLock )
            {
                  SELF_GOCOLD( &( ( DBFAREAP ) pArea )->area );
                  SELF_RAWLOCK( &( ( DBFAREAP ) pArea )->area, FILE_UNLOCK, 0 );
            }
            pAStru->pTStru->pGlobe->bLocked = pAStru->bLocked = HB_FALSE;
         }
      }
   }

   return HB_TRUE;
}

/* HB_GC_LOCKT() moved into this from callers */
static HB_BOOL leto_CloseArea( PUSERSTRU pUStru, PAREASTRU pAStru )
{
   PTABLESTRU pTStru = pAStru->pTStru;
   HB_BOOL    bOk = HB_TRUE;

   HB_GC_LOCKT();

   pTStru->uiAreas--;
   if( pTStru->uiAreas == 0 )  /* close table if this was last area for it */
   {
      AREAP pArea;

      if( pAStru->bNotDetached )
      {
         /* first check if pAStru is the current WA, much faster as to search for */
         pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
         if( pArea != NULL )
         {
            if( ! pArea->atomAlias )
               pArea = NULL;
            else if( strcmp( hb_dynsymName( (PHB_DYNS ) pArea->atomAlias ), pAStru->pTStru->szLetoAlias ) )
               pArea = NULL;
         }

         if( pArea == NULL )  /* not the active one, must search for */
         {
            PHB_DYNS pSymAlias;
            int      iArea;

            /* FAST replace for hb_rddGetAliasNumber( szAlias, &iArea ); */
            pSymAlias = hb_dynsymFind( pTStru->szLetoAlias );
            iArea = pSymAlias ? hb_dynsymAreaHandle( pSymAlias ) : 0;
            if( iArea > 0 )
            {
               hb_rddSelectWorkAreaNumber( iArea );
               pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
            }
         }
      }
      else
         pArea = leto_RequestArea( pAStru->ulAreaID, pTStru->bMemIO, pTStru->szLetoAlias );

      if( pArea )
      {
         /* note: pArea->dbfi.itmCobExpr is released by hb_rddReleaseCurrentArea()-->hb_waClose()-->hb_waClearFilter() */
         if( s_bNoSaveWA && ! pAStru->pTStru->bMemIO )
            pArea->dbfi.itmCobExpr = NULL;  /* leto_ClearFilter( pArea ); */
         hb_rddReleaseCurrentArea();
      }
      else
         bOk = HB_FALSE;
      leto_CloseTable( pTStru );

      HB_GC_UNLOCKT();
   }
   else
   {
      LETOTAG *  pTag, * pTagNext;
      PINDEXSTRU pIStru;

      leto_TableUnlock( pAStru, HB_FALSE, NULL );

      pTag = pAStru->pTag;
      while( pTag )
      {
         pIStru = pTag->pIStru;
         pIStru->uiAreas--;
         if( ! pIStru->uiAreas )
         {
            leto_CloseIndex( pIStru );
            letoDelItemList( &pTStru->IndexList, ( PLETO_LIST_ITEM ) pIStru );
            pTStru->uiIndexCount--;
         }
         pTagNext = pTag->pNext;
         leto_FreeTag( pTag );
         pTag = pTagNext;
      }
      pAStru->pTag = NULL;

      /* note: despite HB_GC_LOCKT(), letop_OpenIndex can interfere -- so detach after free index */
      if( pAStru->bNotDetached )
         bOk = leto_DetachArea( pAStru );

      HB_GC_UNLOCKT();
   }

   if( bOk )
   {
      LETOTAG * pTag, * pTagNext;

      if( pAStru->LocksList.pItem )
         letoClearList( &pAStru->LocksList );

      pTag = pAStru->pTag;
      while( pTag )
      {
         pTagNext = pTag->pNext;
         leto_FreeTag( pTag );
         pTag = pTagNext;
      }

      if( pAStru->itmFltExpr )
         hb_itemRelease( pAStru->itmFltExpr );
#ifdef __BM
      if( pAStru->pBM )
         hb_xfree( pAStru->pBM );
#endif
      if( pUStru->pCurAStru == pAStru )
      {
         pUStru->ulCurAreaID = 0;
         pUStru->pCurAStru = NULL;
      }
      pUStru->uiAreasCount--;
      letoDelItemList( &pUStru->AreasList, ( PLETO_LIST_ITEM ) pAStru );
   }

   return bOk;
}

static void leto_CloseUdfAreas( PUSERSTRU pUStru )
{
   PLETO_LIST_ITEM pListItem = pUStru->AreasList.pItem;
   PAREASTRU       pAStru;

   while( pListItem )
   {
      pAStru = ( PAREASTRU ) ( pListItem + 1 );
      pListItem = pListItem->pNext;
      if( pAStru && pAStru->pTStru && pAStru->ulUdf == ( HB_ULONG ) pUStru->iUserStru )
      {
         leto_wUsLog( pUStru, -1, "DEBUG leto_CloseUdfAreas! %s (used: %d)", pAStru->pTStru->szTable, pAStru->pTStru->uiAreas );
         leto_CloseArea( pUStru, pAStru );
      }
   }
}

static void leto_InitArea( PUSERSTRU pUStru, int iTableStru, HB_ULONG ulAreaID, const char * szAlias,
                           const char * szRealAlias, HB_ULONG ulClientID )
{
   PAREASTRU pAStru = ( PAREASTRU ) letoAddToList( &pUStru->AreasList );
   HB_UCHAR  uLen;

   pUStru->uiAreasCount++;
   pAStru->pTStru = s_tables + iTableStru;

   pAStru->ulAreaID = ulAreaID;
   if( pUStru->bBeQuiet )
      pAStru->ulUdf = ( HB_ULONG ) pUStru->iUserStru;
   pAStru->ulSelectID = ulClientID;
   pAStru->uiSkipBuf = s_uiCacheRecords;
   pAStru->bUseSkipBuffer = HB_TRUE;
   uLen = ( HB_UCHAR ) strlen( szAlias );
   memcpy( pAStru->szAlias, szAlias, uLen );
   pAStru->szAlias[ uLen ] = '\0';
   hb_strUpper( pAStru->szAlias, uLen );
   pAStru->uiCrc = leto_hash( pAStru->szAlias, uLen );
   letoListInit( &pAStru->LocksList, sizeof( HB_ULONG ) );
   pAStru->llNanoSecRead = 1;  /* for initial refresh */
   pAStru->pTagCurrent = NULL;

   if( szRealAlias )
   {
      pAStru->bNotDetached = HB_TRUE;
      pUStru->ulCurAreaID = ulAreaID;
      pUStru->pCurAStru = pAStru;
   }
   else
   {
      pUStru->ulCurAreaID = 0;
      pUStru->pCurAStru = NULL;
      pAStru->bNotDetached = HB_FALSE;
   }

   if( s_iDebugMode > 20 )
      leto_wUsLog( pUStru, -1, "DEBUG leto_InitArea( connect:%d, area:%lu, alias:%s, not detached:%d -- request:%s)",
                   pUStru->iUserStru, ulAreaID, pAStru->szAlias, pAStru->bNotDetached, szRealAlias );
}

static void leto_CloseAll4Us( PUSERSTRU pUStru )
{
   PAREASTRU       pAStru;
   PLETO_LIST_ITEM pListItem;

   pListItem = pUStru->AreasList.pItem;
   while( pListItem )
   {
      pAStru = ( PAREASTRU ) ( pListItem + 1 );
      /* grab it immedeate here, as leto_CloseArea() free pAStru */
      pListItem = pListItem->pNext;
      if( leto_SelectArea( pUStru, pAStru->ulAreaID ) )
      {
         if( ! leto_CloseArea( pUStru, pAStru ) )  /* should not happen, else what then ??  -- report and ignore */
            leto_wUsLog( pUStru, -1, "ERROR! leto_CloseAll4Us() failed leto_CloseArea(%lu) [%lu]: %s",
                                      pAStru->ulAreaID, pAStru->pTStru->ulAreaID, pAStru->pTStru->szTable );
      }
      else
         leto_wUsLog( pUStru, -1, "ERROR! leto_CloseAll4Us() failed leto_SelectArea(%lu) [%lu]: %s",
                                   pAStru->ulAreaID, pAStru->pTStru->ulAreaID, pAStru->pTStru->szTable );
   }
}

/* elch ToDo ? -- not in the very initial 3 s phase, when thread3 try to establish error socket */
void leto_CloseUS( PUSERSTRU pUStru )
{
   hb_threadEnterCriticalSection( &pUStru->pMutex );
   if( pUStru->szHbError )
   {
      hb_xfree( pUStru->szHbError );
      pUStru->szHbError = NULL;
   }
   if( pUStru->pSendBuffer )
   {
      hb_xfree( pUStru->pSendBuffer );
      pUStru->pSendBuffer = NULL;
   }
   if( pUStru->szVersion )
   {
      hb_xfree( pUStru->szVersion );
      pUStru->szVersion = NULL;
   }
   if( pUStru->szDateFormat )
   {
      hb_xfree( pUStru->szDateFormat );
      pUStru->szDateFormat = NULL;
   }

   leto_CloseAll4Us( pUStru );  /* HB_GC_LOCKU() in leto_FindUserStru() leto_wUsLog( NULL ) */

   if( pUStru->pBufCrypt )
   {
      hb_xfree( pUStru->pBufCrypt );
      pUStru->pBufCrypt = NULL;
      pUStru->ulBufCryptLen = 0;
   }
   if( pUStru->iUserStru == s_iUserLock )
      s_iUserLock = -1;

   /* first the error socket */
   if( pUStru->hSocketErr != HB_NO_SOCKET )
   {
      hb_socketShutdown( pUStru->hSocketErr, HB_SOCKET_SHUT_RDWR );
      hb_socketClose( pUStru->hSocketErr );
      pUStru->hSocket = HB_NO_SOCKET;
   }

   /* now main socket */
   if( pUStru->hSocket != HB_NO_SOCKET )
   {
      hb_socketShutdown( pUStru->hSocketErr, HB_SOCKET_SHUT_RDWR );
      hb_socketClose( pUStru->hSocket );
      pUStru->hSocket = HB_NO_SOCKET;
   }
   if( pUStru->zstream )
   {
#ifdef USE_LZ4
      hb_lz4netClose( pUStru->zstream );
#else
      hb_znetClose( pUStru->zstream );
#endif
      pUStru->zstream = NULL;
   }
   if( pUStru->hSockPipe[ 1 ] != FS_ERROR )
   {
      hb_fsClose( pUStru->hSockPipe[ 1 ] );
      pUStru->hSockPipe[ 1 ] = FS_ERROR;
   }
   if( pUStru->hSockPipe[ 0 ] != FS_ERROR )
   {
      hb_fsClose( pUStru->hSockPipe[ 0 ] );
      pUStru->hSockPipe[ 0 ] = FS_ERROR;
   }

   HB_GC_LOCKU();

   if( pUStru->pBuffer )
   {
      hb_xfree( pUStru->pBuffer );
      pUStru->pBuffer = NULL;
   }

   if( s_iDebugMode > 0 )
   {
      const char * szNull = "(null)";

      leto_writelog( NULL, -1, "DEBUG closing connect %s:%d %s users=(%d : %d : %d), tables=(%d : %d)",
                     ( pUStru->szAddr    ? ( char * ) pUStru->szAddr    : szNull ),
                     ( pUStru->iPort     ? ( int ) pUStru->iPort     : 0 ),
                     ( *( pUStru->szExename ) ? ( char * ) pUStru->szExename : szNull ),
                     pUStru->iUserStru, s_uiUsersCurr, s_uiUsersMax,
                     s_uiTablesCurr, s_uiTablesMax );
   }

   if( pUStru->szAddr )
   {
      hb_xfree( pUStru->szAddr );
      pUStru->szAddr = NULL;
   }
   if( pUStru->szNetname )
   {
      hb_xfree( pUStru->szNetname );
      pUStru->szNetname = NULL;
   }
   if( pUStru->szUsername )
   {
      hb_xfree( pUStru->szUsername );
      pUStru->szUsername = NULL;
   }

   leto_varsown_release( pUStru );

   s_uiUsersCurr--;
   if( ( HB_USHORT ) ( ( pUStru - s_users ) / sizeof( USERSTRU ) ) < s_uiUsersFree )
      s_uiUsersFree = ( HB_USHORT ) ( ( pUStru - s_users ) / sizeof( USERSTRU ) );

   pUStru->iUserStru = 0;

   HB_GC_UNLOCKU();

   hb_threadLeaveCriticalSection( &pUStru->pMutex );
}

/* seek the right pUStru for given iServerPort -- used and HB_GC_LOCKU() from thread3 */
HB_BOOL leto_SeekUS( int iServerPort, HB_SOCKET hSocket )
{
   PUSERSTRU pUStru;
   int       iUserStru = 0, iUserCurr = 0;
   HB_BOOL   fRet = HB_FALSE;

   HB_GC_LOCKU();

   pUStru = s_users;
   while( iUserStru < s_uiUsersAlloc )
   {
      if( pUStru->iUserStru )
      {
         if( pUStru->iPort == iServerPort )
         {
            pUStru->hSocketErr = hSocket;
            fRet = HB_TRUE;
            break;
         }
         if( ++iUserCurr >= s_uiUsersCurr )
            break;
      }
      pUStru++;
      iUserStru++;
   }

   HB_GC_UNLOCKU();

   return fRet;
}

void leto_ReallocUSbuff( PUSERSTRU pUStru, HB_ULONG ulRecvLen )
{
   HB_GC_LOCKU();
   if( pUStru->pBuffer && ulRecvLen )
   {
      if( s_iDebugMode > 10 )
         leto_wUsLog( pUStru, -1, "DEBUG recv-buffer size %lu realloc", ulRecvLen + 1 );
      pUStru->ulBufferLen = ulRecvLen + 1;
      pUStru->pBuffer = hb_xrealloc( pUStru->pBuffer, pUStru->ulBufferLen );
   }
   else  /* 0 -> default size*/
   {
      if( s_iDebugMode > 10 )
         leto_wUsLog( pUStru, -1, "DEBUG recv-buffer size %lu free-ed", pUStru->ulBufferLen );
      pUStru->ulBufferLen = LETO_SENDRECV_BUFFSIZE;
      if( ! pUStru->pBuffer )
         pUStru->pBuffer = ( HB_BYTE * ) hb_xgrab( pUStru->ulBufferLen + 1 );
      else
         pUStru->pBuffer = ( HB_BYTE * ) hb_xrealloc( pUStru->pBuffer, pUStru->ulBufferLen + 1  );
   }
   HB_GC_UNLOCKU();

}

PUSERSTRU leto_InitUS( HB_SOCKET hSocket )
{
   PUSERSTRU pUStru = s_users + s_uiUsersFree;
   int       iUserStru = s_uiUsersFree;

   HB_CRITICAL_NEW( pMutex );

   HB_GC_LOCKU();

   while( iUserStru < s_uiUsersAlloc && pUStru->iUserStru )
   {
      pUStru++;
      iUserStru++;
   }

   /* Note: impossible to realloc pUStru during someone is logged in ... :-( */
   if( iUserStru >= s_uiUsersAlloc )
   {
      HB_GC_UNLOCKU();
      return NULL;
   }
   else
   {
      memset( pUStru, 0, sizeof( USERSTRU ) );  /* clean before re-use */
      pUStru->iUserStru = s_uiUsersFree = ( HB_USHORT ) ( iUserStru + 1 );  /* pUStru->iUserStru > 0 indicates a used pUStru */
   }

   s_uiUsersCurr++;
   if( s_uiUsersCurr > s_uiUsersMax )
      s_uiUsersMax = s_uiUsersCurr;

   letoListInit( &pUStru->AreasList, sizeof( AREASTRU ) );

   HB_GC_UNLOCKU();

   pUStru->pMutex = pMutex;
   pUStru->hSocket = hSocket;
   pUStru->uiDriver = s_uiDriverDef;
   pUStru->hSocketErr = HB_NO_SOCKET;
   pUStru->hSockPipe[ 0 ] = FS_ERROR;
   pUStru->hSockPipe[ 1 ] = FS_ERROR;
   pUStru->szAccess[ 0 ] = 0xFF;
   pUStru->szAccess[ 1 ] = 0xFF;
   pUStru->iZipRecord = -1;

   return pUStru;
}

/* elch: ping to clients if they are dead because have since a timespan no action */
/* attention: we can not create log entry after HB_GC_LOCKU() !! */
int leto_PingForZombies( long lTimeInactive )
{
   PUSERSTRU pUStru;
   HB_I64    llTimePoint = leto_MilliSec() / 1000;
   int       iZombies = 0;
   HB_USHORT uiError = 0;
   HB_USHORT uiChecked = 0;
   HB_USHORT * pPingUsers = NULL;
   HB_USHORT uiPingUsers = 0, uiLoop = 0;


   HB_GC_LOCKU();
   if( s_uiUsersCurr > 0 )
   {
      pUStru = s_users;
      pPingUsers = ( HB_USHORT * ) hb_xgrabz( sizeof( HB_USHORT ) * s_uiUsersCurr );

      while( uiLoop < s_uiUsersAlloc )
      {
         if( pUStru->iUserStru )
         {
            if( pUStru->hSocketErr != HB_NO_SOCKET &&
                ( long ) ( llTimePoint - pUStru->llLastAct ) > lTimeInactive )
            {
               pPingUsers[ uiPingUsers++ ] = ( HB_USHORT ) pUStru->iUserStru;
            }
            if( ++uiChecked >= s_uiUsersCurr )
               break;
         }
         uiLoop++;
         pUStru++;
      }
   }
   HB_GC_UNLOCKU();

   uiChecked = 0;
   while( uiPingUsers-- > 0 && ! leto_ExitGlobal( HB_FALSE ) )
   {
      pUStru = s_users + pPingUsers[ uiChecked++ ] - 1;

      /* to prevent this client log out during Zombie check */
      hb_threadEnterCriticalSection( &pUStru->pMutex );

      if( pUStru->iUserStru && pUStru->hSocketErr != HB_NO_SOCKET &&
          ( long ) ( llTimePoint - pUStru->llLastAct ) > lTimeInactive )
      {
         if( ! leto_AskAnswer( pUStru->hSocketErr ) )
         {
            /* wake up the serving thread for this dead connection */
            if( hb_socketShutdown( pUStru->hSocket, HB_SOCKET_SHUT_RDWR ) != 0 )
               uiError++;
            else
            {
               if( pUStru->hSockPipe[ 1 ] != FS_ERROR )
               {
                  const char cToPipe[ 1 ] = { ' ' };

                  hb_fsPipeWrite( pUStru->hSockPipe[ 1 ], cToPipe, 1, 0 );
               }
               iZombies++;
            }
         }
      }

      hb_threadLeaveCriticalSection( &pUStru->pMutex );
   }

   if( uiError )
      leto_writelog( NULL, 0, "DEBUG could not send shutdown to %d zombies ", uiError );
   if( pPingUsers )
      hb_xfree( pPingUsers );

   return iZombies;
}

/* maybe better not MT safe -- only called by main thread, and connection may have locked */
void leto_CloseAllSocket()
{
   PUSERSTRU pUStru;
   int       iUserStru = 0;

   HB_GC_LOCKU();

   pUStru = s_users;
   while( iUserStru < s_uiUsersAlloc )
   {
      if( pUStru->iUserStru )
      {
         if( pUStru->hSocketErr != HB_NO_SOCKET )
         {
            if( hb_socketShutdown( pUStru->hSocketErr, HB_SOCKET_SHUT_RDWR ) != 0 )
               leto_writelog( NULL, 0, "DEBUG cannot send shutdown to err socket..." );
         }
         if( pUStru->hSocket != HB_NO_SOCKET )
         {
            const char cToPipe[ 1 ] = { ' ' };

            if( pUStru->hSockPipe[ 1 ] != FS_ERROR )  /* wake up select() */
               hb_fsPipeWrite( pUStru->hSockPipe[ 1 ], &cToPipe, 1, 0 );
            else if( hb_socketShutdown( pUStru->hSocket, HB_SOCKET_SHUT_RDWR ) != 0 )  /* wake up poll() */
               leto_writelog( NULL, 0, "DEBUG cannot send shutdown ..." );
         }
      }
      pUStru++;
      iUserStru++;
   }

   HB_GC_UNLOCKU();
}

void leto_ForceCloseAllSocket()
{
   PUSERSTRU pUStru;
   int       iUserStru = 0;

   pUStru = s_users;
   while( iUserStru < s_uiUsersAlloc )
   {
      if( pUStru->iUserStru )
      {
         /* first the error socket */
         if( pUStru->hSocketErr != HB_NO_SOCKET )
         {
            hb_socketClose( pUStru->hSocketErr );
            pUStru->hSocketErr = HB_NO_SOCKET;
         }
         /* now main socket */
         if( pUStru->hSocket != HB_NO_SOCKET )
         {
            hb_socketClose( pUStru->hSocket );
            pUStru->hSocket = HB_NO_SOCKET;
         }
         if( pUStru->zstream )
         {
#ifdef USE_LZ4
            hb_lz4netClose( pUStru->zstream );
#else
            hb_znetClose( pUStru->zstream );
#endif
            pUStru->zstream = NULL;
         }
      }
      pUStru++;
      iUserStru++;
   }
}

HB_FUNC( LETO_ADDDATABASE )
{
   HB_USHORT  uiLen = ( HB_USHORT ) hb_parclen( 1 );
   DATABASE * pDBNext, * pDB = ( DATABASE * ) hb_xgrabz( sizeof( DATABASE ) );
   PUSERSTRU  pUStru = letoGetUStru();

   if( ! pUStru )
   {
      pDB->szPath = ( char * ) hb_xgrab( uiLen + 1 );
      memcpy( pDB->szPath, hb_parc( 1 ), uiLen );
      pDB->szPath[ uiLen ] = '\0';
      if( s_bLowerPath )
         hb_strLower( pDB->szPath, uiLen );

      pDB->uiDriver = ( HB_USHORT ) hb_parni( 2 );

      HB_GC_LOCKF();

      if( ! s_pDB )
         s_pDB = pDB;
      else
      {
         pDBNext = s_pDB;
         while( pDBNext->pNext )
            pDBNext = pDBNext->pNext;
         pDBNext->pNext = pDB;
      }

      HB_GC_UNLOCKF();
   }
}

/* ToDo actually not used */
static HB_USHORT leto_getDriver( const char * szPath )
{
   unsigned int iLen, iLenPath;
   char *       szLowerPath;
   HB_USHORT    uiRet = s_uiDriverDef;
   DATABASE *   pDB = s_pDB;

   iLenPath = ( HB_USHORT ) strlen( szPath );
   szLowerPath = ( char * ) hb_xgrab( iLenPath + 1 );
   strcpy( szLowerPath, szPath );
   if( s_bLowerPath )
      hb_strLower( szLowerPath, ( HB_ULONG ) iLenPath );

   while( pDB )
   {
      iLen = strlen( pDB->szPath );
      if( ( iLen <= iLenPath ) && ( ! strncmp( pDB->szPath, szLowerPath, iLen ) ) )
      {
         uiRet = pDB->uiDriver;
         break;
      }
      pDB = pDB->pNext;
   }
   hb_xfree( szLowerPath );

   return uiRet;
}

HB_FUNC( LETO_GETDRIVER )
{
   HB_USHORT uiDriver;

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 )  )
      uiDriver = leto_getDriver( hb_parc( 1 ) );
   else
      uiDriver = s_uiDriverDef;

   if( uiDriver )
      hb_retni( uiDriver );
   else
      hb_ret();
}

/* init function -- only one thread running that time, willful no MT */
HB_FUNC( LETO_CREATEDATA )  /* during server startup */
{
   const char * szAddr = HB_ISCHAR( 1 ) && hb_parclen( 1 ) ? hb_parc( 1 ) : "";
   const int    iPort = HB_ISNUM( 2 ) ? hb_parni( 2 ) : 2812;

   if( ! s_users )
   {
      /* allocate maximum useable user/ tables structures, plus one more to avoid ptr overflow */
      s_users = ( USERSTRU * ) hb_xgrabz( sizeof( USERSTRU ) * ( s_uiUsersAlloc + 1 ) );
      s_tables = ( TABLESTRU * ) hb_xgrabz( sizeof( TABLESTRU ) * ( s_uiTablesAlloc + 1 ) );
      s_globes = ( GLOBESTRU * ) hb_xgrabz( sizeof( GLOBESTRU ) * ( s_uiTablesAlloc + 1 ) );

      s_ulStartDateSec = ( unsigned long ) hb_dateMilliSeconds() / 1000;
      if( szAddr )
         s_szServerAddr = hb_strdup( szAddr );
      s_iServerPort = iPort;

      leto_writelog( NULL, -1, "INFO: %s %s, will run at %s%s:%d ( internal also used :%d )",
                     LETO_RELEASE_STRING, LETO_VERSION_STRING, strlen( szAddr ) ? "IP " : "port ", szAddr, iPort, iPort + 1 );
      leto_writelog( NULL, -1, "INFO: DataPath=%s, ShareTables=%d, NoSaveWA=%d, max database=%d",
                     ( s_pDataPath ? s_pDataPath : "" ), s_bShareTables, s_bNoSaveWA, s_uiTablesAlloc);
      leto_writelog( NULL, -1, "INFO: LoginPassword=%d, CacheRecords=%d, LockExtended=%d",
                     s_bPass4L, s_uiCacheRecords, s_uiLockExtended );
   }
}

/* this is an EXIT PROCEDURE for main thread -- so willful no MT locking done */
HB_FUNC( LETO_RELEASEDATA )  /* during server down */
{
   DATABASE * pDBNext, * pDB = s_pDB;
   HB_UINT    ui = 0;
   PUSERSTRU  pUStru = pUStru = letoGetUStru();
   PTABLESTRU pTStru;

   if( ! pUStru )
   {
      pUStru = s_users;
      if( pUStru )
      {
         while( ( HB_USHORT ) ui < s_uiUsersAlloc )
         {
            if( pUStru->iUserStru )
               leto_CloseUS( pUStru );  /* here in is a HB_GC_LOCKU() */
            ui++;
            pUStru++;
         }

         hb_xfree( s_users );
         s_users = NULL;
      }

      if( s_uiTablesCurr )
      {
         leto_writelog( NULL, -1, "DEBUG leto_ReleaseData: closed %d tables left open at shutdown",
                         s_uiTablesCurr );

         ui = 0;
         pTStru = s_tables;
         while( ui < s_uiTablesAlloc && s_uiTablesCurr )
         {
            if( pTStru->szTable )
            {
               if( leto_RequestArea( pTStru->ulAreaID, pTStru->bMemIO, pTStru->szLetoAlias ) )
                  leto_CloseTable( pTStru );
            }
            ui++;
            pTStru++;
         }
      }

      if( s_tables )
      {
         hb_xfree( s_tables );
         s_tables = NULL;
      }
      if( s_globes )
      {
         hb_xfree( s_globes );
         s_globes = NULL;
      }

      if( s_pDataPath )
      {
         hb_xfree( s_pDataPath );
         s_pDataPath = NULL;
      }
      if( pDB )
      {
         while( pDB )
         {
            pDBNext = pDB->pNext;
            if( pDB->szPath )
               hb_xfree( pDB->szPath );
            hb_xfree( pDB );
            pDB = pDBNext;
         }
      }

      leto_acc_release();  // with leto_acc_flush() before
      leto_vars_release();

      if( s_szServerAddr )
         hb_xfree( s_szServerAddr );
      /* elch: the missing bits */
      if( s_AvailIDS.pulAreaID )
         hb_xfree( s_AvailIDS.pulAreaID );
   }
}


static _HB_INLINE_ void leto_GotoForce( AREAP pArea, HB_ULONG ulRecNo )
{
   HB_ULONG ulOldRecNo;

   SELF_RECNO( pArea, &ulOldRecNo );  /* update pending relations */
   if( ulOldRecNo != ulRecNo )
      SELF_GOTO( pArea, ulRecNo );
}

static HB_ERRCODE leto_GotoIf( AREAP pArea, HB_ULONG ulRecNo )
{
   HB_ULONG ulOldRecNo;

   SELF_RECNO( pArea, &ulOldRecNo );  /* update pending relations */
   if( ulOldRecNo == ulRecNo )
      return HB_SUCCESS;
   else
      return SELF_GOTO( pArea, ulRecNo );
}

#ifdef USE_LZ4
static _HB_INLINE_ void leto_lz4Compress( char * pDst, HB_SIZE * pnDst, const char * pSrc, HB_SIZE nLen, int iLevel )
{
   *pnDst = ( HB_SIZE ) LZ4_compress_fast( pSrc, pDst, nLen, *pnDst, iLevel );
}

static _HB_INLINE_ HB_SIZE leto_lz4CompressBound( HB_ULONG ulLen )
{
   return ( HB_SIZE ) LZ4_compressBound( ulLen );
}
#endif

static HB_ULONG leto_CryptText( PUSERSTRU pUStru, const char * pData, HB_ULONG ulLen )
{
   HB_ULONG  ulBufLen;
   HB_SIZE   nDest;

#ifdef USE_LZ4
   if( pUStru->iZipRecord < 1 && ulLen > LETO_LZ4_COMPRESS_MIN )
   {
      nDest = leto_lz4CompressBound( ulLen );
      ulBufLen = nDest + 21;  /* encrypt +8, zip-lengths +8, length +4, termination + 1  */
   }
#else
   if( pUStru->iZipRecord < 1 && ulLen > LETO_ZIP_MINLENGTH )
   {
      nDest = hb_zlibCompressBound( ulLen );
      ulBufLen = nDest + 21;  /* encrypt +8, zip-lengths +8, length +4, termination + 1  */
   }
#endif
   else
   {
      nDest = 0;
      ulBufLen = ulLen + 21;
   }

   if( ulBufLen > pUStru->ulBufCryptLen )
   {
      if( ! pUStru->ulBufCryptLen )
      {
         pUStru->ulBufCryptLen = HB_MAX( ulBufLen, LETO_SENDRECV_BUFFSIZE );
         pUStru->pBufCrypt = ( HB_BYTE * ) hb_xgrab( pUStru->ulBufCryptLen + 1 );
      }
      else
      {
         pUStru->ulBufCryptLen = ulBufLen;
         pUStru->pBufCrypt = ( HB_BYTE * ) hb_xrealloc( pUStru->pBufCrypt, ulBufLen + 1 );
      }
   }

   if( ulLen )
   {
      /* compress here, if not later done by compressed traffic */
#ifdef USE_LZ4
      if( pUStru->iZipRecord < 1 && ulLen > LETO_LZ4_COMPRESS_MIN )
#else
      if( pUStru->iZipRecord < 1 && ulLen > LETO_ZIP_MINLENGTH )
#endif
      {
#ifdef USE_LZ4
         leto_lz4Compress( ( char * ) pUStru->pBufCrypt + 4, &nDest, ( const char * ) pData, ulLen,
                          HB_ZLIB_COMPRESSION_SPEED );
#else
         hb_zlibCompress( ( char * ) pUStru->pBufCrypt + 4, &nDest, ( const char * ) pData, ulLen,
                          HB_ZLIB_COMPRESSION_SPEED );  // == HB_ZLIB_RES_OK
#endif
         HB_PUT_LE_UINT32( pUStru->pBufCrypt + 4 + nDest, nDest );
         HB_PUT_LE_UINT32( pUStru->pBufCrypt + 4 + nDest + 4, ulLen );
         ulLen = ( nDest + 8 );
         HB_PUT_LE_UINT32( pUStru->pBufCrypt, ulLen | 0x80000000 );
      }
      else
      {
         memcpy( pUStru->pBufCrypt + 4, pData, ulLen );
         HB_PUT_LE_UINT32( pUStru->pBufCrypt, ulLen );
      }
   }
   else  /* ulLen == 0 */
      memset( pUStru->pBufCrypt, '\0', 4 );

   ulLen += 4;
   pUStru->pBufCrypt[ ulLen ] = '\0';

   return ulLen;
}

static void leto_RddInfo( PUSERSTRU pUStru, const char * szData )
{
   const char * pp2, * pp3 = NULL;
   int          nParam = leto_GetParam( szData, &pp2, &pp3, NULL, NULL );
   HB_USHORT    uiIndex;

   if( nParam < 1 || ! *szData || ( uiIndex = ( HB_USHORT ) atoi( pp2 ) ) == 0 )
      leto_SendAnswer( pUStru, szErr2, 4 );
   else
   {
      LPRDDNODE pRDDNode;
      HB_USHORT uiRddID;

      pRDDNode = hb_rddFindNode( szData, &uiRddID );
      if( pRDDNode )
      {
         PHB_ITEM pItem = NULL;
         char     szInfo[ 255 ];

         switch( uiIndex )
         {
            /* numerics */
#if 0 && ! defined( __HARBOUR30__ )
            case RDDI_INDEXPAGESIZE:
#endif
            case RDDI_DEBUGLEVEL:
            case RDDI_AUTOORDER:
            {
               HB_BOOL bSet = ( pp3 && *pp3 );

               if( bSet )
                  pItem = hb_itemPutNI( NULL, ( int ) atoi( pp3 ) );

               if( uiIndex == RDDI_AUTOORDER )
               {
                  int iOldAutOrder = hb_setGetAutOrder();

                  if( bSet )
                  {
                     hb_setSetItem( HB_SET_AUTORDER, pItem );
                     hb_itemRelease( pItem );
                  }
                  pItem = hb_itemPutNI( NULL, iOldAutOrder );
               }
               else if( uiIndex == RDDI_DEBUGLEVEL )  /* set new debug level, NOT TS */
               {
                  int iOldDebugMode = s_iDebugMode;

                  if( bSet )
                  {
                     s_iDebugMode = hb_itemGetNI( pItem );  /* NOT TS */
                     hb_itemRelease( pItem );
                  }
                  pItem = hb_itemPutNI( NULL, iOldDebugMode );
               }
               else  // ToDo
               {
                  if( ! bSet )
                     pItem = hb_itemPutNI( NULL, 0 );
                  SELF_RDDINFO( pRDDNode, uiIndex, 0, pItem );
               }
               sprintf( szInfo, "+%d;", hb_itemGetNI( pItem ) );
               if( pItem )
                  hb_itemRelease( pItem );
               break;
            }

            /* string values */
            case RDDI_TABLEEXT:
            case RDDI_MEMOEXT:
            case RDDI_ORDBAGEXT:      /* multi-TAG default */
            case RDDI_ORDEREXT:       /* single-TAG default */
            case RDDI_ORDSTRUCTEXT:   /* single-TAG default */
            case RDDI_PASSWORD:
               if( pp3 )
                  pItem = hb_itemPutC( NULL, pp3 );
               else
                  pItem = hb_itemPutC( NULL, NULL );

               SELF_RDDINFO( pRDDNode, uiIndex, 0, pItem );
               sprintf( szInfo, "+%s;", hb_itemGetCPtr( pItem ) );
               hb_itemRelease( pItem );
               break;

            /* booleans */
            case RDDI_OPTIMIZE:
            case RDDI_FORCEOPT:
            case RDDI_AUTOOPEN:
            case RDDI_MULTITAG:
            case RDDI_STRUCTORD:
               if( pp3 && strlen( pp3 ) == 1 )
                  pItem = hb_itemPutL( NULL, ( *pp3 == 'T' ) );
               else
                  pItem = hb_itemNew( NULL );

               SELF_RDDINFO( pRDDNode, uiIndex, 0, pItem );
               sprintf( szInfo, "+%c;", hb_itemGetL( pItem ) ? 'T' : 'F' );
               if( pItem )
                  hb_itemRelease( pItem );
               break;

            default:
               strcpy( szInfo, szErr3 );
               break;
         }
         leto_SendAnswer( pUStru, szInfo, strlen( szInfo ) );
      }
      else
         leto_SendAnswer( pUStru, szErr4, 4 );
   }
}

/* possible length of szValue ensured by caller */
static void leto_RddiGetValue( const char * szDriver, HB_USHORT uiIndex, char * szValue )
{
   *szValue = '\0';

   if( szDriver && *szDriver )
   {
      LPRDDNODE pRDDNode;
      HB_USHORT uiRddID;

      pRDDNode = hb_rddFindNode( szDriver, &uiRddID );
      if( pRDDNode )
      {
         switch( uiIndex )
         {
            case RDDI_ORDBAGEXT:      /* multitag default */
            case RDDI_ORDEREXT:       /* single-TAG default */
            case RDDI_TABLEEXT:
            {
               PHB_ITEM pItem = NULL;

               pItem = hb_itemPutC( pItem, NULL );
               SELF_RDDINFO( pRDDNode, uiIndex, 0, pItem );
               if( uiIndex == RDDI_ORDBAGEXT && ! hb_itemGetCLen( pItem ) )
                  SELF_RDDINFO( pRDDNode, RDDI_ORDEREXT, 0, pItem );
               strcpy( szValue, hb_itemGetCPtr( pItem ) );
               hb_itemRelease( pItem );
               break;
            }
         }
      }
   }
}

/* secured, only drop table/ index if not used */
static void leto_Drop( PUSERSTRU pUStru, const char * szData )
{
   const char * pIFile;
   char         szBuf[ HB_PATH_MAX ];
   int          nParam = leto_GetParam( szData, &pIFile, NULL, NULL, NULL );

   if( nParam < 2 )
      leto_SendAnswer( pUStru, szErr2, 4 );
   else
   {
      LPRDDNODE pRDDNode;
      HB_USHORT uiRddID;
      PHB_ITEM  pName1, pName2 = NULL;

      HB_GC_LOCKT();

      /* just to be safe -- there should be a filled pUstru->szDriver .. */
      if( ! *( pUStru->szDriver ) || strlen( pUStru->szDriver ) < 3 )
         pRDDNode = hb_rddFindNode( hb_rddDefaultDrv( NULL ), &uiRddID );
      else
         pRDDNode = hb_rddFindNode( pUStru->szDriver, &uiRddID );

      if( pRDDNode )
      {
         //HB_USHORT uiLen = ( HB_USHORT ) strlen( hb_setGetDefault() );

         if( ! strlen( hb_setGetPath() ) || strchr( szData, DEF_SEP ) || strchr( szData, DEF_SEP ) )
            leto_DataPath( szData, szBuf );
         else
            strcpy( szBuf, szData );
         pName1 = hb_itemPutC( NULL, szBuf );

         if( *pIFile )
         {
            if( ! strlen( hb_setGetPath() ) ||strchr( pIFile, DEF_SEP ) || strchr( pIFile, DEF_SEP ) )
               leto_DataPath( pIFile, szBuf );
            else
               strcpy( szBuf, pIFile );
            pName2 = hb_itemPutC( NULL, szBuf );
         }

         /* pName2 must be NULL ! if no index file is given ! */
         if( SELF_EXISTS( pRDDNode, pName1, pName2 ? pName2 : NULL, 0 ) == HB_SUCCESS )
         {
            PHB_FNAME pFilePath = hb_fsFNameSplit( szData );
            int       iTableStru;

            /* check if table is used, add extension if missing */
            if( ! pFilePath->szExtension )
            {
               char * szFileName = ( char * ) hb_xgrab( HB_PATH_MAX );
               char * szExt = ( char * ) hb_xgrab( HB_MAX_FILE_EXT + 1 );

               if( ! *( pUStru->szDriver ) || strlen( pUStru->szDriver ) < 3 )
                  leto_RddiGetValue( hb_rddDefaultDrv( NULL ), RDDI_TABLEEXT, szExt );
               else
                  leto_RddiGetValue( pUStru->szDriver, RDDI_TABLEEXT, szExt );
               sprintf( szFileName, "%s%s", szData, szExt );
               iTableStru = leto_FindTable( szFileName, NULL );
               hb_xfree( szExt );
               hb_xfree( szFileName );
            }
            else
               iTableStru = leto_FindTable( szData, NULL );

            if( iTableStru < 0 && SELF_DROP( pRDDNode, pName1, pName2 ? pName2 : NULL, 0 ) == HB_SUCCESS )
               leto_SendAnswer( pUStru, "+T;0;", 5 );
            else if( iTableStru >= 0 )
               leto_SendAnswer( pUStru, "+F;1;", 5 );
            else
               leto_SendAnswer( pUStru, "+F;0;", 5 );
            hb_xfree( pFilePath );
         }
         else
            leto_SendAnswer( pUStru, "+F;0;", 5 );

         hb_itemRelease( pName1 );
         if( pName2 )
            hb_itemRelease( pName2 );
      }
      else
         leto_SendAnswer( pUStru, szErr2, 4 );

      HB_GC_UNLOCKT();
   }
}

static void leto_Exists( PUSERSTRU pUStru, const char * szData )
{
   const char * pIFile;
   char         szBuf[ HB_PATH_MAX ];
   int          nParam = leto_GetParam( szData, &pIFile, NULL, NULL, NULL );

   if( nParam < 2 )
      leto_SendAnswer( pUStru, szErr2, 4 );
   else
   {
      LPRDDNODE    pRDDNode;
      HB_USHORT    uiRddID;
      const char * szDriver;
      PHB_ITEM     pName1, pName2 = NULL;

      // HB_GC_LOCKT();

      szDriver = hb_rddDefaultDrv( NULL );
      pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  /* find the RDDNODE */

      if( pRDDNode )
      {
         if( strchr( szData, DEF_SEP ) || strchr( szData, DEF_CH_SEP ) )
            leto_DataPath( szData, szBuf );
         else
            strcpy( szBuf, szData );
         pName1 = hb_itemPutC( NULL, szBuf );

         if( *pIFile )
         {
            if( strchr( pIFile, DEF_SEP ) || strchr( pIFile, DEF_CH_SEP ) )
               leto_DataPath( pIFile, szBuf );
            else
               strcpy( szBuf, pIFile );
            pName2 = hb_itemPutC( NULL, szBuf );
         }

         if( SELF_EXISTS( pRDDNode, pName1, pName2 ? pName2 : NULL, 0 ) == HB_SUCCESS )
            leto_SendAnswer( pUStru, "+T;0;", 5 );
         else
            leto_SendAnswer( pUStru, "+F;0;", 5 );

         hb_itemRelease( pName1 );
         if( pName2 )
            hb_itemRelease( pName2 );
      }
      else
         leto_SendAnswer( pUStru, szErr2, 4 );

      // HB_GC_UNLOCKT();
   }
}

static void leto_Rename( PUSERSTRU pUStru, const char * szData )
{
   const char * pIFile, * pNewFile;
   char         szBuf[ HB_PATH_MAX ];
   int          nParam = leto_GetParam( szData, &pIFile, &pNewFile, NULL, NULL );

   if( nParam < 3 )
      leto_SendAnswer( pUStru, szErr2, 4 );
   else
   {
      LPRDDNODE    pRDDNode;
      HB_USHORT    uiRddID;
      const char * szDriver;
      PHB_ITEM     pName1, pName2, pName3;

      HB_GC_LOCKT();

      szDriver = hb_rddDefaultDrv( NULL );
      pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  /* find the RDDNODE */

      if( pRDDNode )
      {
         leto_DataPath( szData, szBuf );
         pName1 = hb_itemPutC( NULL, szBuf );

         leto_DataPath( pIFile, szBuf );
         pName2 = hb_itemPutC( NULL, szBuf );

         leto_DataPath( pNewFile, szBuf );
         pName3 = hb_itemPutC( NULL, szBuf );

         if( SELF_RENAME( pRDDNode, pName1, pName2, pName3, 0 ) == HB_SUCCESS )
            leto_SendAnswer( pUStru, "+T;0;", 5 );
         else
            leto_SendAnswer( pUStru, "+F;0;", 5 );

         hb_itemRelease( pName1 );
         hb_itemRelease( pName2 );
         hb_itemRelease( pName3 );
      }
      else
         leto_SendAnswer( pUStru, szErr2, 4 );

      HB_GC_UNLOCKT();
   }
}

/* creates multiple DIR in sequence to get the wanted [sub-dir] path */
static HB_BOOL leto_DirMake( const char * szTarget, HB_BOOL bFilename )
{
   HB_USHORT uiLen = ( HB_USHORT ) strlen( szTarget );
   char      szPath[ HB_PATH_MAX ];
   char *    ptr, * ptr2, * ptrEnd, * ptrFull;
   HB_BOOL   bRet = HB_FALSE;

   strcpy( szPath, szTarget );
   leto_StrTran( szPath, DEF_CH_SEP, DEF_SEP, uiLen );
   ptr = szPath;
   if( ! strncmp( ptr, "mem:", 4 ) )
      ptr += 4;
   if( ( ptr2 = strchr( ptr, ':' ) ) != NULL )
      ptr = ptr2 + 1;
   if( bFilename && szPath[ uiLen - 1 ] != DEF_SEP )
   {
      if( ( ptrEnd = strrchr( szPath, DEF_SEP ) ) != NULL )
         *ptrEnd-- = '\0';
      else
         ptrEnd = ptr;
   }
   else
      ptrEnd = ptr + strlen( ptr ) - 1;

   ptrFull = ptrEnd;

   while( ! bRet && ( ptr2 = strrchr( ptrEnd, DEF_SEP ) ) != NULL )
   {
      if( ptrEnd - ptr2 == 0 && ptrEnd == ptrFull )  /* ending slash */
      {
         *ptrEnd-- = '\0';
         ptrFull = ptrEnd;
         continue;
      }
#if defined( __HARBOUR30__ )
      if( hb_fsDirExists( szPath ) )
#else
      if( hb_fileDirExists( szPath ) )
#endif
      {
         bRet = HB_TRUE;
         break;
      }
      *ptr2-- = '\0';
      ptrEnd = ptr2;
   }

   if( ! bRet || ptrEnd != ptrFull )
   {
      if( ptr2 != NULL )
         ptr = ptr2 + 1;
      strcpy( szPath, szTarget );
      leto_StrTran( szPath, DEF_CH_SEP, DEF_SEP, uiLen );
      bRet = HB_TRUE;

      while( bRet && ( ptr2 = strchr( ptr, DEF_SEP ) ) != NULL )
      {
         if( ptr2 - ptr == 0 )  /* leading slash */
         {
            ptr++;
            continue;
         }
         *ptr2 = '\0';
#if defined( __HARBOUR30__ )
         if( ! hb_fsDirExists( szPath ) )
            bRet = hb_fsMkDir( szPath );
#else
         if( ! hb_fileDirExists( szPath ) )
            bRet = hb_fileDirMake( szPath );
#endif
         *ptr2 = DEF_SEP;
         ptr = ptr2 + 1;
      }
   }

   return bRet;
}

static HB_BOOL leto_FilePathChk( const char * szPath )
{
   char * ptr;

   /* need the ':' for "mem:hb_memio file and want to allow *ONE* '..' for elch */
   if( ( ptr = strchr( szPath, ':' ) ) != NULL && ( ptr - szPath < 3 || ptr - szPath > 5 ) )  /* 'C:\... */
      return HB_FALSE;
   else if( ( ptr = strstr( szPath, ".." ) ) != NULL && strstr( ptr + 1, ".." ) )
      return HB_FALSE;

   return HB_TRUE;
}

static void leto_FileFunc( PUSERSTRU pUStru, const char * szData )
{
   const char * pSrcFile, * pp2, * pp3 = NULL;
   char         szData1[ 16 ];
   char         szFile[ HB_PATH_MAX ];
   int          nParam = leto_GetParam( szData, &pSrcFile, &pp2, &pp3, NULL );

   if( ! s_bFileFunc )
      leto_SendAnswer( pUStru, "+F;100;", 7 );
   else if( nParam < 2 || *( szData + 2 ) != '\0' || ! *pSrcFile || ! leto_FilePathChk( pSrcFile ) )
      leto_SendAnswer( pUStru, szErr2, 4 );
   else
   {
      HB_ULONG ulLen = 0;
      char *   pBuffer = NULL;
      HB_BOOL  bFreeBuf = HB_FALSE;

      leto_DataPath( pSrcFile, szFile );

      if( *szData == '0' )
      {
         switch( *( szData + 1 ) )
         {
            case '1':  /* fexists */
               szData1[ 0 ] = '+';
#if defined( __HARBOUR30__ )
               szData1[ 1 ] = ( hb_fsFileExists( szFile ) ) ? 'T' : 'F';
#else
               szData1[ 1 ] = ( hb_fileExists( szFile, NULL ) ) ? 'T' : 'F';
#endif
               szData1[ 2 ] = ';';
               szData1[ 3 ] = '\0';
               break;

            case '2':  /* ferase */
#if defined( __HARBOUR30__ )
               if( hb_fsDelete( szFile ) )
#else
               if( hb_fileDelete( szFile ) )
#endif
                  strcpy( szData1, "+T;0;" );
               else
                  sprintf( szData1, "+F;%d;", hb_fsError() );
               break;

            case '3':  /* frename */
               if( nParam < 3 )
                  strcpy( szData1, szErr2 );
               else
               {
                  char szDest[ HB_PATH_MAX ];

                  leto_DataPath( pp2, szDest );
#if defined( __HARBOUR30__ )
                  if( hb_fsRename( szFile, szDest ) )
#else
                  if( hb_fileRename( szFile, szDest ) )
#endif
                     strcpy( szData1, "+T;0;" );
                  else
                     sprintf( szData1, "+F;%d;", hb_fsError() );
               }
               break;

            case '4':  /* memoread */
               pBuffer = leto_memoread( szFile, &ulLen );
               if( pBuffer )
               {
                  ulLen = leto_CryptText( pUStru, pBuffer, ulLen );
                  hb_xfree( pBuffer );
                  pBuffer = ( char * ) pUStru->pBufCrypt;
               }
               else
                  strcpy( szData1, szErr2 );
               break;

            case '5':  /* MkDir */
               if( leto_DirMake( szFile, HB_FALSE ) )
                  strcpy( szData1, "+T;0;" );
               else
                  sprintf( szData1, "+F;%d;", hb_fsError() );
               break;

            case '6':  /* direxists */
               szData1[ 0 ] = '+';
#if defined( __HARBOUR30__ )
               szData1[ 1 ] = ( hb_fsDirExists( szFile ) ) ? 'T' : 'F';
#else
               szData1[ 1 ] = ( hb_fileDirExists( szFile ) ) ? 'T' : 'F';
#endif
               szData1[ 2 ] = ';';
               szData1[ 3 ] = '\0';
               break;

            case '7':  /* dirremove */
#if defined( __HARBOUR30__ )
               if( hb_fsRmDir( szFile ) )
#else
               if( hb_fileDirRemove( szFile ) )
#endif
                  strcpy( szData1, "+T;0;" );
               else
                  sprintf( szData1, "+F;%d;", hb_fsError() );
               break;

            default:
               strcpy( szData1, szErr2 );
               break;
         }
      }
      else if( *szData == '1' )
      {
         switch( *( szData + 1 ) )
         {
            case '0':  /* FileRead */
               if( nParam < 4 )
                  strcpy( szData1, szErr2 );
               else
               {
                  HB_ULONG ulStart;

                  ulStart = strtoul( pp2, NULL, 10 );
                  ulLen = strtoul( pp3, NULL, 10 );
                  if( ! ulLen )  /* whole file, determine size */
                  {

#if defined( __HARBOUR30__ )
                     leto_filesize( szFile, &ulLen );
#else
                     PHB_ITEM pArray = hb_fileDirectory( szFile, NULL );

                     if( pArray && HB_IS_ARRAY( pArray ) )
                     {
                        if( hb_arrayLen( pArray ) > 0 )
                        {
                           PHB_ITEM pFileArr = hb_itemArrayGet( pArray, 1 );

                           if( pFileArr && HB_IS_ARRAY( pFileArr ) && hb_arrayLen( pFileArr ) > 1 )
                              ulLen = hb_arrayGetNL( pFileArr, 2 );
                           if( pArray )
                              hb_itemRelease( pFileArr );
                        }
                        hb_itemRelease( pArray );
                     }
#endif
                  }
                  pBuffer = ( char * ) hb_xgrab( ulLen + 1 );
                  if( leto_fileread( szFile, pBuffer, ulStart, &ulLen ) && ulLen )
                  {
                     ulLen = leto_CryptText( pUStru, pBuffer, ulLen );
                     hb_xfree( pBuffer );
                     pBuffer = ( char * ) pUStru->pBufCrypt;
                  }
                  else
                  {
                     if( pBuffer )
                        hb_xfree( pBuffer );
                     pBuffer = NULL;
                     sprintf( szData1, "+F;%d;", hb_fsError() );
                  }
               }
               break;

            case '1':  /* FileSize */
            {
#if defined( __HARBOUR30__ )
               if( leto_filesize( szFile, &ulLen ) )  /* note: old method will open the file */
                  sprintf( szData1, "+T;%lu;", ulLen );
               else
                  sprintf( szData1, "+F;%d;", hb_fsError() );
#else
               PHB_ITEM pArray = hb_fileDirectory( szFile, NULL );

               ulLen = 0;
               if( pArray && HB_IS_ARRAY( pArray ) && hb_arrayLen( pArray ) > 0 )
               {
                  PHB_ITEM pFileArr = hb_itemArrayGet( pArray, 1 );

                  if( pFileArr && HB_IS_ARRAY( pFileArr ) && hb_arrayLen( pFileArr ) > 1 )
                     ulLen = hb_arrayGetNL( pFileArr, 2 );
                  if( pFileArr )
                     hb_itemRelease( pFileArr );
                  sprintf( szData1, "+T;%lu;", ulLen );
               }
               else
                  sprintf( szData1, "+F;%d;", hb_fsError() );
               hb_itemRelease( pArray );
#endif
               break;
            }

            case '2':  /* Directory */
               if( nParam < 3 )
                  strcpy( szData1, szErr2 );
               else
               {
                  PHB_ITEM pDir = NULL;
                  HB_SIZE  nSize;
                  char *   pParam;

                  /* leto special: add all file mask when last token in path is a directory */
                  ulLen = strlen( szFile );
                  if( ! ulLen || ( szFile[ ulLen - 1 ] != DEF_SEP ) )
                  {
                     HB_FATTR ulAttr = 0;

#if defined( __HARBOUR30__ )
                     if( ! ulLen || hb_fsGetAttr( szFile, &ulAttr ) )
#else
                     if( ! ulLen || hb_fileAttrGet( szFile, &ulAttr ) )
#endif
                     {
                        if( ! ulLen || ( ulAttr & HB_FA_DIRECTORY ) )
                        {
#if defined( HB_OS_WIN )
                           strcpy( szFile + ulLen + 1, "\\*.*" );
                           ulLen += 4;
#else
                           strcpy( szFile + ulLen, "/*" );
                           ulLen += 2;
#endif
                        }
                     }
                  }

                  if( ulLen )
#if defined( __HARBOUR30__ )
                     pDir = hb_fsDirectory( szFile, ( pp2 && *pp2 ) ? pp2 : "", HB_TRUE );
#else
                     pDir = hb_fileDirectory( szFile, pp2 );
#endif
                  if( ! pDir )
                     pDir = hb_itemArrayNew( 0 );

                  pParam = hb_itemSerialize( pDir, HB_SERIALIZE_NUMSIZE, &nSize );
                  ulLen = leto_CryptText( pUStru, pParam, nSize );
                  pBuffer = ( char * ) pUStru->pBufCrypt;
                  if( pDir )
                     hb_itemRelease( pDir );
                  if( pParam )
                     hb_xfree( pParam );
               }
               break;

            case '3':  /* memowrite */
               if( nParam < 4 )
                  strcpy( szData1, szErr2 );
               else
               {
                  const char * pBuf = pp3 + strlen( pp3 ) + 1;
                  ulLen = HB_GET_LE_UINT32( pBuf );
                  if( leto_memowrite( szFile, pBuf + 4, ulLen ) )
                     strcpy( szData1, "+T;0;" );
                  else
                     sprintf( szData1, "+F;%d;", hb_fsError() );
               }
               break;

            case '4':  /* FileWrite */
               if( nParam < 4 )
                  strcpy( szData1, szErr2 );
               else
               {
                  HB_ULONG     ulStart;
                  const char * pBuf;

                  ulStart = strtoul( pp2, NULL, 10 );
                  ulLen = strtoul( pp3, NULL, 10 );

                  pBuf = pp3 + strlen( pp3 ) + 1;
                  ulLen = HB_GET_LE_UINT32( pBuf );

                  if( leto_filewrite( szFile, pBuf + 4, ulStart, ulLen ) )
                     strcpy( szData1, "+T;0;" );
                  else
                     sprintf( szData1, "+F;%d;", hb_fsError() );
               }
               break;

            case '5':  /* FileAttr */
            {
               HB_FATTR     ulAttr;
               HB_BOOL      fResult = HB_TRUE;
               char         sBuffer[ 16 ];
               const char * ptr = NULL;

               if( nParam == 3 && pp2 && *pp2 )  /* SET/ CLEAR attribute */
               {
                  ulAttr = hb_fsAttrEncode( pp2 );
                  if( pp2[ 0 ] == '-' )
#if defined( __HARBOUR30__ )
                  {
                     HB_FATTR ulActiveAttr;

                     fResult = hb_fsGetAttr( szFile, &ulActiveAttr );
                     if( fResult )
                        ulAttr = ulActiveAttr & ~ulAttr;
                  }
                  fResult = hb_fsSetAttr( szFile, ulAttr );
#else
                  {
                     HB_FATTR ulActiveAttr;

                     fResult = hb_fileAttrGet( szFile, &ulActiveAttr );
                     if( fResult )
                        ulAttr = ulActiveAttr & ~ulAttr;
                  }
                  fResult = hb_fileAttrSet( szFile, ulAttr );
#endif
                  if( fResult )
                     ptr = pp2;
               }

               if( fResult )  /* (re-)request attributes, maybe after a above change */
               {
#if defined ( __HARBOUR30__ )
                  fResult = hb_fsGetAttr( szFile, &ulAttr );
#else
                  fResult = hb_fileAttrGet( szFile, &ulAttr );
#endif
                  if( fResult )
                     ptr = hb_fsAttrDecode( ulAttr, sBuffer );
               }
               if( fResult )
                  sprintf( szData1, "+T;%s;", ptr );
               else
                  sprintf( szData1, "+F;%d;", hb_fsError() );
               break;
            }

            case '7':  /* FileCopy */
            {
               HB_BOOL fResult;

               if( nParam < 3 || ! pp2 )
                  strcpy( szData1, szErr2 );
               else
               {
#if defined( __HARBOUR30__ )
                  fResult = hb_fsCopy( szFile, pp2 );
#else
                  fResult = hb_fileCopy( szFile, pp2 );
#endif
                  if( fResult )
                     strcpy( szData1, "+T;0;" );
                  else
                     sprintf( szData1, "+F;%d;", hb_fsError() );
               }
               break;
            }

            default:
               strcpy( szData1, szErr2 );
               break;
         }
      }
      else
         strcpy( szData1, szErr2 );

      if( pBuffer )
      {
         leto_SendAnswer( pUStru, pBuffer, ulLen );
         if( bFreeBuf )
            hb_xfree( pBuffer );
         if( pUStru->ulBufCryptLen > LETO_SENDRECV_BUFFSIZE )
         {
            if( s_iDebugMode > 10 )
               leto_wUsLog( pUStru, -1, "DEBUG crypt-buffer size %lu free-ed", pUStru->ulBufCryptLen );
            hb_xfree( pUStru->pBufCrypt );
            pUStru->pBufCrypt = NULL;
            pUStru->ulBufCryptLen = 0;
         }
      }
      else
         leto_SendAnswer( pUStru, szData1, strlen( szData1 ) );
   }
}

static _HB_INLINE_ HB_BOOL leto_IsServerLock( PUSERSTRU pUStru )  /* not really mt */
{
   HB_BOOL bRet;

   //HB_GC_LOCKU();
   bRet = ( s_iUserLock > 0 && s_iUserLock != pUStru->iUserStru );
   //HB_GC_UNLOCKU();

   return bRet;
}


/* need HB_GC_LOCKT(), for s_bNoSaveWA mode: scan through the tables for locked record or used order */
static HB_UINT leto_FindTableLockOrder( PTABLESTRU pTStru, HB_ULONG ulRecNo, const char * szBagName )
{
   const char * szTable = ( char * ) pTStru->szTable;
   HB_UINT      uiCrc = pTStru->uiCrc;
   PTABLESTRU   pTStruS = s_tables;
   HB_UINT      uiCurr = 0, uiRet = 0, ui;

   for( ui = 0; ui < s_uiTablesAlloc; pTStruS++, ui++ )
   {
      if( pTStruS->szTable )
      {
         if( pTStruS->uiCrc == uiCrc &&  /* hash value pre-check */
             pTStruS != pTStru && ! strcmp( ( const char * ) pTStruS->szTable, szTable ) )
         {
            if( ulRecNo )  /* search mode for locked record */
            {
               if( letoIsRecInListTS( &pTStruS->LocksList, ulRecNo ) )
               uiRet = ui + 1;
               break;
            }
            else  /* search mode for an used order by BagName */
            {
               HB_USHORT  uo = 0;
               PINDEXSTRU pIStru;

               while( uo < pTStruS->uiIndexCount && ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStruS->IndexList, uo ) ) != NULL )
               {
                  if( ! strcmp( pIStru->szBagName, szBagName ) && pIStru->bShared )
                  {
                     uiRet = ui + 1;
                     break;
                  }
                  uo++;
               }
               if( uiRet )
                  break;
            }
         }
         if( ++uiCurr >= s_uiTablesCurr )
            break;
      }
   }

   return uiRet;
}

static HB_BOOL leto_RecLock( PUSERSTRU pUStru, PAREASTRU pAStru, HB_ULONG ulRecNo, HB_BOOL bAppended, int iTimeOut )
{
   PTABLESTRU pTStru = pAStru->pTStru;

   if( leto_IsServerLock( pUStru ) )
      return HB_FALSE;

   if( pTStru->pGlobe->bLocked )  /* client have to release a file lock beforehand */
      return HB_FALSE;
   else if( ! pTStru->bShared )  /* exclusive opened */
      return HB_TRUE;

   if( s_bNoSaveWA && ! pTStru->bMemIO )  /* HbMemIO tables are without physical lock */
   {
      HB_BOOL bWasLocked = HB_FALSE;

      if( ! bAppended )  /* else physical lock is already set by append */
      {
         /* Simply lock the record with the standard RDD method */
         AREAP      pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
         DBLOCKINFO dbLockInfo;

         dbLockInfo.itmRecID = hb_itemPutNL( NULL, ulRecNo );
         dbLockInfo.uiMethod = DBLM_MULTIPLE;
         do
         {
            dbLockInfo.fResult = HB_FALSE;
            SELF_LOCK( pArea, &dbLockInfo );
            if( dbLockInfo.fResult )
               break;
            else if( iTimeOut > 21 )
            {
               hb_threadReleaseCPU();
               hb_threadReleaseCPU();
               iTimeOut -= 42;
               continue;
            }
            else
            {
               bWasLocked = HB_TRUE;
               break;
            }
         }
         while( iTimeOut > 0 );

         hb_itemRelease( dbLockInfo.itmRecID );
      }

      /* note the pTStru->LocksList, not unused pAStru */
      if( ! bWasLocked )
         letoAddRecToListTS( &pTStru->LocksList, ulRecNo, HB_TRUE );

      return ! bWasLocked;
   }
   else
   {
      /* Add a record number (ulRecNo) to the area's and table's
       * locks lists (pAStru->pLocksPos)
       * Firstly, scanning the table's locks list, with immediate add if no entry found
       * No iTimeOut if that fails, else we would block other connections from unlocking */
      HB_BOOL bWasLocked = letoAddRecToListTS( &pTStru->LocksList, ulRecNo, HB_TRUE );

      if( ! bWasLocked )
      {
         /* the record isn't locked by LetoDbf, now try to physical lock it!
          * as in s_bShareTables mode non-LetoDbf users can disturb */
         if( s_bShareTables && pTStru->bShared && ! pTStru->bMemIO )
         {
            if( ! bAppended )  /* else physical lock is already set by append */
            {
               /* in ShareTables mode, also set the real lock with the standard RDD method */
               AREAP      pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
               DBLOCKINFO dbLockInfo;

               dbLockInfo.itmRecID = hb_itemPutNL( NULL, ulRecNo );
               dbLockInfo.uiMethod = DBLM_MULTIPLE;
               dbLockInfo.fResult = HB_FALSE;
               SELF_LOCK( pArea, &dbLockInfo );
               bWasLocked = ! dbLockInfo.fResult;
               hb_itemRelease( dbLockInfo.itmRecID );

               if( bWasLocked )  /* physical lock failed, remove again from list */
               {
                  letoDelRecFromListTS( &pTStru->LocksList, ulRecNo );
                  return HB_FALSE;
               }
            }
         }
      }

      /* If at this point bWasLocked is HB_TRUE, record was already locked,
       * now we need to determine, if this lock is by current user/area,
       * so secondly scan *with immediate add* the user area's locks list */
      if( letoAddRecToList( &pAStru->LocksList, ulRecNo, ! bWasLocked ) )
         return HB_TRUE;  /* it was ourself record lock */

      /* The record is locked by another user/area, so we return an error */
      return ! bWasLocked;
   }
}

static void leto_RecUnlock( PAREASTRU pAStru, HB_ULONG ulRecNo )
{
   PTABLESTRU pTStru = pAStru->pTStru;

   if( s_bNoSaveWA && ! pTStru->bMemIO )
   {
      /* do nothing for exclusive opened or file locked tables */
      /* Simply unlock the record with the standard RDD method */
      if( pAStru->pTStru->bShared || ! pAStru->bLocked )
      {
         AREAP    pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
         PHB_ITEM pItem = hb_itemPutNL( NULL, ulRecNo );

         if( pArea )
            SELF_UNLOCK( pArea, pItem );
         hb_itemRelease( pItem );

         letoDelRecFromListTS( &pTStru->LocksList, ulRecNo );
      }
   }
   else
   {
      /* Firstly, scanning the area's locks list */
      if( letoDelRecFromList( &pAStru->LocksList, ulRecNo ) )  /* The record was locked, so we unlock it */
      {
         /* Secondly, delete entry from table's locks list - !! mutex unlock list below !! */
         /* if we work in ShareTables mode, unlock with the standard RDD method */
         if( s_bShareTables && pTStru->bShared && ! pTStru->bMemIO )
         {
            AREAP    pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
            PHB_ITEM pItem = hb_itemPutNL( NULL, ulRecNo );

            if( pArea )
               SELF_UNLOCK( pArea, pItem );
            else
               leto_writelog( NULL, 0, "ERROR leto_RecUnlock() no workarea " );
            hb_itemRelease( pItem );
         }

         letoDelRecFromListTS( &pTStru->LocksList, ulRecNo );
      }
   }
}

static HB_BOOL leto_IsAnyLocked( void )
{
   PTABLESTRU pTStru;
   HB_UINT    uiCount = 0, ui;
   HB_BOOL    bRet = HB_FALSE;

   HB_GC_LOCKT();

   for( ui = 0, pTStru = s_tables; ui < s_uiTablesAlloc; pTStru++, ui++ )
   {
      if( pTStru->szTable && ( pTStru->pGlobe->bLocked || ( pTStru->LocksList.pItem ) ) && pTStru->bShared )
      {
         bRet = HB_TRUE;
         break;
      }
      if( ++uiCount >= s_uiTablesCurr )
         break;
   }

   HB_GC_UNLOCKT();

   return bRet;
}

/* need HB_GC_LOCKU() */
static void leto_TryLock( int iSecs, int iUserStru )
{
   s_iUserLock = iUserStru;
   while( iSecs > 0 && leto_IsAnyLocked() )
   {
      hb_idleSleep( 1.0 );
      iSecs--;
   }

   if( leto_IsAnyLocked() )
      s_iUserLock = -1;
   else if( s_bHardCommit )
      hb_rddFlushAll();
}

HB_BOOL leto_ServerLock( PUSERSTRU pUStru, HB_BOOL bLock, int iSecs )
{
   HB_BOOL bRet = ( s_iUserLock >= 0 );

   if( pUStru )
   {
      HB_GC_LOCKU();
      if( bLock )
      {
         if( s_iUserLock < 0 )
            leto_TryLock( iSecs, pUStru->iUserStru );
      }
      else  /* unlock if we are the locking one */
      {
         if( s_iUserLock >= 0 && ! leto_IsServerLock( pUStru ) )  /* ToDO: admin rights -- lock user can unlock */
            s_iUserLock = -1;
      }
      bRet = ( s_iUserLock >= 0 );
      HB_GC_UNLOCKU();
   }

   return bRet;
}

static HB_BOOL leto_dbfCopy( PHB_FILE pSrcFile, const char * pszDest, HB_NHANDLE hSrcFile )
{
   HB_ERRCODE errCode = hb_fsError();
   HB_BOOL    bRetVal = HB_FALSE;
   PHB_FILE   pDestFile;
#if defined( HB_OS_LINUX ) && defined( USE_SPLICE )
   HB_FHANDLE hCopyPipe[ 2 ] = { FS_ERROR, FS_ERROR };
#endif

   if( pSrcFile )
      hSrcFile = hb_fileHandle( pSrcFile );
   if( hSrcFile )
   {
      if( ( pDestFile = hb_fileExtOpen( pszDest, NULL, FXO_TRUNCATE | FO_READWRITE | FO_EXCLUSIVE | FXO_SHARELOCK, NULL, NULL ) ) != NULL )
      {
#if ! ( defined( HB_OS_LINUX ) && defined( USE_SPLICE ) )
         HB_SIZE nBytesRead;
#endif
         HB_SIZE nBytesMax;
         HB_SIZE nBufferSize = 65536;
         void *  pBuffer;

         if( ( nBytesMax = ( HB_SIZE ) hb_fsSeekLarge( hSrcFile, 0, FS_END ) ) > 5 * nBufferSize )
            nBufferSize = 2 * nBufferSize;
         else if( nBytesMax < 65536 )
            nBufferSize = nBytesMax;

#if defined( HB_OS_LINUX ) && defined( USE_SPLICE )
         if( nBufferSize > 65536 && ! hb_fsPipeCreate( hCopyPipe ) )
         {
            hCopyPipe[ 0 ] = FS_ERROR;
            hCopyPipe[ 1 ] = FS_ERROR;
            pBuffer = NULL;
         }
         else
#endif
            pBuffer = hb_xgrab( nBufferSize + 1 );

         hb_fsSeekLarge( hSrcFile, 0, FS_SET );
         for( ;; )
         {
#if defined( HB_OS_LINUX ) && defined( USE_SPLICE )
            if( hCopyPipe[ 1 ] != FS_ERROR )
            {
               int iSplice, iSpliced;

               iSplice = ssplice( hSrcFile, NULL, hCopyPipe[ 1 ], NULL, 65536, 0 );  /* 65536 == nBytesMax */
               if( iSplice < 0 )
               {
                  errCode = hb_fsError();
                  bRetVal = HB_FALSE;
                  break;
               }
               else if( ! iSplice )
                  break;
               iSpliced = ssplice( hCopyPipe[ 0 ], NULL, hb_fileHandle( pDestFile ), NULL, iSplice, SPLICE_F_MOVE );
               if( iSpliced < 0 )
               {
                  errCode = hb_fsError();
                  bRetVal = HB_FALSE;
                  break;
               }
               //nBytesMax -= iSpliced;
               continue;
            }
#else
            if( ( nBytesRead = hb_fsReadLarge( hSrcFile, pBuffer, nBufferSize ) ) > 0 )
            {
               if( nBytesRead != hb_fileWrite( pDestFile, pBuffer, nBytesRead, -1 ) )
               {
                  errCode = hb_fsError();
                  bRetVal = HB_FALSE;
                  break;
               }
            }
            else
            {
               errCode = hb_fsError();
               bRetVal = ( errCode == 0 );
               break;
            }
#endif
         }

         if( pBuffer )
            hb_xfree( pBuffer );

         hb_fileClose( pDestFile );
      }
      else
      {
         errCode = hb_fsError();
         bRetVal = HB_FALSE;
      }
   }
   else
   {
      errCode = hb_fsError();
      bRetVal = HB_FALSE;
   }

   hb_fsSetFError( errCode );

   return bRetVal;
}

/* NOT READY -- work in progress -- maybe ;-) */
// ToFix remove pTStru->bNotDetached, find workaround by scanning users pAStru
/* szTargetDir is expected to end with a path seperator */
HB_BOOL leto_BackupTable( PUSERSTRU pUStru, const char * szTargetDir, HB_BOOL bDryRun )
{
   HB_BOOL  bOk = HB_FALSE;

   if( szTargetDir && *szTargetDir )
   {
      PHB_ITEM   pTblsActive = hb_itemArrayNew( 0 );
      HB_BOOL    bDeleted = hb_setGetDeleted();
      PHB_ITEM   pTableFiles = hb_itemArrayNew( 0 );
      PHB_ITEM   pIndexFiles = hb_itemArrayNew( 0 );
      PTABLESTRU pTStru;
      HB_UINT    uiCount = 1;

      if( bDeleted )
      {
         PHB_ITEM pItem = hb_itemNew( NULL );

         hb_itemPutL( pItem, HB_FALSE );
         hb_setSetItem( HB_SET_DELETED, pItem );
         hb_itemRelease( pItem );
      }

      HB_GC_LOCKT();  // ToFix  error handling -- if error this keep active

      pTStru = s_tables;
      while( pTStru )
      {
         PHB_ITEM pFileName = NULL;
         PHB_ITEM pOneActive;

         pFileName = hb_itemPutC( pFileName, ( const char * ) pTStru->szTable );
         if( ! pTStru->bMemIO && hb_arrayScan( pTableFiles, pFileName, NULL, NULL, HB_TRUE ) < 1 )
         {
            PTABLESTRU pTStruI;
            HB_UINT    uiCountI;

            hb_arrayAdd( pTableFiles, pFileName );
            pOneActive = hb_itemArrayNew( 7 );
            hb_arraySetC( pOneActive, 1, ( const char * ) pTStru->szTable );
            hb_arraySetC( pOneActive, 2, ( const char * ) pTStru->szDriver );
            hb_arraySetNL( pOneActive, 3, pTStru->ulAreaID );
            hb_arraySetL( pOneActive, 4, pTStru->bShared );
            hb_arraySetL( pOneActive, 5, pTStru->bWhoCares );

            /* search opend index */
            uiCountI = uiCount - 1;
            pTStruI = s_tables + uiCountI;
            while( pTStruI && uiCountI < s_uiTablesCurr )
            {
               if( pTStruI->szTable && pTStruI->uiIndexCount &&
                   ! strcmp( ( const char * ) pTStruI->szTable, ( const char * ) pTStru->szTable ) )
               {
                  HB_USHORT ui = 0;
                  PINDEXSTRU pIStru;

                  while( ui < pTStruI->uiIndexCount &&
                         ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStruI->IndexList, ui ) ) != NULL )
                  {
                     if( pIStru->szBagName )
                     {
                        PHB_ITEM pIndexName = NULL;

                        pIndexName = hb_itemPutC( pIndexName, ( const char * ) pIStru->szBagName );
                        if( hb_arrayScan( pIndexFiles, pIndexName, NULL, NULL, HB_TRUE ) < 1 )
                           hb_arrayAdd( pIndexFiles, pIndexName );
                        hb_itemRelease( pIndexName );
                     }
                     // ToDo ? else error
                     ui++;
                  }
               }
               if( pTStruI->szTable )
                  uiCountI++;
               pTStruI++;
            }

            /* we have a problem with exclusive opened in NoSaveWA mode */
            if( ! bDryRun && ( pTStru->bShared || ( ! pTStru->bShared && ! pTStru->bWhoCares ) ) )
            {
               HB_ERRCODE errcode;
               HB_ULONG   ulAreaID = 0;  // ToDo convert to HB_USHORT
               DBFAREAP   pArea = NULL;
               char       szSource[ HB_PATH_MAX ];
               HB_USHORT  uiLen = s_uiDataPathLen;
               HB_USHORT  uiLen2 = ( HB_USHORT ) strlen( ( const char * ) pTStru->szTable );
               HB_BOOL    bRequested = HB_FALSE;

               if( uiLen > HB_PATH_MAX - 1 - uiLen2 )  // Houston
                  uiLen = HB_PATH_MAX - 1 - uiLen2;
               memcpy( szSource, s_pDataPath, uiLen );
               if( szSource[ uiLen - 1 ] != '/' && szSource[ uiLen - 1 ] != '\\' )
               {
                  szSource[ uiLen ] = DEF_SEP;
                  uiLen++;
                  szSource[ uiLen ] = '\0';
               }
               memcpy( &szSource[ uiLen ], pTStru->szTable, uiLen2 );
               szSource[ uiLen + uiLen2 ] = '\0';

               if( ! pTStru->bWhoCares || ( ! s_bNoSaveWA && ! s_bShareTables ) )
               {
                  if( leto_SelectArea( pUStru, pTStru->ulAreaID ) )
                  {
                     bRequested = HB_TRUE;
                     ulAreaID = pTStru->ulAreaID;
                     pArea = ( DBFAREAP ) hb_rddGetCurrentWorkAreaPointer();
                  }
               }
               else
               {
                  errcode = hb_rddOpenTable( ( const char * ) szSource, pTStru->szDriver, ( HB_USHORT ) ulAreaID,
                                             "SNAPSHOT", HB_TRUE, HB_FALSE, pUStru->cdpage->id, 0, NULL, NULL );
                  if( errcode == HB_SUCCESS )
                  {
                     ulAreaID = hb_rddGetCurrentWorkAreaNumber();
                     pArea = ( DBFAREAP ) hb_rddGetCurrentWorkAreaPointer();
                  }
                  else
                  {
                     leto_wUsLog( pUStru, -1, "DEBUG open table %s for copy VIA %s failed in %lu; %d",
                                  szSource, pTStru->szDriver, ulAreaID, hb_fsError() );
                     pArea = NULL;
                  }
               }

               if( pArea )
               {
                  char szTarget[ HB_PATH_MAX ];

                  /* alternative: pArea->szDataFileName */
                  uiLen = ( HB_USHORT ) strlen( szTargetDir );
                  if( uiLen > HB_PATH_MAX - 1 - uiLen2 )  // Houston
                     uiLen = HB_PATH_MAX - 1 -  uiLen2;
                  memcpy( szTarget, szTargetDir, uiLen );
                  if( szTarget[ uiLen - 1 ] != '/' && szTarget[ uiLen - 1 ] != '\\' )
                  {
                     szTarget[ uiLen ] = DEF_SEP;
                     uiLen++;
                  }
                  memcpy( szTarget + uiLen, pTStru->szTable, uiLen2 );
                  szTarget[ uiLen + uiLen2 ] = '\0';

                  if( leto_DirMake( szTarget, HB_TRUE ) && leto_dbfCopy( pArea->pDataFile, szTarget, 0 ) )
                  {
                     HB_FATTR  ulAttr;
                     HB_USHORT ui;

                     if( hb_fileAttrGet( pArea->pDataFile, &ulAttr ) )
                        hb_fileAttrSet( szTarget, ulAttr );
                     errcode = HB_SUCCESS;
                     if( pArea->fHasMemo && pArea->pMemoFile != NULL )
                     {
                        char * ptr = strrchr( szTarget, '.' );
                        char * ptr2 = strrchr( pArea->szMemoFileName, '.' );

                        if( ptr != NULL && ptr2 != 0 )
                        {
                           strcpy( ptr, ptr2 );
                           if( ! leto_dbfCopy( pArea->pMemoFile, szTarget, 0 ) )
                              errcode = HB_FAILURE;
                           else
                              hb_arraySetC( pOneActive, 6, ( const char * ) szTarget );
                        }
                        else
                           errcode = HB_FAILURE;
                        if( errcode == HB_SUCCESS )
                        {
                           if( hb_fileAttrGet( pArea->szMemoFileName, &ulAttr ) )
                              hb_fileAttrSet( szTarget, ulAttr );
                        }
                     }

                     /* tha was dbf + dbt, now indexes */
                     if( ( ui = ( HB_USHORT ) hb_arrayLen( pIndexFiles ) ) > 0 )
                     {
                        char szIndexSource[ HB_PATH_MAX ];
                        char szIndexTarget[ HB_PATH_MAX ];
                        char szIndexPathSrc[ HB_PATH_MAX ];
                        char szIndexPathTrg[ HB_PATH_MAX ];
                        DBORDERINFO pOrderInfo;
                        AREAP       pTmpArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

                        memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
                        uiLen = s_uiDataPathLen;
                        strcpy( szIndexPathSrc, s_pDataPath );
                        if( szIndexPathSrc[ uiLen - 1 ] != '/' && szIndexPathSrc[ uiLen - 1 ] != '\\' )
                        {
                           szIndexPathSrc[ uiLen ] = DEF_SEP;
                           szIndexPathSrc[ uiLen + 1 ] = '\0';
                        }

                        uiLen = ( HB_USHORT ) strlen( szTargetDir );
                        strcpy( szIndexPathTrg, szTargetDir );
                        if( szIndexPathTrg[ uiLen - 1 ] != '/' && szIndexPathTrg[ uiLen - 1 ] != '\\' )
                        {
                           szIndexPathTrg[ uiLen ] = DEF_SEP;
                           szIndexPathTrg[ uiLen + 1 ] = '\0';
                        }

                        while( ui > 0 )
                        {
                           sprintf( szIndexSource, "%s%s", szIndexPathSrc, hb_arrayGetCPtr( pIndexFiles, ui ) );
                           sprintf( szIndexTarget, "%s%s", szIndexPathTrg, hb_arrayGetCPtr( pIndexFiles, ui ) );

                           /* may fail of ! s_bShareTables or ! bShared ==> grab file handle .. */
                           if( ! hb_fileCopy( szIndexSource, szIndexTarget ) )
                           {

                              hb_itemPutNI( pOrderInfo.itmOrder, ui );
                              hb_itemPutC( pOrderInfo.itmResult, 0 );
                              errcode = SELF_ORDINFO( pTmpArea, DBOI_FILEHANDLE, &pOrderInfo );
                              if( errcode == HB_SUCCESS &&
                                  ! leto_dbfCopy( NULL, szIndexTarget, ( HB_NHANDLE ) hb_itemGetNInt( pOrderInfo.itmResult ) ) )
                                 errcode = HB_FAILURE;
                           }
                           else
                              errcode = HB_SUCCESS;
                           if( errcode == HB_SUCCESS )
                           {
                              if( hb_fileAttrGet( szIndexSource, &ulAttr ) )
                                 hb_fileAttrSet( szIndexTarget, ulAttr );
                           }
                           ui--;
                        }
                        if( pOrderInfo.itmOrder )
                        {
                           hb_itemRelease( pOrderInfo.itmOrder );
                           hb_itemRelease( pOrderInfo.itmResult );
                        }

                        hb_arraySet( pOneActive, 7, pIndexFiles );
                        hb_itemRelease( pIndexFiles );
                        pIndexFiles = hb_itemArrayNew( 0 );
                     }
                  }

                  if( bRequested )
                  {
                     if( pUStru->ulUdfAreaID != ulAreaID )  /* free not me myself */
                        leto_FreeArea( pUStru, ulAreaID, HB_FALSE );
                  }
                  else
                     hb_rddReleaseCurrentArea();
               }
            }   /* bShared */

            hb_arrayAdd( pTblsActive, pOneActive );
            hb_itemRelease( pOneActive );
            hb_itemRelease( pFileName );
         }
         else
            hb_itemRelease( pFileName );

         if( uiCount++ >= s_uiTablesCurr )
            break;
         pTStru++;
      }

      if( bDeleted )
      {
         PHB_ITEM pItem = hb_itemNew( NULL );

         hb_itemPutL( pItem, HB_TRUE );
         hb_setSetItem( HB_SET_DELETED, pItem );
         hb_itemRelease( pItem );
      }
      if( pTableFiles )
         hb_itemRelease( pTableFiles );
      if( pIndexFiles )
         hb_itemRelease( pIndexFiles );

      hb_itemReturnRelease( pTblsActive );

      HB_GC_UNLOCKT();
   }

   return bOk;
}


HB_FUNC( LETO_BACKUPTABLES )
{
   PUSERSTRU pUStru = letoGetUStru();

   leto_BackupTable( pUStru, hb_parc( 1 ), hb_parl( 2 ) );
}

/* leto_udf() */
HB_FUNC( LETO_TABLELOCK )
{
   HB_BOOL   bRet = HB_FALSE;
   PUSERSTRU pUStru = letoGetUStru();

   if( pUStru->pCurAStru && ! leto_IsServerLock( pUStru ) )
   {
      PAREASTRU pAStru = pUStru->pCurAStru;
      HB_ULONG  ulSecs = s_bNoSaveWA && HB_ISNUM( 1 ) ? ( HB_ULONG ) ( hb_parnd( 2 ) * 1000 ) : 0;

      if( pAStru )
      {
         bRet = leto_TableLock( pAStru, ( int ) ulSecs );
         if( bRet )
            pAStru->pTStru->ulFlags = ( HB_ULONG ) pUStru->iUserStru;  /* still not used, as mark done by UDF */
      }
   }
   hb_retl( bRet );
}

/* leto_udf() */
HB_FUNC( LETO_TABLEUNLOCK )
{
   PUSERSTRU pUStru = letoGetUStru();

   if( pUStru->pCurAStru )
   {
      PAREASTRU pAStru = pUStru->pCurAStru;

      if( pAStru )
      {
         leto_TableUnlock( pAStru, HB_FALSE, NULL );  /* also record locks */
         pAStru->pTStru->ulFlags = 0;
      }
   }
   hb_ret();
}

/* leto_udf() */
HB_FUNC( LETO_RECLOCK )
{
   PUSERSTRU pUStru = letoGetUStru();

   if( pUStru->pCurAStru && ! leto_IsServerLock( pUStru ) )
   {
      PAREASTRU pAStru = pUStru->pCurAStru;
      HB_ULONG  ulRecNo = 0;

      if( HB_ISNUM( 1 ) )
         ulRecNo = hb_parnl( 1 );
      else
      {
         AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

         if( pArea )
            SELF_RECNO( pArea, &ulRecNo );
      }
      if( ulRecNo )
      {
         HB_BOOL bAppend = HB_ISLOG( 3 ) ? hb_parl( 3 ) : HB_FALSE;

         if( bAppend && ! pAStru-> bLocked )
            hb_retl( leto_RecLock( pUStru, pAStru, ulRecNo, bAppend,
                     HB_ISNUM( 2 ) ? ( int ) ( hb_parnd( 2 ) * 1000 ) : 0 ) );
         else
            hb_retl( HB_TRUE );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/* leto_udf() */
HB_FUNC( LETO_RECLOCKLIST )
{
   PUSERSTRU pUStru = letoGetUStru();

   if( pUStru->pCurAStru && ! pUStru->pCurAStru->bLocked && HB_ISARRAY( 1 ) && ! leto_IsServerLock( pUStru ) )
   {
      PAREASTRU pAStru = pUStru->pCurAStru;
      PHB_ITEM  pArray = hb_param( 1, HB_IT_ARRAY );
      HB_SIZE   nSize = hb_arrayLen( pArray ), n, n2;
      int       iTimeOut = HB_ISNUM( 2 ) ? ( int ) ( hb_parnd( 2 ) * 1000 ) : 0 ;
      HB_BOOL   bLocked = HB_TRUE;

      for( n = 1; n <= nSize; ++n )
      {
         if( ! leto_RecLock( pUStru, pAStru, hb_arrayGetNL( pArray, n ), HB_FALSE, iTimeOut ) )
         {
            for( n2 = 1; n2 < n; ++n2 )
               leto_RecUnlock( pAStru, hb_arrayGetNL( pArray, n2 ) );
            bLocked = HB_FALSE;
            break;
         }
      }
      hb_retl( bLocked );
   }
   else
      hb_retl( HB_FALSE );
}

/* leto_udf() */
HB_FUNC( LETO_RECUNLOCK )
{
   PUSERSTRU pUStru = letoGetUStru();

   if( pUStru->pCurAStru )
   {
      HB_ULONG ulRecNo = 0;

      if( HB_ISNUM( 1 ) )
         ulRecNo = hb_parnl( 1 );
      else
      {
         AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

         if( pArea )
            SELF_RECNO( pArea, &ulRecNo );
      }

      if( ulRecNo )
         leto_RecUnlock( pUStru->pCurAStru, ulRecNo );
   }
   hb_ret();
}

/* result freed by caller */
static char * leto_recWithAlloc( AREAP pArea, PUSERSTRU pUStru, PAREASTRU pAStru, HB_ULONG * pulLen )
{
   HB_ULONG ulRecLen = leto_recLen( pAStru->pTStru );
   char *   szData = ulRecLen ? ( char * ) hb_xgrab( ulRecLen + 1 ) : NULL;

   if( szData )
   {
      *pulLen = leto_rec( pUStru, pAStru, pArea, szData + 1, NULL );
      if( *pulLen )
      {
         szData[ 0 ] = '+';
         *pulLen += 1;  /* Length is incremented because of adding '+' */
      }
      else
      {
         hb_xfree( szData );
         szData = NULL;
      }
   }
   return szData;
}

static void leto_SendRecWithOk( PUSERSTRU pUStru, PAREASTRU pAStru, HB_ULONG ulRecNo )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( leto_GotoIf( pArea, ulRecNo ) == HB_SUCCESS )
   {
      HB_ULONG ulLen;
      char * szData = leto_recWithAlloc( pArea, pUStru, pAStru, &ulLen );

      if( szData )
      {
         leto_SendAnswer( pUStru, szData, ulLen );
         hb_xfree( szData );
      }
      else
         leto_SendAnswer( pUStru, szErr1, 4 );
   }
   else
   {
      char szData[ 5 ];

      strcpy( szData, szErr1 );
      leto_SendAnswer( pUStru, szData, 4 );
   }
}

static void leto_Lock( PUSERSTRU pUStru, const char * szData )
{
   PAREASTRU pAStru = pUStru->pCurAStru;
   char *    pTimeOut = NULL;
   HB_ULONG  ulRecNo = strtoul( szData + 2, &pTimeOut, 10 );

   switch( *szData )
   {
      case 'r':  /* reclock */
         if( ulRecNo && leto_RecLock( pUStru, pAStru, ulRecNo, HB_FALSE,
                                      ( s_bNoSaveWA && pTimeOut && *pTimeOut++ ) ? atoi( pTimeOut ) : 0 ) )
         {
            if( leto_Updated( pAStru ) )
               leto_SendRecWithOk( pUStru, pAStru, ulRecNo );
            else
               leto_SendAnswer( pUStru, szOk, 4 );
         }
         else
         {
            leto_SendAnswer( pUStru, szErr4, 4 );
            if( ! ulRecNo )
               leto_wUsLog( pUStru, 0, "ERROR leto_Lock() missing RecNo! for Rlock" );
         }
         break;

      case 'f':  /* filelock */
         if( leto_IsServerLock( pUStru ) )
         {
            leto_SendAnswer( pUStru, szErr2, 4 );
            break;
         }
         if( ! leto_TableLock( pAStru, ( s_bNoSaveWA && pTimeOut && *pTimeOut++ ) ? atoi( pTimeOut ) : 0 ) )
         {
            leto_SendAnswer( pUStru, szErr4, 4 );
            break;
         }

         if( ulRecNo && leto_Updated( pAStru ) )
            leto_SendRecWithOk( pUStru, pAStru, ulRecNo );
         else
            leto_SendAnswer( pUStru, szOk, 4 );
         break;

      default:  /* should never happen */
         leto_SendAnswer( pUStru, szErr1, 4 );
   }
}

static void leto_Unlock( PUSERSTRU pUStru, const char * szData )
{
   HB_ULONG ulRecNo = 0;

   if( *szData == 'r' && ( ulRecNo = strtoul( szData + 2, NULL, 10 ) ) == 0 )
      leto_SendAnswer2( pUStru, szErr2, 4, HB_FALSE, 1000 );
   else
   {
      int iRes = 0;

      switch( *szData )
      {
         case 'r':  /* Delete record from the area/ table locks lists */
            leto_RecUnlock( pUStru->pCurAStru, ulRecNo );
            leto_SendAnswer2( pUStru, szOk, 4, ! iRes, iRes );
            break;

         case 'f':  /* Unlock table */
            //hb_xvmSeqBegin();
            iRes = leto_TableUnlock( pUStru->pCurAStru, HB_FALSE, NULL ) ? 0 : 1021;  /* also record locks */
            if( ! iRes )
               leto_SendAnswer2( pUStru, szOk, 4, ! iRes, iRes );
            else
               leto_SendAnswer2( pUStru, szErr4, 4, HB_FALSE, 1021 );
            break;

         default:
            leto_SendAnswer2( pUStru, szErr2, 4, HB_FALSE, 1000 );
      }
   }
}

static int leto_UpdateRecord( PUSERSTRU pUStru, const char * szData, HB_BOOL bAppend, HB_ULONG * pRecNo, TRANSACTSTRU * pTA, AREAP pArea )
{
   PAREASTRU    pAStru = pUStru->pCurAStru;
   int          iRes = 0;
   const char * pUpd, * pDelRecall;
   int          nParam = leto_GetParam( szData, &pUpd, &pDelRecall, NULL, NULL );

   if( nParam < 3 )
      iRes = 2;
   else
   {
      HB_ULONG  ulRecNo = strtoul( szData, NULL, 10 );  /* record number or bUnlockAll ( for append ) */
      HB_USHORT uiUpd = ( HB_USHORT ) atoi( pUpd );  /* number of updated fields */

      if( ! pArea )
         pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

      if( ! pTA )
         hb_xvmSeqBegin();

      if( bAppend )
      {
         if( ( ! pAStru->bLocked && pAStru->pTStru->pGlobe->bLocked ) || leto_IsServerLock( pUStru ) )
         {
            if( ! pTA )
               hb_xvmSeqEnd();
            return 201;
         }

         if( pTA )
         {
            pTA->bAppend = HB_TRUE;
            pTA->bLockable = ! pAStru->pTStru->bMemIO &&
                             ( ( ( ! s_bNoSaveWA ) ? s_bShareTables : HB_TRUE ) ?
                             ( ! pAStru->pTStru->bShared || ! pAStru->bLocked ) : HB_FALSE );
         }
         else
         {
            if( ulRecNo )  /* aka bUnlockAll */
               leto_TableUnlock( pAStru, HB_TRUE, pArea );  /* only record locks, leave a file lock */

            hb_rddSetNetErr( HB_FALSE );
            if( SELF_APPEND( pArea, HB_FALSE ) == HB_SUCCESS )  /* unlocks NOT other records */
            {
               ulRecNo = ( ( DBFAREAP ) pArea )->ulRecNo;  /* SELF_RECNO( pArea, &ulRecNo ); */
               // pAStru->pTStru->pGlobe->ulRecCount++;
               if( pRecNo )
                  *pRecNo = ulRecNo;
               if( ! pAStru->bLocked && ! leto_RecLock( pUStru, pAStru, ulRecNo, bAppend, 0 ) )
               {
                  /* this commonly can not happen, above append was succcessful,
                   * and we only register record-number in list */
                  iRes = 101;
                  leto_wUsLog( pUStru, 0, "DEBUG leto_UpdateRecord() record lock failed !?" );
               }
            }
            else
               iRes = 1;
         }
      }
      else
      {
#if 0  // ToDo verify my else is fine, then remove ultimately
         if( ( ! pTA || ulRecNo ) && pAStru->pTStru->bShared &&
            ! ( pAStru->bLocked || leto_IsRecLocked( pAStru, ulRecNo ) ) )
#else
         if( ulRecNo && ! pTA && pAStru->pTStru->bShared &&
             ! ( pAStru->bLocked || leto_IsRecLocked( pAStru, ulRecNo ) ) )
#endif
         {
            /*  The table is opened in shared mode, but the record/ table is not locked */
            if( ! pTA )
               hb_xvmSeqEnd();
            return 201;
         }
         else if( ! pTA )
            leto_GotoForce( pArea, ulRecNo );
         else
            pTA->ulRecNo = ulRecNo;
      }

      if( *pDelRecall != '0' )  /* bDelete || bRecall */
      {
         HB_BOOL bDelete = *pDelRecall == '1' ? HB_TRUE : HB_FALSE;
         HB_BOOL bRecall = *pDelRecall == '2' ? HB_TRUE : HB_FALSE;

         if( pTA )
            pTA->uiFlag = ( ( bDelete ) ? 1 : 0 ) | ( ( bRecall ) ? 2 : 0 );
         else
         {
            HB_BOOL bState;

            SELF_DELETED( pArea, &bState );
            if( bDelete && ! bState )
               iRes = SELF_DELETE( pArea ) == HB_SUCCESS ? 0 : 102;
            else if( bRecall && bState )
               iRes = SELF_RECALL( pArea ) == HB_SUCCESS ? 0 : 102;
         }
      }

      if( uiUpd && ! iRes )  /* no errors up to here, and fields to update */
      {
         LPFIELD      pField;
         HB_USHORT    uiField, uiRealLen;
         PHB_ITEM     pItem = hb_itemNew( NULL );
         HB_USHORT    uiFieldCount = pArea->uiFieldCount;  // SELF_FIELDCOUNT( pArea, &uiFieldCount ) | pAStru->pTStru->uiFields
         HB_UCHAR     uLenLen, n255 = uiFieldCount > 255 ? 2 : 1;
         const char * ptr = pDelRecall + 2;
         int          i;

         if( pTA )
         {
            pTA->uiItems = uiUpd;
            pTA->puiIndex = ( HB_USHORT * ) hb_xgrab( sizeof( HB_USHORT ) * uiUpd );
            pTA->pItems = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) * uiUpd );
         }

         for( i = 0; i < uiUpd; i++ )
         {
            /*  Firstly, calculate the updated field number ( uiField ) */
            uiField = ( HB_USHORT ) leto_b2n( ptr, n255 );
            ptr += n255;

            if( ! uiField || uiField > uiFieldCount )
            {
               iRes = 3;
               break;
            }
            pField = pArea->lpFields + uiField - 1;
            switch( pField->uiType )
            {
               case HB_FT_STRING:
                  uLenLen = ( ( HB_UCHAR ) *ptr ) & 0xFF;
                  ptr++;
                  if( pField->uiLen > 255 )  /* is max limit is 64K ? */
                  {
                     uiRealLen = ( HB_USHORT ) leto_b2n( ptr, uLenLen );
                     ptr += uLenLen;
                  }
                  else
                     uiRealLen = ( HB_USHORT ) uLenLen;
                  if( uiRealLen > pField->uiLen )
                  {
                     iRes = 3;
                     break;
                  }
                  hb_itemPutCL( pItem, ptr, uiRealLen );
                  ptr += uiRealLen;
                  break;

               case HB_FT_LONG:
                  uiRealLen = ( ( HB_UCHAR ) *ptr++ ) & 0xFF;
                  if( uiRealLen > pField->uiLen )
                  {
                     iRes = 3;
                     break;
                  }
                  else
                  {
                     HB_MAXINT lVal;
                     double    dVal;
                     HB_BOOL   fDbl = hb_strnToNum( ptr, uiRealLen, &lVal, &dVal );

                     if( pField->uiDec )
                         hb_itemPutNDLen( pItem, fDbl ? dVal : ( double ) lVal,
                                         ( int ) ( pField->uiLen - pField->uiDec - 1 ),
                                         ( int ) pField->uiDec );
                     else if( fDbl )
                         hb_itemPutNDLen( pItem, dVal, ( int ) pField->uiLen, 0 );
                     else
                         hb_itemPutNIntLen( pItem, lVal, ( int ) pField->uiLen );
                     ptr += uiRealLen;
                     break;
                  }

              case HB_FT_FLOAT:
                  uiRealLen = ( ( HB_UCHAR ) *ptr++ ) & 0xFF;
                  if( uiRealLen > pField->uiLen )
                  {
                     iRes = 3;
                     break;
                  }
                  else
                  {
                     const char *  pszVal = ptr;
                     double        dVal = hb_strVal( pszVal, uiRealLen );
                     HB_SIZE       nLen = uiRealLen;

                     while( --nLen && HB_ISDIGIT( pszVal[ nLen ] ) )
                        ;
                     if( nLen && ( pszVal[ nLen ] == '+' || pszVal[ nLen ] == '-' ) &&
                         ( pszVal[ nLen - 1 ] == 'e' || pszVal[ nLen - 1 ] == 'E' ) )
                     {
                        HB_USHORT uiLen = ( HB_USHORT ) nLen;
                        int iExp = 0;

                        while( ++uiLen < pField->uiLen )
                        {
                           iExp = iExp * 10 + ( pszVal[ uiLen ] - '0' );
                        }
                        if( pszVal[ nLen ] == '-' )
                           iExp = -iExp;
                        dVal = hb_numExpConv( dVal, -iExp );
                     }
                     hb_itemPutNDLen( pItem, dVal, ( int ) ( pField->uiLen - pField->uiDec - 1 ),
                                      ( int ) pField->uiDec );
                     ptr += uiRealLen;
                     break;
                  }

               case HB_FT_DATE:
                  if( pField->uiLen == 8 )
                  {
                     char szBuffer[ 9 ];
                     memcpy( szBuffer, ptr, 8 );
                     szBuffer[ 8 ] = 0;
                     hb_itemPutDS( pItem, szBuffer );
                     ptr += 8;
                  }
                  else if( pField->uiLen == 4 )
                  {
                     hb_itemPutDL( pItem, HB_GET_LE_UINT32( ptr ) );
                     ptr += 4;
                  }
                  else  /*  pField->uiLen == 3 */
                  {
                     hb_itemPutDL( pItem, HB_GET_LE_UINT24( ptr ) );
                     ptr += 3;
                  }
                  break;

               case HB_FT_LOGICAL:
                  hb_itemPutL( pItem, ( *ptr == 'T' ) );
                  ptr++;
                  break;

               case HB_FT_INTEGER:
               case HB_FT_CURRENCY:
                  if( pField->uiDec )
                  {
                     int    iLen;
                     double dVal;

                     switch( pField->uiLen )
                     {
                        case 1:
                           dVal = ( HB_SCHAR ) *ptr;
                           iLen = 4;
                           break;
                        case 2:
                           dVal = HB_GET_LE_INT16( ptr );
                           iLen = 6;
                           break;
                        case 3:
                           dVal = HB_GET_LE_INT24( ptr );
                           iLen = 10;
                           break;
                        case 4:
                           dVal = HB_GET_LE_INT32( ptr );
                           iLen = 10;
                           break;
                        case 8:
                           dVal = ( double ) HB_GET_LE_INT64( ptr );
                           iLen = 20;
                           break;
                        default:
                           dVal = 0;
                           iLen = 0;
                           iRes = 3;
                           break;
                     }
                     hb_itemPutNDLen( pItem, hb_numDecConv( dVal, ( int ) pField->uiDec ),
                                      iLen, ( int ) pField->uiDec );
                  }
                  else
                  {
                     switch( pField->uiLen )
                     {
                        case 1:
                           hb_itemPutNILen( pItem, ( HB_SCHAR ) *ptr, 4 );
                           break;
                        case 2:
                           hb_itemPutNILen( pItem, ( int ) HB_GET_LE_INT16( ptr ), 6 );
                           break;
                        case 3:
                           hb_itemPutNIntLen( pItem, ( HB_MAXINT ) HB_GET_LE_INT24( ptr ), 10 );
                           break;
                        case 4:
                           hb_itemPutNIntLen( pItem, ( HB_MAXINT ) HB_GET_LE_INT32( ptr ), 10 );
                           break;
                        case 8:
#ifndef HB_LONG_LONG_OFF
                           hb_itemPutNIntLen( pItem, ( HB_MAXINT ) HB_GET_LE_INT64( ptr ), 20 );
#else
                           hb_itemPutNLen( pItem, ( double ) HB_GET_LE_INT64( ptr ), 20, 0 );
#endif
                           break;
                        default:
                           iRes = 3;
                           break;
                     }
                  }
                  break;

               case HB_FT_DOUBLE:
               case HB_FT_CURDOUBLE:
                  hb_itemPutNDLen( pItem, HB_GET_LE_DOUBLE( ptr ),
                                   20 - ( pField->uiDec > 0 ? ( pField->uiDec + 1 ) : 0 ),
                                   ( int ) pField->uiDec );
                  ptr += pField->uiLen;
                  break;

               case HB_FT_TIME:
                  if( pField->uiLen == 4 )
                  {
                     hb_itemPutTDT( pItem, 0, HB_GET_LE_INT32( ptr ) );
                     ptr += pField->uiLen;
                     break;
                  }
                  /* no break */
               case HB_FT_MODTIME:
               case HB_FT_TIMESTAMP:
                  hb_itemPutTDT( pItem, HB_GET_LE_INT32( ptr ), HB_GET_LE_INT32( ptr + 4 ) );
                  ptr += pField->uiLen;
                  break;

               case HB_FT_ANY:
                  if( pField->uiLen == 3 )
                  {
                     hb_itemPutDL( pItem, HB_GET_LE_UINT24( ptr ) );
                     ptr += 3;
                  }
                  else if( pField->uiLen == 4 )
                  {
                     hb_itemPutNL( pItem, HB_GET_LE_UINT32( ptr ) );
                     ptr += 4;
                  }
                  else
                  {
                     switch( *ptr++ )
                     {
                        case 'D':
                        {
                           char szBuffer[ 9 ];

                           memcpy( szBuffer, ptr, 8 );
                           szBuffer[ 8 ] = 0;
                           hb_itemPutDS( pItem, szBuffer );
                           ptr += 8;
                           break;
                        }

                        case 'L':
                           hb_itemPutL( pItem, ( *ptr == 'T' ) );
                           ptr += 8;
                           break;

                        case 'N':
                        {
                           HB_USHORT uiLen = ( ( HB_UCHAR ) *ptr ) & 0xFF;
                           HB_MAXINT lVal;
                           double    dVal;
                           HB_BOOL   bDouble;

                           ptr++;
                           bDouble = hb_strnToNum( ptr, uiLen, &lVal, &dVal );
                           if( bDouble )
                              hb_itemPutND( pItem, dVal );
                           else
                              hb_itemPutNInt( pItem, lVal );
                           ptr += uiLen;
                           break;
                        }

                        case 'C':
                        {
                           HB_USHORT uiLen = ( HB_USHORT ) leto_b2n( ptr, 2 );

                           ptr += 2;
                           hb_itemPutCL( pItem, ptr, uiLen );
                           ptr += uiLen;
                           break;
                        }
                     }
                  }
                  break;
            }

            if( iRes )
               break;

            if( pTA )
            {
               pTA->puiIndex[ i ] = uiField;
               pTA->pItems[ i ] = hb_itemNew( pItem );
               hb_itemClear( pItem );
            }
            else
            {
               if( SELF_PUTVALUE( pArea, uiField, pItem ) != HB_SUCCESS )
               {
                  leto_wUsLog( pUStru, 0, "ERROR leto_UpdateRecord(%lu) record lock misses" );
                  iRes = 102;
                  break;
               }
               else if( pUStru->iHbError )
               {
                  leto_wUsLog( pUStru, 0, "ERROR leto_UpdateRecord() record locking" );
                  iRes = 102;
                  break;
               }
            }
         }
         if( s_bShareTables )
            pAStru->llNanoSecRead = pAStru->pTStru->pGlobe->llNanoSecWrite = leto_timeCounter();

         hb_itemRelease( pItem );
      }

      if( ! pTA )
      {
         hb_xvmSeqEnd();
         if( pUStru->iHbError )
            iRes = 101;
      }
   }

   return iRes;
}

static void leto_UpdateRec( PUSERSTRU pUStru, const char * szData, HB_BOOL bAppend )
{
   const char * pData;
   char         szData1[ 24 ];
   char *       szData2 = NULL;
   HB_ULONG     ulRecNo = 0, ulLen = 0;
   int          iRes;

   if( ( s_bPass4D && ! ( pUStru->szAccess[ 0 ] & 0x4 ) ) )
   {
      pData = szErrAcc;
      iRes = 1025;
   }
   else  /* ToDo ? when server is locked */
   {
      iRes = leto_UpdateRecord( pUStru, szData, bAppend, &ulRecNo, NULL, NULL );
      switch( iRes )
      {
         case 0:
         case 1:
            if( bAppend )
            {
               if( iRes || hb_rddGetNetErr() )
                  pData = szErr101;
               else
               {
                  if( *szData == '0' )  /* in case of autoinc fields */
                  {
                     AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

                     szData2 = leto_recWithAlloc( pArea, pUStru, pUStru->pCurAStru, &ulLen );
                     pData = szData2;
                  }
                  else
                  {
                     char * ptr = szData1;

                     *ptr++ = '+';
                     ptr += ultostr( ulRecNo, ptr );
                     *ptr++ = ';';
                     *ptr = '\0';
                     pData = szData1;
                  }
               }
            }
            else
               pData = szOk;
            break;

         case 2:
            pData = szErr2;
            break;

         case 3:
            pData = szErr3;
            break;

         case 4:
            pData = szErr4;
            break;

         case 100:
            szData1[ 0 ] = '-';
            szData1[ 1 ] = '1';
            szData1[ 2 ] = '0';
            szData1[ 3 ] = '0';
            szData1[ 4 ] = '\0';
            pData = szData1;
            break;

         case 201:
            if( bAppend )
            {
               ulLen = sprintf( szData1, "%s", "-004:40-1024-0-4" );  /* EG-APPENDLOCK - EDBF_APPENDLOCK */
               pData = szData1;
               iRes = 1024;
            }
            else
            {
               ulLen = sprintf( szData1, "%s", "-004:38-1022-0-5" );  /* EG_UNLOCKED - EDBF_UNLOCKED */
               pData = szData1;
               iRes = 1022;
            }
            break;

         default:  /* ??? */
            pData = szErr3;
      }
   }

   if( bAppend )  /* immedeate answer, may contain recno */
      leto_SendAnswer( pUStru, pData, ulLen ? ulLen : strlen( pData ) );
   else
      leto_SendAnswer2( pUStru, pData, ulLen ? ulLen : strlen( pData ), ! iRes, iRes < 1000 ? iRes + 1000 : iRes );
      //leto_SendAnswer( pUStru, pData, strlen( pData ) );
   if( szData2 )
      hb_xfree( szData2 );
}

static void leto_Flush( PUSERSTRU pUStru, const char * szData )
{
   AREAP   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL bOk;

   bOk = SELF_FLUSH( pArea ) == HB_SUCCESS;
   if( bOk )
      SELF_GOCOLD( pArea );
   pUStru->pCurAStru->bUseSkipBuffer = HB_FALSE;

   if( szData )  /* else a flush done after append/ update */
      leto_SendAnswer2( pUStru, szOk, 4, bOk, 1000 );
   //leto_SendAnswer( pUStru, szOk|szErrr101, 4 );
}

static void leto_UpdateRecAdd( PUSERSTRU pUStru, const char * szData )
{
   leto_UpdateRec( pUStru, szData, HB_TRUE );
}

static void leto_UpdateRecAddflush( PUSERSTRU pUStru, const char * szData )
{
   leto_UpdateRec( pUStru, szData, HB_TRUE );
   leto_Flush( pUStru, NULL );
}

static void leto_UpdateRecUpd( PUSERSTRU pUStru, const char * szData )
{
   leto_UpdateRec( pUStru, szData, HB_FALSE );
}

static void leto_UpdateRecUpdflush( PUSERSTRU pUStru, const char * szData )
{
   leto_UpdateRec( pUStru, szData, HB_FALSE );
   leto_Flush( pUStru, NULL );
}

static PHB_ITEM leto_KeyToItem( AREAP pArea, const char * ptr, int iKeyLen, const char * pOrder, char cKeyType )
{
   PHB_ITEM pKey = NULL;

   if( ! cKeyType )
      cKeyType = leto_OrdKeyType( pArea, pOrder );

   switch( cKeyType )
   {
      case 'C':
         pKey = hb_itemPutCL( NULL, ptr, ( HB_ULONG ) iKeyLen );
         break;

      case 'N':
      {
         int       iWidth, iDec;
         HB_BOOL   fDbl;
         HB_MAXINT lValue;
         double    dValue;

         fDbl = hb_valStrnToNum( ptr, iKeyLen, ( HB_MAXINT * ) &lValue, &dValue, &iDec, &iWidth );
         if( ! fDbl )
            pKey = hb_itemPutNIntLen( NULL, lValue, iWidth );
         else
            pKey = hb_itemPutNLen( NULL, dValue, iWidth, iDec );
         break;
      }

      case 'D':
      {
         char szBuffer[ 9 ];

         memcpy( szBuffer, ptr, 8 );
         szBuffer[ 8 ] = 0;
         pKey = hb_itemPutDS( NULL, szBuffer );
         break;
      }

      case 'T':
      {
         char * szBuffer = ( char * ) hb_xgrab( iKeyLen + 1 );

         memcpy( szBuffer, ptr, iKeyLen );  /* 17 */
         szBuffer[ iKeyLen ] = '\0';
         pKey = hb_itemPutTS( NULL, szBuffer );
         hb_xfree( szBuffer );
         break;
      }

      case 'L':
         pKey = hb_itemPutL( NULL, *ptr == 'T' );
         break;
   }

   return pKey;
}

#ifdef __BM
static void leto_BMRestore( AREAP pArea, PAREASTRU pAStru )
{
   pArea->dbfi.lpvCargo = pAStru->pBM;
   if( pAStru->pBM )
   {
      pArea->dbfi.fFilter = HB_TRUE;
      pArea->dbfi.fOptimized = HB_TRUE;
   }
}
#endif

static void leto_SetFilter( PAREASTRU pAStru, AREAP pArea, PUSERSTRU pUStru )
{
   if( pAStru->itmFltExpr )
   {
      PHB_ITEM pItem;

      hb_xvmSeqBegin();
      pItem = hb_vmEvalBlockOrMacro( pAStru->itmFltExpr );
      hb_xvmSeqEnd();
      if( pUStru->iHbError || ! HB_IS_LOGICAL( pItem ) )
      {
         pUStru->iHbError = 0;
         hb_itemRelease( pAStru->itmFltExpr );
         pAStru->itmFltExpr = NULL;
         pArea->dbfi.fFilter = HB_FALSE;
         leto_wUsLog( pUStru, -1, "ERROR leto_SetFilter! now invalid filter for WA %s removed", pAStru->szAlias );
      }
      else
      {
         pArea->dbfi.itmCobExpr = pAStru->itmFltExpr;
         pArea->dbfi.fOptimized = HB_FALSE;
         pArea->dbfi.fFilter = HB_TRUE;
      }
   }
#ifdef __BM
   if( pAStru->pBM )
      leto_BMRestore( pArea, pAStru );
#endif
}

static void leto_ClearFilter( AREAP pArea )
{
   pArea->dbfi.itmCobExpr = NULL;
   pArea->dbfi.fFilter = HB_FALSE;
#ifdef __BM
   pArea->dbfi.lpvCargo = NULL;
#endif
}

static void leto_ScopeCommand( AREAP pArea, HB_USHORT uiCommand, PHB_ITEM pKey )
{
   DBORDERINFO pInfo;

   memset( &pInfo, 0, sizeof( DBORDERINFO ) );
   pInfo.itmNewVal = pKey;
   pInfo.itmResult = hb_itemNew( NULL );
   SELF_ORDINFO( pArea, uiCommand, &pInfo );
   hb_itemRelease( pInfo.itmResult );
}

static void leto_SetScope( AREAP pArea, LETOTAG * pTag, HB_BOOL bTop, HB_BOOL bSet )
{
   PHB_ITEM pKey = ( bTop ? pTag->pTopScope : pTag->pBottomScope );

   if( pKey )
   {
      HB_USHORT uiCommand;

      if( bSet )
         uiCommand = ( bTop ? DBOI_SCOPETOP : DBOI_SCOPEBOTTOM );
      else
         uiCommand = ( bTop ? DBOI_SCOPETOPCLEAR : DBOI_SCOPEBOTTOMCLEAR );
      leto_ScopeCommand( pArea, uiCommand, ( bSet ? pKey : NULL ) );
   }
}

static _HB_INLINE_ void leto_SetFocusIf( PAREASTRU pAStru, AREAP pArea, const char * szOrder )
{
   if( ! pAStru->pTagCurrent || leto_stricmp( pAStru->pTagCurrent->szTagName, szOrder ) )
   {
      if( *szOrder )
      {
         if( leto_FindTag( pAStru, szOrder ) && ! leto_SetFocus( pArea, szOrder ) )
            pAStru->pTagCurrent = NULL;
      }
      else if( pAStru->pTagCurrent )
      {
         leto_SetFocus( pArea, "" );
         pAStru->pTagCurrent = NULL;
      }
   }
   else if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
      leto_SetFocusQuick( pArea, szOrder );
}

static void leto_SetAreaEnv( PAREASTRU pAStru, AREAP pArea, PUSERSTRU pUStru )
{
   LETOTAG * pTag = pAStru->pTagCurrent;

   if( pTag )
   {
      leto_SetFocusQuick( pArea, pTag->szTagName );
      if( pTag->pTopScope )
         leto_SetScope( pArea, pTag, HB_TRUE, HB_TRUE );   /* set TopScope */
      if( pTag->pBottomScope )
         leto_SetScope( pArea, pTag, HB_FALSE, HB_TRUE );  /* set BotScope */
   }
   else
      leto_SetFocusQuick( pArea, "" );

   leto_SetFilter( pAStru, pArea, pUStru );
}

static void leto_ClearAreaEnv( AREAP pArea, LETOTAG * pTag )
{
   leto_ClearFilter( pArea );

   if( pTag )
   {
      if( pTag->pTopScope )
         leto_SetScope( pArea, pTag, HB_TRUE, HB_FALSE );   /* clear TopScope */
      if( pTag->pBottomScope )
         leto_SetScope( pArea, pTag, HB_FALSE, HB_FALSE );  /* clear BotScope */
   }
}

static void leto_Seek( PUSERSTRU pUStru, const char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PAREASTRU    pAStru = pUStru->pCurAStru;
   char *       szData1 = NULL;
   const char * pData;
   const char * pOrdKey;
   HB_ULONG     ulLen = 4;
   HB_BOOL      bSoftSeek, bFindLast;
   int          iKeyLen;

   if( strlen( szData ) < 3 || ! pArea )
      pData = szErr2;
   else
   {
      if( pUStru->bDeleted != ( *szData & 0x01 ) )
      {
         pUStru->bDeleted = ( *szData & 0x01 );
         leto_setSetDeleted( pUStru->bDeleted );
      }
      bSoftSeek = ( *szData & 0x10 );
      bFindLast = ( *szData & 0x20 );

      pOrdKey = szData + 2;  /* behind one char + ';' -- following binary data */
      iKeyLen = ( ( ( HB_UCHAR ) *pOrdKey++ ) & 0xFF );

      if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
         leto_SetAreaEnv( pAStru, pArea, pUStru );
      if( pAStru->pTagCurrent )
      {
         PHB_ITEM pKey = leto_KeyToItem( pArea, pOrdKey, iKeyLen, NULL, pAStru->pTagCurrent->pIStru->cKeyType );

         if( pKey )
         {
            HB_BOOL bMutex = ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO && pAStru->itmFltExpr );

            /* see note in leto_Skip(), here the same */
            if( bMutex )
               HB_GC_LOCKA();
            if( SELF_SEEK( pArea, bSoftSeek, pKey, bFindLast ) == HB_SUCCESS )
            {
               szData1 = leto_recWithAlloc( pArea, pUStru, pAStru, &ulLen );
               if( szData1 )
                  pData = szData1;
               else
                  pData = szErr3;
            }
            else
               pData = szErr101;

            if( bMutex )
               HB_GC_UNLOCKA();
            hb_itemRelease( pKey );
         }
         else
            pData = szErr2;
      }
      else
         pData = szErr4;
      if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
         leto_ClearAreaEnv( pArea, pAStru->pTagCurrent );
   }

   leto_SendAnswer( pUStru, pData, ulLen );
   if( szData1 )
      hb_xfree( szData1 );
}

static void leto_Scope( PUSERSTRU pUStru, const char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PAREASTRU    pAStru = pUStru->pCurAStru;
   const char * pOrder, * szKey, * pData;
   int          nParam = leto_GetParam( szData, &pOrder, /* szKey */ NULL, NULL, NULL );

   if( nParam < 2 || ! pArea )
      pData = szErr2;
   else
   {
      HB_USHORT uiCommand = ( HB_USHORT ) atoi( szData );
      int iKeyLen;

      szKey = pOrder + strlen( pOrder ) + 1;  /* binary data */
      iKeyLen = ( ( ( HB_UCHAR ) *szKey++ ) & 0xFF );

      if( *pOrder )
      {
         LETOTAG * pTag;

         leto_SetFocusIf( pAStru, pArea, pOrder );
         pTag = pAStru->pTagCurrent;
         if( pTag )
         {
            if( uiCommand == DBOI_SCOPESET || uiCommand == DBOI_SCOPETOP || uiCommand == DBOI_SCOPEBOTTOM )
            {
               PHB_ITEM pKey = leto_KeyToItem( pArea, szKey, iKeyLen, pOrder, pTag->pIStru->cKeyType );

               if( pKey )
               {
                  if( uiCommand == DBOI_SCOPETOP || uiCommand == DBOI_SCOPESET )
                  {
                     if( ! pTag->pTopScope )
                        pTag->pTopScope = hb_itemNew( NULL );
                     hb_itemCopy( pTag->pTopScope, pKey );
                     /* note: call after allocated */
                     if( s_bNoSaveWA && ! pAStru->pTStru->bMemIO )
                        leto_SetScope( pArea, pTag, HB_TRUE, HB_TRUE );   /* set TopScope */
                  }
                  if( uiCommand == DBOI_SCOPEBOTTOM || uiCommand == DBOI_SCOPESET )
                  {
                     if( ! pTag->pBottomScope )
                        pTag->pBottomScope = hb_itemNew( NULL );
                     hb_itemCopy( pTag->pBottomScope, pKey );
                     if( s_bNoSaveWA && ! pAStru->pTStru->bMemIO )
                        leto_SetScope( pArea, pTag, HB_FALSE, HB_TRUE );  /* set BotScope */
                  }
                  hb_itemRelease( pKey );
                  pData = szOk;
               }
               else
                  pData = szErr2;
            }
            else
            {
               if( pTag->pTopScope && ( uiCommand == DBOI_SCOPETOPCLEAR || uiCommand == DBOI_SCOPECLEAR ) )
               {
                  /* note: call before released */
                  if( s_bNoSaveWA && ! pAStru->pTStru->bMemIO )
                     leto_SetScope( pArea, pTag, HB_TRUE, HB_FALSE );   /* clear TopScope */
                  hb_itemRelease( pTag->pTopScope );
                  pTag->pTopScope = NULL;
               }
               if( pTag->pBottomScope && ( uiCommand == DBOI_SCOPEBOTTOMCLEAR || uiCommand == DBOI_SCOPECLEAR ) )
               {
                  if( s_bNoSaveWA && ! pAStru->pTStru->bMemIO )
                     leto_SetScope( pArea, pTag, HB_FALSE, HB_FALSE );  /* clear BotScope */
                  hb_itemRelease( pTag->pBottomScope );
                  pTag->pBottomScope = NULL;
               }
               pData = szOk;
            }
         }
         else
            pData = szErr3;
      }
      else
         pData = szErr2;
   }

   leto_SendAnswer( pUStru, pData, 4 );
}

static void leto_Skip( PUSERSTRU pUStru, const char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PAREASTRU    pAStru = pUStru->pCurAStru;
   char *       szData1 = NULL;
   const char * pData = NULL;
   const char * pRecNo, * pFlags;
   HB_ULONG     ulRecNo;
   HB_ULONG     ulLen = 4;
   HB_LONG      lSkip;
   HB_BOOL      bMutex;
   HB_ERRCODE   errCode;
   int          nParam = leto_GetParam( szData, &pRecNo, &pFlags, NULL, NULL );

   if( nParam < 3 || ! pArea )
      pData = szErr2;
   else
   {
      lSkip = atol( szData );
      ulRecNo = strtoul( pRecNo, NULL, 10 );
      if( pUStru->bDeleted != ( *pFlags & 0x01 ) )
      {
         pUStru->bDeleted = ( *pFlags & 0x01 );
         leto_setSetDeleted( pUStru->bDeleted );
      }
      bMutex = ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO && pAStru->itmFltExpr );

      if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
         leto_SetAreaEnv( pAStru, pArea, pUStru );
      leto_GotoIf( pArea, ulRecNo );
      if( pArea->dbfi.fFilter )  /* adjust to next valid record one down, one up if EOF */
      {
         HB_BOOL  bFlag;

         SELF_EOF( pArea, &bFlag );
         if( ! bFlag )
         {
            SELF_SKIPFILTER( pArea, 1 );
            SELF_EOF( pArea, &bFlag );
            if( bFlag )
               SELF_SKIP( pArea, -1 );
         }
      }

      /* Note: this is paradox: if *multiple* threads simultanous consecutive skip in same WA with filter condition,
       * it drastically improves performance to NOT let them do that simultanous */
      if( bMutex )
         HB_GC_LOCKA();
      errCode = SELF_SKIP( pArea, lSkip );
      if( errCode == HB_SUCCESS )
      {
         HB_ULONG ulMemSize = leto_recLen( pAStru->pTStru ) * pAStru->uiSkipBuf;
         HB_ULONG ulLenAll;
         HB_ULONG ulRelPos = 0;
         HB_BOOL  bFlag;

         szData1 = ( char * ) hb_xgrab( ulMemSize + 1 );
         ulLenAll = leto_rec( pUStru, pAStru, pArea, szData1 + 1, &ulRelPos );
         if( ! ulLenAll )
            pData = szErr2;
         else if( ! pAStru->bUseSkipBuffer )
            pAStru->bUseSkipBuffer = HB_TRUE;
         else if( lSkip )
         {
            HB_USHORT i = 1;
            HB_ULONG  ulLen2;

            ulRelPos += lSkip;
            while( i++ < pAStru->uiSkipBuf )
            {
               if( lSkip > 0 )
                  SELF_EOF( pArea, &bFlag );
               else
                  SELF_BOF( pArea, &bFlag );

               if( bFlag )
                  break;
               else
               {
                  errCode = SELF_SKIP( pArea, lSkip );
                  if( errCode == HB_SUCCESS )
                  {
                     if( lSkip < 0 )
                     {
                        SELF_BOF( pArea, &bFlag );
                        if( bFlag )
                           break;
                     }
                     ulLen2 = leto_rec( pUStru, pAStru, pArea, szData1 + 1 + ulLenAll, &ulRelPos );
                     ulRelPos += lSkip;
                     if( ! ulLen2 )
                     {
                        pData = szErr2;
                        break;
                     }
                     ulLenAll += ulLen2;
                  }
                  else
                  {
                     pData = szErr101;
                     break;
                  }
               }
            }
#if 0  /* re-sync with client ? */
            if( s_bNoSaveWA )
               leto_GotoIf( pArea, ulRecNo );
#endif
         }

         if( pData == NULL )
         {
            ulLen = ulLenAll + 1;
            szData1[ 0 ] = '+';
            pData = szData1;
         }
      }
      else
         pData = szErr101;

      if( bMutex )
         HB_GC_UNLOCKA();

      if( ulLen == 4 )
         leto_wUsLog( pUStru, -1, "ERROR leto_Skip! %d failed, EOF %d, recno %lu, filter %d", ( int ) lSkip, pArea->fEof,
                      ulRecNo, pArea->dbfi.itmCobExpr ? ( int ) hb_itemGetL( hb_vmEvalBlock( pArea->dbfi.itmCobExpr ) ) : -1 );

      if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
         leto_ClearAreaEnv( pArea, pAStru->pTagCurrent );
   }

   leto_SendAnswer( pUStru, pData, ulLen );
   if( szData1 )
      hb_xfree( szData1 );
}

/* pBlocks in order like for PRG level DbEval(): pBlock, pForCondition, pWhileCondition */
static PHB_ITEM * leto_dbEval( PUSERSTRU pUStru, AREAP pArea, PHB_ITEM * pBlocks[], HB_ULONG ulNext, HB_ULONG ulRecNo, HB_BOOL bRest, HB_ULONG * pulLen )
{
   HB_ULONG   ulRecLen = leto_recLen( pUStru->pCurAStru->pTStru );
   HB_ULONG   ulLen2;
   HB_ULONG   ulRelPos = 0;
   char *     szData1 = ( char * ) hb_xgrab( ulRecLen );
   HB_BOOL    bFlag;
   PHB_ITEM * pFor = NULL;
   PHB_ITEM * pWhile = NULL;
   PHB_ITEM * pResult;
   PHB_ITEM * pFirst = NULL;
   HB_USHORT  i = 1;
   PHB_ITEM   pResultArr = hb_itemArrayNew( 0 );

   if( pBlocks[ 0 ] )
   {
      pFirst = hb_itemNew( NULL );
      hb_itemPutL( pFirst, HB_TRUE );
   }
   if( ! pBlocks[ 1 ] )  /* FOR condition */
   {
      pFor = hb_itemNew( NULL );
      hb_itemPutL( pFor, HB_TRUE );
   }

   hb_xvmSeqBegin();

   if( ulRecNo )
      leto_GotoIf( pArea, ulRecNo );
   else if( ! bRest )
      SELF_GOTOP( pArea );

   for( ;; )
   {
      if( pBlocks[ 2 ] )
      {
         if( pWhile )
            hb_itemRelease( pWhile );
         pWhile = hb_vmEvalBlock( pBlocks[ 2 ] );
         if( ! ( hb_itemType( pWhile ) & HB_IT_LOGICAL ) || ! hb_itemGetL( pWhile ) )
            break;
      }

      if( pBlocks[ 1 ] )
      {
         if( pFor )
            hb_itemRelease( pFor );
         pFor = hb_vmEvalBlock( pBlocks[ 1 ] );
         if( ( hb_itemType( pFor ) & HB_IT_LOGICAL ) && ! hb_itemGetL( pFor ) )
         {
            if( ulNext-- == 1 )
               break;
            else
               continue;
         }
      }

      if( pBlocks[ 0 ] )  /* eval a given block, else collect all record data */
      {
         pResult = hb_vmEvalBlockV( pBlocks[ 0 ], 1, pFirst );  /* lFirst par for CB {|lFirst| .. } */
         if( i++ == 1 )
            hb_itemPutL( pFirst, HB_FALSE );
         hb_arrayAdd( pResultArr, pResult );
      }
      else  // default action: raw record
      {
         ulLen2 = leto_rec( pUStru, pUStru->pCurAStru, pArea, szData1, &ulRelPos );
         ulRelPos += 1;
         i++;
         if( ! ulLen2 )
            break;
         else
         {
            HB_SIZE nLen;

            hb_arraySize( pResultArr, ( nLen = hb_arrayLen( pResultArr ) + 1 ) );
            hb_arraySetCL( pResultArr, nLen, szData1, ulLen2 );
         }
      }

      SELF_SKIP( pArea, 1 );
      if( ulNext-- == 1 )
         break;

      SELF_EOF( pArea, &bFlag );
      if( bFlag )
         break;
   }
   hb_xvmSeqEnd();

   if( pFor )
      hb_itemRelease( pFor );
   if( pWhile )
      hb_itemRelease( pWhile );
   if( pFirst )
      hb_itemRelease( pFirst );
   hb_xfree( szData1 );

   *pulLen = ( HB_ULONG ) i;

   return pResultArr;
}

static PHB_ITEM leto_mkCodeBlock( const char * szExp, HB_ULONG ulLen, HB_BOOL bSecured )
{
   PHB_ITEM pBlock = NULL;

   if( ulLen > 0 )
   {
      char *   szMacro = ( char * ) hb_xgrab( ulLen + 5 );
      PHB_ITEM pFreshBlock;

      if( szExp[ 0 ] == '{' && szExp[ ulLen - 1 ] == '}' )
      {
         memcpy( szMacro, szExp, ulLen );
         szMacro[ ulLen ] = '\0';
      }
      else
      {
         szMacro[ 0 ] = '{';
         szMacro[ 1 ] = '|';
         szMacro[ 2 ] = '|';
         memcpy( szMacro + 3, szExp, ulLen );
         szMacro[ 3 + ulLen ] = '}';
         szMacro[ 4 + ulLen ] = '\0';
         ulLen += 5;
      }

      pBlock = hb_itemNew( NULL );
      hb_vmPushString( szMacro, ulLen );

      if( bSecured )
         hb_xvmSeqBegin();  // ToDo -- understand why crash if this is second call
      pFreshBlock = hb_stackItemFromTop( -1 );
      if( pFreshBlock )
      {
         hb_macroGetValue( pFreshBlock, 0, 64 );  /* 64 = HB_MACRO_GEN_REFER */
         hb_itemMove( pBlock, hb_stackItemFromTop( -1 ) );
         hb_stackPop();
      }
      if( bSecured )
      {
         if( hb_xvmSeqEndTest() )
         {
            if( ! hb_xvmSeqRecover() )  /* no VM quit */
            {
               /* don't care about what pError, just remove from stack */
               hb_stackPop();
               if( pBlock )
                  hb_itemRelease( pBlock );
               pBlock = NULL;
            }
         }
      }
      hb_xfree( szMacro );
   }

   return pBlock;
}

/* leto_udf() leto_DbEval( bBlock, bFor, bWhile, nNext, nRec, lRest ) */
HB_FUNC( LETO_DBEVAL )
{
   PUSERSTRU  pUStru = letoGetUStru();
   AREAP      pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_ULONG   ulLen;
   PHB_ITEM * pBlocks[ 3 ] = { NULL, NULL, NULL };  /* bBlock, bFor, bWhile */
   HB_ULONG   ulNext = HB_ISNUM( 4 ) ? ( HB_ULONG ) hb_parnl( 4 ) : 0;
   HB_ULONG   ulRecNo =  HB_ISNUM( 5 ) ? ( HB_ULONG ) hb_parnl( 5 ) : 0;
   HB_BOOL    bRest = HB_ISLOG( 6 ) ? hb_parl( 6 ) : HB_FALSE;
   int        i;
   PHB_ITEM * pArray = NULL;

   hb_xvmSeqBegin();

   if( HB_ISCHAR( 1 ) )
      pBlocks[ 0 ] = leto_mkCodeBlock( hb_parc( 1 ), hb_parclen( 1 ), HB_FALSE );
   else if( HB_ISEVALITEM( 1 ) )
      pBlocks[ 0 ] = hb_itemClone( hb_param( 1, HB_IT_EVALITEM ) );

   if( HB_ISCHAR( 2 ) )
      pBlocks[ 1 ] = leto_mkCodeBlock( hb_parc( 2 ), hb_parclen( 2 ), HB_FALSE );
   else if( HB_ISEVALITEM( 2 ) )
      pBlocks[ 1 ] = hb_itemClone( hb_param( 2, HB_IT_EVALITEM ) );

   if( HB_ISCHAR( 3 ) )
      pBlocks[ 2 ] = leto_mkCodeBlock( hb_parc( 3 ), hb_parclen( 3 ), HB_FALSE );
   else if( HB_ISEVALITEM( 3 ) )
      pBlocks[ 2 ] = hb_itemClone( hb_param( 3, HB_IT_EVALITEM ) );

   if( pArea && pUStru )
      pArray = leto_dbEval( pUStru, pArea, pBlocks, ulNext, ulRecNo, bRest, &ulLen );

   hb_xvmSeqEnd();

   if( ! pUStru->iHbError && ! pArray )
      pArray = hb_itemArrayNew( 0 );

   if( pArray )
      hb_itemReturnRelease( pArray );
   else
      hb_ret();

   for( i = 0; i < 3; i++ )
   {
      if( pBlocks[ i ] )
         hb_itemRelease( pBlocks[ i ] );
   }
}

static void leto_Goto( PUSERSTRU pUStru, const char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PAREASTRU    pAStru = pUStru->pCurAStru;
   char *       szData1 = NULL;
   const char * pData, * pFlags;
   HB_ULONG     ulLen;
   HB_ERRCODE   errCode;
   int          nParam = leto_GetParam( szData, &pFlags, NULL, NULL, NULL );

   if( nParam < 2 || ! pArea )
   {
      pData = szErr2;
      ulLen = 4;
   }
   else
   {
      HB_LONG lRecNo = atol( szData );

      if( lRecNo > 0 )
         errCode = SELF_GOTO( pArea, ( HB_ULONG ) lRecNo );
      else if( ( lRecNo == -1 ) || ( lRecNo == -2 ) )
      {
         HB_BOOL bTop = lRecNo == -1;

         if( pUStru->bDeleted != ( *pFlags & 0x01 ) )
         {
            pUStru->bDeleted = ( *pFlags & 0x01 );
            leto_setSetDeleted( pUStru->bDeleted );
         }
         if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
            leto_SetAreaEnv( pAStru, pArea, pUStru );
         if( bTop )
            errCode = SELF_GOTOP( pArea );
         else
            errCode = SELF_GOBOTTOM( pArea );

         if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
            leto_ClearAreaEnv( pArea, pAStru->pTagCurrent );
      }
      else /*  lRecNo == -3 || 0  --> EOF */
         errCode = SELF_GOTO( pArea, 0 );

      if( errCode != HB_SUCCESS )
      {
         pData = szErr101;
         ulLen = 4;
      }
      else
      {
         szData1 = leto_recWithAlloc( pArea, pUStru, pAStru, &ulLen );
         if( szData1 )
            pData = szData1;
         else
         {
            ulLen = 4;
            pData = szErr2;
         }
      }
   }

   leto_SendAnswer( pUStru, pData, ulLen );
   if( szData1 )
      hb_xfree( szData1 );
}

static int leto_Memo( PUSERSTRU pUStru, const char * szData, TRANSACTSTRU * pTA, AREAP pArea )
{
   PAREASTRU    pAStru = pUStru->pCurAStru;
   const char * pNumRec, * pField, * pData = NULL;
   int          iRes = 0;
   int          nParam = leto_GetParam( szData, &pNumRec, &pField, /*pMemo*/ NULL, NULL );
   HB_BOOL      bPut = HB_FALSE;
   HB_ULONG     ulLen = 4;

   if( nParam < 3 )
   {
      pData = szErr2;
      iRes = 2;
   }
   else
   {
      HB_ULONG  ulNumrec = strtoul( pNumRec, NULL, 10 );
      HB_USHORT uiField = ( HB_USHORT ) atoi( pField );
      HB_BOOL   bAdd = HB_FALSE;

      if( ( ! ulNumrec && ! pTA ) || ! uiField )
      {
         pData = szErr2;
         iRes = 3;
      }

      switch( *szData )
      {
         case LETOSUB_get:
            break;

         case LETOSUB_put:
            bPut = HB_TRUE;
            break;

         case LETOSUB_add:
            bPut = HB_TRUE;
            bAdd = HB_TRUE;
            break;

         default:
            pData = szErr2;
            iRes = 2;
      }

      if( ! pData )
      {
         //if( ! pTA )
         //   hb_xvmSeqBegin();

         if( bPut && ( ! pTA || ulNumrec ) &&
             pAStru->pTStru->bShared && ! pAStru->bLocked &&
             leto_IsRecLocked( pAStru, ulNumrec ) != 1 )
         {
            /*  The table is opened in shared mode, but the record isn't locked */
            pData = szErr4;
            iRes = 4;
         }
         else
         {
            PHB_ITEM pMemoText = hb_itemNew( NULL );

            if( ! pArea )
               pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

            if( ! pTA )
               leto_GotoForce( pArea, ulNumrec );
            else
            {
               pTA->bAppend = bAdd;
               pTA->ulRecNo = ulNumrec;
            }

            if( bPut )
            {
               const char * pMemo = pField + strlen( pField ) + 1;  /* possible binary data */
               HB_UCHAR     uLenLen;
               HB_ULONG     ulMemoLen;

               if( ( uLenLen = ( ( ( HB_UCHAR ) *pMemo ) & 0xFF ) ) >= 10 )
               {
                  pData = szErr2;
                  iRes = 2;
               }
               else
               {
                  ulMemoLen = leto_b2n( ++pMemo, uLenLen );
                  pMemo += uLenLen;
                  hb_itemPutCL( pMemoText, pMemo, ulMemoLen );

                  if( pTA )
                  {
                     pTA->uiFlag = 4;
                     pTA->uiItems = 1;
                     pTA->puiIndex = ( HB_USHORT * ) hb_xgrab( sizeof( HB_USHORT ) );
                     pTA->pItems = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) );
                     pTA->puiIndex[ 0 ] = uiField;
                     pTA->pItems[ 0 ] = hb_itemNew( pMemoText );
                  }
                  else
                  {
                     SELF_PUTVALUE( pArea, uiField, pMemoText );
                     pData = szOk;
                  }
               }
               /* reset timer for hotbuffer cache */
               if( s_bShareTables )
                  pAStru->llNanoSecRead = pAStru->pTStru->pGlobe->llNanoSecWrite = leto_timeCounter();
            }
            else
            {
               SELF_GETVALUE( pArea, uiField, pMemoText );
               ulLen = leto_CryptText( pUStru, hb_itemGetCPtr( pMemoText ), hb_itemGetCLen( pMemoText ) );
               pData = ( char * ) pUStru->pBufCrypt;
            }
            hb_itemRelease( pMemoText );
         }

         //if( ! pTA )
         //   hb_xvmSeqEnd();
      }
   }

   if( ! pTA )
   {
      if( ! bPut )
         leto_SendAnswer( pUStru, pData, ulLen );
      else
         leto_SendAnswer2( pUStru, pData, ulLen, ! iRes, 1021 + iRes );
      if( pUStru->ulBufCryptLen > LETO_SENDRECV_BUFFSIZE )
      {
         hb_xfree( pUStru->pBufCrypt );
         pUStru->pBufCrypt = NULL;
         pUStru->ulBufCryptLen = 0;
      }
   }

   return iRes;
}

static void leto_MemoRaw( PUSERSTRU pUStru, const char * szData )
{
   leto_Memo( pUStru, szData, NULL, NULL );
}

static void leto_Ordfunc( PUSERSTRU pUStru, const char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PAREASTRU    pAStru = pUStru->pCurAStru;
   char         szData1[ HB_PATH_MAX + 64 ];
   const char * pData = NULL;
   char *       szData2 = NULL;
   HB_ULONG     ulLen = 4;
   const char * pTagName, * pNumber = NULL, * pDopFlags = NULL, * pFlags = NULL;
   int          nParam = leto_GetParam( szData, &pTagName, &pNumber, &pFlags, &pDopFlags );

   if( nParam < 1 || ! pArea )
      pData = szErr2;
   else
   {
      if( *szData == '1' && *( szData + 1 ) == '1' )  /* DbSetOrder() for s_bNoSaveWA */
      {
         HB_ULONG ulRecNo = strtoul( pNumber, NULL, 10 );

         if( ulRecNo )  /* not really needed */
            leto_GotoIf( pArea, ulRecNo );
         leto_SetFocusIf( pUStru->pCurAStru, pArea, pTagName );

         szData2 = leto_recWithAlloc( pArea, pUStru, pAStru, &ulLen );
         if( szData2 )
            pData = szData2;
         else
            pData = szErr2;
      }
      else if( *szData == '1' && *( szData + 1 ) == '2' )  /* DBOI_KEYCOUNTRAW */
      {
         HB_ULONG ulKeyNo = leto_GetOrdInfoNL( pArea, DBOI_KEYCOUNTRAW );

         szData1[ 0 ] = '+';
         ulLen = ultostr( ulKeyNo, szData1 + 1 ) + 1;
         szData1[ ulLen++ ] = ';';
         pData = szData1;
      }
      else if( *szData == '1' && *( szData + 1 ) == '3' )  /* OrdDestroy */
      {
         HB_BOOL    bDelete = HB_FALSE;
         LETOTAG *  pTag, * pTag1, * pTagPrev = NULL;
         PINDEXSTRU pIStru = NULL;
         HB_USHORT  uiOrder = 0;

         HB_GC_LOCKT();

         pTag = pAStru->pTag;
         while( pTag )
         {
            if( ! leto_stricmp( pTag->pIStru->szTagName, pTagName ) )
               pIStru = pTag->pIStru;

            if( pIStru )
            {
               if( pIStru->uiAreas > 1 )
               {
                  if( s_iDebugMode > 0 )
                     leto_wUsLog( pUStru, -1, "ERROR leto_OrdFunc OrdDestroy bag %s in use by other",
                                  pIStru->szBagName );
                  ulLen = sprintf( szData1, "%s%s%s%s", szErr101, ":21-1006-0-1\t", pIStru->szBagName, " used by other" );
                  pData = szData1;
                  break;
               }
               else
               {
                  DBORDERINFO pOrderInfo;

                  memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
                  pOrderInfo.itmOrder = hb_itemPutC( NULL, pTagName );
                  hb_xvmSeqBegin();
                  bDelete = SELF_ORDDESTROY( pArea, &pOrderInfo ) == HB_SUCCESS;
                  hb_xvmSeqEnd();
                  hb_itemRelease( pOrderInfo.itmOrder );
                  if( pUStru->iHbError )
                     break;
               }
            }

            if( bDelete )
            {
               pTag1 = pTag;
               if( pTag == pAStru->pTag )  /* first in list */
                  pAStru->pTag = pTag->pNext;
               else
                  pTagPrev->pNext = pTag->pNext;
               if( pAStru->pTagCurrent == pTag1 )
                  pAStru->pTagCurrent = NULL;
               leto_CloseIndex( pIStru );
               letoDelItemList( &pAStru->pTStru->IndexList, ( PLETO_LIST_ITEM ) pTag1->pIStru );
               pUStru->pCurAStru->pTStru->uiIndexCount--;
               leto_FreeTag( pTag1 );

               pData = szOk;
               break;
            }

            if( pIStru )
               break;
            pTagPrev = pTag;
            pTag = pTag->pNext;
            uiOrder++;
         }
         if( pData == NULL )
            pData = szErr4;

         HB_GC_UNLOCKT();
      }
      else if( *szData == '1' && *( szData + 1 ) == '0' )  /* OrdBagClear */
      {
         HB_BOOL      bDelete = HB_FALSE, bFound = HB_FALSE, bProdIdx = HB_FALSE;
         LETOTAG *    pTag, * pTag1, * pTagPrev = NULL;
         char         szBagName[ HB_PATH_MAX ];
         PINDEXSTRU   pIStru;
         char *       ptr;
         HB_USHORT    uiLen;

         if( ! *pTagName && pAStru->pTagCurrent )  /* note this is a BAGname this case */
            strcpy( szBagName, pAStru->pTagCurrent->pIStru->szBagName );
         else
            strcpy( szBagName, pTagName );
         uiLen = ( HB_USHORT ) strlen( szBagName );
         leto_StrTran( szBagName, DEF_CH_SEP, DEF_SEP, uiLen );
         if( szBagName[ 0 ] == DEF_SEP )
            memmove( szBagName, szBagName + 1, uiLen-- );
         if( ( ptr = strchr( szBagName, '.' ) ) != NULL )
         {
            *( ptr + 1 ) = '\0';
            uiLen = ( HB_USHORT ) strlen( szBagName );
         }
         else
         {
            szBagName[ uiLen++ ] = '.';
            szBagName[ uiLen ] = '\0';
         }

         HB_GC_LOCKT();

         if( uiLen )
         {
            HB_UCHAR uNtxType = ( strstr( pAStru->pTStru->szDriver, "NTX" ) != NULL ) ? 1 : 0;

            pTag = pAStru->pTag;
            while( pTag )
            {
               /* compare Bagname against wanted one */
               if( ( ! s_bLowerPath ) ? ! strncmp( pTag->pIStru->szBagName, szBagName, uiLen ) :
                                        ! hb_strnicmp( pTag->pIStru->szBagName, szBagName, uiLen ) )
               {
                  if( ! uNtxType && hb_setGetAutOpen() && pTag->pIStru->bProduction )
                  {
                     bProdIdx = HB_TRUE;
                     break;
                  }

                  pTag->pIStru->bClear = HB_TRUE;
                  pTag->pIStru->uiAreas--;
                  if( ! pTag->pIStru->uiAreas )
                     bDelete = HB_TRUE;
                  if( s_iDebugMode > 20 )
                     leto_wUsLog( pUStru, -1, "DEBUG leto_OrdFunc OrdBagClear accepted to clear %s for %s",
                                               pTag->pIStru->szTagName, pTag->pIStru->szBagName );

                  pTag1 = pTag;
                  if( pTag == pAStru->pTag )  /* first in list */
                     pTag = pAStru->pTag = pTag->pNext;
                  else
                     pTag = pTagPrev->pNext = pTag->pNext;
                  if( pAStru->pTagCurrent == pTag1 )
                     pAStru->pTagCurrent = NULL;
                  leto_FreeTag( pTag1 );
                  bFound = HB_TRUE;
               }
               else
               {
                  pTagPrev = pTag;
                  pTag = pTag->pNext;
               }
            }
         }
         if( ! uiLen || ( ! bProdIdx && ! bFound ) )
            pData = szErr4;

         if( bDelete )  /* at least one order to remove */
         {
            DBORDERINFO pOrderInfo;
            HB_ULONG    ulMultiPos = 0, ulMultiLen = 0;
            HB_USHORT   ui;
            char *      szMultiTags = NULL;
            char *      pMultiTag;

            hb_xvmSeqBegin();
            memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
            pOrderInfo.atomBagName = hb_itemPutC( NULL, szBagName );
            if( SELF_ORDLSTDELETE( pArea, &pOrderInfo ) != HB_SUCCESS )
            {
               pData = szErr101;
               bDelete = HB_FALSE;
            }
            hb_xvmSeqEnd();
            if( pOrderInfo.atomBagName )
               hb_itemRelease( pOrderInfo.atomBagName );

            ui = 0;
            while( ui < pAStru->pTStru->uiIndexCount &&
                   ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pAStru->pTStru->IndexList, ui ) ) != NULL )
            {
               if( pUStru->iHbError )
                  break;

               if( pIStru->bClear )
               {
                  pIStru->bClear = HB_FALSE;
                  if( ! pIStru->uiAreas )
                  {
                     if( s_iDebugMode > 20 )
                        leto_wUsLog( pUStru, -1, "DEBUG leto_OrdFunc OrdBagClear de-registered %s for %s",
                                     pIStru->szTagName, pAStru->szAlias );
                     leto_CloseIndex( pIStru );
                     letoDelFromList( &pAStru->pTStru->IndexList, ui );
                     pAStru->pTStru->uiIndexCount--;
                     /* do not decrement ui ! */
                  }
                  else if( bDelete && ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
                  {
                     if( ulMultiLen - ulMultiPos < HB_PATH_MAX + 1 )
                     {
                        ulMultiLen = ulMultiPos + ( HB_PATH_MAX * 2 ) + 2;
                        if( ! ulMultiPos )
                        {
                           szMultiTags = ( char * ) hb_xgrabz( ulMultiLen );
                           szMultiTags[ 0 ] = ';';
                           ulMultiPos = 1;
                        }
                        else
                           szMultiTags = ( char * ) hb_xrealloc( szMultiTags, ulMultiLen );
                     }
                     if( ( pMultiTag = strstr( szMultiTags, pIStru->szBagName ) ) == NULL ||
                         ( *( pMultiTag - 1 ) == ';' && *( pMultiTag + strlen( pIStru->szBagName ) ) == ';' ) )
                     {
                        if( s_iDebugMode > 20 )
                           leto_wUsLog( pUStru, -1, "DEBUG leto_OrdFunc OrdBagClear re-open %s for %s",
                                        pIStru->szTagName, pAStru->szAlias );
                        memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
                        pOrderInfo.atomBagName = hb_itemPutC( pOrderInfo.atomBagName, pIStru->szFullPath );
                        pOrderInfo.itmResult = hb_itemNew( NULL );
                        hb_xvmSeqBegin();
                        SELF_ORDLSTADD( pArea, &pOrderInfo );
                        hb_xvmSeqEnd();
                        ulMultiPos += sprintf( szMultiTags + ulMultiPos, "%s;", pIStru->szBagName );
                        hb_itemRelease( pOrderInfo.itmResult );
                        hb_itemRelease( pOrderInfo.atomBagName );
                     }
                     ui++;
                  }
                  else
                     ui++;
               }
               else
                  ui++;
            }

         }
         if( ! pData )
            pData = szOk;

         HB_GC_UNLOCKT();
      }
      else if( *( szData + 1 ) == '3' )  /* *szData == '0'  Reindex */
      {
         hb_xvmSeqBegin();
         SELF_ORDLSTREBUILD( pArea );
         hb_xvmSeqEnd();
         if( pUStru->iHbError )
            pData = szErr101;
         else
            pData = szOk;
      }
      else if( *( szData + 1 ) == '4' )  /* *szData == '0'  OrdListClear() / DbClearIndex() */
      {
         PTABLESTRU pTStru = pAStru->pTStru;
         PINDEXSTRU pIStru;
         LETOTAG *  pTag = pAStru->pTag, * pTag1, * pTagPrev = NULL;
         HB_BOOL    bClear = HB_FALSE;
         HB_UCHAR   uNtxType = ( strstr( pAStru->pTStru->szDriver, "NTX" ) != NULL ) ? 1 : 0;
         HB_USHORT  ui;

         HB_GC_LOCKT();

         /* determine non production orders in list of used index, set marker if found one */
         ui = 0;
         while( ui < pTStru->uiIndexCount && ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStru->IndexList, ui ) ) != NULL )
         {
            if( uNtxType || ( ! hb_setGetAutOpen() || ! pIStru->bProduction ) )
            {
               if( s_iDebugMode > 20 )
                  leto_wUsLog( pUStru, -1, "DEBUG leto_OrdFunc OrdListClear (%s) order %s for table %s",
                               pIStru->bProduction ? "prod." : "non-prod.",pTag->szTagName, pAStru->pTStru->szTable );
               if( ! bClear )
                  bClear = HB_TRUE;

               pIStru->bClear = HB_TRUE;
            }
            ui++;
         }

         /* close private links to non production orders, decrement counter */
         if( bClear )
         {
            HB_USHORT uiCleared = 0;

            /* Now release and remove all closed user tags */
            pTag = pAStru->pTag;
            while( pTag )
            {
               if( pTag->pIStru->bClear )
               {
                  pTag1 = pTag;
                  if( pTag == pAStru->pTag )  /* first in list */
                     pTag = pAStru->pTag = pTag->pNext;
                  else
                     pTag = pTagPrev->pNext = pTag->pNext;
                  if( pAStru->pTagCurrent == pTag1 )
                     pAStru->pTagCurrent = NULL;
                  pTag1->pIStru->uiAreas--;
                  if( ! pTag1->pIStru->uiAreas )
                  {
                     pIStru = pTag1->pIStru;
                     if( s_iDebugMode > 20 )
                        leto_wUsLog( pUStru, -1, "DEBUG leto_OrdFunc OrdListClear closing %s for %s", pIStru->szTagName, pAStru->szAlias );
                     leto_CloseIndex( pIStru );
                     letoDelItemList( &pTStru->IndexList, ( PLETO_LIST_ITEM ) pIStru );
                     pTStru->uiIndexCount--;
                     uiCleared++;
                  }
                  leto_FreeTag( pTag1 );
               }
               else
               {
                  pTagPrev = pTag;
                  pTag = pTag->pNext;
               }
            }

            /* no order closed --> no ORDLSTCLEAR need, just remove marker */
            if( ! uiCleared || ( s_bNoSaveWA && ! pTStru->bMemIO ) )
            {
               if( uiCleared )
               {
                  hb_xvmSeqBegin();
                  SELF_ORDLSTCLEAR( pArea );
                  hb_xvmSeqEnd();
               }

               if( s_iDebugMode > 20 )
                  leto_wUsLog( pUStru, -1, "DEBUG leto_OrdFunc OrdListClear no re-open need for %s",  pAStru->szAlias );
               ui = 0;
               while( ui < pTStru->uiIndexCount &&
                      ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStru->IndexList, ui ) ) != NULL )
               {
                  pIStru->bClear = HB_FALSE;
                  ui++;
               }
            }
            else  /* check for orders to be re-opened for others */
            {
               DBORDERINFO pOrderInfo;
               HB_ULONG    ulMultiPos = 0, ulMultiLen = 0;
               char *      szMultiTags = NULL;
               char *      pMultiTag;

               hb_xvmSeqBegin();
               SELF_ORDLSTCLEAR( pArea );
               hb_xvmSeqEnd();

               memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
               ui = 0;
               while( ui < pTStru->uiIndexCount &&
                      ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStru->IndexList, ui ) ) != NULL )
               {
                  if( pUStru->iHbError )
                     break;

                  if( pIStru->bClear )
                  {
                     pIStru->bClear = HB_FALSE;
                     if( ulMultiLen - ulMultiPos < HB_PATH_MAX + 1 )
                     {
                        ulMultiLen = ulMultiPos + ( HB_PATH_MAX * 2 ) + 2;
                        if( ! ulMultiPos )
                        {
                           szMultiTags = ( char * ) hb_xgrabz( ulMultiLen );
                           szMultiTags[ 0 ] = ';';
                           ulMultiPos = 1;
                        }
                        else
                           szMultiTags = ( char * ) hb_xrealloc( szMultiTags, ulMultiLen );
                     }
                     if( ( pMultiTag = strstr( szMultiTags, pIStru->szBagName ) ) == NULL ||
                         ( *( pMultiTag - 1 ) == ';' && *( pMultiTag + strlen( pIStru->szBagName ) ) == ';' ) )
                     {
                        if( s_iDebugMode > 20 )
                           leto_wUsLog( pUStru, -1, "DEBUG leto_OrdFunc OrdListClear re-open %s for %s",
                                        pIStru->szTagName, pAStru->szAlias );
                        /* note: full path including filename and extension needed */
                        pOrderInfo.atomBagName = hb_itemPutC( pOrderInfo.atomBagName, pIStru->szFullPath );
                        pOrderInfo.itmResult = hb_itemNew( NULL );
                        hb_xvmSeqBegin();
                        SELF_ORDLSTADD( pArea, &pOrderInfo );
                        hb_xvmSeqEnd();
                        ulMultiPos += sprintf( szMultiTags + ulMultiPos, "%s;", pIStru->szBagName );
                        hb_itemRelease( pOrderInfo.itmResult );
                     }
                  }
                  ui++;
               }
               if( pOrderInfo.atomBagName )
                  hb_itemRelease( pOrderInfo.atomBagName );
               if( szMultiTags )
                  hb_xfree( szMultiTags );
            }
         }

         HB_GC_UNLOCKT();
         pData = szOk;
      }
      else if( *szData != '0' )
      {
         pData = szErr2;
      }
      else
      {
         /* Check, if the order name transferred */
         if( nParam < 2 )
            pData = szErr2;
         else
         {
            if( nParam >= 4 )
            {
               if( pUStru->bDeleted != ( *pFlags & 0x01 ) )
               {
                  pUStru->bDeleted = ( *pFlags & 0x01 );
                  leto_setSetDeleted( pUStru->bDeleted );
               }
            }

            if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
               leto_SetAreaEnv( pAStru, pArea, pUStru );  /* pTagName removed */
            if( pAStru->pTagCurrent )
            {
               switch( *( szData + 1 ) )
               {
                  case '1':  /* ordKeyCount */
                  {
                     HB_ULONG ulKeyCount = leto_GetOrdInfoNL( pArea, DBOI_KEYCOUNT );

                     szData1[ 0 ] = '+';
                     ulLen = ultostr( ulKeyCount, szData1 + 1 ) + 1;
                     szData1[ ulLen++ ] = ';';
                     pData = szData1;
                     break;
                  }

                  case '2':  /* ordKeyNo */
                  case '8':  /* DBOI_KEYNORAW */
                     /* Check, if the current record number is transferred */
                     if( nParam < 3 )
                        pData = szErr2;
                     else
                     {
                        HB_ULONG ulRecNo, ulKeyNo;

                        ulRecNo = strtoul( pNumber, NULL, 10 );
                        leto_GotoIf( pArea, ulRecNo );
                        ulKeyNo = leto_GetOrdInfoNL( pArea, ( *( szData + 1 ) == '2' ? DBOI_POSITION : DBOI_KEYNORAW ) );

                        szData1[ 0 ] = '+';
                        ulLen = ultostr( ulKeyNo, szData1 + 1 ) + 1;
                        szData1[ ulLen++ ] = ';';
                        pData = szData1;
                     }
                     break;

                  case '5':  /* ordKeyGoto */
                  case '9':  /* DBOI_KEYNORAW */
                  {
                     DBORDERINFO pInfo;
                     HB_ULONG    ulRecNo;

                     /* Check, if the current record number is transferred */
                     if( nParam < 3 )
                        pData = szErr2;
                     else
                     {
                        ulRecNo = strtoul( pNumber, NULL, 10 );
                        memset( &pInfo, 0, sizeof( DBORDERINFO ) );
                        pInfo.itmNewVal = hb_itemPutNL( NULL, ulRecNo );
                        pInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
                        SELF_ORDINFO( pArea, ( *( szData + 1 ) == '5' ? DBOI_POSITION : DBOI_KEYNORAW ), &pInfo );
                        if( ! hb_itemGetL( pInfo.itmResult ) )
                           pData = szErr101;
                        else
                        {
                           szData2 = leto_recWithAlloc( pArea, pUStru, pAStru, &ulLen );
                           if( szData2 )
                              pData = szData2;
                           else
                              pData = szErr2;
                        }
                        hb_itemRelease( pInfo.itmNewVal );
                        hb_itemRelease( pInfo.itmResult );
                     }
                     break;
                  }

                  case '6':  /* ordSkipUnique */
                  {
                     DBORDERINFO pInfo;
                     HB_ULONG    ulDirection, ulRecNo;

                     /* Check, if the direction is transferred */
                     if( nParam < 5 )
                        pData = szErr2;
                     else
                     {
                        ulRecNo = strtoul( pNumber, NULL, 10 );
                        ulDirection = strtoul( pDopFlags, NULL, 10 );

                        memset( &pInfo, 0, sizeof( DBORDERINFO ) );
                        pInfo.itmNewVal = hb_itemPutNL( NULL, ulDirection );
                        pInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
                        leto_GotoIf( pArea, ulRecNo );
                        SELF_ORDINFO( pArea, DBOI_SKIPUNIQUE, &pInfo );
                        if( ! hb_itemGetL( pInfo.itmResult ) )
                           pData = szErr101;
                        else
                        {
                           szData2 = leto_recWithAlloc( pArea, pUStru, pAStru, &ulLen );
                           if( szData2 )
                              pData = szData2;
                           else
                              pData = szErr2;
                        }
                        hb_itemRelease( pInfo.itmNewVal );
                        hb_itemRelease( pInfo.itmResult );
                     }
                     break;
                  }

                  case '7':  /* ordSkipWild, ordSkipRegex */
                     if( nParam < 6 )
                        pData = szErr2;
                     else
                     {
                        DBORDERINFO pInfo;
                        HB_USHORT   uiCommand;
                        HB_ULONG    ulBufLen, ulRecNo;

                        ulRecNo = strtoul( pNumber, NULL, 10 );
                        uiCommand = ( HB_USHORT ) atoi( pDopFlags );
                        pDopFlags += strlen( pDopFlags ) + 1;
                        ulBufLen = strtoul( pDopFlags, NULL, 10 );
                        pDopFlags = strchr( pDopFlags, ';' );
                        if( pDopFlags )
                        {
                           memset( &pInfo, 0, sizeof( DBORDERINFO ) );
                           pInfo.itmNewVal = hb_itemPutCL( NULL, pDopFlags + 1, ulBufLen );
                           pInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
                           leto_GotoIf( pArea, ulRecNo );
                           hb_xvmSeqBegin();
                           SELF_ORDINFO( pArea, uiCommand, &pInfo );
                           hb_xvmSeqEnd();
                           hb_itemRelease( pInfo.itmNewVal );
                           hb_itemRelease( pInfo.itmResult );

                           if( ! pUStru->iHbError )
                           {
                              szData2 = leto_recWithAlloc( pArea, pUStru, pAStru, &ulLen );
                              if( szData2 )
                                  pData = szData2;
                              else
                                 pData = szErr2;
                           }
                        }
                        else
                           pData = szErr2;
                     }
                     break;

                  default:
                     pData = szErr2;
                     break;
               }
               if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
                  leto_ClearAreaEnv( pArea, pAStru->pTagCurrent );
            }
            else
               pData = szErr4;
         }
      }

      if( pUStru->iHbError )
      {
         if( pUStru->szHbError )
         {
            ulLen = strlen( pUStru->szHbError );
            if( ulLen > 4 )
            {
               memcpy( pUStru->szHbError, szErr2, 4 );
               pData = pUStru->szHbError;
            }
            else
            {
               pData = szErr2;
               ulLen = 4;
            }
         }
         else
            pData = szErr2;
      }
   }

   leto_SendAnswer( pUStru, pData, ulLen );
   if( szData2 )
      hb_xfree( szData2 );
}

static _HB_INLINE_ void leto_BufCheck( char ** ppData, char ** pptr, HB_ULONG * pulMemSize, HB_ULONG ulMax, HB_ULONG ulTimesAdd )
{
   HB_ULONG ulPos;

   if( *pulMemSize - ( *pptr - *ppData ) <= ulMax )
   {
      ulPos = *pptr - *ppData;
      *pulMemSize += ulTimesAdd * ulMax;
      *ppData = ( char * ) hb_xrealloc( *ppData, *pulMemSize );
      *pptr = *ppData + ulPos;
   }
}

#if defined( HB_OS_LINUX )
static HB_U64 leto_LinuxRam( int uiType )
{
   FILE * pFd = NULL;
   HB_U64 ullFreeRam = 0;

   if( ( pFd = fopen( "/proc/meminfo", "r" ) ) != NULL )
   {
      char * szBuff = ( char * ) hb_xgrab( 1024 );
      int    iRead;

      iRead = fread( szBuff, 1, 1023, pFd );

      while( iRead > 0 )
      {
         char * pPos;

         szBuff[ iRead ] = '\0';

         if( uiType >= 0 )
         {
            if( ( pPos = strstr( szBuff, "MemFree:" ) ) != NULL )
            {
               pPos += 8;
               ullFreeRam += strtoull( pPos, NULL, 10 );
            }

            if( uiType < 10 )
            {
               /* add the cached area -- will be freed on need */
               if( ( pPos = strstr( szBuff, "Cached:" ) ) != NULL )
               {
                  pPos += 7;
                  ullFreeRam += strtoull( pPos, NULL, 10 );
                  break;
               }
            }
         }
         else /* total physical */
         {
            if( ( pPos = strstr( szBuff, "MemTotal:" ) ) != NULL )
            {
               pPos += 8;
               ullFreeRam += strtoull( pPos, NULL, 10 );
               break;
            }

         }

         iRead = fread( szBuff, 1, 1023, pFd );
      }

      fclose( pFd );
      hb_xfree( szBuff );
   }
   else
      ullFreeRam = 1;

   return ullFreeRam;
}
#endif

static void leto_Mgmt( PUSERSTRU pUStru, const char * szData )
{
   char *       ptr = NULL;
   HB_BOOL      bShow;
   const char * pp1 = NULL;
   const char * pp2 = NULL;
   const char * pp3 = NULL;
   int          nParam = leto_GetParam( szData, &pp1, &pp2, &pp3, NULL );

   if( ( s_bPass4M && ! ( pUStru->szAccess[ 0 ] & 0x2 ) ) || nParam < 1 || *szData != '0' )
      leto_SendAnswer( pUStru, szErrAcc, 4 );
   else
   {
      switch( *( szData + 1 ) )
      {
         case '0':   /* LETO_MGGETINFO */
         {
            char      s[ HB_PATH_MAX + HB_PATH_MAX ];
            char      s1[ 21 ], s2[ 21 ], s3[ 21 ], s4[ 21 ];
            HB_UINT   uiTablesCurr, uiTablesMax, uiIndexCurr, uiIndexMax;
            HB_USHORT uiUsersCurr, uiUsersMax;
            HB_ULONG  ulLen;

            HB_GC_LOCKU();
            uiUsersCurr = s_uiUsersCurr;
            uiUsersMax = s_uiUsersMax;
            HB_GC_UNLOCKU();

            HB_GC_LOCKT();
            uiTablesCurr = s_uiTablesCurr;
            uiTablesMax = s_uiTablesMax;
            uiIndexCurr = s_uiIndexCurr;
            uiIndexMax = s_uiIndexMax;
            /* end of PFLL games .. */
            ultostr( leto_Statistics( 1 ), s1 );
            ultostr( leto_Statistics( 2 ), s2 );
            ultostr( leto_Statistics( 3 ), s3 );
            ultostr( leto_Statistics( 4 ), s4 );
            /* ToDo: divide these values into high and low frequent changing */
            ulLen = sprintf( s, "+%d;%d;%d;%d;%f;%s;%s;%s;%u;%u;%s;%s;%d;%lu;%lu;%d;%d;%d;",
                             uiUsersCurr, uiUsersMax, uiTablesCurr, uiTablesMax,
                             0.0,
                             s1, s3, s2, uiIndexCurr, uiIndexMax,
                             ( s_pDataPath ? s_pDataPath : "" ), s4, leto_CPUCores(),
                             s_ulTransAll, s_ulTransOK, 0 /*ullFreeRam*/, 0 /*hb_xquery( 1002 )*/,
                             leto_CPULoad() );
            HB_GC_UNLOCKT();
            leto_SendAnswer( pUStru, s, ulLen );
            break;
         }

         case '1':   /* LETO_MGGETUSERS */
         {
            PUSERSTRU pUStru1;
            int       iTable = -1;
            HB_USHORT ui, uiUsers;
            HB_I64    llTimePoint = leto_MilliSec() / 1000;
            char      szRequest[ 64 ];
            HB_ULONG  ulMemSize;
            HB_ULONG  ulAreaId = 0;
            int       iListStart = 0;
            int       iListLen = 65535;
            HB_USHORT uiCount = 0;
            char *    pData;

            if( nParam >= 2 )
            {
               HB_GC_LOCKT();
               iTable = atoi( pp1 );
               if( iTable < 0 || iTable >= ( int ) s_uiTablesCurr )
                  iTable = -1;
               else
                  ulAreaId = s_tables[ iTable ].ulAreaID;
               HB_GC_UNLOCKT();
            }
            if( nParam >= 3 )
            {
               char * pTmp;

               iListStart = atoi( pp2 );
               if( iListStart < 0 )
                  iListStart = 0;
               if( ( pTmp = strchr( pp2, '<' ) ) != NULL )
                  iListLen = atoi( ++pTmp );
            }
            ulMemSize = ( HB_PATH_MAX + HB_RDD_MAX_DRIVERNAME_LEN + 256 ) * 42;
            szRequest[ 63 ] = '\0';  /* will never be overwritten below */
            pData = ( char * ) hb_xgrab( ulMemSize );
            ptr = pData;
            *ptr++ = '+'; *ptr++ = '0'; *ptr++ = '0'; *ptr++ = '0'; *ptr++ = ';';

            ui = 0;
            uiUsers = 0;

            pUStru1 = s_users;
            while( ui < s_uiUsersAlloc && uiCount < iListLen )
            {
               HB_GC_LOCKU();
               if( pUStru1->iUserStru )  /* ->hThread */
               {
                  if( iTable != -1 )  // users of a table
                  {
                     PAREASTRU       pAStru;
                     PLETO_LIST_ITEM pListItem = pUStru1->AreasList.pItem;

                     bShow = HB_FALSE;
                     while( pListItem )
                     {
                        pAStru = ( PAREASTRU ) ( pListItem + 1 );
                        if( pAStru && pAStru->pTStru && pAStru->pTStru->ulAreaID == ulAreaId )
                        {
                           bShow = HB_TRUE;
                           break;
                        }
                        pListItem = pListItem->pNext;
                     }
                  }
                  else
                     bShow = HB_TRUE;

                  if( bShow )
                  {
                     char * pTmp;

                     if( uiUsers >= iListStart )
                     {
                        const char * szNull = "(null)";

                        if( *( pUStru1->szLastRequest ) )
                           memcpy( szRequest, pUStru1->szLastRequest, 63 );
                        else
                        {
                           szRequest[ 0 ] = '?';
                           szRequest[ 1 ] = '\0';
                        }

                        pTmp = szRequest;
                        while( *pTmp && ( pTmp = strchr( pTmp, ';' ) ) != NULL )
                           *pTmp++ = ':';

                        leto_BufCheck( &pData, &ptr, &ulMemSize, HB_PATH_MAX + HB_RDD_MAX_DRIVERNAME_LEN + 256, 42 );
                        ptr += sprintf( ptr, "%d;%s;%s;%s;%ld;%s;%d;%s;%d;",
                                        ui,
                                        pUStru1->szAddr    ? ( char * ) pUStru1->szAddr    : szNull,
                                        pUStru1->szNetname ? ( char * ) pUStru1->szNetname : szNull,
                                        *( pUStru1->szExename ) ? ( char * ) pUStru1->szExename : szNull,
                                        ( long int ) ( llTimePoint - pUStru1->llLastAct ),
                                        szRequest,
                                        pUStru1->uiDriver,
                                        pUStru1->szUsername ? ( char * ) pUStru1->szUsername : szNull,
                                        pUStru1->iPort );
                        uiCount++;
                     }
                  }
                  if( ++uiUsers >= s_uiUsersCurr )
                  {
                     HB_GC_UNLOCKU();
                     break;
                  }
               }
               HB_GC_UNLOCKU();
               ui++;
               pUStru1++;
            }

            *( pData + 3 ) = ( char ) ( ( uiCount % 10 ) + 0x30 );
            *( pData + 2 ) = ( char ) ( ( ( uiCount / 10 ) % 10 ) + 0x30 );
            *( pData + 1 ) = ( char ) ( ( ( uiCount / 100 ) % 10 ) + 0x30 );
            *ptr = '\0';
            leto_SendAnswer( pUStru, pData, ptr - pData );
            hb_xfree( pData );
            break;
         }

         case '2':   /* LETO_MGGETTABLES */
         {
            HB_UINT  uiCount = 0;
            int      iUser = -1;
            HB_UINT  uiTables = 0;
            HB_ULONG ulMemSize;
            int      iListStart = 0;
            int      iListLen = s_uiTablesAlloc;
            char *   pData;

            if( nParam >= 2 )
               iUser = atoi( pp1 );
            if( nParam >= 3 )
            {
               char * pTmp;

               iListStart = atoi( pp2 );
               if( ( pTmp = strchr( pp2, '<' ) ) != NULL )
                  iListLen = atoi( ++pTmp );
            }
            ulMemSize = 42 * ( HB_RDD_MAX_ALIAS_LEN + HB_PATH_MAX + 21 );
            pData = ( char * ) hb_xgrab( ulMemSize );
            ptr = pData;
            *ptr++ = '+';
            *ptr++ = '0';
            *ptr++ = '0';
            *ptr++ = '0';
            *ptr++ = '0';
            *ptr++ = '0';
            *ptr++ = ';';
            *ptr = '\0';

            if( iUser >= 0 && iUser < ( int ) s_uiUsersAlloc )
            {
               PAREASTRU       pAStru;
               PLETO_LIST_ITEM pListItem;

               HB_GC_LOCKU();
               HB_GC_LOCKT();

               if( s_uiTablesCurr && s_users[ iUser ].iUserStru )
               {
                  iListLen = ( int ) s_uiTablesCurr;
                  pListItem = s_users[ iUser ].AreasList.pItem;
                  while( pListItem )
                  {
                     pAStru = ( PAREASTRU ) ( pListItem + 1 );
                     if( pAStru && pAStru->pTStru && pAStru->pTStru->szTable )
                     {
                        if( ( int ) uiTables >= iListStart )
                        {
                           leto_BufCheck( &pData, &ptr, &ulMemSize, HB_RDD_MAX_ALIAS_LEN + HB_PATH_MAX + 21, 42 );
                           ptr += sprintf( ptr, "%d;%s;%lu;%s%s;%c;",
                                        uiTables, pAStru->pTStru->szTable, pAStru->ulSelectID,
                                        s_users[ iUser ].pCurAStru == pAStru ? "*" : "", pAStru->szAlias,
                                        pAStru->pTStru->bShared ? 'T' : 'F' );
                           uiCount++;
                           if( iListLen-- < 1 )
                              break;
                        }
                        uiTables++;
                     }
                     pListItem = pListItem->pNext;
                  }
               }
               HB_GC_UNLOCKT();
               HB_GC_UNLOCKU();
            }
            else
            {
               PTABLESTRU pTStru1 = s_tables;
               HB_BOOL    bAdd = HB_TRUE;
               HB_UINT    ui = 0;

               HB_GC_LOCKT();

               if( iListLen > ( int ) s_uiTablesCurr )
                  iListLen = ( int ) s_uiTablesCurr;
               while( ui < s_uiTablesAlloc )
               {
                  if( pTStru1->szTable )
                  {
                     if( ( int ) uiTables >= iListStart )
                     {
                        leto_BufCheck( &pData, &ptr, &ulMemSize, HB_RDD_MAX_ALIAS_LEN + HB_PATH_MAX + 21, 42 );
                        if( s_bNoSaveWA )  /* filter double table names */
                        {
                           char * ptr2 = strstr( pData, ( const char * ) pTStru1->szTable );

                           if( ptr2 != NULL && *( ptr2 - 1 ) == ';' )
                              bAdd = HB_FALSE;
                           else
                              bAdd = HB_TRUE;
                        }
                        if( bAdd )
                        {
                           ptr += sprintf( ptr, "%d;%s;0;;%c;",
                                        uiTables, ( char * ) pTStru1->szTable,
                                        pTStru1->bShared ? 'T' : 'F' );
                           uiCount++;
                           if( iListLen-- < 1 )
                              break;
                        }
                     }
                     if( ++uiTables >= s_uiTablesCurr )
                        break;
                  }
                  ui++;
                  pTStru1++;
               }

               HB_GC_UNLOCKT();
            }

            if( uiCount )
            {
               *( pData + 5 ) = ( char ) ( ( uiCount % 10 ) + 0x30 );
               *( pData + 4 ) = ( char ) ( ( ( uiCount / 10 ) % 10 ) + 0x30 );
               *( pData + 3 ) = ( char ) ( ( ( uiCount / 100 ) % 10 ) + 0x30 );
               *( pData + 2 ) = ( char ) ( ( ( uiCount / 1000 ) % 10 ) + 0x30 );
               *( pData + 1 ) = ( char ) ( ( ( uiCount / 10000 ) % 10 ) + 0x30 );
               leto_SendAnswer( pUStru, pData, ptr - pData );
            }
            else
               leto_SendAnswer( pUStru, "+000;", 5 );
            if( pData )
               hb_xfree( pData );

            break;
         }

         case '3': /* LETO_MGGETTIME */
         {
            char     s[ 64 ];
            int      iYear, iMonth, iDay;
            HB_ULONG ulLen;

            hb_dateToday( &iYear, &iMonth, &iDay );
            ulLen = sprintf( s, "+%lu;%9.3f;%lu;", hb_dateEncode( iYear, iMonth, iDay ), hb_dateSeconds(),
                         ( ( unsigned long ) hb_dateMilliSeconds() / 1000 ) - s_ulStartDateSec );
            leto_SendAnswer( pUStru, s, ulLen );
            break;
         }

         case '4': /* LETO_MGGETLOCKS */
         {
            HB_USHORT       uiCount = 0;
            PLETO_LOCK_ITEM pLockA;
            int             iUser = -1;
            HB_ULONG        ulMemSize = 0;
            int             iListLen = 999;
            char *          pData = NULL;

            if( nParam >= 2 )
            {
               iUser = atoi( pp1 );
               if( iUser < 0 || iUser >= s_uiUsersMax )
                  iUser = -1;
            }
            if( nParam >= 3 )
            {
               if( ! pp2 || ! *pp2 )
                  pp2 = NULL;
            }
            if( nParam >= 4 )
            {
               char * pTmp;

                if( ( pTmp = strchr( pp3, '<' ) ) != NULL )
                  iListLen = atoi( ++pTmp );
            }

            if( iUser >= 0 && iUser < ( int ) s_uiUsersAlloc )
            {
#if 0  /*  user locks list is by intention not mutex secured, temporary outcommented until changed */
               PAREASTRU       pAStru;
               PLETO_LIST_ITEM pListItem;

               HB_GC_LOCKT();

               if( s_users[ iUser ].iUserStru )
               {
                  pListItem = s_users[ iUser ].AreasList.pItem;
                  while( pListItem )
                  {
                     pAStru = ( PAREASTRU ) ( pListItem + 1 );
                     if( pAStru && pAStru->pTStru && pAStru->pTStru->szTable &&
                         ( pp2 ? ! strcmp( ( const char * ) pAStru->pTStru->szTable, pp2 ) : HB_TRUE ) )
                     {
                        if( ! pData )
                        {
                           ulMemSize = HB_PATH_MAX + 42;
                           pData = hb_xgrab( ulMemSize );
                           ptr = pData;
                           *ptr++ = '+';
                           *ptr++ = '0';
                           *ptr++ = '0';
                           *ptr++ = '0';
                           *ptr++ = ';';
                        }
                        else
                           leto_BufCheck( &pData, &ptr, &ulMemSize, HB_PATH_MAX + 42, 21 );
                        ptr += sprintf( ptr, "%s;", ( char * ) pAStru->pTStru->szTable );

                        if( ! s_bNoSaveWA || pAStru->pTStru->bMemIO )
                        {
                           /* ToDo ToFix check if it's problem as local list is not mutex secured */
                           for( pLockA = ( PLETO_LOCK_ITEM ) pAStru->LocksList.pItem; pLockA; pLockA = pLockA->pNext )
                           {
                              leto_BufCheck( &pData, &ptr, &ulMemSize, HB_PATH_MAX + 42, 21 );
                              if( *( ptr - 1 ) != ';' )
                                 *ptr++ = ',';
                              ptr += sprintf( ptr, "%lu", pLockA->ulRecNo );
                              if( uiCount++ > iListLen )
                                 break;
                           }
                        }
                        else
                        {
                           letoListLock( &pAStru->pTStru->LocksList );
                           for( pLockA = ( PLETO_LOCK_ITEM ) pAStru->pTStru->LocksList.pItem; pLockA; pLockA = pLockA->pNext )
                           {
                              leto_BufCheck( &pData, &ptr, &ulMemSize, HB_PATH_MAX + 42, 21 );
                              if( *( ptr - 1 ) != ';' )
                                 *ptr++ = ',';
                              ptr += sprintf( ptr, "%lu", pLockA->ulRecNo );
                              if( uiCount++ > iListLen )
                                 break;
                           }
                           letoListUnlock( &pAStru->pTStru->LocksList );
                        }
                        *ptr++ = ';';
                        if( pp2 )
                           break;
                     }
                     pListItem = pListItem->pNext;
                  }
               }

               HB_GC_UNLOCKT();
#endif
            }
            else
            {
               PTABLESTRU pTStru1;
               HB_UINT    ui = 0;
               HB_UINT    uiTables = 0;
               int        iTableStru = 0;

               HB_GC_LOCKT();

               if( pp2 )
                  iTableStru = leto_FindTable( pp2, NULL );

               if( iTableStru >= 0 )
                  ui = iTableStru;

               pTStru1 = s_tables + ui;
               while( ui < s_uiTablesAlloc && uiCount < ( HB_USHORT ) iListLen )
               {
                  if( pTStru1->szTable )
                  {
                     if( pp2 ? ! strcmp( ( const char * ) pTStru1->szTable, pp2 ) : HB_TRUE )
                     {
                        if( ! pData )
                        {
                           ulMemSize = HB_PATH_MAX + 42;
                           pData = hb_xgrab( ulMemSize );
                           ptr = pData;
                           *ptr++ = '+';
                           *ptr++ = '0';
                           *ptr++ = '0';
                           *ptr++ = '0';
                           *ptr++ = ';';
                        }
                        else
                           leto_BufCheck( &pData, &ptr, &ulMemSize, HB_PATH_MAX + 42, 21 );
                        ptr += sprintf( ptr, "%s;", ( char * ) pTStru1->szTable );

                        // ToDo scan through same tables in mode s_bNoSaveWA
                        letoListLock( &pTStru1->LocksList );
                        for( pLockA = ( PLETO_LOCK_ITEM ) pTStru1->LocksList.pItem; pLockA; pLockA = pLockA->pNext )
                        {
                           leto_BufCheck( &pData, &ptr, &ulMemSize, HB_PATH_MAX + 42, 21 );
                           if( *( ptr - 1 ) != ';' )
                              *ptr++ = ',';
                           ptr += sprintf( ptr, "%lu", pLockA->ulRecNo );
                           if( uiCount++ > iListLen )
                              break;
                        }
                        letoListUnlock( &pTStru1->LocksList );
                        *ptr++ = ';';
                        if( pp2 && ( ! s_bNoSaveWA || pTStru1->bMemIO ) )
                           break;
                     }
                     if( ++uiTables >= s_uiTablesCurr )
                        break;
                  }
                  ui++;
                  pTStru1++;
               }

               HB_GC_UNLOCKT();
            }

            if( uiCount )
            {
               *( pData + 3 ) = ( char ) ( ( uiCount % 10 ) + 0x30 );
               *( pData + 2 ) = ( char ) ( ( ( uiCount / 10 ) % 10 ) + 0x30 );
               *( pData + 1 ) = ( char ) ( ( ( uiCount / 100 ) % 10 ) + 0x30 );
               leto_SendAnswer( pUStru, pData, ptr - pData );
            }
            else
               leto_SendAnswer( pUStru, "+000;", 5 );
            if( pData )
               hb_xfree( pData );

            break;
         }

         case '5':   /* LETO_MGSYSINFO -- need no lock of static var, but may be time intensive */
         {
            char     s[ HB_PATH_MAX + 90 ];
            HB_ULONG ulLen;
#if defined( HB_OS_LINUX )
            unsigned long ulFreeRam = ( unsigned long ) leto_LinuxRam( 0 );
#else
            unsigned long ulFreeRam = ( unsigned long ) hb_xquery( HB_MEM_CHAR );
#endif

            ulLen = sprintf( s, "+%lu;%s;%d;%lu;%d;%lu;%lu;%lu;%lu;",
#if defined( __HARBOUR30__ )
                      0L,
#else
                      ( unsigned long ) hb_fsDiskSpace( s_pDataPath, HB_DISK_AVAIL ),
#endif
                      s_pDataPath,
                      leto_CPUCores(),
                      ulFreeRam,
                      s_bNoSaveWA ? ( s_bShareTables ? 4 : 3 ) : ( s_bShareTables ? 2 : 1 ),
                      ( unsigned long ) hb_xquery( HB_MEM_USEDMAX ),
                      ( unsigned long ) hb_xquery( HB_MEM_STACKITEMS ),
                      ( unsigned long ) hb_xquery( HB_MEM_STACK ),
                      ( unsigned long ) hb_xquery( HB_MEM_STACK_TOP ) );
            leto_SendAnswer( pUStru, s, ulLen );
            break;
         }

         case '6':   /* LETO_MGGETINDEX */
         {
            int        iUser = -1;
            HB_UINT    ui = 0;
            HB_USHORT  uiIndex;
            HB_UINT    uiIndexAll = 0;
            HB_ULONG   ulMemSize;
            PINDEXSTRU pIStru;
            int        iListStart = 0;
            int        iListLen = 999;
            int        uiCount = 0;
            char *     pData;

            if( nParam >= 2 )
            {
               iUser = atoi( pp1 );
               if( iUser < 0 )
                  iUser = -1;
            }
            if( nParam >= 3 )
            {
               if( ! pp2 || ! *pp2 )
                  pp2 = NULL;
            }
            if( nParam >= 4 )
            {
               char * pTmp;

               iListStart = atoi( pp3 );
               if( ( pTmp = strchr( pp3, '<' ) ) != NULL )
                  iListLen = atoi( ++pTmp );
            }
            ulMemSize = ( LETO_MAX_TAGNAME + HB_PATH_MAX + 256 + 21 ) * 42;
            pData = ( char * ) hb_xgrab( ulMemSize );
            ptr = pData;
            *ptr++ = '+';
            *ptr++ = '0';
            *ptr++ = '0';
            *ptr++ = '0';
            *ptr++ = '0';
            *ptr++ = '0';
            *ptr++ = '0';
            *ptr++ = ';';
            *ptr = '\0';

            /* only for one specified connection, possible also table specfic */
            if( iUser >= 0 && iUser < ( int ) s_uiUsersAlloc )
            {
               PLETO_LIST_ITEM pListItem;
               char            szTagCompare[ LETO_MAX_TAGNAME + HB_PATH_MAX + 256 + 21 ];

               //HB_GC_LOCKU();
               HB_GC_LOCKT();

               if( s_uiTablesCurr && s_users[ iUser ].iUserStru )
               {
                  pListItem = s_users[ iUser ].AreasList.pItem;
                  while( pListItem )
                  {
                     PAREASTRU  pAStru = ( PAREASTRU ) ( pListItem + 1 );
                     PUSERSTRU  pUStru1 = s_users + iUser;
                     PINDEXSTRU pCurIndex = pUStru1->pCurAStru && pUStru1->pCurAStru->pTagCurrent ?
                                            pUStru1->pCurAStru->pTagCurrent->pIStru : NULL;

                     if( pAStru && pAStru->pTStru && pAStru->pTStru->szTable &&
                         ( pp2 ? ( strstr( ( const char * ) pAStru->pTStru->szTable, pp2 ) != NULL ) : HB_TRUE ) )
                     {
                        LETOTAG * pTag = pAStru->pTag;

#if 1
                        while( pTag )
                        {
                           pIStru = pTag->pIStru;
                           leto_BufCheck( &pData, &ptr, &ulMemSize, LETO_MAX_TAGNAME + HB_PATH_MAX + 256 + 21, 42 );
                           if( pIStru->szBagName )
                           {
                              sprintf( szTagCompare, ";%s;%s%s;", pIStru->szBagName,
                                                                     pCurIndex == pIStru ? "*" : "",
                                                                     pIStru->szTagName );
                              if( uiIndexAll >= ( HB_UINT ) iListStart )
                              {
                                 if( ! pp2 || strstr( pData, szTagCompare ) == NULL )
                                 {
                                    ptr += sprintf( ptr, "%d%s%s%s%s;", uiIndexAll, szTagCompare, pIStru->szOrdKey,
                                                    pTag->pTopScope ? ">" : "", pTag->pBottomScope ? "<" : ""  );
                                    uiCount++;
                                    if( iListLen-- < 1 )
                                       break;
                                 }
                              }
                              uiIndexAll++;
                           }
                           pTag = pTag->pNext;
                        }
#else
                        uiIndex = 0;
                        while( ( HB_USHORT ) uiIndex < pAStru->pTStru->uiIndexCount &&
                               ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pAStru->pTStru->IndexList, uiIndex ) ) != NULL )
                        {
                           leto_BufCheck( &pData, &ptr, &ulMemSize, LETO_MAX_TAGNAME + HB_PATH_MAX + 256 + 21, 42 );
                           if( pIStru->szBagName )
                           {
                              sprintf( szTagCompare, ";%s;%s%s;%s;", pIStru->szBagName,
                                       pCurIndex == pIStru ? "*" : "",
                                       pIStru->szTagName, pIStru->szOrdKey );
                              if( uiIndexAll >= ( HB_UINT ) iListStart )
                              {
                                 if( ! pp2 || strstr( pData, szTagCompare ) == NULL )
                                 {
                                    ptr += sprintf( ptr, "%d%s", uiIndexAll, szTagCompare );
                                    uiCount++;
                                    if( iListLen-- < 1 )
                                       break;
                                 }
                              }
                              uiIndexAll++;
                           }
                           uiIndex++;
                        }
#endif
                        if( pp2 )
                           break;
                     }
                     pListItem = pListItem->pNext;
                  }
               }

               HB_GC_UNLOCKT();
               //HB_GC_UNLOCKU();
            }
            else  /* connection unrelated, possible table specific */
            {
               PTABLESTRU pTStru1;
               HB_BOOL    bAdd = HB_TRUE;
               HB_UINT    uiTables = 0;
               char       szTagCompare[ LETO_MAX_TAGNAME + HB_PATH_MAX + 256 + 21 ];
               int        iTableStru = 0;

               HB_GC_LOCKT();

               if( iListLen == 999 )
                  iListLen = ( int ) s_uiIndexCurr;

               if( pp2 )
                  iTableStru = leto_FindTable( pp2, NULL );

               if( iTableStru >= 0 && s_uiTablesCurr )
               {
                  szTagCompare[ 0 ] = '\0';
                  ui = iTableStru;
                  pTStru1 = s_tables + ui;
                  while( ui < s_uiTablesAlloc && uiIndexAll <  ( HB_UINT ) iListLen )
                  {
                     if( pTStru1->szTable )
                     {
                        if( pp2 ? strstr( ( const char * ) pTStru1->szTable, pp2 ) != NULL : HB_TRUE )
                        {
                           uiIndex = 0;
                           while( uiIndex < pTStru1->uiIndexCount &&
                                  ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStru1->IndexList, uiIndex ) ) != NULL )
                           {
                              leto_BufCheck( &pData, &ptr, &ulMemSize, LETO_MAX_TAGNAME + HB_PATH_MAX + 256 + 21, 42 );

                              /* filter double index BAG names */
                              if( ! pp2 && s_bNoSaveWA )
                              {
                                 char * ptr2;

                                 if( ! pIStru->szBagName )
                                    bAdd = HB_FALSE;
                                 else if( ( ptr2 = strstr( pData, ( const char * ) pIStru->szBagName ) ) != NULL )
                                 {
                                    if( *( ptr2 - 1 ) == ';' )
                                       bAdd = HB_FALSE;
                                    else
                                       bAdd = HB_TRUE;
                                 }
                                 else
                                    bAdd = HB_TRUE;
                              }
                              else
                              {
                                 if( pIStru->szBagName )
                                 {
                                    sprintf( szTagCompare, ";%s;%s;", pIStru->szBagName, pIStru->szTagName );
                                    if( ! pp2 && strstr( pData, szTagCompare ) != NULL )
                                       bAdd = HB_FALSE;
                                    else
                                       bAdd = HB_TRUE;
                                 }
                              }

                              if( bAdd )
                              {
                                 if( uiIndexAll >= ( HB_UINT ) iListStart )
                                 {
                                    ptr += sprintf( ptr, "%d;%s;%s;%s;",
                                                    uiIndexAll, ( char * ) pIStru->szBagName, pIStru->szTagName, pIStru->szOrdKey );
                                    uiCount++;
                                    if( iListLen-- < 1 )
                                       break;
                                 }
                                 uiIndexAll++;
                              }
                              uiIndex++;
                           }
                           if( pp2 )
                              break;
                        }
                        if( ++uiTables >= s_uiTablesCurr )
                           break;
                     }
                     ui++;
                     pTStru1++;
                  }
               }

               HB_GC_UNLOCKT();
            }

            *( pData + 6 ) = ( char ) ( ( uiCount % 10 ) + 0x30 );
            *( pData + 5 ) = ( char ) ( ( ( uiCount / 10 ) % 10 ) + 0x30 );
            *( pData + 4 ) = ( char ) ( ( ( uiCount / 100 ) % 10 ) + 0x30 );
            *( pData + 3 ) = ( char ) ( ( ( uiCount / 1000 ) % 10 ) + 0x30 );
            *( pData + 2 ) = ( char ) ( ( ( uiCount / 10000 ) % 10 ) + 0x30 );
            *( pData + 1 ) = ( char ) ( ( ( uiCount / 100000 ) % 10 ) + 0x30 );
            leto_SendAnswer( pUStru, pData, ptr - pData );
            hb_xfree( pData );
            break;
         }

         case '7':   /* LETO_MGLOG */
         {
            int     iUser = atoi( pp1 );
            int     iRows = atoi( pp2 );
            HB_SIZE nLen = 0;
            char *  szContent;

            if( iRows < 0 )
               iRows = 0;

            szContent = leto_readServerLog( iUser, iRows, &nLen );
            leto_SendAnswer( pUStru, szContent, nLen );
            hb_xfree( szContent );
            break;
         }

         case '8':   /* LETO_MGID */
         {
            char szMyId[ 16 ];

            sprintf( szMyId, "%s;%d;", szOk, pUStru->iUserStru - 1 );
            leto_SendAnswer( pUStru, szMyId, strlen( szMyId ) );
            break;
         }

         case '9':   /* LETO_MGKILL -- except threads with no socket */
         {
            int     iUser = -1;
            HB_BOOL bOk = HB_FALSE;
            int     iKilled = 0;

            if( nParam == 2 && *pp1 )
            {
               if( strchr( pp1, '.' ) )  /* by address */
               {
                  PUSERSTRU pUStru1;
                  HB_USHORT ui = 0, uiChecked = 0;

                  //HB_GC_LOCKU();

                  pUStru1 = s_users;
                  while( ui < s_uiUsersAlloc )
                  {
                     if( pUStru != pUStru1 && pUStru1->hSocket != HB_NO_SOCKET &&
                         pUStru1->szAddr && ! strcmp( ( char * ) pUStru1->szAddr, pp1 ) )
                     {
                        hb_threadEnterCriticalSection( &pUStru1->pMutex );
                        if( pUStru1->iUserStru && pUStru1->hSocket != HB_NO_SOCKET )
                        {
                           if( hb_socketShutdown( pUStru1->hSocket, HB_SOCKET_SHUT_RDWR ) == 0 )
                           {
                              if( pUStru1->hSockPipe[ 1 ] != FS_ERROR )
                              {
                                 const char cToPipe[ 1 ] = { ' ' };

                                 hb_fsPipeWrite( pUStru1->hSockPipe[ 1 ], &cToPipe, 1, 0 );
                              }
                              iKilled = pUStru1->iUserStru;
                           }
                           else  /* the hard way, kill socket and close areas */
                           {
                              if( pUStru1->hSocketErr )
                              {
                                 hb_socketClose( pUStru1->hSocketErr );
                                 pUStru1->hSocketErr = HB_NO_SOCKET;
                              }

                              hb_socketClose( pUStru1->hSocket );
                              pUStru1->hSocket = HB_NO_SOCKET;
                              if( pUStru1->zstream )
                              {
#ifdef USE_LZ4
                                 hb_lz4netClose( pUStru1->zstream );
#else
                                 hb_znetClose( pUStru1->zstream );
#endif
                                 pUStru1->zstream = NULL;
                              }
                              leto_CloseAll4Us( pUStru1 );    /* ToDo verify */
                              iKilled = pUStru1->iUserStru;
                           }
                           if( ++uiChecked >= s_uiUsersCurr )
                              break;
                           pUStru1->bCloseConnection = HB_TRUE;
                        }
                        hb_threadLeaveCriticalSection( &pUStru1->pMutex );
                     }
                     ui++;
                     pUStru1++;
                  }

                  //HB_GC_UNLOCKU();
                  bOk = HB_TRUE;
                  if( s_iDebugMode > 0 )
                     leto_writelog( NULL, -1, "DEBUG connection %d closed by management", iKilled );

               }
               else  /* by connection-ID, also possible for socketless RPC */
               {
                  PUSERSTRU pUStru1;
                  char      szInfo[ HB_PATH_MAX + HB_PATH_MAX + 42 ];

                  szInfo[ 0 ] = '\0';
                  iUser = atoi( pp1 );
                  if( iUser >= 0 && iUser < s_uiUsersAlloc )
                  {
                     //HB_GC_LOCKU();

                     pUStru1 = s_users + iUser;
                     hb_threadEnterCriticalSection( &pUStru1->pMutex );
                     if( pUStru != pUStru1 && pUStru1->iUserStru && pUStru1->hSocket != HB_NO_SOCKET )
                     {
                        sprintf( szInfo, "%s:%d %s %s",
                                 ( pUStru1->szAddr    ? ( char * ) pUStru1->szAddr    : "0" ),
                                 ( pUStru1->iPort     ? ( int ) pUStru1->iPort     : 0 ),
                                 ( pUStru1->szNetname ? ( char * ) pUStru1->szNetname : "?" ),
                                 ( *( pUStru1->szExename ) ? ( char * ) pUStru1->szExename : "?" ) );
                        if( hb_socketShutdown( pUStru1->hSocket, HB_SOCKET_SHUT_RDWR ) == 0 )
                        {
                           if( pUStru1->hSockPipe[ 1 ] != FS_ERROR )
                           {
                              const char cToPipe[ 1 ] = { ' ' };

                              hb_fsPipeWrite( pUStru1->hSockPipe[ 1 ], &cToPipe, 1, 0 );
                           }
                           iKilled = pUStru1->iUserStru;
                        }
                        else  /* above wakes up thread with 'bad' socket to let it close All4Us */
                        {
                           if( pUStru1->hSocketErr )
                           {
                              hb_socketClose( pUStru1->hSocketErr );
                              pUStru1->hSocketErr = HB_NO_SOCKET;
                           }

                           hb_socketClose( pUStru1->hSocket );
                           pUStru1->hSocket = HB_NO_SOCKET;
                           if( pUStru1->zstream )
                           {
#ifdef USE_LZ4
                              hb_lz4netClose( pUStru1->zstream );
#else
                              hb_znetClose( pUStru1->zstream );
#endif
                              pUStru1->zstream = NULL;
                           }
                           leto_CloseAll4Us( pUStru1 );  /* ToDo - verify elch */
                           iKilled = pUStru1->iUserStru;
                        }
                        pUStru1->bCloseConnection = HB_TRUE;
                        bOk = HB_TRUE;
                     }
                     else if( pUStru != pUStru1 && pUStru1->iUserStru )  /* a headless UDF */
                     {
                        sprintf( szInfo, "%s:%d %s %s",
                                 ( pUStru1->szAddr    ? ( char * ) pUStru1->szAddr    : "RPC.thread" ),
                                 ( pUStru1->iPort     ? ( int ) pUStru1->iPort     : 0 ),
                                 ( pUStru1->szNetname ? ( char * ) pUStru1->szNetname : "?" ),
                                 ( *(pUStru1->szExename ) ? ( char * ) pUStru1->szExename : "?" ) );
                        pUStru1->bCloseConnection = HB_TRUE;
                        iKilled = pUStru1->iUserStru;
                        bOk = HB_TRUE;
                     }

                     hb_threadLeaveCriticalSection( &pUStru1->pMutex );
                     //HB_GC_UNLOCKU();
                     if( bOk && s_iDebugMode > 0 )
                        leto_writelog( NULL, -1, "DEBUG connection %d closed by management (%s)", iKilled, szInfo );
                  }
               }
            }
            if( bOk )
            {
               char szAnswer[ 21 ];

               sprintf( szAnswer, "%s;%d;", szOk, --iKilled );  /* iUserStru ID is connection Nr + 1 */
               leto_SendAnswer( pUStru, szAnswer, strlen( szAnswer ) );
            }
            else
               leto_SendAnswer( pUStru, szErrAcc, 4 );
            break;
         }

         default:
            leto_SendAnswer( pUStru, szErr2, 4 );
            break;
      }
   }
}

/* remove drive letter and duplicated path separator at beginning */
static void leto_BeautifyPath( char * szPath )
{
   char * ptr = szPath;

   if( *ptr && ptr[ 1 ] == ':' )  /* C:... */
      memmove( szPath, ptr + 2, strlen( ptr + 2 ) + 1 );  /* including '\0' */

   leto_StrTran( ptr, DEF_CH_SEP, DEF_SEP, strlen( ptr ) );
   if( *ptr == DEF_SEP )
   {
      ptr++;
      while( *ptr == DEF_SEP )
      {
         memmove( ptr, ptr + 1, strlen( ptr + 1 ) + 1 );  /* including '\0' */
      }
   }
}

static void leto_PathFinder( char * szOnePath, const char * szDataPath )
{
   char     szTmpPath[ HB_PATH_MAX ];
   char *   pContain = strstr( szOnePath, szDataPath );
   HB_ULONG ulLenOne;

   if( pContain )
   {
      ulLenOne = ( pContain - szOnePath ) + strlen( szDataPath );
      strcpy( szTmpPath, szOnePath + ulLenOne );
      leto_BeautifyPath( szTmpPath );
      strcpy( szOnePath, szTmpPath );
   }
   else
   {
      char * pTmp;

      strcpy( szTmpPath, szOnePath );
      while( ( pTmp = strrchr( szTmpPath, DEF_SEP ) ) != NULL )
      {
         ulLenOne = pTmp - szTmpPath;
         if( ! ulLenOne )
            break;
         memcpy( szTmpPath, szOnePath, ulLenOne );
         szTmpPath[ ulLenOne ] = '\0';

         pContain = strstr( szDataPath, szTmpPath );
         if( pContain )
         {
            strcpy( szTmpPath, szOnePath + ( pTmp - szTmpPath ) );
            leto_BeautifyPath( szTmpPath );
            strcpy( szOnePath, szTmpPath );
            break;
         }
      }
   }
}

static void leto_Intro( PUSERSTRU pUStru, const char * szData )
{
   const char * pData = NULL;
   const char * pp1 = NULL, * pp2 = NULL, * pp3 = NULL, * pp4 = NULL;
   HB_BOOL      bSuccess = HB_FALSE;
   int          nParam = leto_GetParam( szData, &pp1, &pp2, &pp3, &pp4 );

   if( nParam < 3 )
      pData = szErr2;
   else
   {
      HB_ULONG ulLen = 0;

      pUStru->szVersion = ( char * ) hb_xgrab( strlen( szData ) + 1 );
      strcpy( ( char * ) pUStru->szVersion, szData );
      sscanf( pUStru->szVersion, "%u.%u", &pUStru->uiMajorVer, &pUStru->uiMinorVer );

      pUStru->szNetname = ( HB_BYTE * ) hb_xgrab( strlen( pp1 ) + 1 );
      strcpy( ( char * ) pUStru->szNetname, pp1 );

      strncpy( ( char * ) pUStru->szExename, pp2, 20 );
      pUStru->szExename[ strlen( ( const char * ) pUStru->szExename ) ] = '\0';

      /* ever store a given username, independent from login restrictions */
      if( nParam >= 4 && ( ulLen = strlen( pp3 ) ) > 0 )
      {
         pUStru->szUsername = ( HB_BYTE * ) hb_xgrab( ulLen + 1 );
         strcpy( ( char * ) pUStru->szUsername, pp3 );
      }

      if( s_bPass4L || s_bPass4M || s_bPass4D )
      {
         HB_BOOL fAccepted = HB_FALSE;

         if( nParam >= 5 && ulLen > 0 && ulLen <= LETO_MAX_USERNAME )
         {
            char * szPass;

            ulLen = strlen( pp4 );
            szPass = ( char * ) hb_xgrab( ( ulLen / 2 ) + 1 );
            if( ulLen > 0 && ulLen < 50 )
            {
               char * szKey = leto_localKey( pUStru->cDopcode, LETO_DOPCODE_LEN );

               leto_hexchar2byte( pp4, ulLen, szPass );
               leto_decrypt( szPass, ulLen / 2, szPass, &ulLen, szKey, HB_FALSE );
               if( szKey )
                  hb_xfree( szKey );
            }
            else
               szPass[ 0 ] = '\0';

            fAccepted = leto_acc_find( ( const char * ) pUStru->szUsername, szPass, ( char ** ) &pUStru->szAccess );
            hb_xfree( szPass );
         }
         if( ! fAccepted )
         {
#if 0  /* no answer if Pass wrong, close Connection */
            if( s_bPass4L )
            {
               pUStru->bNoAnswer = HB_TRUE;
               pUStru->bCloseConnection = HB_TRUE;
               return;
            }
            else  /* send wrong access answer, close connection */
#endif
            {
               pUStru->bCloseConnection = HB_TRUE;
               pData = szErrAcc;
            }
         }
      }
      else
         pUStru->szAccess[ 0 ] = pUStru->szAccess[ 1 ] = 0xFF;

      if( ! pData && nParam > 5 )
      {
         const char * pp5;

         if( ! *pp4 )  /* find last valid param */
         {
            if( ! *pp3 )
               pp5 = pp2 + strlen( pp2 ) + 3;
            else
               pp5 = pp3 + strlen( pp3 ) + 2;
         }
         else
            pp5 = pp4 + strlen( pp4 ) + 1;

         if( pp5 && *pp5 )  /* pp5 terminated by leto_GetParam() */
         {
            pUStru->cdpage = hb_cdpFind( pp5 );
            if( pUStru->cdpage )
            {
               hb_vmSetCDP( pUStru->cdpage );
               if( s_iDebugMode > 10 )
                  leto_wUsLog( pUStru, -1, "DEBUG leto_Intro() -- Client CP %s is set", pp5 );
            }
            else
               leto_wUsLog( pUStru, -1, "ERROR leto_Intro() -- CP: %s not found", pp5 );

            pp5 += strlen( pp5 ) + 1;
         }

         if( pp5 && *pp5 )
         {
            char *   ptr;
            PHB_ITEM pItem = NULL;

            if( ( ptr = strchr( pp5, ';' ) ) != NULL )
            {
               pUStru->szDateFormat = ( char * ) hb_xgrab( ptr - pp5 + 1 );
               memcpy( pUStru->szDateFormat, pp5, ptr - pp5 );
               pUStru->szDateFormat[ ptr - pp5 ] = '\0';
               if( pUStru->szDateFormat )
               {
                  pItem = hb_itemPutC( pItem, pUStru->szDateFormat );
                  hb_setSetItem( HB_SET_DATEFORMAT, pItem );
                  hb_itemClear( pItem );
               }

               /* epoch */
               pUStru->uiEpoch = ( unsigned int ) atoi( ++ptr );
               if( pUStru->uiEpoch )
               {
                  pItem = hb_itemPutNI( pItem, pUStru->uiEpoch );
                  hb_setSetItem( HB_SET_EPOCH, pItem );
               }
               if( pItem )
               {
                  hb_itemRelease( pItem );
                  pItem = NULL;
               }

               /* one ! default path, not allowed to be terminated with ';' ! -- need newest client lib */
               if( *ptr && ( ptr = strchr( ptr, ';' ) ) != NULL )
               {
                  char *    pEnd = strchr( ++ptr, ';' );
                  HB_USHORT uiLen = pEnd ? ( HB_USHORT ) ( pEnd - ptr ) : 0;

                  if( uiLen )
                  {
                     char * szOnePath = ( char * ) hb_xgrab( HB_PATH_MAX );
                     char * szDefaultPath = ( char * ) hb_xgrab(  HB_PATH_MAX );

                     memcpy( szOnePath, ptr, HB_MIN( uiLen, HB_PATH_MAX - 1 ) );
                     szOnePath[ HB_MIN( uiLen, HB_PATH_MAX - 1 ) ] = '\0';
                     leto_BeautifyPath( szOnePath );
                     leto_PathFinder( szOnePath, s_pDataPath );

                     uiLen = ( HB_USHORT ) strlen( szOnePath );
                     if( uiLen > 1 && szOnePath[ uiLen - 1 ] == DEF_SEP )
                        szOnePath[ uiLen - 1 ] = '\0';
                     leto_DataPath( szOnePath, szDefaultPath );
                     pItem = hb_itemPutC( pItem, szDefaultPath );
                     hb_setSetItem( HB_SET_DEFAULT, pItem );
                     pItem = hb_itemPutC( pItem, s_pDataPath );
                     hb_setSetItem( HB_SET_PATH, pItem );
                     hb_itemRelease( pItem );
                     pItem = NULL;
                     hb_xfree( szDefaultPath );
                     hb_xfree( szOnePath );
                  }
                  else  /* set DEFAULT to DataPath of letodb.ini */
                  {
                     pItem = hb_itemPutC( pItem, s_pDataPath );
                     hb_setSetItem( HB_SET_DEFAULT, pItem );
                     /* hb_setSetItem( HB_SET_PATH, pItem ); */
                     hb_itemRelease( pItem );
                     pItem = NULL;
                  }

                  /* search paths, if given by newest client lib */
                  if( pEnd )
                  {
                     ptr = ++pEnd;
                     uiLen = ( HB_USHORT ) strlen( ptr );
                  }
                  else
                  {
                     uiLen = 0;
                     leto_writelog( NULL, 0, "ERROR leto_Intro() elch told you not to use old client lib .. :-)" );
                  }
                  if( uiLen )
                  {
                     char *    szOnePath= ( char * ) hb_xgrab(  HB_PATH_MAX );
                     char *    szSearchPath;
                     HB_UCHAR  uPaths = 1;  /* first will become the letodb.ini DataPAth */
                     HB_USHORT uiLenOne;

                     pEnd = ptr;
                     while( pEnd - ptr < uiLen )
                     {
                        uPaths++;
                        if( ( pEnd = strchr( pEnd, ';' ) ) == NULL )
                           break;
                        pEnd++;
                     }
                     szSearchPath = ( char * ) hb_xgrab( uPaths-- * HB_PATH_MAX );
                     strcpy( szSearchPath, s_pDataPath );
                     uiLen = ( HB_USHORT ) strlen( szSearchPath );
                     leto_StrTran( szSearchPath, DEF_CH_SEP, DEF_SEP, uiLen );
                     szSearchPath[ uiLen++ ] = DEF_SEPPATH;
                     szSearchPath[ uiLen ] = '\0';

                     while( uPaths > 0 )
                     {
                        pEnd = strchr( ptr, ';' );
                        if( ! pEnd )  /* last one not terminated by ';' */
                           pEnd = ptr + strlen( ptr );

                        memcpy( szOnePath, ptr, HB_MIN( pEnd - ptr, HB_PATH_MAX - 1 ) );
                        szOnePath[ HB_MIN( pEnd - ptr, HB_PATH_MAX - 1 ) ] = '\0';
                        leto_BeautifyPath( szOnePath );
                        leto_PathFinder( szOnePath, s_pDataPath );

                        uiLenOne = ( HB_USHORT ) strlen( szOnePath );
                        if( szOnePath[ uiLenOne - 1 ] == DEF_SEP )
                           szOnePath[ uiLenOne - 1 ] = '\0';
                        uiLen += leto_DataPath( szOnePath, szSearchPath + uiLen );
                        if( uPaths > 1 )
                           szSearchPath[ uiLen++ ] = DEF_SEPPATH;

                        ptr = pEnd + 1;
                        uPaths--;
                     }

                     pItem = hb_itemPutC( pItem, szSearchPath );
                     hb_setSetItem( HB_SET_PATH, pItem );
                     hb_itemRelease( pItem );
                     hb_xfree( szSearchPath );
                     hb_xfree( szOnePath );
                  }
               }
            }
         }
      }

      if( ! pData )  /* not empty in case of wrong Pass */
      {
         char * szVersion = hb_verHarbour();
         char   pBuf[ 255 ];

         bSuccess = HB_TRUE;
         hb_snprintf( pBuf, 254, "+%c%c%c;%s;%s;%d;%d;%d;%d;%d;%c;%c;",
                      ( pUStru->szAccess[ 0 ] & 0x1 ) ? 'Y' : 'N',
                      ( pUStru->szAccess[ 0 ] & 0x2 ) ? 'Y' : 'N',
                      ( pUStru->szAccess[ 0 ] & 0x4 ) ? 'Y' : 'N',
                      szVersion, leto_Driver( s_uiDriverDef ),
                      s_uiMemoType, s_uiMemoBlocksize, s_uiLockExtended, pUStru->iPort,
                      s_iAutOrder,
                      s_bNoSaveWA ? ( s_bShareTables ? '4' : '3' ) : ( s_bShareTables ? '2' : '1' ),
                      s_bLowerPath ? '1' : '0' );
         hb_xfree( szVersion );
         pData = pBuf;

         if( pUStru->hSocketErr == HB_NO_SOCKET && iDebugMode() > 0 )  /* second socket comes also here */
            leto_writelog( NULL, -1, "INFO: connected from %s :%d %s CP: %s  DF: %s  conn-ID %d",
                                     pUStru->szAddr, pUStru->iPort,
                                     ( *( pUStru->szExename ) ? ( char * ) pUStru->szExename : "?exe?" ),
                                     hb_cdpID(), hb_setGetDateFormat(), pUStru->iUserStru - 1 );
         if( s_iDebugMode > 10 )
            leto_writelog( NULL, -1, "DEBUG leto_Intro() DEFAULT %s  PATH %s", hb_setGetDefault(), hb_setGetPath() );
      }
   }

   leto_SendAnswer( pUStru, pData, strlen( pData ) );
   if( bSuccess )
      leto_wUsLogDelete( pUStru );
}

static void leto_CloseT( PUSERSTRU pUStru, const char * szData )
{
   const char * pData;
   HB_BOOL      bOk = HB_FALSE;
   HB_ULONG     ulAreaID = strtoul( szData, NULL, 10 );

   pUStru->bLastAct = HB_FALSE;
   if( ! ulAreaID )
      pData = szErr2;
   else
   {
      PAREASTRU pAStru = leto_FindArea( pUStru, ulAreaID );

      if( pAStru )
      {
         if( leto_SelectArea( pUStru, pAStru->ulAreaID ) )
         {
            bOk = leto_CloseArea( pUStru, pAStru );
            if( ! bOk )
            {
               leto_wUsLog( pUStru, -1, "ERROR leto_CloseT failed close area (%lu) %s",
                            pAStru->ulAreaID, pAStru->szAlias );
               pData = szErr4;
            }
            else
               pData = szOk;
         }
         else
            pData = szErr4;
      }
      else
      {
         pData = szErr4;
         leto_wUsLog( pUStru, -1, "DEBUG! leto_CloseT workarea (%lu) not found (not opened ?), developer error", ulAreaID );
      }
   }

   if( ! pUStru->bBeQuiet )
      leto_SendAnswer2( pUStru, pData, 4, bOk, 1000 );
   //leto_SendAnswer( pUStru, pData, 4 );

   if( bOk )
      pUStru->bLastAct = HB_TRUE;
}

/* leto_udf() */
HB_FUNC( LETO_DBCLOSEAREA )
{
   PUSERSTRU    pUStru = letoGetUStru();
   HB_ULONG     ulAreaID = 0;
   const char * szAlias = NULL;
   PAREASTRU    pAStru;
   HB_BOOL      bOk = HB_FALSE;

   if( ! ( pUStru->bRpcMode || s_bNoSaveWA ) )
   {
      hb_ret();
      return;
   }
   if( HB_ISNUM( 1 ) && hb_parni( 1 ) > 0 )
      ulAreaID = ( HB_ULONG ) hb_parni( 1 );
   else if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) )
      szAlias = hb_parc( 1 );

   pAStru = leto_Select( pUStru, ulAreaID, szAlias, HB_TRUE );
   if( pAStru )
   {
      if( pAStru->ulUdf == ( HB_ULONG ) pUStru->iUserStru )  /* really close only UDF opened areas */
      {
         char szData[ 12 ];

         sprintf( szData, "%lu;", pAStru->ulAreaID );
         leto_CloseT( pUStru, szData );
         bOk = pUStru->bLastAct;
         if( ! bOk && s_iDebugMode > 0 )
            leto_wUsLog( pUStru, -1, "DEBUG leto_DbCloseArea( %s ) failed", szData );
      }
      else if( ! s_bNoSaveWA || pAStru->pTStru->bMemIO )  // tofix
      {
         leto_FreeArea( pUStru, pAStru->ulAreaID, HB_FALSE );
         if( s_iDebugMode > 20 )
            leto_wUsLog( pUStru, -1, "DEBUG leto_DbCloseArea( %lu ) no UDF area -> leto_FreeArea()", ulAreaID );
         bOk = HB_TRUE;
      }
   }

   hb_retl( bOk );
}

static void leto_CloseTall( PUSERSTRU pUStru, const char * szData  )
{
   HB_SYMBOL_UNUSED( szData );

   HB_GC_LOCKU();  // ToDo verify lock is needed

   leto_CloseAll4Us( pUStru );
   leto_SendAnswer2( pUStru, szOk, 4, HB_TRUE, 1000 );
   // leto_SendAnswer( pUStru, szOk, 4 );

   HB_GC_UNLOCKU();
}

/* not used, client closes connection by socket shutdown */
static void leto_CloseUStru( PUSERSTRU pUStru, const char * szData  )
{
   HB_SYMBOL_UNUSED( szData );

   pUStru->bCloseConnection = HB_TRUE;
   pUStru->bNoAnswer = HB_TRUE;
}

static void leto_Pack( PUSERSTRU pUStru, const char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   const char * pData;
   HB_SYMBOL_UNUSED( szData );

   if( pArea->valResult )
      hb_itemClear( pArea->valResult );
   else
      pArea->valResult = hb_itemNew( NULL );
   hb_xvmSeqBegin();
   SELF_PACK( pArea );
   hb_xvmSeqEnd();
   if( pUStru->iHbError )
      pData = szErr101;
   else
      pData = szOk;

   leto_SendAnswer( pUStru, pData, 4 );
}

static void leto_Zap( PUSERSTRU pUStru, const char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   const char * pData;
   HB_SYMBOL_UNUSED( szData );

   hb_xvmSeqBegin();
   SELF_ZAP( pArea );
   hb_xvmSeqEnd();
   if( pUStru->iHbError )
      pData = szErr101;
   else
      pData = szOk;

   leto_SendAnswer( pUStru, pData, 4 );
}

static void leto_Reccount( PUSERSTRU pUStru, const char * szData )
{
   AREAP     pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_ULONG  ulRecCount;
   char      szRet[ 21 ];
   char *    ptr = szRet;

   HB_SYMBOL_UNUSED( szData );

   SELF_RECCOUNT( pArea, &ulRecCount );
   *ptr++ = '+';
   ptr += ultostr( ulRecCount, ptr );
   *ptr++ = ';';
   *ptr = '\0';

   leto_SendAnswer( pUStru, szRet, ptr - szRet );
}

/* ToDo - rework for wrong params */
static void leto_Set( PUSERSTRU pUStru, const char * szData )
{
   const char * pAreaID, * pSkipBuf = NULL;
   int          nParam = leto_GetParam( szData, &pAreaID, &pSkipBuf, NULL, NULL );

   if( nParam < 1 )   // changed protocol || *szData != '0' )
      leto_SendAnswer2( pUStru, szErr2, 4, HB_FALSE, 1000 );
      //leto_SendAnswer( pUStru, szErr2, 4 );
   else
   {
      int iCmd = atoi( szData );

      switch( iCmd )
      {
         case 2:  /* leto_setSkipBuffer */
            if( nParam >= 3 )
            {
               HB_ULONG  ulAreaID = strtoul( pAreaID, NULL, 10 );
               PAREASTRU pAStru = leto_FindArea( pUStru, ulAreaID );
               int       iSkipBuf = atoi( pSkipBuf );

               if( pAStru != NULL && iSkipBuf >= 1 )
                  pAStru->uiSkipBuf = ( HB_USHORT ) iSkipBuf;
               else
                  leto_wUsLog( pUStru, -1, "ERROR leto_Set! set SkipBuffer for area: %d to: %d failed",
                               ulAreaID, iSkipBuf );
            }
            else
               leto_wUsLog( pUStru, 0, "ERROR leto_Set! set SkipBuffer failed" );
            break;

         case 3:  /* RDDI_BUFKEYNO */
         case 4:  /* RDDI_BUFKEYCOUNT */
            if( pAreaID )
            {
               HB_BOOL bSet = ( *pAreaID == 'T' );

               if( iCmd == 3 )
                  pUStru->bBufKeyNo = bSet;
               else
                  pUStru->bBufKeyCount = bSet;
            }
            break;
      }
      leto_SendAnswer2( pUStru, szOk, 4, HB_TRUE, 1000 );
      //leto_SendAnswer( pUStru, szOk, 4 );
   }
}

static void leto_Transaction( PUSERSTRU pUStru, const char * szData )
{
   char           szData1[ 16 ];
   char *         ptrPar;
   char *         pResult = NULL;
   const char *   ptr = szData;
   const char *   pData = NULL;
   int            iRecNumber, i = 0, i1;
   int            iRes = 0;
   int            iAppended = 0;
   AREAP          pArea = NULL;
   HB_BOOL        bUnlockAll, bTransWithAppend = HB_FALSE;
   HB_ULONG *     pulAppends = NULL;
   HB_ULONG       ulResLen = 4;
   HB_ULONG       ulTransOk = 0;
   TRANSACTSTRU * pTA;

   if( ( s_bPass4D && ! ( pUStru->szAccess[ 0 ] & 4 ) ) )
   {
      leto_SendAnswer( pUStru, szErrAcc, 4 );
      return;
   }

   iRecNumber = HB_GET_LE_UINT32( ptr );  /* formerly = leto_b2n( ptr, 4 ); */
   ptr += 4;
   bUnlockAll = ( *ptr & 1 );
   ptr++;

   if( iRecNumber )
   {
      HB_ULONG ulTaLen = pUStru->ulDataLen - 2;
      HB_ULONG ulAreaIDOld = 0;
      HB_UCHAR uLenLen;

      pTA = ( TRANSACTSTRU * ) hb_xgrabz( sizeof( TRANSACTSTRU ) * iRecNumber );

      while( ! iRes && i < iRecNumber && ( ( HB_ULONG ) ( ptr - szData ) ) < ulTaLen )
      {
         if( ( uLenLen = ( ( ( HB_UCHAR ) *ptr ) & 0xFF ) ) < 10 )
         {
            HB_ULONG ulLen = leto_b2n( ptr + 1, uLenLen );

            ptr += uLenLen + 1;

            if( ulLen )
            {
               HB_ULONG ulAreaID = strtoul( ptr + 2, &ptrPar, 10 );

               if( ptrPar != NULL && ulAreaID &&
                   ( ulAreaIDOld == ulAreaID || ( pArea = leto_SelectArea( pUStru, ulAreaID ) ) != NULL ) )
               {
                  ulAreaIDOld = ulAreaID;
                  pTA[ i ].ulSelectID = ulAreaID;
                  pTA[ i ].pArea = pArea;
                  pTA[ i ].pAStru = pUStru->pCurAStru;
                  ++ptrPar;
                  switch( ptr[ 0 ] )
                  {
                     case LETOCMD_upd:
                        iRes = leto_UpdateRecord( pUStru, ptrPar, HB_FALSE, NULL, &pTA[ i ], pArea );
                        break;
                     case LETOCMD_add:
                        iRes = leto_UpdateRecord( pUStru, ptrPar, HB_TRUE, NULL, &pTA[ i ], pArea );
                        bTransWithAppend = HB_TRUE;
                        break;
                     case LETOCMD_memo:
                        iRes = leto_Memo( pUStru, ptrPar, &pTA[ i ], pArea );
                        if( pTA[ i ].bAppend )
                           bTransWithAppend = HB_TRUE;
                        break;
                     default:
                        leto_wUsLog( pUStru, 0, "ERROR leto_Transaction! bad command!" );
                        iRes = 2;
                        break;
                  }
               }
               else
               {
                  leto_wUsLog( pUStru, -1, "ERROR leto_Transaction! bad nAreaID %lu", ulAreaID );
                  iRes = 2;
               }
            }
            else
            {
               leto_wUsLog( pUStru, 0, "ERROR leto_Transaction! missing nAreaID!" );
               iRes = 2;
            }
            ptr += ulLen;
         }
         else
         {
            leto_wUsLog( pUStru, 0, "ERROR leto_Transaction! bad message length!" );
            iRes = 2;
            pData = szErr2;
            break;
         }
         i++;
      }

      if( ! iRes )
      {
         PLETO_LIST_ITEM pListItem;
         PAREASTRU       pAStru;
         HB_BOOL         bFlushed;

         hb_xvmSeqBegin();

         for( i = 0; i < iRecNumber; i++ )
         {
            pArea = pTA[ i ].pArea;

            if( pTA[ i ].bAppend )
            {
               /* ensured by client that nobody else can Flock() after done validation */
               SELF_APPEND( pArea, HB_FALSE );  /* changed: not to unlock other records */
               SELF_RECNO( pArea, &pTA[ i ].ulRecNo );
               /* pUStru->pCurAStru->pTStru->pGlobe->ulRecCount++; */

               for( i1 = 0; i1 < iAppended; i1++ )
               {
                  if( pulAppends[ i1 * 2 ] == pTA[ i ].ulSelectID )
                     break;
               }
               if( i1 == iAppended )
               {
                  iAppended++;
                  if( pulAppends )
                     pulAppends = hb_xrealloc( pulAppends, sizeof( HB_ULONG ) * iAppended * 2 );
                  else
                     pulAppends = hb_xgrab( sizeof( HB_ULONG ) * iAppended * 2 );
                  pulAppends[ i1 * 2 ] = pTA[ i ].ulSelectID;
               }
               pulAppends[ i1 * 2 + 1 ] = pTA[ i ].ulRecNo;

            }
            else
            {
               HB_ULONG ulRecNo = pTA[ i ].ulRecNo;

               if( ! ulRecNo )
               {
                  for( i1 = i - 1; i1 >= 0; i1-- )
                     if( pArea == pTA[ i1 ].pArea && pTA[ i1 ].bAppend )
                     {
                        ulRecNo = pTA[ i1 ].ulRecNo;
                        break;
                     }
               }
               leto_GotoForce( pArea, ulRecNo );
            }

            if( pTA[ i ].uiFlag & 1 )
               SELF_DELETE( pArea );
            else if( pTA[ i ].uiFlag & 2 )
               SELF_RECALL( pArea );

            for( i1 = 0; i1 < pTA[ i ].uiItems; i1++ )
            {
               if( pTA[ i ].pItems[ i1 ] )
                  SELF_PUTVALUE( pArea, pTA[ i ].puiIndex[ i1 ], pTA[ i ].pItems[ i1 ] );
            }
         }

         /* unlocking all appended records, nowbody else knew about these locks */
         if( bTransWithAppend )
         {
            PHB_ITEM pLock = NULL;

            for( i = 0; i < iRecNumber; i++ )
            {
               if( pTA[ i ].bAppend )
               {
                  if( bUnlockAll )  /* default */
                  {
                     if( pTA[ i ].bLockable )  /* possible reclocked append */
                     {
                        pArea = pTA[ i ].pArea;
                        pLock = hb_itemPutNL( pLock, pTA[ i ].ulRecNo );
                        SELF_UNLOCK( pArea, pLock );
                     }
                  }
                  else  /* only register append lock */
                     leto_RecLock( pUStru, pTA[ i ].pAStru, pTA[ i ].ulRecNo, HB_TRUE, 0 );
               }
            }
            if( pLock )
               hb_itemRelease( pLock );
         }

         /* flushing with server hardcommit setting, [default] unlock tables of transaction, detaching */
         pListItem = pUStru->AreasList.pItem;
         while( pListItem )
         {
            pAStru = ( PAREASTRU ) ( pListItem + 1 );
            if( pAStru->pTStru )
            {
               for( i = 0; i < iRecNumber; i++ )
               {
                  if( pTA[ i ].ulSelectID == pAStru->ulAreaID )
                  {
                     int ii = i + 1;

                     pArea = pTA[ i ].pArea;
                     pTA[ i ].pArea = NULL;

                     if( ( ( DBFAREAP ) pArea )->fUpdateHeader || ( ( DBFAREAP ) pArea )->fDataFlush ||
                         ( ( DBFAREAP ) pArea )->fMemoFlush )
                     {
                        SELF_FLUSH( pArea );
                        bFlushed = HB_TRUE;
                     }
                     else
                        bFlushed = HB_FALSE;

                     if( bUnlockAll )
                        leto_TableUnlock( pAStru, HB_TRUE, pArea );  /* only R-locks, F-locks are later removed by client */
                     leto_DetachArea( pAStru );
                     while( ii < iRecNumber )  /* mark all same WA as done */
                     {
                        if( pTA[ ii ].ulSelectID == pAStru->ulAreaID )
                           pTA[ ii ].pArea = NULL;
                        ii++;
                     }
                     if( s_iDebugMode > 20 )
                        leto_wUsLog( pUStru, -1, "DEBUG leto_Transaction! table %s %s%sdetached", pAStru->pTStru->szTable,
                                     bFlushed ? "flushed " : "", bUnlockAll ? "unlocked " : "" );

                     break;
                  }
               }
            }
            pListItem = pListItem->pNext;
         }

         hb_xvmSeqEnd();
         if( pUStru->iHbError )
            pData = szErr101;
         else
         {
            if( pulAppends )
            {
               pResult = hb_xgrab( iAppended * 2 * 22 + 15 );
               memcpy( pResult, szOk, 4 );
               ptrPar = pResult + 4;
               ptrPar += sprintf( ptrPar, ";%d;", iAppended );
               for( i1 = 0; i1 < iAppended; i1++ )
                  ptrPar += sprintf( ptrPar, "%lu,%lu;", pulAppends[ i1 * 2 ], pulAppends[ i1 * 2 + 1 ] );
               pData = pResult;
               ulResLen = strlen( pResult );
            }
            else
               pData = szOk;
            ulTransOk++;
         }
      }
      else if( iRes == 2 )
         pData = szErr2;
      else if( iRes == 3 )
         pData = szErr3;
      else if( iRes == 4 )
         pData = szErr4;
      else if( iRes > 100 )
      {
         sprintf( szData1, "-%04d;", iRes );
         pData = szData1;
      }
      if( pulAppends )
         hb_xfree( pulAppends );

      for( i = 0; i < iRecNumber; i++ )
      {
         if( pTA[ i ].pArea != NULL )  /* may be left open in case of error */
            leto_FreeArea( pUStru, pTA[ i ].ulSelectID, HB_FALSE );
         if( pTA[ i ].puiIndex )
            hb_xfree( pTA[ i ].puiIndex );
         if( pTA[ i ].pItems )
         {
            for( i1 = 0; i1 < pTA[ i ].uiItems; i1++ )
               if( pTA[ i ].pItems[ i1 ] )
                  hb_itemRelease( pTA[ i ].pItems[ i1 ] );
            hb_xfree( pTA[ i ].pItems );
         }
      }
      hb_xfree( pTA );
   }
   else
      pData = szOk;

   leto_SendAnswer( pUStru, pData, ulResLen );

   HB_GC_LOCKTRAN();
   s_ulTransAll++;
   s_ulTransOK += ulTransOk;
   HB_GC_UNLOCKTRAN();

   if( pResult )
      hb_xfree( pResult );
}

static void leto_ResetFilter( PAREASTRU pAStru )
{
   if( pAStru->itmFltExpr )
   {
      hb_itemRelease( pAStru->itmFltExpr );
      pAStru->itmFltExpr = NULL;
      pAStru->itmFltOptimized = HB_FALSE;
   }

#ifdef __BM
   if( pAStru->pBM )
   {
      hb_xfree( pAStru->pBM );
      pAStru->pBM = NULL;
   }
#endif
}

static void leto_Filter( PUSERSTRU pUStru, const char * szData )
{
   AREAP     pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_ULONG  ulLen = strlen( szData + 2 );
   PAREASTRU pAStru = pUStru->pCurAStru;
   HB_BOOL   bClear = ( ulLen == 0 );

   leto_ResetFilter( pAStru );
   if( s_bNoSaveWA && ! pAStru->pTStru->bMemIO )
      leto_ClearFilter( pArea );

   if( bClear )
      leto_SendAnswer2( pUStru, szOk, 4, HB_TRUE, 1021 );
   else
   {
      HB_BOOL      bRes = HB_FALSE;
      HB_BOOL      bForce = ( *szData == 'T' );
      PHB_ITEM     pFilterBlock = NULL;
      const char * szFilter = szData + 2;

      /* pre-test filter for executable at server, and without alias */
      if( hb_setGetOptimize() )
         bRes = leto_ParseFilter( pUStru, szData, ulLen );

      if( bRes || bForce )
      {
         bRes = leto_ExprGetType( pUStru, szFilter, ( int ) ulLen ) == 'L';
         if( bRes )
         {
            hb_xvmSeqBegin();
            pFilterBlock = leto_mkCodeBlock( szFilter, ulLen, HB_FALSE );
            if( pFilterBlock == NULL )
               bRes = HB_FALSE;
            else
               bRes = HB_IS_LOGICAL( hb_vmEvalBlockOrMacro( pFilterBlock ) );
            hb_xvmSeqEnd();
            if( pUStru->iHbError || ! bRes )
            {
               bRes = HB_FALSE;
               if( pFilterBlock )
               {
                  hb_itemRelease( pFilterBlock );
                  pFilterBlock = NULL;
               }
            }
         }
      }

      if( bRes && pFilterBlock )
      {
         pAStru->itmFltOptimized = HB_TRUE;
         pAStru->itmFltExpr = pFilterBlock;
      }

      if( ! bRes && bForce )
      {
         if( s_iDebugMode >= 2 )
         {
            if( ! hb_setGetForceOpt() )
               leto_wUsLog( pUStru, -1, "ERROR leto_Filter! <%s>  not optimized or syntax error:", szFilter );
            else
               leto_wUsLog( pUStru, -1, "ERROR leto_Filter! <%s>  syntax error:", szData );
         }
         if( pUStru->iHbError )
            leto_SendError( pUStru, szErr4, 4 );
         else
            leto_SendAnswer( pUStru, szErr4, 4 );
      }
      else
      {
         if( pAStru->itmFltOptimized )
         {
            if( s_iDebugMode > 10 )
               leto_wUsLog( pUStru, -1, "DEBUG leto_Filter! [ ForceOpt(%d) ] accepted <%s>", bForce, szFilter);
            leto_SendAnswer( pUStru, "++++", 4 );
            if( s_bNoSaveWA && ! pAStru->pTStru->bMemIO )
               leto_SetFilter( pAStru, pArea, pUStru );
         }
         else
            leto_SendAnswer( pUStru, "+---", 4 );
      }
   }
}

static HB_BOOL leto_RelationIsCyclic( PUSERSTRU pUStru, AREAP pArea )
{
   AREAP           pChildArea;
   PLETO_LIST_ITEM pListItem;
   PAREASTRU       pAStru;
   DBRELINFO *     pRelations;
   HB_BOOL         bCyclic = HB_FALSE;

   pListItem = pUStru->AreasList.pItem;
   while( ! bCyclic && pListItem )
   {
      pAStru = ( PAREASTRU ) ( pListItem + 1 );
      if( pAStru != pUStru->pCurAStru )
      {
         pChildArea = ( AREAP ) hb_rddGetWorkAreaPointer( pAStru->ulAreaID );
         pRelations = pChildArea->lpdbRelations;
         while( pRelations )
         {
            if( ( ( AREAP ) pRelations->lpaChild ) == pArea )
            {
               bCyclic = HB_TRUE;
               break;
            }
            pRelations = pRelations->lpdbriNext;
         }
      }
      pListItem = pListItem->pNext;
   }

   return bCyclic;
}

/* set/ clear relation only for mode s_bNoSaveWA */
static void leto_Relation( PUSERSTRU pUStru, const char * szData )
{
   HB_BOOL bFail = HB_FALSE;

   if( s_bNoSaveWA && ! pUStru->pCurAStru->pTStru->bMemIO )
   {
      switch( *( szData + 1 ) )
      {
         case '1':  /* set relation */
         {
            const char * szTmp = szData;
            const char * pp1 = NULL, * pp2 = NULL;
            int          nParam;

            for( ;; )
            {
               int iAreaChild;

               nParam = leto_GetParam( szTmp, &pp1, &pp2, NULL, NULL );
               if( s_iDebugMode > 10 && nParam > 0 )
                  leto_wUsLog( pUStru, -1, "DEBUG leto_Relation WA %s %s into %s", pUStru->pCurAStru->szAlias,
                               nParam > 2 ? pp2 : "", nParam > 1 ? pp1 : "" );
               if( nParam < 3 )  /* includes szTmp */
                  break;

               iAreaChild = atoi( pp1 );
               if( iAreaChild < 1 && *pp1 )
                  hb_rddGetAliasNumber( pp1, &iAreaChild );

               if( iAreaChild > 0 && *pp2 )
               {
                  AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
                  AREAP pChildArea = ( AREAP ) hb_rddGetWorkAreaPointer( iAreaChild );

                  if( pArea && pChildArea )
                  {
                     DBRELINFO dbRelations;
                     PHB_ITEM  pAbKey = hb_itemPutC( NULL, pp2 );
                     PHB_ITEM  pRelBlock;

                     hb_xvmSeqBegin();
                     pRelBlock = leto_mkCodeBlock( pp2, strlen( pp2 ), HB_FALSE );
                     if( pRelBlock != NULL )
                        hb_vmEvalBlock( pRelBlock );
                     hb_xvmSeqEnd();
                     if( pUStru->iHbError )
                        bFail = HB_TRUE;
                     if( ! bFail && leto_RelationIsCyclic( pUStru, pArea ) )
                     {
                        pUStru->iHbError = 1;
                        if( ! pUStru->szHbError )
                           pUStru->szHbError = ( char * ) hb_xgrab( 64 );
                        strcpy( pUStru->szHbError, "    :12-1006-0-0! cyclic relation detected" );
                        bFail = HB_TRUE;
                     }
                     if( bFail )
                     {
                        if( pAbKey )
                           hb_itemRelease( pAbKey );
                        if( pRelBlock )
                           hb_itemRelease( pRelBlock );
                        SELF_CLEARREL( pArea );
                        break;
                     }
                     else
                     {
                        dbRelations.itmCobExpr = pRelBlock;
                        dbRelations.abKey = pAbKey;
                        dbRelations.isScoped = HB_FALSE;
                        dbRelations.isOptimized = HB_FALSE;
                        dbRelations.lpaChild = pChildArea;
                        dbRelations.lpaParent = pArea;
                        dbRelations.lpdbriNext = NULL;
                        hb_xvmSeqBegin();
                        if( SELF_SETREL( pArea, &dbRelations ) != HB_SUCCESS )
                           bFail = HB_TRUE;
                        hb_xvmSeqEnd();
                        if( pUStru->iHbError || bFail )
                        {
                           SELF_CLEARREL( pArea );
                           break;
                        }
                     }
                  }
               }

               szTmp = pp2 + strlen( pp2 );  /* position at the ';' */
            }
            break;
         }

         case '2':  /* clear relation */
         {
            AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

            if( pArea )
            {
               if( s_iDebugMode > 10 )
                  leto_wUsLog( pUStru, -1, "DEBUG leto_Relation WA %s clear relation", pUStru->pCurAStru->szAlias );
               SELF_CLEARREL( pArea );
            }
            break;
         }
      }
   }

   if( pUStru->iHbError )
      leto_SendError( pUStru, szErr4, 4 );
   else
      leto_SendAnswer( pUStru, ! bFail ? szOk : "-004:12-1006-0-0", ! bFail ? 4 : 16  );
}

#ifdef __BM
/* leto_udf() */
HB_FUNC( LETO_BMRESTORE )
{
   PUSERSTRU pUStru = letoGetUStru();

   if( pUStru->pCurAStru )
   {
      AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

      if( pArea )
      {
         if( hb_parl( 1 ) )
         {
            leto_ResetFilter( pUStru->pCurAStru );
            if( s_bNoSaveWA && ! pUStru->pCurAStru->pTStru->bMemIO )
               leto_ClearFilter( pArea );
         }
         if( pUStru->pCurAStru->pBM )
            leto_BMRestore( pArea, pUStru->pCurAStru );
      }
   }
}

/* leto_udf() */
HB_FUNC( LETO_BMSAVE )
{
   PUSERSTRU pUStru = letoGetUStru();
   HB_BOOL   bRet = HB_FALSE;

   if( pUStru->pCurAStru )
   {
      PAREASTRU pAStru = pUStru->pCurAStru;
      AREAP     pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

      if( pArea )
      {
         pAStru->pBM = pArea->dbfi.lpvCargo;
         if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
         {
            pArea->dbfi.lpvCargo = NULL;
            pArea->dbfi.fFilter = pArea->dbfi.fOptimized = HB_FALSE;
         }
         bRet = HB_TRUE;
      }
   }
   hb_retl( bRet );
}
#endif

static void letoPutDouble( char * ptr, PHB_ITEM pItem, double dSum, HB_SHORT iDec )
{
   HB_SIZE nLen;
   HB_BOOL bFreeReq;
   char *  buffer, * bufstr;

   hb_itemPutNDDec( pItem, dSum, iDec );
   buffer = hb_itemString( pItem, &nLen, &bFreeReq );
   bufstr = buffer;
   while( *bufstr && *bufstr == ' ' )
      bufstr++;
   strcpy( ptr, bufstr );

   if( bFreeReq )
      hb_xfree( buffer );
}

static void leto_GroupBy( PUSERSTRU pUStru, const char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   const char * pOrder = szData, * pGroup, * pFields, * pFilter, * pFlag;
   HB_ULONG     ulDataLen = strlen( szData );
   int          nParam = leto_GetParam( szData, &pGroup, &pFields, &pFilter, &pFlag );

   if( nParam < 5 || ! pArea )
      leto_SendAnswer( pUStru, szErr2, 4 );
   else
   {
      typedef struct
      {
         HB_SHORT  Pos;
         PHB_ITEM  pBlock;
         HB_USHORT uDec;
      } VALGRP;

      HB_USHORT    uiCount = 0, uiAllocated = 10, uiIndex, uiGroup;
      HB_BOOL      bEnd = HB_FALSE;
      VALGRP *     pSumFields = hb_xgrab( sizeof( VALGRP ) * uiAllocated );
      int          iKeyLen, iDec;
      char         szFieldName[ 12 ];
      PHB_ITEM     pValFilter, pNumItem;
      PHB_ITEM     pGroupBlock = NULL;
      PHB_ITEM     pGroupVal = NULL;
      PHB_ITEM     pItem = hb_itemNew( NULL ), pBlock;
      PHB_ITEM     pFilterBlock = NULL;
      PHB_ITEM     pTopScope = NULL;
      PHB_ITEM     pBottomScope = NULL;
      HB_BOOL      bEof;
      const char * ptr, * pNext;
      char         cGroupType;
      double       dValue;
      PHB_ITEM     pHash = hb_hashNew( NULL );
      PHB_ITEM     pDefault = hb_itemNew( NULL );
      PHB_ITEM     pSubArray;

      if( pFields - pGroup - 1 < 12 )
      {
         memcpy( szFieldName, pGroup, pFields - pGroup - 1 );
         szFieldName[ pFields - pGroup - 1 ] = '\0';
         uiGroup = hb_rddFieldIndex( pArea, szFieldName );
      }
      else
         uiGroup = 0;

      if( uiGroup )
      {
         PHB_ITEM pItemType = hb_itemNew( NULL );

         if( SELF_FIELDINFO( pArea, uiGroup, DBS_TYPE, pItemType ) == HB_SUCCESS )
            cGroupType = hb_itemGetCPtr( pItemType )[ 0 ];
         else
            cGroupType = 'U';
         hb_itemRelease( pItemType );
         pGroupVal = hb_itemNew( NULL );
      }
      else
      {
         cGroupType = leto_ExprGetType( pUStru, pGroup, pFields - pGroup - 1 );
         pGroupBlock = leto_mkCodeBlock( pGroup, pFields - pGroup - 1, HB_TRUE );
      }

      ptr = pFields;
      for( ;; )
      {
         HB_SHORT iPos;

         pNext = strchr( ptr, ',' );
         if( ! pNext )
         {
            pNext = ptr + strlen( ptr );
            bEnd = HB_TRUE;
         }
         if( pNext - ptr < 12 )
         {
            memcpy( szFieldName, ptr, pNext - ptr );
            szFieldName[ pNext - ptr ] = '\0';
         }
         else
            szFieldName[ 0 ] = '\0';

         pBlock = NULL;
         if( szFieldName[ 0 ] == '#' )
            iPos = -1;
         else
         {
            if( szFieldName[ 0 ] )
               iPos = ( HB_SHORT ) hb_rddFieldIndex( pArea, szFieldName );
            else
               iPos = 0;
            if( ( iPos == 0 ) && leto_ExprGetType( pUStru, ptr, pNext - ptr ) == 'N' )
               pBlock = leto_mkCodeBlock( ptr, pNext - ptr, HB_TRUE );
         }

         if( iPos || pBlock )
         {
            uiCount++;
            if( uiCount > uiAllocated )
            {
               uiAllocated += 10;
               pSumFields = ( VALGRP * ) hb_xrealloc( pSumFields, sizeof( VALGRP ) * uiAllocated );
            }
            pSumFields[ uiCount - 1 ].Pos = iPos;
            pSumFields[ uiCount - 1 ].pBlock = pBlock;
            if( iPos > 0 )
            {
               SELF_FIELDINFO( pArea, iPos, DBS_DEC, pItem );
               pSumFields[ uiCount - 1 ].uDec = ( HB_USHORT ) hb_itemGetNI( pItem );
            }
            else
               pSumFields[ uiCount - 1 ].uDec = 0;
         }

         if( bEnd )
            break;
         ptr = pNext + 1;
      }

      if( uiCount && ( uiGroup || ( pGroupBlock && cGroupType != 'U' ) ) )
      {
         PAREASTRU pAStru = pUStru->pCurAStru;

         hb_arrayNew( pDefault, uiCount + 1 );
         for( uiIndex = 0; uiIndex < uiCount; uiIndex++ )
         {
            if( pSumFields[ uiIndex ].uDec == 0 )
               hb_arraySetNL( pDefault, uiIndex + 2, 0 );
            else
               hb_arraySetND( pDefault, uiIndex + 2, 0.0 );
         }
         hb_hashSetFlags( pHash, HB_HASH_AUTOADD_ALWAYS );
         hb_hashSetDefault( pHash, pDefault );

         leto_SetFocusIf( pUStru->pCurAStru, pArea, pOrder );

         ptr = pFlag + 2;
         if( ptr < pOrder + ulDataLen )
         {
            iKeyLen = ( ( ( HB_UCHAR ) *ptr++ ) & 0xFF );

            if( iKeyLen )
            {
               pTopScope = leto_KeyToItem( pArea, ptr, iKeyLen, pOrder,
                                           pAStru->pTagCurrent->pIStru->cKeyType );
               ptr += iKeyLen;
               iKeyLen = ( ( ( HB_UCHAR ) *ptr++ ) & 0xFF );
               if( iKeyLen && ptr < pOrder + ulDataLen )
                  pBottomScope = leto_KeyToItem( pArea, ptr, iKeyLen, pOrder,
                                                 pAStru->pTagCurrent->pIStru->cKeyType );
            }
         }

         hb_xvmSeqBegin();

         if( *pFilter != '\0' )
            pFilterBlock = leto_mkCodeBlock( pFilter, strlen( pFilter ), HB_FALSE );
         if( pFilterBlock == NULL && ! ( ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) ) )
            leto_SetFilter( pAStru, pArea, pUStru );

         while( ! pUStru->iHbError )
         {
            leto_setSetDeleted( *pFlag == 0x41 );
            if( pBottomScope )
               leto_ScopeCommand( pArea, DBOI_SCOPETOP, pTopScope );
            if( pBottomScope )
               leto_ScopeCommand( pArea, DBOI_SCOPEBOTTOM, pBottomScope );
#if 0  /* ToFix-- this i don't understand */
            else if( pTopScope )
               leto_ScopeCommand( pArea, DBOI_SCOPEBOTTOM, pTopScope );
#endif
            if( ! ( pTopScope || pBottomScope ) && ! ( ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) ) )
            {
               LETOTAG * pTag = pAStru->pTagCurrent;

               if( pTag )
               {
                  leto_SetScope( pArea, pTag, HB_TRUE, HB_TRUE );   /* set TopScope if exist */
                  leto_SetScope( pArea, pTag, HB_FALSE, HB_TRUE );  /* set BotScope if exist */
               }
            }
            if( pFilterBlock )  /* temporary filter */
            {
               pValFilter = hb_vmEvalBlock( pFilterBlock );
               if( ! HB_IS_LOGICAL( pValFilter ) )
                  break;
               pArea->dbfi.itmCobExpr = pFilterBlock;
               pArea->dbfi.fOptimized = HB_FALSE;
               pArea->dbfi.fFilter = HB_TRUE;
            }

            SELF_GOTOP( pArea );
            for( ;; )
            {
               SELF_EOF( pArea, &bEof );
               if( bEof )
                  break;

               if( pFilterBlock )
               {
                  pValFilter = hb_vmEvalBlock( pFilterBlock );
                  bEnd = HB_IS_LOGICAL( pValFilter ) && hb_itemGetL( pValFilter );
               }
               else
                  bEnd = HB_TRUE;

               if( bEnd )
               {
                  if( uiGroup )
                     bEnd = ( SELF_GETVALUE( pArea, uiGroup, pGroupVal ) == HB_SUCCESS );
                  else
                  {
                     pGroupVal = hb_vmEvalBlock( pGroupBlock );
                     bEnd = HB_TRUE;
                  }
               }

               if( bEnd )
               {
                  pSubArray = hb_hashGetItemPtr( pHash, pGroupVal, HB_HASH_AUTOADD_ACCESS );

                  if( ! pSubArray )
                     break;

                  if( HB_IS_NIL( hb_arrayGetItemPtr( pSubArray, 1 ) ) )
                     hb_arraySet( pSubArray, 1, pGroupVal );

                  for( uiIndex = 0; uiIndex < uiCount; uiIndex++ )
                  {
                     if( pSumFields[ uiIndex ].Pos < 0 )
                        hb_arraySetNL( pSubArray, uiIndex + 2, hb_arrayGetNL( pSubArray, uiIndex + 2 ) + 1 );
                     else if( pSumFields[ uiIndex ].pBlock )
                     {
                        pNumItem = hb_vmEvalBlock( pSumFields[ uiIndex ].pBlock );
                        if( HB_IS_DOUBLE( hb_arrayGetItemPtr( pSubArray, uiIndex + 2 ) ) || HB_IS_DOUBLE( pNumItem ) )
                        {
                           dValue = hb_itemGetNDDec( pNumItem, &iDec );
                           hb_arraySetND( pSubArray, uiIndex + 2, hb_arrayGetND( pSubArray, uiIndex + 2 ) + dValue );
                           if( pSumFields[ uiIndex ].uDec < iDec )
                              pSumFields[ uiIndex ].uDec = ( HB_USHORT ) iDec;
                        }
                        else
                           hb_arraySetNL( pSubArray, uiIndex + 2, hb_arrayGetNL( pSubArray, uiIndex + 2 ) + hb_itemGetNL( pNumItem ) );
                     }
                     else if( pSumFields[ uiIndex ].Pos > 0 )
                     {
                        if( SELF_GETVALUE( pArea, pSumFields[ uiIndex ].Pos, pItem ) == HB_SUCCESS )
                        {
                           if( HB_IS_DOUBLE( hb_arrayGetItemPtr( pSubArray, uiIndex + 2 ) ) )
                              hb_arraySetND( pSubArray, uiIndex + 2, hb_arrayGetND( pSubArray, uiIndex + 2 ) + hb_itemGetND( pItem ) );
                           else
                              hb_arraySetNL( pSubArray, uiIndex + 2, hb_arrayGetNL( pSubArray, uiIndex + 2 ) + hb_itemGetNL( pItem ) );
                        }
                     }
                  }
               }
               SELF_SKIP( pArea, 1 );
            }

            break;
         }

         hb_xvmSeqEnd();

         if( ! pFilterBlock && ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
             leto_ClearFilter( pArea );
         if( pFilterBlock )
         {
            hb_itemRelease( pFilterBlock );
            if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
               leto_ClearFilter( pArea );
            else if( ! pAStru->itmFltExpr )  /* restore filter */
            {
               pArea->dbfi.itmCobExpr = pAStru->itmFltExpr;
               pArea->dbfi.fFilter = HB_TRUE;
            }
         }
         if( ! ( pTopScope || pBottomScope ) && ! ( ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) ) )
         {
            LETOTAG * pTag = pAStru->pTagCurrent;

            if( pTag )
               leto_ScopeCommand( pArea, DBOI_SCOPECLEAR, NULL );
         }
         if( pTopScope || pBottomScope )
         {
            LETOTAG * pTag = pAStru->pTagCurrent;

            if( ! ( ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) ) )
               leto_ScopeCommand( pArea, DBOI_SCOPECLEAR, NULL );
            else if( pTag )
            {
               if( pTag->pTopScope )
                  leto_SetScope( pArea, pTag, HB_TRUE, HB_TRUE );    /* set TopScope */
               else
                  leto_SetScope( pArea, pTag, HB_TRUE, HB_FALSE );   /* clear TopScope */
               if( pTag->pBottomScope )
                  leto_SetScope( pArea, pTag, HB_FALSE, HB_TRUE );   /* set BotScope */
               else
                  leto_SetScope( pArea, pTag, HB_FALSE, HB_FALSE );  /* clear BotScope */
            }
            if( pTopScope )
               hb_itemRelease( pTopScope );
            if( pBottomScope )
               hb_itemRelease( pBottomScope );
         }

         if( pUStru->iHbError )
            leto_SendError( pUStru, szErr4, 4 );
         else
         {
            HB_ULONG  ulLen = hb_hashLen( pHash ), ulSize, ulRow;
            HB_USHORT uiLen;
            char *    pData;
            char *    ptrTmp;
            PHB_ITEM  pValue;

            if( uiGroup )
            {
               SELF_FIELDINFO( pArea, uiGroup, DBS_LEN, pItem );
               uiLen = ( HB_USHORT ) hb_itemGetNI( pItem );
            }
            else
            {
               hb_xvmSeqBegin();
               pGroupVal = hb_vmEvalBlock( pGroupBlock );
               hb_xvmSeqEnd();

               if( HB_IS_STRING( pGroupVal ) )
                  uiLen = ( HB_USHORT ) hb_itemGetCLen( pGroupVal ) + 2;
               else if( HB_IS_DATE( pGroupVal ) )
                  uiLen = 8;
               else if( HB_IS_NUMBER( pGroupVal ) )
                  uiLen = 20;
               else if( HB_IS_LOGICAL( pGroupVal ) )
                  uiLen = 1;
               else
                  uiLen = 0;
            }
            ulSize = ulLen * ( uiLen + 1 + uiCount * 21 ) + 25;
            pData = hb_xgrab( ulSize );

            ptrTmp = pData + sprintf( pData, "+%lu;%d;%c;", ulLen, uiCount, cGroupType );

            for( ulRow = 1; ulRow <= ulLen; ulRow++ )
            {
               pSubArray = hb_hashGetValueAt( pHash, ulRow );
               pValue = hb_arrayGetItemPtr( pSubArray, 1 );

               if( HB_IS_STRING( pValue ) )
               {
                  uiLen = ( HB_USHORT ) hb_itemGetCLen( pValue );
                  HB_PUT_LE_UINT16( ptrTmp, uiLen );
                  ptrTmp += 2;
                  memcpy( ptrTmp, hb_itemGetCPtr( pValue ), uiLen );
                  ptrTmp += uiLen;
               }
               else if( HB_IS_DATE( pValue ) )
               {
                  hb_itemGetDS( pValue, ptrTmp );
                  ptrTmp += 8;
               }
               else if( HB_IS_NUMBER( pValue ) )
               {
                  char *   sNum = hb_itemStr( pValue, NULL, NULL );
                  HB_ULONG ulTmp = strlen( sNum );

                  memcpy( ptrTmp, sNum, ulTmp );
                  hb_xfree( sNum );
                  ptrTmp += ulTmp;
               }
               else if( HB_IS_LOGICAL( pValue ) )
                  *ptrTmp++ = hb_itemGetL( pValue ) ? 'T' : 'F';

               for( uiIndex = 0; uiIndex < uiCount; uiIndex++ )
               {
                  pValue = hb_arrayGetItemPtr( pSubArray, uiIndex + 2 );
                  *ptrTmp++ = ',';
                  if( HB_IS_DOUBLE( pValue ) )
                     letoPutDouble( ptrTmp, pItem, hb_itemGetND( pValue ), pSumFields[ uiIndex ].uDec );
                  else
                     sprintf( ptrTmp, "%ld", hb_itemGetNL( pValue ) );
                  ptrTmp += strlen( ptrTmp );
               }
               *ptrTmp++ = ';';
            }

            *ptrTmp++ = ';';
            *ptrTmp = '\0';

            leto_SendAnswer( pUStru, pData, ptrTmp - pData );

            hb_xfree( pData );
         }
      }
      else
         leto_SendAnswer( pUStru, szErr2, 4 );

      for( uiIndex = 0; uiIndex < uiCount; uiIndex++ )
      {
         if( pSumFields[ uiIndex ].pBlock )
            hb_itemRelease( pSumFields[ uiIndex ].pBlock );
      }
      hb_xfree( pSumFields );
      hb_itemRelease( pItem );
      if( pGroupBlock )
         hb_itemRelease( pGroupBlock );
      if( pGroupVal )
         hb_itemRelease( pGroupVal );
      hb_itemRelease( pHash );
      hb_itemRelease( pDefault );
   }
}

static void leto_Sum( PUSERSTRU pUStru, const char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   const char * pOrder = szData, * pFields, * pFilter, * pFlag;
   HB_ULONG     ulDataLen = strlen( szData );
   int          nParam = leto_GetParam( szData, &pFields, &pFilter, &pFlag, NULL );

   if( nParam < 4 || ! pArea )
      leto_SendAnswer( pUStru, szErr2, 4 );
   else
   {
      typedef struct
      {
         HB_SHORT  Pos;
         PHB_ITEM  pBlock;
         HB_USHORT uDec;
         union
         {
            double  dSum;
            HB_LONG lSum;
         } value;
      } SUMSTRU;

      SUMSTRU *    pSums;
      HB_USHORT    uiCount = 0, uiAllocated = 10, uiIndex;
      HB_BOOL      bEnd = HB_FALSE;
      int          iKeyLen, iDec;
      char         szFieldName[ 12 ];
      PHB_ITEM     pValFilter, pNumItem;
      PHB_ITEM     pItem = hb_itemNew( NULL );
      PHB_ITEM     pFilterBlock = NULL;
      PHB_ITEM     pTopScope = NULL;
      PHB_ITEM     pBottomScope = NULL;
      HB_BOOL      bEof;
      const char * ptr, * pNext;
      double       dValue;
      PAREASTRU    pAStru = pUStru->pCurAStru;

      pSums = ( SUMSTRU * ) hb_xgrabz( sizeof( SUMSTRU ) * uiAllocated );
      ptr = pFields;
      for( ;; )
      {
         pNext = strchr( ptr, ',' );
         if( ! pNext )
         {
            pNext = ptr + strlen( ptr );
            bEnd = HB_TRUE;
         }
         if( pNext - ptr < 12 )
         {
            memcpy( szFieldName, ptr, pNext - ptr );
            szFieldName[ pNext - ptr ] = '\0';
         }
         else
            szFieldName[ 0 ] = '\0';

         uiCount++;
         if( uiCount > uiAllocated )
         {
            uiAllocated += 10;
            pSums = ( SUMSTRU * ) hb_xrealloc( pSums, sizeof( SUMSTRU ) * uiAllocated );
         }

         pSums[ uiCount - 1 ].pBlock = NULL;
         if( szFieldName[ 0 ] == '#' )
         {
            pSums[ uiCount - 1 ].Pos = -1;
            pSums[ uiCount - 1 ].uDec = 0;
         }
         else
         {
            if( szFieldName[ 0 ] )
               pSums[ uiCount - 1 ].Pos = ( HB_SHORT ) hb_rddFieldIndex( pArea, szFieldName );
            else
               pSums[ uiCount - 1 ].Pos = 0;
            if( pSums[ uiCount - 1 ].Pos )
            {
               SELF_FIELDINFO( pArea, pSums[ uiCount - 1 ].Pos, DBS_DEC, pItem );
               pSums[ uiCount - 1 ].uDec = ( HB_USHORT ) hb_itemGetNI( pItem );
            }
            else if( leto_ExprGetType( pUStru, ptr, pNext - ptr ) == 'N' )
            {
               pSums[ uiCount - 1 ].pBlock = leto_mkCodeBlock( ptr, pNext - ptr, HB_TRUE );
               pSums[ uiCount - 1 ].uDec = 0;
            }
            else
               pSums[ uiCount - 1 ].uDec = 0;
         }

         if( pSums[ uiCount - 1 ].uDec > 0 )
            pSums[ uiCount - 1 ].value.dSum = 0.0;
         else
            pSums[ uiCount - 1 ].value.lSum = 0;

         if( bEnd )
            break;
         ptr = pNext + 1;
      }

      leto_SetFocusIf( pUStru->pCurAStru, pArea, pOrder );

      ptr = pFlag + 2;
      if( ptr < pOrder + ulDataLen )
      {
         iKeyLen = ( ( ( HB_UCHAR ) *ptr++ ) & 0xFF );

         if( iKeyLen )
         {
            pTopScope = leto_KeyToItem( pArea, ptr, iKeyLen, pOrder,
                                        pAStru->pTagCurrent->pIStru->cKeyType );
            ptr += iKeyLen;
            iKeyLen = ( ( ( HB_UCHAR ) *ptr++ ) & 0xFF );
            if( iKeyLen && ptr < pOrder + ulDataLen )
               pBottomScope = leto_KeyToItem( pArea, ptr, iKeyLen, pOrder,
                                              pAStru->pTagCurrent->pIStru->cKeyType );
         }
      }

      hb_xvmSeqBegin();

      if( *pFilter != '\0' )
         pFilterBlock = leto_mkCodeBlock( pFilter, strlen( pFilter ), HB_FALSE );
      if( pFilterBlock == NULL && ! ( ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) ) )
         leto_SetFilter( pAStru, pArea, pUStru );

      while( ! pUStru->iHbError )
      {
         if( pFilterBlock )  /* temporary filter */
         {
            pValFilter = hb_vmEvalBlock( pFilterBlock );
            if( ! HB_IS_LOGICAL( pValFilter ) )
            {
               pUStru->iHbError = 4;
               break;
            }
            pArea->dbfi.itmCobExpr = pFilterBlock;
            pArea->dbfi.fOptimized = HB_FALSE;
            pArea->dbfi.fFilter = HB_TRUE;
         }
         leto_setSetDeleted( *pFlag == 0x41 );
         if( pTopScope )
            leto_ScopeCommand( pArea, DBOI_SCOPETOP, pTopScope );
         if( pBottomScope )
            leto_ScopeCommand( pArea, DBOI_SCOPEBOTTOM, pBottomScope );
#if 0  /* ToFix -- this i don't understand */
         else if( pTopScope )
            leto_ScopeCommand( pArea, DBOI_SCOPEBOTTOM, pTopScope );
#endif
         if( ! ( pTopScope || pBottomScope ) && ! ( ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) ) )
         {
            LETOTAG * pTag = pUStru->pCurAStru->pTagCurrent;

            if( pTag )
            {
               leto_SetScope( pArea, pAStru->pTagCurrent, HB_TRUE, HB_TRUE );   /* set TopScope */
               leto_SetScope( pArea, pAStru->pTagCurrent, HB_FALSE, HB_TRUE );  /* set BotScope */
            }
         }

         SELF_GOTOP( pArea );
         for( ;; )
         {
            SELF_EOF( pArea, &bEof );
            if( bEof )
               break;

            for( uiIndex = 0; uiIndex < uiCount; uiIndex++ )
            {
               if( pSums[ uiIndex ].Pos < 0 )
               {
                  pSums[ uiIndex ].value.lSum++;
               }
               else if( pSums[ uiIndex ].pBlock )
               {
                  pNumItem = hb_vmEvalBlock( pSums[ uiIndex ].pBlock );
                  if( pSums[ uiIndex ].uDec > 0 || HB_IS_DOUBLE( pNumItem ) )
                  {
                     dValue = hb_itemGetNDDec( pNumItem, &iDec );
                     if( pSums[ uiIndex ].uDec == 0 )
                     {
                        pSums[ uiIndex ].uDec = iDec ? ( HB_USHORT ) iDec : 2;
                        if( uiIndex > 0 )
                           pSums[ uiIndex ].value.dSum = ( double ) pSums[ uiIndex ].value.lSum;
                     }
                     pSums[ uiIndex ].value.dSum += dValue;
                  }
                  else
                     pSums[ uiIndex ].value.lSum += hb_itemGetNL( pNumItem );
               }
               else if( pSums[ uiIndex ].Pos > 0 )
               {
                  if( SELF_GETVALUE( pArea, pSums[ uiIndex ].Pos, pItem ) == HB_SUCCESS )
                  {
                     if( pSums[ uiIndex ].uDec > 0 )
                        pSums[ uiIndex ].value.dSum += hb_itemGetND( pItem );
                     else
                        pSums[ uiIndex ].value.lSum += hb_itemGetNL( pItem );
                  }
               }
            }

            SELF_SKIP( pArea, 1 );
         }

         break;
      }
      hb_xvmSeqEnd();

      if( ! pFilterBlock && ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
          leto_ClearFilter( pArea );
      if( pFilterBlock )
      {
         hb_itemRelease( pFilterBlock );
         if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
            leto_ClearFilter( pArea );
         else if( ! pAStru->itmFltExpr )  /* restore filter */
         {
            pArea->dbfi.itmCobExpr = pAStru->itmFltExpr;
            pArea->dbfi.fFilter = HB_TRUE;
         }
      }
      if( ! ( pTopScope || pBottomScope ) && ! ( ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) ) )
      {
         LETOTAG * pTag = pAStru->pTagCurrent;

         if( pTag )
            leto_ScopeCommand( pArea, DBOI_SCOPECLEAR, NULL );
      }
      if( pTopScope || pBottomScope )
      {
         LETOTAG * pTag = pAStru->pTagCurrent;

         if( ! ( ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) ) )
            leto_ScopeCommand( pArea, DBOI_SCOPECLEAR, NULL );
         else if( pTag )
         {
            if( pTag->pTopScope )
               leto_SetScope( pArea, pTag, HB_TRUE, HB_TRUE );    /* set TopScope */
            else
               leto_SetScope( pArea, pTag, HB_TRUE, HB_FALSE );   /* clear TopScope */
            if( pTag->pBottomScope )
               leto_SetScope( pArea, pTag, HB_FALSE, HB_TRUE );   /* set BotScope */
            else
               leto_SetScope( pArea, pTag, HB_FALSE, HB_FALSE );  /* clear BotScope */
         }
         if( pTopScope )
            hb_itemRelease( pTopScope );
         if( pBottomScope )
            hb_itemRelease( pBottomScope );
      }

      if( pUStru->iHbError )
         leto_SendError( pUStru, szErr4, 4 );
      else
      {
         char * pData = hb_xgrab( uiCount * 21 + 3 );
         char * ptrTmp;

         pData[ 0 ] = '+';
         ptrTmp = pData + 1;
         for( uiIndex = 0; uiIndex < uiCount; uiIndex++ )
         {
            if( ptrTmp > pData + 1 )
               *ptrTmp++ = ',';
            if( pSums[ uiIndex ].uDec > 0 )
               letoPutDouble( ptrTmp, pItem, pSums[ uiIndex ].value.dSum, pSums[ uiIndex ].uDec );
            else
               sprintf( ptrTmp, "%ld", pSums[ uiIndex ].value.lSum );
            ptrTmp += strlen( ptrTmp );
         }
         *ptrTmp++ = ';';
         *ptrTmp = '\0';

         leto_SendAnswer( pUStru, pData, strlen( pData ) );

         hb_xfree( pData );
      }
      for( uiIndex = 0; uiIndex < uiCount; uiIndex++ )
      {
         if( pSums[ uiIndex ].pBlock )
            hb_itemRelease( pSums[ uiIndex ].pBlock );
      }
      hb_xfree( pSums );
      hb_itemRelease( pItem );
   }
}

static const char * letoFillTransInfo( LPDBTRANSINFO pTransInfo, const char * pData, AREAP pAreaSrc, AREAP pAreaDst )
{
   const char * pp1, * pp2;
   HB_ULONG     ulTemp;
   HB_USHORT    uiIndex;

   memset( pTransInfo, 0, sizeof( DBTRANSINFO ) );

   pTransInfo->lpaSource = pAreaSrc;
   pTransInfo->lpaDest = pAreaDst;

   pp1 = pData;
   if( pp1 )
   {
      if( *pp1 == ';' )
         pp1++;
      else if( ( pp2 = strchr( pp1 + 1, ';' ) ) != NULL )
      {
         pTransInfo->dbsci.itmCobFor = leto_mkCodeBlock( pp1, pp2 - pp1, HB_FALSE );
         pTransInfo->dbsci.lpstrFor = NULL;
         pp1 = pp2 + 1;
      }
   }

   if( pp1 )
   {
      if( *pp1 == ';' )
         pp1++;
      else if( ( pp2 = strchr( pp1 + 1, ';' ) ) != NULL )
      {
         pTransInfo->dbsci.itmCobWhile = leto_mkCodeBlock( pp1, pp2 - pp1, HB_FALSE );
         pTransInfo->dbsci.lpstrWhile = NULL;
         pp1 = pp2 + 1;
      }
   }

   if( pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL )
   {
      ulTemp = strtoul( pp1, NULL, 10 );
      if( ulTemp )
         pTransInfo->dbsci.lNext = hb_itemPutNL( NULL, ulTemp );
      pp1 = pp2 + 1;
   }

   if( pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL )
   {
      ulTemp = strtoul( pp1, NULL, 10 );
      if( ulTemp )
         pTransInfo->dbsci.itmRecID = hb_itemPutNL( NULL, ulTemp );
      pp1 = pp2 + 1;
   }
   if( pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL )
   {
      pTransInfo->dbsci.fRest = hb_itemPutL( NULL, ( *pp1 == 'T' ) );
      pp1 = pp2 + 1;
   }

   if( pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL )
   {
      pTransInfo->dbsci.fIgnoreFilter = ( *pp1 == 'T' );
      pp1 = pp2 + 1;
   }
   if( pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL )
   {
      pTransInfo->dbsci.fIncludeDeleted = ( *pp1 == 'T' );
      pp1 = pp2 + 1;
   }
   if( pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL )
   {
      pTransInfo->dbsci.fLast = ( *pp1 == 'T' );
      pp1 = pp2 + 1;
   }
   if( pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL )
   {
      pTransInfo->dbsci.fIgnoreDuplicates = ( *pp1 == 'T' );
      pp1 = pp2 + 1;
   }
   if( pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL )
   {
      pTransInfo->dbsci.fBackward = ( *pp1 == 'T' );
      pp1 = pp2 + 1;
   }
   if( pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL )
   {
      pTransInfo->dbsci.fOptimized = ( *pp1 == 'T' );
      pp1 = pp2 + 1;
   }
   if( pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL )
   {
      pTransInfo->uiFlags = ( HB_USHORT ) atoi( pp1 );
      pp1 = pp2 + 1;
   }
   if( pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL )
   {
      pTransInfo->uiItemCount = ( HB_USHORT ) atoi( pp1 );
      pp1 = pp2 + 1;
   }

   if( pp1 && pTransInfo->uiItemCount )
   {
      pTransInfo->lpTransItems = ( LPDBTRANSITEM )
                                 hb_xgrab( pTransInfo->uiItemCount * sizeof( DBTRANSITEM ) );
      for( uiIndex = 0;
           uiIndex < pTransInfo->uiItemCount && pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL;
           uiIndex++ )
      {
         /* "%hu,%hu;" */
         pTransInfo->lpTransItems[ uiIndex ].uiSource = ( HB_USHORT ) atoi( pp1 );
         pp1++;
         while( *pp1++ != ',' )
            ;
         pTransInfo->lpTransItems[ uiIndex ].uiDest = ( HB_USHORT ) atoi( pp1 );
         pp1 = pp2 + 1;
      }
   }

   return pp1;
}

static void letoFreeTransInfo( LPDBTRANSINFO pTransInfo )
{
   if( pTransInfo->dbsci.itmCobFor )
      hb_itemRelease( pTransInfo->dbsci.itmCobFor );
   if( pTransInfo->dbsci.itmCobWhile )
      hb_itemRelease( pTransInfo->dbsci.itmCobWhile );
   if( pTransInfo->dbsci.lNext )
      hb_itemRelease( pTransInfo->dbsci.lNext );
   if( pTransInfo->dbsci.itmRecID )
      hb_itemRelease( pTransInfo->dbsci.itmRecID );
   if( pTransInfo->dbsci.fRest )
      hb_itemRelease( pTransInfo->dbsci.fRest );
   if( pTransInfo->lpTransItems )
      hb_xfree( pTransInfo->lpTransItems );

}

static void leto_Trans( PUSERSTRU pUStru, const char * szData, HB_BOOL bSort )
{
   PAREASTRU    pAStru = pUStru->pCurAStru;
   AREAP        pAreaSrc, pAreaDst;
   HB_ULONG     ulAreaDst, ulRecNo;
   const char * pp1, * pp2, * pp3;
   int          nParam = leto_GetParam( szData, &pp1, &pp2, NULL, NULL );

   if( nParam > 2 )
   {
      HB_ULONG ulCurAreaID = pUStru->ulCurAreaID;

      ulRecNo = strtoul( szData, NULL, 10 );
      if( pUStru->bDeleted != ( *pp1 == 'T' ) )
      {
         pUStru->bDeleted = ( *pp1 == 'T' );
         leto_setSetDeleted( pUStru->bDeleted );
      }
      ulAreaDst = strtoul( pp2, NULL, 10 );
      pp3 = pp2 + strlen( pp2 ) + 1;

      pAreaSrc = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      pAreaDst = leto_SelectArea( pUStru, ulAreaDst );  /* may changes pUStru->ulCurAreaID; */

      if( pAreaDst && pAreaSrc )
      {
         hb_xvmSeqBegin();
         if( bSort )
         {
            DBSORTINFO dbSortInfo;

            pp1 = letoFillTransInfo( &dbSortInfo.dbtri, pp3, pAreaSrc, pAreaDst );

            if( pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL )
            {
               dbSortInfo.uiItemCount = ( HB_USHORT ) atoi( pp1 );
               pp1 = pp2 + 1;
            }
            else
            {
               dbSortInfo.uiItemCount = 0;
               dbSortInfo.lpdbsItem = NULL;
            }

            if( pp1 && dbSortInfo.uiItemCount )
            {
               HB_USHORT uiIndex;

               dbSortInfo.lpdbsItem = ( LPDBSORTITEM )
                                      hb_xgrab( dbSortInfo.uiItemCount * sizeof( DBSORTITEM ) );
               for( uiIndex = 0;
                    uiIndex < dbSortInfo.uiItemCount && pp1 && ( pp2 = strchr( pp1, ';' ) ) != NULL;
                    uiIndex++ )
               {
                  /* "%hu,%hu;" */
                  dbSortInfo.lpdbsItem[ uiIndex ].uiField = ( HB_USHORT ) atoi( pp1 );
                  pp1++;
                  while( *pp1++ != ',' )
                     ;
                  dbSortInfo.lpdbsItem[ uiIndex ].uiFlags = ( HB_USHORT ) atoi( pp1 );
                  pp1 = pp2 + 1;
               }
            }

            if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
               leto_SetAreaEnv( pAStru, pAreaSrc, pUStru );
            leto_GotoIf( pAreaSrc, ulRecNo );
            SELF_SORT( pAreaSrc, &dbSortInfo );
            if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
               leto_ClearAreaEnv( pAreaSrc, pAStru->pTagCurrent );

            letoFreeTransInfo( &dbSortInfo.dbtri );

            if( dbSortInfo.lpdbsItem )
               hb_xfree( dbSortInfo.lpdbsItem );
         }
         else
         {
            DBTRANSINFO dbTransInfo;

            letoFillTransInfo( &dbTransInfo, pp3, pAreaSrc, pAreaDst );

            if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
               leto_SetAreaEnv( pAStru, pAreaSrc, pUStru );
            leto_GotoIf( pAreaSrc, ulRecNo );
            SELF_TRANS( dbTransInfo.lpaSource, &dbTransInfo );
            if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
               leto_ClearAreaEnv( pAreaSrc, pAStru->pTagCurrent );

            letoFreeTransInfo( &dbTransInfo );
         }

         hb_xvmSeqEnd();

         if( pUStru->iHbError )
            leto_SendError( pUStru, szErr4, 4 );
         else if( ! bSort )
         {
            PAREASTRU pAStruDst = leto_FindArea( pUStru, ulAreaDst );
            HB_ULONG  ulLen;
            char *    szData1;

            if( pAStruDst )
            {
               ulLen = leto_recLen( pAStruDst->pTStru );
               szData1 = ( char * ) hb_xgrab( ulLen + 6 );
               memcpy( szData1, szOk, 4 );
               szData1[ 4 ] = ';';
               ulLen = leto_rec( pUStru, pAStruDst, pAreaDst, szData1 + 5, NULL ) + 5;

               leto_SendAnswer( pUStru, szData1, ulLen );
               hb_xfree( szData1 );
            }
            else
               leto_SendAnswer( pUStru, szErr2, 4 );
         }
         else
            leto_SendAnswer( pUStru, szOk, 4 );
      }
      else
         leto_SendAnswer( pUStru, szErr2, 4 );

      if( pAreaDst )
      {
         /* note above leto_SelectArea() have changed pUStru->ulCurAreaID,
          * so detaching here the ulCurAreaID when leto_Trans() was called,
          * the destination area is closed by SendAnswer.
          * A close command by client for destination area will soon follow. */
         leto_FreeArea( pUStru, ulCurAreaID, HB_FALSE );
      }
   }
   else
      leto_SendAnswer( pUStru, szErr2, 4 );
}

static void leto_TransSort( PUSERSTRU pUStru, const char * szData )
{
   leto_Trans( pUStru, szData, HB_TRUE );
}

static void leto_TransNoSort( PUSERSTRU pUStru, const char * szData )
{
   leto_Trans( pUStru, szData, HB_FALSE );
}

/* note : only used for udf_reload() */
static void leto_runFunc( PUSERSTRU pUStru, PHB_DYNS * ppSym, const char * szCommand, const char * szData, HB_ULONG ulLen )
{
#if 1
   if( pUStru && leto_IsServerLock( pUStru ) )
   {
      leto_SendAnswer( pUStru, szErr4, 4 );
      return;
   }
#endif
   if( pUStru )
      letoSetUStru( pUStru );  /* UDF functions then can retrieve it */

   if( ! ( *ppSym ) )
      *ppSym = hb_dynsymFindName( szCommand );

   if( *ppSym )
   {
      hb_vmPushDynSym( *ppSym );
      hb_vmPushNil();
      if( pUStru )
      {
         hb_vmPushString( szData, ulLen );
         hb_vmDo( 1 );
         leto_SendAnswer( pUStru, hb_parc( -1 ), hb_parclen( -1 ) );
      }
      else
      {
         HB_GC_LOCKT();
         hb_vmDo( 0 );
         HB_GC_UNLOCKT();
      }
   }
}

/* the thread for 'headless' UDF threads */
static HB_THREAD_STARTFUNC( threadX )
{
   PUSERSTRU pUStru = ( PUSERSTRU ) Cargo;
   PHB_ITEM  pArray = NULL;
   HB_USHORT uiPCount = 0;
   PHB_DYNS  pSym = hb_dynsymFindName( pUStru->szLastRequest );

   hb_vmThreadInit( NULL );
   leto_initSet();

   pUStru->llLastAct = leto_MilliSec() / 1000;
   pUStru->bRpcMode = HB_TRUE;

   hb_threadEnterCriticalSection( &s_ThxMtx );
   if( s_pThxArray && HB_IS_ARRAY( s_pThxArray ) )
      pArray = hb_arrayClone( s_pThxArray );
   hb_threadCondBroadcast( &s_ThxCond );
   hb_threadLeaveCriticalSection( &s_ThxMtx );

   /* set TSD pUStru for the following UDF call */
   letoSetUStru( pUStru );

   while( pSym )
   {
      if( s_iDebugMode > 10 )
         leto_writelog( NULL, -1, "INFO: task %s() started ..", pUStru->szLastRequest );

      pUStru->bBeQuiet = HB_TRUE;
      hb_rddDefaultDrv( pUStru->szDriver );
      hb_xvmSeqBegin();
      if( hb_vmRequestReenter() )
      {

         hb_vmPushDynSym( pSym );
         hb_vmPushNil();
         if( pArray && HB_IS_ARRAY( pArray ) )
         {
            HB_USHORT uiALen = ( HB_USHORT ) hb_arrayLen( pArray ), uiIndex;

            for( uiIndex = 1; uiIndex <= uiALen; uiIndex++ )
               hb_vmPush( hb_arrayGetItemPtr( pArray, uiIndex ) );
            uiPCount += uiALen;
         }

         hb_vmDo( uiPCount );
         hb_vmRequestRestore();
      }
      hb_xvmSeqEnd();
      if( ! pUStru->iHbError && iDebugMode() > 1 )
         leto_writelog( NULL, -1, "INFO: task %s() SUCCESSFUL", ( const char * ) pUStru->szLastRequest );
      else if( pUStru->iHbError )
         leto_writelog( NULL, -1, "ERROR task %s() ended with ERROR: ", ( const char * ) pUStru->szLastRequest,
                        pUStru->szHbError ? pUStru->szHbError : "??" );
      pUStru->bBeQuiet = HB_FALSE;
      break;
   }

   if( pArray )
      hb_itemRelease( pArray );
   leto_CloseUS( pUStru );
   hb_rddCloseAll();

   hb_vmThreadQuit();
   HB_THREAD_END
}

/* ToDo detect if UDF changed WA or active order */
static void leto_Udf( PUSERSTRU pUStru, const char * szData, HB_ULONG ulAreaID )
{
   const char * pp2 = NULL, * pp3 = NULL, * pp4 = NULL, * pp5 = NULL, * pp6 = NULL;
   char *       ptr = NULL;
   int          nParam;
   HB_BYTE      uCommand = ( ( HB_BYTE ) szData[ 0 ] & 0xFF ) - 48;

   if( ! uCommand || strlen( szData ) <  4 )
   {
      leto_wUsLog( pUStru, 0, "ERROR leto_Udf() no data about what to do ;-)" );
      leto_SendAnswer( pUStru, szErr1, 4 );
      return;
   }
   else if( ! s_bUdfEnabled )
   {
      leto_wUsLog( pUStru, 0, "DEBUG leto_Udf() use is prohibited !" );
      leto_SendAnswer( pUStru, szErr2, 4 );
      return;
   }
   else if( ( s_bPass4D && ! ( pUStru->szAccess[ 0 ] & 0x4 ) ) )
   {
      leto_SendAnswer( pUStru, szErrAcc, 4 );
      return;
   }

   /* pp2 = 0x40|0x41; pp3 = ulRecNo; pp4 = func name; for uCommand 2,3 : pp5 = _SET_EXCLUSIVE; pp6 = size, then params */
   nParam = leto_GetParam( szData, &pp2, &pp3, &pp4, NULL );
   if( uCommand != 3 && nParam >= 4 )
   {
      pp5 = pp4 + strlen( pp4 ) + 1;
      if( ( ptr = strchr( pp5, ';' ) ) != NULL )
      {
         *ptr++ = '\0';
         pp6 = ptr;
         nParam += 2;
      }
   }
   if( nParam < ( uCommand == 3 ? 2 : 6 ) || ! ( pp4 && *pp4 ) )
      leto_SendAnswer( pUStru, szErr2, 4 );
   else
   {
      PHB_DYNS pSym = hb_dynsymFindName( pp4 );

      if( uCommand == 3 ) /* leto_udfexist */
      {
         char szData1[ 4 ];

         sprintf( szData1, "+%c;", ( pSym ) ? 'T' : 'F' );
         leto_SendAnswer( pUStru, szData1, 3 );
      }
      else if( pSym )
      {
         HB_SIZE   nSize;
         PHB_ITEM  pArray = NULL;
         HB_USHORT uiPCount = 0;
         AREAP     pArea = NULL;
         PAREASTRU pAStru = NULL;
         LETOTAG * pTag = NULL;

         pUStru->bSetExclusive = ( pp5[ 0 ] == 'T' );

         if( uCommand == 9 )  /* run command in extra thread */
         {
            PUSERSTRU pUStruT = leto_InitUS( HB_NO_SOCKET );

            pUStruT->cdpage = hb_vmCDP();
            strcpy( pUStruT->szDriver, pUStru->szDriver );
            nSize = strtoul( pp6, &ptr, 10 );
            hb_threadEnterCriticalSection( &pUStru->pMutex );
            hb_threadEnterCriticalSection( &s_ThxMtx );
            if( nSize )
            {
               const char * ptrTmp = ++ptr;

               s_pThxArray = hb_itemDeserialize( &ptrTmp, &nSize );
               strncpy( ( char * ) pUStruT->szExename, pp4, 20 );
               strncpy( pUStruT->szLastRequest, pp4, 63 );
               pUStruT->szExename[ strlen( ( char * ) pUStruT->szExename ) ] = '\0';
            }
            pUStruT->hThread = hb_threadCreate( &pUStruT->hThreadID, threadX, ( void * ) pUStruT );
            if( pUStruT->hThread )
            {
               hb_threadDetach( pUStruT->hThread );
               if( s_iDebugMode > 0 )
                  leto_wUsLog( pUStru, -1, "INFO: leto_Udf threadstart with task %s() from %s:%d  connect-ID %d",
                               pp4, pUStru->szAddr, pUStru->iPort, pUStru->iUserStru );
            }
            hb_threadCondTimedWait( &s_ThxCond, &s_ThxMtx, 2100 );
            hb_threadLeaveCriticalSection( &s_ThxMtx );

            if( s_pThxArray )
            {
               hb_itemRelease( s_pThxArray );
               s_pThxArray = NULL;
            }
            if( pUStruT->hThread )
               leto_SendAnswer( pUStru, "+321", 4 );
            else
               leto_SendAnswer( pUStru, szErr4, 4 );

            hb_threadLeaveCriticalSection( &pUStru->pMutex );
         }
         else
         {
            /* ->bBeQuiet as mark that we come in peace ( UDF ) mode ;-) */
            pUStru->bBeQuiet = HB_TRUE;
            /* set TSD pUStru for the following UDF call */
            letoSetUStru( pUStru );
            if( ulAreaID )  /* LETOCMD_udf_dbf */
            {
               pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
               pAStru = pUStru->pCurAStru;
               if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
               {
                  pTag = pAStru->pTagCurrent;
                  if( pUStru->bDeleted != ( *pp2 == 0x41 ) )
                  {
                     pUStru->bDeleted = ( *pp2 == 0x41 );
                     leto_setSetDeleted( pUStru->bDeleted );
                  }
                  leto_SetAreaEnv( pAStru, pArea, pUStru );
               }
               leto_GotoIf( pArea, strtoul( pp3, NULL, 10 ) );
            }
            pUStru->ulUdfAreaID = pUStru->ulCurAreaID;
            nSize = strtoul( pp6, &ptr, 10 );
            if( nSize )
            {
               const char * ptrTmp = ++ptr;

               pArray = hb_itemDeserialize( &ptrTmp, &nSize );
            }

            if( hb_vmRequestReenter() )
            {
               hb_xvmSeqBegin();
               hb_vmPushDynSym( pSym );
               hb_vmPushNil();
               if( pArray && HB_IS_ARRAY( pArray ) )
               {
                  HB_USHORT uiALen = ( HB_USHORT ) hb_arrayLen( pArray ), uiIndex;

                  for( uiIndex = 1; uiIndex <= uiALen; uiIndex++ )
                     hb_vmPush( hb_arrayGetItemPtr( pArray, uiIndex ) );
                  uiPCount += uiALen;
               }
               hb_vmProc( uiPCount );
               hb_xvmSeqEnd();

               if( pArray )
                  hb_itemRelease( pArray );
               if( ! pUStru->iHbError )
               {
                  char *   pParam = hb_itemSerialize( hb_param( -1, HB_IT_ANY ),
                                                      HB_SERIALIZE_NUMSIZE, &nSize );
                  HB_ULONG ulLen = leto_CryptText( pUStru, pParam, nSize );

                  leto_SendAnswer( pUStru, ( char * ) pUStru->pBufCrypt, ulLen );
                  if( pParam )
                     hb_xfree( pParam );
                  if( pUStru->ulBufCryptLen > LETO_SENDRECV_BUFFSIZE )
                  {
                     if( s_iDebugMode > 10 )
                        leto_wUsLog( pUStru, -1, "DEBUG leto_Udf crypt-buffer size %lu free-ed", pUStru->ulBufCryptLen );
                     hb_xfree( pUStru->pBufCrypt );
                     pUStru->pBufCrypt = NULL;
                     pUStru->ulBufCryptLen = 0;
                  }
               }
               else
                  leto_SendError( pUStru, szErr4, 4 );
               hb_vmRequestRestore();

               /* detach all areas new requested by LETO_ALIAS() */
               if( ! s_bNoSaveWA && ulAreaID )
                  leto_FreeArea( pUStru, ulAreaID, HB_TRUE );
               /* close all tables left open after create/ open by udf */
               leto_CloseUdfAreas( pUStru );
               pUStru->ulUdfAreaID = 0;
               if( pArea != ( AREAP ) hb_rddGetCurrentWorkAreaPointer() && pAStru )
               {
                  if( s_iDebugMode > 10 )
                     leto_wUsLog( pUStru, -1, "DEBUG leto_Udf changed WA, reset to <%lu>", ulAreaID );
                  pUStru->ulCurAreaID = ulAreaID;
                  pUStru->pCurAStru = pAStru;
               }
               /* reset order for active WA when UDF was called, as maybe changed by UDF */
               if( pArea && pAStru && pTag != pAStru->pTagCurrent )
               {
                  if( s_iDebugMode > 10 )
                     leto_wUsLog( pUStru, -1, "DEBUG leto_Udf changed order, reset to <%s>", pTag ? pTag->szTagName : "" );
                  leto_SetFocus( pArea, pTag ? pTag->szTagName : "" );
                  pAStru->pTagCurrent = pTag;
               }
            }

            if( pArea )
            {
               if( ! ( s_bNoSaveWA && pAStru && ! pAStru->pTStru->bMemIO ) )
                  leto_ClearAreaEnv( pArea, pTag );
            }
         }
      }
      else
         leto_SendAnswer( pUStru, szErr2, 4 );
   }
}

static void leto_UdfFun( PUSERSTRU pUStru, const char * szData )
{
   leto_Udf( pUStru, szData, 0 );
}

static void leto_UdfDbf( PUSERSTRU pUStru, const char * szData )
{
   leto_Udf( pUStru, szData, pUStru->ulCurAreaID );
}

static void leto_Info( PUSERSTRU pUStru, const char * szData )
{
   const char * pp1;
   int          nParam = leto_GetParam( szData, &pp1, NULL, NULL, NULL );

   if( nParam < 1 )
      leto_SendAnswer( pUStru, szErr3, 4 );
   else
   {
      AREAP     pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      HB_USHORT uiCommand = ( HB_USHORT ) atoi( szData );

      if( ! pArea )
      {
         leto_SendAnswer( pUStru, szErr2, 4 );
         return;
      }

      switch( uiCommand )
      {
         case DBI_ISENCRYPTED:
            if( ( ( DBFAREAP ) pArea )->fTableEncrypted )
               leto_SendAnswer( pUStru, szOk, 4 );
            else
               leto_SendAnswer( pUStru, szErr1, 4 );
            break;

         case DBI_PASSWORD:
         case DBI_ENCRYPT:  /* hb_dbfTableCrypt( pArea, pPasswd, uiCommand == DBI_ENCRYPT ? HB_TRUE, HB_FALSE ); */
         case DBI_DECRYPT:
            if( ( ( DBFAREAP ) pArea )->fHasMemo )  /* table encryption only for tables without memofields */
               leto_SendAnswer( pUStru, szErr2, 4 );
            else if( uiCommand == DBI_DECRYPT && ! ( ( DBFAREAP ) pArea )->fTableEncrypted )
               leto_SendAnswer( pUStru, szErr2, 4 );
            else if( uiCommand == DBI_ENCRYPT && ( ( DBFAREAP ) pArea )->fTableEncrypted )
               leto_SendAnswer( pUStru, szErr2, 4 );
            else if( pp1 && *pp1 )
            {
               PHB_ITEM  pPasswd = NULL;
               char *    szKey = leto_localKey( pUStru->cDopcode, LETO_DOPCODE_LEN );
               char *    szPass;
               HB_ULONG  ulLen = strlen( pp1 );

               szPass = ( char * ) hb_xgrab( ulLen / 2 + 1 );
               leto_hexchar2byte( pp1, ( int ) ulLen, szPass );
               leto_decrypt( szPass, ulLen / 2, szPass, &ulLen, szKey, HB_FALSE );
               if( szKey )
                  hb_xfree( szKey );

               if( ulLen > 0 )
               {
                  pPasswd = hb_itemPutC( NULL, szPass );
                  SELF_INFO( pArea, uiCommand, pPasswd );
                  leto_SendAnswer( pUStru, szOk, 4 );
                  memset( szPass, ' ', ulLen );
                  hb_itemPutC( pPasswd, szPass );
               }
               else
                  leto_SendAnswer( pUStru, szErr1, 4 );
               hb_xfree( szPass );
               if( pPasswd )
                  hb_itemRelease( pPasswd );
            }
            break;

         case DBI_LOCKCOUNT:
         case DBI_GETLOCKARRAY:
         {
            PLETO_LOCK_ITEM pLockA;
            char * szData1;
            char * ptr;
            int    iLen = 0;

            HB_GC_LOCKT();
            /* ToDo ? alternative use: pArray = hb_itemArrayNew(0); SELF_INFO( pArea, DBI_GETLOCKARRAY, pArray ) */
            /*                       : DBFAREAP pArea->ulNumLocksPos */
            letoListLock( &pUStru->pCurAStru->pTStru->LocksList );

            for( pLockA = ( PLETO_LOCK_ITEM ) pUStru->pCurAStru->pTStru->LocksList.pItem;
                 pLockA; pLockA = pLockA->pNext )
            {
               iLen++;
            }

            if( uiCommand == DBI_GETLOCKARRAY )
            {
               szData1 = ( char * ) hb_xgrab( ( iLen * 11 ) + 13 );
               ptr = szData1;
               ptr += sprintf( ptr, "+%d;", iLen );

               for( pLockA = ( PLETO_LOCK_ITEM ) pUStru->pCurAStru->pTStru->LocksList.pItem;
                    pLockA; pLockA = pLockA->pNext )
               {
                  ptr += ultostr( pLockA->ulRecNo, ptr );
                  *ptr++ = ';';
               }
            }
            else
            {
               szData1 = ( char * ) hb_xgrab( 13 );
               ptr = szData1;
               ptr += sprintf( szData1, "+%d;", iLen );
            }

            letoListUnlock( &pUStru->pCurAStru->pTStru->LocksList );
            HB_GC_UNLOCKT();
            leto_SendAnswer( pUStru, szData1, ptr - szData1 );
            hb_xfree( szData1 );
            break;
         }

         case DBI_TRIGGER:
         {
            PHB_ITEM pItem = hb_itemNew( NULL );
            char     szData1[ 258 ];
            int      iLen;

            if( *pp1 )
            {
               if( *pp1 == '.' )
                  hb_itemPutL( pItem, ( *( pp1 + 1 ) == 'T' ) );
               else
                  hb_itemPutC( pItem, pp1 );
            }
            SELF_INFO( pArea, DBI_TRIGGER, pItem );
            szData1[ 0 ] = '+';
            iLen = hb_itemGetCLen( pItem );
            if( iLen > 255 )
               iLen = 255;
            if( iLen > 0 )
               memcpy( szData1 + 1, hb_itemGetCPtr( pItem ), iLen );
            szData1[ iLen + 1 ] = ';';
            szData1[ iLen + 2 ] = '\0';

            leto_SendAnswer( pUStru, szData1, strlen( szData1 ) );
            hb_itemRelease( pItem );
            break;
         }

         case DBI_DBS_COUNTER:
         case DBI_DBS_STEP:
         {
            PHB_ITEM     pItem = hb_itemNew( NULL );
            HB_USHORT    uiField = ( HB_USHORT ) atoi( pp1 );
            HB_ERRCODE   errCode;
            const char * pp2 = pp1 + strlen( pp1 ) + 1;

            if( *pp2 != ';' )
               pItem = hb_itemPutNInt( pItem, strtoul( pp2, NULL, 10 ) );
             if( uiCommand == DBI_DBS_COUNTER )
                errCode = SELF_FIELDINFO( pArea, uiField, DBS_COUNTER, pItem );
             else
                errCode = SELF_FIELDINFO( pArea, uiField, DBS_STEP, pItem );
            if( errCode == HB_SUCCESS && HB_IS_NUMERIC( pItem ) )
            {
               char     szData1[ 32 ];
               HB_ULONG ulLen;

               szData1[ 0 ] = '+';
               ulLen = 1 + ultostr( hb_itemGetNInt( pItem ), szData1 + 1 );
               szData1[ ulLen++ ] = ';';
               szData1[ ulLen ] = '\0';
               leto_SendAnswer( pUStru, szData1, ulLen );
            }
            else
               leto_SendAnswer( pUStru, szErr4, 4 );
            hb_itemRelease( pItem );
            break;
         }

         default:
            leto_SendAnswer( pUStru, szErr4, 4 );
      }
   }
}

static void leto_OrderInfo( PUSERSTRU pUStru, const char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   const char * pOrder, * pOrdPar;
   const char * pData;
   char         szData1[ 20 ];
   int          nParam = leto_GetParam( szData, &pOrder, &pOrdPar, NULL, NULL );

   if( nParam < 2 || ! pArea )
      pData = szErr2;
   else
   {
      HB_USHORT uiCommand = ( HB_USHORT ) atoi( szData );

      if( ! uiCommand || ! *pOrder )
         pData = szErr3;
      else
      {
         DBORDERINFO pOrderInfo;

         memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
         pOrderInfo.itmOrder = hb_itemPutC( NULL, pOrder );
         switch( uiCommand )
         {
            case DBOI_ISDESC:
            case DBOI_CUSTOM:
            case DBOI_UNIQUE:
               pOrderInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
               if( nParam > 3 && pOrdPar[ 0 ] )
                  pOrderInfo.itmNewVal = hb_itemPutL( NULL, ( pOrdPar[ 0 ] == 'T' ) );
               break;

            case DBOI_KEYADD:
            case DBOI_KEYDELETE:
            {
               HB_UCHAR     uKeyLen;
               const char * pOrdKey;

               if( leto_GotoIf( pArea, strtoul( pOrdPar, NULL, 10 ) ) == HB_SUCCESS )
               {
                  pOrdKey = pOrdPar + strlen( pOrdPar ) + 1;       /* binary data */
                  uKeyLen = ( ( ( HB_UCHAR ) *pOrdKey++ ) & 0xFF );
                  if( uKeyLen )
                     pOrderInfo.itmNewVal = leto_KeyToItem( pArea, pOrdKey, uKeyLen, pOrder, '\0' );
               }

               break;
            }

            default:
               pOrderInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
               if( nParam > 3 && pOrdPar[ 0 ] )
                  pOrderInfo.itmNewVal = hb_itemPutC( NULL, pOrdPar );
         }

         SELF_ORDINFO( pArea, uiCommand, &pOrderInfo );
         szData1[ 0 ] = '+';
         szData1[ 1 ] = hb_itemGetL( pOrderInfo.itmResult ) ? 'T' : 'F';
         szData1[ 2 ] = ';';
         szData1[ 3 ] = '\0';
         hb_itemRelease( pOrderInfo.itmResult );
         if( pOrderInfo.itmNewVal )
            hb_itemRelease( pOrderInfo.itmNewVal );
         pData = szData1;
      }
   }

   leto_SendAnswer( pUStru, pData, strlen( pData ) );
}

static void leto_Pong( PUSERSTRU pUStru, const char * szData )
{
   HB_SYMBOL_UNUSED( pUStru );
   HB_SYMBOL_UNUSED( szData );

#if 0  /* to test the delayed error */
   leto_SendAnswer2( pUStru, szErr3, 4, HB_FALSE, 1001 );
#endif
   leto_SendAnswer( pUStru, "+@;", 3 );
}

static int leto_ValidateAlias( PUSERSTRU pUStru, const char * szAlias, PTABLESTRU pTStru, HB_ULONG *pulDblAreaID )
{
   PAREASTRU       pAStru;
   PLETO_LIST_ITEM pListItem = pUStru->AreasList.pItem;
   int             iDifferentAlias = 0;
   int             iDoubleAlias = 0;
   HB_U32          uiCrc = leto_hash( szAlias, strlen( szAlias ) );

   while( pListItem )
   {
      pAStru = ( PAREASTRU ) ( pListItem + 1 );
      if( pAStru->ulAreaID == pTStru->ulAreaID )
      {
         if( uiCrc == pAStru->uiCrc && ! leto_stricmp( pAStru->szAlias, szAlias ) )
         {
            /* dev*i*loper will re-use ALIAS for same table ..*/
            iDifferentAlias = -1;
            leto_wUsLog( pUStru, -1, "ERROR try to open same DBF with same alias: %s", szAlias );
            break;
         }
         else /* we already use this table, but with different alias -- check further for any doubles */
            iDifferentAlias = 1;
      }
      if( uiCrc == pAStru->uiCrc && ! leto_stricmp( pAStru->szAlias, szAlias ) )
      {
         if( ++iDoubleAlias > 1 )
         {
            *pulDblAreaID = pAStru->ulAreaID;
            iDifferentAlias = -2;
            leto_wUsLog( pUStru, -1, "ERROR ALIAS %s used more than once", szAlias );
            break;
         }
      }

      pListItem = pListItem->pNext;
   }

   return iDifferentAlias;
}

static HB_USHORT leto_FindFreeUdfArea( void )
{
   AREAP pArea;
   int   iArea;

   for( iArea = 32768; iArea < 65535; iArea++ )
   {
      pArea = ( AREAP ) hb_rddGetWorkAreaPointer( iArea );
      if( pArea )
      {
         char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

         if( SELF_ALIAS( pArea, szAlias ) != HB_SUCCESS || ! *szAlias )
            return ( HB_USHORT ) iArea;
      }
      else
         return ( HB_USHORT ) iArea;
   }

   leto_writelog( NULL, -1, "DEBUG leto_FindFreeUdfArea() no more free areas, %d used", 65535 - iArea );
   return 0;
}

/* based on pAStru, return next free area */
static HB_USHORT leto_FindFreeArea( PUSERSTRU pUStru )
{
   PLETO_LIST_ITEM pListItem;
   PAREASTRU       pAStru;
   HB_ULONG        ulArea = 0;

   pListItem = pUStru->AreasList.pItem;
   while( pListItem )
   {
      pAStru = ( PAREASTRU ) ( pListItem + 1 );
      if( pAStru->ulSelectID > ulArea )
         ulArea = pAStru->ulSelectID;
      pListItem = pListItem->pNext;
   }

   ulArea++;

   return ( HB_USHORT ) ulArea;
}

static void leto_OpenTable( PUSERSTRU pUStru, const char * szRawData )
{
   char *       szFileRaw = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFileName = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFile = ( char * ) hb_xgrab( HB_PATH_MAX );
   char         szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   char         szRealAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   char         szDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];
   unsigned int uiReplyBufLen = 1023;
   char *       szReply = hb_xgrab( uiReplyBufLen + 1 );
   char *       ptr, * ptr2;
   const char * pData = NULL;
   const char * pp2, * pp3, * szCdp, * pp5;
   int          nParam;
   HB_USHORT    uiNtxType;
   AREAP        pArea  = NULL;
   HB_BOOL      bLeadSep, bMemIO, bShared, bReadonly;
   HB_SIZE      nLen;
   PHB_FNAME    pFilePath;
   HB_ERRCODE   errcode = HB_SUCCESS;
   HB_ULONG     ulLenLen = 4;
   HB_ULONG     ulSelectID = 0;
   HB_ULONG     ulAreaID = 0;

   szReply[ 0 ] = '\0';
   szDriver[ 0 ] = '\0';
   szFileName[ 0 ] = '\0';
   szRealAlias[ 0 ] = '\0';
   pUStru->bLastAct = HB_FALSE;

   /* szRawData = filename, pp2 = Alias, pp3 = shared/readonly, szCdp = codepage, pp5 = DBdriver */
   nParam = leto_GetParam( szRawData, &pp2, &pp3, &szCdp, &pp5 );
   nLen = strlen( szRawData );

   if( leto_IsServerLock( pUStru ) )
      strcpy( szReply, szErrLck );
   else if( nParam < 5 || nLen < 1 || ! pp3 || strlen( pp3 ) < 2 )
      strcpy( szReply, szErr2 );
   else
   {
      memcpy( szFileRaw, szRawData, nLen );
      szFileRaw[ nLen ] = '\0';
      leto_StrTran( szFileRaw, DEF_CH_SEP, DEF_SEP, nLen );
      bLeadSep = ( szFileRaw[ 0 ] == DEF_SEP );
      pFilePath = hb_fsFNameSplit( szFileRaw );
      bMemIO = ! strncmp( szFileRaw + ( bLeadSep ? 1 : 0 ), "mem:", 4 );

      ptr = szFileName;
      ptr2 = szFileRaw;
      if( bLeadSep )
         ptr2++;
      if( ! bMemIO )  /* add root path before */
      {
         strcpy( ptr, s_pDataPath );
         ptr += strlen( ptr );
         *ptr++ = DEF_SEP;
      }
      strcpy( szFile, ptr2 );

      if( s_bLowerPath )
      {
         hb_strLower( szFileName, strlen( szFileName ) );
         hb_strLower( szFile, strlen( szFile ) );
      }
      strcpy( ptr, szFile );

      /* Alias */
      if( pp2 && *pp2 )
      {
         strcpy( szAlias, pp2 );
         hb_strUpper( szAlias, strlen( szAlias ) );
      }
      else
      {
         /* create an ALIAS from szFileName, cut off any extension */
         ptr2 = szFile + strlen( szFile ) - 1;
         while( ( ptr2 - szFile ) > 0 && *ptr2 != DEF_SEP && *ptr2 != ':' )
            ptr2--;
         if( *ptr2 == DEF_SEP || *ptr2 == ':' )
            ptr2++;
         strcpy( szAlias, ptr2 );
         if( ( ptr2 = strrchr( szAlias, '.' ) ) != NULL )
            *ptr2 = '\0';
         hb_strUpper( szAlias, strlen( szAlias ) );
         if( s_iDebugMode > 20 )
            leto_wUsLog( pUStru, -1, "DEBUG universal ALIAS <%s> created from filename", szAlias );
      }

      bShared = pp3[ 0 ] == 'T' ? HB_TRUE : HB_FALSE;
      bReadonly = ( pp3[ 1 ] == 'T' ) ? HB_TRUE : HB_FALSE;

      if( ! szCdp || ! *szCdp )  /* important: valid strlen or NULL ! */
         szCdp = NULL;

      strcpy( szDriver, pp5 );
      if( ! *szDriver )  /* should not happen, client ever transmit driver type */
         strcpy( szDriver, leto_Driver( s_uiDriverDef ) );
      hb_strUpper( szDriver, strlen( szDriver ) );
      uiNtxType = ( strstr( szDriver, "NTX" ) != NULL ) ? 1 : 0;

      if( ! pFilePath->szExtension )
      {
         char * szExt = ( char * ) hb_xgrab( HB_MAX_FILE_EXT + 1 );

         leto_RddiGetValue( szDriver, RDDI_TABLEEXT, szExt );
         if( *szExt )
         {
            if( s_bLowerPath )
               hb_strLower( szExt, strlen( szExt ) );
            strcpy( szFile + strlen( szFile ), szExt );
            strcpy( szFileName + strlen( szFileName ), szExt );
         }
         hb_xfree( szExt );
      }
      hb_xfree( pFilePath );

      if( ! bMemIO && ! strchr( szFile, DEF_SEP ) )
      {
         char * szFilePath = ( char * ) hb_xgrab( HB_PATH_MAX );

         if( hb_fileExists( szFile, szFilePath ) )
         {
            strcpy( szFileName, szFilePath );
            strcpy( szFile, szFileName + s_uiDataPathLen + 1 );
         }
         hb_xfree( szFilePath );
      }

      pp5 += strlen( pp5 ) + 1;
      if( *pp5 )
         ulSelectID = strtoul( pp5, &ptr2, 10 );
      else
         ptr2 = NULL;
      if( ( s_bNoSaveWA && ! bMemIO ) && ! ulSelectID )
         errcode = HB_FAILURE;

      if( ptr2 )
      {
         pp5 = ptr2 + 1;
         if( ( ptr2 = strchr( pp5, ';' ) ) != NULL && ptr2 - pp5 > 1 )
         {
            if( ! pUStru->szDateFormat || strncmp( pUStru->szDateFormat, pp5, ptr2 - pp5 ) ||
                strlen( pUStru->szDateFormat ) != ( unsigned int ) ( ptr2 - pp5 ) )
            {
               PHB_ITEM pItem = hb_itemNew( NULL );

               if( pUStru->szDateFormat )
                  hb_xfree( pUStru->szDateFormat );
               pUStru->szDateFormat = ( char * ) hb_xgrab( ptr2 - pp5 + 1 );
               memcpy( pUStru->szDateFormat, pp5, ptr2 - pp5 );
               pUStru->szDateFormat[ ptr2 - pp5 ] = '\0';
               pItem = hb_itemPutC( pItem, pUStru->szDateFormat );
               hb_setSetItem( HB_SET_DATEFORMAT, pItem );
               hb_itemRelease( pItem );
               if( s_iDebugMode > 10 )
                  leto_wUsLog( pUStru, -1, "DEBUG leto_CreateTable new dateformat %s set", hb_setGetDateFormat() );
            }
         }
      }

      if( ! szFileName[ 0 ] )
      {
         pData = szErr1;
         errcode = HB_FAILURE;
      }
      else
      {
         HB_BOOL  bUnlocked = HB_FALSE;
         int      iTableStru = -1;
         int      iOpenDouble = 0;

         HB_GC_LOCKT();  /* up here nobody shell open simultanous another area until leto_InitTable() is done */

         /* MemIO Tables are *ever* opened only once */
         if( ! ( ! bMemIO && s_bNoSaveWA ) )  /* else we must not search ToDo: ? easy early validate */
         {
            iTableStru = leto_FindTable( szFile, &ulAreaID );

            if( ( iTableStru >= 0 ) )
            {
               HB_ULONG ulDblAreaID = 0;

               iOpenDouble = leto_ValidateAlias( pUStru, szAlias, s_tables + iTableStru, &ulDblAreaID );
               if( iOpenDouble < 0 )  /* same ALIAS for same TABLE, or double used ALIAS - SLAP !! */
               {
                  pData = szErr1;
                  errcode = HB_FAILURE;
                  if( pUStru->bBeQuiet )  // ToFix
                     pUStru->ulCurAreaID = ulDblAreaID;
               }
               else if( iOpenDouble > 0 )  /* if possible re-open with diff ALIAS in NON exclusive mode */
               {
                  if( ( s_tables + iTableStru )->bShared && ( s_bShareTables || s_bNoSaveWA ) && bShared )
                  {
                     if( s_iDebugMode >= 10 )
                        leto_wUsLog( pUStru, -1, "INFO: leto_Opentable re-open with Alias: <%s> %s (in %lu)",
                                     szAlias, szFileName, ulAreaID );
                     iOpenDouble = 0;
                  }
                  else
                  {
                     pData = szErr1;
                     errcode = HB_FAILURE;
                     if( pUStru->bBeQuiet )  // ToFix
                        pUStru->ulCurAreaID = ulDblAreaID;
                  }
               }
            }
         }

         if( iTableStru < 0 && s_uiTablesCurr >= s_uiTablesAlloc )
         {
            leto_writelog( NULL, -1, "ERROR: leto_Opentable configured maximum tables %d exceeded (active: %d file: %s)",
                           s_uiTablesAlloc, s_uiTablesCurr, szFile );
            pData = szErr2;
            errcode = HB_FAILURE;
         }

         if( errcode == HB_FAILURE )
         {
            if( iOpenDouble < 0 )
               leto_wUsLog( pUStru, -1, "ERROR leto_Opentable same alias: %s for same %s", szAlias, szFileName );
            else if( iOpenDouble > 0 )
               leto_wUsLog( pUStru, -1, "ERROR leto_Opentable EXCL. mode ALIAS %s already used for %s",
                            szAlias, szFileName );
         }
         else if( iTableStru < 0 )
         {
            HB_USHORT uiArea = 0;
            PHB_ITEM  pStruct = NULL, pDelim = NULL;

            if( ( s_bNoSaveWA && ! bMemIO ) )
            {
               strcpy( szRealAlias, szAlias );
               ulAreaID =  ulSelectID;
               uiArea = ( HB_USHORT ) ulSelectID;
            }
            else
            {
               if( ! s_bNoSaveWA )
               {
                  ulAreaID = leto_CreateAreaID();
                  leto_MakeAlias( ulAreaID, szRealAlias );
               }
               else  /* ( s_bNoSaveWA && bMemIO ) */
               {
                  ulAreaID =  ulSelectID;
                  leto_MakeAlias( leto_CreateAreaID(), szRealAlias );  // ToDO memio alias
               }
            }

            hb_xvmSeqBegin();
            hb_rddSetNetErr( HB_FALSE );
            errcode = hb_rddOpenTable( szFileName, ( const char * ) szDriver, uiArea, szRealAlias,
                                       ( ( s_bShareTables || s_bNoSaveWA ) && bShared && ! bMemIO ),
                                       ( ( s_bShareTables || s_bNoSaveWA ) && bReadonly ),
                                       szCdp, 0, pStruct, pDelim );
            hb_xvmSeqEnd();

            if( pUStru->iHbError )
               errcode = HB_FAILURE;
            else
               pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
            if( ! pArea )
               errcode = HB_FAILURE;

            if( errcode != HB_SUCCESS )
            {
               if( s_bNoSaveWA && bMemIO )
                  leto_DelAreaID( ulAreaID );
               if( pUStru->iHbError == 32 || hb_rddGetNetErr() )
                  sprintf( szReply, "%s%s%s", szErr4, pUStru->szHbError ? pUStru->szHbError + 4 : ":21-1023-0-0\t", szFile );  /* EDBF_SHARED */
               else
                  sprintf( szReply, "%s%s%s", szErr3, pUStru->szHbError ? pUStru->szHbError + 4 : ":21-1001-0-0\t", szFile );  /* EDBF_OPEN_DBF */
            }
            else
            {
               PHB_ITEM pItem = hb_itemNew( NULL );

               hb_itemPutNI( pItem, leto_lockScheme( uiNtxType ) );
               hb_setSetItem( HB_SET_DBFLOCKSCHEME, pItem );
               if( pItem )
                  hb_itemRelease( pItem );

               strcpy( pUStru->szDriver, szDriver );
               if( ! ulSelectID )
                  ulSelectID = hb_rddGetCurrentWorkAreaNumber();

               if( ( iTableStru = leto_InitTable( ulAreaID, szFile, szDriver, bShared, szRealAlias, bReadonly, szCdp ) ) >= 0 )
               {
                  if( ! leto_InitGlobeValidCP( iTableStru, szCdp ) )
                  {
                     /* tried to open with different CP */
                     ptr = szReply;
                     strcpy( szReply, szErr4 );
                     strcpy( szReply + 4, ":21-1055-0-0\t" );  /* EDBF_SIGNATURE */
                     ptr += 17;
                     strcpy( ptr, szFile );
                     errcode = HB_FAILURE;

                     leto_CloseTable( ( s_tables + iTableStru ) );
                  }
                  HB_GC_UNLOCKT();
                  bUnlocked = HB_TRUE;
                  if( errcode == HB_SUCCESS )
                     leto_InitArea( pUStru, iTableStru, ulAreaID, szAlias, szRealAlias, ulSelectID );
               }
               else
               {
                  pData = szErr3;
                  errcode = HB_FAILURE;
               }
            }
         }
         else
         {
            if( ! ( ( s_tables + iTableStru )->bShared ) || ! bShared )  // todo check for memio
            {
               /* The table is already opened exclusive by another user */
               ptr = szReply;
               strcpy( szReply, szErr4 );
               strcpy( szReply + 4, ":21-1023-0-0\t" );  /* EDBF_SHARED */
               ptr += 17;
               strcpy( ptr, szFile );
               errcode = HB_FAILURE;
            }
            else
            {
               if( leto_InitGlobeValidCP( iTableStru, szCdp ) )
               {
                  ( s_tables + iTableStru )->uiAreas++;
                  HB_GC_UNLOCKT();
                  bUnlocked = HB_TRUE;
                  leto_InitArea( pUStru, iTableStru, ulAreaID, szAlias, NULL, ulSelectID );
                  leto_SelectArea( pUStru, ulAreaID );
               }
               else
               {
                  /* tried to open with different CP */
                  ptr = szReply;
                  strcpy( szReply, szErr4 );
                  strcpy( szReply + 4, ":21-1055-0-0\t" );  /* EDBF_SIGNATURE */
                  ptr += 17;
                  strcpy( ptr, szFile );
                  errcode = HB_FAILURE;
               }
            }
         }

         if( ! bUnlocked )
            HB_GC_UNLOCKT();
      }

      if( errcode == HB_SUCCESS && ! pArea )
      {
         pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
         if( ! pArea )
            errcode = HB_FAILURE;
         else if( ! uiNtxType && hb_setGetAutOpen() )  /* non NTX */
         {
            DBORDERINFO pInfo;

            memset( &pInfo, 0, sizeof( DBORDERINFO ) );
            pInfo.itmOrder = hb_itemPutNI( NULL, hb_setGetAutOrder() );
            pInfo.itmResult = hb_itemPutC( NULL, NULL );
            SELF_ORDLSTFOCUS( pArea, &pInfo );
            hb_itemRelease( pInfo.itmOrder );
            hb_itemRelease( pInfo.itmResult );
         }
      }

      if( errcode != HB_SUCCESS )
      {
         if( ! pData )
            pData = szErr1;
         if( ! *szReply )
            strcpy( szReply, pData );
         ulLenLen = strlen( szReply );
      }
      else
      {
         PHB_ITEM  pFldName = hb_itemNew( NULL );
         PHB_ITEM  pFldType = hb_itemNew( NULL );
         PHB_ITEM  pFldLen = hb_itemNew( NULL );
         PHB_ITEM  pFldDec = hb_itemNew( NULL );
         HB_ULONG  ulRecLen, ulLen;
         HB_USHORT uiFieldCount, ui;
         char *    szTmp = NULL;

         strcpy( pUStru->szDriver, szDriver );
         ptr = szReply;
         *ptr++ = '+';
         ptr += ultostr( ulAreaID, ptr );
         *ptr++ = ';';
         *ptr++ = ( char ) ( '0' + uiNtxType );  /* LETO_NTX = 1, LETO_CDX = 0 */
         pUStru->uiDriver = uiNtxType;
         *ptr++ = ';';
         ptr += leto_MemoInfo( pArea, ptr );
         ptr += ultostr( leto_LastUpdate( pArea ), ptr );
         *ptr++ = ';';
         SELF_FIELDCOUNT( pArea, &uiFieldCount );
         ptr += eprintf( ptr, "%u", uiFieldCount );
         *ptr++ = ';';
         ulLenLen = ptr - szReply;

         /* estimate max needed length -- see leto_recLen() for explanation .. */
         if( ( ulLen = ulLenLen + leto_RecordLen( pArea ) + 25 + ( uiFieldCount * 30 ) ) > uiReplyBufLen )
         {
            uiReplyBufLen = ulLen;
            szReply = ( char * ) hb_xrealloc( szReply, uiReplyBufLen + 1 );
            ptr = szReply + ulLenLen;
         }

         for( ui = 1; ui <= uiFieldCount; ui++ )
         {
            SELF_FIELDINFO( pArea, ui, DBS_NAME, pFldName );
            SELF_FIELDINFO( pArea, ui, DBS_TYPE, pFldType );
            SELF_FIELDINFO( pArea, ui, DBS_LEN, pFldLen );
            SELF_FIELDINFO( pArea, ui, DBS_DEC, pFldDec );
            ptr += eprintf( ptr, "%s;%s;%u;%u;", hb_itemGetCPtr( pFldName ), hb_itemGetCPtr( pFldType ),
                                                 hb_itemGetNI( pFldLen ), hb_itemGetNI( pFldDec ) );
            hb_itemClear( pFldName );
            hb_itemClear( pFldType );
         }
         hb_itemRelease( pFldName );
         hb_itemRelease( pFldType );
         hb_itemRelease( pFldLen );
         hb_itemRelease( pFldDec );

         if( ! uiNtxType && hb_setGetAutOpen() )  /* non NTX */
         {
            char * szExt = ( char * ) hb_xgrab( HB_MAX_FILE_EXT + 1 );
            char * szIFile = ( char * ) hb_xgrab( HB_PATH_MAX );
            char * szIFileName = ( char * ) hb_xgrab( HB_PATH_MAX );

            /* calculate filename of possible production index */
            leto_RddiGetValue( szDriver, RDDI_ORDBAGEXT, szExt );
            if( ( ptr2 = strrchr( szFile, '.' ) ) != NULL )
            {
               memcpy( szIFile, szFile, ptr2 - szFile );
               szIFile[ ptr2 - szFile ] = '\0';
            }
            else
               strcpy( szIFile, szFile );
            if( ( ptr2 = strrchr( szFileName, '.' ) ) != NULL )
            {
               memcpy( szIFileName, szFileName, ptr2 - szFileName );
               szIFileName[ ptr2 - szFileName ] = '\0';
            }
            else
               strcpy( szIFileName, szFileName );
            if( *szExt )
            {
               if( s_bLowerPath )
                  hb_strLower( szExt, strlen( szExt ) );
               strcpy( szIFile + strlen( szIFile ), szExt );
               strcpy( szIFileName + strlen( szIFileName ), szExt );
            }

            szTmp = leto_IndexesInfo( pUStru, szIFile, pArea );
            hb_xfree( szIFileName );
            hb_xfree( szIFile );
            hb_xfree( szExt );
         }

         /* add info about orders */
         if( szTmp )
         {
            ulLen = strlen( szTmp );
            /* if( ulLen > 2 && iTableStru < 0 ==> production index opened by RDD */
            ulLenLen = ptr - szReply;
            if( ulLen + ulLenLen > uiReplyBufLen )
            {
               uiReplyBufLen = ulLen + ulLenLen;
               szReply = ( char * ) hb_xrealloc( szReply, uiReplyBufLen + 1 );
               ptr = szReply + ulLenLen;
            }
            strcpy( ptr, szTmp );
            ptr += ulLen;
            hb_xfree( szTmp );
         }
         else
         {
            strcpy( ptr, "0;" );  /* '0;' for non index opened */
            ptr += 2;
         }

         /* add record data */
         ulLenLen = ptr - szReply;
         ulRecLen = leto_recLen( pUStru->pCurAStru->pTStru );
         if( ulRecLen > 0 )
         {
            HB_ULONG ulRealLen;

            szTmp = ( char * ) hb_xgrab( ulRecLen );
            ulRealLen = leto_rec( pUStru, pUStru->pCurAStru, pArea, szTmp, NULL );
            if( ulRealLen )
            {
               if( ulRealLen + ulLenLen > uiReplyBufLen )
               {
                  uiReplyBufLen = ulRealLen + ulLenLen;
                  szReply = ( char * ) hb_xrealloc( szReply, uiReplyBufLen + 1 );
                  ptr = szReply + ulLenLen;
               }
               memcpy( ptr, szTmp, ulRealLen );
               ptr += ulRealLen;
               *ptr = '\0';
               ulLenLen += ulRealLen;
            }
            else  /* should not happen */
            {
               strcpy( szReply, szErr4 );
               ulLenLen = 4;
            }

            hb_xfree( szTmp );
         }
      }
   }

   if( ! pUStru->bBeQuiet )
      leto_SendAnswer( pUStru, szReply, ulLenLen );
   hb_xfree( szReply );
   hb_xfree( szFile );
   hb_xfree( szFileName );
   hb_xfree( szFileRaw );
   if( errcode == HB_SUCCESS )
      pUStru->bLastAct = HB_TRUE;
}

/* leto_udf() leto_DbUseArea( [ cDriver ], cFileName, [cAlias, lShared, lReadOnly, cdp ] ) */
HB_FUNC( LETO_DBUSEAREA )  /* 'wrong' number for not fresh opened */
{
   HB_USHORT uiArea;
   PUSERSTRU pUStru = letoGetUStru();

   if( s_bNoSaveWA )
      uiArea = leto_FindFreeUdfArea();
   else
      uiArea = leto_FindFreeArea( pUStru );

   if( ( pUStru->bRpcMode || s_bNoSaveWA ) && uiArea && HB_ISCHAR( 2 ) && hb_parclen( 2 ) )
   {
      char         szFile[ HB_PATH_MAX ];
      const char * szDriver = HB_ISCHAR( 1 ) ? hb_parc( 1 ) : pUStru->szDriver;
      const char * szAlias = HB_ISCHAR( 3 ) ? hb_parc( 3 ) : "";
      HB_BOOL      fShared = HB_ISLOG( 4 ) ? hb_parl( 4 ) : ! pUStru->bSetExclusive;
      HB_BOOL      fReadOnly = HB_ISLOG( 5 ) ? hb_parl( 5 ) : HB_FALSE;
      const char * szCdpage = HB_ISCHAR( 6 ) && hb_parclen( 6 ) ? hb_parc( 6 ) : NULL;
      char         szData[ HB_PATH_MAX + HB_RDD_MAX_ALIAS_LEN + HB_RDD_MAX_DRIVERNAME_LEN + 23 ];
      HB_BOOL      bRet;
      char *       ptrdouble;

      ptrdouble = szFile;
      strncpy( szFile, HB_ISCHAR( 2 ) ? hb_parc( 2 ) : "", HB_PATH_MAX );
      szFile[ HB_ISCHAR( 2 ) ? HB_MIN( hb_parclen( 2 ), HB_PATH_MAX ) : 0 ] = '\0';
      if( strlen( ptrdouble ) >= 2 && *( ptrdouble + 1 ) == ':' )  /* C:\... */
         ptrdouble += 2;
      while( *ptrdouble == '/' || *ptrdouble == '\\' )
      {
         ptrdouble++;
      }
      if( ptrdouble > szFile )
         memmove( szFile, ptrdouble, strlen( ptrdouble ) + 1 );
      while( ( ptrdouble = strstr( szFile, "//" ) ) != NULL ||
             ( ptrdouble = strstr( szFile, "\\\\" ) ) != NULL )
      {
         memmove( ptrdouble, ptrdouble + 1, strlen( ptrdouble + 1 ) + 1 );
      }

      if( ! szCdpage || ! hb_cdpFind( szCdpage ) )
         szCdpage = pUStru->cdpage->id;

      sprintf( szData, "%s;%s;%c%c;%s;%s;%d;", szFile,             /* LETOCMD_open */
               szAlias, ( fShared ) ? 'T' : 'F', ( fReadOnly ) ? 'T' : 'F', szCdpage,
               szDriver, uiArea );

      leto_OpenTable( pUStru, szData );
      bRet = pUStru->bLastAct;

      if( ! bRet && pUStru->ulCurAreaID )
         bRet = HB_TRUE;
      if( bRet && ( ! s_bNoSaveWA || ! pUStru->pCurAStru->ulUdf ) )  // ToDo
         bRet = ( leto_SelectArea( pUStru, pUStru->ulCurAreaID ) != NULL );

      if( bRet )
         hb_retnl( pUStru->pCurAStru->ulSelectID );
      else
         hb_retnl( 0 );
   }
   else
      hb_retnl( 0 );
}

/* help function for leto_CreateTable() to set non-standard memofile */
static void leto_SetMemoEnv( PUSERSTRU pUStru, const char * szDBType, int iMemoType, int iMemoVersion, int iMemoBlocksize )
{
   LPRDDNODE pRDDNode = NULL;
   HB_USHORT uiRddID;

   if( szDBType && *szDBType )
      pRDDNode = hb_rddFindNode( szDBType, &uiRddID );

   if( iMemoType >= 0 )
   {
      PHB_ITEM pItem = NULL;

      pUStru->uiMemoType = ( HB_USHORT ) iMemoType;
      if( pRDDNode )
      {
         PHB_ITEM pFileExt = NULL;

         pItem = hb_itemPutNI( NULL, iMemoType );
         SELF_RDDINFO( pRDDNode, RDDI_MEMOTYPE, 0, pItem );
         /* to check: need that hb_setSetItem() ? */
         switch( iMemoType )
         {
            case DB_MEMO_DBT:
               pFileExt = hb_itemPutC( NULL, ".dbt" );
               break;
            case DB_MEMO_FPT:
               pFileExt = hb_itemPutC( NULL, ".fpt" );
               break;
            case DB_MEMO_SMT:
               pFileExt = hb_itemPutC( NULL, ".smt" );
               break;
         }
         if( pFileExt )
         {
            hb_setSetItem( HB_SET_MFILEEXT, pFileExt );
            SELF_RDDINFO( pRDDNode, RDDI_MEMOEXT, 0, pFileExt );
            hb_itemRelease( pFileExt );
         }
      }
      if( pItem )
         hb_itemRelease( pItem );
   }
   if( iMemoVersion > 0 )
   {
      PHB_ITEM pItem = NULL;

      pUStru->uiMemoVersion = ( HB_USHORT ) iMemoVersion;
      if( pRDDNode )
      {
         pItem = hb_itemPutNI( NULL, iMemoVersion );
         SELF_RDDINFO( pRDDNode, RDDI_MEMOVERSION, 0, pItem );
      }
      if( pItem )
         hb_itemRelease( pItem );
   }
   if( iMemoBlocksize > 0 )
   {
      PHB_ITEM pItem = NULL;

      pUStru->uiMemoBlocksize = ( HB_USHORT ) iMemoBlocksize;
      if( pRDDNode )
      {
         pItem = hb_itemPutNI( NULL, iMemoBlocksize );
         SELF_RDDINFO( pRDDNode, RDDI_MEMOBLOCKSIZE, 0, pItem );
      }
      if( pItem )
         hb_itemRelease( pItem );
   }
}

static void leto_CreateTable( PUSERSTRU pUStru, const char * szRawData )
{
   char *       szFileRaw = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFileName = ( char * ) hb_xgrab(  HB_PATH_MAX );
   char *       szFile = ( char * ) hb_xgrab( HB_PATH_MAX );
   char         szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   char         szRealAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   char         szDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];
   unsigned int uiReplyBufLen = 1023;
   char *       szReply = hb_xgrab( uiReplyBufLen + 1 );
   char *       szCdp = NULL;
   char *       ptr, * ptr2, * ptrTmp;
   const char * pData = NULL;
   const char * pp2 = NULL, * pp3 = NULL, * pp4, * pp5;
   int          nParam;
   int          iMemoType, iMemoVersion, iMemoBlocksize;
   AREAP        pArea;
   HB_BOOL      bLeadSep, bMemIO;
   HB_BOOL      bKeepOpen = HB_TRUE;  /* default, Harbour (client) will send afterwards a close */
   HB_BOOL      bUnlocked = HB_FALSE;
   HB_SIZE      nLen;
   PHB_FNAME    pFilePath;
   HB_USHORT    ui, uiFieldCount, uiNtxType;
   HB_ERRCODE   errcode = HB_SUCCESS;
   PHB_ITEM     pDelim = NULL;
   PHB_ITEM     pFields;
   HB_ULONG     ulLenLen = 4;
   HB_ULONG     ulAreaID = 0;
   HB_ULONG     ulSelectID = 0;
   HB_ULONG     ulRecordSize = 0;

   szReply[ 0 ] = '\0';
   pUStru->bLastAct = HB_FALSE;
   nParam = leto_GetParam( szRawData, &pp2, &pp3, NULL, NULL );
   nLen = strlen( szRawData );

   if( leto_IsServerLock( pUStru ) )
      strcpy( szReply, szErrLck );
   else if( nParam < 3 || nLen < 1 || ! pp3 || strlen( pp3 ) < 2 )
      strcpy( szReply, szErr2 );
   else
   {
      memcpy( szFileRaw, szRawData, nLen );
      szFileRaw[ nLen ] = '\0';
      leto_StrTran( szFileRaw, DEF_CH_SEP, DEF_SEP, nLen );
      bLeadSep = ( szFileRaw[ 0 ] == DEF_SEP );
      pFilePath = hb_fsFNameSplit( szFileRaw );
      bMemIO = ! strncmp( szFileRaw + ( bLeadSep ? 1 : 0 ), "mem:", 4 );

      ptr = szFileName;
      ptr2 = szFileRaw;
      if( bLeadSep )
         ptr2++;
      if( ! bMemIO )
      {
         strcpy( ptr, s_pDataPath );
         ptr += strlen( ptr );
         *ptr++ = DEF_SEP;
      }
      strcpy( szFile, ptr2 );

      if( s_bLowerPath )
      {
         hb_strLower( szFileName, strlen( szFileName ) );
         hb_strLower( szFile, strlen( szFile ) );
      }
      strcpy( ptr, szFile );

      if( pp2 && *pp2 )
      {
         strcpy( szAlias, pp2 );
         hb_strUpper( szAlias, strlen( szAlias ) );
      }
      else
      {
         /* create an ALIAS from clean szFileName */
         ptr2 = szFile + strlen( szFile ) - 1;
         while( ( ptr2 - szFile ) > 0 && *ptr2 != DEF_SEP && *ptr2 != ':' )
            ptr2--;
         if( *ptr2 == DEF_SEP || *ptr2 == ':' )
            ptr2++;
         strcpy( szAlias, ptr2 );
         /* cut off extension */
         if( ( ptr2 = strrchr( szAlias, '.' ) ) != NULL )
            *ptr2 = '\0';
         hb_strUpper( szAlias, strlen( szAlias ) );
         if( s_iDebugMode > 20 )
            leto_wUsLog( pUStru, -1, "DEBUG universal ALIAS <%s> created from filename", szAlias );
      }

      if( pp3 && strlen( pp3 ) > 0 )
      {
         strcpy( szDriver, pp3 );
         hb_strUpper( szDriver, strlen( szDriver ) );  /* DRIVER is ever UPPER */
      }
      else
         strcpy( szDriver, leto_Driver( s_uiDriverDef ) );
      uiNtxType = ( strstr( szDriver, "NTX" ) != NULL ) ? 1 : 0;

      if( ! pFilePath->szExtension )
      {
         char * szExt = ( char * ) hb_xgrab( HB_MAX_FILE_EXT + 1 );

         leto_RddiGetValue( szDriver, RDDI_TABLEEXT, szExt );
         if( *szExt )
         {
            if( s_bLowerPath )
               hb_strLower( szExt, strlen( szExt ) );
            strcpy( szFile + strlen( szFile ), szExt );
            strcpy( szFileName + strlen( szFileName ), szExt );
            hb_xfree( pFilePath );
            pFilePath = hb_fsFNameSplit( szFile );
         }
         hb_xfree( szExt );
      }
      if( ! s_bAnyExt && leto_stricmp( pFilePath->szExtension, ".dbf" ) )
         errcode = HB_FAILURE;
      hb_xfree( pFilePath );
      if( ! bMemIO && ! strchr( szFile, DEF_SEP ) )
      {
         char *    szFilePath = ( char * ) hb_xgrab( HB_PATH_MAX );
         HB_ULONG  uiLen;

         hb_strncpy( szFilePath, hb_setGetDefault(), HB_PATH_MAX - 1 );
         uiLen = strlen( szFilePath );
         if( uiLen )
         {
            if( szFilePath[ uiLen - 1 ] != DEF_SEP )
               szFilePath[ uiLen++ ] = DEF_SEP;
            hb_strncpy( szFilePath + uiLen, szFile, HB_PATH_MAX - uiLen - 1 );
            strcpy( szFileName, szFilePath );
            strcpy( szFile, szFileName + s_uiDataPathLen + 1 );
         }
         hb_xfree( szFilePath );
      }

      pp4 = pp3 + strlen( pp3 ) + 1;
      iMemoType = ( int ) strtol( pp4, &ptrTmp, 10 );
      pp4 = ++ptrTmp;
      iMemoVersion = ( int ) strtol( pp4, &ptrTmp, 10 );
      pp4 = ++ptrTmp;
      iMemoBlocksize = ( int ) strtol( pp4, &ptrTmp, 10 );
      pp4 = ++ptrTmp;
      if( iMemoType || iMemoVersion || iMemoBlocksize )
         leto_SetMemoEnv( pUStru, szDriver, iMemoType, iMemoVersion, iMemoBlocksize );

      uiFieldCount = ( HB_USHORT ) strtol( pp4, &ptrTmp, 10 );
      pp4 = ++ptrTmp;
      pp5 = pp4;

      pFields = hb_itemArrayNew( uiFieldCount );

      for( ui = 1; ui <= uiFieldCount; ++ui )
      {
         PHB_ITEM pField = hb_itemArrayNew( 4 );

         while( *pp5 != ';' )
            pp5++;
         hb_arraySetCL( pField, 1, pp4, pp5 - pp4 );
         pp5++;
         pp4 = pp5;

         while( *pp5 != ';' )
            pp5++;
         hb_arraySetCL( pField, 2, pp4, pp5 - pp4 );  /* may include additional field flags */
         pp5++;
         pp4 = pp5;

         hb_arraySetNI( pField, 3, ( int ) strtoul( pp4, &ptrTmp, 10 ) );
         ulRecordSize += hb_arrayGetNI( pField, 3 );
         pp4 = ++ptrTmp;

         hb_arraySetNI( pField, 4, ( int ) strtoul( pp4, &ptrTmp, 10 ) );
         pp4 = ++ptrTmp;
         pp5 = pp4;

         hb_arraySet( pFields, ui, pField );
         hb_itemRelease( pField );
      }

      if( *pp5++ == ';' && *pp5 )  /* note: a double ';' after the fields */
         ulSelectID = strtoul( pp5, &ptrTmp, 10 );
      else
         ptrTmp = NULL;
      if( ptrTmp )
         pp5 = ptrTmp + 1;

      if( pp5 && *pp5++ != ';' )
      {
         if( ( ptrTmp = strchr( pp5, ';' ) ) != NULL )
         {
            *ptrTmp = '\0';
            if( hb_cdpFind( pp5 ) )
               strcpy( szCdp, pp5 );
            else
               szCdp = NULL;
            pp5 = ptrTmp + 1;
         }
      }
      else
         szCdp = NULL;

      if( pp5 && ( ptrTmp = strchr( pp5, ';' ) ) != NULL && ptrTmp - pp5 > 1 )
      {
         if( ! pUStru->szDateFormat || strncmp( pUStru->szDateFormat, pp5, ptrTmp - pp5 ) ||
             strlen( pUStru->szDateFormat ) != ( unsigned int ) ( ptrTmp - pp5 ) )
         {
            PHB_ITEM pItem = hb_itemNew( NULL );

            if( pUStru->szDateFormat )
               hb_xfree( pUStru->szDateFormat );
            pUStru->szDateFormat = ( char * ) hb_xgrab( ptrTmp - pp5 + 1 );
            memcpy( pUStru->szDateFormat, pp5, ptrTmp - pp5 );
            pUStru->szDateFormat[ ptrTmp - pp5 ] = '\0';
            pItem = hb_itemPutC( pItem, pUStru->szDateFormat );
            hb_setSetItem( HB_SET_DATEFORMAT, pItem );
            hb_itemRelease( pItem );
            if( s_iDebugMode > 10 )
               leto_wUsLog( pUStru, -1, "DEBUG leto_CreateTable new dateformat %s set", hb_setGetDateFormat() );
         }
      }

      /* last checks - note: there ever must be an ID for s_bNoSaveWA */
      if( ulRecordSize >= 0xFFFF || uiFieldCount > 2047 )  /* no errcode, let Harbour even try it ;-) */
         leto_wUsLog( pUStru, -1, "ERROR leto_CreateTable DBF %s oversized: record size %lu, fields %lu",
                                  szFile, ulRecordSize, uiFieldCount );
      if( ! uiFieldCount || ! szFileName[ 0 ] || ( ( s_bNoSaveWA && ! bMemIO ) && ! ulSelectID ) )
         errcode = HB_FAILURE;

      HB_GC_LOCKT();

      if( errcode == HB_SUCCESS )
      {
         if( leto_FindTable( szFile, NULL ) >= 0 )
         {
            sprintf( szReply, "%s%s%s", szErr4, ":21-1023-0-0\t", szFile );  /* EDBF_SHARED */
            leto_wUsLog( pUStru, -1, "ERROR leto_CreateTable will not overwrite table: %s in use by other", szFile );
            errcode = HB_FAILURE;
         }
         if( s_uiTablesCurr >= s_uiTablesAlloc )
         {
            sprintf( szReply, "%s%s%s", szErr4, ":21-1004-0-0\t", szFile );  /* EBDF_CREATE_DBF */
            leto_writelog( NULL, -1, "ERROR leto_CreateTable configured maximum tables %d exceeded (active: %d file: %s)",
                           s_uiTablesAlloc, s_uiTablesCurr, szFile );
            errcode = HB_FAILURE;
         }
      }

      if( errcode == HB_SUCCESS )
      {
         HB_USHORT uiArea = 0;

         if( ( s_bNoSaveWA && ! bMemIO ) )  // ToFix UDF mode ?
         {
            strcpy( szRealAlias, szAlias );
            ulAreaID =  ulSelectID;
            uiArea = ( HB_USHORT ) ulSelectID;
         }
         else
         {
            if( ! s_bNoSaveWA )
            {
               ulAreaID = leto_CreateAreaID();
               leto_MakeAlias( ulAreaID, szRealAlias );
            }
            else  /* ( s_bNoSaveWA && bMemIO ) */
            {
               ulAreaID =  ulSelectID;
               leto_MakeAlias( leto_CreateAreaID(), szRealAlias );  // ToDO memio alias
            }
         }

         hb_xvmSeqBegin();
         errcode = hb_rddCreateTable( szFileName, szDriver, uiArea, szRealAlias, bKeepOpen,
                                                  szCdp, 0 /*ulConnection*/, pFields, pDelim );
         hb_xvmSeqEnd();
         if( pUStru->iHbError )
            errcode = HB_FAILURE;

         if( errcode != HB_SUCCESS )
         {
            if( s_bNoSaveWA && bMemIO )
               leto_DelAreaID( ulAreaID );
            if( pUStru->szHbError )
               sprintf( szReply, "%s%s", szErr4, pUStru->szHbError + 4 );
            else
               sprintf( szReply, "%s%s%s", szErr4, ":21-1004-0-0\t", szFile );  /* EBDF_CREATE_DBF */
         }
         else
         {
            int iTableStru = leto_InitTable( ulAreaID, szFile, szDriver, HB_FALSE, szRealAlias, HB_FALSE, szCdp );

            if( iTableStru >= 0 )
            {
               HB_GC_UNLOCKT();
               bUnlocked = HB_TRUE;
               if( ! ulSelectID )
                  ulSelectID = hb_rddGetCurrentWorkAreaNumber();
               leto_InitArea( pUStru, iTableStru, ulAreaID, szAlias, szRealAlias, ulSelectID );
            }
            else
            {
               pData = szErr3;
               errcode = HB_FAILURE;
            }

            if( errcode == HB_SUCCESS )
            {
               PHB_ITEM pItem = hb_itemNew( NULL );

               hb_itemPutNI( pItem, leto_lockScheme( uiNtxType ) );
               hb_setSetItem( HB_SET_DBFLOCKSCHEME, pItem );
               if( pItem )
                  hb_itemRelease( pItem );

               if( ! strcmp( pUStru->szDriver, szDriver ) )
                  strcpy( pUStru->szDriver, szDriver );
            }
         }
      }
      else
         pData = szErr2;

      if( ! bUnlocked )
         HB_GC_UNLOCKT();

      if( errcode != HB_SUCCESS )
      {
         if( ! pData )
            pData = szErr1;
         if( ! *szReply )
            strcpy( szReply, pData );
         ulLenLen = strlen( szReply );
      }
      else
      {
         HB_ULONG ulRecLen;

         pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

         ptr = szReply;
         *ptr++ = '+';
         ptr += ultostr( ulAreaID, ptr );
         *ptr++ = ';';
         *ptr++ = ( char ) ( '0' + uiNtxType );  /* LETO_NTX = 1, LETO_CDX = 0 */
         pUStru->uiDriver = uiNtxType;
         *ptr++ = ';';
         ptr += leto_MemoInfo( pArea, ptr );
         ptr += ultostr( leto_LastUpdate( pArea ), ptr );
         *ptr++ = ';';

         ulLenLen = ptr - szReply;
         ulRecLen = leto_recLen( pUStru->pCurAStru->pTStru );
         if( ulRecLen > 0 )
         {
            char *   szTmp = ( char * ) hb_xgrab( ulRecLen );
            HB_ULONG ulRealLen = leto_rec( pUStru, pUStru->pCurAStru, pArea, szTmp, NULL );

            if( ! ulRealLen )
            {
               strcpy( szReply, szErr4 );
               ulLenLen = 4;
            }
            else
            {
               if( ulRealLen + ulLenLen > uiReplyBufLen )
               {
                  uiReplyBufLen = ulRealLen + ulLenLen;
                  szReply = ( char * ) hb_xrealloc( szReply, uiReplyBufLen + 1 );
                  ptr = szReply + ulLenLen;
               }
               memcpy( ptr, szTmp, ulRealLen );
               ptr += ulRealLen;
               *ptr = '\0';
               ulLenLen += ulRealLen;
            }
            hb_xfree( szTmp );
         }
      }

      if( pFields )
         hb_itemRelease( pFields );
   }

   if( ! pUStru->bBeQuiet )  /* keep open for UDF */
      leto_SendAnswer( pUStru, szReply, ulLenLen );
   hb_xfree( szReply );
   hb_xfree( szFile );
   hb_xfree( szFileName );
   hb_xfree( szFileRaw );
   if( errcode == HB_SUCCESS )
      pUStru->bLastAct = HB_TRUE;
}

/* leto_udf() leto_DbCreate( cFile, aStruct, cRDD, lKeepOpen, cAlias, cCodePage ) */
HB_FUNC( LETO_DBCREATE )
{
   PUSERSTRU pUStru = letoGetUStru();
   HB_USHORT uiArea;
   HB_BOOL   bRet = HB_TRUE;

   if( s_bNoSaveWA )
      uiArea = leto_FindFreeUdfArea();
   else
      uiArea = leto_FindFreeArea( pUStru );

   if( ( pUStru->bRpcMode || s_bNoSaveWA ) && uiArea && HB_ISCHAR( 1 ) && hb_parclen( 1 ) && HB_ISARRAY( 2 )  )
   {
      char         szFile[ HB_PATH_MAX ];
      PHB_ITEM     pStruct = hb_param( 2, HB_IT_ARRAY );
      const char * szDriver = HB_ISCHAR( 3 ) ? hb_parc( 3 ) : pUStru->szDriver;
      HB_BOOL      bKeepOpen = HB_ISLOG( 4 ) ? hb_parl( 4 ) : HB_FALSE;
      const char * szAlias = HB_ISCHAR( 5 ) ? hb_parc( 5 ) : "";
      const char * szCdpage = HB_ISCHAR( 6 ) && hb_parclen( 6 ) ? hb_parc( 6 ) : NULL;
      char *       szData, * szFields;
      HB_SIZE      nFields = hb_arrayLen( pStruct );
      HB_USHORT    uiFields;
      PHB_ITEM     pField;
      char *       ptr, * ptrdouble;
      int          i;

      ptrdouble = szFile;
      strncpy( szFile, HB_ISCHAR( 1 ) ? hb_parc( 1 ) : "", HB_PATH_MAX - 1);
      szFile[ HB_ISCHAR( 1 ) ? HB_MIN( hb_parclen( 1 ), HB_PATH_MAX - 1 ) : 0 ] = '\0';
      if( strlen( ptrdouble ) >= 2 && *( ptrdouble + 1 ) == ':' )  /* C:\... */
         ptrdouble += 2;
      while( *ptrdouble == '/' || *ptrdouble == '\\' )
      {
         ptrdouble++;
      }
      if( ptrdouble > szFile )
         memmove( szFile, ptrdouble, strlen( ptrdouble ) + 1 );
      while( ( ptrdouble = strstr( szFile, "//" ) ) != NULL ||
             ( ptrdouble = strstr( szFile, "\\\\" ) ) != NULL )
      {
         memmove( ptrdouble, ptrdouble + 1, strlen( ptrdouble + 1 ) + 1 );
      }

      if( nFields < 1 || nFields > 0xFFFF )
      {
         hb_retl( HB_FALSE );
         leto_wUsLog( pUStru, -1, "DEBUG leto_DbCreate field count:%ld", nFields );
         return;
      }
      uiFields = ( HB_USHORT ) nFields;
      /* ( 10 + ';' + 7 [ C:5-flags ] + ';' + 5 + ';' + 3 + ';' ) */
      szFields = ( char * ) hb_xgrab( ( nFields * 29 ) + 1 );
      szFields[ 0 ] = '\0';
      ptr = szFields;
      i = 1;
      while( nFields )
      {
         pField = hb_arrayGetItemPtr( pStruct, i++ );
         if( ! ( hb_itemType( pField ) & HB_IT_ARRAY ) ||
             hb_arrayLen( pField ) != 4 ||
             ! ( hb_itemType( hb_arrayGetItemPtr( pField, 1 ) ) & HB_IT_STRING ) ||
             ! ( hb_itemType( hb_arrayGetItemPtr( pField, 2 ) ) & HB_IT_STRING ) ||
             ! ( hb_itemType( hb_arrayGetItemPtr( pField, 3 ) ) & HB_IT_NUMERIC ) ||
             ! ( hb_itemType( hb_arrayGetItemPtr( pField, 4 ) ) & HB_IT_NUMERIC ) ||
             hb_arrayGetNI( pField, 3 ) > 0xFFFF ||
             hb_arrayGetNI( pField, 4 ) > 0xFE )
         {
            bRet = HB_FALSE;
            break;
         }

         if( hb_arrayGetCPtr( pField, 2 )[ 0 ] == 'C' && hb_arrayGetNI( pField, 4 ) )
         {
            HB_ULONG ulLen = hb_arrayGetNI( pField, 3 ) + ( hb_arrayGetNI( pField, 4 ) * 255 );

            if( ulLen < 1 || ulLen > 0xFFFF )
            {
               leto_wUsLog( pUStru, -1, "DEBUG leto_DbCreate() wrong C field len:%lu", ulLen );
               bRet = HB_FALSE;
               break;
            }
            else
               ptr += sprintf( ptr, "%s;%s;%lu;%d;",
                               hb_arrayGetCPtr( pField, 1 ), hb_arrayGetCPtr( pField, 2 ), ulLen, 0 );
         }
         else
            ptr += sprintf( ptr, "%s;%s;%d;%d;", hb_arrayGetCPtr( pField, 1 ), hb_arrayGetCPtr( pField, 2 ),
                            hb_arrayGetNI( pField, 3 ), hb_arrayGetNI( pField, 4 ) );
         nFields--;
      }
      *ptr = '\0';
      nFields = ptr - szFields;

      if( ! bRet )
      {
         leto_wUsLog( pUStru, -1, "DEBUG leto_DbCreate() bad fields after: %s", szFields );
         hb_xfree( szFields );
         hb_retl( bRet );
         return;
      }

      if( ! szCdpage || ! hb_cdpFind( szCdpage ) )
         szCdpage = NULL;  /* pUStru->cdpage->id; */

#if 0  /* should be validated by the caller */
      if( strlen( szFile ) > HB_PATH_MAX - 1 )
      {
         ptr = ( char * ) szFile;
         ptr[ HB_PATH_MAX - 1 ] = '\0';
      }
      if( strlen( szAlias ) > HB_RDD_MAX_ALIAS_LEN )
      {
         ptr = ( char * ) szAlias;
         ptr[ HB_RDD_MAX_ALIAS_LEN ] = '\0';
      }
      if( strlen( szDriver ) > HB_RDD_MAX_DRIVERNAME_LEN )
      {
         ptr = ( char * ) szDriver;
         ptr[ HB_RDD_MAX_DRIVERNAME_LEN ] = '\0';
      }
#endif
      szData = ( char * ) hb_xgrab( HB_PATH_MAX + HB_RDD_MAX_ALIAS_LEN +
                                    HB_RDD_MAX_DRIVERNAME_LEN + 42 + nFields );
      /* note: a double ';' after last field[dec] */
      sprintf( szData, "%s;%s;%s;%d;%d;%d;%d;%s;%d;%s;", szFile, szAlias, szDriver,  /* LETOCMD_creat */
               pUStru->uiMemoType, pUStru->uiMemoVersion, pUStru->uiMemoBlocksize,
               uiFields, szFields, uiArea, szCdpage ? szCdpage : "" );

      leto_CreateTable( pUStru, ( const char * ) szData );
      bRet = pUStru->bLastAct;

      if( bRet && ! bKeepOpen )
         leto_CloseArea( pUStru, pUStru->pCurAStru );
      hb_xfree( szFields );
      hb_xfree( szData );
   }
   else
      bRet = HB_FALSE;

   hb_retl( bRet );
}

static void leto_OpenIndex( PUSERSTRU pUStru, const char * szRawData )
{
   char *       szFileRaw = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFileName = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFile = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szBagName = ( char * ) hb_xgrab( HB_PATH_MAX );
   unsigned int uiReplyBufLen = 1023;
   char *       szReply = hb_xgrab( uiReplyBufLen + 1 );
   char *       ptr, * ptr2;
   const char * pp1 = NULL;
   AREAP        pArea;
   HB_ERRCODE   errcode = HB_SUCCESS;
   HB_BOOL      bLeadSep;
   HB_BOOL      bMemIO;
   HB_ULONG     ulAreaID;
   HB_ULONG     ulLenLen;

   pUStru->bLastAct = HB_FALSE;
   ulAreaID = strtoul( szRawData, &ptr, 10 );
   if( leto_IsServerLock( pUStru ) )
      errcode = HB_FAILURE;
   else if( ! ulAreaID || ! ptr || *ptr++ != ';' )
      errcode = HB_FAILURE;
   else
   {
      HB_ULONG ulRawLen = pUStru->ulDataLen - 2 - ( ptr - szRawData );
      szRawData = ( const char * ) ptr;
      pp1 = szRawData;
      while( ( HB_ULONG ) ( pp1 - szRawData ) < ulRawLen && *pp1 != ';' )
         pp1++;
      if( *pp1 != ';' || ( pp1 - szRawData ) < 1 )
         errcode = HB_FAILURE;
   }

   if( errcode == HB_FAILURE )
   {
      if( leto_IsServerLock( pUStru ) )
         strcpy( szReply, szErrLck );
      else
         strcpy( szReply, szErr2 );
      ulLenLen = 4;
   }
   else
   {
      memcpy( szFileRaw, szRawData, ( pp1 - szRawData ) );
      szFileRaw[ pp1 - szRawData ] = '\0';
      leto_StrTran( szFileRaw, DEF_CH_SEP, DEF_SEP, strlen( szFileRaw ) );
      bLeadSep = ( szFileRaw[ 0 ] == DEF_SEP );
      bMemIO = ! strncmp( szFileRaw + ( bLeadSep ? 1 : 0 ), "mem:", 4 );

      ptr = szFileName;
      ptr2 = szFileRaw;
      if( bLeadSep )
         ptr2++;
      if( ! bMemIO )
      {
         strcpy( ptr, s_pDataPath );
         ptr += s_uiDataPathLen;
         *ptr++ = DEF_SEP;
      }
      strcpy( szFile, ptr2 );

      /* now add possible path to the filename */
      if( bMemIO || bLeadSep || strchr( szFile, DEF_SEP ) != NULL )
         strcpy( ptr, szFile );
      else
      {
         /* blank filename given: append to relative path_to_table */
         PHB_FNAME pDbfPath;

         /* UDF may fail here */
         if( pUStru->pCurAStru && pUStru->pCurAStru->pTStru )
         {
            pDbfPath = hb_fsFNameSplit( ( char * ) pUStru->pCurAStru->pTStru->szTable );

            if( pDbfPath && pDbfPath->szPath )
            {
               strcpy( ptr, pDbfPath->szPath );
               ptr += strlen( ptr );
            }
            if( pDbfPath )
               hb_xfree( pDbfPath );
            strcpy( ptr, szFile );
         }
      }

      if( s_bLowerPath )
      {
         hb_strLower( szFileName, strlen( szFileName ) );
         hb_strLower( szFile, strlen( szFile ) );
      }

      /* clean BAGname: after possible DEF_SEP */
      ptr2 = szFile + strlen( szFile ) - 1;
      while( ( ptr2 - szFile ) > 0 && *ptr2 != DEF_SEP )
         ptr2--;
      if( *ptr2 == DEF_SEP )
         ptr2++;
      strcpy( szBagName, ptr2 );
      /* cut off any extension after a dot*/
      if( ( ptr2 = strchr( szBagName, '.' ) ) != NULL )
         *ptr2 = '\0';

      /* add extension to filename if not given, for management */
      if( strrchr( szFile, '.' ) == NULL )
      {
         char * szExt = ( char * ) hb_xgrab( HB_MAX_FILE_EXT + 1 );

         leto_RddiGetValue( pUStru->pCurAStru->pTStru->szDriver, RDDI_ORDBAGEXT, szExt );
         strcpy( szFile + strlen( szFile ), szExt );
         strcpy( szFileName + strlen( szFileName ), szExt );
         hb_xfree( szExt );
      }

      if( ! bMemIO && ! strchr( szFile, DEF_SEP ) )
      {
         char * szFilePath = ( char * ) hb_xgrab( HB_PATH_MAX );

         if( hb_fileExists( szFile, szFilePath ) )
         {
            strcpy( szFileName, szFilePath );
            strcpy( szFile, szFileName + s_uiDataPathLen + 1 );
         }
         hb_xfree( szFilePath );
      }

      pArea = leto_SelectArea( pUStru, ulAreaID );
      if( ! pArea )
         errcode = HB_FAILURE;  /* ups, call Houston ... */

      // HB_GC_LOCKT();

      if( errcode == HB_SUCCESS )
      {
         HB_BOOL   bRegistered = HB_FALSE;
         HB_USHORT uiOrdToSet = 0;

         hb_rddSetNetErr( HB_FALSE );

         if( ! ( s_bNoSaveWA && ! pUStru->pCurAStru->pTStru->bMemIO ) )
         {
            PTABLESTRU pTStru = pUStru->pCurAStru->pTStru;
            PINDEXSTRU pIStru;

            HB_GC_LOCKT();

            /* check if already known to table */
            while( uiOrdToSet < pTStru->uiIndexCount &&
                   ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStru->IndexList, uiOrdToSet ) ) != NULL )
            {
               uiOrdToSet++;
               if( ! strcmp( pIStru->szBagName, szFile ) )
               {
                  bRegistered = HB_TRUE;
                  break;
               }
            }

            HB_GC_UNLOCKT();
         }

         if( ! bRegistered )
         {
            DBORDERINFO pOrderInfo;

            memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
            pOrderInfo.atomBagName = hb_itemPutC( NULL, szFileName );
            /* pOrderInfo.itmResult = hb_itemNew( NULL ); */
            hb_xvmSeqBegin();
            errcode = SELF_ORDLSTADD( pArea, &pOrderInfo );
            hb_xvmSeqEnd();
            if( pOrderInfo.itmResult )
               hb_itemRelease( pOrderInfo.itmResult );
            hb_itemRelease( pOrderInfo.atomBagName );

            /* ToDo here a hb_rddGetNetErr() is maybe also available */
            if( pUStru->iHbError )
               errcode = HB_FAILURE;
         }
         else  /* try to emulate Harbour behaviour -- what a fun ;-) */
         {
            HB_USHORT uiDriver = ( strstr( pUStru->pCurAStru->pTStru->szDriver, "NTX" ) != NULL ) ? 1 : 0;
            LETOTAG * pTag = pUStru->pCurAStru->pTagCurrent;

            if( ! uiDriver )  /* CDX: set to order 1 if this is first opened, else stay */
            {
               if( ! pUStru->pCurAStru->pTag )
                  uiOrdToSet = 1;
               else
                  uiOrdToSet = 0;
            }
            else  /* NTX: if no active order, set to new opened, else stay  */
            {
               if( pTag )
                  uiOrdToSet = 0;
            }
            if( uiOrdToSet )
            {
               DBORDERINFO pInfo;

               memset( &pInfo, 0, sizeof( DBORDERINFO ) );
               pInfo.itmOrder = hb_itemPutNI( NULL, uiOrdToSet );
               pInfo.itmResult = hb_itemPutC( NULL, NULL );
               SELF_ORDLSTFOCUS( pArea, &pInfo );
               hb_itemRelease( pInfo.itmOrder );
               hb_itemRelease( pInfo.itmResult );
            }
            else  /* set focus to the last active order */
               leto_SetFocus( pArea, pTag ? pTag->szTagName : "" );
         }
      }

      // HB_GC_UNLOCKT();

      if( errcode != HB_SUCCESS )
      {
         /* ToDo here a hb_rddGetNetErr() is maybe available, see above */
         if( pUStru->iHbError )
            ulLenLen = sprintf( szReply, "%s%s", szErr4, pUStru->szHbError + 4 );
         else
            ulLenLen = sprintf( szReply, "%s%s%s", szErr4, ":21-1003-0-0\t", szFile );  /* EBDF_OPEN_INDEX */
      }
      else
      {
         char *    szTmp;
         HB_ULONG  ulRecLen, ulLen;

         szTmp = leto_IndexesInfo( pUStru, szFile, pArea );
         ulLen = strlen( szTmp );
         ulRecLen = leto_recLen( pUStru->pCurAStru->pTStru );

         if( ulRecLen + ulLen > uiReplyBufLen )
         {
            uiReplyBufLen = ulRecLen + ulLen;
            szReply = ( char * ) hb_xrealloc( szReply, uiReplyBufLen + 1 );
         }

         ptr = szReply;
         *ptr++ = '+';
         strcpy( ptr, szTmp );
         ptr += ulLen;
         ulLenLen = ptr - szReply;
         hb_xfree( szTmp );
         if( ulRecLen > 0 )
         {
            char *   szTemp = ( char * ) hb_xgrab( ulRecLen );
            HB_ULONG ulRealLen = leto_rec( pUStru, pUStru->pCurAStru, pArea, szTemp, NULL );

            if( ulRealLen )
            {
               memcpy( ptr, szTemp, ulRealLen );
               ptr += ulRealLen;
               *ptr = '\0';
               ulLenLen += ulRealLen;
            }
            else  /* should never happen */
            {
               strcpy( szReply, szErr3 );
               ulLenLen = 4;
            }
            hb_xfree( szTemp );
         }
      }
   }

   if( ! pUStru->bBeQuiet )
      leto_SendAnswer( pUStru, szReply, ulLenLen );
   hb_xfree( szReply );
   hb_xfree( szBagName );
   hb_xfree( szFile );
   hb_xfree( szFileName );
   hb_xfree( szFileRaw );
   if( errcode == HB_SUCCESS )
      pUStru->bLastAct = HB_TRUE;
}

/* leto_udf() leto_OrdListAdd( cBagName, ncWorkarea ) */
HB_FUNC( LETO_ORDLISTADD )
{
   PUSERSTRU    pUStru = letoGetUStru();
   PAREASTRU    pAStru = NULL;
   const char * szBagName = HB_ISCHAR( 1 ) && hb_parclen( 1 ) ? hb_parc( 1 ) : "";
   HB_ULONG     ulAreaID = 0;
   const char * szAlias = NULL;
   char         szData[ 32 ];
   HB_BOOL      bRet = HB_FALSE;

   if( HB_ISNUM( 2 ) && hb_parni( 2 ) > 0 )
      ulAreaID = ( HB_ULONG ) hb_parni( 2 );
   else if( HB_ISCHAR( 2 ) && hb_parclen( 2 ) )
      szAlias = hb_parc( 2 );

   if( *szBagName )
      pAStru = leto_Select( pUStru, ulAreaID, szAlias, HB_TRUE );

   if( ( pUStru->bRpcMode || s_bNoSaveWA ) && pAStru && pAStru->ulUdf == ( HB_ULONG ) pUStru->iUserStru )
   {
      sprintf( szData, "%lu;%s;", pAStru->ulAreaID, szBagName );  /* LETOCMD_open_i */
      pUStru->ulDataLen = strlen( szData ) + 2; /* note: minus -2 in below func */
      leto_OpenIndex( pUStru, szData );
      bRet = pUStru->bLastAct;
      if( ! bRet )
         leto_wUsLog( pUStru, -1, "DEBUG leto_OrdListAdd( %lu %s )", pAStru->ulAreaID, szBagName );
   }
   else if( pAStru && s_iDebugMode > 0 )
      leto_wUsLog( pUStru, -1, "DEBUG leto_OrdListAdd( %lu ) : not an UDF area !", pAStru->ulSelectID );
   else if( s_iDebugMode > 20 )
      leto_wUsLog( pUStru, -1, "DEBUG leto_OrdListAdd( %lu - %s ) : WA not found !", ulAreaID, szAlias );

   hb_retl( bRet );
}

static void leto_CreateIndex( PUSERSTRU pUStru, const char * szRawData )
{
   char *       szFileRaw = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFileName = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFile = ( char * ) hb_xgrab( HB_PATH_MAX );
   char         szTagName[ LETO_MAX_TAGNAME + 1 ];
   unsigned int uiReplyBufLen = 1023;
   char *       szReply = hb_xgrab( uiReplyBufLen + 1 );
   char *       ptr, * ptr2, * ptr3;
   const char * pData = NULL;
   const char * pp2 = NULL, * pp3 = NULL, * pp4;
   const char * szFor, * szWhile, * szUseOrder;
   AREAP        pArea;
   PHB_FNAME    pFilePath;
   HB_ERRCODE   errcode = HB_SUCCESS;
   HB_BOOL      bLeadSep, bMemIO, bUnique, bAll, bRest, bDescend, bCustom, bAdditive, bTemporary, bExclusive, bFilter;
   HB_BOOL      bUseCur = HB_FALSE;
   HB_SIZE      nLen = 0;
   HB_ULONG     ulRecNo, ulNext, ulRecord;
   HB_ULONG     ulAreaID;
   HB_ULONG     ulLenLen = 4;
   HB_ULONG     ulRawLen = pUStru->ulDataLen - 2;

   szReply[ 0 ] = '\0';
   pUStru->bLastAct = HB_FALSE;
   ulAreaID = strtoul( szRawData, &ptr, 10 );
   if( leto_IsServerLock( pUStru ) )
      pData = szErrLck;
   else if( ! ulAreaID || ! ptr || *ptr++ != ';' )
      pData = szErr2;
   else
   {
      int nParam;

      ulRawLen -= ptr - szRawData;
      szRawData = ( const char * ) ptr;
      /* now szRawData = BagName; pp2 = TagName, pp3 = key expression for index */
      nParam = leto_GetParam( szRawData, &pp2, &pp3, NULL, NULL );
      if( nParam < 3 || ( ( nLen = strlen( szRawData ) ) < 1 && strlen( pp2 ) < 1 ) || strlen( pp3 ) < 1 )
         pData = szErr2;
   }

   if( pData )  /* very early error if ! NULL */
   {
      pArea = NULL;
      errcode = HB_FAILURE;
      strcpy( szReply, pData );
   }
   else
   {
      pArea = leto_SelectArea( pUStru, ulAreaID );
      if( pArea == NULL )
      {
         strcpy( szReply, szErr3 );
         errcode = HB_FAILURE;
      }
   }

   if( errcode == HB_SUCCESS )
   {
      char * szDefaultExt = ( char * ) hb_xgrab( HB_MAX_FILE_EXT + 1 );

      if( nLen )
         memcpy( szFileRaw, szRawData, nLen );
      else  /* derive production index filename from tablename */
      {
         strcpy( szFileRaw, ( char * ) pUStru->pCurAStru->pTStru->szTable );
         ptr = strrchr( szFileRaw, '.' );
         if( ptr )
            nLen = ptr - szFileRaw;
         else
            nLen = strlen( szFileRaw );
      }
      szFileRaw[ nLen ] = '\0';

      leto_StrTran( szFileRaw, DEF_CH_SEP, DEF_SEP, nLen );
      bLeadSep = ( szFileRaw[ 0 ] == DEF_SEP );
      pFilePath = hb_fsFNameSplit( szFileRaw );
      bMemIO = ! strncmp( szFileRaw + ( bLeadSep ? 1 : 0 ), "mem:", 4 );

      ptr = szFileName;
      ptr2 = szFileRaw;
      if( bLeadSep )
         ptr2++;
      if( ! bMemIO )
      {
         strcpy( ptr, s_pDataPath );
         ptr += s_uiDataPathLen;
         *ptr++ = DEF_SEP;
      }
      strcpy( szFile, ptr2 );

      /* now add to path the filename */
      if( bMemIO || bLeadSep || strchr( szFile, DEF_SEP ) != NULL )
         strcpy( ptr, szFile );
      else  /* raw filename given: append to relative path of table */
      {
         PHB_FNAME pDbfPath = hb_fsFNameSplit( ( char * ) pUStru->pCurAStru->pTStru->szTable );

         if( pDbfPath->szPath )
         {
            strcpy( ptr, pDbfPath->szPath );
            ptr += strlen( ptr );
         }
         hb_xfree( pDbfPath );
         strcpy( ptr, szFile );
      }

      leto_RddiGetValue( pUStru->pCurAStru->pTStru->szDriver, RDDI_ORDBAGEXT, szDefaultExt );
      if( ! pFilePath->szExtension )
      {
         if( s_bLowerPath )
            hb_strLower( szDefaultExt, strlen( szDefaultExt ) );
         pFilePath->szExtension = ( const char * ) szDefaultExt;
         strcpy( szFile + strlen( szFile ), szDefaultExt );
         strcpy( szFileName + strlen( szFileName ), szDefaultExt );
      }

      /* non default file extensions allowed ? */
      if( ! s_bAnyExt && leto_stricmp( pFilePath->szExtension, szDefaultExt ) )
         errcode = HB_FAILURE;
      hb_xfree( szDefaultExt );

      if( s_bLowerPath )
      {
         hb_strLower( szFileName, strlen( szFileName ) );
         hb_strLower( szFile, strlen( szFile ) );
      }
      hb_xfree( pFilePath );

      if( pp2 && *pp2 )
      {
         nLen = strlen( pp2 );
         if( nLen > LETO_MAX_TAGNAME )
            nLen = LETO_MAX_TAGNAME;
         memcpy( szTagName, pp2, nLen );
         szTagName[ nLen ] = '\0';
      }
      else  /* create a clean TagName, without bMemIO prefix */
      {
         ptr2 = szFile + strlen( szFile ) - 1;
         while( ( ptr2 - szFile ) > 0 && *ptr2 != DEF_SEP )
            ptr2--;
         if( *ptr2 == DEF_SEP )
            ptr2++;
         if( bMemIO )
            ptr2 += 4;
         memset( szTagName, '\0', LETO_MAX_TAGNAME + 1 );
         strncpy( szTagName, ptr2, LETO_MAX_TAGNAME );
         /* cut off any file extension */
         if( ( ptr2 = strchr( szTagName, '.' ) ) != NULL )
            *ptr2 = '\0';
         if( s_iDebugMode > 20 )
            leto_wUsLog( pUStru, -1, "DEBUG leto_CreateIndex TagName: %s created from BagName", szTagName );
      }
      hb_strUpper( szTagName, strlen( szTagName ) );

      if( ! bMemIO && ! strchr( szFile, DEF_SEP ) )
      {
         char *    szFilePath = ( char * ) hb_xgrab( HB_PATH_MAX );
         HB_ULONG  uiLen;

         hb_strncpy( szFilePath, hb_setGetDefault(), HB_PATH_MAX - 1 );
         uiLen = strlen( szFilePath );
         if( uiLen )
         {
            if( szFilePath[ uiLen - 1 ] != DEF_SEP )
               szFilePath[ uiLen++ ] = DEF_SEP;
            hb_strncpy( szFilePath + uiLen, szFile, HB_PATH_MAX - uiLen - 1 );
            strcpy( szFileName, szFilePath );
            strcpy( szFile, szFileName + s_uiDataPathLen + 1 );
         }
         hb_xfree( szFilePath );
      }

      pp4 = pp3 + strlen( pp3 ) + 1;
      bUnique = ( *pp4 == 'T' ? HB_TRUE : HB_FALSE );
      pp4 += 2;
      szFor = pp4;
      ptr3 = strchr( pp4, ';' );
      *ptr3++ = '\0';
      pp4 = ptr3;
      szWhile = pp4;
      ptr3 = strchr( pp4, ';' );
      *ptr3++ = '\0';
      pp4 = ptr3;
      bAll = ( *pp4 == 'T' ? HB_TRUE : HB_FALSE );
      pp4 += 2;
      ulRecNo = strtoul( pp4, &ptr3, 10 );
      pp4 = ++ptr3;
      ulNext = strtoul( pp4, &ptr3, 10 );
      pp4 = ++ptr3;
      /* ever 0 -- not transmitted */
      ulRecord = strtoul( pp4, &ptr3, 10 );
      pp4 = ++ptr3;
      bRest = ( *pp4 == 'T' ? HB_TRUE : HB_FALSE );
      pp4 += 2;
      bDescend = ( *pp4 == 'T' ? HB_TRUE : HB_FALSE );
      pp4 += 2;
      bCustom = ( *pp4 == 'T' ? HB_TRUE : HB_FALSE );
      pp4 += 2;
      bAdditive = ( *pp4 == 'T' ? HB_TRUE : HB_FALSE );
      pp4 += 2;
      if( s_bNoSaveWA && ! pUStru->pCurAStru->pTStru->bMemIO )
         bTemporary = ( *pp4 == 'T' ? HB_TRUE : HB_FALSE );
      else
         bTemporary = HB_FALSE;  /*ToDo better throw an error */
      pp4++;
      bExclusive = ( *pp4++ == 'T' ? HB_TRUE : HB_FALSE );
      bFilter = ( *pp4 == 'T' ? HB_TRUE : HB_FALSE );
      pp4 += 2;
      ptr3 += 12;
      if( ( HB_ULONG ) ( pp4 - szRawData + 1 ) < ulRawLen )
      {
         char * ptrTmp = ptr3;

         szUseOrder = pp4;
         if( ( ptr3 = strchr( pp4, ';' ) ) == NULL )
            ptr3 = ptrTmp;
         *ptr3 = '\0';
         if( *szUseOrder )
         {
            hb_strUpper( ptrTmp, strlen( szUseOrder ) );
            leto_SetFocusIf( pUStru->pCurAStru, pArea, szUseOrder );
            if( pUStru->pCurAStru->pTagCurrent )
               bUseCur = HB_TRUE;
            else
               errcode = HB_FAILURE;
         }
      }

      if( ! szFileName[ 0 ] && ! szTagName[ 0 ] )
         errcode = HB_FAILURE;

      if( errcode != HB_SUCCESS )
         pData = szErr2;
      else
      {
         LPDBORDERCONDINFO lpdbOrdCondInfo = NULL;

         hb_xvmSeqBegin();
         hb_rddSetNetErr( HB_FALSE );

         if( *szFor || *szWhile || ulRecNo || ulNext || ulRecord ||
             bDescend || bAll || bRest || bAdditive || bCustom || bTemporary || bExclusive || bFilter || bUseCur )
         {
            //LPDBORDERCONDINFO lpdbOrdCondInfo;
            PHB_ITEM pItem = hb_itemNew( NULL );

            lpdbOrdCondInfo = ( LPDBORDERCONDINFO ) hb_xgrab( sizeof( DBORDERCONDINFO ) );
            if( *szFor )
            {
               lpdbOrdCondInfo->abFor = hb_strdup( szFor );
               lpdbOrdCondInfo->itmCobFor = leto_mkCodeBlock( szFor, strlen( szFor ), HB_FALSE );
            }
            else
            {
               lpdbOrdCondInfo->abFor = NULL;
               lpdbOrdCondInfo->itmCobFor = NULL;
            }
            lpdbOrdCondInfo->fAll = bAll;
            if( *szWhile )
            {
               lpdbOrdCondInfo->abWhile = *szWhile ? hb_strdup( szWhile ) : NULL;
               lpdbOrdCondInfo->itmCobWhile = leto_mkCodeBlock( szWhile, strlen( szWhile ), HB_FALSE );
            }
            else
            {
               lpdbOrdCondInfo->abWhile = NULL;
               lpdbOrdCondInfo->itmCobWhile = NULL;
            }
            /* do not use ->itmCobEval, lets create it by Harbour from string */
            lpdbOrdCondInfo->itmCobEval = NULL;
            lpdbOrdCondInfo->lStep = 0;
            if( ulRecNo )
            {
               hb_itemPutNL( pItem, ulRecNo );
               lpdbOrdCondInfo->itmStartRecID = hb_itemNew( pItem );
            }
            else
               lpdbOrdCondInfo->itmStartRecID = NULL;
            lpdbOrdCondInfo->lNextCount = ulNext;
            if( ulRecord )
            {
               hb_itemPutNL( pItem, ulRecord );
               lpdbOrdCondInfo->itmRecID = hb_itemNew( pItem );
            }
            else
               lpdbOrdCondInfo->itmRecID = NULL;
            lpdbOrdCondInfo->fRest = bRest;
            lpdbOrdCondInfo->fDescending = bDescend;
            /* 12th parameter is always nil in CL5.3, in CL5.2 it's compound flag */
            lpdbOrdCondInfo->fCompound = HB_FALSE;
            lpdbOrdCondInfo->fAdditive = bAdditive;
            lpdbOrdCondInfo->fUseCurrent = bUseCur;
            lpdbOrdCondInfo->fCustom = bCustom;
            lpdbOrdCondInfo->fNoOptimize = HB_FALSE;  // determine if RDD supports it
            lpdbOrdCondInfo->fTemporary = bTemporary;
            lpdbOrdCondInfo->fExclusive = bExclusive;
            lpdbOrdCondInfo->fUseFilter = bFilter;

            if( lpdbOrdCondInfo->itmCobWhile )
               lpdbOrdCondInfo->fRest = HB_TRUE;
            if( lpdbOrdCondInfo->lNextCount || lpdbOrdCondInfo->itmRecID || lpdbOrdCondInfo->fRest ||
                lpdbOrdCondInfo->fUseCurrent || lpdbOrdCondInfo->fUseFilter )
               lpdbOrdCondInfo->fAll = HB_FALSE;
            lpdbOrdCondInfo->fActive = ! lpdbOrdCondInfo->fAll ||
                                       lpdbOrdCondInfo->abFor || lpdbOrdCondInfo->itmCobFor ||
                                       lpdbOrdCondInfo->abWhile || lpdbOrdCondInfo->itmCobWhile ||
                                       lpdbOrdCondInfo->fNoOptimize || lpdbOrdCondInfo->itmCobEval ||
                                       lpdbOrdCondInfo->fTemporary;
            lpdbOrdCondInfo->fScoped = ! lpdbOrdCondInfo->fAll;
            lpdbOrdCondInfo->lpvCargo = NULL;

            errcode = SELF_ORDSETCOND( pArea, lpdbOrdCondInfo );
            if( pItem )
               hb_itemRelease( pItem );
         }

         hb_xvmSeqEnd();
         if( pUStru->iHbError )  /* fail to set SELF_ORDSETCOND */
         {
            errcode = HB_FAILURE;
            if( pUStru->szHbError )
               sprintf( szReply, "%s%s", "-011", pUStru->szHbError + 4 );
            else
               sprintf( szReply, "%s%s%s", szErr4, ":21-1006-0-0\t", szFile );  /* EBDF_CREATE_INDEX */
            if( lpdbOrdCondInfo )
               SELF_ORDSETCOND( pArea, NULL );
         }
         else
         {
            DBORDERCREATEINFO dbCreateInfo;
            PTABLESTRU        pTStru = pUStru->pCurAStru->pTStru;
            HB_UINT           uiIndexInUse = 0;
            LETOTAG *         pTag;
            PINDEXSTRU        pIStru;

            HB_GC_LOCKT();

            /* check if index Bag in use by other -- different check for s_bNoSaveWA */
            if( bTemporary || bExclusive )
            {
               if( s_iDebugMode > 10 )
                  leto_wUsLog( pUStru, -1, "DEBUG leto_CreateIndex %s-index: %s", bExclusive ? "exclusive" : "temporary" , szFileName );
               /* ToDo  filename wrong for temporary */
            }
            else if( ! ( s_bNoSaveWA && ! pTStru->bMemIO ) )
            {
               HB_USHORT uo = 0;

               while( uo < pTStru->uiIndexCount && ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStru->IndexList, uo ) ) != NULL )
               {
                  if( ! strcmp( pIStru->szBagName, szFile ) )
                  {
                     if( pIStru->uiAreas > 1 )
                        uiIndexInUse = uo + 1;
                     else if( pIStru->bShared )  /* if no temporary order, check if used only by me own */
                     {
                        HB_BOOL bMyself = HB_FALSE;

                        pTag = pUStru->pCurAStru->pTag;
                        while( pTag )
                        {
                           if( ! strcmp( pTag->pIStru->szBagName, szFile ) && pTag->pIStru == pIStru )
                           {
                              bMyself = HB_TRUE;
                              break;
                           }
                           pTag = pTag->pNext;
                        }
                        if( ! bMyself )
                           uiIndexInUse = uo + 1;
                     }
                     if( uiIndexInUse )
                        break;
                  }
                  uo++;
               }
            }
            else
               uiIndexInUse = leto_FindTableLockOrder( pTStru, 0, szFile );

            if( uiIndexInUse )
            {
               if( s_iDebugMode > 0 )
                  leto_wUsLog( pUStru, -1, "DEBUG leto_CreateIndex Tag: %s %s", szFile, "in use by other" );
               errcode = HB_FAILURE;
               sprintf( szReply, "%s%s%s%s", szErr4, ":21-1006-0-0\t", szFile, " in use by other" );  /* EBDF_CREATE_INDEX */
               if( lpdbOrdCondInfo )
                  SELF_ORDSETCOND( pArea, NULL );
            }

            if( errcode == HB_SUCCESS )
            {
               memset( &dbCreateInfo, 0, sizeof( DBORDERCREATEINFO ) );
               dbCreateInfo.lpdbOrdCondInfo = pArea->lpdbOrdCondInfo;
               dbCreateInfo.abBagName = szFileName;
               dbCreateInfo.atomBagName = szTagName;
               dbCreateInfo.itmOrder = NULL;
               dbCreateInfo.fUnique = bUnique;
               dbCreateInfo.abExpr = hb_itemPutC( NULL, pp3 );  /* szKey */;
               /* dbOrderInfo.itmCobExpr = leto_mkCodeBlock( pp3, strlen( pp3 ), HB_FALSE ); */
               dbCreateInfo.lpdbConstraintInfo = NULL;

               hb_rddSetNetErr( HB_FALSE );

               /* after long miles of preparation, here it comes: CREATE the index ;-) */
               hb_xvmSeqBegin();
               errcode = SELF_ORDCREATE( pArea, &dbCreateInfo );
               hb_xvmSeqEnd();

               if( dbCreateInfo.abExpr )
                  hb_itemRelease( dbCreateInfo.abExpr );
               if( pUStru->iHbError )
               {
                  errcode = HB_FAILURE;
                  sprintf( szReply, "%s%s", "-011", pUStru->szHbError + 4 );
               }
               else if( errcode == HB_SUCCESS )
               {
                  HB_USHORT    uo;
#ifdef LETO_HBNONCONFORM
                  DBORDERINFO  pOrderInfo;
                  char *       szMultiTags = NULL;
                  HB_ULONG     ulMultiPos = 0, ulMultiLen = 0;
                  char *       pMultiTag;
#else
                  LETOTAG *    pTagFree;
#endif

#ifdef LETO_HBNONCONFORM  /* add the new order to list of already opened */

                  memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
                  hb_xvmSeqBegin();

                  SELF_ORDLSTCLEAR( pArea );

                  pTag = pUStru->pCurAStru->pTag;
                  while( pTag )
                  {
                     pIStru = pTag->pIStru;
                     if( ! ( s_bNoSaveWA && ! pTStru->bMemIO ) )
                        pIStru->bClear = HB_TRUE;

                     /* open multiBags only once */  // ToDo - non prod
                     if( ! szMultiTags || ( ! ( pMultiTag = strstr( szMultiTags, pIStru->szBagName ) ) ||
                         ( *( pMultiTag - 1 ) == ';' && *( pMultiTag + strlen( szFile ) ) == ';' ) ) )
                     {
                        if( ulMultiLen - ulMultiPos < HB_PATH_MAX + 1 )
                        {
                           ulMultiLen = ulMultiPos + ( HB_PATH_MAX * 2 ) + 2;
                           if( ! ulMultiPos )
                           {
                              szMultiTags = ( char * ) hb_xgrabz( ulMultiLen );
                              szMultiTags[ 0 ] = ';';
                              ulMultiPos = 1;
                           }
                           else
                              szMultiTags = ( char * ) hb_xrealloc( szMultiTags, ulMultiLen );
                        }
                        pOrderInfo.atomBagName = hb_itemPutC( pOrderInfo.atomBagName, pIStru->szFullPath );
                        pOrderInfo.itmResult = hb_itemNew( NULL );
                        SELF_ORDLSTADD( pArea, &pOrderInfo );
                        ulMultiPos += sprintf( szMultiTags + ulMultiPos, "%s;", pIStru->szBagName );
                        hb_itemRelease( pOrderInfo.itmResult );
                        pOrderInfo.itmResult = NULL;
                     }

                     pTag = pTag->pNext;
                  }

                  if( ! ( s_bNoSaveWA && ! pTStru->bMemIO ) )  /* invert bClear flag */
                  {
                     uo = 0;
                     while( uo < pTStru->uiIndexCount && ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStru->IndexList, uo ) ) != NULL )
                     {
                        if( pIStru->bClear )
                           pIStru->bClear = HB_FALSE;
                        else if( strcmp( pIStru->szBagName, szFile ) )
                           pIStru->bClear = HB_TRUE;
                        uo++;
                     }
                  }

                  if( ! szMultiTags || ( ! ( pMultiTag = strstr( szMultiTags, szFile ) ) ||
                      ( *( pMultiTag - 1 ) == ';' && *( pMultiTag + strlen( szFile ) ) == ';' ) ) )
                  {
                     pOrderInfo.atomBagName = hb_itemPutC( pOrderInfo.atomBagName, szFileName );
                     pOrderInfo.itmResult = hb_itemNew( NULL );
                     SELF_ORDLSTADD( pArea, &pOrderInfo );
                     SELF_GOTOP( pArea );
                     hb_itemRelease( pOrderInfo.itmResult );
                     pOrderInfo.itmResult = NULL;
                  }

                  hb_xvmSeqEnd();
                  if( pUStru->iHbError )
                  {
                     if( pOrderInfo.itmResult )
                        hb_itemRelease( pOrderInfo.itmResult );
                     errcode = HB_FAILURE;
                     if( pUStru->szHbError )
                        sprintf( szReply, "%s%s", "-011", pUStru->szHbError + 4 );
                     else
                        sprintf( szReply, "%s%s%s", szErr4, ":21-1006-0-0\t", szFile );  /* EBDF_CREATE_INDEX */
                  }
                  if( pOrderInfo.atomBagName )
                     hb_itemRelease( pOrderInfo.atomBagName );
                  if( szMultiTags )
                     hb_xfree( szMultiTags );
                  /* set focus to the new created order */
                  leto_SetFocusIf( pUStru->pCurAStru, pArea, szTagName );

#else  /* de-register all private index orders, register later the ones [ auto ] opened by Harbour */

                  pTag = pUStru->pCurAStru->pTag;
                  while( pTag )
                  {
                     pIStru = pTag->pIStru;
                     if( pIStru->uiAreas > 0 )  /* for safety ;-) */
                        pIStru->uiAreas--;
                     if( ! pIStru->uiAreas )
                     {
                        leto_CloseIndex( pIStru );
                        letoDelItemList( &pTStru->IndexList, ( PLETO_LIST_ITEM ) pIStru );
                        pTStru->uiIndexCount--;
                     }

                     pTagFree = pTag;
                     if( pTag == pUStru->pCurAStru->pTag )
                        pTag = pUStru->pCurAStru->pTag = pTag->pNext;
                     else
                        pTag = pTag->pNext;
                     leto_FreeTag( pTagFree );
                  }
                  pUStru->pCurAStru->pTag = NULL;

                  if( ! ( s_bNoSaveWA && ! pTStru->bMemIO ) )
                  {
                     /* mark all orders used by others for re-open, as closed with my ordcreate */
                     uo = 0;
                     while( uo < pTStru->uiIndexCount && ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStru->IndexList, uo ) ) != NULL )
                     {
                        pIStru->bClear = HB_TRUE;
                        uo++;
                     }
                  }

#endif  /* LETO_HBNONCONFORM */

               }
            }

            HB_GC_UNLOCKT();
         }
      }

      if( errcode != HB_SUCCESS )
      {
         if( ! pData )
            pData = szErr1;
         if( ! *szReply )
            strcpy( szReply, pData );
         ulLenLen = strlen( szReply );
      }
      else
      {
         HB_ULONG ulRecLen = leto_recLen( pUStru->pCurAStru->pTStru );
         char *   szTmp = leto_IndexesInfo( pUStru, "*", pArea );
         HB_ULONG ulLen = strlen( szTmp );

         if( ! ( s_bNoSaveWA && ! pUStru->pCurAStru->pTStru->bMemIO ) )
         {
            HB_USHORT   ui = 0;
            PTABLESTRU  pTStru = pUStru->pCurAStru->pTStru;
            PINDEXSTRU  pIStru;
            DBORDERINFO pOrderInfo;
            HB_ULONG    ulMultiPos = 0, ulMultiLen = 0;
            char *      szMultiTags = NULL, * pMultiTag;

            memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
            hb_xvmSeqBegin();
            while( ui < pTStru->uiIndexCount &&
                   ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStru->IndexList, ui ) ) != NULL )
            {
               if( pIStru->bClear )
               {
                  pIStru->bClear = HB_FALSE;
                  /* open multi-Tag index only once */
                  if( ulMultiLen - ulMultiPos < HB_PATH_MAX + 1 )
                  {
                     ulMultiLen = ulMultiPos + ( HB_PATH_MAX * 2 ) + 2;
                     if( ! ulMultiPos )
                     {
                        szMultiTags = ( char * ) hb_xgrabz( ulMultiLen );
                        szMultiTags[ 0 ] = ';';
                        ulMultiPos = 1;
                     }
                     else
                        szMultiTags = ( char * ) hb_xrealloc( szMultiTags, ulMultiLen );
                  }
                  if( ( pMultiTag = strstr( szMultiTags, pIStru->szBagName ) ) == NULL ||
                     ( *( pMultiTag - 1 ) == ';' && *( pMultiTag + strlen( pIStru->szBagName ) ) == ';' ) )
                  {
                     if( s_iDebugMode > 0 )
                        leto_wUsLog( pUStru, -1, "DEBUG leto_CreateIndex re-open Tag: %s Bag: %s",
                                     pIStru->szTagName, pIStru->szBagName );
                     pOrderInfo.atomBagName = hb_itemPutC( pOrderInfo.atomBagName, pIStru->szFullPath );
                     pOrderInfo.itmResult = hb_itemNew( NULL );
                     SELF_ORDLSTADD( pArea, &pOrderInfo );
                     ulMultiPos += sprintf( szMultiTags + ulMultiPos, "%s;", pIStru->szBagName );
                     hb_itemRelease( pOrderInfo.itmResult );
                     pOrderInfo.itmResult = NULL;
                  }
               }
               ui++;
            }
            hb_xvmSeqEnd();
            if( pOrderInfo.itmResult )
               hb_itemRelease( pOrderInfo.itmResult );
            if( pOrderInfo.atomBagName )
               hb_itemRelease( pOrderInfo.atomBagName );
            if( szMultiTags )
               hb_xfree( szMultiTags );
            /* ToDo what if above failed ? */
         }

         if( ulRecLen + ulLen + 1 > uiReplyBufLen )  /* +1 for the '+' */
         {
            uiReplyBufLen = ulRecLen + ulLen + 1;
            szReply = ( char * ) hb_xrealloc( szReply, uiReplyBufLen + 1 );
         }
         *szReply = '+';
         strcpy( szReply + 1, szTmp );
         hb_xfree( szTmp );
         ulLenLen = leto_rec( pUStru, pUStru->pCurAStru, pArea, szReply + 1 + ulLen, NULL ) + 1 + ulLen;
      }
   }

   if( ! pUStru->bBeQuiet )
      leto_SendAnswer( pUStru, szReply, ulLenLen );
   hb_xfree( szReply );
   hb_xfree( szFile );
   hb_xfree( szFileName );
   hb_xfree( szFileRaw );
   if( errcode == HB_SUCCESS )
      pUStru->bLastAct = HB_TRUE;
}

/* leto_udf() leto_OrdCreate( [ nWorkArea ], cBagFileName, cKey, cTagName, lUnique, cFor,
 *            cWhile, lAll, nRecNo, nNext, lRest, lDesc, lCustom, lAdditive, lTemp, cOrder, lExclusive ) */
HB_FUNC( LETO_ORDCREATE )
{
   PUSERSTRU pUStru = letoGetUStru();
   HB_BOOL   bRet = HB_FALSE;
   HB_ULONG  ulAreaID;
   AREAP     pArea;

   if( ! ( pUStru->bRpcMode || s_bNoSaveWA ) )
   {
      hb_retl( bRet );
      return;
   }

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) )
   {
      PAREASTRU pAStru = leto_Select( pUStru, 0, hb_parc( 1 ), HB_TRUE );

      if( pAStru )
         ulAreaID = pAStru->ulSelectID;
      else
         ulAreaID = hb_rddGetCurrentWorkAreaNumber();
   }
   else if( HB_ISNUM( 1 ) && hb_parni( 1 ) > 0 )
      ulAreaID = hb_parni( 1 );
   else
      ulAreaID = hb_rddGetCurrentWorkAreaNumber();

   pArea = ( AREAP ) hb_rddGetWorkAreaPointer( ( int ) ulAreaID );
   if( pArea )
   {
      HB_ULONG  ulRecNo;

      if( SELF_RECNO( pArea, &ulRecNo ) != HB_SUCCESS )
         ulAreaID = 0;
   }
   else
      ulAreaID = 0;

   if( ulAreaID )  /* translate to server internal space */
   {
      PAREASTRU pAStru = leto_Select( pUStru, ulAreaID, NULL, HB_TRUE );

      if( pAStru )
      {
         ulAreaID = pAStru->ulAreaID;
         if( pAStru->ulUdf != ( HB_ULONG ) pUStru->iUserStru )
            ulAreaID = 0;
      }
      else
         ulAreaID = 0;
   }

   if( ( HB_ISCHAR( 2 ) && hb_parclen( 2 ) ) ||
       ( HB_ISCHAR( 4 ) && hb_parclen( 4 ) ) )  /* else BAGname or TAGname */
      bRet = HB_TRUE;
   if( ! ( HB_ISCHAR( 3 ) || ! hb_parclen( 3 ) ) )  /* key forgotten ;-) */
      bRet = HB_FALSE;

   if( ulAreaID && bRet )
   {
      const char * szBagName = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : "";
      const char * szKey = hb_parc( 3 );
      const char * szTagName = HB_ISCHAR( 4 ) ? hb_parc( 4 ) : "";
      HB_BOOL      fUnique = hb_parl( 5 );
      const char * szFor = HB_ISCHAR( 6 ) ? hb_parc( 6 ) : "";
      const char * szWhile = HB_ISCHAR( 7 ) ? hb_parc( 7 ) : "";
      HB_BOOL      fAll = hb_parl( 8 );
      HB_ULONG     ulRecNo = hb_parni( 9 );
      HB_ULONG     ulNext = hb_parni( 10 );
      HB_BOOL      fRest = hb_parl( 11 );
      HB_BOOL      fDescend = hb_parl( 12 );
      HB_BOOL      fCustom = hb_parl( 13 );
      HB_BOOL      fAdditive = hb_parl( 14 );
      HB_BOOL      fTemp = hb_parl( 15 );
      const char * szOrder = HB_ISCHAR( 16 ) ? hb_parc( 16 ) : NULL;
      HB_BOOL      fExlusive = hb_parl( 17 );
      HB_BOOL      fFilter = ( *szFor || *szWhile ) ? HB_TRUE : HB_FALSE;
      char *       szData = ( char * ) hb_xgrab( HB_PATH_MAX + LETO_MAX_TAGNAME + LETO_MAX_KEY + ( 2 * LETO_MAX_EXP ) + 42 );
      char *       ptr = szData;

      ptr += sprintf( szData, "%lu;%s;%s;%s;%c;%s;%s;%c;%lu;%lu;%s;%c;%c;%c;%c;%c%c%c;",  /* LETOCMD_creat_i */
                      ulAreaID,
                      szBagName ? szBagName : "",
                      szTagName ? szTagName : "",
                      szKey,
                      fUnique ? 'T' : 'F',
                      szFor ? szFor : "",
                      szWhile ? szWhile : "",
                      fAll  ? 'T' : 'F',
                      ulRecNo,
                      ulNext,
                      "",
                      fRest ? 'T' : 'F',
                      fDescend ? 'T' : 'F',
                      fCustom ? 'T' : 'F',
                      fAdditive  ? 'T' : 'F',
                      fTemp ? 'T' : 'F',
                      fExlusive ? 'T' : 'F',
                      fFilter ? 'T' : 'F' );
      if( szOrder )
         ptr += sprintf( ptr, "%s;", szOrder );

      pUStru->ulDataLen = ( ptr - szData ) + 2;
      leto_CreateIndex( pUStru, ( const char * ) szData );
      bRet = pUStru->bLastAct;
      hb_xfree( szData );
   }

   hb_retl( bRet );
}

/* only used local */
static void leto_UdfReload( PUSERSTRU pUStru, const char * szData )
{
   PHB_DYNS pSym_Freload = NULL;

   leto_runFunc( NULL, &pSym_Freload, "HS_UDFRELOAD", szData, strlen( szData ) );
   pUStru->bNoAnswer = HB_TRUE;
}

/* only used local */
static void leto_StopServer( PUSERSTRU pUStru, const char * szData )
{
   HB_SYMBOL_UNUSED( szData );

   leto_SrvShutDown( 0 );
   pUStru->bNoAnswer = HB_TRUE;
   pUStru->bCloseConnection = HB_TRUE;
}

/* two groups of commands: with and without workarea preload -- loaded with server start */
void leto_CommandSetInit()
{
   /* @ A - Z */
   s_cmdSet[ LETOCMD_admin   - LETOCMD_OFFSET ] = &leto_Admin;
   s_cmdSet[ LETOCMD_close   - LETOCMD_OFFSET ] = &leto_CloseT;         /* HB_GC_LOCKT() */
   s_cmdSet[ LETOCMD_closall - LETOCMD_OFFSET ] = &leto_CloseTall;      /* HB_GC_LOCKU() + HB_GC_LOCKT() */
   s_cmdSet[ LETOCMD_creat   - LETOCMD_OFFSET ] = &leto_CreateTable;    /* HB_GC_LOCKT() + leto_FreeArea() */
   s_cmdSet[ LETOCMD_creat_i - LETOCMD_OFFSET ] = &leto_CreateIndex;    /* leto_FreeArea() */
   s_cmdSet[ LETOCMD_drop    - LETOCMD_OFFSET ] = &leto_Drop;           /* HB_GC_LOCKT() */
   s_cmdSet[ LETOCMD_exists  - LETOCMD_OFFSET ] = &leto_Exists;
   s_cmdSet[ LETOCMD_file    - LETOCMD_OFFSET ] = &leto_FileFunc;       /* ToDo: verify if a HB_GC_LOCKO() is needed */
   s_cmdSet[ LETOCMD_intro   - LETOCMD_OFFSET ] = &leto_Intro;
   s_cmdSet[ LETOCMD_mgmt    - LETOCMD_OFFSET ] = &leto_Mgmt;
   s_cmdSet[ LETOCMD_open    - LETOCMD_OFFSET ] = &leto_OpenTable;      /* HB_GC_LOCKT() + leto_FreeArea() */
   s_cmdSet[ LETOCMD_open_i  - LETOCMD_OFFSET ] = &leto_OpenIndex;      /* leto_FreeArea() */
   s_cmdSet[ LETOCMD_ping    - LETOCMD_OFFSET ] = &leto_Pong;
   s_cmdSet[ LETOCMD_rddinfo - LETOCMD_OFFSET ] = &leto_RddInfo;
   s_cmdSet[ LETOCMD_rename  - LETOCMD_OFFSET ] = &leto_Rename;         /* HB_GC_LOCKT() */
   s_cmdSet[ LETOCMD_set     - LETOCMD_OFFSET ] = &leto_Set;
   s_cmdSet[ LETOCMD_stop    - LETOCMD_OFFSET ] = &leto_StopServer;
   s_cmdSet[ LETOCMD_quit    - LETOCMD_OFFSET ] = &leto_CloseUStru;
   s_cmdSet[ LETOCMD_ta      - LETOCMD_OFFSET ] = &leto_Transaction;
   s_cmdSet[ LETOCMD_udf_fun - LETOCMD_OFFSET ] = &leto_UdfFun;         /* without workarea */
   s_cmdSet[ LETOCMD_udf_rel - LETOCMD_OFFSET ] = &leto_UdfReload;      /* HB_GC_LOCKT() */
   s_cmdSet[ LETOCMD_var     - LETOCMD_OFFSET ] = &leto_Variables;      /* HB_GC_LOCKV() */
   s_cmdSet[ LETOCMD_zip     - LETOCMD_OFFSET ] = &leto_ToggleZip;      /* toggle [LZ4|zip] compress level -1-9 */

   /* a - z + more */
   s_cmdSet[ LETOCMD_add     - LETOCMD_OFFSET ] = &leto_UpdateRecAdd;
   s_cmdSet[ LETOCMD_cmta    - LETOCMD_OFFSET ] = &leto_UpdateRecAddflush;
   s_cmdSet[ LETOCMD_dbi     - LETOCMD_OFFSET ] = &leto_Info;
   s_cmdSet[ LETOCMD_dboi    - LETOCMD_OFFSET ] = &leto_OrderInfo;
   s_cmdSet[ LETOCMD_flush   - LETOCMD_OFFSET ] = &leto_Flush;
   s_cmdSet[ LETOCMD_goto    - LETOCMD_OFFSET ] = &leto_Goto;
   s_cmdSet[ LETOCMD_group   - LETOCMD_OFFSET ] = &leto_GroupBy;
   s_cmdSet[ LETOCMD_islock  - LETOCMD_OFFSET ] = &leto_IsRecLockedUS;
   s_cmdSet[ LETOCMD_lock    - LETOCMD_OFFSET ] = &leto_Lock;
   s_cmdSet[ LETOCMD_memo    - LETOCMD_OFFSET ] = &leto_MemoRaw;
   s_cmdSet[ LETOCMD_ord     - LETOCMD_OFFSET ] = &leto_Ordfunc;
   s_cmdSet[ LETOCMD_pack    - LETOCMD_OFFSET ] = &leto_Pack;
   s_cmdSet[ LETOCMD_rcou    - LETOCMD_OFFSET ] = &leto_Reccount;
   s_cmdSet[ LETOCMD_rela    - LETOCMD_OFFSET ] = &leto_Relation;
   s_cmdSet[ LETOCMD_scop    - LETOCMD_OFFSET ] = &leto_Scope;
   s_cmdSet[ LETOCMD_filt    - LETOCMD_OFFSET ] = &leto_Filter;
   s_cmdSet[ LETOCMD_skip    - LETOCMD_OFFSET ] = &leto_Skip;
   s_cmdSet[ LETOCMD_sort    - LETOCMD_OFFSET ] = &leto_TransSort;
   s_cmdSet[ LETOCMD_seek    - LETOCMD_OFFSET ] = &leto_Seek;
   s_cmdSet[ LETOCMD_sum     - LETOCMD_OFFSET ] = &leto_Sum;
   s_cmdSet[ LETOCMD_trans   - LETOCMD_OFFSET ] = &leto_TransNoSort;
   s_cmdSet[ LETOCMD_unlock  - LETOCMD_OFFSET ] = &leto_Unlock;
   s_cmdSet[ LETOCMD_upd     - LETOCMD_OFFSET ] = &leto_UpdateRecUpd;
   s_cmdSet[ LETOCMD_cmtu    - LETOCMD_OFFSET ] = &leto_UpdateRecUpdflush;
   s_cmdSet[ LETOCMD_udf_dbf - LETOCMD_OFFSET ] = &leto_UdfDbf;         /* with ulAreaID */
   s_cmdSet[ LETOCMD_zap     - LETOCMD_OFFSET ] = &leto_Zap;
}

/* the beloved heart: central command dispatcher */
HB_BOOL leto_ParseCommand( PUSERSTRU pUStru )
{
   const int iCmd = *pUStru->pBuffer - LETOCMD_OFFSET;

   if( iCmd < LETOCMD_SETLEN && s_cmdSet[ iCmd ] )
   {
      char * ptrPar = ( char * ) ( pUStru->pBuffer + 2 );  /* datasegment after LETOCMD_* + ';' */

      if( iCmd <= 'Z' - LETOCMD_OFFSET )  /* _no_ workarea pre-selected before execute */
         ( *s_cmdSet[ iCmd ] )( pUStru, ptrPar );
      else
      {
         HB_ULONG ulAreaID = strtoul( ptrPar, &ptrPar, 10 );

         if( ! ulAreaID || ! ptrPar || *ptrPar++ != ';' )
            return HB_FALSE;
         else if( ! leto_SelectArea( pUStru, ulAreaID ) )
         {
            pUStru->bCloseConnection = HB_TRUE;
            return HB_FALSE;
         }

         ( *s_cmdSet[ iCmd ] )( pUStru, ptrPar );

         /* release workarea lastly here, if not already done */
         if( pUStru->pCurAStru )
            leto_FreeCurrArea( pUStru );
      }

      if( ! pUStru->bNoAnswer && ! pUStru->ulBytesSend )
         return HB_FALSE;  /* something badly failed, no expected answer was send */

      return HB_TRUE;
   }

   /* unknown request, should not happen */
   return HB_FALSE;
}

