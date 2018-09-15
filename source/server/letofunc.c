/*
 * Leto db server functions
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 *
 * modification and additions 2015-18 Rolf 'elch' Beckmann
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

#define PARSE_MAXDEEP            5   /* used in leto_ParseFilter() */
#define SHIFT_FOR_LEN            3

typedef void ( *CMDSET )( PUSERSTRU, char * );
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

static PHB_ITEM s_pThxArray = NULL;
static HB_COND_NEW( s_ThxCond );
static HB_CRITICAL_NEW( s_ThxMtx );

/* ToDo: make a static struct over all one time set config settings, instead each as single static */
static int       s_iDebugMode = 1;
static char *    s_szServerAddr = NULL;
static char *    s_pDataPath = NULL;
static char *    s_pSharePath = NULL;
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
static char      s_pTrigger[ HB_SYMBOL_NAME_LEN + 1 ] = "";
static HB_BOOL   s_bHardCommit = HB_FALSE;
static HB_BOOL   s_bPass4L = HB_FALSE;        //  Pass needs: Login,Manage,Datamodify
static HB_BOOL   s_bPass4M = HB_FALSE;
static HB_BOOL   s_bPass4D = HB_FALSE;
static HB_BOOL   s_bSMBServer = HB_FALSE;
static char      s_szServerID[ HB_PATH_MAX ] = { 0 };


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
extern void leto_SrvSetPort( int iPort, const char * szAddrSpace );

extern char * leto_memoread( const char * szFilename, HB_ULONG * pulLen );
extern HB_BOOL leto_fileread( const char * szFilename, char * pBuffer, const HB_ULONG ulStart, HB_ULONG * ulLen );
extern HB_BOOL leto_filewrite( const char * szFilename, const char * pBuffer, const HB_ULONG ulStart, HB_ULONG ulLen, HB_BOOL bTrunc );
extern HB_BOOL leto_memowrite( const char * szFilename, const char * pBuffer, HB_ULONG ulLen );

extern void leto_acc_release( void );
extern HB_BOOL leto_acc_find( PUSERSTRU pUStru, const char * szPass );
extern void leto_acc_setPath( const char * szPath );
extern void leto_ToggleZip( PUSERSTRU pUStru, char * szData );

extern void leto_SendError( PUSERSTRU pUStru, const char * szData, HB_ULONG ulLen );
extern void leto_SendAnswer( PUSERSTRU pUStru, const char * szData, HB_ULONG ulLen );
extern void leto_SendAnswer2( PUSERSTRU pUStru, const char * szData, HB_ULONG ulLen, HB_BOOL bAllFine, int iError );
extern HB_BOOL leto_AskAnswer( HB_SOCKET hSocket );
extern void leto_Admin( PUSERSTRU pUStru, char * szData );

extern void leto_Variables( PUSERSTRU pUStru, char * szData );
extern void leto_setVarsMax( HB_ULONG ulMaxVars, HB_ULONG ulMaxVarLen );
extern void leto_varsown_release( PUSERSTRU pUStru );
extern void leto_vars_release( void );

extern void letoListInit( PLETO_LIST pList, HB_ULONG ulSize );
extern void letoListFree( PLETO_LIST pList );
extern HB_BOOL letoListEmptyTS( PLETO_LIST pList );
extern void * letoAddToList( PLETO_LIST pList );
extern void * letoGetListItem( PLETO_LIST pList, HB_USHORT uiNum );
extern void letoDelFromList( PLETO_LIST pList, HB_USHORT uiNum );
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
   PUSERSTRU * pUStruTSD = ( PUSERSTRU * ) hb_stackGetTSD( &s_TSDustru );

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
   return s_uiUsersAlloc;  /* changes only at startup */
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

   strcpy( sFileDef, s_szDirBase );
   if( ! sFile )
      strcpy( sFileDef + strlen( sFileDef ), "letodbf.log" );
   else
      hb_strncpy( sFileDef + strlen( sFileDef ), sFile, HB_PATH_MAX - 1 - strlen( sFileDef ) );

   if( iMsgLen > 0 )
   {
      if( iMsgLen > 1 && s[ iMsgLen - 2 ] == '\r' && s[ iMsgLen - 1 ] == '\n' )
         iMsgLen -= 2;
      else if( s[ iMsgLen - 1 ] == '\n' )
         iMsgLen -= 1;
   }

   HB_GC_LOCKF();

   handle = hb_fsOpen( sFileDef, FO_WRITE | FO_EXCLUSIVE | FO_CREAT );
   if( handle != FS_ERROR )  /* ToDo: how else to inform, quit */
   {
      int  iYear, iMonth, iDay;
      char szDateTime[ 21 ];
      char szTime[ 9 ];

      hb_dateToday( &iYear, &iMonth, &iDay );
      hb_dateTimeStr( szTime );
      sprintf( szDateTime, "%02d.%02d.%04d %s ", iMonth, iDay, iYear, szTime );

      hb_fsSeekLarge( handle, 0, SEEK_END );
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

static char * leto_readServerLog( int iUser, unsigned int nRows, HB_SIZE * nLen )
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
HB_BOOL leto_wUsLogDelete( PUSERSTRU pUStru )
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
      strcpy( szName, s_szDirBase );
      hb_snprintf( szName + strlen( s_szDirBase ), HB_PATH_MAX - 1 - strlen( s_szDirBase ),
                   "letodbf_%0*d.log", leto_log10Len( s_uiUsersAlloc ), pUStru->iUserStru - 1 );

      if( iMsgLen == 0 )
         iMsgLen = strlen( s );

      HB_GC_LOCKF();

      handle = hb_fsOpen( szName, FO_WRITE | FO_CREAT );
      if( handle != FS_ERROR )
      {
         int  iYear, iMonth, iDay;
         char szDateTime[ 30 ];
         char szTime[ 9 ];

         hb_fsSeekLarge( handle, 0, SEEK_END );

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
   PUSERSTRU pUStru = letoGetUStru();

   leto_wUsLog( pUStru, hb_parclen( 1 ), hb_parc( 1 ) );
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
      else if( s_uiLockExtended == DB_DBFLOCK_HB64 )
         iLockScheme = DB_DBFLOCK_HB64;
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

   leto_setSetDeleted( HB_FALSE );  /* Harbour default */

   pRDDNode = hb_rddFindNode( leto_Driver( s_uiDriverDef ), &uiRddID );
   if( ! pRDDNode )
   {
      hb_itemRelease( pItem );
      return;
   }

   /* all below need pRDDNode */

   if( s_uiMemoType )
   {
      hb_itemPutNI( pItem, s_uiMemoType );
      SELF_RDDINFO( pRDDNode, RDDI_MEMOTYPE, 0, pItem );
   }
   if( s_uiMemoBlocksize )
   {
      hb_itemPutNI( pItem, s_uiMemoBlocksize );
      SELF_RDDINFO( pRDDNode, RDDI_MEMOBLOCKSIZE, 0, pItem );
   }
   hb_itemPutNI( pItem, 0 );
   hb_setSetItem( HB_SET_MBLOCKSIZE, pItem );

   if( s_uiLockExtended )
   {
      hb_itemPutNI( pItem, leto_lockScheme( s_uiDriverDef ) );
      SELF_RDDINFO( pRDDNode, RDDI_LOCKSCHEME, 0, pItem );
   }

#if 0
   if( *s_pTrigger )  /* verified DynSym */
   {
      hb_itemPutC( pItem, s_pTrigger );
      SELF_RDDINFO( pRDDNode, RDDI_TRIGGER, 0, pItem );
   }
#endif

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
         else if( leto_stricmp( hb_dynsymName( ( PHB_DYNS ) pArea->atomAlias ), pAStru->pTStru->szLetoAlias ) )
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
      HB_ULONG ulAreaID = pUStru->ulCurAreaID;  /* <> hb_rddGetCurrentWorkAreaNumber() */

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
      PAREASTRU pAStru = leto_FindArea( pUStru, ulAreaID );

      if( pAStru )
      {
         if( pAStru->bNotDetached )
         {
            int iArea;

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
               else
                  leto_wUsLog( pUStru, -1, "ERROR leto_selectArea() not detached - no Area %d", iArea );
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

   if( pUStru->pCurAStru && leto_SelectArea( pUStru, ulAreaID ) )
      hb_retl( HB_TRUE );
   else
      hb_retl( HB_FALSE );
}

/* HB_GC_LOCKT() must be ensured by caller */
static HB_ULONG leto_CreateAreaID( void )
{
   HB_ULONG ulRetVal;

   if( s_AvailIDS.iCurIndex <= 0 )
      ulRetVal = ++s_AvailIDS.ulNextID;
   else
      ulRetVal = s_AvailIDS.pulAreaID[ --s_AvailIDS.iCurIndex ];

   return ulRetVal;
}

/* HB_GC_LOCKT() must be ensured by caller */
static void leto_DelAreaID( const char * szAlias )
{
   HB_ULONG ulAreaID = *szAlias ? strtoul( szAlias + 1, NULL, 10 ) : 0;

   if( ! ulAreaID || ulAreaID > s_AvailIDS.ulNextID )  /* should never happen */
   {
      leto_writelog( NULL, -1, "ERROR leto_DelAreaID! nAreaID (%lu) invalid, s_AvailIDS.ulNextID == %lu )",
                               ulAreaID, s_AvailIDS.ulNextID );
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

   s_AvailIDS.pulAreaID[ s_AvailIDS.iCurIndex++ ] = ulAreaID;
}

/* caller must free result: translate client ALIAS to possible different server-side ALIAS */
static char * leto_AliasTranslate( PUSERSTRU pUStru, const char * szExpr, HB_ULONG * nLen )
{
   PAREASTRU pAStru;
   HB_LONG   lDstLenDif = HB_RDD_MAX_ALIAS_LEN;
   HB_SIZE   nStart = 0, nDst = 0, nDstLen = *nLen + lDstLenDif, nAliasLen, nDiff;
   char      szSrcAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   char *    szDst = ( char * ) hb_xgrab( nDstLen + 1 );
   const char * ptr = strstr( szExpr + nStart, "->" );

   while( ptr )
   {
      nAliasLen = 0;
      while( ptr - nAliasLen - 1 >= szExpr )
      {
         if( ! HB_ISNEXTIDCHAR( *( ptr - nAliasLen - 1 ) ) )
            break;
         nAliasLen++;
      }
      nDiff = ptr - ( szExpr + nStart ) - nAliasLen;
      memcpy( szDst + nDst, szExpr + nStart, nDiff );
      nDst += nDiff;
      if( nAliasLen && nAliasLen <= HB_RDD_MAX_ALIAS_LEN )
      {
         memcpy( szSrcAlias, ptr - nAliasLen, nAliasLen );
         szSrcAlias[ nAliasLen ] = '\0';
         if( ! leto_stricmp( szSrcAlias, "FIELD" ) )  /* MEMVAR-> not allowed */
            pAStru = leto_FindAlias( pUStru, NULL );
         else
            pAStru = leto_FindAlias( pUStru, szSrcAlias );
         if( pAStru && pAStru->pTStru )
         {
            nDiff = strlen( pAStru->pTStru->szLetoAlias );
            lDstLenDif -= nDiff - nAliasLen;
            if( lDstLenDif < 0 )
            {
               nDstLen += HB_RDD_MAX_ALIAS_LEN;
               lDstLenDif += HB_RDD_MAX_ALIAS_LEN;
               szDst = ( char * ) hb_xrealloc( szDst, nDstLen + 1 );
            }
            memcpy( szDst + nDst, pAStru->pTStru->szLetoAlias, nDiff );
            nDst += nDiff;
         }
         else if( nAliasLen )
         {
            memcpy( szDst + nDst, szSrcAlias, nAliasLen );
            nDst += nAliasLen;
         }
      }
      *( szDst +nDst++ ) = '-';
      *( szDst +nDst++ ) = '>';

      nStart += ptr - ( szExpr + nStart ) + 2;
      ptr = strstr( szExpr + nStart, "->" );
      if( ptr && ( HB_ULONG ) ( ptr - szExpr ) > *nLen )
         ptr = NULL;
   }

   if( nStart < *nLen )
   {
      memcpy( szDst + nDst, szExpr + nStart, *nLen - nStart );
      nDst += *nLen - nStart;
   }
   szDst[ nDst ] = '\0';
   *nLen = nDst;

   return szDst;
}

static char leto_ExprGetType( PUSERSTRU pUStru, const char * pExpr, HB_SIZE nLen )
{
   PHB_ITEM     pItem = hb_itemPutCL( NULL, pExpr, nLen );
   const char * szType;
   char         cRet;

   if( strstr( hb_itemGetCPtr( pItem ), "->" ) )
   {
      HB_ULONG ulLen = ( HB_ULONG ) nLen;
      char *   szExpr = leto_AliasTranslate( pUStru, hb_itemGetCPtr( pItem ), &ulLen );

      pItem = hb_itemPutCL( pItem, szExpr, ulLen );
      hb_xfree( szExpr );
   }

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
   if( hb_parclen( 8 ) )
      leto_acc_setPath( hb_parc( 8 ) );

   if( HB_ISLOG( 10 ) )
      s_bShareTables = hb_parl( 10 );
   if( HB_ISLOG( 11 ) )
      s_bNoSaveWA = hb_parl( 11 );

   if( HB_ISNUM( 12 ) && HB_ISNUM( 13 ) )
      leto_setVarsMax( hb_parnl( 12 ), ( HB_ULONG ) hb_parni( 13 ) );

   if( HB_ISNUM( 14 ) )
   {
      s_uiCacheRecords = ( HB_USHORT ) hb_parni( 14 );
      if( s_uiCacheRecords < 1 )
         s_uiCacheRecords = 10;
      else if( s_uiCacheRecords > 1000 )
         s_uiCacheRecords = 1000;
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
   {
      s_iAutOrder = hb_parni( 19 );
      if( s_iAutOrder < 0 )
         s_iAutOrder = 0;
   }
   if( HB_ISNUM( 20 ) && hb_parni( 20 ) > 0 )
      s_uiMemoType = ( HB_USHORT ) hb_parni( 20 );
   else
   {
      if( s_uiDriverDef == LETO_NTX )
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
   if( HB_ISNUM( 24 ) && hb_parni( 24 ) > 0 && hb_parni( 24 ) <= 0x10000 && hb_parni( 24 ) % 32 == 0 )
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
      if( uiLen > 0 && uiLen <= HB_SYMBOL_NAME_LEN )
         strcpy( s_pTrigger, hb_parc( 26 ) );
   }
   if( HB_ISLOG( 27 ) )
      s_bHardCommit = hb_parl( 27 );
   if( HB_ISLOG( 28 ) )
      s_bSMBServer = hb_parl( 28 );
   if( hb_parclen( 29 ) )
   {
      s_pSharePath = ( char * ) hb_xgrab( hb_parclen( 29 ) + 1 );
      strcpy( s_pSharePath, hb_parc( 29 ) );
   }
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
      case LETOOPT_DATAPATH:
         hb_retc( s_pDataPath );
         break;
      case LETOOPT_INDEXTYPE:
         hb_retni( s_uiDriverDef );
         break;
      case LETOOPT_ANYEXT:
         hb_retl( s_bAnyExt );
         break;
      case LETOOPT_SHARETABLES:
         hb_retl( s_bShareTables );
         break;
      case LETOOPT_NOSAVEWA:
         hb_retl( s_bNoSaveWA );
         break;
      case LETOOPT_OPTIMIZE:
         hb_retl( s_bOptimize );
         break;
      case LETOOPT_AUTOORDER:
         hb_retni( s_iAutOrder );
         break;
      case LETOOPT_MEMOTYPE:
         hb_retni( s_uiMemoType );
         break;
      case LETOOPT_LOCKCHEME:
         hb_retni( leto_lockScheme( s_uiDriverDef ) );  // TO Fix: actual driver or only if extended lock ?
         break;
      case LETOOPT_UDFENABLED:
         hb_retl( s_bUdfEnabled );
         break;
      case LETOOPT_MEMOSIZE:
         hb_retni( s_uiMemoBlocksize );
         break;
      case LETOOPT_LOWERPATH:
         hb_retl( s_bLowerPath );
         break;
      case LETOOPT_TRIGGER:
         hb_retc( s_pTrigger );
         break;
      case LETOOPT_HARDCOMMIT:
         hb_retl( s_bHardCommit );
         break;
      case LETOOPT_DEBUGLEVEL:
         hb_retni( s_iDebugMode );
         break;
      case 42:  /* what else is the answer to all !? */
      {
#if 0  /* intentional forced crash test for debug test internal crash log */
         char * pCrash = NULL;

   #if 0  /* to test the delayed error */
         leto_SendAnswer2( pUStru, szErr3, 4, HB_FALSE, 1001 );
   #endif

         hb_xfree( pCrash );
         hb_retni( 1 );
#else
         PUSERSTRU pUStru = letoGetUStru();

         leto_writelog( NULL, -1, "INFO: USERSTRU %d, AREASTRU %d, TABLESTRU %d, GLOBESTRU %d INDEXSTRU %d ",
                        sizeof( USERSTRU ), sizeof( AREASTRU ), sizeof( TABLESTRU ), sizeof( GLOBESTRU ), sizeof( INDEXSTRU )  );
         /* test delayed error */
         if( pUStru->hSocketErr )
            leto_SendAnswer2( pUStru, "3210", 4, HB_FALSE, 1000 );

#endif
         break;
      }

      default:
         hb_retc( "Error: unknown option for LETO_GETAPPOPTIONS()" );
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
            pAStru = NULL;
         else if( s_iDebugMode > 20 )
            leto_wUsLog( pUStru, -1,"DEBUG leto_Select( %s ) found in %lu ", pAStru->szAlias, pAStru->ulAreaID );
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
         if( s_iDebugMode > 20 )
            leto_wUsLog( pUStru, -1,"DEBUG leto_Select( %s ) pAStru area %lu (%lu) ", szAlias, pAStru->ulAreaID, pAStru->ulSelectID );
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
   else if( hb_parclen( 1 ) )
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

   if( hb_parclen( 1 ) )
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
      ulLen = sprintf( szTemp, "%s;%s;%d;%d;%d;%d;", szBagExt ? szBagExt : "",
                       szBagExt ? szBagExt : "", iMemoType, iMemoVer, iMemoBlocksize, iLockScheme );

   hb_itemRelease( pItem );
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

int leto_GetParam( char * szData, ... )  /* last 'char **' in list must be NULL */
{
   char * ptr;
   int    iRes = 0;

   if( ( ptr = strchr( szData, ';' ) ) != NULL )
   {
      va_list ap;
      char ** pptr;

      iRes++;
      *ptr = '\0';
      va_start( ap, szData );

      for( ;; )
      {
         pptr = va_arg( ap, char ** );
         if( pptr == NULL )
            break;
         *pptr = ++ptr;
         if( ( ptr = strchr( ptr, ';' ) ) == NULL )
         {
            *pptr = NULL;
            break;
         }
         *ptr = '\0';
         iRes++;
      }

      va_end( ap );
   }

   return iRes;
}

static HB_USHORT leto_DataPath( const char * szFilename, char * szBuffer )
{
   const char * ptr = szFilename;
   int          iLen = 0;

   szBuffer[ 0 ] = '\0';
   if( szFilename && szFilename[ 0 ] )
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
   const char * ptr;

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

static void leto_IsRecLockedUS( PUSERSTRU pUStru, char * szData )
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
                  if( pField->uiFlags )  /* binary, compressed, encrypted, ... */
                     ulRealLen = pField->uiLen;
                  else  /* Trimmed field length */
                  {
                     ptrEnd = ptr + pField->uiLen - 1;
                     while( ptrEnd > ptr && *ptrEnd == ' ' )
                        ptrEnd--;
                     ulRealLen = ptrEnd - ptr + ( *ptrEnd == ' ' ? 0 : 1 );
                  }
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

      ulRealLen = pData - szData;
      HB_PUT_LE_UINT24( szData, ( HB_U32 ) ulRealLen - SHIFT_FOR_LEN );
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

static HB_BOOL leto_ProdSupport( const char * szDriver )
{
   LPRDDNODE pRDDNode;
   HB_USHORT uiRddID;
   HB_BOOL   fSupportStruct = HB_FALSE;

   pRDDNode = hb_rddFindNode( szDriver, &uiRddID );
   if( pRDDNode )
   {
      PHB_ITEM pItem = hb_itemPutC( NULL, NULL );

      if( SELF_RDDINFO( pRDDNode, RDDI_STRUCTORD, 0, pItem ) == HB_SUCCESS )
         fSupportStruct = hb_itemGetL( pItem );
      hb_itemRelease( pItem );
   }

   return fSupportStruct;
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
       ( ( pIStru->bProduction && leto_ProdSupport( pAStru->pTStru->szDriver ) ) || *szFileName == '*' ||
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
         if( uiRegistered )
         {
            if( iLenLen - iLen )
               memmove( pIdxInfo + iLen, pIdxInfo + iLenLen, iIdxInfoStart + 1 - iLenLen );  /* move including '\0' */
            iLenLen -= ( iLenLen - iLen );
         }
         else
            pIdxInfo[ iLen ] = '\0';
      }
      /* catch an ignored possible double TagName by user */
      if( uiRegistered && ( uiOrderActive > uiRegistered || ( uiOrderActive && ! pUStru->pCurAStru->pTagCurrent ) ) )
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
   if( ! s_bNoSaveWA || pTStru->bMemIO )
      leto_DelAreaID( pTStru->szLetoAlias );

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
}

static PGLOBESTRU leto_InitGlobe( const char * szTable, HB_U32 uiCrc, HB_UINT uiTable, HB_USHORT uiLen, const char * szCdp, unsigned char uMemoType )
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
      pGStru->uMemoType = uMemoType;
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
   PHB_ITEM   pItem;
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
   SELF_FIELDCOUNT( pArea, &pTStru->uiFields );

   pTStru->ulFlags = 0;
   pTStru->uiIndexCount = 0;
   pTStru->uiAreas = 1;

   if( uiTable >= s_uiTablesMax )
      s_uiTablesMax = uiTable + 1;
   s_uiTablesCurr++;

   pItem = hb_itemNew( NULL );
   SELF_INFO( pArea, DBI_GETRECSIZE, pItem );  /* including delete flag */
   pTStru->uiRecordLen = ( HB_USHORT ) hb_itemGetNI( pItem );

   pItem = hb_itemPutNI( pItem, 0 );
   SELF_INFO( pArea, DBI_MEMOTYPE, pItem );

   pTStru->pGlobe = leto_InitGlobe( szName, pTStru->uiCrc, uiTable, uiLen, szCdp, ( unsigned char ) hb_itemGetNI( pItem ) );

   hb_itemRelease( pItem );
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
         iTimeOut++;
         do
         {
            dbLockInfo.fResult = HB_FALSE;
            SELF_LOCK( pArea, &dbLockInfo );
            bRet = dbLockInfo.fResult ? HB_TRUE : HB_FALSE;
            if( bRet )
               break;
            else if( ! pAStru->pTStru->pGlobe->bLocked && iTimeOut > 20 )
            {
               /*  it's a 3rd party lock */
               hb_threadReleaseCPU();
               iTimeOut -= 20;
               if( iTimeOut > 100 )
               {
                  hb_threadReleaseCPU();
                  iTimeOut -= 20;
               }
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
         if( s_iDebugMode > 1 )
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
      pUStru->hSocketErr = HB_NO_SOCKET;
   }

   /* now main socket */
   if( pUStru->hSocket != HB_NO_SOCKET )
   {
      hb_socketShutdown( pUStru->hSocket, HB_SOCKET_SHUT_RDWR );
      hb_socketClose( pUStru->hSocket );
      pUStru->hSocket = HB_NO_SOCKET;
   }
   if( pUStru->zstream )
   {
#ifdef USE_LZ4
      hb_lz4netClose( ( PHB_LZ4NET ) pUStru->zstream );
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

      leto_writelog( NULL, -1, "INFO: disconnect %s:%d %s users=(%d : %d : %d), tables=(%d : %d)",
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
   if( pUStru->ulOpenHandles )
   {
      HB_ULONG ulPos;

      for( ulPos = 0; ulPos < pUStru->ulOpenHandles; ulPos++ )
      {
         hb_fsClose( hb_numToHandle( pUStru->pOpenHandles[ ulPos ] ) );
      }
      hb_xfree( pUStru->pOpenHandles );
      pUStru->pOpenHandles = NULL;
      pUStru->ulOpenHandles = 0;
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
HB_BOOL leto_SeekUS( int iServerPort, HB_SOCKET hSocketErr )
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
            pUStru->hSocketErr = hSocketErr;
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
      pUStru->pBuffer = ( HB_BYTE * ) hb_xrealloc( pUStru->pBuffer, pUStru->ulBufferLen );
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
   PUSERSTRU pUStru;
   int       iUserStru;
   HB_CRITICAL_NEW( pMutex );

   HB_GC_LOCKU();

   pUStru = s_users + s_uiUsersFree;
   iUserStru = s_uiUsersFree;

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
   pUStru->bDbEvalCompat = HB_TRUE;

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
               hb_fsPipeWrite( pUStru->hSockPipe[ 1 ], cToPipe, 1, 0 );
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
            hb_lz4netClose( ( PHB_LZ4NET ) pUStru->zstream );
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

   if( hb_parclen( 1 )  )
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
   const char * szAddr = hb_parclen( 1 ) ? hb_parc( 1 ) : "";
   const int    iPort = HB_ISNUM( 2 ) ? hb_parni( 2 ) : LETO_DEFAULT_PORT;
   const char * szAddrSpace = hb_parclen( 3 ) ? hb_parc( 3 ) : NULL;
   const char * szServerID = hb_parclen( 4 ) ? hb_parc( 4 ) : NULL;

   if( ! s_users )
   {
      /* allocate maximum useable user/ tables structures, plus one more to avoid ptr overflow */
      s_users = ( USERSTRU * ) hb_xgrabz( sizeof( USERSTRU ) * ( s_uiUsersAlloc + 1 ) );
      s_tables = ( TABLESTRU * ) hb_xgrabz( sizeof( TABLESTRU ) * ( s_uiTablesAlloc + 1 ) );
      s_globes = ( GLOBESTRU * ) hb_xgrabz( sizeof( GLOBESTRU ) * ( s_uiTablesAlloc + 1 ) );

      s_ulStartDateSec = ( unsigned long ) hb_dateMilliSeconds() / 1000;
      if( szAddr )
         s_szServerAddr = hb_strdup( szAddr );
      if( szServerID && *szServerID )
         hb_strncpy( s_szServerID, szServerID, HB_PATH_MAX - 1 );

      leto_SrvSetPort( iPort, szAddrSpace );

      leto_writelog( NULL, -1, "INFO: %s %s, will run at %s%s:%d ( internal also used :%d )",
                     LETO_RELEASE_STRING, LETO_VERSION_STRING, strlen( szAddr ) ? "IP " : "port ", szAddr, iPort, iPort + 1 );
      leto_writelog( NULL, -1, "INFO: DataPath=%s, ShareTables=%d, NoSaveWA=%d, max database=%d",
                     ( s_pDataPath ? s_pDataPath : "" ), s_bShareTables, s_bNoSaveWA, s_uiTablesAlloc);
      leto_writelog( NULL, -1, "INFO: LoginPassword=%d, CacheRecords=%d, LockExtended=%d",
                     s_bPass4L, s_uiCacheRecords, s_uiLockExtended );
   }

   if( *s_pTrigger )
   {
      PHB_DYNS pTrigger = hb_dynsymFindName( s_pTrigger );

      if( ! pTrigger || ! hb_dynsymIsFunction( pTrigger ) )
         *s_pTrigger = '\0';
   }
}

/* this is an EXIT PROCEDURE for main thread -- so willful no MT locking done */
HB_FUNC( LETO_RELEASEDATA )  /* during server down */
{
   DATABASE * pDBNext, * pDB = s_pDB;
   HB_UINT    ui = 0;
   PUSERSTRU  pUStru = letoGetUStru();
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
      if( s_pSharePath )
      {
         hb_xfree( s_pSharePath );
         s_pSharePath = NULL;
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

static _HB_INLINE_ HB_ERRCODE leto_GotoIf( AREAP pArea, HB_ULONG ulRecNo )
{
   HB_ULONG ulOldRecNo;

   SELF_RECNO( pArea, &ulOldRecNo );  /* update pending relations */
   if( ulOldRecNo == ulRecNo )
      return HB_SUCCESS;
   else
      return SELF_GOTO( pArea, ulRecNo );
}

#ifdef USE_LZ4
static _HB_INLINE_ void leto_lz4Uncompress( char * pDst, HB_SIZE * pnDst, const char * pSrc, HB_SIZE nSrc )
{
   /* basically we can verify decompression with: return length == *pnDst */
   if( LZ4_decompress_safe( pSrc, pDst, nSrc, *pnDst ) <= 0 )
      *pnDst = 0;
}

static _HB_INLINE_ void leto_lz4Compress( char * pDst, HB_SIZE * pnDst, const char * pSrc, HB_SIZE nLen, int iLevel )
{
   *pnDst = ( HB_SIZE ) LZ4_compress_fast( pSrc, pDst, nLen, *pnDst, iLevel );
}
#endif

static const char * leto_DecryptText( PUSERSTRU pUStru, HB_ULONG * pulLen, char * ptr )
{
   HB_BOOL fCompressed;

   *pulLen = HB_GET_LE_UINT32( ( HB_BYTE * ) ptr );
   fCompressed = ( *pulLen & 0x80000000 );
   if( fCompressed )
      *pulLen &= 0x7FFFFFFF;
   ptr += 4;

   if( fCompressed )
   {
      HB_SIZE nSize = HB_GET_LE_UINT32( ( HB_BYTE * ) ptr + *pulLen - 8 );

      *pulLen = HB_GET_LE_UINT32( ( HB_BYTE * ) ptr + *pulLen - 4 );
      if( *pulLen > pUStru->ulBufCryptLen )
      {
         if( ! pUStru->ulBufCryptLen )
         {
            pUStru->ulBufCryptLen = HB_MAX( *pulLen, LETO_SENDRECV_BUFFSIZE );
            pUStru->pBufCrypt = ( HB_BYTE * ) hb_xgrab( pUStru->ulBufCryptLen + 1 );
         }
         else
         {
            pUStru->ulBufCryptLen = *pulLen;
            pUStru->pBufCrypt = ( HB_BYTE * ) hb_xrealloc( pUStru->pBufCrypt, *pulLen + 1 );
         }
      }

#ifdef USE_LZ4
      leto_lz4Uncompress( ( char * ) pUStru->pBufCrypt, ( HB_SIZE * ) pulLen, ( const char * ) ptr, nSize );
#else
      hb_zlibUncompress( ( char * ) pUStru->pBufCrypt, ( HB_SIZE * ) pulLen, ( const char * ) ptr, nSize );
#endif
      ptr = ( char * ) pUStru->pBufCrypt;
   }

   ptr[ *pulLen ] = '\0';
   return ptr;
}

static HB_ULONG leto_CryptText( PUSERSTRU pUStru, const char * pData, HB_ULONG ulLen, HB_ULONG ulPrelead )
{
   HB_ULONG ulBufLen;
   HB_SIZE  nDest;
   HB_BOOL  fCompress;

#ifdef USE_LZ4
   fCompress = ( pUStru->iZipRecord < 1 && ulLen > LETO_LZ4_COMPRESS_MIN ) ? HB_TRUE : HB_FALSE;
   if( fCompress )
   {
      nDest = ( HB_SIZE ) LZ4_COMPRESSBOUND( ulLen );
      if( ! nDest )  /* too big > 0x7E000000 */
      {
         ulBufLen = 5 + ulPrelead;
         ulLen = 0;
      }
      else
         ulBufLen = nDest + 21;  /* encrypt +8, zip-lengths +8, length +4, termination + 1  */
   }
#else
   fCompress = ( pUStru->iZipRecord < 1 && ulLen > LETO_ZIP_MINLENGTH ) ? HB_TRUE : HB_FALSE;
   if( fCompress )
   {
      nDest = hb_zlibCompressBound( ulLen );
      ulBufLen = nDest + 21 + ulPrelead;  /* encrypt +8, zip-lengths +8, length +4, termination + 1  */
   }
#endif
   else
   {
      nDest = 0;
      ulBufLen = ulLen + 21 + ulPrelead;
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
      if( fCompress )
      {
#ifdef USE_LZ4
         leto_lz4Compress( ( char * ) pUStru->pBufCrypt + 4 + ulPrelead, &nDest, ( const char * ) pData, ulLen,
                          HB_ZLIB_COMPRESSION_SPEED );
#else
         hb_zlibCompress( ( char * ) pUStru->pBufCrypt + 4 + ulPrelead, &nDest, ( const char * ) pData, ulLen,
                          HB_ZLIB_COMPRESSION_SPEED );  // == HB_ZLIB_RES_OK
#endif
         HB_PUT_LE_UINT32( pUStru->pBufCrypt + 4 + nDest + ulPrelead, nDest );
         HB_PUT_LE_UINT32( pUStru->pBufCrypt + 4 + nDest + 4 + ulPrelead, ulLen );
         ulLen = ( nDest + 8 );
         HB_PUT_LE_UINT32( pUStru->pBufCrypt + ulPrelead, ulLen | 0x80000000 );
      }
      else
      {
         memcpy( pUStru->pBufCrypt + 4 + ulPrelead, pData, ulLen );
         HB_PUT_LE_UINT32( pUStru->pBufCrypt + ulPrelead, ulLen );
      }
   }
   else  /* ulLen == 0 */
      memset( pUStru->pBufCrypt + ulPrelead, '\0', 4 );

   ulLen += 4;
   pUStru->pBufCrypt[ ulLen + ulPrelead ] = '\0';

   return ulLen;
}

static void leto_RddInfo( PUSERSTRU pUStru, char * szData )
{
   char *    pp2, * pp3 = NULL;
   int       nParam = leto_GetParam( szData, &pp2, &pp3, NULL );
   HB_USHORT uiIndex;

   if( nParam < 2 || ! *szData || ( uiIndex = ( HB_USHORT ) atoi( pp2 ) ) == 0 )
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
            case RDDI_DEBUGLEVEL:
            case RDDI_AUTOORDER:
            case RDDI_LOCKTIMEOUT:
            {
               HB_BOOL bSet = ( pp3 && *pp3 );
               int     iOldValue = 0;

               if( bSet )
                  pItem = hb_itemPutNI( NULL, ( int ) atoi( pp3 ) );

               if( uiIndex == RDDI_AUTOORDER )
               {
                  iOldValue = hb_setGetAutOrder();
                  if( bSet )
                     hb_setSetItem( HB_SET_AUTORDER, pItem );
               }
               else if( uiIndex == RDDI_DEBUGLEVEL )  /* set new debug level, NOT TS */
               {
                  iOldValue = s_iDebugMode;
                  if( bSet )
                     s_iDebugMode = hb_itemGetNI( pItem );  /* NOT TS */
               }
               else if( uiIndex == RDDI_LOCKTIMEOUT )  /* set new debug level, NOT TS */
               {
                  iOldValue = pUStru->iLockTimeOut;
                  if( bSet )
                     pUStru->iLockTimeOut = hb_itemGetNI( pItem );
               }
               sprintf( szInfo, "+%d;", iOldValue );
               hb_itemRelease( pItem );
               break;
            }

            /* booleans */
            case RDDI_MULTITAG:
               {
                  pItem = hb_itemPutNI( NULL, 0 );
                  SELF_RDDINFO( pRDDNode, uiIndex, 0, pItem );
                  sprintf( szInfo, "+%c;", hb_itemGetL( pItem ) ? 'T' : 'F' );
                  hb_itemRelease( pItem );
                  break;
               }

            case RDDI_OPTIMIZE:
               sprintf( szInfo, "+%c;", hb_setGetOptimize() ? 'T' : 'F' );
               if( pp3 && strlen( pp3 ) == 1 )
               {
                  pItem = hb_itemPutL( NULL, ( *pp3 == 'T' ) );
                  hb_setSetItem( HB_SET_OPTIMIZE, pItem );
                  hb_itemRelease( pItem );
               }
               break;

            case RDDI_FORCEOPT:
               sprintf( szInfo, "+%c;", hb_setGetForceOpt() ? 'T' : 'F' );
               if( pp3 && strlen( pp3 ) == 1 )
               {
                  pItem = hb_itemPutL( NULL, ( *pp3 == 'T' ) );
                  hb_setSetItem( HB_SET_FORCEOPT, pItem );
                  hb_itemRelease( pItem );
               }
               break;

            case RDDI_AUTOLOCK:
               sprintf( szInfo, "+%c;", ( pUStru->uSrvLock & 0x01 ) ? 'T' : 'F' );
               if( pp3 && strlen( pp3 ) == 1 )
               {
                  if( *pp3 == 'T' )
                     pUStru->uSrvLock |= 0x01;
                  else
                     pUStru->uSrvLock &= ~( 0x01 );
               }
               break;

            case RDDI_AUTOOPEN:
               sprintf( szInfo, "+%c;", hb_setGetAutOpen() ? 'T' : 'F' );
               if( pp3 && strlen( pp3 ) == 1 )
               {
                  pItem = hb_itemPutL( NULL, ( *pp3 == 'T' ) );
                  hb_setSetItem( HB_SET_AUTOPEN, pItem );
                  hb_itemRelease( pItem );
               }
               break;

            case RDDI_STRUCTORD:
               sprintf( szInfo, "+%c;", hb_itemGetL( pItem ) ? 'T' : 'F' );
               if( pp3 && strlen( pp3 ) == 1 )
                  pItem = hb_itemPutL( NULL, ( *pp3 == 'T' ) );
               else
                  pItem = hb_itemNew( NULL );

               SELF_RDDINFO( pRDDNode, uiIndex, 0, pItem );
               hb_itemRelease( pItem );
               break;

            case RDDI_DBEVALCOMPAT:
               sprintf( szInfo, "+%c;", pUStru->bDbEvalCompat ? 'T' : 'F' );
               if( pp3 && strlen( pp3 ) == 1 )
                  pUStru->bDbEvalCompat = ( *pp3 == 'T' );
               break;

            case RDDI_TRIGGER:
               sprintf( szInfo, "+%s", s_pTrigger );
               break;

            default:
               strcpy( szInfo, szErr3 );
               break;
         }
         leto_SendAnswer( pUStru, szInfo, strlen( szInfo ) );
      }
      else
         leto_SendAnswer( pUStru, szErr1, 4 );
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
               PHB_ITEM pItem = hb_itemPutC( NULL, NULL );

               SELF_RDDINFO( pRDDNode, uiIndex, 0, pItem );
               if( uiIndex == RDDI_ORDBAGEXT && ! hb_itemGetCLen( pItem ) )
                  SELF_RDDINFO( pRDDNode, RDDI_ORDEREXT, 0, pItem );
               strcpy( szValue, hb_itemGetCPtr( pItem ) );
               hb_itemRelease( pItem );
               break;
            }

            case RDDI_PENDINGTRIGGER:
            {
               LPDBFDATA pData = DBFNODE_DATA( pRDDNode );

               if( pData->szPendingTrigger )
               {
                  strcpy( szValue, pData->szPendingTrigger );
                  hb_xfree( pData->szPendingTrigger );
                  pData->szPendingTrigger = NULL;
               }
               break;
            }
         }
      }
   }
}

/* secured, only drop table/ index if not used */
static void leto_Drop( PUSERSTRU pUStru, char * szData )
{
   char * pIFile, * szNonDefMemoExt = NULL;
   char   szBuf[ HB_PATH_MAX ];

   if( leto_GetParam( szData, &pIFile, &szNonDefMemoExt, NULL ) < 2 )
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
         if( ! strlen( hb_setGetPath() ) || strchr( szData, DEF_SEP ) || strchr( szData, DEF_CH_SEP ) )
            leto_DataPath( szData, szBuf );
         else
            strcpy( szBuf, szData );
         pName1 = hb_itemPutC( NULL, szBuf );

         if( *pIFile )
         {
            if( ! strlen( hb_setGetPath() ) || strchr( pIFile, DEF_SEP ) || strchr( pIFile, DEF_CH_SEP ) )
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
            char      szFileName[ HB_PATH_MAX ];

            /* check if table is used, add extension if missing */
            if( ! pFilePath->szExtension )
            {
               char szExt[ HB_MAX_FILE_EXT + 1 ];

               if( ! *( pUStru->szDriver ) || strlen( pUStru->szDriver ) < 3 )
                  leto_RddiGetValue( hb_rddDefaultDrv( NULL ), RDDI_TABLEEXT, szExt );
               else
                  leto_RddiGetValue( pUStru->szDriver, RDDI_TABLEEXT, szExt );
               pFilePath->szExtension = szExt;
               hb_fsFNameMerge( szFileName, pFilePath );
            }
            else
               hb_strncpy( szFileName, szData, HB_PATH_MAX - 1 );
            iTableStru = leto_FindTable( szFileName, NULL );

            /* check if table is used solely by active user */
            if( iTableStru >= 0 && ( ( PTABLESTRU ) s_tables + iTableStru )->pGlobe->uiAreas == 1 )
            {
               PAREASTRU       pAStru;
               PLETO_LIST_ITEM pListItem = pUStru->AreasList.pItem;

               while( pListItem )
               {
                  pAStru = ( PAREASTRU ) ( pListItem + 1 );
                  if( pAStru->pTStru->pGlobe == ( ( PTABLESTRU ) s_tables + iTableStru )->pGlobe )
                  {
                     /* leto_CloseArea( pUStru, pAStru ) */
                     iTableStru = -1;
                     break;
                  }

                  pListItem = pListItem->pNext;
               }
            }

#if ! defined( __HARBOUR30__ )
            if( ! pName2 )
               hb_fileExists( szFileName, szFileName );
#endif
            if( iTableStru < 0 && SELF_DROP( pRDDNode, pName1, pName2 ? pName2 : NULL, 0 ) == HB_SUCCESS )
            {
#if ! defined( __HARBOUR30__ )
               if( ! pName2 && szNonDefMemoExt && *szNonDefMemoExt )
               {
                  PHB_ITEM pDefaultMemoExt = hb_itemPutC( NULL, NULL );

                  SELF_RDDINFO( pRDDNode, RDDI_MEMOEXT, 0, pDefaultMemoExt );
                  if( leto_stricmp( hb_itemGetCPtr( pDefaultMemoExt ), szNonDefMemoExt ) )
                  {
                     pFilePath->szExtension = szNonDefMemoExt;
                     hb_fsFNameMerge( szFileName, pFilePath );
                     if( hb_fileExists( szFileName, szFileName ) )
                        hb_fileDelete( szFileName );
                  }

                  hb_itemRelease( pDefaultMemoExt );
               }
#endif
               leto_SendAnswer( pUStru, "+T;0;", 5 );
            }
            else if( iTableStru >= 0 )
               leto_SendAnswer( pUStru, "+F;1;", 5 );
            else
               leto_SendAnswer( pUStru, "+F;0;", 5 );
            hb_xfree( pFilePath );
         }
         else
            leto_SendAnswer( pUStru, "+F;2;", 5 );

         hb_itemRelease( pName1 );
         if( pName2 )
            hb_itemRelease( pName2 );
      }
      else
         leto_SendAnswer( pUStru, szErr2, 4 );

      HB_GC_UNLOCKT();
   }
}

static void leto_Exists( PUSERSTRU pUStru, char * szData )
{
   char * pIFile;
   char   szBuf[ HB_PATH_MAX ];

   if( leto_GetParam( szData, &pIFile, NULL ) < 2 )
      leto_SendAnswer( pUStru, szErr2, 4 );
   else
   {
      const char * szDriver = hb_rddDefaultDrv( NULL );
      HB_USHORT    uiRddID;
      LPRDDNODE    pRDDNode = hb_rddFindNode( szDriver, &uiRddID );

      if( pRDDNode )
      {
         PHB_ITEM pName1, pName2 = NULL;

         if( strchr( szData, DEF_SEP ) || strchr( szData, DEF_CH_SEP ) )
            leto_DataPath( szData, szBuf );
         else
            hb_strncpy( szBuf, szData, HB_PATH_MAX - 1 );
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
   }
}

static void leto_Rename( PUSERSTRU pUStru, char * szData )
{
   char * pIFile, * pNewFile;
   char   szBuf[ HB_PATH_MAX ];

   if( leto_GetParam( szData, &pIFile, &pNewFile, NULL ) < 3 )
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
   char *    ptr, * ptr2;
   HB_BOOL   bRet = HB_FALSE;

   strcpy( szPath, szTarget );
   leto_StrTran( szPath, DEF_CH_SEP, DEF_SEP, uiLen );
   ptr = szPath;

   if( strncmp( ptr, "mem:", 4 ) )
   {
      if( ( ptr2 = strchr( ptr, ':' ) ) != NULL )
      {
         ptr = ptr2 + 1;
         if( *ptr == DEF_SEP )
            ptr++;
      }
   }
   else
      return HB_FALSE;  /* HbMemIO does not support DIR */

   if( bFilename && szPath[ uiLen - 1 ] != DEF_SEP )
   {
      if( ( ptr2 = strrchr( szPath, DEF_SEP ) ) != NULL )
         *ptr2 = '\0';    /* trailing filename */
      else
         return HB_FALSE;
   }

#if defined( __HARBOUR30__ )
   if( hb_fsDirExists( szPath ) )
      bRet = HB_TRUE;
#else
   if( hb_fileDirExists( szPath ) )
      bRet = HB_TRUE;
#endif

   if( ! bRet )
   {
      bRet = HB_TRUE;
      while( bRet )
      {
         ptr2 = strchr( ptr, DEF_SEP );
         if( ! ptr2 )
         {
            if( ptr - szPath >= uiLen )
               break;
            else
               ptr2 = szPath + uiLen;
         }
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
         if( ptr - szPath >= uiLen )
            break;
      }
   }

   return bRet;
}

static void leto_AddHandle( PUSERSTRU pUStru, HB_MAXINT nHandle )
{
   if( nHandle >= 0 )
   {
      if( ! pUStru->ulOpenHandles )
      {
         pUStru->ulOpenHandles = 1;
         pUStru->pOpenHandles = ( HB_MAXINT * ) hb_xgrab( sizeof( HB_MAXINT ) );
      }
      else
         pUStru->pOpenHandles = ( HB_MAXINT * ) hb_xrealloc( pUStru->pOpenHandles, sizeof( HB_MAXINT ) * ++pUStru->ulOpenHandles );
      pUStru->pOpenHandles[ pUStru->ulOpenHandles - 1 ] = nHandle;
   }
}

static HB_BOOL leto_DelHandle( PUSERSTRU pUStru, HB_MAXINT nHandle )
{
   if( nHandle >= 0 && pUStru->ulOpenHandles )
   {
      HB_ULONG ulPos;

      for( ulPos = 0; ulPos < pUStru->ulOpenHandles; ulPos++ )
      {
         if( pUStru->pOpenHandles[ ulPos ] == nHandle )
         {
            if( ulPos + 1 < pUStru->ulOpenHandles )
            {
               memmove( pUStru->pOpenHandles + ulPos, pUStru->pOpenHandles + ulPos + 1,
                        sizeof( HB_MAXINT ) * pUStru->ulOpenHandles - ( ulPos - 1 ) );
            }
            if( --pUStru->ulOpenHandles )
               pUStru->pOpenHandles = ( HB_MAXINT * ) hb_xrealloc( pUStru->pOpenHandles, sizeof( HB_MAXINT ) * pUStru->ulOpenHandles );
            else
            {
               hb_xfree( pUStru->pOpenHandles );
               pUStru->pOpenHandles = NULL;
            }

            return HB_TRUE;
         }
      }
   }

   return HB_FALSE;
}

/* leto_udf() */
HB_FUNC( LETO_FOPEN )
{
   PUSERSTRU pUStru = letoGetUStru();

   if( s_bFileFunc && hb_parclen( 1 ) > 0 )
   {
      char      szPath[ HB_PATH_MAX ];
      HB_MAXINT nHandle;

      leto_DataPath( hb_parc( 1 ), szPath );
      nHandle = ( HB_NHANDLE ) hb_fsOpen( szPath, ( HB_USHORT ) hb_parnidef( 2, FO_READ | FO_COMPAT ) );
      leto_AddHandle( pUStru, nHandle );
      hb_retnint( nHandle );
      hb_fsSetFError( hb_fsError() );
   }
   else
   {
      hb_retni( -1 );
      hb_fsSetFError( 0 );
   }
}

/* leto_udf() */
HB_FUNC( LETO_FCREATE )
{
   PUSERSTRU pUStru = letoGetUStru();

   if( s_bFileFunc && hb_parclen( 1 ) > 0 )
   {
      char      szPath[ HB_PATH_MAX ];
      HB_MAXINT nHandle;

      leto_DataPath( hb_parc( 1 ), szPath );
      nHandle = ( HB_NHANDLE ) hb_fsCreate( szPath, hb_parnidef( 2, FC_NORMAL ) );
      leto_AddHandle( pUStru, nHandle );
      hb_retnint( nHandle );
      hb_fsSetFError( hb_fsError() );
   }
   else
   {
      hb_retni( -1 );
      hb_fsSetFError( 0 );
   }
}

/* leto_udf() */
HB_FUNC( LETO_FCLOSE )
{
   PUSERSTRU pUStru = letoGetUStru();

   if( s_bFileFunc && HB_ISNUM( 1 ) && hb_parni( 1 ) >= 0 )
   {
      HB_MAXINT  nHandle = hb_parnint( 1 );
      HB_ERRCODE uiError;

      if( leto_DelHandle( pUStru, nHandle ) )
      {
         hb_fsClose( hb_numToHandle( nHandle ) );
         uiError = hb_fsError();
      }
      else
         uiError = 2;
      hb_retl( uiError == 0 );
      hb_fsSetFError( uiError );
   }
   else
   {
      hb_retl( HB_FALSE );
      hb_fsSetFError( 0 );
   }
}

/* leto_udf() */
HB_FUNC( LETO_FERASE )
{
   if( s_bFileFunc && hb_parclen( 1 ) > 0 )
   {
      char szSrc[ HB_PATH_MAX ];

      leto_DataPath( hb_parc( 1 ), szSrc );
      hb_retni( hb_fsDelete( szSrc ) ? 0 : -1 );
      hb_fsSetFError( hb_fsError() );
   }
   else
   {
      hb_retni( -1 );
      hb_fsSetFError( 3 );
   }
}

/* leto_udf() */
HB_FUNC( LETO_FRENAME )
{
   if( s_bFileFunc && hb_parclen( 1 ) > 0 )
   {
      char szSrc[ HB_PATH_MAX ];
      char szDst[ HB_PATH_MAX ];

      leto_DataPath( hb_parc( 1 ), szSrc );
      leto_DataPath( hb_parc( 2 ), szDst );
      hb_retni( hb_fsRename( szSrc, szDst ) ? 0 : -1 );
      hb_fsSetFError( hb_fsError() );
   }
   else
   {
      hb_retni( -1 );
      hb_fsSetFError( 2 );
   }
}


static HB_BOOL leto_FilePathChk( const char * szPath )
{
   const char * ptr;

   /* need the ':' for "mem:hb_memio file and want to allow *ONE* '..' for elch */
   if( ( ptr = strchr( szPath, ':' ) ) != NULL && ( ptr - szPath < 3 || ptr - szPath > 5 ) )  /* 'C:\... */
      return HB_FALSE;
   else if( ( ptr = strstr( szPath, ".." ) ) != NULL && strstr( ptr + 1, ".." ) )
      return HB_FALSE;

   return HB_TRUE;
}

static void leto_FileFunc( PUSERSTRU pUStru, char * szData )
{
   char * pSrcFile, * pp2, * pp3 = NULL;
   char   szData1[ 16 ];
   char   szFile[ HB_PATH_MAX ];
   int    nParam = leto_GetParam( szData, &pSrcFile, &pp2, &pp3, NULL );

   if( ! s_bFileFunc )
      leto_SendAnswer( pUStru, "+F;100;", 7 );
   else if( nParam < 2 || *( szData + 2 ) != '\0' || ! *pSrcFile || ! leto_FilePathChk( pSrcFile ) )
      leto_SendAnswer( pUStru, "+F;1;", 5 );
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
#if defined( __HARBOUR30__ )
               if( hb_fsFileExists( szFile ) )
#else
               if( hb_fileExists( szFile, NULL ) )
#endif
                  strcpy( szData1, "+T;0;" );
               else
                  strcpy( szData1, "+F;2;" );
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
                  strcpy( szData1, "+F;1;" );
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
                  HB_ULONG ulLenOrg = ulLen;

                  ulLen = leto_CryptText( pUStru, pBuffer, ulLen, 3 );
                  hb_xfree( pBuffer );
                  if( ulLen == 4 && ulLenOrg )
                  {
                     pBuffer = NULL;
                     strcpy( szData1, "+F;1;" );
                  }
                  else
                  {
                     pBuffer = ( char * ) pUStru->pBufCrypt;
                     memcpy( pBuffer, "+T;", 3 );
                     ulLen += 3;
                  }
               }
               else
                  strcpy( szData1, "+F;2;" );
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
               strcpy( szData1, "+F;1;" );
               break;
         }
      }
      else if( *szData == '1' )
      {
         switch( *( szData + 1 ) )
         {
            case '0':  /* FileRead */
               if( nParam < 4 )
                  strcpy( szData1, "+F;1;" );
               else
               {
                  HB_ULONG ulStart = strtoul( pp2, NULL, 10 );
                  HB_BOOL  bFound;

                  ulLen = strtoul( pp3, NULL, 10 );
                  bFound = ulLen ? HB_TRUE : HB_FALSE;
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
                           {
                              ulLen = hb_arrayGetNL( pFileArr, 2 );
                              bFound = HB_TRUE;
                           }
                           hb_itemRelease( pFileArr );
                        }
                        hb_itemRelease( pArray );
                     }
#endif
                  }
                  if( bFound )
                  {
                     pBuffer = ( char * ) hb_xgrab( ulLen + 1 );
                     if( leto_fileread( szFile, pBuffer, ulStart, &ulLen ) )
                     {
                        HB_ULONG ulLenOrg = ulLen;

                        ulLen = leto_CryptText( pUStru, pBuffer, ulLen, 3 );
                        hb_xfree( pBuffer );
                        if( ulLen == 4 && ulLenOrg )
                        {
                           pBuffer = NULL;
                           if( hb_fsError() )
                              sprintf( szData1, "+F;%d;", hb_fsError() );
                           else
                              strcpy( szData1, "+F;1;" );
                        }
                        else
                        {
                           pBuffer = ( char * ) pUStru->pBufCrypt;
                           memcpy( pBuffer, "+T;", 3 );
                           ulLen += 3;
                        }
                     }
                     else
                     {
                        hb_xfree( pBuffer );
                        pBuffer = NULL;
                        if( hb_fsError() != 2 )
                           sprintf( szData1, "+F;%d;", hb_fsError() );
                        else
                           strcpy( szData1, "+F;6;" );
                     }
                  }
                  else
                     strcpy( szData1, "+F;2;" );
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
#if defined( __HARBOUR30__ )
               strcpy( szData1, "+F;1;" );
#else
               if( nParam < 3 )
                  strcpy( szData1, "+F;1;" );
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

                     if( ! ulLen || hb_fileAttrGet( szFile, &ulAttr ) )
                     {
                        if( ! ulLen || ( ulAttr & HB_FA_DIRECTORY ) )
                        {
#if defined( HB_OS_WIN )
                           strcpy( szFile + ulLen, "\\*.*" );
                           ulLen += 4;
#else
                           strcpy( szFile + ulLen, "/*" );
                           ulLen += 2;
#endif
                        }
                     }
                  }

                  if( ulLen )
                     pDir = hb_fileDirectory( szFile, pp2 );
                  if( ! pDir )
                     pDir = hb_itemArrayNew( 0 );

                  pParam = hb_itemSerialize( pDir, HB_SERIALIZE_NUMSIZE, &nSize );
                  ulLen = leto_CryptText( pUStru, pParam, nSize, 3 ) + 3;
                  pBuffer = ( char * ) pUStru->pBufCrypt;
                  memcpy( pBuffer, "+T;", 3 );
                  hb_itemRelease( pDir );
                  if( pParam )
                     hb_xfree( pParam );
               }
               break;
#endif  /* ! __HARBOUR30__ */

            case '3':  /* memowrite */
               if( nParam < 4 )
                  strcpy( szData1, "+F;1;" );
               else
               {
                  char *       ptr;
                  const char * pBuf;

                  ulLen = strtoul( pp3, &ptr, 10 );
                  ptr++;

                  pBuf = leto_DecryptText( pUStru, &ulLen, ptr );
                  if( leto_memowrite( szFile, pBuf, ulLen ) )
                     strcpy( szData1, "+T;0;" );
                  else
                     sprintf( szData1, "+F;%d;", hb_fsError() );
               }
               break;

            case '4':  /* FileWrite */
               if( nParam < 3 )
                  strcpy( szData1, "+F;1;" );
               else
               {
                  HB_ULONG     ulStart;
                  char *       ptr;
                  const char * pBuf;

                  ulStart = strtoul( pp2, &ptr, 10 );
                  ptr++;

                  pBuf = leto_DecryptText( pUStru, &ulLen, ptr );
                  if( leto_filewrite( szFile, pBuf, ulStart, ulLen, HB_FALSE ) )
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
               if( nParam < 3 || ! pp2 )
                  strcpy( szData1, "+F;1;" );
               else
               {
                  HB_BOOL fResult;
                  char    szDest[ HB_PATH_MAX ];

                  leto_DataPath( pp2, szDest );
#if defined( __HARBOUR30__ )
                  fResult = hb_fsCopy( szFile, szDest );
#else
                  fResult = hb_fileCopy( szFile, szDest );
#endif
                  if( fResult )
                     strcpy( szData1, "+T;0;" );
                  else
                     sprintf( szData1, "+F;%d;", hb_fsError() );
               }
               break;
            }

            default:
               strcpy( szData1, "+F;1;" );
               break;
         }
      }
      else
         strcpy( szData1, "+F;1;" );

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
         dbLockInfo.fResult = HB_FALSE;
         iTimeOut++;
         do
         {
            SELF_LOCK( pArea, &dbLockInfo );
            if( dbLockInfo.fResult || iTimeOut < 21 )
               break;
            else
            {
               hb_threadReleaseCPU();
               iTimeOut -= 20;
               if( iTimeOut > 100 )
               {
                  hb_threadReleaseCPU();
                  iTimeOut -= 20;
               }
            }
         }
         while( iTimeOut > 0 );

         bWasLocked = ! dbLockInfo.fResult;
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

#ifndef __HARBOUR30__
static HB_BOOL leto_dbfCopy( PHB_FILE pSrcFile, const char * pszDest, HB_NHANDLE hSrcFile )
{
   HB_ERRCODE errCode;
   HB_BOOL    bRetVal;
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

      HB_GC_LOCKT();
      hb_xvmSeqBegin();

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

            if( ! bDryRun )
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

               if( ( ! s_bNoSaveWA && ! s_bShareTables ) || pTStru->bMemIO )
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

                  SELF_GOCOLD( ( AREAP ) pArea );
                  if( leto_DirMake( szTarget, HB_TRUE ) && leto_dbfCopy( pArea->pDataFile, szTarget, 0 ) )
                  {
                     HB_FATTR  ulAttr;
                     HB_USHORT ui;

                     if( hb_fileAttrGet( pArea->szDataFileName, &ulAttr ) )
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

      hb_xvmSeqEnd();
      HB_GC_UNLOCKT();

      if( pUStru->iHbError )
         bOk = HB_FALSE;
      if( bDeleted )
      {
         PHB_ITEM pItem = hb_itemNew( NULL );

         hb_itemPutL( pItem, HB_TRUE );
         hb_setSetItem( HB_SET_DELETED, pItem );
         hb_itemRelease( pItem );
      }
      hb_itemRelease( pTableFiles );
      hb_itemRelease( pIndexFiles );

      hb_itemReturnRelease( pTblsActive );
   }

   return bOk;
}

HB_FUNC( LETO_BACKUPTABLES )
{
   PUSERSTRU pUStru = letoGetUStru();

   leto_BackupTable( pUStru, hb_parc( 1 ), hb_parl( 2 ) );
}
#endif  /* ! __HARBOUR30__ */

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
   HB_BOOL   bRet   = HB_FALSE;
   PUSERSTRU pUStru = letoGetUStru();

   if( pUStru->pCurAStru )
   {
      PAREASTRU pAStru = pUStru->pCurAStru;

      if( pAStru )
      {
         bRet = ! pAStru->pTStru->bShared || pAStru->bLocked ? HB_TRUE : HB_FALSE;
         leto_TableUnlock( pAStru, HB_FALSE, NULL );  /* also record locks */
         pAStru->pTStru->ulFlags = 0;
      }
   }
   hb_retl( bRet );
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

         if( ! pAStru->bLocked )
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
   HB_BOOL   bRet   = HB_FALSE;
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
      {
         bRet = leto_IsRecLocked( pUStru->pCurAStru, ulRecNo );
         leto_RecUnlock( pUStru->pCurAStru, ulRecNo );
      }
   }
   hb_retl( bRet );
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

static void leto_Lock( PUSERSTRU pUStru, char * szData )
{
   AREAP    pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   char *   pTimeOut = NULL;
   HB_ULONG ulRecNo = strtoul( szData + 2, &pTimeOut, 10 );

   if( ulRecNo ? ( leto_GotoIf( pArea, ulRecNo ) == HB_SUCCESS ) : HB_TRUE )
   {
      PAREASTRU pAStru = pUStru->pCurAStru;
      HB_ULONG  ulLen;
      char *    szData1;

      if( *szData == 'r' )  /* reclock */
      {
         if( ulRecNo && leto_RecLock( pUStru, pAStru, ulRecNo, HB_FALSE,
                                      ( s_bNoSaveWA && pTimeOut && *pTimeOut++ ) ? atoi( pTimeOut ) : 0 ) )
         {
            szData1 = leto_recWithAlloc( pArea, pUStru, pAStru, &ulLen );
            if( szData1 )
            {
               leto_SendAnswer( pUStru, szData1, ulLen );
               hb_xfree( szData1 );
            }
            else
               leto_SendAnswer( pUStru, szErr1, 4 );
         }
         else
         {
            leto_SendAnswer( pUStru, szErr4, 4 );
            if( ! ulRecNo )
               leto_wUsLog( pUStru, 0, "ERROR leto_Lock() missing RecNo! for Rlock" );
         }
      }
      else /* 'f' == filelock */
      {
         if( leto_IsServerLock( pUStru ) )
            leto_SendAnswer( pUStru, szErr2, 4 );
         else if( ! leto_TableLock( pAStru, ( s_bNoSaveWA && pTimeOut && *pTimeOut++ ) ? atoi( pTimeOut ) : 0 ) )
            leto_SendAnswer( pUStru, szErr4, 4 );
         else if( ulRecNo )
         {
            szData1 = leto_recWithAlloc( pArea, pUStru, pAStru, &ulLen );
            if( szData1 )
            {
               leto_SendAnswer( pUStru, szData1, ulLen );
               hb_xfree( szData1 );
            }
            else
               leto_SendAnswer( pUStru, szErr1, 4 );
         }
         else
            leto_SendAnswer( pUStru, szOk, 4 );
      }
   }
   else
      leto_SendAnswer( pUStru, szErr1, 4 );
}

static void leto_Unlock( PUSERSTRU pUStru, char * szData )
{
   HB_ULONG ulRecNo = 0;

   if( *szData == 'r' && ( ulRecNo = strtoul( szData + ( *( szData + 1 ) == ';' ? 2 : 1 ), NULL, 10 ) ) == 0 )
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

/* ToDo a bad admin can jump in the way of a running transaction and lock the server */
static int leto_UpdateRecord( PUSERSTRU pUStru, const char * szData, HB_BOOL bAppend, HB_ULONG * pRecNo, TRANSACTSTRU * pTA, AREAP pArea )
{
   PAREASTRU    pAStru = pUStru->pCurAStru;
   int          iRes = 0;
   char *       ptrPar;
   HB_ULONG     ulRecNo = strtoul( szData, &ptrPar, 10 );  /* record number or bUnlockAll ( for append ) */

   if( *ptrPar != ';' )
      iRes = 2;
   else
   {
      HB_USHORT uiUpd = ( HB_USHORT ) strtoul( ++ptrPar, &ptrPar, 10 );  /* number of updated fields */

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
         if( ! pTA )  /* transactions locks already ensured by client */
         {
            if( ulRecNo && pAStru->pTStru->bShared &&
                ! ( pAStru->bLocked || leto_IsRecLocked( pAStru, ulRecNo ) ) )
            {
               /*  table opened in shared mode, but the record/ table is not locked */
               hb_xvmSeqEnd();
               return 201;
            }
            else
               leto_GotoIf( pArea, ulRecNo );
         }
         else
            pTA->ulRecNo = ulRecNo;
      }

      if( *( ++ptrPar ) != '0' )  /* bDelete || bRecall */
      {
         HB_BOOL bDelete = *ptrPar == '1' ? HB_TRUE : HB_FALSE;
         HB_BOOL bRecall = *ptrPar == '2' ? HB_TRUE : HB_FALSE;

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
         const char * ptr = ptrPar + 2;
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
               case HB_FT_AUTOINC:
               case HB_FT_ROWVER:
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
                     if( ! iRes )
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
                  if( ! iRes )
                     ptr += pField->uiLen;
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
                  }
                  else
                  {
                     hb_itemPutTDT( pItem, HB_GET_LE_INT32( ptr ), HB_GET_LE_INT32( ptr + 4 ) );
                     ptr += pField->uiLen;
                  }
                  break;

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

               default:  /* unhandled field type */
                  iRes = 3;
                  break;
            }

            if( iRes )
            {
               leto_wUsLog( pUStru, -1, "ERROR leto_UpdateRecord( %lu:%d ) put value invalid", ulRecNo, uiField );
               break;
            }

            if( ! pTA )
            {
               if( SELF_PUTVALUE( pArea, uiField, pItem ) != HB_SUCCESS )
               {
                  leto_wUsLog( pUStru, -1, "ERROR leto_UpdateRecord( %lu:%d ) put value failed", ulRecNo, uiField );
                  iRes = 102;
                  break;
               }
            }
            else
            {
               pTA->puiIndex[ i ] = uiField;
               pTA->pItems[ i ] = hb_itemNew( pItem );
            }
         }

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
   else
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
                     szData1[ 0 ] = '+';
                     ulLen = ultostr( ulRecNo, szData1 + 1 ) + 1;
                     szData1[ ulLen++ ] = ';';
                     szData1[ ulLen ] = '\0';
                     pData = szData1;
                  }
               }
            }
            else
            {
               ulLen = 4;
               pData = szOk;
            }
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
   if( szData2 )
      hb_xfree( szData2 );
}

static void leto_Flush( PUSERSTRU pUStru, char * szData )
{
   AREAP   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL bOk;

   bOk = SELF_FLUSH( pArea ) == HB_SUCCESS;
   if( bOk )
      SELF_GOCOLD( pArea );
   pUStru->pCurAStru->bUseSkipBuffer = HB_FALSE;

   if( szData )  /* else a flush done after append/ update */
      leto_SendAnswer2( pUStru, szOk, 4, bOk, 1000 );
}

static void leto_UpdateRecAdd( PUSERSTRU pUStru, char * szData )
{
   leto_UpdateRec( pUStru, szData, HB_TRUE );
}

static void leto_UpdateRecAddflush( PUSERSTRU pUStru, char * szData )
{
   leto_UpdateRec( pUStru, szData, HB_TRUE );
   leto_Flush( pUStru, NULL );
}

static void leto_UpdateRecUpd( PUSERSTRU pUStru, char * szData )
{
   leto_UpdateRec( pUStru, szData, HB_FALSE );
}

static void leto_UpdateRecUpdflush( PUSERSTRU pUStru, char * szData )
{
   leto_UpdateRec( pUStru, szData, HB_FALSE );
   if( *szData == ' ' || *szData == '0' )
   {
      if( *szData == '0' )
         *szData = 'f';
      else
         *szData = 'r';
      leto_Unlock( pUStru, szData );
   }
   else
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

         memcpy( szBuffer, ptr, iKeyLen );
         szBuffer[ iKeyLen ] = '\0';
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
         if( iKeyLen )
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

/* leto_udf()  Leto_FTS( [ cSearch[, lCaseInsensitive, [ lNoMemos ] ] ] ) */
HB_FUNC( LETO_FTS )  /* Full Text Search in all fields ( default plus memos ) */
{
   DBFAREAP  pDbfArea = ( DBFAREAP ) hb_rddGetCurrentWorkAreaPointer();
   AREAP     pArea = pDbfArea ? &pDbfArea->area : NULL;
   HB_SIZE   nPos = 0;
   HB_USHORT uiRecordLen = 0, uiSearchLen = ( HB_USHORT ) hb_parclen( 1 );
   HB_BOOL   bCaseI;
   HB_BYTE * pRecord = NULL;

   if( pArea && ! pArea->fEof )
   {
      if( SELF_GETREC( pArea, &pRecord ) == HB_SUCCESS )
      {
         uiRecordLen = pDbfArea->uiRecordLen;
         if( uiRecordLen && uiSearchLen )
         {
            const char * szSearch = hb_parc( 1 );

            bCaseI = HB_ISLOG( 2 ) && hb_parl( 2 );
            if( bCaseI )
               nPos = hb_strAtI( szSearch, uiSearchLen, ( char * ) pRecord, uiRecordLen );
            else
               nPos = hb_strAt( szSearch, uiSearchLen, ( char * ) pRecord, uiRecordLen );

            if( ! nPos && pDbfArea->fHasMemo && ! ( HB_ISLOG( 3 ) && hb_parl( 3 ) ) )
            {
               HB_USHORT uiCount = pArea->uiFieldExtent;
               LPFIELD   pField;
               PHB_ITEM  pItem = hb_itemNew( NULL );
               int       iLen;

               while( uiCount-- )
               {
                  pField = pArea->lpFields + uiCount;
                  /* as Harbour does not know a HB_FF_EXTERN ... ;-) */
                  if( pField->uiType == HB_FT_MEMO || pField->uiType == HB_FT_BLOB ||
                      pField->uiType == HB_FT_IMAGE || pField->uiType == HB_FT_OLE ||
                      ( pField->uiType == HB_FT_ANY && pField->uiLen >= 6 ) )
                  {
                     SELF_GETVALUE( pArea, uiCount + 1, pItem );
                     if( ( iLen = hb_itemGetCLen( pItem ) ) > 0 )
                     {
                        if( bCaseI )
                            nPos = hb_strAtI( szSearch, uiSearchLen, hb_itemGetCPtr( pItem ), iLen );
                        else
                            nPos = hb_strAt( szSearch, uiSearchLen, hb_itemGetCPtr( pItem ), iLen );
                     }
                  }
               }
               hb_itemRelease( pItem );
            }
         }
      }
   }

   if( ! HB_ISCHAR( 1 ) )  /* raw record for extern process */
      hb_retclen( ( char * ) pRecord, uiRecordLen );
   else
      hb_retl( nPos != 0 );
}

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

static void leto_Seek( PUSERSTRU pUStru, char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PAREASTRU    pAStru = pUStru->pCurAStru;
   char *       szData1 = NULL;
   const char * pData;
   const char * pOrdKey;
   HB_ULONG     ulLen = 4;
   HB_BOOL      bSoftSeek, bFindLast;
   int          iKeyLen;

   if( strlen( szData ) < 2 || ! pArea )
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
            HB_BOOL bMutex = ( ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) && pAStru->itmFltExpr );

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

static void leto_Scope( PUSERSTRU pUStru, char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PAREASTRU    pAStru = pUStru->pCurAStru;
   char *       pOrder;
   const char * szKey, * pData;

   if( ! pArea || leto_GetParam( szData, &pOrder, /* szKey */ NULL ) < 2 )
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

static void leto_Skip( PUSERSTRU pUStru, char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PAREASTRU    pAStru = pUStru->pCurAStru;
   char *       szData1 = NULL, * ptr;
   const char * pData = NULL;
   HB_ULONG     ulRecNo, ulLen = 4;
   HB_LONG      lSkip = strtol( szData, &ptr, 10 );
   HB_BOOL      bMutex, bHotBuffer;

   if( *ptr != ';' )
      pData = szErr2;
   else
   {
      ulRecNo = strtoul( ++ptr, &ptr, 10 );
      ptr++;
      if( pUStru->bDeleted != ( *ptr & 0x01 ) )
      {
         pUStru->bDeleted = ( *ptr & 0x01 );
         leto_setSetDeleted( pUStru->bDeleted );
      }
      ptr += 2;
      bHotBuffer = *ptr == 'T';
      bMutex = ( ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) && pAStru->itmFltExpr );

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
      if( SELF_SKIP( pArea, lSkip ) == HB_SUCCESS )
      {
         HB_ULONG ulLenAll, ulRelPos = 0;

         szData1 = ( char * ) hb_xgrab( ( leto_recLen( pAStru->pTStru ) * pAStru->uiSkipBuf ) + 1 );
         ulLenAll = leto_rec( pUStru, pAStru, pArea, szData1 + 1, &ulRelPos );
         if( ! ulLenAll )
            pData = szErr2;
         else if( ! pAStru->bUseSkipBuffer )
            pAStru->bUseSkipBuffer = HB_TRUE;
         else if( lSkip && bHotBuffer )
         {
            HB_USHORT i = 1;
            HB_ULONG  ulLenRec;

            ulRelPos += lSkip;
            while( i++ < pAStru->uiSkipBuf )
            {
               if( lSkip > 0 )
               {
                  if( ( ( DBFAREAP ) pArea )->area.fEof )
                     break;
               }
               else
               {
                  if( ( ( DBFAREAP ) pArea )->area.fBof )
                     break;
               }

               if( SELF_SKIP( pArea, lSkip ) == HB_SUCCESS )
               {
                  if( lSkip < 0 )
                  {
                     if( ( ( DBFAREAP ) pArea )->area.fBof )
                        break;
                  }
                  ulLenRec = leto_rec( pUStru, pAStru, pArea, szData1 + 1 + ulLenAll, &ulRelPos );
                  if( ! ulLenRec )
                  {
                     pData = szErr2;
                     break;
                  }
                  ulRelPos += lSkip;
                  ulLenAll += ulLenRec;
               }
               else
               {
                  pData = szErr101;
                  break;
               }
            }
#if 0  /* re-sync with client ? */
            if( s_bNoSaveWA )
               leto_GotoIf( pArea, ulRecNo );
#endif
         }

         if( pData == NULL )  /* success */
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

#if 0
      if( ulLen == 4 )
         leto_wUsLog( pUStru, -1, "ERROR leto_Skip! %d failed, EOF %d, recno %lu, filter %d", ( int ) lSkip, pArea->fEof,
                      ulRecNo, pArea->dbfi.itmCobExpr ? ( int ) hb_itemGetL( hb_vmEvalBlock( pArea->dbfi.itmCobExpr ) ) : -1 );
#endif

      if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
         leto_ClearAreaEnv( pArea, pAStru->pTagCurrent );
   }

   leto_SendAnswer( pUStru, pData, ulLen );
   if( szData1 )
      hb_xfree( szData1 );
}

static PHB_ITEM leto_mkCodeBlock( PUSERSTRU pUStru, const char * szExp, HB_ULONG ulLen, HB_BOOL bSecured )
{
   PHB_ITEM pBlock = NULL;

   if( ulLen > 0 )
   {
      char *   szMacro = ( char * ) hb_xgrab( ulLen + 5 );
      char *   szFree = NULL;
      PHB_ITEM pFreshBlock;

      if( szExp[ 0 ] == '{' && szExp[ ulLen - 1 ] == '}' )
         memcpy( szMacro, szExp, ulLen );
      else
      {
         szMacro[ 0 ] = '{';
         szMacro[ 1 ] = '|';
         szMacro[ 2 ] = '|';
         memcpy( szMacro + 3, szExp, ulLen );
         szMacro[ 3 + ulLen ] = '}';
         ulLen += 4;
      }
      szMacro[ ulLen ] = '\0';
      if( strstr( szMacro, "->" ) )
      {
         szFree = szMacro;
         szMacro = leto_AliasTranslate( pUStru, szMacro, &ulLen );
      }

      if( bSecured )
         hb_xvmSeqBegin();

      pBlock = hb_itemNew( NULL );
      hb_vmPushString( szMacro, ulLen );

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
               hb_itemRelease( pBlock );
               pBlock = NULL;
            }
         }
      }
      hb_xfree( szMacro );
      if( szFree )
         hb_xfree( szFree );
   }

   return pBlock;
}

static HB_BOOL leto_dbEvalJoinRel( AREAP pArea, AREAP pJoined )
{
   HB_BOOL bForbidden = HB_FALSE;
   LPDBRELINFO lpDbRel = pArea->lpdbRelations;

   while( lpDbRel )
   {
      if( lpDbRel->lpaChild == pJoined || leto_dbEvalJoinRel( lpDbRel->lpaChild, pJoined ) )
      {
         bForbidden = HB_TRUE;
         break;
      }
      lpDbRel = lpDbRel->lpdbriNext;
   }

   return bForbidden;
}

static PHB_ITEM leto_dbEvalJoinAdd( PUSERSTRU pUStru, const char * ptr, AREAP pArea )
{
   char         szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ] = { 0 };
   char *       pAlias;
   const char * ptr1, * ptr2;
   int          iArea;
   PHB_ITEM     pOne, pBlock, pJoins = hb_itemArrayNew( 0 );
   HB_BOOL      bNeedKey, bValid = HB_TRUE;
   DBORDERINFO  pOrderInfo;
   HB_SIZE      nPos;
   HB_ULONG     ulLen;

   while( bValid && ptr && *ptr && *ptr != ';' )
   {
      ptr1 = strchr( ptr, ',' );
      if( ptr1 )
         ptr2 = strchr( ++ptr1, ',' );
      else
         ptr2 = NULL;
      if( ! ptr1 || ! ptr2 )
         break;
      if( ptr1 - ptr > 1 )
      {
         bNeedKey = HB_TRUE;
         pOne = hb_itemArrayNew( 5 );
         if( hb_strAtI( "CROSS", 5, ptr, ptr1 - ptr - 1 ) )
         {
            hb_arraySetC( pOne, 1, "CROSS" );
            bNeedKey = HB_FALSE;
         }
         else if( hb_strAtI( "INNER", 5, ptr, ptr1 - ptr - 1 ) )
            hb_arraySetC( pOne, 1, "INNER" );
         else if( hb_strAtI( "LEFT", 4, ptr, ptr1 - ptr - 1 ) )
            hb_arraySetC( pOne, 1, "LEFT" );
         else if( hb_strAtI( "RIGHT", 5, ptr, ptr1 - ptr - 1 ) )
            hb_arraySetC( pOne, 1, "RIGHT" );
         else
         {
            if( s_iDebugMode > 0 )
               leto_wUsLog( pUStru, -1, "ERROR leto_dbEvalJoinAdd() unknown JOIN type: %.*s", ptr1 - ptr - 1, ptr );
            hb_itemRelease( pOne );
            ptr = strchr( ptr2 + 1, ';' );
            if( ptr )
               ptr++;
            bValid = HB_FALSE;
            continue;
         }

         while( *ptr1 == ' ' )
         {
            ptr1++;
         }
         ulLen = ( HB_ULONG ) ( ptr2 - ptr1 );
         memcpy( szAlias, ptr1, HB_MIN( ulLen, HB_RDD_MAX_ALIAS_LEN ) );
         strcpy( szAlias + HB_MIN( ulLen, HB_RDD_MAX_ALIAS_LEN - 2 ), "->" );
         ulLen += 2;
         pAlias = leto_AliasTranslate( pUStru, szAlias, &ulLen );
         memcpy( szAlias, pAlias, ulLen - 2 );
         szAlias[ ulLen - 2 ] = '\0';
         hb_xfree( pAlias );
         hb_rddGetAliasNumber( szAlias, &iArea );
         if( iArea )
         {
            hb_arraySetPtr( pOne, 2, hb_rddGetWorkAreaPointer( ( HB_AREANO ) iArea ) );
            nPos = hb_arrayLen( pJoins );
            while( nPos )  /* one JOIN per WA allowed */
            {
               if( hb_arrayGetItemPtr( hb_arrayGetItemPtr( pJoins, nPos ), 2 ) == hb_arrayGetItemPtr( pOne, 2 ) )
               {
                  if( s_iDebugMode > 0 )
                     leto_wUsLog( pUStru, -1, "ERROR leto_dbEvalJoinAdd() double used WA %s for a %.*s JOIN: %.*s", szAlias, ptr1 - ptr - 1, ptr );
                  bValid = HB_FALSE;
                  iArea = 0;
                  break;
               }
               nPos--;
            }
         }

         ptr = strchr( ptr2 + 1, ';' );
         if( ptr && iArea )
         {
            if( s_iDebugMode >= 15 )
               leto_wUsLog( pUStru, -1,"DEBUG leto_dbEvalJoinAdd() join %s on '%.*s' to (%s)",
                            hb_arrayGetCPtr( pOne, 1 ), ! bNeedKey ? 0 : ptr - ptr2 - 1, ptr2 + 1, szAlias );

            if( ptr - ptr2 - 1 > 0 && bNeedKey )
            {
               ulLen = ( HB_ULONG ) ( ptr - ptr2 - 1 );
               pAlias = leto_AliasTranslate( pUStru, ptr2 + 1, &ulLen );
               pBlock = leto_mkCodeBlock( pUStru, pAlias, ulLen, HB_FALSE );

               if( ( hb_itemType( hb_vmEvalBlock( pBlock ) ) & HB_IT_LOGICAL ) )
                  hb_arraySetL( pOne, 4, HB_TRUE );
               else
                  hb_arraySetL( pOne, 4, HB_FALSE );

               /* RIGHT JOIN: relation pArea -> pJoinArea not allowed */
               if( *( hb_arrayGetCPtr( pOne, 1 ) ) == 'R' )
               {
                  if( leto_dbEvalJoinRel( pArea, ( AREAP ) hb_arrayGetPtr( pOne, 2 ) ) )
                  {
                     if( s_iDebugMode > 0 )
                        leto_wUsLog( pUStru, -1, "ERROR leto_dbEvalJoinAdd() relation active for JOIN type: %s", hb_arrayGetCPtr( pOne, 1 ) );
                     bValid = HB_FALSE;
                  }
               }
               else  /* other JOIN types: relation pJoinArea -> pArea not allowed */
               {
                  if( leto_dbEvalJoinRel( ( AREAP ) hb_arrayGetPtr( pOne, 2 ), pArea ) )
                  {
                     if( s_iDebugMode > 0 )
                        leto_wUsLog( pUStru, -1, "ERROR leto_dbEvalJoinAdd() relation active for JOIN type: %s", hb_arrayGetCPtr( pOne, 1 ) );
                     bValid = HB_FALSE;
                  }
               }

               if( ! hb_arrayGetL( pOne, 4 ) )  /* check value type same as indexkey type */
               {
                  memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
                  if( bValid )
                  {
                     pOrderInfo.itmResult = hb_itemPutC( NULL, NULL );
                     if( *( hb_arrayGetCPtr( pOne, 1 ) ) == 'R' )
                        SELF_ORDINFO( pArea, DBOI_KEYTYPE, &pOrderInfo );
                     else
                        SELF_ORDINFO( ( AREAP ) hb_arrayGetPtr( pOne, 2 ), DBOI_KEYTYPE, &pOrderInfo );
                     if( ! hb_itemGetCLen( pOrderInfo.itmResult ) )
                        pOrderInfo.itmResult = hb_itemPutC( pOrderInfo.itmResult, "U" );
                  }
                  else
                     pOrderInfo.itmResult = hb_itemPutC( pOrderInfo.itmResult, "X" );

                  if( *( hb_itemGetCPtr( pOrderInfo.itmResult ) ) == hb_itemTypeStr( hb_vmEvalBlock( pBlock ) )[ 0 ] &&
                      *( hb_itemGetCPtr( pOrderInfo.itmResult ) ) != 'U' )
                     hb_arraySet( pOne, 3, pBlock );
                  else
                  {
                     bValid = HB_FALSE;
                     if( s_iDebugMode > 0 )
                        leto_wUsLog( pUStru, -1, "ERROR leto_dbEvalJoinAdd() indexkey type '%c' <> value type '%c' for JOIN type: %s",
                                     *( hb_itemGetCPtr( pOrderInfo.itmResult ) ), hb_itemTypeStr( hb_vmEvalBlock( pBlock ) )[ 0 ], hb_arrayGetCPtr( pOne, 1 ) );
                  }
                  hb_itemRelease( pOrderInfo.itmResult );
               }
               else if( bValid )
                  hb_arraySet( pOne, 3, pBlock );

               hb_vmDestroyBlockOrMacro( pBlock );
               hb_xfree( pAlias );
            }
            else if( bNeedKey )
            {
               bValid = HB_FALSE;
               if( s_iDebugMode > 0 )
                  leto_wUsLog( pUStru, -1, "ERROR leto_dbEvalJoinAdd() missing key/ expression for JOIN type: %s", hb_arrayGetCPtr( pOne, 1 ) );
            }

            if( ! bNeedKey || ( hb_itemType( hb_arrayGetItemPtr( pOne, 3 ) ) & HB_IT_BLOCK ) )
               hb_arrayAdd( pJoins, pOne );
         }
         if( ptr )
            ptr++;
         hb_itemRelease( pOne );
      }
   }
   if( ! hb_arrayLen( pJoins ) || ! bValid )
   {
      if( ( bValid && s_iDebugMode > 20 ) || ( ! bValid && s_iDebugMode > 0 ) )
         leto_wUsLog( pUStru, -1, "%s leto_dbEvalJoinAdd() no valid JOINs for leto_dbEval", bValid ? "DEBUG" : "ERROR" );
      hb_itemRelease( pJoins );
      pJoins = NULL;
   }

   return pJoins;
}

static HB_BOOL leto_dbEvalJoinStart( PHB_ITEM pJoins )
{
   HB_BOOL  bResult = HB_FALSE, bEof = HB_FALSE;
   PHB_ITEM pJoin;
   AREAP    pJoinArea;
   HB_SIZE  nPos = 0;

   while( ++nPos <= hb_arrayLen( pJoins ) )
   {
      pJoin = hb_arrayGetItemPtr( pJoins, nPos );
      pJoinArea = ( AREAP ) hb_arrayGetPtr( pJoin, 2 );
      switch( *( hb_arrayGetCPtr( pJoin, 1 ) ) )
      {
         case 'C':  /* CROSS JOIN */
         case 'R':  /* RIGHT JOIN */
            SELF_GOTOP( pJoinArea );
            SELF_EOF( pJoinArea, &bEof );
            break;
      }
      if( bEof )
         break;
   }

   return bResult;
}

static HB_BOOL leto_dbEvalJoinSeekSkip( AREAP pArea, PHB_ITEM pEval )
{
   while( ! pArea->fEof )
   {
      if( hb_itemGetL( hb_vmEvalBlock( pEval ) ) )
         break;
      SELF_SKIP( pArea, 1 );
   }

   return pArea->fEof;
}

static HB_BOOL leto_dbEvalJoinFor( PHB_ITEM pJoins, AREAP pArea )
{
   HB_BOOL  bResult = HB_TRUE, bEof;
   PHB_ITEM pJoin;
   AREAP    pJoinArea;
   HB_SIZE  nPos = 0;
   HB_ULONG ulRecNo = 0;

   while( bResult && ++nPos <= hb_arrayLen( pJoins ) )
   {
      pJoin = hb_arrayGetItemPtr( pJoins, nPos );
      pJoinArea = ( AREAP ) hb_arrayGetPtr( pJoin, 2 );
      switch( *( hb_arrayGetCPtr( pJoin, 1 ) ) )
      {
         case 'I':
            if( ! hb_arrayGetType( pJoin, 5 ) )
            {
               if( hb_arrayGetL( pJoin, 4 ) )
               {
                  SELF_GOTOP( pJoinArea );
                  bResult = leto_dbEvalJoinSeekSkip( pJoinArea, hb_arrayGetItemPtr( pJoin, 3 ) );
               }
               else
               {
                  if( SELF_SEEK( pJoinArea, HB_FALSE, hb_vmEvalBlock( hb_arrayGetItemPtr( pJoin, 3 ) ), HB_FALSE ) == HB_SUCCESS )
                     SELF_FOUND( pJoinArea, &bResult );
                  else
                     bResult = HB_FALSE;
               }

               if( bResult )
               {
                  SELF_RECNO( pJoinArea, &ulRecNo );
                  hb_arraySetNL( pJoin, 5, ulRecNo );
               }
            }
            break;

         case 'L':
            if( ! hb_arrayGetType( pJoin, 5 ) )
            {
               if( hb_arrayGetL( pJoin, 4 ) )
               {
                  SELF_GOTOP( pJoinArea );
                  leto_dbEvalJoinSeekSkip( pJoinArea,  hb_arrayGetItemPtr( pJoin, 3 ) );
               }
               else
                  SELF_SEEK( pJoinArea, HB_FALSE, hb_vmEvalBlock( hb_arrayGetItemPtr( pJoin, 3 ) ), HB_FALSE );
               bResult = HB_TRUE;
            }
            break;

         case 'R':
            SELF_EOF( pJoinArea, &bEof );
            if( ! bEof )
            {
               if( hb_arrayGetL( pJoin, 4 ) )  /* condition block */
               {
                  SELF_GOTOP( pArea );
                  leto_dbEvalJoinSeekSkip( pArea,  hb_arrayGetItemPtr( pJoin, 3 ) );
               }
               else
                  SELF_SEEK( pArea, HB_FALSE, hb_vmEvalBlock( hb_arrayGetItemPtr( pJoin, 3 ) ), HB_FALSE );
               bResult = HB_TRUE;
            }
            else
               bResult = HB_FALSE;
            break;
      }
   }

   return bResult;
}

static HB_BOOL leto_dbEvalJoinPreSkip( PHB_ITEM pJoins, AREAP pArea )
{
   DBORDERINFO pOrderInfo;
   HB_BOOL  bEof, bResult = HB_FALSE;
   PHB_ITEM pJoin;
   AREAP    pJoinArea;
   HB_SIZE  nPos = 0;
   int      iResult;
   HB_ULONG ulRecNo;

   while( ! bResult && ++nPos <= hb_arrayLen( pJoins ) )
   {
      pJoin = hb_arrayGetItemPtr( pJoins, nPos );
      pJoinArea = ( AREAP ) hb_arrayGetPtr( pJoin, 2 );

      switch( *( hb_arrayGetCPtr( pJoin, 1 ) ) )
      {
         case 'C':  /* CROSS JOIN */
            SELF_SKIP( pJoinArea, 1 );
            SELF_EOF( pJoinArea, &bEof );
            if( ! bEof )
               bResult = HB_TRUE;
            else if( nPos < hb_arrayLen( pJoins ) )
               SELF_GOTOP( pJoinArea );
            break;

         case 'I':  /* INNER JOIN */
            if( hb_arrayGetType( pJoin, 5 ) )
            {
               SELF_SKIP( pJoinArea, 1 );
               SELF_EOF( pJoinArea, &bEof );
               if( ! bEof )
               {
                  if( hb_arrayGetL( pJoin, 4 ) )  /* condition block */
                     bResult = leto_dbEvalJoinSeekSkip( pJoinArea, hb_arrayGetItemPtr( pJoin, 3 ) );
                  else
                  {
                     memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
                     pOrderInfo.itmResult = hb_itemNew( NULL );
                     SELF_ORDINFO( pJoinArea, DBOI_KEYVAL, &pOrderInfo );
                     bResult = hb_itemCompare( pOrderInfo.itmResult, hb_vmEvalBlock( hb_arrayGetItemPtr( pJoin, 3 ) ), HB_TRUE, &iResult );
                     if( iResult )
                        bResult = HB_FALSE;
                     hb_itemRelease( pOrderInfo.itmResult );
                  }
               }
               else
                  bResult = HB_FALSE;

               if( ! bResult )
               {
                  hb_itemSetNil( hb_arrayGetItemPtr( pJoin, 5 ) );
                  if( ! bEof )
                     SELF_GOTO( pJoinArea, 0 );
               }
               else
               {
                  SELF_RECNO( pJoinArea, &ulRecNo );
                  hb_arraySetNL( pJoin, 5, ulRecNo );
               }
            }
            else
               bResult = HB_FALSE;
            break;

         case 'R':
            SELF_SKIP( pJoinArea, 1 );
            SELF_EOF( pJoinArea, &bEof );
            if( bEof )
            {
               SELF_GOTO( pArea, 0 );
               bResult = HB_FALSE;
            }
            else
               bResult = HB_TRUE;
            break;
      }
   }

   return bResult;
}

static HB_BOOL leto_dbEvalJoinPostSkip( PHB_ITEM pJoins )
{
   HB_BOOL  bResult = HB_TRUE;
   PHB_ITEM pJoin;
   AREAP    pJoinArea;
   HB_SIZE  nPos = 0;

   while( ++nPos <= hb_arrayLen( pJoins ) )
   {
      pJoin = hb_arrayGetItemPtr( pJoins, nPos );
      switch( *( hb_arrayGetCPtr( pJoin, 1 ) ) )
      {
         case 'C' :  /* CROSS JOIN */
            pJoinArea = ( AREAP ) hb_arrayGetPtr( pJoin, 2 );
            bResult = SELF_GOTOP( pJoinArea ) == HB_SUCCESS;
            break;
      }
   }

   return bResult;
}

static HB_ERRCODE leto_dbEval( PUSERSTRU pUStru, AREAP pArea, LPDBEVALINFO pEvalInfo, const int iLockTime, HB_BOOL bStay )
{
   DBLOCKINFO dbLockInfo;
   HB_ERRCODE errCode = HB_FAILURE;
   HB_BOOL    bValid = ( hb_itemType( pEvalInfo->itmBlock ) & HB_IT_BLOCK ) ? HB_TRUE : HB_FALSE;
   HB_BOOL    bProved = bValid ? HB_TRUE : HB_FALSE;
   HB_BOOL    bOneMore = HB_FALSE;
   PHB_ITEM   pProces = hb_itemPutNS( NULL, 0 );
   PHB_ITEM   pEvalut = hb_itemPutNS( NULL, 0 );
   PHB_ITEM   pRLocks = NULL;
   PHB_ITEM   pSaveValResult = NULL;
   HB_ULONG   ulLastRecNo, ulNextRecNo = 0;
   HB_LONG    lNext = -1;

   memset( &dbLockInfo, 0, sizeof( DBLOCKINFO ) );
   SELF_RECNO( pArea, &ulLastRecNo );

   if( bValid )
   {
      HB_BOOL bGoTop;

      if( pArea->valResult )  /* used if (optimized) filter block active */
      {
         pSaveValResult = hb_itemClone( pArea->valResult );
         hb_vmDestroyBlockOrMacro( pArea->valResult );
         pArea->valResult = NULL;
      }
      else
         pSaveValResult = hb_itemNew( NULL );

      if( pEvalInfo->dbsci.lNext && hb_itemGetNL( pEvalInfo->dbsci.lNext ) >= 0 )
         lNext = hb_itemGetNL( pEvalInfo->dbsci.lNext );

      bGoTop = ( ! pEvalInfo->dbsci.fRest || ! hb_itemGetL( pEvalInfo->dbsci.fRest ) );
      if( pUStru->bDbEvalCompat && ( pEvalInfo->dbsci.itmCobWhile || lNext >= 0 ) )
         bGoTop = HB_FALSE;

      if( pEvalInfo->dbsci.itmRecID )
         bValid = ( leto_GotoIf( pArea, hb_itemGetNL( pEvalInfo->dbsci.itmRecID ) ) == HB_SUCCESS );
      else if( bGoTop )
      {
         if( ! pEvalInfo->dbsci.fBackward )
            bValid = ( SELF_GOTOP( pArea ) == HB_SUCCESS );
         else
            bValid = ( SELF_GOBOTTOM( pArea ) == HB_SUCCESS );
      }

      if( lNext >= 0 )
      {
         if( ! lNext )
         {
            bValid = HB_FALSE;
            errCode = HB_SUCCESS;
         }
      }
      else
         lNext = 0;  /* with bValid == true --> infinite, default */
   }

   if( bValid && iLockTime >= 0 )  /* try to lock all records beforehand */
   {
      HB_ULONG ulNewRecNo, ulLockRecNo;
      HB_LONG  lNewNext = lNext;
      HB_BOOL  bEof;

      pRLocks = hb_itemArrayNew( 0 );
      /* use not default setting of bStay to set to initial try a F-lock */
      dbLockInfo.uiMethod = ! bStay ? DBLM_FILE : DBLM_MULTIPLE;
      SELF_RECNO( pArea, &ulNewRecNo );

      hb_xvmSeqBegin();

      SELF_EOF( pArea, &bEof );
      if( ! bEof && pEvalInfo->dbsci.fBackward )
         SELF_BOF( pArea, &bEof );
      if( ! bEof )
         hb_itemPutNS( pEvalut, 1 );

      if( ! bEof && pEvalInfo->dbsci.lpstrFor )
         bEof = leto_dbEvalJoinStart( pEvalInfo->dbsci.lpstrFor );
      if( ! bEof )
         hb_itemPutNS( pEvalut, 1 );

      while( ! bEof && ( ! pEvalInfo->dbsci.itmCobWhile || hb_itemGetL( hb_vmEvalBlockV( pEvalInfo->dbsci.itmCobWhile, 2, pProces, pEvalut ) ) ) )
      {
         if( pEvalInfo->dbsci.itmCobFor )  /* FOR */
            bValid = hb_itemGetL( hb_vmEvalBlockV( pEvalInfo->dbsci.itmCobFor, 2, pProces, pEvalut ) );
         if( bValid && pEvalInfo->dbsci.lpstrFor )
            bValid = leto_dbEvalJoinFor( pEvalInfo->dbsci.lpstrFor, pArea );
         hb_itemPutNS( pEvalut, hb_itemGetNS( pEvalut ) + 1 );
         if( bValid )
         {
            hb_itemPutNS( pProces, hb_itemGetNS( pProces ) + 1 );

            SELF_RECNO( pArea, &ulLockRecNo );
            dbLockInfo.itmRecID = hb_itemPutNL( dbLockInfo.itmRecID, ulLockRecNo );

            /* an initial Flock was successful, dbLockInfo.fResult is HB_TRUE */
            if( ! ( dbLockInfo.uiMethod == DBLM_FILE && hb_arrayLen( pRLocks ) ) )
            {
               do
               {
                  if( dbLockInfo.uiMethod == DBLM_FILE )
                  {
                     if( leto_TableLock( pUStru->pCurAStru, iLockTime ) )
                        dbLockInfo.fResult = HB_TRUE;
                     else
                        dbLockInfo.fResult = HB_FALSE;
                  }
                  else if( leto_RecLock( pUStru, pUStru->pCurAStru, ulLockRecNo, HB_FALSE, iLockTime ) )
                     dbLockInfo.fResult = HB_TRUE;
                  else
                     dbLockInfo.fResult = HB_FALSE;

                  if( ! dbLockInfo.fResult && dbLockInfo.uiMethod == DBLM_FILE )
                     dbLockInfo.uiMethod = DBLM_MULTIPLE;  /* second try as Rlock */
                  else
                     break;
               }
               while( HB_TRUE );
            }

            if( ! dbLockInfo.fResult )
            {
               bProved = HB_FALSE;
               break;
            }

            hb_arrayAdd( pRLocks, dbLockInfo.itmRecID );

            if( pEvalInfo->dbsci.lpstrFor )
               bOneMore = leto_dbEvalJoinPreSkip( pEvalInfo->dbsci.lpstrFor, pArea );
            if( ! bOneMore &&
                ( ( pEvalInfo->dbsci.itmRecID && hb_itemGetNL( pEvalInfo->dbsci.itmRecID ) ) || ( lNext && --lNext < 1 ) ) )
               break;
         }
         else if( pEvalInfo->dbsci.lpstrFor )
            bOneMore = leto_dbEvalJoinPreSkip( pEvalInfo->dbsci.lpstrFor, pArea );

         if( ! bOneMore )
         {
            SELF_SKIP( pArea, ! pEvalInfo->dbsci.fBackward ? 1 : -1 );
            if( ! pEvalInfo->dbsci.fBackward )
               SELF_EOF( pArea, &bEof );
            else
               SELF_BOF( pArea, &bEof );
         }

         if( ! bEof && ! bOneMore && pEvalInfo->dbsci.lpstrFor )
            leto_dbEvalJoinPostSkip( pEvalInfo->dbsci.lpstrFor );
      }

      hb_xvmSeqEnd();

      if( pUStru->iHbError )
      {
         if( s_iDebugMode > 1 )
            leto_wUsLog( pUStru, 0,"DEBUG leto_dbEval at least one expression (block/ for/ while) failed" );
         bProved = HB_FALSE;
      }
      else if( ! bProved && s_iDebugMode > 1 )
         leto_wUsLog( pUStru, -1,"DEBUG leto_dbEval failed to R-lock after %ld locks [ Timeout: %d ]",
                                 hb_itemGetNS( pProces ), iLockTime );
      lNext = lNewNext;
      bValid = HB_TRUE;
      SELF_RECNO( pArea, &ulNextRecNo );
      leto_GotoIf( pArea, ulNewRecNo );
      hb_itemRelease( dbLockInfo.itmRecID );
   }

   if( bValid && bProved )
   {
      PHB_ITEM pResult, pLast;
      HB_SIZE  nLen = 0;
      HB_BOOL  bEof, bAsArr;

      hb_itemPutNS( pProces, 0 );
      hb_itemPutNS( pEvalut, 0 );
      bOneMore = HB_FALSE;

      bAsArr = ( hb_itemType( pSaveValResult ) & HB_IT_ARRAY );
      if( bAsArr )
         nLen = hb_arrayLen( pSaveValResult );

      if( pRLocks )
      {
         if( hb_arrayLen( pRLocks ) )
         {
            SELF_GOTO( pArea, hb_arrayGetNL( pRLocks, 1 ) );
            bEof = HB_FALSE;
         }
         else
            bEof = HB_TRUE;
      }
      else
      {
         SELF_EOF( pArea, &bEof );
         if( ! bEof && pEvalInfo->dbsci.fBackward )
            SELF_BOF( pArea, &bEof );
      }

      hb_xvmSeqBegin();

      if( ! bEof && pEvalInfo->dbsci.lpstrFor )
         bEof = leto_dbEvalJoinStart( pEvalInfo->dbsci.lpstrFor );
      if( ! bEof )
         hb_itemPutNS( pEvalut, 1 );

      while( ! bEof && ( pRLocks || ! pEvalInfo->dbsci.itmCobWhile || hb_itemGetL( hb_vmEvalBlockV( pEvalInfo->dbsci.itmCobWhile, 2, pProces, pEvalut ) ) ) )
      {
         if( ! pRLocks && pEvalInfo->dbsci.itmCobFor )  /* FOR */
            bValid = hb_itemGetL( hb_vmEvalBlockV( pEvalInfo->dbsci.itmCobFor, 2, pProces, pEvalut ) );
         else
            bValid = HB_TRUE;
         if( bValid && pEvalInfo->dbsci.lpstrFor )
            bValid = leto_dbEvalJoinFor( pEvalInfo->dbsci.lpstrFor, pArea );

         hb_itemPutNS( pEvalut, hb_itemGetNS( pEvalut ) + 1 );

         if( bValid )
         {
            hb_itemPutNS( pProces, hb_itemGetNS( pProces ) + 1 );
            if( bAsArr )
            {
               if( nLen )
                  pLast = hb_arrayGetItemPtr( pSaveValResult, nLen );
               else  /* first call */
                  pLast = hb_itemNew( NULL );
            }
            else
               pLast = pSaveValResult;

            /* fails if needed lock not given, RTE catched and break to below */
            pResult = hb_vmEvalBlockV( pEvalInfo->itmBlock, 2, pProces, pLast );

            if( bAsArr )
            {
               if( ++nLen == 1 )
                  hb_itemRelease( pLast );
               hb_arrayAdd( pSaveValResult, pResult );
            }
            else
               hb_itemCopy( pSaveValResult, pResult );

            if( ! pRLocks && pEvalInfo->dbsci.lpstrFor )
               bOneMore = leto_dbEvalJoinPreSkip( pEvalInfo->dbsci.lpstrFor, pArea );
            if( ! bOneMore && ! pRLocks &&
                ( ( pEvalInfo->dbsci.itmRecID && hb_itemGetNL( pEvalInfo->dbsci.itmRecID ) ) || ( lNext && --lNext < 1 ) ) )
               break;
         }
         else if( ! pRLocks && pEvalInfo->dbsci.lpstrFor )
            bOneMore = leto_dbEvalJoinPreSkip( pEvalInfo->dbsci.lpstrFor, pArea );

         if( pRLocks )
         {
            if( ( HB_SIZE ) hb_itemGetNL( pProces ) < hb_arrayLen( pRLocks ) )
               SELF_GOTO( pArea, hb_arrayGetNL( pRLocks, hb_itemGetNL( pProces ) + 1 ) );
            else
            {
               SELF_GOTO( pArea, ulNextRecNo );
               break;
            }
         }
         else
         {
            if( ! bOneMore )
            {
               SELF_SKIP( pArea, ! pEvalInfo->dbsci.fBackward ? 1 : -1 );

               if( ! pEvalInfo->dbsci.fBackward )
                  SELF_EOF( pArea, &bEof );
               else
                  SELF_BOF( pArea, &bEof );
            }

            if( ! bEof && ! bOneMore && pEvalInfo->dbsci.lpstrFor )
               leto_dbEvalJoinPostSkip( pEvalInfo->dbsci.lpstrFor );
         }
      }

      hb_xvmSeqEnd();

      if( ! pUStru->iHbError )
         errCode = HB_SUCCESS;

      if( s_iDebugMode >= 15 )
      {
         leto_wUsLog( pUStru, -1,"DEBUG leto_dbEval %s %s %ld of %ld records",
                                  iLockTime >= 0 ? "locked" : "handled",
                                  errCode != HB_SUCCESS ? "FAIL" : "fine",
                                  hb_itemGetNS( pProces ), hb_itemGetNS( pEvalut ) - 1 );
      }
   }

   if( pRLocks && hb_arrayLen( pRLocks ) )
   {
      if( ! bStay )  /* initial fLock tried */
         leto_TableUnlock( pUStru->pCurAStru, HB_FALSE , pArea );
      else  /* throw away all locks at once if too many */
         leto_TableUnlock( pUStru->pCurAStru, hb_arrayLen( pRLocks ) > 42 ? HB_FALSE : HB_TRUE, pArea );
   }

   if( pArea )
   {
      if( ! bStay || ! bProved )
         leto_GotoIf( pArea, ulLastRecNo );

      if( pArea->valResult )
      {
         hb_vmDestroyBlockOrMacro( pArea->valResult );
         pArea->valResult = NULL;
      }
      if( pSaveValResult )
      {
         pArea->valResult = hb_itemClone( pSaveValResult );
         hb_itemRelease( pSaveValResult );
      }
   }

   hb_itemRelease( pProces );
   hb_itemRelease( pEvalut );
   hb_itemRelease( pRLocks );

   return errCode;
}

static void leto_Dbeval( PUSERSTRU pUStru, char * szData )
{
   AREAP      pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   DBEVALINFO pEvalInfo;
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_ITEM   pNext = NULL, pRec = NULL, pRest = NULL;
   HB_ULONG   ulRecNo;
   HB_BOOL    bValid = HB_TRUE;
   HB_BOOL    bResultAsArr, bNeedLock, bStay;
   HB_U32     uiLen;
   HB_SIZE    nPos;
   char *     ptr, * szData1 = NULL;

   letoSetUStru( pUStru );
   memset( &pEvalInfo, 0, sizeof( DBEVALINFO ) );

   hb_xvmSeqBegin();

   uiLen = HB_GET_LE_UINT32( szData );
   if( uiLen )
      pEvalInfo.itmBlock = leto_mkCodeBlock( pUStru, szData + 4, uiLen, HB_FALSE );

   nPos = 4 + uiLen + 1;
   uiLen = HB_GET_LE_UINT32( szData + nPos );
   if( uiLen )
      pEvalInfo.dbsci.itmCobFor = leto_mkCodeBlock( pUStru, szData + 4 + nPos, uiLen, HB_FALSE );
   nPos += 4 + uiLen + 1;
   uiLen = HB_GET_LE_UINT32( szData + nPos );
   if( uiLen )
      pEvalInfo.dbsci.itmCobWhile = leto_mkCodeBlock( pUStru, szData + 4 + nPos, uiLen, HB_FALSE );
   nPos += 4 + uiLen + 1;

   pNext = hb_itemPutNL( pNext, strtol( szData + nPos, &ptr, 10 ) );
   if( hb_itemGetNL( pNext ) >= 0 )
      pEvalInfo.dbsci.lNext = pNext;
   pRec = hb_itemPutNL( pRec, strtol( ++ptr, &ptr, 10 ) );
   if( hb_itemGetNL( pRec ) >= 0 )
      pEvalInfo.dbsci.itmRecID = pRec;
   pRest = hb_itemPutNL( pRest, strtol( ++ptr, &ptr, 10 ) );
   if( hb_itemGetNL( pRest ) >= 0 )
   {
      pRest = hb_itemPutL( pRest, hb_itemGetNL( pRest ) == 0 ? HB_FALSE : HB_TRUE );
      pEvalInfo.dbsci.fRest = pRest;
   }

   ptr++;
   bResultAsArr = *ptr == 'T';
   ptr += 2;
   bNeedLock = *ptr == 'T';
   ptr += 2;
   pEvalInfo.dbsci.fBackward = *ptr == 'T' ? HB_TRUE : HB_FALSE;
   ptr += 2;
   bStay = *ptr == 'T';
   ptr += 2;
   ulRecNo = strtoul( ptr, &ptr, 10 );
   if( *( ++ptr ) != ';' && *ptr )
   {
      pEvalInfo.dbsci.lpstrFor = leto_dbEvalJoinAdd( pUStru, ptr, pArea );
      if( ! pEvalInfo.dbsci.lpstrFor )
         bValid = HB_FALSE;
   }

   hb_xvmSeqEnd();

   if( ! pUStru->iHbError && ! pEvalInfo.itmBlock && ( pEvalInfo.dbsci.itmCobFor || pEvalInfo.dbsci.itmCobWhile ) )
   {
      PHB_ITEM pProces = hb_itemPutNS( NULL, 0 );
      PHB_ITEM pEvalut = hb_itemPutNS( NULL, 0 );

      if( s_iDebugMode > 20 )
         leto_wUsLog( pUStru, -1, "DEBUG leto_DbevalTest: FOR %p WHILE %p",
                                  pEvalInfo.dbsci.itmCobFor, pEvalInfo.dbsci.itmCobWhile );

      hb_xvmSeqBegin();

      if( pEvalInfo.dbsci.itmCobFor )
         hb_vmEvalBlockV( pEvalInfo.dbsci.itmCobFor, 2, pProces, pEvalut );
      if( pEvalInfo.dbsci.itmCobWhile )
         hb_vmEvalBlockV( pEvalInfo.dbsci.itmCobWhile, 2, pProces, pEvalut );

      hb_xvmSeqEnd();

      hb_itemRelease( pProces );
      hb_itemRelease( pEvalut );
   }

   if( pUStru->iHbError || ! pEvalInfo.itmBlock )
      bValid = HB_FALSE;
   if( bValid && bNeedLock )
   {
      if( pUStru->pCurAStru->pTStru->bReadonly )
         bValid = HB_FALSE;
      else
      {
         bNeedLock = ! ( pUStru->pCurAStru->bLocked || ! pUStru->pCurAStru->pTStru->bShared );
         if( bNeedLock && ! ( pUStru->uSrvLock & 0x01 ) )  /* RDDI_AUTOLOCK */
         {
            if( s_iDebugMode > 0 )
               leto_wUsLog( pUStru, -1, "ERROR leto_Dbeval: R-locks need, but no RDDI_AUTOLOCK for WA %s", pUStru->pCurAStru->szAlias );
            bValid = HB_FALSE;
         }
         else if( s_iDebugMode > 20 )
            leto_wUsLog( pUStru, -1, "DEBUG leto_Dbeval: will use RDDI_AUTOLOCK for WA %s", pUStru->pCurAStru->szAlias );
      }
   }

   if( bValid )
   {
      if( pArea->valResult )
         hb_vmDestroyBlockOrMacro( pArea->valResult );
      if( bResultAsArr )
         pArea->valResult = hb_itemArrayNew( 0 );
      else
         pArea->valResult = hb_itemNew( NULL );
      SELF_GOTO( pArea, ulRecNo );

      if( s_iDebugMode > 20 )
      {
         HB_LONG  lNext = pNext ? hb_itemGetNL( pNext ) : -1;
         HB_ULONG lRecNo = pRec ? hb_itemGetNL( pRec ) : -1;
         int      iRest = pRest ? ( int ) hb_itemGetL( pRest ) : -1;

         leto_wUsLog( pUStru, -1, "DEBUG leto_Dbeval: %s FOR %s WHILE %s (RECNO %ld NEXT %ld REST %d) [RESULT/LOCKS/DESC:%d/%d/%d]",
                                   szData + 4, pEvalInfo.dbsci.itmCobFor ? "!":"", pEvalInfo.dbsci.itmCobWhile ? "!":"", lRecNo, lNext, iRest,
                                   ( int ) bResultAsArr, ( int ) bNeedLock, ( int ) pEvalInfo.dbsci.fBackward );
      }

      errCode = leto_dbEval( pUStru, pArea, &pEvalInfo, bNeedLock ? pUStru->iLockTimeOut : -1, bStay );
   }

   if( errCode == HB_SUCCESS )
   {
      HB_SIZE  nSize;
      char *   pParam = hb_itemSerialize( pArea->valResult, HB_SERIALIZE_NUMSIZE, &nSize );
      HB_ULONG ulLen = 0, ulPLen = leto_CryptText( pUStru, pParam, nSize, 0 );

      szData1 = leto_recWithAlloc( pArea, pUStru, pUStru->pCurAStru, &ulLen );
      szData1 = ( char * ) hb_xrealloc( szData1, ulLen + ulPLen + 1 );
      memcpy( szData1 + ulLen, pUStru->pBufCrypt, ulPLen );
      ulLen += ulPLen;

      leto_SendAnswer( pUStru, szData1, ulLen );

      if( pParam )
         hb_xfree( pParam );
   }
   else if( pUStru->iHbError )
      leto_SendError( pUStru, szErr4, 4 );
   else if( ! pEvalInfo.itmBlock )
      leto_SendAnswer( pUStru, szErr2, 4 );
   else
      leto_SendAnswer( pUStru, szErr3, 4 );

   if( pArea->valResult )
      hb_vmDestroyBlockOrMacro( pArea->valResult );
   pArea->valResult = NULL;

   if( szData1 )
      hb_xfree( szData1 );
   if( pEvalInfo.itmBlock )
      hb_vmDestroyBlockOrMacro( pEvalInfo.itmBlock );
   if( pEvalInfo.dbsci.itmCobFor)
      hb_vmDestroyBlockOrMacro( pEvalInfo.dbsci.itmCobFor );
   if( pEvalInfo.dbsci.itmCobWhile )
      hb_vmDestroyBlockOrMacro( pEvalInfo.dbsci.itmCobWhile );
   hb_itemRelease( pNext );
   hb_itemRelease( pRec );
   hb_itemRelease( pRest );
   if( pEvalInfo.dbsci.lpstrFor )  /* pJoins */
   {
      HB_SIZE nPos = 0;

      while( ++nPos <= hb_arrayLen( pEvalInfo.dbsci.lpstrFor ) )
      {
         if( hb_arrayGetType( pEvalInfo.dbsci.lpstrFor, 3 ) & HB_IT_BLOCK )
            hb_vmDestroyBlockOrMacro( hb_arrayGetItemPtr( pEvalInfo.dbsci.lpstrFor, 3 ) );
      }
      hb_itemRelease( pEvalInfo.dbsci.lpstrFor );
   }
}

/* leto_udf() leto_DbEval( cbBlock, cbFor, cbWhile, nNext, nRec, lRest[, lResultArr, lNeedLock, lBackward ] ) */
HB_FUNC( LETO_DBEVAL )
{
   PUSERSTRU  pUStru = letoGetUStru();
   HB_ERRCODE errCode = HB_FAILURE;
   DBEVALINFO pEvalInfo;
   AREAP      pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PHB_ITEM   pNext = hb_param( 4, HB_IT_NUMERIC );
   PHB_ITEM   pRec = hb_param( 5, HB_IT_NUMERIC );
   PHB_ITEM   pRest = hb_param( 6, HB_IT_LOGICAL );
   HB_BOOL    bResultAsArr = hb_parldef( 7, HB_FALSE );
   HB_BOOL    bNeedLock = hb_parldef( 8, HB_FALSE );
   HB_BOOL    bBackward = hb_parldef( 9, HB_FALSE );
   HB_BOOL    bStay = hb_parldef( 10, HB_TRUE );
   HB_BOOL    bValid = pArea ? HB_TRUE : HB_FALSE;

   memset( &pEvalInfo, 0, sizeof( DBEVALINFO ) );
   if( pArea && bNeedLock )
   {
      if( pUStru->pCurAStru->pTStru->bReadonly )
         bValid = HB_FALSE;
      else
      {
         bNeedLock = ! ( pUStru->pCurAStru->bLocked || ! pUStru->pCurAStru->pTStru->bShared );
         if( bNeedLock && ! ( pUStru->uSrvLock & 0x01 ) )  /* RDDI_AUTOLOCK */
         {
            if( s_iDebugMode > 0 )
               leto_wUsLog( pUStru, -1, "ERROR LETO_DBEVAL: R-locks need, but no RDDI_AUTOLOCK for WA %s", pUStru->pCurAStru->szAlias );
            bValid = HB_FALSE;
         }
         else if( s_iDebugMode > 20 )
            leto_wUsLog( pUStru, -1, "DEBUG LETO_DBEVAL: will use RDDI_AUTOLOCK for WA %s", pUStru->pCurAStru->szAlias );
      }
   }

   hb_xvmSeqBegin();

   if( HB_ISCHAR( 1 ) )
      pEvalInfo.itmBlock = leto_mkCodeBlock( pUStru, hb_parc( 1 ), hb_parclen( 1 ), HB_FALSE );
   else if( HB_ISEVALITEM( 1 ) || HB_ISBLOCK( 1 ) )
      pEvalInfo.itmBlock = hb_itemClone( hb_param( 1, HB_IT_EVALITEM ) );
   if( ! pEvalInfo.itmBlock )
      pUStru->iHbError = 1;

   if( HB_ISCHAR( 2 ) )
      pEvalInfo.dbsci.itmCobFor = leto_mkCodeBlock( pUStru, hb_parc( 2 ), hb_parclen( 2 ), HB_FALSE );
   else if( HB_ISEVALITEM( 2 ) || HB_ISBLOCK( 2 ) )
      pEvalInfo.dbsci.itmCobFor = hb_itemClone( hb_param( 2, HB_IT_EVALITEM | HB_IT_BLOCK  ) );

   if( HB_ISCHAR( 3 ) )
      pEvalInfo.dbsci.itmCobWhile = leto_mkCodeBlock( pUStru, hb_parc( 3 ), hb_parclen( 3 ), HB_FALSE );
   else if( HB_ISEVALITEM( 3 ) || HB_ISBLOCK( 3 ) )
      pEvalInfo.dbsci.itmCobWhile = hb_itemClone( hb_param( 3, HB_IT_EVALITEM | HB_IT_BLOCK ) );

   hb_xvmSeqEnd();

   if( pArea->valResult )
      hb_vmDestroyBlockOrMacro( pArea->valResult );
   pArea->valResult = NULL;

   if( ! pUStru->iHbError && bValid )
   {
      pEvalInfo.dbsci.lNext = pNext;
      pEvalInfo.dbsci.itmRecID = pRec;
      pEvalInfo.dbsci.fRest = pRest;
      pEvalInfo.dbsci.fBackward = bBackward;
      if( hb_parclen( 11 ) )
      {
         pEvalInfo.dbsci.lpstrFor = leto_dbEvalJoinAdd( pUStru, hb_parc( 11 ), pArea );
         if( ! pEvalInfo.dbsci.lpstrFor )
            bValid = HB_FALSE;
      }
      if( bValid && bResultAsArr )
         pArea->valResult = hb_itemArrayNew( 0 );
      else if( bValid )
         pArea->valResult = hb_itemNew( NULL );

      if( ( bValid && s_iDebugMode > 20 ) || ( ! bValid && s_iDebugMode > 1 ) )
      {
         HB_LONG  lNext = pNext ? hb_itemGetNL( pNext ) : -1;
         HB_ULONG lRecNo = pRec ? hb_itemGetNL( pRec ) : -1;
         int      iRest = pRest ? ( int ) hb_itemGetL( pRest ) : -1;

         leto_wUsLog( pUStru, -1, "%s LETO_DBEVAL: %s FOR %s WHILE %s (RECNO %ld NEXT %ld REST %d) [RESULT/LOCKS/DESC:%d/%d/%d]",
                                  bValid ? "DEBUG" : "ERROR", hb_parc( 1 ), hb_parc( 2 ), hb_parc( 3 ), lRecNo, lNext, iRest,
                                  ( int ) bResultAsArr, ( int ) bNeedLock, ( int ) bBackward );
      }

      if( bValid )
         errCode = leto_dbEval( pUStru, pArea, &pEvalInfo, bNeedLock ? pUStru->iLockTimeOut : -1, bStay );
   }

   if( pUStru->iHbError || errCode != HB_SUCCESS || ! bValid )
      hb_retl( HB_FALSE );
   else
   {
      if( ( hb_itemType( pArea->valResult ) & HB_IT_NIL ) )
         hb_retl( HB_TRUE );
      else
         hb_itemReturn( pArea->valResult );
   }

   if( pArea->valResult )
   {
      hb_itemRelease( pArea->valResult );
      pArea->valResult = NULL;
   }
   /* destroy only for created CB */
   if( HB_ISCHAR( 1 ) && pEvalInfo.itmBlock )
      hb_vmDestroyBlockOrMacro( pEvalInfo.itmBlock );
   if( HB_ISCHAR( 2 ) && pEvalInfo.dbsci.itmCobFor)
      hb_vmDestroyBlockOrMacro( pEvalInfo.dbsci.itmCobFor );
   if( HB_ISCHAR( 3 ) && pEvalInfo.dbsci.itmCobWhile )
      hb_vmDestroyBlockOrMacro( pEvalInfo.dbsci.itmCobWhile );
}

/* __dbLocate( cbFor, cbWhile, lNext, nRec, fRest ) */
HB_FUNC( LETO_DBLOCATE )
{
   PUSERSTRU pUStru = letoGetUStru();
   AREAP     pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_ULONG  ulRecNo = 0;

   if( pArea )
   {
      DBSCOPEINFO dbScopeInfo;

      memset( &dbScopeInfo, 0, sizeof( DBSCOPEINFO ) );
      if( hb_parinfa( 1, 0 ) == 2 )
      {
         if( ( hb_itemType( hb_arrayGetItemPtr( hb_stackItemFromBase( 1 ), 1 ) ) & HB_IT_BLOCK ) )
            dbScopeInfo.itmCobFor = hb_arrayGetItemPtr( hb_stackItemFromBase( 1 ), 1 );
         if( ( hb_itemType( hb_arrayGetItemPtr( hb_stackItemFromBase( 1 ), 2 ) ) & HB_IT_STRING ) )
            dbScopeInfo.lpstrFor  = hb_arrayGetItemPtr( hb_stackItemFromBase( 1 ), 2 );
      }
      else
      {
         dbScopeInfo.itmCobFor   = hb_param( 1, HB_IT_BLOCK );
         dbScopeInfo.lpstrFor    = hb_param( 1, HB_IT_STRING );
      }
      if( hb_parinfa( 1, 0 ) == 2 )
      {
         if( ( hb_itemType( hb_arrayGetItemPtr( hb_stackItemFromBase( 2 ), 1 ) ) & HB_IT_BLOCK ) )
            dbScopeInfo.itmCobWhile = hb_arrayGetItemPtr( hb_stackItemFromBase( 2 ), 1 );
         if( ( hb_itemType( hb_arrayGetItemPtr( hb_stackItemFromBase( 2 ), 2 ) ) & HB_IT_STRING ) )
            dbScopeInfo.lpstrWhile  = hb_arrayGetItemPtr( hb_stackItemFromBase( 2 ), 2 );
      }
      else
      {
         dbScopeInfo.itmCobWhile = hb_param( 2, HB_IT_BLOCK );
         dbScopeInfo.lpstrWhile  = hb_param( 2, HB_IT_STRING );
      }
      dbScopeInfo.lNext       = hb_param( 3, HB_IT_NUMERIC );
      dbScopeInfo.itmRecID    = hb_param( 4, HB_IT_NUMERIC );
      dbScopeInfo.fRest       = hb_param( 5, HB_IT_LOGICAL );
      if( hb_param( 2, HB_IT_BLOCK | HB_IT_STRING ) )
         hb_itemPutL( dbScopeInfo.fRest, HB_TRUE );

      dbScopeInfo.fIgnoreFilter = HB_TRUE;
      dbScopeInfo.fIncludeDeleted = HB_TRUE;

      hb_xvmSeqBegin();

      if( ! dbScopeInfo.itmCobFor && dbScopeInfo.lpstrFor )
         dbScopeInfo.itmCobFor = leto_mkCodeBlock( pUStru, hb_itemGetCPtr( dbScopeInfo.lpstrFor ),
                                                           hb_itemGetCLen( dbScopeInfo.lpstrFor ), HB_FALSE );
      if( ! dbScopeInfo.itmCobWhile && dbScopeInfo.lpstrWhile )
         dbScopeInfo.itmCobWhile = leto_mkCodeBlock( pUStru, hb_itemGetCPtr( dbScopeInfo.lpstrWhile ),
                                                             hb_itemGetCLen( dbScopeInfo.lpstrWhile ), HB_FALSE );
      if( SELF_SETLOCATE( pArea, &dbScopeInfo ) == HB_SUCCESS )
      {
         HB_BOOL bFound = HB_FALSE;

         SELF_LOCATE( pArea, HB_FALSE );
         SELF_FOUND( pArea, &bFound );
         if( bFound )
            SELF_RECNO( pArea, &ulRecNo );
      }

      hb_xvmSeqEnd();
   }

   hb_retnl( ulRecNo );
}

HB_FUNC( LETO_DBCONTINUE )
{
   AREAP    pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_ULONG ulRecNo = 0;

   if( pArea )
   {
      HB_BOOL bFound = HB_FALSE;

      SELF_LOCATE( pArea, HB_TRUE );
      SELF_FOUND( pArea, &bFound );
      if( bFound )
         SELF_RECNO( pArea, &ulRecNo );
   }

   hb_retnl( ulRecNo );
}

/* leto_udf() leto_dbTotal( cFile, xKey, aFields, xFor, xWhile, nNext, nRec, lRest, cRDD, nConnection, cCodePage ) */
HB_FUNC( LETO_DBTOTAL )
{
   PUSERSTRU  pUStru = letoGetUStru();
   AREAP      pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PHB_DYNS   pDo = hb_dynsymFind( "__DBTOTAL" );
   PHB_ITEM   pBlock, pFor, pWhile;

   hb_xvmSeqBegin();

   if( HB_ISCHAR( 2 ) )
      pBlock = leto_mkCodeBlock( pUStru, hb_parc( 2 ), hb_parclen( 2 ), HB_FALSE );
   else
      pBlock = hb_param( 2, HB_IT_BLOCK );
   if( HB_ISCHAR( 4 ) )
      pFor = leto_mkCodeBlock( pUStru, hb_parc( 4 ), hb_parclen( 4 ), HB_FALSE );
   else
      pFor = hb_param( 4, HB_IT_BLOCK );
   if( HB_ISCHAR( 5 ) )
      pWhile = leto_mkCodeBlock( pUStru, hb_parc( 5 ), hb_parclen( 5 ), HB_FALSE );
   else
      pWhile = hb_param( 5, HB_IT_BLOCK );

   hb_xvmSeqEnd();

   if( ! pUStru->iHbError && pArea && pDo )
   {
      char   szFile[ HB_PATH_MAX ];
      int    i, iLen = hb_pcount();

      leto_DataPath( hb_parc( 1 ), szFile );

      hb_vmPushDynSym( pDo );
      hb_vmPushNil();
      hb_vmPushString( szFile, strlen( szFile ) );
      hb_vmPush( pBlock );
      hb_vmPush( hb_param( 2, HB_IT_ARRAY ) );
      hb_vmPush( pFor );
      hb_vmPush( pWhile );

      for( i = 6; i <= iLen; i++ )
      {
         hb_vmPush( hb_param( i, HB_IT_ANY ) );
      }
      hb_vmDo( ( HB_USHORT ) ( HB_MAX( 5, iLen ) ) );
   }
   else
      hb_retl( HB_FALSE );
}

/* __dbUpdate( cAlias, cKey, lRandom, aAssign, aFields ) -- pre-validated */
HB_FUNC( LETO_DBUPDATE )
{
   PUSERSTRU  pUStru = letoGetUStru();
   AREAP      pMArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   int        iMArea = hb_rddGetCurrentWorkAreaNumber();
   AREAP      pSArea = NULL;
   int        iSArea = 0;
   HB_ULONG   ulSelectID = 0;
   HB_BOOL    bRandom = hb_parldef( 3, HB_FALSE );
   PHB_ITEM   pExpres = hb_param( 4, HB_IT_ARRAY );
   PHB_ITEM   pFields = hb_param( 5, HB_IT_ARRAY );
   PHB_ITEM   pBlocks = NULL, pKey = NULL;
   HB_SIZE    n, nLen = pFields ? hb_arrayLen( pFields ) : 0;
   HB_BOOL    bValid = ( pMArea && pUStru->pCurAStru ) ? HB_TRUE : HB_FALSE;

   if( bValid )
   {
      if( ! ( pUStru->pCurAStru->bLocked || ! pUStru->pCurAStru->pTStru->bShared ) )
         bValid = HB_FALSE;
      else if( pUStru->pCurAStru->pTStru->bReadonly )
         bValid = HB_FALSE;
      else if( ! hb_parclen( 1 ) )
         bValid = HB_FALSE;
      else if( ! ( hb_parclen( 2 ) || HB_IS_BLOCK( 2 ) ) )
         bValid = HB_FALSE;
      else if( ! nLen || ! pExpres || nLen != hb_arrayLen( pExpres ) )
         bValid = HB_FALSE;
   }
   if( ! bValid )
      leto_wUsLog( pUStru, 0, "ERROR LETO_DBUPDATE: failed cause of an invalid param" );

   if( bValid )
   {
      PAREASTRU pAStru;

      /* request slave */
      if( s_bNoSaveWA )
         pAStru = leto_FindAlias( pUStru, hb_parc( 1 ) );
      else
         pAStru = leto_Select( pUStru, 0, hb_parc( 1 ), HB_TRUE );
      if( pAStru )
      {
         bValid = HB_TRUE;
         if( s_bNoSaveWA && pAStru->pTStru->bMemIO )  /* need to request */
            pAStru = leto_Select( pUStru, 0, hb_parc( 1 ), HB_TRUE );

         hb_rddGetAliasNumber( pAStru->pTStru->szLetoAlias, &iSArea );
         pSArea = ( AREAP ) hb_rddGetWorkAreaPointer( ( HB_AREANO ) iSArea );
         ulSelectID = pAStru->ulSelectID;
      }

      hb_rddSelectWorkAreaNumber( iMArea );

      if( ! iSArea )
         bValid = HB_FALSE;
      else if( bValid )
      {
         char     szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
         int      iPos, iLen;
         HB_SIZE  nNeedSize, nBlockSize = 4;
         char *   szBlock = ( char * ) hb_xgrab( nBlockSize );
         char *   szAliasTranslate;
         HB_ULONG ulLen;

         SELF_ALIAS( pSArea, szAlias );
         ulLen = strlen( szAlias );
         strcpy( szAlias + ulLen, "->" );
         ulLen += 2;
         szAliasTranslate = leto_AliasTranslate( pUStru, szAlias, &ulLen );
         hb_strncpy( szAlias, szAliasTranslate, HB_RDD_MAX_ALIAS_LEN );

         strcpy( szBlock, "{||" );
         iPos = 3;
         for( n = 1; n <= nLen; n++ )
         {
            nNeedSize = strlen( hb_arrayGetCPtr( pExpres, n ) ) + 5 +
                        strlen( szAlias ) + 10 + 16;
            nBlockSize += nNeedSize;
            szBlock = ( char * ) hb_xrealloc( szBlock, nBlockSize );
            iLen = eprintf( szBlock + iPos, "%s->%s:=(%d)->(%s), ", szAlias, hb_arrayGetCPtr( pFields, n ),
                                                                    iSArea, hb_arrayGetCPtr( pExpres, n ) );
            iPos += iLen;
         }
         strcpy( szBlock + iPos++, "}" );

         hb_xvmSeqBegin();

         pBlocks = leto_mkCodeBlock( pUStru, szBlock, iPos, HB_FALSE );

         nNeedSize = hb_parclen( 2 ) + 6;
         if( nNeedSize > nBlockSize )
            szBlock = ( char * ) hb_xrealloc( szBlock, nBlockSize );
         iLen = eprintf( szBlock, "{||%s}", iSArea, hb_parc( 2 ) );
         pKey = leto_mkCodeBlock( pUStru, szBlock, iLen, HB_FALSE );

         hb_xvmSeqEnd();

         if( pUStru->iHbError )
         {
            bValid = HB_FALSE;
            leto_wUsLog( pUStru, -1, "ERROR LETO_DBUPDATE: failed, invalid/ not optimized CB: %s", szBlock );
         }
         hb_xfree( szBlock );
      }
   }

   if( bValid )
   {
      DBORDERINFO pOrderInfo;
      HB_BOOL     fEof, fFirst = HB_TRUE;
      HB_ULONG    ulMRecNo = 0, ulSRecNo = 0;
      PHB_ITEM    pKeyValue;
      PHB_ITEM    pSkipValue;
      int         iResult = 1;

      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );

      SELF_RECNO( pMArea, &ulMRecNo );
      SELF_RECNO( pSArea, &ulSRecNo );
      SELF_GOTOP( pMArea );
      SELF_GOTOP( pSArea );

      hb_xvmSeqBegin();

      SELF_EOF( pSArea, &fEof );
      while( ! fEof && bValid )
      {
         hb_rddSelectWorkAreaNumber( iSArea );
         pKeyValue = hb_vmEvalBlock( pKey );  /* in slave ! */
         hb_rddSelectWorkAreaNumber( iMArea );
         if( bRandom && fFirst)  /* check type */
         {
            fFirst = HB_FALSE;
            pOrderInfo.itmResult = hb_itemNew( NULL );
            SELF_ORDINFO( pMArea, DBOI_KEYVAL, &pOrderInfo );
            bValid = hb_itemCompare( pOrderInfo.itmResult, pKeyValue, HB_TRUE, &iResult );
            hb_itemRelease( pOrderInfo.itmResult );
            if( ! bValid )
               continue;  /* -> break of different type */
         }
         if( bRandom )
         {
            if( SELF_SEEK( pMArea, HB_TRUE, pKeyValue, HB_FALSE ) == HB_SUCCESS )  /* no softseek */
               hb_vmEvalBlock( pBlocks );
         }
         else
         {
            while( ! fEof )
            {
               pSkipValue = hb_vmEvalBlock( pKey );  /* in master */
               bValid = hb_itemCompare( pSkipValue, pKeyValue, HB_TRUE, &iResult );
               if( ! bValid )  /* different type */
                  break;
               if( iResult < 0 )
               {
                  SELF_SKIP( pMArea, 1 );
                  SELF_EOF( pSArea, &fEof );
               }
               else
                  break;
            }

            if( ! fEof && iResult == 0 && bValid )
            {
               hb_vmEvalBlock( pBlocks );
            }
         }

         SELF_SKIP( pSArea, 1 );
         SELF_EOF( pSArea, &fEof );
      }

      hb_xvmSeqEnd();

      if( pUStru->iHbError )
         bValid = HB_FALSE;

      SELF_GOTO( pSArea, ulSRecNo );
      SELF_GOTO( pMArea, ulMRecNo );
   }

   if( iMArea && ulSelectID )  // select ord ulaarea tofix ??
   {
      leto_FreeArea( pUStru, ulSelectID, HB_FALSE );
      hb_rddSelectWorkAreaNumber( iMArea );
   }

   hb_vmDestroyBlockOrMacro( pKey );
   hb_vmDestroyBlockOrMacro( pBlocks );

   hb_retl( bValid );
}

/* leto_udf() : client created cDestAlias, WA is opened as active, other CBs pre-processed */
HB_FUNC( LETO_DBJOIN )  /* ( cSlaveAlias, cDestAlias, aFields(*) , cFor ) - (*) with->alias _or_ literal-CB */
{
   PUSERSTRU pUStru = letoGetUStru();
   PAREASTRU pOldAStru = pUStru->pCurAStru;
   HB_ULONG  ulOldAreaID = pUStru->ulCurAreaID;
   AREAP     pDArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   int       iDArea = hb_rddGetCurrentWorkAreaNumber();
   AREAP     pMArea = NULL, pSArea = NULL;
   int       iMArea = 0;
   PHB_ITEM  pFlds = hb_param( 3, HB_IT_ARRAY );
   PHB_ITEM  pFields = NULL;
   PHB_ITEM  pFor = NULL;
   PHB_ITEM  pBlock = NULL;

   if( pDArea )
   {
      PAREASTRU pAStru;

      int i, iArea = 0;

      for( i = 1; i <= 2; i++ )  /* request for slave and master WA */
      {
         if( hb_parclen( i ) )
         {
            if( s_bNoSaveWA )
               pAStru = leto_FindAlias( pUStru, hb_parc( i ) );
            else
               pAStru = leto_Select( pUStru, 0, hb_parc( i ), HB_TRUE );
            if( pAStru )
            {
               if( s_bNoSaveWA && pAStru->pTStru->bMemIO )  /* need to request */
                  pAStru = leto_Select( pUStru, 0, hb_parc( i ), HB_TRUE );

               hb_rddGetAliasNumber( pAStru->pTStru->szLetoAlias, &iArea );
               if( i == 1 )  /* slave*/
                  pSArea = ( AREAP ) hb_rddGetWorkAreaPointer( ( HB_AREANO ) iArea );
               else          /* master */
               {
                  iMArea = iArea;
                  pMArea = ( AREAP ) hb_rddGetWorkAreaPointer( ( HB_AREANO ) iArea );
               }
            }
         }
      }
   }

   if( pMArea && pSArea && pDArea )  /* prepare pre-processed params */
   {
      hb_rddSelectWorkAreaNumber( iMArea );

      if( hb_parclen( 4 ) > 0 )
         pFor = leto_mkCodeBlock( pUStru, hb_parc( 4 ), hb_parclen( 4 ), HB_TRUE );
      else
         pFor = leto_mkCodeBlock( pUStru, "{||.T.}", 7, HB_TRUE );

      if( pFor && ! ( hb_itemType( hb_vmEvalBlock( pFor ) ) & HB_IT_LOGICAL ) )
      {
         pFields = hb_itemArrayNew( 0 );
         hb_itemRelease( pFor );
         pFor = NULL;
      }
      if( ! pFor )
         leto_wUsLog( pUStru, -1, "ERROR LETO_DBJOIN <%s> for-expression invalid", hb_parc( 4 ) );

      if( pFor )
      {
         if( pFlds && hb_arrayLen( pFlds ) )
         {
            HB_USHORT ui, uiFields = 0;
            PHB_ITEM  pCBlock;
            char *    szExpr = NULL;
            HB_ULONG  ulLen;
            int       iLen;

            SELF_FIELDCOUNT( pMArea, &uiFields );
            uiFields = HB_MIN( ( HB_USHORT ) hb_arrayLen( pFlds ), uiFields );
            pFields = hb_itemArrayNew( uiFields );

            hb_xvmSeqBegin();

            for( ui = 1; ui <= uiFields; ui++ )
            {
               pCBlock = hb_arrayGetItemPtr( pFlds, ui );
               iLen = hb_itemGetCLen( pCBlock );
               if( iLen )
               {
                  ulLen = ( HB_ULONG ) iLen;
                  szExpr = leto_AliasTranslate( pUStru, hb_itemGetCPtr( pCBlock ), &ulLen );
                  pBlock = leto_mkCodeBlock( pUStru, szExpr, ulLen, HB_FALSE );
                  hb_xfree( szExpr );
                  szExpr = NULL;
               }
               if( pBlock )
               {
                  hb_arraySet( pFields, ui, pBlock );
                  hb_itemRelease( pBlock );
                  pBlock = NULL;
               }
            }

            hb_xvmSeqEnd();

            if( pUStru->iHbError )
            {
               if( s_iDebugMode > 0 )
                  leto_wUsLog( pUStru, -1, "ERROR LETO_DBJOIN fail to create block %u: %s", ui, szExpr );
               hb_itemRelease( pFields );
               pFields = NULL;
               if( szExpr )
                  hb_xfree( szExpr );
            }
         }
      }
   }

   if( pFields && hb_arrayLen( pFields ) )  /* action loop */
   {
      const HB_SIZE nLen = hb_arrayLen( pFields );
      PHB_ITEM * pBlocks = ( PHB_ITEM * ) hb_xgrabz( sizeof( PHB_ITEM ) * hb_arrayLen( pFields ) );
      HB_ERRCODE errcode = HB_SUCCESS;
      HB_BOOL    bEof = HB_FALSE;
      HB_SIZE    nProcessed = 0;
      HB_USHORT  ui;

      for( ui = 0; ui < ( HB_USHORT ) hb_arrayLen( pFields ); ui++ )
      {
         if( HB_IS_BLOCK( hb_arrayGetItemPtr( pFields, ui + 1 ) ) )
            pBlocks[ ui ] = hb_arrayGetItemPtr( pFields, ui + 1 );
      }

      SELF_GOTOP( pMArea );
      SELF_EOF( pMArea, &bEof );
      while( ! bEof && errcode == HB_SUCCESS )
      {
         SELF_GOTOP( pSArea );
         SELF_EOF( pSArea, &bEof );
         while( ! bEof && errcode == HB_SUCCESS )
         {
            if( hb_itemGetL( hb_vmEvalBlock( pFor ) ) )
            {
               errcode = SELF_APPEND( pDArea, HB_TRUE );
               if( errcode == HB_SUCCESS )
               {
                  for( ui = 0; ui < ( HB_USHORT ) nLen; ui++ )
                  {
                     if( pBlocks[ ui ] )  /* blocks are verified */
                        SELF_PUTVALUE( pDArea, ui + 1, hb_vmEvalBlock( pBlocks[ ui ] ) );
                  }
                  nProcessed++;
               }
            }

            SELF_SKIP( pSArea, 1 );
            SELF_EOF( pSArea, &bEof );
         }

         SELF_SKIP( pMArea, 1 );
         SELF_EOF( pMArea, &bEof );
      }

      if( s_iDebugMode >= 15 )
         leto_wUsLog( pUStru, -1, "DEBUG LETO_DBJOIN %ld records with %ld fields processed", nProcessed, nLen );
      hb_xfree( pBlocks );
      hb_retl( errcode == HB_SUCCESS  );
   }
   else
      hb_retl( HB_FALSE );

   if( pUStru->ulCurAreaID != ulOldAreaID )
   {
      pUStru->pCurAStru = pOldAStru;
      pUStru->ulCurAreaID = ulOldAreaID;
      leto_FreeArea( pUStru, ulOldAreaID, HB_TRUE );
      if( s_iDebugMode > 20 )
         leto_wUsLog( pUStru, -1, "DEBUG LETO_DBJOIN free-ed other workares as %lu", pUStru->ulCurAreaID );
   }
   hb_rddSelectWorkAreaNumber( iDArea );

   hb_itemRelease( pFor );
   hb_itemRelease( pFields );
}

static void leto_Goto( PUSERSTRU pUStru, char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PAREASTRU    pAStru = pUStru->pCurAStru;
   char *       szData1 = NULL, * ptr;
   const char * pData;
   HB_ULONG     ulLen;
   HB_LONG      lRecNo = strtol( szData, &ptr, 10 );
   HB_ERRCODE   errCode;

   if( *ptr != ';' )
   {
      pData = szErr2;
      ulLen = 4;
   }
   else
   {
      if( lRecNo > 0 )
         errCode = SELF_GOTO( pArea, ( HB_ULONG ) lRecNo );
      else if( ( lRecNo == -1 ) || ( lRecNo == -2 ) )
      {
         HB_BOOL bTop = lRecNo == -1;

         ptr++;
         if( pUStru->bDeleted != ( *ptr & 0x01 ) )
         {
            pUStru->bDeleted = ( *ptr & 0x01 );
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

static int leto_Memo( PUSERSTRU pUStru, char * szData, TRANSACTSTRU * pTA, AREAP pArea )
{
   PAREASTRU    pAStru = pUStru->pCurAStru;
   const char * pData = NULL;
   int          iRes = 0;
   HB_BOOL      bPut = HB_FALSE;
   HB_ULONG     ulLen = 4;

   if( *( szData + 1 ) != ';' )
   {
      pData = szErr2;
      iRes = 2;
   }
   else
   {
      char *    ptrPar;
      HB_ULONG  ulRecNo = strtoul( szData + 2, &ptrPar, 10 );
      HB_USHORT uiField = ( HB_USHORT ) strtoul( ++ptrPar, &ptrPar, 10 );
      HB_BOOL   bAdd = HB_FALSE;

      if( ( ! ulRecNo && ! pTA ) || ! uiField )
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

         if( bPut && ( ! pTA || ulRecNo ) &&
             pAStru->pTStru->bShared && ! pAStru->bLocked &&
             leto_IsRecLocked( pAStru, ulRecNo ) != 1 )
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
               SELF_GOTO( pArea, ulRecNo );
            else
            {
               pTA->bAppend = bAdd;
               pTA->ulRecNo = ulRecNo;
            }

            if( bPut )
            {
               const char * pMemo = ptrPar + 1;  /* possible binary data */
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
            }
            else
            {
               SELF_GETVALUE( pArea, uiField, pMemoText );
               ulLen = leto_CryptText( pUStru, hb_itemGetCPtr( pMemoText ), hb_itemGetCLen( pMemoText ), 0 );
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
      /* sync pos */
      if( pUStru->ulBufCryptLen > LETO_SENDRECV_BUFFSIZE )
      {
         hb_xfree( pUStru->pBufCrypt );
         pUStru->pBufCrypt = NULL;
         pUStru->ulBufCryptLen = 0;
      }
   }

   return iRes;
}

static void leto_MemoRaw( PUSERSTRU pUStru, char * szData )
{
   leto_Memo( pUStru, szData, NULL, NULL );
}

static void leto_Ordfunc( PUSERSTRU pUStru, char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PAREASTRU    pAStru = pUStru->pCurAStru;
   char         szData1[ HB_PATH_MAX + 64 ];
   const char * pData = NULL;
   char *       szData2 = NULL;
   HB_ULONG     ulLen = 4;
   char         * pTagName, * pNumber = NULL, * pDopFlags = NULL, * pFlags = NULL;
   int          nParam = leto_GetParam( szData, &pTagName, &pNumber, &pFlags, &pDopFlags, NULL );

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
                  {
                     pData = szErr4;
                     break;
                  }
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
            pData = szOk;

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
            pTag = pAStru->pTag;
            while( pTag )
            {
               /* compare Bagname against wanted one */
               if( ( ! s_bLowerPath ) ? ! strncmp( pTag->pIStru->szBagName, szBagName, uiLen ) :
                                        ! hb_strnicmp( pTag->pIStru->szBagName, szBagName, uiLen ) )
               {
                  if( pTag->pIStru->bProduction && hb_setGetAutOpen() && leto_ProdSupport( pAStru->pTStru->szDriver ) )
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
         HB_USHORT  ui;

         HB_GC_LOCKT();

         /* determine non production orders in list of used index, set marker if found one */
         ui = 0;
         while( ui < pTStru->uiIndexCount && ( pIStru = ( PINDEXSTRU ) letoGetListItem( &pTStru->IndexList, ui ) ) != NULL )
         {
            if( ! pIStru->bProduction || ! leto_ProdSupport( pAStru->pTStru->szDriver ) || ! hb_setGetAutOpen() )
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
                     if( nParam < 5 )
                        pData = szErr2;
                     else
                     {
                        DBORDERINFO pInfo;
                        HB_USHORT   uiCommand;
                        HB_ULONG    ulBufLen, ulRecNo;
                        char *      pTmp;

                        ulRecNo = strtoul( pNumber, NULL, 10 );
                        /* DBOI_SKIPWILD, DBOI_SKIPWILDBACK, DBOI_SKIPREGEX, DBOI_SKIPREGEXBACK */
                        uiCommand = ( HB_USHORT ) atoi( pDopFlags );
                        pDopFlags += strlen( pDopFlags ) + 1;
                        ulBufLen = strtoul( pDopFlags, &pTmp, 10 );
                        pData = szErr2;
                        if( *pTmp++ == ';' )
                        {
                           memset( &pInfo, 0, sizeof( DBORDERINFO ) );
                           pInfo.itmNewVal = hb_itemPutCL( NULL, pTmp, ulBufLen );
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
                           }
                           else
                              pData = szErr101;
                        }
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

static HB_USHORT leto_DriverID( PUSERSTRU pUStru )
{
   HB_USHORT uiDriver;

   if( ! strcmp( pUStru->szDriver, "DBFCDX"  ) )
      uiDriver = 0;
   else if( ! strcmp( pUStru->szDriver, "DBFNTX"  ) )
      uiDriver = 1;
   else if( ! strcmp( pUStru->szDriver, "DBFNSX"  ) )
      uiDriver = 2;
   else if( ! strcmp( pUStru->szDriver, "DBFFPT"  ) )
      uiDriver = 3;
   else if( ! strcmp( pUStru->szDriver, "SIXCDX"  ) )
      uiDriver = 4;
   else if( ! strcmp( pUStru->szDriver, "BMDBFCDX"  ) )
      uiDriver = 10;
   else if( ! strcmp( pUStru->szDriver, "BMDBFNTX"  ) )
      uiDriver = 11;
   else if( ! strcmp( pUStru->szDriver, "BMDBFNSX"  ) )
      uiDriver = 12;
   else
      uiDriver = 9;

   return uiDriver;
}

static void leto_Mgmt( PUSERSTRU pUStru, char * szData )
{
   char *  ptr = NULL;
   HB_BOOL bShow;
   char *  pp1 = NULL, * pp2 = NULL, * pp3 = NULL;
   int     nParam = leto_GetParam( szData, &pp1, &pp2, &pp3, NULL );

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
            PUSERSTRU  pUStru1;
            int        iTable = -1;
            HB_USHORT  ui, uiUsers;
            HB_I64     llTimePoint = leto_MilliSec() / 1000;
            char       szRequest[ 64 ];
            HB_ULONG   ulMemSize;
            PGLOBESTRU pGlobe = NULL;
            int        iListStart = 0;
            int        iListLen = 65535;
            HB_USHORT  uiCount = 0;
            char *     pData;

            if( nParam >= 2 )
            {
               iTable = atoi( pp1 );
               if( iTable < 0 )
                  iTable = -1;
               else
               {
                  HB_GC_LOCKT();
                  if( iTable < ( int ) s_uiTablesCurr )
                     pGlobe = s_tables[ iTable ].pGlobe;
                  HB_GC_UNLOCKT();
               }
            }
            if( nParam >= 3 )
            {
               const char * pTmp;

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
                        if( pAStru && pAStru->pTStru && pAStru->pTStru->pGlobe == pGlobe )
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
                                        leto_DriverID( pUStru1 ),
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
               const char * pTmp;

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
                           ptr += sprintf( ptr, "%d;%s;%lu;%s%s;%c;%s;%d;",
                                        uiTables, pAStru->pTStru->szTable, pAStru->ulSelectID,
                                        s_users[ iUser ].pCurAStru == pAStru ? "*" : "", pAStru->szAlias,
                                        pAStru->pTStru->bShared ? 'T' : 'F',
                                        pAStru->pTStru->szDriver, pAStru->pTStru->pGlobe->uMemoType );
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
                           ptr += sprintf( ptr, "%d;%s;0;;%c;%s;%d;",
                                        uiTables, ( char * ) pTStru1->szTable,
                                        pTStru1->bShared ? 'T' : 'F',
                                        pTStru1->szDriver, pTStru1->pGlobe->uMemoType );
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
            HB_USHORT       uiLocks = 0, uiCount = 0;
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
               const char * pTmp;

               if( ( pTmp = strchr( pp3, '<' ) ) != NULL )
                  iListLen = atoi( ++pTmp );
               if( iListLen < 1 )
                  iListLen = 1;
               else
                  iListLen = HB_MIN( 999, iListLen );
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
                  while( pListItem && uiCount < ( HB_USHORT ) iListLen )
                  {
                     pAStru = ( PAREASTRU ) ( pListItem + 1 );
                     if( pAStru && pAStru->pTStru && pAStru->pTStru->szTable &&
                         ( pp2 ? ! strcmp( ( const char * ) pAStru->pTStru->szTable, pp2 ) : HB_TRUE ) )
                     {
                        uiCount++;
                        if( ! pData )
                        {
                           ulMemSize = HB_PATH_MAX + 42;
                           pData = ( char * ) hb_xgrab( ulMemSize );
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
                              if( uiLocks++ > iListLen )
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
                              if( uiLocks++ > iListLen )
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
                        uiCount++;
                        if( ! pData )
                        {
                           ulMemSize = HB_PATH_MAX + 42;
                           pData = ( char * ) hb_xgrab( ulMemSize );
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
                           if( uiLocks++ > iListLen )
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
               const char * pTmp;

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

                                 hb_fsPipeWrite( pUStru1->hSockPipe[ 1 ], cToPipe, 1, 0 );
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
                                 hb_lz4netClose( ( PHB_LZ4NET ) pUStru1->zstream );
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

                              hb_fsPipeWrite( pUStru1->hSockPipe[ 1 ], cToPipe, 1, 0 );
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
                              hb_lz4netClose( ( PHB_LZ4NET ) pUStru1->zstream );
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
   const char *   pContain = strstr( szOnePath, szDataPath );
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

static void leto_Intro( PUSERSTRU pUStru, char * szData )
{
   const char * pData = NULL;
   char *       pp1 = NULL, * pp2 = NULL, * pp3 = NULL, * pp4 = NULL;
   char *       pp5 = NULL, * pp6 = NULL;
   int          nParam = leto_GetParam( szData, &pp1, &pp2, &pp3, &pp4, &pp5, &pp6, NULL );
   char         pBuf[ 256 ];

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

      strncpy( ( char * ) pUStru->szExename, pp2, 23 );  /* 24 bytes pre-filled '\0' */

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

            fAccepted = leto_acc_find( pUStru, szPass );
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
         const char * ptr7 = NULL;

         if( pp5 && *pp5 )
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
         }

         if( pp6 && *pp6 )
         {
            PHB_ITEM pItem = hb_itemNew( NULL );
            char *   pTmp;

            hb_itemPutL( pItem, *pp6++ == 'T' ? HB_TRUE : HB_FALSE );
            hb_setSetItem( HB_SET_SOFTSEEK, pItem );

            hb_itemPutL( pItem, *pp6++ == 'T' ? HB_TRUE : HB_FALSE );
            hb_setSetItem( HB_SET_DELETED, pItem );

            hb_itemPutL( pItem, *pp6++ == 'T' ? HB_TRUE : HB_FALSE );
            hb_setSetItem( HB_SET_AUTOPEN, pItem );

            hb_itemPutNI( pItem, strtoul( pp6, &pTmp, 10 ) );
            hb_setSetItem( HB_SET_AUTORDER, pItem );
            hb_itemRelease( pItem );

            ptr7 = pTmp;
            if( ptr7 )
               ptr7++;
         }

         if( ptr7 && *ptr7 )
         {
            const char *   ptr;
            PHB_ITEM pItem = NULL;

            if( ( ptr = strchr( ptr7, ';' ) ) != NULL )
            {
               pUStru->szDateFormat = ( char * ) hb_xgrab( ptr - ptr7 + 1 );
               memcpy( pUStru->szDateFormat, ptr7, ptr - ptr7 );
               pUStru->szDateFormat[ ptr - ptr7 ] = '\0';
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
                  const char * pEnd = strchr( ++ptr, ';' );
                  int          iLen = pEnd ? ( pEnd - ptr ) : 0;

                  if( iLen )
                  {
                     char * szOnePath = ( char * ) hb_xgrab( HB_PATH_MAX );
                     char * szDefaultPath = ( char * ) hb_xgrab(  HB_PATH_MAX );

                     memcpy( szOnePath, ptr, HB_MIN( iLen, HB_PATH_MAX - 1 ) );
                     szOnePath[ HB_MIN( iLen, HB_PATH_MAX - 1 ) ] = '\0';
                     leto_BeautifyPath( szOnePath );
                     leto_PathFinder( szOnePath, s_pDataPath );

                     iLen = ( HB_USHORT ) strlen( szOnePath );
                     if( iLen > 1 && szOnePath[ iLen - 1 ] == DEF_SEP )
                        szOnePath[ iLen - 1 ] = '\0';
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
                     iLen = strlen( ptr );
                  }
                  else
                  {
                     iLen = 0;
                     leto_writelog( NULL, 0, "ERROR leto_Intro() elch told you not to use old client lib .. :-)" );
                  }
                  if( iLen )
                  {
                     char *    szOnePath= ( char * ) hb_xgrab(  HB_PATH_MAX );
                     char *    szSearchPath;
                     HB_UCHAR  uPaths = 1;  /* first will become the letodb.ini DataPAth */
                     HB_USHORT uiLenOne;

                     pEnd = ptr;
                     while( pEnd - ptr < iLen )
                     {
                        uPaths++;
                        if( ( pEnd = strchr( pEnd, ';' ) ) == NULL )
                           break;
                        pEnd++;
                     }
                     szSearchPath = ( char * ) hb_xgrab( uPaths-- * HB_PATH_MAX );
                     strcpy( szSearchPath, s_pDataPath );
                     iLen = strlen( szSearchPath );
                     leto_StrTran( szSearchPath, DEF_CH_SEP, DEF_SEP, ( HB_SIZE ) iLen );
                     szSearchPath[ iLen++ ] = DEF_SEPPATH;
                     szSearchPath[ iLen ] = '\0';

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
                        iLen += leto_DataPath( szOnePath, szSearchPath + iLen );
                        if( uPaths > 1 )
                           szSearchPath[ iLen++ ] = DEF_SEPPATH;

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

         hb_snprintf( pBuf, 255, "+%c%c%c;%s;%s;%d;%d;%d;%d;%d;%c;%c;",
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
         letoSetUStru( pUStru );

         if( pUStru->hSocketErr == HB_NO_SOCKET && s_iDebugMode > 0 )  /* second socket comes also here */
            leto_writelog( NULL, -1, "INFO: connected  %s:%d %s CP: %s  DF: %s  conn-ID %d",
                                     pUStru->szAddr, pUStru->iPort,
                                     ( *( pUStru->szExename ) ? ( char * ) pUStru->szExename : "?exe?" ),
                                     hb_cdpID(), hb_setGetDateFormat(), pUStru->iUserStru - 1 );
         if( s_iDebugMode > 10 )
            leto_writelog( NULL, -1, "DEBUG leto_Intro() AUTO -open %d -order %d DEFAULT '%s' PATH '%s'",
                           ( int ) hb_setGetAutOpen(), hb_setGetAutOrder(), hb_setGetDefault(), hb_setGetPath() );
      }
   }

   leto_SendAnswer( pUStru, pData, strlen( pData ) );
}

static void leto_CloseT( PUSERSTRU pUStru, char * szData )
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
         pData = szOk;
         bOk = HB_TRUE;
         if( s_iDebugMode > 1 )
            leto_wUsLog( pUStru, -1, "DEBUG! leto_CloseT failed to close not opened WA: %lu", ulAreaID );
      }
   }

   if( ! pUStru->bBeQuiet )
      leto_SendAnswer2( pUStru, pData, 4, bOk, 1000 );

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
   else if( hb_parclen( 1 ) )
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

static void leto_CloseTall( PUSERSTRU pUStru, char * szData  )
{
   HB_SYMBOL_UNUSED( szData );

   HB_GC_LOCKU();  // ToDo verify lock is needed

   leto_CloseAll4Us( pUStru );
   leto_SendAnswer2( pUStru, szOk, 4, HB_TRUE, 1000 );

   HB_GC_UNLOCKU();
}

/* not used, client closes connection by socket shutdown */
static void leto_CloseUStru( PUSERSTRU pUStru, char * szData  )
{
   HB_SYMBOL_UNUSED( szData );

   pUStru->bCloseConnection = HB_TRUE;
   pUStru->bNoAnswer = HB_TRUE;
}

static void leto_Pack( PUSERSTRU pUStru, char * szData )
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

static void leto_Zap( PUSERSTRU pUStru, char * szData )
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

static void leto_Reccount( PUSERSTRU pUStru, char * szData )
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
static void leto_Set( PUSERSTRU pUStru, char * szData )
{
   char * pAreaID, * pSkipBuf = NULL;
   int    nParam = leto_GetParam( szData, &pAreaID, &pSkipBuf, NULL );

   if( nParam < 1 )   // changed protocol || *szData != '0' )
      leto_SendAnswer2( pUStru, szErr2, 4, HB_FALSE, 1000 );
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

         default:  /* SET #defines + 1000 */
            if( nParam >= 2 && iCmd > 1000 )
            {
               PHB_ITEM  pItem = hb_itemNew( NULL );

               if( *pAreaID == 'T' || *pAreaID == 'F' )
                  hb_itemPutL( pItem, *pAreaID == 'T' );
               else
                  hb_itemPutNI( pItem, atoi( pAreaID ) );
               hb_setSetItem( ( HB_set_enum ) ( iCmd - 1000 ), pItem );
               hb_itemRelease( pItem );
               if( s_iDebugMode >= 10 )
                  leto_wUsLog( pUStru, -1, "DEBUG leto_Set: Set( %d, %s )", iCmd - 1000, pAreaID );
            }
            break;
      }

      leto_SendAnswer2( pUStru, szOk, 4, HB_TRUE, 1000 );
   }
}

static void leto_Transaction( PUSERSTRU pUStru, char * szData )
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
                     pulAppends = ( HB_ULONG * ) hb_xrealloc( pulAppends, sizeof( HB_ULONG ) * iAppended * 2 );
                  else
                     pulAppends = ( HB_ULONG * ) hb_xgrab( sizeof( HB_ULONG ) * iAppended * 2 );
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
                  {
                     if( pArea == pTA[ i1 ].pArea && pTA[ i1 ].bAppend )
                     {
                        ulRecNo = pTA[ i1 ].ulRecNo;
                        break;
                     }
                  }
               }
               SELF_GOTO( pArea, ulRecNo );
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
                  else /* if( pTA[ i ].bLockable ) ) */  /* only register append lock */
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
               pResult = ( char * ) hb_xgrab( iAppended * 2 * 22 + 15 );
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
            {
               if( pTA[ i ].pItems[ i1 ] )
                  hb_itemRelease( pTA[ i ].pItems[ i1 ] );
            }
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

static void leto_Filter( PUSERSTRU pUStru, char * szData )
{
   AREAP     pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_ULONG  ulLen = strlen( szData + 2 );
   PAREASTRU pAStru = pUStru->pCurAStru;

   leto_ResetFilter( pAStru );
   if( s_bNoSaveWA && ! pAStru->pTStru->bMemIO )
      leto_ClearFilter( pArea );

   if( ! ulLen )
      leto_SendAnswer2( pUStru, szOk, 4, HB_TRUE, 1021 );
   else
   {
      HB_BOOL      bRes = HB_FALSE;
      HB_BOOL      bForce = ( *szData == 'T' );
      PHB_ITEM     pFilterBlock = NULL;
      const char * szFilter = szData + 2;

      /* pre-test filter for executable at server, and without alias */
      if( hb_setGetOptimize() )
         bRes = leto_ParseFilter( pUStru, szFilter, ulLen );

      if( bRes || bForce )
      {
         bRes = leto_ExprGetType( pUStru, szFilter, ulLen ) == 'L';
         if( bRes )
         {
            hb_xvmSeqBegin();
            pFilterBlock = leto_mkCodeBlock( pUStru, szFilter, ulLen, HB_FALSE );
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

/* set/ clear relation only for mode s_bNoSaveWA */
static void leto_Relation( PUSERSTRU pUStru, char * szData )
{
   HB_BOOL bFail = HB_FALSE;

   if( s_bNoSaveWA && ! pUStru->pCurAStru->pTStru->bMemIO )
   {
      switch( *( szData + 1 ) )
      {
         case '1':  /* set relation */
         {
            char * szTmp = szData;
            char * pp1 = NULL, * pp2 = NULL;
            int    iAreaChild;

            for( ;; )
            {
               if( leto_GetParam( szTmp, &pp1, &pp2, NULL ) < 3 )
                  break;

               iAreaChild = atoi( pp1 );
               if( iAreaChild < 1 && *pp1 )
                  hb_rddGetAliasNumber( pp1, &iAreaChild );

               if( iAreaChild > 0 && *pp2 )
               {
                  AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
                  AREAP pChildArea = ( AREAP ) hb_rddGetWorkAreaPointer( iAreaChild );

                  if( s_iDebugMode > 10 )
                  {
                     PAREASTRU pChildAStru = leto_FindArea( pUStru, ( HB_ULONG ) iAreaChild );

                     leto_wUsLog( pUStru, -1, "DEBUG leto_Relation %s->(%s) into %s",
                                              pUStru->pCurAStru->szAlias, pp2,
                                              pChildAStru ? pChildAStru->szAlias : "?" );
                  }

                  if( pArea && pChildArea )
                  {
                     DBRELINFO dbRelations;
                     PHB_ITEM  pAbKey = hb_itemPutC( NULL, pp2 );
                     PHB_ITEM  pRelBlock;

                     hb_xvmSeqBegin();
                     pRelBlock = leto_mkCodeBlock( pUStru, pp2, strlen( pp2 ), HB_FALSE );
                     if( pRelBlock != NULL )
                        hb_vmEvalBlock( pRelBlock );
                     hb_xvmSeqEnd();
                     if( pUStru->iHbError )
                        bFail = HB_TRUE;

                     if( bFail )
                     {
                        hb_itemRelease( pAbKey );
                        if( pRelBlock )
                           hb_itemRelease( pRelBlock );
                        SELF_CLEARREL( pArea );
                        if( s_iDebugMode > 10 )
                           leto_wUsLog( pUStru, 0, "DEBUG leto_Relation rejected!, CB not optimzed ");
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
                           bFail = HB_TRUE;
                           if( s_iDebugMode > 10 )
                              leto_wUsLog( pUStru, 0, "DEBUG leto_Relation rejected! at server" );
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

static void leto_GroupBy( PUSERSTRU pUStru, char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   const char * pOrder = szData;
   char       * pGroup, * pFields, * pFilter, * pFlag;
   HB_ULONG     ulDataLen = strlen( szData );

   if( ! pArea || leto_GetParam( szData, &pGroup, &pFields, &pFilter, &pFlag, NULL ) < 5 )
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
      VALGRP *     pSumFields = ( VALGRP * ) hb_xgrab( sizeof( VALGRP ) * uiAllocated );
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
         pGroupBlock = leto_mkCodeBlock( pUStru, pGroup, pFields - pGroup - 1, HB_TRUE );
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
               pBlock = leto_mkCodeBlock( pUStru, ptr, pNext - ptr, HB_TRUE );
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
            pFilterBlock = leto_mkCodeBlock( pUStru, pFilter, strlen( pFilter ), HB_FALSE );
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
            pData = ( char * ) hb_xgrab( ulSize );

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

static void leto_Sum( PUSERSTRU pUStru, char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   const char * pOrder = szData;
   char       * pFields, * pFilter, * pFlag;
   HB_ULONG     ulDataLen = strlen( szData );

   if( ! pArea || leto_GetParam( szData, &pFields, &pFilter, &pFlag, NULL ) < 4 )
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
               pSums[ uiCount - 1 ].pBlock = leto_mkCodeBlock( pUStru, ptr, pNext - ptr, HB_TRUE );
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
         pFilterBlock = leto_mkCodeBlock( pUStru, pFilter, strlen( pFilter ), HB_FALSE );
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
         char * pData = ( char * ) hb_xgrab( uiCount * 21 + 3 );
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

static const char * letoFillTransInfo( PUSERSTRU pUStru, LPDBTRANSINFO pTransInfo, const char * pData, AREAP pAreaSrc, AREAP pAreaDst )
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
         pTransInfo->dbsci.itmCobFor = leto_mkCodeBlock( pUStru, pp1, pp2 - pp1, HB_FALSE );
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
         pTransInfo->dbsci.itmCobWhile = leto_mkCodeBlock( pUStru, pp1, pp2 - pp1, HB_FALSE );
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

/* note: pAreaDst->fTransRec flag handled by client, also changes of DBS_COUNTER|STEP */
static void leto_Trans( PUSERSTRU pUStru, char * szData, HB_BOOL bSort )
{
   PAREASTRU    pAStru = pUStru->pCurAStru;
   AREAP        pAreaSrc, pAreaDst;
   HB_ULONG     ulAreaDst, ulRecNo;
   const char * pp1, * pp2, * pp3;
   char *       ptr1, * ptr2;

   if( leto_GetParam( szData, &ptr1, &ptr2, NULL ) < 3 )
      leto_SendAnswer( pUStru, szErr2, 4 );
   else
   {
      HB_ULONG ulCurAreaID = pUStru->ulCurAreaID;

      ulRecNo = strtoul( szData, NULL, 10 );
      if( pUStru->bDeleted != ( *ptr1 == 'T' ) )
      {
         pUStru->bDeleted = ( *ptr1 == 'T' );
         leto_setSetDeleted( pUStru->bDeleted );
      }
      ulAreaDst = strtoul( ptr2, NULL, 10 );
      pp3 = ptr2 + strlen( ptr2 ) + 1;

      pAreaSrc = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      pAreaDst = leto_SelectArea( pUStru, ulAreaDst );  /* may changes pUStru->ulCurAreaID; */

      if( pAreaDst && pAreaSrc )
      {
         HB_ERRCODE errCode;

         hb_xvmSeqBegin();
         if( bSort )
         {
            DBSORTINFO dbSortInfo;

            pp1 = letoFillTransInfo( pUStru, &dbSortInfo.dbtri, pp3, pAreaSrc, pAreaDst );

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
            errCode = SELF_SORT( pAreaSrc, &dbSortInfo );
            if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
               leto_ClearAreaEnv( pAreaSrc, pAStru->pTagCurrent );

            letoFreeTransInfo( &dbSortInfo.dbtri );

            if( dbSortInfo.lpdbsItem )
               hb_xfree( dbSortInfo.lpdbsItem );
         }
         else
         {
            DBTRANSINFO dbTransInfo;

            letoFillTransInfo( pUStru, &dbTransInfo, pp3, pAreaSrc, pAreaDst );

            if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
               leto_SetAreaEnv( pAStru, pAreaSrc, pUStru );
            leto_GotoIf( pAreaSrc, ulRecNo );
            errCode = SELF_TRANS( dbTransInfo.lpaSource, &dbTransInfo );
            if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
               leto_ClearAreaEnv( pAreaSrc, pAStru->pTagCurrent );

            letoFreeTransInfo( &dbTransInfo );
         }

         hb_xvmSeqEnd();

         if( pUStru->iHbError )
            leto_SendError( pUStru, szErr4, 4 );
         else if( errCode != HB_SUCCESS )
            leto_SendAnswer( pUStru, szErr4, 4 );
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
          * the destination area was detached by SendAnswer */
         leto_FreeArea( pUStru, ulCurAreaID, HB_FALSE );
      }
   }
}

static void leto_TransSort( PUSERSTRU pUStru, char * szData )
{
   leto_Trans( pUStru, szData, HB_TRUE );
}

static void leto_TransNoSort( PUSERSTRU pUStru, char * szData )
{
   leto_Trans( pUStru, szData, HB_FALSE );
}

/* the thread for 'headless' UDF threads */
static HB_THREAD_STARTFUNC( threadX )
{
   PUSERSTRU pUStru = ( PUSERSTRU ) Cargo;
   PHB_ITEM  pArray = NULL;
   HB_SIZE   nPCount = 0;
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
            HB_SIZE nIndex;

            nPCount = hb_arrayLen( pArray );
            for( nIndex = 1; nIndex <= nPCount; nIndex++ )
            {
               hb_vmPush( hb_arrayGetItemPtr( pArray, nIndex ) );
            }
         }

         hb_vmDo( ( HB_USHORT ) nPCount );
         hb_vmRequestRestore();
      }
      hb_xvmSeqEnd();
      if( ! pUStru->iHbError && s_iDebugMode > 1 )
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

static void leto_Udf( PUSERSTRU pUStru, char * szData, HB_ULONG ulAreaID )
{
   char *  pp2 = NULL, * pp3 = NULL, * pp4 = NULL, * pp5 = NULL, * pp6 = NULL;
   char *  ptr = NULL;
   int     nParam;
   HB_BYTE uCommand = ( ( HB_BYTE ) szData[ 0 ] & 0xFF ) - 48;

   if( ! uCommand || strlen( szData ) <  4 )
   {
      leto_wUsLog( pUStru, 0, "ERROR leto_Udf() no data about what to do ;-)" );
      leto_SendAnswer( pUStru, szErr1, 4 );
      return;
   }
   else if( ! s_bUdfEnabled )
   {
      if( s_iDebugMode > 0 )
         leto_wUsLog( pUStru, 0, "DEBUG leto_Udf() use is prohibited !" );
      leto_SendAnswer( pUStru, szErr3, 4 );
      return;
   }
   else if( ( s_bPass4D && ! ( pUStru->szAccess[ 0 ] & 0x4 ) ) )
   {
      leto_SendAnswer( pUStru, szErrAcc, 4 );
      return;
   }

   /* pp2 = 0x40|0x41; pp3 = ulRecNo; pp4 = func name; for uCommand 2,3 : pp5 = _SET_EXCLUSIVE; pp6 = size, then params */
   nParam = leto_GetParam( szData, &pp2, &pp3, &pp4, &pp5, &pp6, NULL );
   if( nParam < ( uCommand == 3 ? 2 : 6 ) || ! ( pp4 && *pp4 ) )
      leto_SendAnswer( pUStru, szErr2, 4 );
   else
   {
      PHB_DYNS pSym = hb_dynsymFindName( pp4 );
      PHB_ITEM pBlock = NULL;

      if( uCommand != 3 && uCommand != 9 && ! pSym )
         pBlock = leto_mkCodeBlock( pUStru, pp4, strlen( pp4 ), HB_TRUE );

      if( uCommand == 3 ) /* leto_udfexist */
      {
         char szData1[ 4 ];

         sprintf( szData1, "+%c;", ( pSym ) ? 'T' : 'F' );
         leto_SendAnswer( pUStru, szData1, 3 );
      }
      else if( pSym || pBlock )
      {
         HB_SIZE   nSize;
         PHB_ITEM  pArray = NULL;
         HB_SIZE   nPCount = 0;
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
               pTag = pAStru->pTagCurrent;
               if( ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
               {
                  if( pUStru->bDeleted != ( *pp2 == 0x41 ) )
                  {
                     pUStru->bDeleted = ( *pp2 == 0x41 );
                     leto_setSetDeleted( pUStru->bDeleted );
                  }
                  leto_SetAreaEnv( pAStru, pArea, pUStru );
               }
               leto_GotoIf( pArea, strtoul( pp3, NULL, 10 ) );
               pUStru->ulUdfAreaID = pUStru->ulCurAreaID;
            }
            else if( hb_rddGetCurrentWorkAreaPointer() )
            {
               pUStru->ulUdfAreaID = ( HB_ULONG ) hb_rddGetCurrentWorkAreaNumber();
               hb_rddSelectWorkAreaNumber( 0 );
            }

            nSize = strtoul( pp6, &ptr, 10 );
            if( nSize )
            {
               const char * ptrTmp = ++ptr;

               pArray = hb_itemDeserialize( &ptrTmp, &nSize );
            }

            if( hb_vmRequestReenter() )
            {
               hb_xvmSeqBegin();

               if( pSym )
               {
                  hb_vmPushDynSym( pSym );
                  hb_vmPushNil();
               }
               else  /* pBlock */
               {
                  hb_vmPushEvalSym();
                  hb_vmPush( pBlock );
               }

               if( pArray && HB_IS_ARRAY( pArray ) )
               {
                  HB_SIZE nIndex;

                  nPCount = hb_arrayLen( pArray );
                  for( nIndex = 1; nIndex <= nPCount; nIndex++ )
                  {
                     hb_vmPush( hb_arrayGetItemPtr( pArray, nIndex ) );
                  }
               }

               if( pSym )
                  hb_vmProc( ( HB_USHORT ) nPCount );
               else
                  hb_vmSend( ( HB_USHORT ) nPCount );

               hb_xvmSeqEnd();

               if( pArray )
                  hb_itemRelease( pArray );
               if( ! pUStru->iHbError )
               {
                  char *   pParam = hb_itemSerialize( hb_param( -1, HB_IT_ANY ),  /* not for HB_IT_POINTER */
                                                      HB_SERIALIZE_NUMSIZE, &nSize );
                  HB_ULONG ulLen = leto_CryptText( pUStru, pParam, nSize, 1 );

                  pUStru->pBufCrypt[ 0 ] = '+';    /* NEW: '+' prefix */
                  leto_SendAnswer( pUStru, ( char * ) pUStru->pBufCrypt, ulLen + 1 );
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
               if( ulAreaID )
                  leto_FreeArea( pUStru, ulAreaID, HB_TRUE );
               else if( pUStru->ulUdfAreaID )
               {
                  hb_rddSelectWorkAreaNumber( pUStru->ulUdfAreaID );
                  pUStru->ulUdfAreaID = 0;
               }
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
               if( pAStru && ! ( s_bNoSaveWA && ! pAStru->pTStru->bMemIO ) )
                  leto_ClearAreaEnv( pArea, pTag );
            }
         }
      }
      else
         leto_SendAnswer( pUStru, szErr2, 4 );

      if( pBlock )
         hb_vmDestroyBlockOrMacro( pBlock );
   }
}

static void leto_UdfFun( PUSERSTRU pUStru, char * szData )
{
   leto_Udf( pUStru, szData, 0 );
}

static void leto_UdfDbf( PUSERSTRU pUStru, char * szData )
{
   leto_Udf( pUStru, szData, pUStru->ulCurAreaID );
}

static void leto_Info( PUSERSTRU pUStru, char * szData )
{
   char * pp1 = NULL;

   if( leto_GetParam( szData, &pp1, NULL ) < 1 )
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

         case DBI_LOCKTEST:
         {
            PHB_ITEM pItem = hb_itemNew( NULL );
            HB_ULONG ulRecNo = strtoul( pp1, NULL, 10 );

            if( ulRecNo )
               pItem = hb_itemPutNI( pItem, ulRecNo );

            if( SELF_INFO( pArea, DBI_LOCKTEST, pItem ) == HB_SUCCESS )
            {
               char szData1[ 32 ];

               sprintf( szData1, "+%d;", hb_itemGetNI( pItem ) );
               leto_SendAnswer( pUStru, szData1, strlen( szData1 ) );
            }
            else
               leto_SendAnswer( pUStru, szErr1, 4 );
            hb_itemRelease( pItem );
            break;
         }

         case DBI_LOCKCOUNT:
         case DBI_GETLOCKARRAY:
         {
            LETO_LIST       LocksList;
            PLETO_LOCK_ITEM pLockA;
            char *          szData1, * ptr;
            int             iLen = 0;

#ifdef LETO_LOCKS_GLOBAL  /* not HB conform -- need mutex */
            HB_GC_LOCKT();
            LocksList = pUStru->pCurAStru->pTStru->LocksList;
            letoListLock( &LocksList );
#else
            LocksList = pUStru->pCurAStru->LocksList;
#endif

            for( pLockA = ( PLETO_LOCK_ITEM ) LocksList.pItem; pLockA; pLockA = pLockA->pNext )
            {
               iLen++;
            }

            ptr = szData1 = ( char * ) hb_xgrab( 24 );
            ptr += sprintf( szData1, "+%d;", iLen );

            if( uiCommand == DBI_GETLOCKARRAY && iLen )
            {
               szData1 = ( char * ) hb_xrealloc( szData1, ( iLen * 12 ) + 24 );
               ptr = szData1 + strlen( szData1 );

               for( pLockA = ( PLETO_LOCK_ITEM ) LocksList.pItem; pLockA; pLockA = pLockA->pNext )
               {
                  ptr += ultostr( pLockA->ulRecNo, ptr );
                  *ptr++ = ';';
               }
            }

#ifdef LETO_LOCKS_GLOBAL
            letoListUnlock( &LocksList );
            HB_GC_UNLOCKT();
#endif
            leto_SendAnswer( pUStru, szData1, ptr - szData1 );
            hb_xfree( szData1 );
            break;
         }

         case DBI_TRIGGER:
         {
            PHB_ITEM pItem = NULL;
            char     szData1[ HB_SYMBOL_NAME_LEN + 9 ];
            int      iLen;

            if( *pp1 && ! *s_pTrigger )
            {
               if( *pp1 == '.' )
                  hb_itemPutL( pItem, ( *( pp1 + 1 ) == 'T' ) );
               else if( strlen( pp1 ) <= HB_SYMBOL_NAME_LEN )
               {
                  PHB_DYNS pTrigger = hb_dynsymFindName( pp1 );

                  if( pTrigger && hb_dynsymIsFunction( pTrigger ) )
                     hb_itemPutC( pItem, pp1 );
               }
            }

            if( pItem )
            {
               SELF_INFO( pArea, DBI_TRIGGER, pItem );
               szData1[ 0 ] = '+';
               iLen = hb_itemGetCLen( pItem );
               if( iLen > 0 )
                  memcpy( szData1 + 1, hb_itemGetCPtr( pItem ), iLen );
               szData1[ iLen + 1 ] = ';';
               szData1[ iLen + 2 ] = '\0';

               leto_SendAnswer( pUStru, szData1, strlen( szData1 ) );
               hb_itemRelease( pItem );
            }
            else
               leto_SendAnswer( pUStru, szErr4, 4 );
            break;
         }

#ifndef __HARBOUR30__

         case DBI_TRANSREC:
         {
            if( *pp1 &&  *pp1 == '.' )
            {
               PHB_ITEM pItem = hb_itemNew( NULL );

               hb_itemPutL( pItem, ( *( pp1 + 1 ) == 'T' ) );
               SELF_INFO( pArea, DBI_TRANSREC, pItem );

               leto_SendAnswer( pUStru, szOk, 4 );
               hb_itemRelease( pItem );
            }
            else
               leto_SendAnswer( pUStru, szErr4, 4 );
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
#endif

         default:
            leto_SendAnswer( pUStru, szErr4, 4 );
      }
   }
}

static void leto_OrderInfo( PUSERSTRU pUStru, char * szData )
{
   AREAP        pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   char       * pOrder, * pOrdPar;
   const char * pData;
   char         szData1[ 20 ];
   int          nParam = leto_GetParam( szData, &pOrder, &pOrdPar, NULL );

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
            case DBOI_ISMULTITAG:
            case DBOI_MULTIKEY:
            case DBOI_ISCOND:
            case DBOI_ISDESC:
            case DBOI_CUSTOM:
            case DBOI_UNIQUE:
               pOrderInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
               if( nParam > 2 && pOrdPar[ 0 ] )
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
               if( nParam > 2 && pOrdPar[ 0 ] )
                  pOrderInfo.itmNewVal = hb_itemPutC( NULL, pOrdPar );
         }

         hb_xvmSeqBegin();
         SELF_ORDINFO( pArea, uiCommand, &pOrderInfo );
         hb_xvmSeqEnd();

         if( pUStru->iHbError )
            pOrderInfo.itmResult = hb_itemPutL( pOrderInfo.itmResult, HB_FALSE );

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

static void leto_Pong( PUSERSTRU pUStru, char * szData )
{
   HB_SYMBOL_UNUSED( szData );

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
      if( pAStru->pTStru->pGlobe == pTStru->pGlobe )  /* pAStru->ulAreaID == pTStru->ulAreaID */
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

/* mode 0 = a single, 1 = double found, 2 = DENY_ALL */
static HB_BOOL leto_SMBTest( const char * szFilename, int iMode )
{
   char *  pStdOutBuf = NULL;
   HB_SIZE nStdOut = 0;
   int     iResult;

   iResult = hb_fsProcessRun( "/usr/bin/smbstatus -L 2>/dev/null", NULL, 0, &pStdOutBuf, &nStdOut, NULL, 0, HB_FALSE );
   hb_fsSetFError( hb_fsError() );
   if( iResult || hb_fsGetFError() || ! pStdOutBuf )  /* not expected serious problem to execute above */
   {
      if( pStdOutBuf )
         hb_xfree( pStdOutBuf );
      if( iResult )
      {
         leto_writelog( NULL, 0, "ERROR leto_SMBTest() failed to correct execute: /usr/bin/smbstatus" );
         return HB_TRUE;
      }
      else
         return HB_FALSE;
   }
   else
   {
      HB_BOOL      fResult = HB_FALSE;
      char         szFile[ HB_PATH_MAX ];
      int          iLen, iStart;
      const char * ptr, * ptr1, * ptr2;

      if( ( ptr = strchr( szFilename, '.' ) ) != NULL )
      {
         /* cut away dot in first pos or ".." elks */
         if( ptr == szFilename || *( ptr + 1 ) == '.' )
         {
            if( *( ptr + 1 ) == '.' )
               szFilename = ptr + 2;
            else
               szFilename = ptr + 1;
            while( *szFilename == '/' || *szFilename == '\\' )
               szFilename++;
         }
      }
      hb_strncpy( szFile, szFilename, HB_MIN( HB_PATH_MAX - 1, strlen( szFilename ) ) );
      leto_BeautifyPath( szFile );

      if( ( iLen = strlen( szFile ) ) > 0 )
      {
         pStdOutBuf[ nStdOut ] = '\0';
         ptr1 = pStdOutBuf;
         ptr = leto_stristr( ptr1, szFile );
         if( ptr && iMode < 2 )
         {
            if( iMode == 1 )
               ptr = leto_stristr( ptr, szFile );
            if( ptr )
            {
               fResult = HB_TRUE;
               ptr = NULL;
            }
         }

         while( ptr )
         {
            iStart = HB_MIN( 99, ptr - ptr1 - iLen );
            fResult = ( ( ptr2 = strstr( ptr - iStart, "DENY_ALL" ) ) != NULL ) && ptr2 < ptr - iLen;
            if( fResult )
               break;
            else
            {
               ptr1 = ptr;
               ptr = leto_stristr( ptr1, szFile );
            }
         }
      }

      hb_xfree( pStdOutBuf );
      return fResult;
   }
}

/* mode 0 = single, 1 = double found */
static HB_BOOL leto_ELSOFtest( const char * szFilename, int iMode )
{
   char *  pStdOutBuf = NULL;
   HB_SIZE nStdOut = 0;
   char    szFile[ HB_PATH_MAX ];
   char    szCmd[ HB_PATH_MAX + 64 ];
   int     iStart = strchr( s_pSharePath, ':' ) - s_pSharePath + 1;
   int     iLen = strlen( s_pSharePath + iStart );
   int     iLenLen = strlen( szFilename + iLen );
   int     iResult;

   if( iLenLen + iStart < HB_PATH_MAX )
   {
      char * ptr, * pptr;
      int    iCount;

      memcpy( szFile, s_pSharePath, iStart - 1 );
      memcpy( szFile + iStart - 1, szFilename + iLen, iLenLen + 1 );  /* incl. '\0' */

      /* need to resolve ".." elks */
      while( ( ptr = strstr( szFile, ".." ) ) != NULL )
      {
         iCount = 0;
         pptr = ptr - 1;
         while( iCount < 2 && pptr > szFile )
         {
            if( *pptr-- == '/' )
               iCount++;
         }
         iCount = ( pptr - szFile ) + 1;
         iLenLen -= ( ptr + 1 - pptr ) + iCount;
         memmove( szFile + iCount, szFile + ( ptr + 1 - pptr ) + iCount, iStart + iLenLen );  /* incl. '\0' */
      }
      sprintf( szCmd, "/usr/bin/elsof %s%c", szFile, '\0' );
   }
   else
      return HB_TRUE;  /* too long, won't analyze -> don't open */

   iResult = hb_fsProcessRun( szCmd, NULL, 0, &pStdOutBuf, &nStdOut, NULL, 0, HB_FALSE );
   if( iResult || ! pStdOutBuf || ! nStdOut )
   {
      if( pStdOutBuf )
         hb_xfree( pStdOutBuf );
      if( iResult )  /* missing suid-root ? */
      {
         leto_writelog( NULL, 0, "ERROR leto_ELSOFtest() failed to correct execute: /usr/bin/elsof" );
         return HB_TRUE;
      }
      else
         return HB_FALSE;
   }
   else
   {
      HB_BOOL      fResult = HB_FALSE;
      const char * ptr;

      pStdOutBuf[ nStdOut ] = '\0';
      ptr = leto_stristr( pStdOutBuf, szFile );
      if( ptr && iMode < 2 )
      {
         if( iMode == 1 )
            ptr = leto_stristr( ptr, szFile );
         if( ptr )
            fResult = HB_TRUE;
      }

      hb_xfree( pStdOutBuf );
      return fResult;
   }
}

static void leto_OpenTable( PUSERSTRU pUStru, char * szRawData )
{
   char *       szFileRaw = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFileName = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFile = ( char * ) hb_xgrab( HB_PATH_MAX );
   char         szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   char         szRealAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   char         szDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];
   unsigned int uiReplyBufLen = 1023;
   char *       szReply = ( char * ) hb_xgrab( uiReplyBufLen + 1 );
   char *       ptr, * ptr2;
   const char * pData = NULL;
   char         * pp2, * pp3, * szCdp, * pp5;
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
   nParam = leto_GetParam( szRawData, &pp2, &pp3, &szCdp, &pp5, NULL );
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
         hb_strncpy( szAlias, pp2, HB_RDD_MAX_ALIAS_LEN );
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
         hb_strncpy( szAlias, ptr2, HB_RDD_MAX_ALIAS_LEN );
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

         if( ! hb_fileExists( szFileName, NULL ) && hb_fileExists( szFile, szFilePath ) )
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
                  leto_wUsLog( pUStru, -1, "DEBUG leto_OpenTable new dateformat %s set", hb_setGetDateFormat() );
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

            if( ( s_bNoSaveWA && ! bMemIO ) )
            {
               strcpy( szRealAlias, szAlias );
               ulAreaID = ulSelectID;
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
                                       szCdp, 0, NULL, NULL );
            hb_xvmSeqEnd();

            if( pUStru->iHbError )
               errcode = HB_FAILURE;
            else
               pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
            if( ! pArea )
               errcode = HB_FAILURE;

            /* very special query to ensure concurrency <exclusive> with CIFS using apps */
            if( s_bSMBServer && errcode == HB_SUCCESS )
            {
               if( ! bMemIO && ! ( ( s_bShareTables || s_bNoSaveWA ) && bShared && ! bMemIO ) )
               {
                  if( s_pSharePath ? leto_ELSOFtest( szFileName, 1 ) : leto_SMBTest( szFile, 1 ) )
                  {
                     hb_rddReleaseCurrentArea();
                     hb_rddSetNetErr( HB_TRUE );
                     errcode = HB_FAILURE;
                  }
                  if( s_iDebugMode > 1 && hb_rddGetNetErr() )
                     leto_wUsLog( pUStru, -1, "DEBUG leto_OpenTable(%s) collision with SMB", szFile );
               }
            }

            if( errcode != HB_SUCCESS )
            {
               if( ! s_bNoSaveWA || bMemIO )
                  leto_DelAreaID( szRealAlias );
               if( pUStru->iHbError == 32 || hb_rddGetNetErr() )
                  sprintf( szReply, "%s%s\t%s", szErr4, pUStru->szHbError ? pUStru->szHbError + 4 : ":21-1023-0-0", szFile );  /* EDBF_SHARED */
               else
                  sprintf( szReply, "%s%s\t%s", szErr3, pUStru->szHbError ? pUStru->szHbError + 4 : ":21-1001-0-0", szFile );  /* EDBF_OPEN_DBF */
               pUStru->ulCurAreaID = 0;
               pUStru->pCurAStru = NULL;
            }
            else
            {
               PHB_ITEM pItem = hb_itemPutNI( NULL, leto_lockScheme( uiNtxType ) );

               hb_setSetItem( HB_SET_DBFLOCKSCHEME, pItem );
               hb_itemRelease( pItem );

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
                  else if( *s_pTrigger )
                  {
                     PHB_ITEM pTrigger = hb_itemPutC( NULL, s_pTrigger );

                     SELF_INFO( pArea, DBI_TRIGGER, pTrigger );
                     hb_itemRelease( pTrigger );
                     /* ToDo: execute event EVENT_POSTUSE */
                  }
                  HB_GC_UNLOCKT();
                  bUnlocked = HB_TRUE;
                  if( errcode == HB_SUCCESS )
                     leto_InitArea( pUStru, iTableStru, ulAreaID, szAlias, szRealAlias, ulSelectID );
               }
               else
               {
                  if( ! s_bNoSaveWA || bMemIO )
                     leto_DelAreaID( szRealAlias );
                  pData = szErr3;
                  errcode = HB_FAILURE;
               }
            }
         }
         else
         {
            if( ! ( ( s_tables + iTableStru )->bShared ) || ! bShared )
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
                  pArea = leto_SelectArea( pUStru, ulAreaID );
                  if( ! pArea )
                     errcode = HB_FAILURE;
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
         SELF_INFO( pArea, DBI_LASTUPDATE, pFldDec );
         ptr += ultostr( hb_itemGetDL( pFldDec ), ptr );
         *ptr++ = ';';
         SELF_FIELDCOUNT( pArea, &uiFieldCount );
         ptr += eprintf( ptr, "%u", uiFieldCount );
         *ptr++ = ';';
         ulLenLen = ptr - szReply;

         /* estimate max needed length -- see leto_recLen() for explanation .. */
         SELF_INFO( pArea, DBI_GETRECSIZE, pFldLen );
         ulRecLen = hb_itemGetNL( pFldLen );
         if( ( ulLen = ulLenLen + ulRecLen + 25 + ( uiFieldCount * 30 ) ) > uiReplyBufLen )
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

         if( hb_setGetAutOpen() && leto_ProdSupport( szDriver ) )
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

   if( ( pUStru->bRpcMode || s_bNoSaveWA ) && uiArea && hb_parclen( 2 ) )
   {
      char         szFile[ HB_PATH_MAX ];
      const char * szDriver = HB_ISCHAR( 1 ) ? hb_parc( 1 ) : pUStru->szDriver;
      const char * szAlias = HB_ISCHAR( 3 ) ? hb_parc( 3 ) : "";
      HB_BOOL      fShared = HB_ISLOG( 4 ) ? hb_parl( 4 ) : ! pUStru->bSetExclusive;
      HB_BOOL      fReadOnly = HB_ISLOG( 5 ) ? hb_parl( 5 ) : HB_FALSE;
      const char * szCdpage = hb_parclen( 6 ) ? hb_parc( 6 ) : NULL;
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
static void leto_SetMemoEnv( const char * szDriver, int iMemoType, int iMemoBlocksize, const char * szExt )
{
   HB_USHORT uiRddID;
   LPRDDNODE pRDDNode = hb_rddFindNode( szDriver, &uiRddID );

   if( pRDDNode )
   {
      PHB_ITEM pItem = NULL;

      if( ! iMemoType && ! iMemoBlocksize && ! szExt )  /* reset to defaults */
      {
         LPDBFDATA pData = DBFNODE_DATA( pRDDNode );

         memset( pData->szMemoExt, '\0', sizeof( pData->szMemoExt ) );
         pData->ulMemoBlockSize = 0;
         pData->bMemoType = '\0';

         pItem = hb_itemPutNI( pItem, 0 );
         hb_setSetItem( HB_SET_MBLOCKSIZE, pItem );
         pItem = hb_itemPutC( pItem, NULL );
         hb_setSetItem( HB_SET_MFILEEXT, pItem );
         hb_itemRelease( pItem );

         return;
      }

      if( iMemoBlocksize )
      {
         pItem = hb_itemPutNI( pItem, iMemoBlocksize );
         SELF_RDDINFO( pRDDNode, RDDI_MEMOBLOCKSIZE, 0, pItem );
      }

      if( szExt && strlen( szExt ) )
      {
         pItem = hb_itemPutC( pItem, szExt );
         SELF_RDDINFO( pRDDNode, RDDI_MEMOEXT, 0, pItem );
         hb_itemClear( pItem );
      }

      if( iMemoType )
      {
         pItem = hb_itemPutNI( pItem, iMemoType );
         SELF_RDDINFO( pRDDNode, RDDI_MEMOTYPE, 0, pItem );
      }

      if( pItem )
         hb_itemRelease( pItem );
    }
}

static void leto_CreateTable( PUSERSTRU pUStru, char * szRawData )
{
   char *       szFileRaw = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFileName = ( char * ) hb_xgrab(  HB_PATH_MAX );
   char *       szFile = ( char * ) hb_xgrab( HB_PATH_MAX );
   char         szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   char         szRealAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   char         szDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];
   unsigned int uiReplyBufLen = 1023;
   char *       szReply = ( char * ) hb_xgrab( uiReplyBufLen + 1 );
   char *       ptr, * ptr2, * ptrTmp;
   const char * szCdp = NULL;
   const char * pData = NULL;
   const char * szMemoExt = NULL;
   char *       pp2 = NULL, * pp3 = NULL, * pp4, * pp5;
   int          nParam;
   int          iMemoType, iMemoBlocksize;
   AREAP        pArea;
   HB_BOOL      bLeadSep, bMemIO;
   HB_BOOL      bTemporary = HB_FALSE, bTempTable = HB_FALSE;
   HB_BOOL      bKeepOpen = HB_TRUE;  /* default, Harbour (client) will send afterwards a close */
   HB_BOOL      bUnlocked = HB_FALSE;
   HB_SIZE      nLen;
   PHB_FNAME    pFilePath;
   HB_USHORT    ui, uiFieldCount, uiNtxType;
   HB_ERRCODE   errcode = HB_SUCCESS;
   PHB_ITEM     pFields;
   HB_ULONG     ulLenLen = 4;
   HB_ULONG     ulAreaID = 0;
   HB_ULONG     ulSelectID = 0;
   HB_ULONG     ulRecordSize = 0;

   szReply[ 0 ] = '\0';
   pUStru->bLastAct = HB_FALSE;
   nParam = leto_GetParam( szRawData, &pp2, &pp3, &pp4, NULL );
   nLen = strlen( szRawData );

   if( leto_IsServerLock( pUStru ) )
      strcpy( szReply, szErrLck );
   else if( nParam < 4 || ( nLen < 1 && ! s_bNoSaveWA ) || ! pp3 || strlen( pp3 ) < 2 )
      strcpy( szReply, szErr2 );
   else
   {
      if( nLen < 1 )  /* temporary table without name */
      {
         bTemporary = HB_TRUE;
         strcpy( szFileRaw, "tempfile.dbf" );  /* later replaced */
         nLen = strlen( szFileRaw );
      }
      else
      {
         memcpy( szFileRaw, szRawData, nLen );
         szFileRaw[ nLen ] = '\0';
      }
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
         hb_strncpy( szAlias, pp2, HB_RDD_MAX_ALIAS_LEN );
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
         hb_strncpy( szAlias, ptr2, HB_RDD_MAX_ALIAS_LEN );
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

      iMemoType = ( int ) strtol( pp4, &ptrTmp, 10 );
      ptrTmp++;
      if( *ptrTmp == '.' )
         szMemoExt = ptrTmp;
      ptrTmp = strchr( ptrTmp, ';' );
      *ptrTmp = '\0';
      pp4 = ++ptrTmp;
      iMemoBlocksize = ( int ) strtol( pp4, &ptrTmp, 10 );
      pp4 = ++ptrTmp;

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
               szCdp = pp5;
            pp5 = ptrTmp + 1;
         }
      }

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

         if( ! bTemporary && ptrTmp && *( ptrTmp + 2 ) == ';' && *( ptrTmp + 1 ) == 'T' )
            bTempTable = HB_TRUE;
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
         if( ! bTemporary && leto_FindTable( szFile, NULL ) >= 0 )
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
         HB_BOOL   bRestore = HB_FALSE;

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

         if( *szDriver && ( iMemoType || iMemoBlocksize || szMemoExt ) )
         {
            leto_SetMemoEnv( szDriver, iMemoType, iMemoBlocksize, szMemoExt );
            bRestore = HB_TRUE;
         }

         hb_xvmSeqBegin();
         if( bTemporary )
            errcode = hb_rddCreateTableTemp( szDriver, szRealAlias, szCdp, 0 /*ulConnection*/, pFields );
         else
            errcode = hb_rddCreateTable( szFileName, szDriver, uiArea, szRealAlias, bKeepOpen,
                                         szCdp, 0 /*ulConnection*/, pFields, NULL /* pDelim */ );
         hb_xvmSeqEnd();
         if( pUStru->iHbError )
            errcode = HB_FAILURE;

         if( bRestore )
            leto_SetMemoEnv( szDriver, 0, 0, NULL );
         if( errcode == HB_SUCCESS )
         {
            pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
            if( bTemporary )  /* unnamed temp */
            {
               if( ! s_bNoSaveWA )
               {
                  ulAreaID = ( HB_ULONG ) hb_rddGetCurrentWorkAreaNumber();
                  uiArea = ( HB_USHORT ) ulAreaID;
               }
               if( *( ( ( DBFAREAP ) pArea )->szDataFileName ) )
                  strcpy( szFile, ( ( DBFAREAP ) pArea )->szDataFileName );
            }
            else if( bTempTable )  /* named temp */
            {
               /* SELF_INFO( pArea, DBI_ISTEMPORARY, pItem ) -> create tempname */
               ( ( DBFAREAP ) pArea )->fTemporary = HB_TRUE;    /* so the! hack */
            }
         }

         if( errcode != HB_SUCCESS )
         {
            if( ! s_bNoSaveWA || bMemIO )
               leto_DelAreaID( szRealAlias );
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
               if( bTemporary || bTempTable )
                  ( s_tables + iTableStru )->bTemporary = HB_TRUE;
               HB_GC_UNLOCKT();
               bUnlocked = HB_TRUE;
               if( ! ulSelectID )
                  ulSelectID = hb_rddGetCurrentWorkAreaNumber();
               leto_InitArea( pUStru, iTableStru, ulAreaID, szAlias, szRealAlias, ulSelectID );
            }
            else
            {
               if( ! s_bNoSaveWA || bMemIO )
                  leto_DelAreaID( szRealAlias );
               pData = szErr3;
               errcode = HB_FAILURE;
            }

            if( errcode == HB_SUCCESS )
            {
               PHB_ITEM pItem = hb_itemNew( NULL );

               hb_itemPutNI( pItem, leto_lockScheme( uiNtxType ) );
               hb_setSetItem( HB_SET_DBFLOCKSCHEME, pItem );

               if( *s_pTrigger )
               {
                  pItem = hb_itemPutC( pItem, s_pTrigger );
                  pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
                  SELF_INFO( pArea, DBI_TRIGGER, pItem );
                  /* ToDo: execute event EVENT_POSTUSE */
               }

               hb_itemRelease( pItem );
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
         PHB_ITEM pItem = hb_itemNew( NULL );

         pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

         ptr = szReply;
         *ptr++ = '+';
         ptr += ultostr( ulAreaID, ptr );
         *ptr++ = ';';
         *ptr++ = ( char ) ( '0' + uiNtxType );  /* LETO_NTX = 1, LETO_CDX = 0 */
         pUStru->uiDriver = uiNtxType;
         *ptr++ = ';';
         ptr += leto_MemoInfo( pArea, ptr );
         SELF_INFO( pArea, DBI_LASTUPDATE, pItem );
         ptr += ultostr( hb_itemGetDL( pItem ), ptr );
         hb_itemRelease( pItem );
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

   if( ( pUStru->bRpcMode || s_bNoSaveWA ) && uiArea && hb_parclen( 1 ) && HB_ISARRAY( 2 )  )
   {
      char         szFile[ HB_PATH_MAX ];
      PHB_ITEM     pStruct = hb_param( 2, HB_IT_ARRAY );
      const char * szDriver = HB_ISCHAR( 3 ) ? hb_parc( 3 ) : pUStru->szDriver;
      HB_BOOL      bKeepOpen = HB_ISLOG( 4 ) ? hb_parl( 4 ) : HB_FALSE;
      const char * szAlias = HB_ISCHAR( 5 ) ? hb_parc( 5 ) : "";
      const char * szCdpage = hb_parclen( 6 ) ? hb_parc( 6 ) : NULL;
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
      sprintf( szData, "%s;%s;%s;%d;%s;%d;%d;%s;%d;%s;%s", szFile, szAlias, szDriver,  /* LETOCMD_creat */
               0, "", 0,  /* defaults of RDD: memotype, meoblocksize, memoext */
               uiFields, szFields, uiArea, szCdpage ? szCdpage : "", hb_setGetDateFormat() );

      leto_CreateTable( pUStru, szData );
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

static void leto_OpenIndex( PUSERSTRU pUStru, char * szRawData )
{
   char *       szFileRaw = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFileName = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFile = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szBagName = ( char * ) hb_xgrab( HB_PATH_MAX );
   unsigned int uiReplyBufLen = 1023;
   char *       szReply = ( char * ) hb_xgrab( uiReplyBufLen + 1 );
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

      szRawData = ptr;
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

      /* add relative path_to_table if raw index filename given without path */
      if( ! bMemIO && ! bLeadSep && ! strchr( ptr2, DEF_SEP ) &&
          pUStru->pCurAStru && strchr( ( char * ) pUStru->pCurAStru->pTStru->szTable, DEF_SEP ) )
      {
         PHB_FNAME pDbfPath = hb_fsFNameSplit( ( char * ) pUStru->pCurAStru->pTStru->szTable );

         if( pDbfPath->szPath )
            strcpy( szFile, pDbfPath->szPath );
         else
            szFile[ 0 ] = '\0';
         strcpy( szFile + strlen( szFile ), ptr2 );
         hb_xfree( pDbfPath );
      }
      else
         strcpy( szFile, ptr2 );
      strcpy( ptr, szFile );

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

         if( ! hb_fileExists( szFileName, NULL ) && hb_fileExists( szFile, szFilePath ) )
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
               else if( pTag )
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
   const char * szBagName = hb_parclen( 1 ) ? hb_parc( 1 ) : "";
   HB_ULONG     ulAreaID = 0;
   const char * szAlias = NULL;
   char         szData[ 32 ];
   HB_BOOL      bRet = HB_FALSE;

   if( HB_ISNUM( 2 ) && hb_parni( 2 ) > 0 )
      ulAreaID = ( HB_ULONG ) hb_parni( 2 );
   else if( hb_parclen( 2 ) )
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

static void leto_CreateIndex( PUSERSTRU pUStru, char * szRawData )
{
   char *       szFileRaw = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFileName = ( char * ) hb_xgrab( HB_PATH_MAX );
   char *       szFile = ( char * ) hb_xgrab( HB_PATH_MAX );
   char         szTagName[ LETO_MAX_TAGNAME + 1 ];
   char         szDefaultExt[ HB_MAX_FILE_EXT + 1 ];
   unsigned int uiReplyBufLen = 1023;
   char *       szReply = ( char * ) hb_xgrab( uiReplyBufLen + 1 );
   char *       ptr, * ptr2, * ptr3;
   const char * pData = NULL;
   char *       pp2, * pp3, * pp4, * pp5, * szFor, * szWhile;
   AREAP        pArea;
   HB_ERRCODE   errcode = HB_FAILURE;
   HB_BOOL      bLeadSep, bMemIO, bUnique, bAll, bRest, bDescend, bCustom, bAdditive, bTemporary, bExclusive, bFilter;
   HB_BOOL      bUseCur = HB_FALSE;
   HB_SIZE      nLen;
   HB_ULONG     ulRecNo, ulNext, ulRecord;
   HB_ULONG     ulAreaID;
   HB_ULONG     ulLenLen = 4;
   HB_ULONG     ulRawLen = pUStru->ulDataLen - 2;

   szReply[ 0 ] = '\0';
   pUStru->bLastAct = HB_FALSE;
   ulAreaID = strtoul( szRawData, &ptr, 10 );
   if( leto_IsServerLock( pUStru ) )
      strcpy( szReply, szErrLck );
   else if( ! ulAreaID || ! ptr || *ptr++ != ';' )
      strcpy( szReply, szErr2 );
   else if( ( pArea = leto_SelectArea( pUStru, ulAreaID ) ) == NULL )
      strcpy( szReply, szErr3 );
   else if( leto_GetParam( ptr, &pp2, &pp3, &pp4, &szFor, &szWhile, &pp5, NULL ) < 7 )
      strcpy( szReply, szErr2 );
   else if( ( ( nLen = strlen( ptr ) ) < 1 && strlen( pp2 ) < 1 ) || strlen( pp3 ) < 1 )
      strcpy( szReply, szErr2 );  /* ptr = BagName; pp2 = TagName, pp3 = key expression */
   else
   {
      PHB_FNAME pFilePath;

      errcode = HB_SUCCESS;
      ulRawLen -= ptr - szRawData;
      szRawData = ptr;

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

      /* add relative path_to_table if raw index filename given without path */
      if( ! bMemIO && ! bLeadSep && ! strchr( ptr2, DEF_SEP ) &&
          pUStru->pCurAStru && strchr( ( char * ) pUStru->pCurAStru->pTStru->szTable, DEF_SEP ) )
      {
         PHB_FNAME pDbfPath = hb_fsFNameSplit( ( char * ) pUStru->pCurAStru->pTStru->szTable );

         if( pDbfPath->szPath )
            strcpy( szFile, pDbfPath->szPath );
         else
            szFile[ 0 ] = '\0';
         strcpy( szFile + strlen( szFile ), ptr2 );
         hb_xfree( pDbfPath );
      }
      else
         strcpy( szFile, ptr2 );
      strcpy( ptr, szFile );

      leto_RddiGetValue( pUStru->pCurAStru->pTStru->szDriver, RDDI_ORDBAGEXT, szDefaultExt );
      if( ! pFilePath->szExtension )
      {
         if( s_bLowerPath )
            hb_strLower( szDefaultExt, strlen( szDefaultExt ) );
         pFilePath->szExtension = szDefaultExt;
         strcpy( szFile + strlen( szFile ), szDefaultExt );
         strcpy( szFileName + strlen( szFileName ), szDefaultExt );
      }
      /* non default file extensions allowed ? */
      if( ! s_bAnyExt && leto_stricmp( pFilePath->szExtension, szDefaultExt ) )
         errcode = HB_FAILURE;

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

      bUnique = ( *pp4 == 'T' ? HB_TRUE : HB_FALSE );
      bAll = ( *pp5 == 'T' ? HB_TRUE : HB_FALSE );
      ulRecNo = strtoul( pp5 + 2, &ptr3, 10 );
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
      if( pUStru->pCurAStru->pTStru->bTemporary )
         bTemporary = HB_TRUE;
      else if( s_bNoSaveWA )
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
         char * szUseOrder = pp4;

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
               lpdbOrdCondInfo->itmCobFor = leto_mkCodeBlock( pUStru, szFor, strlen( szFor ), HB_FALSE );
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
               lpdbOrdCondInfo->itmCobWhile = leto_mkCodeBlock( pUStru, szWhile, strlen( szWhile ), HB_FALSE );
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
            HB_BOOL           bLocked = HB_FALSE;

            if( pTStru->bShared && ! ( bTemporary || bExclusive ) )
            {
               HB_GC_LOCKT();
               bLocked = HB_TRUE;
            }

            /* check if index Bag in use by other -- different check for s_bNoSaveWA */
            if( bTemporary || bExclusive )
            {
               if( s_iDebugMode > 15 )
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
               /* dbOrderInfo.itmCobExpr = leto_mkCodeBlock( pUStru, pp3, strlen( pp3 ), HB_FALSE ); */
               dbCreateInfo.lpdbConstraintInfo = NULL;

               hb_rddSetNetErr( HB_FALSE );

               /* after long miles of preparation, here it comes: CREATE the index ;-) */
               hb_xvmSeqBegin();
               errcode = SELF_ORDCREATE( pArea, &dbCreateInfo );
               hb_xvmSeqEnd();

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

                  if( ! bLocked )
                  {
                     HB_GC_LOCKT();
                     bLocked = HB_TRUE;
                  }

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

            if( bLocked )
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

   if( hb_parclen( 1 ) )
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

   if( hb_parclen( 2 ) || hb_parclen( 4 ) )  /* else BAGname or TAGname */
      bRet = HB_TRUE;
   if( ! hb_parclen( 3 ) )  /* key forgotten ;-) */
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
      leto_CreateIndex( pUStru, szData );
      bRet = pUStru->bLastAct;
      hb_xfree( szData );
   }

   hb_retl( bRet );
}

static void leto_UdfReload( PUSERSTRU pUStru, char * szData )
{
   PHB_DYNS pSym = hb_dynsymFindName( "LETO_UDFRELOAD" );

   if( pSym )
   {
      int iLen = strlen( szData );

      HB_GC_LOCKT();

      hb_vmPushDynSym( pSym );
      hb_vmPushNil();
      if( iLen )
      {
         hb_vmPushString( szData, iLen );
         hb_vmDo( 1 );
      }
      else
         hb_vmDo( 0 );

      HB_GC_UNLOCKT();
   }

   pUStru->bNoAnswer = HB_TRUE;
}

static void leto_StopServer( PUSERSTRU pUStru, char * szData )
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
   s_cmdSet[ LETOCMD_admin   - LETOCMD_OFFSET ] = leto_Admin;
   s_cmdSet[ LETOCMD_close   - LETOCMD_OFFSET ] = leto_CloseT;         /* HB_GC_LOCKT() */
   s_cmdSet[ LETOCMD_closall - LETOCMD_OFFSET ] = leto_CloseTall;      /* HB_GC_LOCKU() + HB_GC_LOCKT() */
   s_cmdSet[ LETOCMD_creat   - LETOCMD_OFFSET ] = leto_CreateTable;    /* HB_GC_LOCKT() + leto_FreeArea() */
   s_cmdSet[ LETOCMD_creat_i - LETOCMD_OFFSET ] = leto_CreateIndex;    /* leto_FreeArea() */
   s_cmdSet[ LETOCMD_drop    - LETOCMD_OFFSET ] = leto_Drop;           /* HB_GC_LOCKT() */
   s_cmdSet[ LETOCMD_exists  - LETOCMD_OFFSET ] = leto_Exists;
   s_cmdSet[ LETOCMD_file    - LETOCMD_OFFSET ] = leto_FileFunc;       /* ToDo: verify if a HB_GC_LOCKO() is needed */
   s_cmdSet[ LETOCMD_intro   - LETOCMD_OFFSET ] = leto_Intro;
   s_cmdSet[ LETOCMD_mgmt    - LETOCMD_OFFSET ] = leto_Mgmt;
   s_cmdSet[ LETOCMD_open    - LETOCMD_OFFSET ] = leto_OpenTable;      /* HB_GC_LOCKT() + leto_FreeArea() */
   s_cmdSet[ LETOCMD_open_i  - LETOCMD_OFFSET ] = leto_OpenIndex;      /* leto_FreeArea() */
   s_cmdSet[ LETOCMD_ping    - LETOCMD_OFFSET ] = leto_Pong;
   s_cmdSet[ LETOCMD_rddinfo - LETOCMD_OFFSET ] = leto_RddInfo;
   s_cmdSet[ LETOCMD_rename  - LETOCMD_OFFSET ] = leto_Rename;         /* HB_GC_LOCKT() */
   s_cmdSet[ LETOCMD_set     - LETOCMD_OFFSET ] = leto_Set;
   s_cmdSet[ LETOCMD_stop    - LETOCMD_OFFSET ] = leto_StopServer;
   s_cmdSet[ LETOCMD_quit    - LETOCMD_OFFSET ] = leto_CloseUStru;
   s_cmdSet[ LETOCMD_ta      - LETOCMD_OFFSET ] = leto_Transaction;
   s_cmdSet[ LETOCMD_udf_fun - LETOCMD_OFFSET ] = leto_UdfFun;         /* without workarea */
   s_cmdSet[ LETOCMD_udf_rel - LETOCMD_OFFSET ] = leto_UdfReload;      /* HB_GC_LOCKT() */
   s_cmdSet[ LETOCMD_var     - LETOCMD_OFFSET ] = leto_Variables;      /* HB_GC_LOCKV() */
   s_cmdSet[ LETOCMD_zip     - LETOCMD_OFFSET ] = leto_ToggleZip;      /* toggle [LZ4|zip] compress level -1-9 */

   /* a - z + more */
   s_cmdSet[ LETOCMD_add     - LETOCMD_OFFSET ] = leto_UpdateRecAdd;
   s_cmdSet[ LETOCMD_cmta    - LETOCMD_OFFSET ] = leto_UpdateRecAddflush;
   s_cmdSet[ LETOCMD_dbi     - LETOCMD_OFFSET ] = leto_Info;
   s_cmdSet[ LETOCMD_dboi    - LETOCMD_OFFSET ] = leto_OrderInfo;
   s_cmdSet[ LETOCMD_dbeval  - LETOCMD_OFFSET ] = leto_Dbeval;
   s_cmdSet[ LETOCMD_flush   - LETOCMD_OFFSET ] = leto_Flush;
   s_cmdSet[ LETOCMD_goto    - LETOCMD_OFFSET ] = leto_Goto;
   s_cmdSet[ LETOCMD_group   - LETOCMD_OFFSET ] = leto_GroupBy;
   s_cmdSet[ LETOCMD_islock  - LETOCMD_OFFSET ] = leto_IsRecLockedUS;
   s_cmdSet[ LETOCMD_lock    - LETOCMD_OFFSET ] = leto_Lock;
   s_cmdSet[ LETOCMD_memo    - LETOCMD_OFFSET ] = leto_MemoRaw;
   s_cmdSet[ LETOCMD_ord     - LETOCMD_OFFSET ] = leto_Ordfunc;
   s_cmdSet[ LETOCMD_pack    - LETOCMD_OFFSET ] = leto_Pack;
   s_cmdSet[ LETOCMD_rcou    - LETOCMD_OFFSET ] = leto_Reccount;
   s_cmdSet[ LETOCMD_rela    - LETOCMD_OFFSET ] = leto_Relation;
   s_cmdSet[ LETOCMD_scop    - LETOCMD_OFFSET ] = leto_Scope;
   s_cmdSet[ LETOCMD_filt    - LETOCMD_OFFSET ] = leto_Filter;
   s_cmdSet[ LETOCMD_skip    - LETOCMD_OFFSET ] = leto_Skip;
   s_cmdSet[ LETOCMD_sort    - LETOCMD_OFFSET ] = leto_TransSort;
   s_cmdSet[ LETOCMD_seek    - LETOCMD_OFFSET ] = leto_Seek;
   s_cmdSet[ LETOCMD_sum     - LETOCMD_OFFSET ] = leto_Sum;
   s_cmdSet[ LETOCMD_trans   - LETOCMD_OFFSET ] = leto_TransNoSort;
   s_cmdSet[ LETOCMD_unlock  - LETOCMD_OFFSET ] = leto_Unlock;
   s_cmdSet[ LETOCMD_upd     - LETOCMD_OFFSET ] = leto_UpdateRecUpd;
   s_cmdSet[ LETOCMD_cmtu    - LETOCMD_OFFSET ] = leto_UpdateRecUpdflush;
   s_cmdSet[ LETOCMD_udf_dbf - LETOCMD_OFFSET ] = leto_UdfDbf;         /* with ulAreaID */
   s_cmdSet[ LETOCMD_zap     - LETOCMD_OFFSET ] = leto_Zap;
}

/* the beloved heart: central command dispatcher */
HB_BOOL leto_ParseCommand( PUSERSTRU pUStru )
{
   const unsigned int iCmd = *pUStru->pBuffer - LETOCMD_OFFSET;

   if( iCmd < LETOCMD_SETLEN && s_cmdSet[ iCmd ] )
   {
      char * ptrPar = ( char * ) ( pUStru->pBuffer + 2 );  /* datasegment after LETOCMD_* + ';' */

      if( iCmd > 'Z' - LETOCMD_OFFSET )
      {
         HB_ULONG ulAreaID = strtoul( ptrPar, &ptrPar, 10 );

         if( ! ulAreaID || *ptrPar++ != ';' )
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
      else  /* _no_ workarea pre-selected before execute */
         ( *s_cmdSet[ iCmd ] )( pUStru, ptrPar );

      if( ! pUStru->bNoAnswer && ! pUStru->ulBytesSend )
         return HB_FALSE;  /* something badly failed, no expected answer was send */

      return HB_TRUE;
   }

   /* unknown request, should not happen */
   return HB_FALSE;
}

