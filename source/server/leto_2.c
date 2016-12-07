/*
 * Leto db server core function, Harbour mt model
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 *           2014-16 Rolf 'elch' Beckmann
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

#if defined( HB_OS_WIN_32 ) || defined( HB_OS_WIN )
   #include <winsock2.h>
   #include <windows.h>
#endif

#include "srvleto.h"

/* for counting statistic */
#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   static HB_SPINLOCK_T s_StatsMtx = HB_SPINLOCK_INIT;
   #define HB_GC_LOCKS()    HB_SPINLOCK_ACQUIRE( &s_StatsMtx )
   #define HB_GC_UNLOCKS()  HB_SPINLOCK_RELEASE( &s_StatsMtx )
#else
   static HB_CRITICAL_NEW( s_StatsMtx );
   #define HB_GC_LOCKS()    hb_threadEnterCriticalSection( &s_StatsMtx )
   #define HB_GC_UNLOCKS()  hb_threadLeaveCriticalSection( &s_StatsMtx )
#endif

/* for exchanging port number of second socket from main thread to thread3 */
#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   static HB_SPINLOCK_T s_SocksMtx = HB_SPINLOCK_INIT;
   #define HB_GC_LOCKX()    HB_SPINLOCK_ACQUIRE( &s_SocksMtx )
   #define HB_GC_UNLOCKX()  HB_SPINLOCK_RELEASE( &s_SocksMtx )
#else
   static HB_CRITICAL_NEW( s_SocksMtx );    // also use for s_bLockLock
   #define HB_GC_LOCKX()       hb_threadEnterCriticalSection( &s_SocksMtx )
   #define HB_GC_UNLOCKX()     hb_threadLeaveCriticalSection( &s_SocksMtx )
#endif

#define GC_RATE   0xFFF
#define LETO_CPU_STATISTIC  1

static const char * szOk = "++++";
static const char * szErr1 = "-001";

static HB_SOCKET s_hSocketMain = HB_NO_SOCKET;
static HB_UINT * s_paSocks;
static int       s_iSocksMax = 0;
static int       s_iTimeOut = -1;
static int       s_iZombieCheck = 0;     /* dead connection check time interval */
static char **   s_szCmdSetDesc = NULL;

/* statistics */
static HB_U64 s_ullOperations = 0;
static HB_U64 s_ullUDFOps = 0;
static HB_U64 s_ullBytesRead = 0;
static HB_U64 s_ullBytesSend = 0;
static HB_U64 s_ullCPULoad = 0;          /* sum in ms of CPU load for requests */


extern HB_USHORT leto_ActiveUser( void );
extern HB_USHORT leto_MaxUsers( void );
extern char leto_CryptTraf( void );
extern int iDebugMode( void );
extern const char * leto_sDirBase( void );
extern HB_BOOL leto_ConnectIsLock( void );

extern PUSERSTRU leto_InitUS( HB_SOCKET hSocket );
extern HB_BOOL leto_SeekUS( int iServerPort, HB_SOCKET hSocket );
extern void leto_CloseUS( PUSERSTRU pUStru );
extern HB_BOOL leto_ParseCommand( PUSERSTRU pUStru );
extern PUSERSTRU leto_FindUserStru( HB_THREAD_ID hThreadID );
extern void leto_wUsLog( PUSERSTRU pUStru, int n, const char * s, ... );
extern void leto_CloseAllSocket( void );
extern void leto_ForceCloseAllSocket( void );
extern int leto_PingForZombies( long lTimeInactive );
extern void leto_initSet( void );
extern void leto_ReallocUSbuff( PUSERSTRU pUStru, HB_ULONG ulNewsize );
extern void leto_FreeCurrArea( PUSERSTRU pUStru );

extern void leto_CommandSetInit( void );

extern HB_I64 leto_MilliSec( void );
extern void leto_setTimeout( HB_ULONG iTimeOut );


HB_U64 leto_Statistics( int iEntry )
{
   HB_U64 ullRet;

   HB_GC_LOCKS();
   if( iEntry == 1 )
      ullRet = s_ullOperations;
   else if( iEntry == 2 )
      ullRet = s_ullBytesRead;
   else if( iEntry == 3 )
      ullRet = s_ullBytesSend;
   else if( iEntry == 4 )
      ullRet = s_ullCPULoad / 1000;
   else
      ullRet = 0;
   HB_GC_UNLOCKS();

   return ullRet;
}

static void leto_CommandDescInit( void )
{
   if( ! s_szCmdSetDesc )
      s_szCmdSetDesc = ( char ** ) hb_xgrabz( sizeof( char * ) * LETOCMD_SETLEN );

   /* @ A - Z */
   s_szCmdSetDesc[ LETOCMD_admin   - LETOCMD_OFFSET ] = "admin";
   s_szCmdSetDesc[ LETOCMD_close   - LETOCMD_OFFSET ] = "close";
   s_szCmdSetDesc[ LETOCMD_closall - LETOCMD_OFFSET ] = "closeall";
   s_szCmdSetDesc[ LETOCMD_creat   - LETOCMD_OFFSET ] = "create";
   s_szCmdSetDesc[ LETOCMD_creat_i - LETOCMD_OFFSET ] = "create_index";
   s_szCmdSetDesc[ LETOCMD_drop    - LETOCMD_OFFSET ] = "drop";
   s_szCmdSetDesc[ LETOCMD_exists  - LETOCMD_OFFSET ] = "exists";
   s_szCmdSetDesc[ LETOCMD_file    - LETOCMD_OFFSET ] = "file";
   s_szCmdSetDesc[ LETOCMD_intro   - LETOCMD_OFFSET ] = "intro";
   s_szCmdSetDesc[ LETOCMD_mgmt    - LETOCMD_OFFSET ] = "management";
   s_szCmdSetDesc[ LETOCMD_open    - LETOCMD_OFFSET ] = "open";
   s_szCmdSetDesc[ LETOCMD_open_i  - LETOCMD_OFFSET ] = "open_index";
   s_szCmdSetDesc[ LETOCMD_ping    - LETOCMD_OFFSET ] = "ping";
   s_szCmdSetDesc[ LETOCMD_rddinfo - LETOCMD_OFFSET ] = "rddinfo";
   s_szCmdSetDesc[ LETOCMD_rename  - LETOCMD_OFFSET ] = "rename";
   s_szCmdSetDesc[ LETOCMD_set     - LETOCMD_OFFSET ] = "set";
   s_szCmdSetDesc[ LETOCMD_sql     - LETOCMD_OFFSET ] = "sql";
   s_szCmdSetDesc[ LETOCMD_stop    - LETOCMD_OFFSET ] = "shutdown";
   s_szCmdSetDesc[ LETOCMD_quit    - LETOCMD_OFFSET ] = "logout";
   s_szCmdSetDesc[ LETOCMD_ta      - LETOCMD_OFFSET ] = "transaction";
   s_szCmdSetDesc[ LETOCMD_udf_fun - LETOCMD_OFFSET ] = "udf_function";
   s_szCmdSetDesc[ LETOCMD_udf_rel - LETOCMD_OFFSET ] = "udf_reload";
   s_szCmdSetDesc[ LETOCMD_var     - LETOCMD_OFFSET ] = "variable";
   s_szCmdSetDesc[ LETOCMD_zip     - LETOCMD_OFFSET ] = "togglezip";

   /* a - z + more */
   s_szCmdSetDesc[ LETOCMD_add     - LETOCMD_OFFSET ] = "append";
   s_szCmdSetDesc[ LETOCMD_cmta    - LETOCMD_OFFSET ] = "commit_append";
   s_szCmdSetDesc[ LETOCMD_cmtu    - LETOCMD_OFFSET ] = "commit_update";
   s_szCmdSetDesc[ LETOCMD_dbi     - LETOCMD_OFFSET ] = "dbinfo";
   s_szCmdSetDesc[ LETOCMD_dboi    - LETOCMD_OFFSET ] = "dborderinfo";
   s_szCmdSetDesc[ LETOCMD_flush   - LETOCMD_OFFSET ] = "flush";
   s_szCmdSetDesc[ LETOCMD_goto    - LETOCMD_OFFSET ] = "goto";
   s_szCmdSetDesc[ LETOCMD_group   - LETOCMD_OFFSET ] = "group";
   s_szCmdSetDesc[ LETOCMD_islock  - LETOCMD_OFFSET ] = "islocked";
   s_szCmdSetDesc[ LETOCMD_lock    - LETOCMD_OFFSET ] = "lock";
   s_szCmdSetDesc[ LETOCMD_memo    - LETOCMD_OFFSET ] = "memo";
   s_szCmdSetDesc[ LETOCMD_ord     - LETOCMD_OFFSET ] = "order";
   s_szCmdSetDesc[ LETOCMD_pack    - LETOCMD_OFFSET ] = "pack";
   s_szCmdSetDesc[ LETOCMD_rcou    - LETOCMD_OFFSET ] = "reccount";
   s_szCmdSetDesc[ LETOCMD_rela    - LETOCMD_OFFSET ] = "relation";
   s_szCmdSetDesc[ LETOCMD_scop    - LETOCMD_OFFSET ] = "scope";
   s_szCmdSetDesc[ LETOCMD_filt    - LETOCMD_OFFSET ] = "setfilter";
   s_szCmdSetDesc[ LETOCMD_skip    - LETOCMD_OFFSET ] = "skip";
   s_szCmdSetDesc[ LETOCMD_sort    - LETOCMD_OFFSET ] = "sort";
   s_szCmdSetDesc[ LETOCMD_seek    - LETOCMD_OFFSET ] = "seek";
   s_szCmdSetDesc[ LETOCMD_sum     - LETOCMD_OFFSET ] = "sum";
   s_szCmdSetDesc[ LETOCMD_trans   - LETOCMD_OFFSET ] = "transition";
   s_szCmdSetDesc[ LETOCMD_unlock  - LETOCMD_OFFSET ] = "unlock";
   s_szCmdSetDesc[ LETOCMD_upd     - LETOCMD_OFFSET ] = "update";
   s_szCmdSetDesc[ LETOCMD_udf_dbf - LETOCMD_OFFSET ] = "udf_dbf";
   s_szCmdSetDesc[ LETOCMD_zap     - LETOCMD_OFFSET ] = "zap";
}

static void leto_CommandDescFree( void )
{
   if( s_szCmdSetDesc )
      hb_xfree( s_szCmdSetDesc );
}

static const char * leto_CmdToHuman( const char szLetoCmd )
{
   const int iCmd = szLetoCmd - LETOCMD_OFFSET;

   if( iCmd < 0 || iCmd > LETOCMD_SETLEN - 1 || ! s_szCmdSetDesc[ iCmd ] )  /* unknown request */
      return "?";
   else
      return s_szCmdSetDesc[ iCmd ];
}

void leto_errInternal( HB_ULONG ulIntCode, const char * szText, const char * szPar1, const char * szPar2 )
{
   FILE *     hLog;
   PUSERSTRU  pUStru;
   char       sFileDef[ HB_PATH_MAX ];
   HB_BOOL    fStack = hb_stackId() != NULL;
   char       file[ HB_PATH_MAX ];
   char       buffer[ 8192 ];
   HB_USHORT  uiLine;

   leto_writelog( NULL, 0, "!!!!! leto_errInternal !!!!!" );

   pUStru = leto_FindUserStru( HB_THREAD_SELF() );
   strcpy( sFileDef, leto_sDirBase() );
   strcpy( sFileDef + strlen( sFileDef ), "letodb_crashf.log" );

   hLog = hb_fopen( sFileDef, "a+" );
   if( hLog )
   {
      char szTime[ 9 ];
      int  iYear, iMonth, iDay;

      hb_dateToday( &iYear, &iMonth, &iDay );
      hb_dateTimeStr( szTime );

      fprintf( hLog, HB_I_( "Breakdown at: %04d.%02d.%02d %s.%03d\n" ), iYear, iMonth, iDay, szTime, ( int ) ( hb_dateMilliSeconds() % 1000 ) );
      fprintf( hLog, "Unrecoverable error %lu: ", ulIntCode );
      if( szText )
         fprintf( hLog, "%s %s %s\n", szText, szPar1, szPar2 );
      else
         fprintf( hLog, "no description available\n" );

      fprintf( hLog, "------------------------------------------------------------------------\n" );
      if( pUStru )
      {
         fprintf( hLog, "User: %s %s %s  connection %d\n", pUStru->szAddr, pUStru->szNetname, pUStru->szExename, pUStru->iUserStru - 1 );
         if( pUStru->pBuffer )
            fprintf( hLog, "Command: %s <%s>\n", leto_CmdToHuman( *pUStru->pBuffer ), ( char * ) pUStru->pBuffer );
         if( pUStru->pCurAStru && pUStru->pCurAStru->pTStru )
            fprintf( hLog, "Table: %s\n", pUStru->pCurAStru->pTStru->szTable );
      }

      buffer[ 0 ] = '\0';
      if( fStack && hb_stackTotalItems() )
      {
         int iLevel = 0;

         while( hb_procinfo( iLevel++, buffer, &uiLine, file ) )
         {
            char msg[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

            hb_snprintf( msg, sizeof( msg ), "Called from %s(%hu)%s%s\n", buffer, uiLine, *file ? " in " : "", file );
            fprintf( hLog, "%s", msg );
         }
      }
      fprintf( hLog, "\n\n" );

      fclose( hLog );
   }

   /* we do not close possible open/ detached tables here */
}

/*
 * elch: all threads will read, only one set it for shutdown --> no mutex
 * Note: cannot use hb_vmRequestQuery(): thread3 is without HVM; also used in Windows leto_ServiceControlHandler()
 */
int leto_ExitGlobal( HB_BOOL fExit )
{
   static int s_iGlobalExit = 0;

   if( fExit )
      s_iGlobalExit = 1;

   return s_iGlobalExit;
}

void leto_SrvShutDown( unsigned int uiWait )
{
   leto_ExitGlobal( HB_TRUE );
   hb_socketShutdown( s_hSocketMain, HB_SOCKET_SHUT_RDWR );
   if( uiWait )
      hb_idleSleep( uiWait );
}

#if defined( HB_OS_WIN )
#  define LETO_SOCK_GETERROR()       WSAGetLastError()
#  define LETO_SOCK_IS_EINTR( err )  ( ( err ) == WSAEINTR )
#elif defined( HB_OS_OS2 ) && defined( __WATCOMC__ )
#  define LETO_SOCK_GETERROR()       sock_errno()
#  define LETO_SOCK_IS_EINTR( err )  ( ( err ) == EINTR )
#else
#  define LETO_SOCK_GETERROR()       errno
#  define LETO_SOCK_IS_EINTR( err )  ( ( err ) == EINTR )
#endif

static HB_ULONG leto_SockSend( HB_SOCKET hSocket, const char * pBuf, HB_ULONG ulLen, PHB_ZNETSTREAM zstream, int flags )
{
   HB_ULONG ulSend = 0;
   long     lTmp;
#ifndef USE_LZ4
   long     lLast = 1;
#endif

#ifdef USE_LZ4
   HB_SYMBOL_UNUSED( zstream );
#endif

   // hb_vmUnlock();

#if defined( MSG_NOSIGNAL )
   flags |= MSG_NOSIGNAL;
#endif

   do
   {
#ifndef USE_LZ4
      if( zstream )
         lTmp = hb_znetWrite( zstream, hSocket, pBuf + ulSend, ulLen - ulSend, s_iTimeOut, &lLast );
      else
#endif
      {
#if 1
         lTmp = send( hSocket, ( const char * ) ( pBuf + ulSend ), ulLen - ulSend, flags );
         if( lTmp < 1 )
         {
            if( LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
               continue;
            lTmp = 0;
         }
#else
         /* attention: hb_socketSend() leaves with with hb_vmlock() */
         lTmp = lLast = hb_socketSend( hSocket, pBuf + ulSend, ulLen - ulSend, flags, s_iTimeOut );
#endif
      }

      if( lTmp > 0 )
         ulSend += lTmp;
#ifndef USE_LZ4
      else if( zstream ? ( lLast <= 0 || hb_socketGetError() != HB_SOCKET_ERR_TIMEOUT ) : ! LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
         break;
#else
      else if( LETO_SOCK_GETERROR() )
         break;
#endif
   }
   while( ulSend < ulLen );

   // hb_vmLock();

#ifndef USE_LZ4
   if( zstream && lLast > 0 )
   {
#if defined( __HARBOUR30__ )  /* 26.08.2015 new param --> hb_sockexFlush() */
      if( hb_znetFlush( zstream, hSocket, -1 ) != 0 )
#else
      if( hb_znetFlush( zstream, hSocket, -1, HB_FALSE ) != 0 )
#endif
      {
         leto_writelog( NULL, 0, "DEBUG not flushed" );
         if( hb_znetError( zstream ) != 0 )
            leto_writelog( NULL, 0, "DEBUG znet error" );
         ulSend = 0;
      }
   }
#endif  /* ! USE_LZ4 */

   return ulSend;
}

#ifdef USE_LZ4
static HB_ULONG leto_SockRecv( HB_SOCKET hSocket, char * pBuf, HB_ULONG ulLen )
#else
static HB_ULONG leto_SockRecv( HB_SOCKET hSocket, char * pBuf, HB_ULONG ulLen, PUSERSTRU pUStru )
#endif
{
   HB_ULONG ulRead = 0;
   HB_BOOL  bDoNotWait = HB_FALSE;
   long     lTmp;

   //hb_vmUnlock();  // done by caller

   do
   {
#ifndef USE_LZ4
      if( pUStru && pUStru->zstream )  // IS EMPTY with stopping server call
         lTmp = hb_znetRead( pUStru->zstream, hSocket, pBuf + ulRead, ulLen - ulRead, -1 );
      else
#endif
      {
         lTmp = recv( hSocket, pBuf + ulRead, ulLen - ulRead, 0 );
         if( lTmp < 1 )
         {
            if( LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
               continue;
            lTmp = 0;
         }
      }

      if( lTmp > 0 )
      {
         ulRead += lTmp;
         bDoNotWait = HB_FALSE;
      }
      else  /* missing data not ready here, wait max approx 0.5 sec for more */
      {
         int  iChange;

         if( LETO_SOCK_GETERROR() == 0 && ! bDoNotWait )
         {
#if 1 && defined( HB_HAS_POLL )
            struct pollfd pPoll[ 1 ];

            pPoll[ 0 ].fd = hSocket;
            pPoll[ 0 ].events = POLLIN | POLLRDNORM;
            bDoNotWait = HB_TRUE;
            do
            {
               iChange = poll( pPoll, 1, 500 );
               if( iChange < 0 && LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
                  continue;
            }
            while( HB_FALSE );

            if( iChange < 0 )
            {
                  leto_writelog( NULL, 0, "ERROR leto_SockRecv() poor poll error " );
                  break;
            }
            else if( pPoll[ 0 ].revents & ( POLLERR | POLLHUP | POLLNVAL ) )
               break;
            else if( iChange == 0 )
               break;
#else
            struct timeval MicroWait;
            fd_set readfds;

            bDoNotWait = HB_TRUE;
            do
            {
               MicroWait.tv_sec = 0;
               MicroWait.tv_usec = 500000;
               FD_ZERO( &readfds );
               FD_SET( hSocket, &readfds );
               iChange = select( ( int ) hSocket + 1, &readfds, NULL, NULL, &MicroWait );
               if( iChange < 0 && LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
                  continue;
            }
            while( HB_FALSE );

            if( iChange < 0 )
            {
               leto_writelog( NULL, 0, "ERROR leto_SockRecv() poor select error " );
               break;
            }
            else if( iChange == 0 )
               break;
#endif
         }
         else  /* after one time waiting, nothing more received --> give up */
            break;
      }
   }
   while( ulRead < ulLen );

   //hb_vmLock();  done by caller

   pBuf[ ulRead ] = '\0';
   return ulRead;
}

/* connection health check */
HB_BOOL leto_AskAnswer( HB_SOCKET hSocket )
{
   HB_BOOL bIsAlive = HB_FALSE;
   char szData[ 8 ];

   szData[ 4 ] = '+';
   szData[ 5 ] = LETOCMD_ping;
   szData[ 6 ] = ';';
   szData[ 7 ] = '\0';
   HB_PUT_LE_UINT32( szData, 3 );

   if( leto_SockSend( hSocket, szData, 7, NULL, 0 ) == 7 )
   {
      char *   pBuffer;
      HB_ULONG ulRecvLen;

      hb_vmUnlock();
#ifdef USE_LZ4
      ulRecvLen = leto_SockRecv( hSocket, szData, LETO_MSGSIZE_LEN );
#else
      ulRecvLen = leto_SockRecv( hSocket, szData, LETO_MSGSIZE_LEN, NULL );
#endif

      if( ulRecvLen == LETO_MSGSIZE_LEN )
      {
         ulRecvLen = HB_GET_LE_UINT32( szData );
         if( ulRecvLen > 0 && ulRecvLen < 1024 )
         {
            pBuffer = ( char * ) hb_xalloc( ulRecvLen + 1 );
#ifdef USE_LZ4
            if( leto_SockRecv( hSocket, pBuffer, ulRecvLen ) == ulRecvLen && ulRecvLen > 1 )
#else
            if( leto_SockRecv( hSocket, pBuffer, ulRecvLen, NULL ) == ulRecvLen && ulRecvLen > 1 )
#endif
            {
               if( pBuffer[ 0 ] == '+' && pBuffer[ 1 ] == '@' )
                  bIsAlive = HB_TRUE;
            }
            hb_xfree( pBuffer );
         }
      }
      hb_vmLock();
   }

   return bIsAlive;
}

/* for delayed errors send at second socket -- or answer at main socket if no second available */
void leto_SendAnswer2( PUSERSTRU pUStru, char * szData, HB_ULONG ulLen, HB_BOOL bAllFine, int iError )
{
   HB_SOCKET      hSocket = HB_NO_SOCKET;
   PHB_ZNETSTREAM zstream = NULL;
   HB_BOOL        bDelayedError = HB_FALSE;

   if( pUStru->hSocketErr != HB_NO_SOCKET )
   {
      if( bAllFine )
         pUStru->bNoAnswer = HB_TRUE;
      else
      {
         hSocket = pUStru->hSocketErr;
         bDelayedError = HB_TRUE;
      }
   }
   else
   {
      hSocket = pUStru->hSocket;
      zstream = pUStru->zstream;
   }

/*
 * extern HB_EXPORT PHB_ITEM  hb_errRT_New( HB_USHORT uiSeverity,  ES_WARNING, ES_ERROR
 *                                          const char * szSubSystem,  HB_ERR_SS_DBCMD
 *                                          HB_ERRCODE errGenCode,   EG_SYNTAX, EG_UNLOCKED
 *                                          HB_ERRCODE errSubCode,   val(szdata)
 *                                          const char * szDescription, const char * leto_CmdToHuman( const char szLetoCmd )
 *                                          const char * szOperation,   pUStru->szLastRequest
 *                                          HB_ERRCODE uiOsCode,  hb_fsOsError()
 *                                          HB_USHORT uiFlags );   EF_CANRETRY, EF_CANSUBSTITUTE
 */

   if( ! pUStru->bNoAnswer )
   {
      if( pUStru->ulSndBufLen < ulLen + LETO_MSGSIZE_LEN + 1 )
      {
         if( ! pUStru->ulSndBufLen || ! pUStru->pSendBuffer )
         {
            pUStru->ulSndBufLen = HB_MAX( ulLen + LETO_MSGSIZE_LEN, LETO_SENDRECV_BUFFSIZE );
            pUStru->pSendBuffer = ( HB_BYTE * ) hb_xgrab( pUStru->ulSndBufLen + 1 );
         }
         else
         {
            pUStru->ulSndBufLen = ulLen + LETO_MSGSIZE_LEN;
            pUStru->pSendBuffer = ( HB_BYTE * ) hb_xrealloc( pUStru->pSendBuffer, pUStru->ulSndBufLen + 1 );
         }
      }

      if( bDelayedError )
      {
         char     szOperation[ 256 ];
         char     szCommand[ 128 ];
         char     szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
         HB_ULONG ulElapse = 0;
         char *   ptr2;
         HB_ULONG ulBufLen = 127;
         HB_I64   llTmp = leto_MilliSec();

         if( iError < 0 )
            iError *= -1;
         if( iError > 9999 )
            iError %= 10000;

         /* how much *whole seconds* old is the failure event ? */
         if( ( llTmp < 0 ) == ( pUStru->llLastAct < 0 ) )  /* same signed */
            ulElapse = ( HB_ULONG ) ( ( llTmp / 1000 ) - pUStru->llLastAct );

         /* was there an alias available ? */
         if( pUStru->pCurAStru && pUStru->pCurAStru->szAlias )
            strcpy( szAlias, pUStru->pCurAStru->szAlias );
         else
            szAlias[ 0 ] = '\0';

         /* prepare the first 128 chars of the failed command: replace ';' with ':' */
         if( pUStru->ulDataLen < 127 )
            ulBufLen = pUStru->ulDataLen;
         memcpy( szCommand, pUStru->pBuffer, ulBufLen );
         szCommand[ ulBufLen ] = '\0';

         ptr2 = szCommand;
         ptr2 = strchr( ptr2, ';' );
         while( ptr2 )
         {
            *ptr2++ = ':';
            ptr2 = strchr( ptr2, ';' );
         }

         /* prepare a human readable description for the operation */
         szOperation[ 0 ] = '\0';
         if( ulElapse )  /* older command */
            sprintf( szOperation, " TIME <%lu>, ", ulElapse );
         if( *szAlias )
            sprintf( szOperation + strlen( szOperation ), "ALIAS (%s), ", szAlias );
         if( ulBufLen )
            sprintf( szOperation + strlen( szOperation ), "COMMAND (%s)", szCommand );
         else
            szOperation[ strlen( szOperation ) ] = '\0';

         /* and combine all above infos */
         ulLen = sprintf( ( char * ) ( pUStru->pSendBuffer + LETO_MSGSIZE_LEN ),
                          "----;%d;%s;%d;%d;%s;%s;%d;%d;",
                          ES_ERROR,
                          HB_ERR_SS_DBCMD,
                          iError == 1000 ? EG_SYNTAX : iError,
                          atoi( szData ),
                          leto_CmdToHuman( *pUStru->pBuffer ),
                          szOperation,
                          hb_fsOsError(),
                          EF_CANRETRY | EF_CANDEFAULT );
         HB_PUT_LE_UINT32( ( char * ) pUStru->pSendBuffer, ulLen );
         ulLen += LETO_MSGSIZE_LEN;
         pUStru->pSendBuffer[ ulLen ] = '\0';
      }
      else
      {
         /* these minor bytes will ever fit into sendbuffer -- no bufferlen check */
         HB_PUT_LE_UINT32( ( char * ) pUStru->pSendBuffer, ulLen );
         memcpy( ( char * ) pUStru->pSendBuffer + LETO_MSGSIZE_LEN, szData, ulLen );
         ulLen += LETO_MSGSIZE_LEN;
         pUStru->pSendBuffer[ ulLen ] = '\0';
      }

      pUStru->ulBytesSend = leto_SockSend( hSocket, ( char * ) pUStru->pSendBuffer, ulLen, zstream, 0 );

      if( bDelayedError )
      {
         if( pUStru->ulBytesSend )
         {
            leto_wUsLog( pUStru, -1, "DEBUG delayed error send: %s",
                         ( const char * ) ( pUStru->pSendBuffer + LETO_MSGSIZE_LEN ) );
         }
         else
         {
            //pUStru->hSocketErr = HB_NO_SOCKET;
            leto_wUsLog( pUStru, 0, "ERROR could not send error message at extra socket !" );
         }
      }
   }
}

void leto_SendAnswer( PUSERSTRU pUStru, const char * szData, HB_ULONG ulLen )
{
#if ! defined( MSG_MORE )
   HB_BOOL bUseBuffer = HB_TRUE;
#else
   HB_BOOL bUseBuffer = pUStru->zstream ? HB_TRUE : HB_FALSE;
#endif

   /* free the area before sending the answer */
   if( ! pUStru->bBeQuiet && pUStru->pCurAStru )
       leto_FreeCurrArea( pUStru );

   if( iDebugMode() >= 15 )  /* debug feedback */
   {
      if( iDebugMode() < 20 )
         leto_wUsLog( pUStru, -1, "DEBUG answer send %lu bytes", ulLen );
      else
      {
         HB_ULONG ulData = ( ulLen > 2048 ) ? 1024 : ulLen;

         leto_wUsLog( pUStru, ulData, szData );
      }
   }

   /* ensure buffer size for pre-leading LETO_MSGSIZE_LEN plus possible crypt/ compress extras */
   if( bUseBuffer && pUStru->ulSndBufLen < ulLen + 17 )
   {
      if( ! pUStru->ulSndBufLen || ! pUStru->pSendBuffer )
      {
         pUStru->ulSndBufLen = HB_MAX( ulLen + 17, LETO_SENDRECV_BUFFSIZE );
         pUStru->pSendBuffer = ( HB_BYTE * ) hb_xgrab( pUStru->ulSndBufLen + 1 );
      }
      else
      {
         pUStru->ulSndBufLen = ulLen + 17;
         pUStru->pSendBuffer = ( HB_BYTE * ) hb_xrealloc( pUStru->pSendBuffer, pUStru->ulSndBufLen + 1 );
      }
   }

   hb_vmUnlock();

   if( pUStru->zstream )
   {
#ifndef USE_LZ4
      HB_PUT_LE_UINT32( pUStru->pSendBuffer, ulLen );
      memcpy( pUStru->pSendBuffer + LETO_MSGSIZE_LEN, szData, ulLen );
      ulLen += LETO_MSGSIZE_LEN;
      pUStru->ulBytesSend = ulLen;
#else
      pUStru->ulBytesSend = ulLen + LETO_MSGSIZE_LEN;
      ulLen = hb_lz4netEncrypt( pUStru->zstream, ( char ** ) &pUStru->pSendBuffer, ulLen, &pUStru->ulSndBufLen, szData );
#endif
      if( leto_SockSend( pUStru->hSocket, ( const char * ) pUStru->pSendBuffer, ulLen, pUStru->zstream, 0 ) != ulLen )
         pUStru->ulBytesSend = 0;
   }
   else
   {
#if defined( MSG_MORE )
      char szMsgSize[ LETO_MSGSIZE_LEN ];

      HB_PUT_LE_UINT32( szMsgSize, ulLen );
      pUStru->ulBytesSend = leto_SockSend( pUStru->hSocket, szMsgSize, LETO_MSGSIZE_LEN, pUStru->zstream, MSG_MORE );
      if( pUStru->ulBytesSend == LETO_MSGSIZE_LEN )
         pUStru->ulBytesSend += leto_SockSend( pUStru->hSocket, szData, ulLen, pUStru->zstream, 0 );
      else
         pUStru->ulBytesSend = 0;
#else
      HB_PUT_LE_UINT32( pUStru->pSendBuffer, ulLen );
      memcpy( pUStru->pSendBuffer + LETO_MSGSIZE_LEN, szData, ulLen );
      ulLen += LETO_MSGSIZE_LEN;
      pUStru->ulBytesSend = leto_SockSend( pUStru->hSocket, ( const char * ) pUStru->pSendBuffer, ulLen, pUStru->zstream, 0 );
#endif
   }

   hb_vmLock();

   if( bUseBuffer && pUStru->ulSndBufLen > LETO_SENDRECV_BUFFSIZE )
   {
      if( iDebugMode() > 10 )
         leto_wUsLog( pUStru, -1, "DEBUG oversized send-buffer of size %lu free-ed", pUStru->ulSndBufLen );
      pUStru->ulSndBufLen = LETO_SENDRECV_BUFFSIZE;
      pUStru->pSendBuffer = hb_xrealloc( pUStru->pSendBuffer, LETO_SENDRECV_BUFFSIZE + 1 );
   }
}

void leto_SendError( PUSERSTRU pUStru, const char * szData, HB_ULONG ulLen )
{
   HB_ULONG ulLenAll = pUStru->szHbError ? strlen( pUStru->szHbError ) : 0;

   if( ulLenAll > ulLen )
   {
      memcpy( pUStru->szHbError, szData, ulLen );
      leto_SendAnswer( pUStru, pUStru->szHbError, ulLenAll );
   }
   else
      leto_SendAnswer( pUStru, szData, ulLen );
}

HB_FUNC( LETO_SENDMESSAGE )
{
   char     szBuffer[ 64 ];
   HB_BOOL  bRetVal = HB_FALSE;

   void *       pSockAddr;
   unsigned int uiLen;
   HB_SOCKET    hSocket;

   int iPort = hb_parni( 1 );
   const char * szMessage = hb_parc( 2 );
   const char * szData = HB_ISCHAR( 4 ) && hb_parclen( 4 ) ? hb_parc( 4 ) : NULL;
   char *       szAddr;

   if( HB_ISCHAR( 3 ) && hb_parclen( 3 ) > 6 )
      szAddr = hb_strdup( hb_parc( 3 ) );
   else
      szAddr = hb_strdup( "127.0.0.1" );

   /* security : only allow this two commands */
   if( hb_parclen( 2 ) > 1 || ! ( szMessage[ 0 ] == LETOCMD_stop || szMessage[ 0 ] == LETOCMD_udf_rel ) )
   {
      hb_retl( bRetVal );
      return;
   }

   hb_socketInit();

   if( ( hSocket = hb_socketOpen( HB_SOCKET_AF_INET, HB_SOCKET_PT_STREAM, 0 ) ) != HB_NO_SOCKET )
   {
      if( hb_socketInetAddr( &pSockAddr, &uiLen, szAddr, iPort ) )
      {
         //hb_socketSetKeepAlive( hSocketMain, HB_TRUE );
         if( hb_socketConnect( hSocket, pSockAddr, uiLen, -1 ) == 0 )
         {
            char *   pBuffer;
            HB_ULONG ulRecvLen;

            hb_socketSetNoDelay( hSocket, HB_TRUE );
            hb_vmUnlock();
            /* ignore server intro answer */
#ifdef USE_LZ4
            ulRecvLen = leto_SockRecv( hSocket, szBuffer, LETO_MSGSIZE_LEN );
#else
            ulRecvLen = leto_SockRecv( hSocket, szBuffer, LETO_MSGSIZE_LEN, NULL );
#endif
            hb_vmLock();

            if( ulRecvLen == LETO_MSGSIZE_LEN )
            {
               ulRecvLen = HB_GET_LE_UINT32( szBuffer );
               pBuffer = ( char * ) hb_xalloc( ulRecvLen + 1 );
               hb_vmUnlock();
#ifdef USE_LZ4
               if( leto_SockRecv( hSocket, pBuffer, ulRecvLen ) == ulRecvLen )
#else
               if( leto_SockRecv( hSocket, pBuffer, ulRecvLen, NULL ) == ulRecvLen )
#endif
               {
                  HB_ULONG ulSendLen;

                  hb_vmLock();
                  strcpy( szBuffer + LETO_MSGSIZE_LEN, szMessage );
                  strcpy( szBuffer + LETO_MSGSIZE_LEN + 1, ";" );
                  if( szData )
                  {
                     strcpy( szBuffer + LETO_MSGSIZE_LEN + 2, szData );
                     strcpy( szBuffer + LETO_MSGSIZE_LEN + 2 + strlen( szData ), ";" );
                  }
                  ulSendLen = strlen( szBuffer + LETO_MSGSIZE_LEN );
                  HB_PUT_LE_UINT32( szBuffer, ulSendLen );
                  if( leto_SockSend( hSocket, szBuffer, ulSendLen + LETO_MSGSIZE_LEN, NULL, 0 ) )
                     bRetVal = HB_TRUE;
               }
               else
                  hb_vmLock();

               hb_xfree( pBuffer );
            }
         }
         hb_xfree( pSockAddr );
      }

      if( hSocket != HB_NO_SOCKET )
      {
         hb_idleSleep( 0.1 );
         hb_socketShutdown( hSocket, HB_SOCKET_SHUT_RDWR );
         hb_idleSleep( 0.1 );
         hb_socketClose( hSocket );
      }
   }

   hb_xfree( szAddr );
   hb_retl( bRetVal );
}


/*
 * Note: without HVM
 * Thread3 will wait max 2 second to establish a second socket for error feedback
 * Should be ever run/ sleep -- waken up by pipe from master, then dispatch one single or a set of sockets
 * to their corresponding threads already serving the regularly socket for a connection
 */
static HB_THREAD_STARTFUNC( thread3 )
{
   HB_FHANDLE *    hThreadPipe = ( HB_FHANDLE * ) Cargo;
   HB_ULONG        ulRecvLen;
   char            pBuffer[ 100 ];
   HB_USHORT       ui, uiCount, uiChanged, uiReadSocks;
   int             iChange;
   HB_BOOL         fEndOfGame = HB_FALSE;
   const HB_USHORT uiMax = 1001;  // leto_MaxUsers();
   HB_SOCKET *     pReadSocks = ( HB_SOCKET * ) hb_xgrabz( sizeof( HB_SOCKET ) * uiMax );
#if 1 && defined(  HB_HAS_POLL )
   struct pollfd   pPoll[ 1001 ];
#else
   fd_set          readfds;
   struct timeval  MicroWait;
   unsigned int    nfds;
   #if defined( HB_OS_WIN )
   int             iPipeIn = 0;
   #endif
#endif

   HB_SYMBOL_UNUSED( Cargo );
#if defined( HB_OS_WIN )
   hb_vmThreadInit( NULL );
#endif

   if( iDebugMode() > 0 )
      leto_writelog( NULL, -1, "DEBUG thread3() with pipe handle: %d started .. ",
                     ( int ) hThreadPipe[ 0 ] );

   HB_GC_LOCKX();
   s_paSocks[ 0 ] = ( int ) hThreadPipe[ 0 ];
   s_iSocksMax = 1;
   HB_GC_UNLOCKX();

   while( ! fEndOfGame && ! leto_ExitGlobal( HB_FALSE ) )
   {
#if 1 && ! defined( HB_HAS_POLL )
      FD_ZERO( &readfds );
      nfds = 0;
#endif

#if defined( HB_OS_WIN )
      uiCount = 1;
      ui = 1;
#else
      uiCount = 0;
      ui = 0;
#endif
      HB_GC_LOCKX();
      for( ; ui < uiMax; ui++ )
      {
         if( s_paSocks[ ui ] )
         {
#if 1 && defined( HB_HAS_POLL )
            pPoll[ uiCount ].fd = s_paSocks[ ui ];
            pPoll[ uiCount ].events = POLLIN | POLLRDNORM;
#else
            FD_SET( s_paSocks[ ui ], &readfds );
            if( s_paSocks[ ui ] > nfds )
               nfds = s_paSocks[ ui ];
#endif
            if( ++uiCount >= s_iSocksMax )
               break;
         }
      }
      HB_GC_UNLOCKX();

#if 1 && defined( HB_HAS_POLL )
      iChange = poll( pPoll, uiCount, uiCount == 1 ? -1 : 2000 );
      if( iChange <= 0 )
      {
         if( LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
            continue;
         else
         {
            leto_writelog( NULL, 0, "ERROR thread3() poll error" );
            break;
         }
      }
#else
   #if defined( HB_OS_WIN )

      if( uiCount == 1 )  /* infinite for only the wake-up pipe */
      {
         iChange = iPipeIn = hb_fsPipeIsData( s_paSocks[ 0 ], 1, -1 );
      }
      else
      {
         int iRound = 0;
         iChange = 0;
         while( iChange == 0 && iRound++ < 100 )
         {
            MicroWait.tv_sec = 0;
            MicroWait.tv_usec = 20000;  /* 20 ms */
            iChange = select( nfds + 1, &readfds, NULL, NULL, &MicroWait );
            if( iChange >= 0 )
               iChange += ( iPipeIn = hb_fsPipeIsData( s_paSocks[ 0 ], 1, 0 ) );
         }
      }
   #else
      if( uiCount == 1 )  /* infinite for only the wake-up pipe */
         iChange = select( nfds + 1, &readfds, NULL, NULL, NULL );
      else
      {
         MicroWait.tv_sec = 0;
         MicroWait.tv_usec = 2000000;
         iChange = select( nfds + 1, &readfds, NULL, NULL, &MicroWait );
      }
   #endif
      if( iChange < 0 )
      {
         if( LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
            continue;
         else
         {
            leto_writelog( NULL, 0, "DEBUG thread3() select() pipe: %d error", hThreadPipe[ 0 ] );
            break;
         }
      }
#endif

      if( iChange < 0 )
         break;
      else if( iChange == 0 )
      {
         /* remove not got active */
         for( ui = 1; ui < uiMax; ui++ )
         {
            if( s_paSocks[ ui ] )
               s_paSocks[ ui ] = 0;
         }
         continue;
      }

#if 1 && defined( HB_HAS_POLL )
      uiCount = 0;
#endif
      uiChanged = 0;
      uiReadSocks = 0;
      HB_GC_LOCKX();
      for( ui = 0; ui < uiMax; ui++ )
      {
         if( s_paSocks[ ui ] )
         {
#if 1 && defined( HB_HAS_POLL )
            if( ! ( pPoll[ uiCount ].revents & ( POLLERR | POLLHUP | POLLNVAL ) ) &&
                ( pPoll[ uiCount++ ].revents & ( POLLIN | POLLRDNORM ) ) )
#elif defined( HB_OS_WIN )
            if( ui ? FD_ISSET( s_paSocks[ ui ], &readfds ) : iPipeIn )
#else
            if( FD_ISSET( s_paSocks[ ui ], &readfds ) )
#endif
            {
               if( ui == 0 )  /* the pipe for wake up this thread */
               {
                  char c[ 1 ];

                  hb_fsPipeRead( hThreadPipe[ 0 ], &c, 1, 0 );
#if defined( HB_OS_WIN )
                  iPipeIn = 0;
#endif
                  if( c[ 0 ] == '@' )
                  {
                     fEndOfGame = HB_TRUE;
                     break;
                  }
                  if( ++uiChanged >= iChange )
                     break;
               }
               else  /* socket copy, as we handle them outside the mutex */
               {
                  pReadSocks[ uiReadSocks++ ] = ( HB_SOCKET ) s_paSocks[ ui ];
                  s_paSocks[ ui ] = 0;
                  s_iSocksMax--;
                  if( ++uiChanged >= iChange )
                     break;
               }
            }
         }
      }
      HB_GC_UNLOCKX();

      /* ready sockets handling */
      if( uiReadSocks )
      {
         for( ui = 0; ui < uiMax; ui++ )
         {
            HB_SOCKET hSocket = pReadSocks[ ui ];
            HB_LONG   lTmp;

            lTmp = hb_socketRecv( hSocket, pBuffer, LETO_MSGSIZE_LEN, 0, 500 );
            if( lTmp == LETO_MSGSIZE_LEN )
            {
               ulRecvLen = HB_GET_LE_UINT32( pBuffer );
               if( ulRecvLen >= 2 && ulRecvLen < 100 )  // minimum request, maximum buffer
               {
                  lTmp = hb_socketRecv( hSocket, pBuffer, ulRecvLen, 0, 100 );
                  if( lTmp > 2 && *pBuffer == LETOCMD_intro )
                  {
                     char * ptr = pBuffer + 1;

                     pBuffer[ lTmp ] = '\0';
                     //if( ( ptr = strchr( pBuffer, ';' ) ) != NULL )
                     if( *ptr++ == ';' )
                     {
                        int          iServerPort = atoi( ptr );
                        HB_BOOL      fConfirmed = HB_FALSE, fFound = HB_FALSE;
                        const char * szAddrPort = NULL;

                        /* we seek and countercheck IPport, set hSocket for pUStru .. */
                        /* not found means client have disapeared right after LETOCMD_intro reqquest */
                        if( leto_SeekUS( iServerPort, hSocket ) )
                        {
                           fConfirmed = fFound = HB_TRUE;
                           if( ( ptr = strchr( ptr, ';' ) ) != NULL )
                              szAddrPort = ++ptr;
                        }

                        if( fConfirmed )
                        {
                           if( hb_socketSend( hSocket, fConfirmed ? szOk : szErr1, 4, 0, 100 ) < 4 )
                              fConfirmed = HB_FALSE;
                           else if( iDebugMode() >= 10 )
                              leto_writelog( NULL, -1, "DEBUG thread3() 2. socket for client at address: %s:%d",
                                             szAddrPort ? szAddrPort : "?:?", iServerPort );
                        }

                        if( ! fConfirmed )
                        {
                           /* others don't know about socket, as so not in pUStru registered */
                           /* or we had above answer problems -- both cases socket is closed */
                           hb_socketShutdown( hSocket, HB_SOCKET_SHUT_RDWR  );
                           hb_socketClose( hSocket );
                           if( iDebugMode() > 0 )
                              leto_writelog( NULL, -1, "DEBUG thread3() no second socket at address: %s:%d",
                                             szAddrPort ? szAddrPort : "?:?", iServerPort );
                           if( fFound )
                              leto_SeekUS( iServerPort, HB_NO_SOCKET );
                        }
                     }
#if 0  /* 4 bytes send, and about 10 bytes received in one Op, no time for statistics, i'm in hurry */
                     HB_GC_LOCKS();
                     s_ullOperations++;
                     s_ullBytesRead += ulRecvLen + LETO_MSGSIZE_LEN;
                     s_ullBytesSend += 4;
                     HB_GC_UNLOCKS();
#endif
                  }
               }
               else  /* close that parrot */
               {
                  hb_socketShutdown( hSocket, HB_SOCKET_SHUT_RDWR  );
                  hb_socketClose( hSocket );
                  leto_writelog( NULL, -1, "ERROR thread3() no second socket %d -- it's a parrot", ( int ) hSocket );
               }
            }
            else
            {
               hb_socketShutdown( hSocket, HB_SOCKET_SHUT_RDWR  );
               hb_socketClose( hSocket );
               leto_writelog( NULL, -1, "ERROR thread3() - socket: %d traffic failure (bytes: %d)",
                              ( int ) hSocket, ( int ) lTmp );
            }

            /* pReadSocks[ ui ] = ( HB_SOCKET ) 0; */
            if( ! --uiReadSocks )
               break;
         }
      }
   }

   if( hThreadPipe[ 0 ] != FS_ERROR )
   {
      hb_fsClose( hThreadPipe[ 0 ] );
      hThreadPipe[ 0 ] = FS_ERROR;
   }
   if( hThreadPipe[ 1 ] != FS_ERROR )
   {
      hb_fsClose( hThreadPipe[ 1 ] );
      hThreadPipe[ 1 ] = FS_ERROR;
   }

   hb_xfree( s_paSocks );
   s_paSocks = NULL;
   hb_xfree( pReadSocks );
   if( iDebugMode() > 0 )
      leto_writelog( NULL, 0, "DEBUG thread3() ending .." );

#if defined( HB_OS_WIN )
   hb_vmThreadQuit();
#endif
   HB_THREAD_END
}

static HB_BOOL leto_idleSleep( double dSeconds )
{
   HB_I64  llLastCheck = leto_MilliSec();
   HB_BOOL fNoQuit = HB_TRUE;

   if( dSeconds > 0 )
   {
      while( leto_MilliSec() - llLastCheck < ( dSeconds * 1000 ) )
      {
         hb_releaseCPU();
         if( leto_ExitGlobal( HB_FALSE ) || hb_vmRequestQuery() != 0 )
         {
            fNoQuit = HB_FALSE;
            break;
         }
      }
   }
   return fNoQuit;
}

HB_FUNC( LETO_IDLESLEEP )
{
   double dSeconds = hb_parnd( 1 );

   if( dSeconds > 0 )
      hb_retl( leto_idleSleep( dSeconds ) );
}

/* healthy: an extra server thread for periodically testing for dead connections */
static HB_THREAD_STARTFUNC( healthy )
{
   long lTimeNextCheck = s_iZombieCheck / 3;
   HB_SYMBOL_UNUSED( Cargo );
   /* we check 3 times per interval, to more early detect if a zombie outreached it's time */

   hb_vmThreadInit( NULL );

   if( iDebugMode() > 0 )
      leto_writelog( NULL, -1, "DEBUG zombie watch thread active with %d sec interval", s_iZombieCheck );

   while( ! leto_ExitGlobal( HB_FALSE ) && hb_vmRequestQuery() == 0 )
   {
      int iZombies;

      /* special idleSleep */
      if( ! leto_idleSleep( lTimeNextCheck ) )
         break;
      iZombies = leto_PingForZombies( s_iZombieCheck );

      if( iZombies > 0 && iDebugMode() > 0 )
         leto_writelog( NULL, -1, "DEBUG %d zombies send home", iZombies );
   }

   if( iDebugMode() > 0 )
      leto_writelog( NULL, 0, "DEBUG zombie watch thread ends " );

   hb_vmThreadQuit();
   HB_THREAD_END
}

/* ### connection specific thread, one for each connection, this is their main loop ### */
static HB_THREAD_STARTFUNC( thread2 )
{
   PUSERSTRU pUStru = ( PUSERSTRU ) Cargo;
   HB_ULONG  ulRecvLen = 0;
   HB_I64    llTimePoint;
   HB_U64    ullTimeElapse;
   int       iChange;
#ifdef USE_LZ4
   HB_BOOL   bCompressed = HB_FALSE;
#else
   HB_BOOL   bCheckForNext = HB_FALSE;
#endif

#if 1 && defined( HB_HAS_POLL )
   struct pollfd pPoll[ 1 ];

   pPoll[ 0 ].fd = pUStru->hSocket;
   pPoll[ 0 ].events = POLLIN | POLLRDNORM;
#else
   fd_set readfds;
   int    nfds = pUStru->hSocket;

   #if ! defined( HB_OS_WIN )
      if( ! hb_fsPipeCreate( pUStru->hSockPipe ) )
      {
         /* without a thread will not wake up in Linux with Select() */
         pUStru->hSockPipe[ 0 ] = FS_ERROR;
         pUStru->hSockPipe[ 1 ] = FS_ERROR;
         leto_writelog( NULL, 0, "ERROR thread2() cannot create the wakeup pipe !!" );
      }
   #else
      pUStru->hSockPipe[ 0 ] = FS_ERROR;
      pUStru->hSockPipe[ 1 ] = FS_ERROR;
   #endif
#endif

   hb_vmThreadInit( NULL );
   leto_initSet();

   if( iDebugMode() > 0 )
      leto_writelog( NULL, -1, "INFO: connect from %s:%d  thread-ID %lu  conn-ID %d",
                     pUStru->szAddr, pUStru->iPort, ( HB_ULONG ) pUStru->hThreadID, pUStru->iUserStru - 1 );

   /* prepare and send initial answer for fresh connect */
   {
      char     szBuffer[ 64 ];
      char     szTmp[ 16 ];
      HB_ULONG ulTmp, ulLen;

      ulTmp = sprintf( szBuffer, "%s %s", LETO_RELEASE_STRING, LETO_VERSION_STRING );
      szBuffer[ ulTmp++ ] = ';';
      szBuffer[ ulTmp++ ] = leto_CryptTraf();

      leto_random_block( pUStru->cDopcode, LETO_DOPCODE_LEN, 42 );
      leto_encrypt( pUStru->cDopcode, LETO_DOPCODE_LEN, szTmp, &ulLen, LETO_PASSWORD, HB_TRUE );
      leto_byte2hexchar( szTmp, ( int ) ulLen, szBuffer + ulTmp );
      ulLen *= 2;
      *( szBuffer + ulTmp + ulLen ) = '\0';
      leto_SendAnswer( pUStru, szBuffer, ulTmp + ulLen );

      HB_GC_LOCKS();
      s_ullBytesSend += pUStru->ulBytesSend;
      HB_GC_UNLOCKS();
   }

   if( iDebugMode() > 0 )
      leto_writelog( NULL, -1, "INFO: connected from %s:%d  thread-ID %lu  conn-ID %d",
                     pUStru->szAddr, pUStru->iPort, ( HB_ULONG ) pUStru->hThreadID, pUStru->iUserStru - 1 );

   while( ! leto_ExitGlobal( HB_FALSE ) && pUStru->hSocket != HB_NO_SOCKET )
   {
#ifndef USE_LZ4
      /* bCheckForNext used only for zLib! communication, as a maybe next request already read into buffer */
      if( ! bCheckForNext )
      {
#endif
#if 1 && defined( HB_HAS_POLL )

         hb_vmUnlock();

         iChange = poll( pPoll, 1, -1 );
         if( iChange <= 0 )
         {
            if( LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
            {
               hb_vmLock();
               continue;
            }
            else
            {
               hb_vmLock();
               leto_writelog( NULL, 0, "ERROR thread2() 1. poor poll error " );
               break;
            }
         }
         else if( pPoll[ 0 ].revents & ( POLLERR | POLLHUP | POLLNVAL ) )
         {
            hb_vmLock();
            if( iDebugMode() > 0 && ! leto_ExitGlobal( HB_FALSE ) )
               leto_writelog( NULL, 0, "DEBUG thread2() socket error - maybe closed .." );
            pUStru->hSocket = HB_NO_SOCKET;
            break;
         }
#else
         FD_ZERO( &readfds );
         FD_SET( pUStru->hSocket, &readfds );
   #if ! defined( HB_OS_WIN )
         if( pUStru->hSockPipe[ 0 ] != FS_ERROR )
         {
            if( ( int ) pUStru->hSockPipe[ 0 ] > ( int ) pUStru->hSocket )
               nfds = ( int ) pUStru->hSockPipe[ 0 ];
            FD_SET( ( int ) ( pUStru->hSockPipe[ 0 ] ), &readfds );
         }
   #endif

         hb_vmUnlock();
         iChange = select( nfds + 1, &readfds, NULL, NULL /* &exceptfds*/, NULL /*&MicroWait*/ );
         if( iChange <= 0 )
         {
            if( LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
            {
               hb_vmLock();
               continue;
            }
            else
            {
               hb_vmLock();
               leto_writelog( NULL, -1, "DEBUG thread2() in select(%d) for socket: %d [pipe: %d] with error: %d",
                              nfds + 1, ( int ) pUStru->hSocket,
                              pUStru->hSockPipe[ 0 ] != FS_ERROR ? pUStru->hSockPipe[ 0 ] : -1,
                              LETO_SOCK_GETERROR() );
               break;
            }
         }
         else
         {

            if( ! FD_ISSET( ( int ) pUStru->hSocket, &readfds ) )  // no socket read event
            {
               hb_vmLock();
   #if ! defined( HB_OS_WIN )
               if( pUStru->hSockPipe[ 0 ] != FS_ERROR &&
                   FD_ISSET( ( int ) pUStru->hSockPipe[ 0 ], &readfds ) )
               {
                  char c[ 1 ];

                  hb_fsPipeRead( pUStru->hSockPipe[ 0 ], &c, 1, 0 );
                  break;
               }
   #endif
               continue;
            }
         }
#endif  /* poll() / select() */
#ifndef USE_LZ4
      }
      else
      {
         hb_vmUnlock();
         iChange = 1;  /* bCheckForNext exception */
      }
#endif

      if( leto_ExitGlobal( HB_FALSE ) || hb_vmRequestQuery() != 0 )
      {
         hb_vmLock();
         if( iDebugMode() >= 10 )
            leto_wUsLog( pUStru, 0, "Thread2 exit by management shutdown" );
         break;
      }
      else if( iChange > 0 )
      {
#ifdef USE_LZ4
         ulRecvLen = leto_SockRecv( pUStru->hSocket, ( char * ) pUStru->pBuffer, LETO_MSGSIZE_LEN );
#else
         ulRecvLen = leto_SockRecv( pUStru->hSocket, ( char * ) pUStru->pBuffer, LETO_MSGSIZE_LEN, pUStru );
         if( bCheckForNext && ! ulRecvLen )
         {
            bCheckForNext = HB_FALSE;
            hb_vmLock();
            continue;
         }
#endif
      }
      else
      {
         hb_vmLock();
         continue;
      }

      if( ulRecvLen != LETO_MSGSIZE_LEN )
      {
         hb_vmLock();

         if( pUStru->hSocket == HB_NO_SOCKET )   // connection closed
         {
            if( iDebugMode() > 0 )
               leto_writelog( NULL, -1, "DEBUG thread2() %s:%d terminated by mgmt(0)",
                              pUStru->szAddr, pUStru->iPort );
         }
         else
         {
            if( ulRecvLen )
            {
               pUStru->pBuffer[ ulRecvLen ] = '\0';
               leto_writelog( NULL, 0, "ERROR thread2() leto_SockRec <> LETO_MSGSIZE_LEN" );
               leto_writelog( NULL, 0, ( const char * ) pUStru->pBuffer );
            }
            else // if( iDebugMode() > 0 )
            {
               int iSocks = hb_socketGetError();

               if( iSocks )
                  leto_writelog( NULL, -1, "ERROR thread2() Socket error: %s",
                                 hb_socketErrorStr( iSocks ) );
            }
         }
         break;
      }

      ulRecvLen = HB_GET_LE_UINT32( pUStru->pBuffer );
#ifdef USE_LZ4
      if( pUStru->zstream )
      {
         bCompressed = ( ulRecvLen & 0x80000000 );
         if( bCompressed )
            ulRecvLen &= 0x7FFFFFFF;
      }
#endif

      if( ! ulRecvLen )
      {
         hb_vmLock();
         if( pUStru->hSocket == HB_NO_SOCKET && iDebugMode() > 0 )
            leto_writelog( NULL, 0, "DEBUG thread2() terminated by mgmt(1)" );
         else
            leto_writelog( NULL, 0, "ERROR thread2() ulRecvLen == 0" );
         break;
      }
      else if( pUStru->ulBufferLen <= ulRecvLen )
      {
         if( ulRecvLen > LETO_MAX_RECV_BLOCK )
         {
            hb_vmLock();
            leto_writelog( NULL, -1, "ERROR thread2() %s:%d too big packet size %lu",
                           pUStru->szAddr, pUStru->iPort, ulRecvLen );
            break;
         }
         else
            leto_ReallocUSbuff( pUStru, ulRecvLen );
      }

#ifdef USE_LZ4
      if( leto_SockRecv( pUStru->hSocket, ( char * ) pUStru->pBuffer, ulRecvLen ) != ulRecvLen )
#else
      if( leto_SockRecv( pUStru->hSocket, ( char * ) pUStru->pBuffer, ulRecvLen, pUStru ) != ulRecvLen )
#endif
      {
         hb_vmLock();
         leto_writelog( NULL, 0, "ERROR thread2() leto_SockRec != ulRecvLen" );
         break;
      }
      else  /* !! here the request is successfull complete received !! */
         hb_vmLock();

#ifdef USE_LZ4
      if( bCompressed || pUStru->bZipCrypt )  /* means compressed and/or encrypted */
         ulRecvLen = hb_lz4netDecrypt( pUStru->zstream, ( char ** ) &pUStru->pBuffer, ulRecvLen, &pUStru->ulBufferLen, bCompressed );
#endif

      if( ulRecvLen < 2 )  /* must be at least command char + ';' */
      {
         leto_SendAnswer( pUStru, szErr1, 4 );
         leto_writelog( NULL, -1, "ERROR thread2() command format: %lu too short", ulRecvLen );
         if( ulRecvLen )
            leto_writelog( NULL, ulRecvLen, ( char * ) pUStru->pBuffer );
         continue;
      }

      pUStru->ulDataLen = ulRecvLen;
      llTimePoint = leto_MilliSec();

      if( iDebugMode() >= 15 )
      {
         leto_wUsLog( pUStru, -1, "<< %s: (len %lu)",
                                  leto_CmdToHuman( *pUStru->pBuffer ),
                                  pUStru->ulDataLen );
         if( iDebugMode() >= 20 )  /* log complete communication data */
            leto_wUsLog( pUStru, pUStru->ulDataLen, ( char * ) pUStru->pBuffer );
      }

      if( ! leto_ParseCommand( pUStru ) )
      {
         leto_writelog( NULL, 0, "ERROR leto_ParseCommand()" );
         leto_writelog( NULL, pUStru->ulDataLen, ( char * ) pUStru->pBuffer );
         if( ! pUStru->ulBytesSend && ! pUStru->bNoAnswer )  /* else client won't expect something after given answer */
            leto_SendAnswer( pUStru, szErr1, 4 );
         /* ToDo then a leto_SendAnswer2() ? -- client shell throw RTE */
      }
      if( pUStru->bNoAnswer )
      {
#ifndef USE_LZ4
         /* in ZLIB zstream mode, maybe all data from socket is read, but next request already in buffer
          * then select()/ poll() will sleep and we have to check before ... */
         if( pUStru->iZipRecord > 0 )
            bCheckForNext = HB_TRUE;
#endif
         pUStru->bNoAnswer = HB_FALSE;
      }
      if( pUStru->bCloseConnection )  /* commonly together with bNoAnswer, exception wrong Pass leto_Intro */
         break;

      /* Note: LetoDB monitor <read> these two values, maybe in a race condition with wrong content,
       *       but leto_Mgmt() ever ensures a zero terminated copy of this buffer;
       *       further these values are just used for 'fancy info' */
      pUStru->llLastAct = llTimePoint / 1000;
      memcpy( pUStru->szLastRequest, pUStru->pBuffer, ulRecvLen > 63 ? 63 : ulRecvLen );
      pUStru->szLastRequest[ ulRecvLen > 63 ? 63 : ulRecvLen ] = '\0';

      ullTimeElapse = LETO_CPU_STATISTIC ? ( HB_U64 ) ( leto_MilliSec() - llTimePoint ) : 0;
      pUStru->ullCPULoad += ullTimeElapse;
      pUStru->iHbError = 0;  /* leave the pUStru->szHbError description for e.g. console monitor */

      HB_GC_LOCKS();
      s_ullOperations++;
      if( pUStru->bBeQuiet )  /* UDF executed, HVM called */
      {
         pUStru->bBeQuiet = HB_FALSE;
         if( ( ++s_ullUDFOps & 0xFF ) == 0 )  /* ToDo verify 256 */
            pUStru->bGCCollect = HB_TRUE;
      }
      s_ullBytesRead += ulRecvLen + LETO_MSGSIZE_LEN;
      s_ullBytesSend += pUStru->ulBytesSend;
      s_ullCPULoad += ullTimeElapse;  /* result in seconds, but we can't / 1000 here => + 0 */
      HB_GC_UNLOCKS();

      if( pUStru->bGCCollect )
      {
         hb_gcCollectAll( HB_FALSE );
         pUStru->bGCCollect = HB_FALSE;
      }

      /* resize to default size if last request needed big buffer */
      if( pUStru->ulBufferLen > LETO_SENDRECV_BUFFSIZE )
         leto_ReallocUSbuff( pUStru, 0 );  /* '0' means default size */
      pUStru->ulBytesSend = 0;
   }

   leto_CloseUS( pUStru );
   hb_vmThreadQuit();
   HB_THREAD_END
}

/* ### MASTER MAIN THREAD handling incoming connections, create and dispatch them to threads ### */
HB_FUNC( LETO_SERVER )
{
   HB_BOOL          bSocketErr = HB_FALSE;
   HB_SOCKET        incoming = HB_NO_SOCKET;
   void *           pSockAddr = NULL;
   char *           szAddr = NULL;
   unsigned int     uiLen = 0;
   HB_THREAD_ID     th_id;
   HB_THREAD_HANDLE th_h;
   HB_BOOL          bExtraWait;
   int              iChange;
   int              iServerPort = hb_parni( 1 );
   const char *     szServerAddr = ( HB_ISCHAR( 2 ) && hb_parclen( 2 ) > 6 ) ? hb_parc( 2 ) : NULL;
   HB_SOCKET        hSocketMain;               /* main server socket */
   HB_SOCKET        hSocketErr = HB_NO_SOCKET; /* server second socket */
   HB_FHANDLE       hThreadPipe[ 2 ] = { FS_ERROR, FS_ERROR };

#if 1 && defined( HB_HAS_POLL )
   struct pollfd pPoll[ 2 ];
#else
   struct timeval MicroWait;
   fd_set readfds;
#endif

   if( iServerPort <= 0 )
      iServerPort = 2812;
   if( HB_ISNUM( 3 ) )
   {
      s_iTimeOut = hb_parni( 3 );
      if( s_iTimeOut > 0 )
         s_iTimeOut *= 1000;
      if( s_iTimeOut > 10000 )
         leto_setTimeout( ( HB_ULONG ) s_iTimeOut );
   }
   if( HB_ISNUM( 4 ) )
      s_iZombieCheck = hb_parni( 4 );

   hb_socketInit();
   if( ( hSocketMain = hb_socketOpen( HB_SOCKET_AF_INET, HB_SOCKET_PT_STREAM, 0 ) ) != HB_NO_SOCKET )
   {
      if( hb_socketInetAddr( &pSockAddr, &uiLen, szServerAddr, iServerPort ) )
      {
         hb_socketSetKeepAlive( hSocketMain, HB_TRUE );
         hb_socketSetNoDelay( hSocketMain, HB_TRUE );
         if( hb_socketBind( hSocketMain, pSockAddr, uiLen ) != 0 ||
            hb_socketListen( hSocketMain, 10 ) != 0 )
         {
            leto_writelog( NULL, -1, "ERROR hb_socketBind() port %d used -- often a short temporary problem", iServerPort );
            hb_socketClose( hSocketMain );
            hSocketMain = HB_NO_SOCKET;
         }
      }

      hb_xfree( pSockAddr );
      pSockAddr = NULL;

      /* now the socket for async error */
      if( hSocketMain != HB_NO_SOCKET &&
          ( hSocketErr = hb_socketOpen( HB_SOCKET_AF_INET, HB_SOCKET_PT_STREAM, 0 ) ) != HB_NO_SOCKET )
      {
         HB_BOOL bFail = HB_FALSE;

         if( hb_socketInetAddr( &pSockAddr, &uiLen, szServerAddr, iServerPort + 1 ) )
         {
            hb_socketSetKeepAlive( hSocketMain, HB_TRUE );
            hb_socketSetNoDelay( hSocketMain, HB_TRUE );
            if( hb_socketBind( hSocketErr, pSockAddr, uiLen ) != 0 ||
               hb_socketListen( hSocketErr, 10 ) != 0 )
            {
               bFail = HB_TRUE;
            }
         }
         else
            bFail = HB_TRUE;
         if( bFail )
         {
            hb_socketClose( hSocketMain );
            hb_socketClose( hSocketErr );
            hSocketMain = HB_NO_SOCKET;
            hSocketErr = HB_NO_SOCKET;
            leto_writelog( NULL, -1, "ERROR to establish second socket port %d: %s",
                           iServerPort + 1, hb_socketErrorStr( hb_socketGetError() ) );
         }
         else if( iDebugMode() > 0 )
            leto_writelog( NULL, -1, "DEBUG second socket: %d for errors established", hSocketErr );

         if( pSockAddr )
         {
            hb_xfree( pSockAddr );
            pSockAddr = NULL;
         }
      }
      else if( hSocketMain != HB_NO_SOCKET )
      {
         hb_socketClose( hSocketMain );
         hSocketMain = HB_NO_SOCKET;
         leto_writelog( NULL, -1, "ERROR Server can't establish second socket port %d ...", iServerPort + 1 );
      }
   }
   else
      leto_writelog( NULL, -1, "ERROR hb_socketOpen() port %d -- often a temporary problem ", iServerPort );

   if( hSocketMain == HB_NO_SOCKET )
   {
      hb_retl( HB_FALSE );
      return;
   }

   /* the zombie watch thread -- socket not created if not wanted */
   if( s_iZombieCheck && hSocketErr != HB_NO_SOCKET )
   {
      th_h = hb_threadCreate( &th_id, healthy, NULL );
      if( th_h )
         hb_threadDetach( th_h );
   }

   s_hSocketMain = hSocketMain;

   /* the second socket advisor thread3 */
   if( hb_fsPipeCreate( hThreadPipe ) )
   {
      /* ugly: thread3() must hb_xfree this, but here grabbed to ensure it's ready */
      s_paSocks = ( unsigned int * ) hb_xgrabz( sizeof( unsigned int ) * 1001 );  // leto_MaxUsers();
      th_h = hb_threadCreate( &th_id, thread3, ( void * ) &hThreadPipe );
      if( th_h )
         hb_threadDetach( th_h );
   }

   /* ToDo -- integrate uhura ? */

#if 1 && defined( HB_HAS_POLL )  /* using poll() instead FD_SET() for Linux */
   pPoll[ 0 ].fd = hSocketMain;
   pPoll[ 0 ].events = POLLIN | POLLRDNORM;
   pPoll[ 1 ].fd = hSocketErr;
   pPoll[ 1 ].events = POLLIN | POLLRDNORM;
   pPoll[ 1 ].revents = 0;
#endif

   leto_CommandSetInit();
   leto_CommandDescInit();

   while( ! leto_ExitGlobal( HB_FALSE ) )
   {
      if( szAddr )
      {
         hb_xfree( szAddr );
         szAddr = NULL;
      }
      if( pSockAddr )
      {
         hb_xfree( pSockAddr );
         pSockAddr = NULL;
      }

#if 1 && defined( HB_HAS_POLL )

      hb_vmUnlock();
      iChange = poll( pPoll, hSocketErr != HB_NO_SOCKET ? 2 : 1, -1 );
      hb_vmLock();

      if( iChange <= 0 )
      {
         if( LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
            continue;
         else
         {
            leto_writelog( NULL, 0, "ERROR main thread socket problems" );
            break;
         }
      }
      else if( pPoll[ 0 ].revents & ( POLLERR | POLLHUP | POLLNVAL ) ||
               pPoll[ 1 ].revents & ( POLLERR | POLLHUP | POLLNVAL ) )
      {
         leto_writelog( NULL, 0, "ERROR main thread socket shutdown or problems" );
         break;
      }
      else if( iChange > 0 )
      {
         if( pPoll[ 0 ].revents & ( POLLIN | POLLRDNORM ) )
         {
            if( iDebugMode() > 21 )
               leto_writelog( NULL, 0, "DEBUG Server saw activity at 1. socket ..." );
            incoming = hb_socketAccept( hSocketMain, &pSockAddr, &uiLen, 3000 );
            bSocketErr = HB_FALSE;
         }
         else if( hSocketErr != HB_NO_SOCKET && pPoll[ 1 ].revents & ( POLLIN | POLLRDNORM ) )
         {
            if( iDebugMode() > 21 )
               leto_writelog( NULL, 0, "DEBUG Server saw activity at 2. socket ..." );
            incoming = hb_socketAccept( hSocketErr, &pSockAddr, &uiLen, 3000 );
            bSocketErr = HB_TRUE;
         }
      }
#else
      MicroWait.tv_sec = 0;
      MicroWait.tv_usec = 1500000;
      FD_ZERO( &readfds );
      FD_SET( hSocketMain, &readfds );
      if( hSocketErr != HB_NO_SOCKET )
         FD_SET( hSocketErr, &readfds );

      hb_vmUnlock();
      if( hSocketErr != HB_NO_SOCKET )
         iChange = select( ( int ) hSocketErr + 1, &readfds, NULL, NULL, &MicroWait );
      else
         iChange = select( ( int ) hSocketMain + 1, &readfds, NULL, NULL, &MicroWait );
      hb_vmLock();
      if( iChange > 0 )
      {
         if( FD_ISSET( hSocketMain, &readfds ) )
         {
            if( iDebugMode() > 21 )
               leto_writelog( NULL, 0, "DEBUG Server saw activity at 1. socket ..." );
            bSocketErr = HB_FALSE;
            incoming = hb_socketAccept( hSocketMain, &pSockAddr, &uiLen, 3000 );
         }
         else if( hSocketErr != HB_NO_SOCKET && FD_ISSET( hSocketErr, &readfds )  )
         {
            if( iDebugMode() > 21 )
               leto_writelog( NULL, 0, "DEBUG Server saw activity at 2. socket ..." );
            bSocketErr = HB_TRUE;
            incoming = hb_socketAccept( hSocketErr, &pSockAddr, &uiLen, 3000 );
         }
      }
      else if( iChange == 0 )
         continue;
      else
         break;
#endif

      szAddr = hb_socketAddrGetName( pSockAddr, uiLen );
      bExtraWait = ( ! strncmp( ( const char * ) szAddr, "127.0.0.1", 9 ) );

      /* local connection can log-in into even into locked server */
      if( leto_ConnectIsLock() && incoming != HB_NO_SOCKET && ! bExtraWait )
      {
         char szMsg[ LETO_MSGSIZE_LEN + 15 + 1 ];

         HB_PUT_LE_UINT32( szMsg, 15 );
         hb_vmUnlock();
         sprintf( szMsg + LETO_MSGSIZE_LEN, "%s", "-LCK:SRV_LOCKED" );
         leto_SockSend( incoming, szMsg, LETO_MSGSIZE_LEN + 15, NULL, 0 );
         hb_vmLock();

         hb_socketClose( incoming );
      }
      else if( incoming != HB_NO_SOCKET )
      {
         PUSERSTRU pUStru;

         hb_socketSetKeepAlive( incoming, HB_TRUE );
         hb_socketSetNoDelay( incoming, HB_TRUE );
         // hb_socketSetBlockingIO( incoming, HB_TRUE );
         /* set 64KB send and receive buffer */
         hb_socketSetSndBufSize( incoming, 0xFFFF );
         hb_socketSetRcvBufSize( incoming, 0xFFFF );

         /* pass the second socket to thread3() to let handle him the init sequence with client */
         if( bSocketErr )
         {
            HB_USHORT  uiMax = 1001;  // leto_MaxUsers();
            const char cToPipe[ 1 ] = { '!' };
            HB_USHORT  ui;

            if( hThreadPipe[ 1 ] == FS_ERROR )  /* thread3 have quit ? */
               continue;

            HB_GC_LOCKX();
            for( ui = 0; ui < uiMax; ui++ )
            {
               if( ! s_paSocks[ ui ] )
               {
                  s_paSocks[ ui ] = ( int ) incoming;
                  s_iSocksMax++;
                  break;
               }
            }
            HB_GC_UNLOCKX();

            /* wake the sleeping thread3 */
            hb_fsPipeWrite( hThreadPipe[ 1 ], cToPipe, 1, 0 );
            continue;
         }

         pUStru = leto_InitUS( incoming );
         if( ! pUStru )
         {
            leto_writelog( NULL, 0, "ERROR configured mamimum number of users reached ..."  );
            continue;
         }

         pUStru->ulBufferLen = LETO_SENDRECV_BUFFSIZE;
         pUStru->pBuffer = ( HB_BYTE * ) hb_xgrab( LETO_SENDRECV_BUFFSIZE + 1 );
         pUStru->ulSndBufLen = LETO_SENDRECV_BUFFSIZE;
         pUStru->pSendBuffer = ( HB_BYTE * ) hb_xgrab( LETO_SENDRECV_BUFFSIZE + 1 );
         pUStru->szAddr = ( HB_BYTE * ) hb_strdup( szAddr );
         pUStru->iPort = hb_socketAddrGetPort( pSockAddr, uiLen );

         pUStru->hThread = hb_threadCreate( &pUStru->hThreadID, thread2, ( void * ) pUStru );
         if( ! pUStru->hThread )
         {
            leto_writelog( NULL, 0, "ERROR thread create error !" );
            leto_SendAnswer( pUStru, "-ERR:MAX_THREADS", 16 );
            leto_CloseUS( pUStru );
         }
         else
            hb_threadDetach( pUStru->hThread );
      }
      else if( hb_socketGetError() & HB_SOCKET_ERR_TIMEOUT )
      {
         leto_writelog( NULL, -1, "ERROR socket err: %d = %s",
                      hb_socketGetError(), hb_socketErrorStr( hb_socketGetError() ) );
         break;
      }

      /* server is now so fast, we have to wait for shutdown command from local adress */
      if( bExtraWait )
         hb_idleSleep( 0.25 );
   }

   if( szAddr )
      hb_xfree( szAddr );
   if( pSockAddr )
      hb_xfree( pSockAddr );

   leto_ExitGlobal( HB_TRUE );  /* set if not ready done */

   if( hThreadPipe[ 1 ] != FS_ERROR )  /* wake up to stop thread3() */
   {
      const char cToPipe[ 1 ] = { '@' };

      hb_fsPipeWrite( hThreadPipe[ 1 ], cToPipe, 1, 0 );
   }

   if( hSocketErr != HB_NO_SOCKET )
   {
      hb_socketShutdown( hSocketErr, HB_SOCKET_SHUT_RDWR );
      hb_socketClose( hSocketErr );
   }
   if( hSocketMain != HB_NO_SOCKET )
   {
      hb_socketShutdown( hSocketMain, HB_SOCKET_SHUT_RDWR );
      hb_socketClose( hSocketMain );
   }

   leto_CloseAllSocket();
   hb_idleSleep( 0.5 );  /* give possible many threads a second to finalize their quit */

   iChange = 0;
   /* Note: a thread may be waiting for WA in leto_requestArea() */
   while( leto_ActiveUser() && iChange < 2 )
   {
      if( iChange++ == 0 )
         leto_writelog( NULL, -1, "DEBUG waiting about %d s for other threads ...", 2 );
      hb_idleSleep( 1 );
   }

   /* if a thread hungs ( still have not seen ), this will force all close of all sockets closed */
   if( leto_ActiveUser() )
   {
      PHB_ITEM pDetached;
      int      iLen;

      leto_ForceCloseAllSocket();
      hb_vmTerminateThreads();
      hb_idleSleep( 1 );

      /* detached areas would prevent executable from final QUIT */
      pDetached = hb_rddDetachedList();
      iLen = ( int ) hb_arrayLen( pDetached );
      if( iLen > 0 )
      {
#if 1  /* one by one request and release */
         const char * szAlias;
         int          i;
         AREAP        pArea;
         HB_BOOL      bFine = HB_TRUE;

         for( i = iLen; i > 0; --i )
         {
            szAlias = hb_arrayGetCPtr( pDetached, i );
            pArea = hb_rddRequestArea( szAlias, NULL, HB_TRUE, 0 );
            if( pArea )
            {
               hb_rddSelectWorkAreaNumber( pArea->uiArea );
               hb_rddReleaseCurrentArea();
            }
            else
               bFine = HB_FALSE;
         }
         if( bFine )
            leto_writelog( NULL, 0, "DEBUG finally closed detached workareas " );
         else
            leto_writelog( NULL, 0, "ERROR check you taskmanager for final killing LetoDB" );
#else
      /* hb_rddCloseDetachedAreas(); is in below */
      hb_rddShutDown();
#endif
      }
      if( pDetached )
         hb_itemRelease( pDetached );
   }

   leto_CommandDescFree();

   if( hThreadPipe[ 1 ] != FS_ERROR )
   {
      hb_fsClose( hThreadPipe[ 1 ] );
      hThreadPipe[ 1 ] = FS_ERROR;
   }
   if( hThreadPipe[ 0 ] != FS_ERROR )
   {
      hb_fsClose( hThreadPipe[ 0 ] );
      hThreadPipe[ 0 ] = FS_ERROR;
   }

   hb_retl( HB_TRUE );
}

