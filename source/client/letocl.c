/*
 * Harbour Leto client
 *
 * Copyright 2013 Alexander S. Kresin <alex / at / kresin.ru>
 * www - http://www.kresin.ru
 *
 *           2015-16 Rolf 'elch' Beckmann
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

#define LETO_MT 1   /* outcomment this to disable TLS for LetoDBf client, for non MT */

#if defined( HB_OS_WIN_32 ) || defined( HB_OS_WIN )
   #include <winsock2.h>
   #include <windows.h>
#endif

#include "rddleto.h"

#if defined( HB_OS_UNIX )
   #if defined( HB_OS_LINUX ) && ! defined( __USE_GNU )
      #define __USE_GNU
   #endif
   #include <unistd.h>
   #include <sys/socket.h>   /* only with above __USE_GNU: MSG_MORE flag */
   #if defined( _POSIX_C_SOURCE ) && _POSIX_C_SOURCE >= 200112L
      #define HB_HAS_POLL
      #include <poll.h>
      #ifndef POLLRDNORM
         #define POLLRDNORM  0x0040
      #endif
   #endif
   #include <errno.h>
   #include <netinet/in.h>
   #include <arpa/inet.h>
   #include <pwd.h>
   #include <sys/types.h>
#endif

#if defined ( _MSC_VER )
  #define _WINSOCKAPI_
#endif

#define LETO_DEFAULT_TIMEOUT 120000;  /* two minutes */

#ifdef LETO_MT
   typedef struct
   {
      unsigned int     uiConnCount;   /* count of items in letoConnPool */
      LETOCONNECTION * letoConnPool;  /* array of connections */
      LETOCONNECTION * pCurrentConn;  /* pointer into letoConnPool list */
   } LETOPOOL;
#else
   static HB_USHORT        s_uiConnCount = 0;
   static LETOCONNECTION * s_letoConnPool = NULL;
   static LETOCONNECTION * s_pCurrentConn = NULL;
#endif

static PHB_ITEM s_pError = NULL;   /* elch's prepared GLOBAL error object from second thread */
static int      s_iSessions = 0;   /* a counter to control hb_socketInit() */

#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   static HB_SPINLOCK_T s_ErrorMtx = HB_SPINLOCK_INIT;
   #define HB_GC_LOCKE()    HB_SPINLOCK_ACQUIRE( &s_ErrorMtx )
   #define HB_GC_UNLOCKE()  HB_SPINLOCK_RELEASE( &s_ErrorMtx )
#else
   static HB_CRITICAL_NEW( s_ErrorMtx );
   #define HB_GC_LOCKE()    hb_threadEnterCriticalSection( &s_ErrorMtx )
   #define HB_GC_UNLOCKE()  hb_threadLeaveCriticalSection( &s_ErrorMtx )
#endif

/* for backward compatibility to older Harbour */
#if defined( __HARBOUR30__ )

static char * s_szModName = NULL;

HB_EXPORT char * LetoSetModName( char * szModule )
{
   unsigned int uiLen;

   if( szModule )
   {
      if( s_szModName )
         hb_xfree( s_szModName );

      uiLen = strlen( szModule );
      s_szModName = ( char * ) hb_xgrab( uiLen + 1 );
      memcpy( s_szModName, szModule, uiLen );
      s_szModName[ uiLen ] = '\0';
   }
   return ( char * ) s_szModName;
}

extern HB_SIZE hb_fsPipeWrite( HB_FHANDLE hPipeHandle, const void * buffer, HB_SIZE nSize, HB_MAXINT nTimeOut );

#endif  /* __HARBOUR30__ */


#ifdef LETO_MT

/* functions using Harbour Thread Local Storage technics */
static void leto_TSDInit( void * cargo )
{
   LETOPOOL * pLetoPool = ( LETOPOOL * ) cargo;

   pLetoPool->uiConnCount  = 0;
   pLetoPool->letoConnPool = NULL;
   pLetoPool->pCurrentConn = NULL;
}

static void leto_TSDExit( void * cargo )
{
   LETOPOOL * pLetoPool = ( LETOPOOL * ) cargo;

   if( pLetoPool->letoConnPool )
   {
      LETOCONNECTION * pConnection;
      unsigned int i;

      for( i = 0; i < pLetoPool->uiConnCount; i++ )
      {
         pConnection = pLetoPool->letoConnPool + i;
         if( pConnection->pAddr )
            LetoConnectionClose( pConnection );
      }
      hb_xfree( pLetoPool->letoConnPool );
      pLetoPool->letoConnPool = NULL;
   }
   pLetoPool->uiConnCount = 0;
}

static HB_TSD_NEW( s_TSData, sizeof( LETOPOOL ), leto_TSDInit, leto_TSDExit );

/* helper functions for easy global access elements of TSD struct */
LETOCONNECTION * letoGetConnPool( HB_UINT uiConnection )
{
   LETOPOOL * pLetoPool = ( LETOPOOL * ) hb_stackGetTSD( &s_TSData );

   if( uiConnection < pLetoPool->uiConnCount )
      return pLetoPool->letoConnPool + uiConnection;
   else
      return pLetoPool->letoConnPool;  /* UGLY try to pacify MinGW GNU C 6.2 warning: return NULL; */
}

/* _HB_INLINE_ */
LETOCONNECTION * letoGetCurrConn( void )
{
   LETOPOOL * pLetoPool = ( LETOPOOL * ) hb_stackGetTSD( &s_TSData );

   return pLetoPool->pCurrentConn;
}

static void letoClearCurrConn( LETOCONNECTION * pConnection )
{
   LETOPOOL *   pLetoPool = ( LETOPOOL * ) hb_stackGetTSD( &s_TSData );
   unsigned int ui;

   if( pLetoPool->pCurrentConn && pLetoPool->pCurrentConn == pConnection )
      pLetoPool->pCurrentConn = NULL;

   for( ui = 0; ui < pLetoPool->uiConnCount; ui++ )
   {
      if( pLetoPool->letoConnPool[ ui ].pAddr )
      {
         pLetoPool->pCurrentConn = pLetoPool->letoConnPool + ui;
         break;
      }
   }
}

unsigned int uiGetConnCount( void )
{
   LETOPOOL * pLetoPool = ( LETOPOOL * ) hb_stackGetTSD( &s_TSData );

   return pLetoPool->uiConnCount;
}

#else  /* helper funtions without Harbour TSD technic - for non MT usage */

LETOCONNECTION * letoGetConnPool( HB_UINT uiConnection )
{
   if( uiConnection < s_uiConnCount )
      return s_letoConnPool + uiConnection;
   else
      return s_letoConnPool;  /* UGLY try to pacify MinGW GNU C 6.2 warning: return NULL; */
}

LETOCONNECTION * letoGetCurrConn( void )
{
   return s_pCurrentConn;
}

static void letoClearCurrConn( LETOCONNECTION * pConnection )
{
   if( s_pCurrentConn && s_pCurrentConn == pConnection )
      s_pCurrentConn = NULL;
}

HB_USHORT uiGetConnCount( void )
{
   return s_uiConnCount;
}

#endif  /* LETO_MT */


static char * leto_AddLen( char * pData, HB_ULONG * ulLen, HB_BOOL fBefore )
{
   if( *ulLen < 256 )
   {
      if( fBefore )
      {
         *( --pData ) = ( char ) *ulLen & 0xFF;
         *( --pData ) = '\1';
      }
      else
      {
         *( pData++ ) = '\1';
         *( pData++ ) = ( char ) *ulLen & 0xFF;
      }
      *ulLen += 2;
   }
   else
   {
      HB_UCHAR uiLenLen, ui;
      char     sNum[ 4 ];

      uiLenLen = leto_n2b( sNum, *ulLen );
      if( fBefore )
      {
         ui = uiLenLen;
         while( ui-- )
         {
            *( --pData ) = sNum[ ui ];
         }
         *( --pData ) = ( char ) uiLenLen;
      }
      else
      {
         *( pData++ ) = ( char ) uiLenLen;
         ui = 0;
         while( ui < uiLenLen )
         {
            *( pData++ ) = sNum[ ui++ ];
         }
      }
      *ulLen += uiLenLen + 1;
   }

   return pData;
}

void leto_clientlog( const char * sFile, int n, const char * s, ... )
{
   HB_FHANDLE handle;
#if defined( __XHARBOUR__ ) || defined( __HARBOUR30__ )
   char * sFileDefault = LetoSetModName( NULL );
#else
   char * sFileDefault = hb_cmdargProgName();
#endif
   char * sFileDef;

   if( sFileDefault )
   {
      int i = strlen( sFileDefault ) - 1;

      while( i >= 0 && sFileDefault[ i ] != '\\' && sFileDefault[ i ] != '/' )
         i--;
      if( i < 0 )
         i = 0;
      if( sFileDefault[ i ] == '/' || sFileDefault[ i ] == '\\' )
         i++;
      sFileDef = ( char * ) hb_xgrab( strlen( sFileDefault ) - i + 5 );
      strcpy( sFileDef, sFileDefault + i );
#if defined( HB_OS_WIN_32 ) || defined( HB_OS_WIN )
      i = strlen( sFileDef ) - 1;
      while( i >= 0 && sFileDef[ i ] != '.' )
         i--;
      if( i > 0 && sFileDef[ i ] == '.' )
         sFileDef[ i ] = '\0';
#endif
      strcpy( sFileDef + strlen( sFileDef ), ".log" );
   }
   else
   {
      sFileDef = ( char * ) hb_xgrab( 11 );
      strcpy( sFileDef, "client.log" );
   }

   if( hb_fsFile( ( sFile ) ? sFile : ( const char * ) sFileDef ) )
      handle = hb_fsOpen( ( sFile ) ? sFile : ( const char * ) sFileDef, FO_WRITE );
   else
      handle = hb_fsCreate( ( sFile ) ? sFile : ( const char * ) sFileDef, 0 );

   if( handle >= 0 )
   {
      hb_fsSeek( handle, 0, SEEK_END );
      if( n > 0 )
         hb_fsWrite( handle, s, n < ( int ) strlen( s ) ? ( HB_USHORT ) n : ( HB_USHORT ) strlen( s ) );
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

   hb_xfree( sFileDef );
#if ! defined( __HARBOUR30__ )
   hb_xfree( sFileDefault );
#endif
}

static PHB_ITEM leto_cloneError( PHB_ITEM pSourceError )
{
   PHB_ITEM pDestError = hb_errNew();

   hb_errPutCargo( pDestError, hb_errGetCargo( pSourceError ) );
   hb_errPutDescription( pDestError, hb_errGetDescription( pSourceError ) );
   hb_errPutFileName( pDestError, hb_errGetFileName( pSourceError ) );
   hb_errPutFlags( pDestError, hb_errGetFlags( pSourceError ) );
   hb_errPutGenCode( pDestError, hb_errGetGenCode( pSourceError ) );
   hb_errPutOperation( pDestError, hb_errGetOperation( pSourceError ) );
   hb_errPutOsCode( pDestError, hb_errGetOsCode( pSourceError ) );
   hb_errPutSeverity( pDestError, hb_errGetSeverity( pSourceError ) );
   hb_errPutSubCode( pDestError, hb_errGetSubCode( pSourceError ) );
   hb_errPutSubSystem( pDestError, hb_errGetSubSystem( pSourceError ) );
   hb_errPutTries( pDestError, hb_errGetTries( pSourceError ) );

   return pDestError;
}


/* check for thread global s_pError; clone into local var to keep mutex lock short */
HB_ERRCODE delayedError( void )
{
   HB_ERRCODE errCode = 0;
   PHB_ITEM   pError = NULL;

   if( s_pError )  /* this hurts mutex, but should be ok for this quick pre-test */
   {
      HB_GC_LOCKE();
      pError = leto_cloneError( s_pError );
      hb_errRelease( s_pError );
      s_pError = NULL;
      HB_GC_UNLOCKE();
   }

   if( pError )
   {
      HB_USHORT uiAction = hb_errLaunch( pError );

      if( ! ( uiAction == E_RETRY || uiAction == E_DEFAULT ) )
         errCode = hb_errGetSubCode( pError );
      hb_errRelease( pError );
      if( uiAction == E_BREAK )
      {
         hb_vmRequestQuit();
         /* ToDo wake up error thread ? */
      }
#ifdef LETO_CLIENTLOG
      leto_clientlog( NULL, 0, "delayedError( %d )", uiAction );
#endif
   }
   return errCode;
}

void leto_AddKeyToBuf( char * szData, const char * szKey, unsigned int uiKeyLen, unsigned long * pulLen )
{
   char * pData = szData + *pulLen;

   *pData++ = ( ( HB_BYTE ) uiKeyLen ) & 0xFF;
   memcpy( pData, szKey, uiKeyLen );
   pData += uiKeyLen;
   *pData = '\0';

   *pulLen = pData - szData;
}

static void leto_ClearTagInfos( LETOTABLE * pTable )
{
   LETOTAGINFO * pTagInfo = pTable->pTagInfo;

   while( pTagInfo )
   {
      LETOTAGINFO * pTagNext = pTagInfo->pNext;

      LetoDbFreeTag( pTagInfo );
      pTagInfo = pTagNext;
   }

   pTable->pTagInfo = NULL;
}

/* search reversed as at least for transactions expected to be a lastly added one */
static HB_BOOL leto_IsRecLocked( LETOTABLE * pTable, unsigned long ulRecNo )
{
   if( pTable->pLocksPos && pTable->ulLocksMax )
   {
      unsigned long ul = pTable->ulLocksMax - 1;

      do
      {
         if( pTable->pLocksPos[ ul ] == ulRecNo )
            return HB_TRUE;
      }
      while( ul-- );
   }
   return HB_FALSE;
}

HB_EXPORT int LetoGetConnectRes( void )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
      return pCurrentConn->iConnectRes;
   else
      return 1;
}

HB_EXPORT int LetoGetError( void )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
      return pCurrentConn->iError;
   else
      return -1;
}

HB_EXPORT const char * LetoGetServerVer( LETOCONNECTION * pConnection )
{
   return pConnection->szVersion;
}

HB_EXPORT int LetoGetCmdItem( char ** pptr, char * szDest )
{
   char * ptr = *pptr;

   if( ptr )
   {
      while( *ptr && *ptr != ';' )
         ptr++;
      if( *ptr )
      {
         if( ptr > *pptr )
            memcpy( szDest, *pptr, ptr - *pptr );
         szDest[ ptr - *pptr ] = '\0';
         *pptr = ptr;
         return 1;
      }
   }

   return 0;
}

HB_EXPORT const char * LetoFindCmdItem( const char * ptr )
{
   if( ptr )
   {
      while( *ptr )
      {
         if( *ptr == ';' )
            return ptr;
         ptr++;
      }
   }

   return NULL;
}

static const char * LetoParseItemEnd( const char * ptr )
{
   while( *ptr )
   {
      if( *ptr == ';' )
         return ptr;
      ptr++;
   }

   return ptr;
}

static void leto_ipInit( void )
{
#ifdef LETO_CLIENTLOG
   leto_clientlog( NULL, 0, "leto_ipInit( %d )", s_iSessions );
#endif

   if( s_iSessions )
      s_iSessions++;
   else
   {
      s_iSessions = 1;
      hb_socketInit();
   }
}

static void leto_ipCleanup( void )
{
#ifdef LETO_CLIENTLOG
   leto_clientlog( NULL, 0, "leto_ipCleanup( %d )", s_iSessions );
#endif

   if( --s_iSessions == 0 )
   {
      hb_socketCleanup();
   }
}

static HB_SOCKET leto_ipConnect( const char * szHost, int iPort, int iTimeOut, LETOCONNECTION * pConnection )
{
   HB_SOCKET sd;
   char *    pszIpAddres;
   int       iErrorCode = 0;

   pszIpAddres = hb_socketResolveAddr( szHost, HB_SOCKET_AF_INET );
   if( pszIpAddres == NULL )
      return HB_NO_SOCKET;
   sd = hb_socketOpen( HB_SOCKET_PF_INET, HB_SOCKET_PT_STREAM, 0 );
   if( sd != HB_NO_SOCKET )
   {
      void *   pSockAddr;
      unsigned uiLen;

      if( hb_socketInetAddr( &pSockAddr, &uiLen, pszIpAddres, iPort ) )
      {
         int iSize;

         hb_socketSetKeepAlive( sd, HB_TRUE );
         /* set 64KB send and receive buffer */
         hb_socketGetSndBufSize( sd, &iSize );
         if( iSize < 0xFFFF )
            hb_socketSetSndBufSize( sd, 0xFFFF );
         hb_socketGetRcvBufSize( sd, &iSize );
         if( iSize < 0xFFFF )
            hb_socketSetSndBufSize( sd, 0xFFFF );

         if( hb_socketConnect( sd, pSockAddr, uiLen, iTimeOut ) == 0 )
            hb_socketSetNoDelay( sd, HB_TRUE );
         else
         {
            iErrorCode = hb_socketGetError();
            hb_socketShutdown( sd, HB_SOCKET_SHUT_RDWR  );
            hb_socketClose( sd );
            sd = HB_NO_SOCKET;
         }
      }
      else
      {
         iErrorCode = hb_socketGetError();
         hb_socketShutdown( sd, HB_SOCKET_SHUT_RDWR  );
         hb_socketClose( sd );
         sd = HB_NO_SOCKET;
      }
      hb_xfree( pSockAddr );
   }
   else
      iErrorCode = hb_socketGetError();

   hb_xfree( pszIpAddres );

   if( pConnection )
      pConnection->iErrorCode = iErrorCode;

   return sd;
}

/* hb_xinit(), hb_threadInit( hb_threadStateNew() ) */

HB_EXPORT void LetoInit( void )
{
#ifdef LETO_MT
   LETOPOOL * pLetoPool = ( LETOPOOL * ) hb_stackGetTSD( &s_TSData );

   pLetoPool->uiConnCount = 1;
   pLetoPool->letoConnPool = hb_xgrabz( sizeof( LETOCONNECTION ) );
#else
   s_letoConnPool = hb_xgrabz( sizeof( LETOCONNECTION ) );
   s_uiConnCount = 1;
#endif

   leto_ipInit();
}

/*  hb_stackDestroyTSD(), hb_stackRelease(), hb_threadExit(), hb_xexit() */

#ifdef LETO_MT
HB_EXPORT void LetoExit( unsigned int uiFull )
{
   LETOPOOL * pLetoPool = ( LETOPOOL * ) hb_stackGetTSD( &s_TSData );

   if( pLetoPool->letoConnPool )
   {
      LETOCONNECTION * pConnection;
      unsigned int     ui;

      for( ui = 0; ui < pLetoPool->uiConnCount; ui++ )
      {
         pConnection = pLetoPool->letoConnPool + ui;
         if( pConnection->pAddr )
            LetoConnectionClose( pConnection );
      }
      hb_xfree( pLetoPool->letoConnPool );
      pLetoPool->letoConnPool = NULL;
   }
   pLetoPool->uiConnCount = 0;

#if defined( __XHARBOUR__ ) || defined( __HARBOUR30__ )
   if( s_szModName )
   {
      hb_xfree( s_szModName );
      s_szModName = NULL;
   }
#endif

   if( uiFull )
   {
#ifdef LETO_CLIENTLOG
      leto_clientlog( NULL, 0, "LetoExit( full )" );
#endif
      leto_ipCleanup();
   }
}

#else

HB_EXPORT void LetoExit( unsigned int uiFull )
{
   if( s_letoConnPool )
   {
      LETOCONNECTION * pConnection;
      unsigned int     i;

      for( i = 0; i < s_uiConnCount; i++ )
      {
         pConnection = s_letoConnPool + i;
         if( pConnection->pAddr )
            LetoConnectionClose( pConnection );
      }
      hb_xfree( s_letoConnPool );
      s_letoConnPool = NULL;
   }
   s_uiConnCount = 0;

#if defined( __XHARBOUR__ ) || defined( __HARBOUR30__ )
   if( s_szModName )
   {
      hb_xfree( s_szModName );
      s_szModName = NULL;
   }
#endif

   if( uiFull )
      leto_ipCleanup();
}
#endif

/* Note: first answer from server is never compressed, no pConnection->zstream matters */
static long leto_RecvFirst( LETOCONNECTION * pConnection )
{
   HB_SOCKET hSocket = pConnection->hSocket;
   long      lRet;
   char      szRet[ LETO_MSGSIZE_LEN + 1 ];
   char *    ptr = pConnection->szBuffer;
   HB_U32    ulMsgLen;

   lRet = hb_socketRecv( hSocket, szRet, LETO_MSGSIZE_LEN, 0, pConnection->iTimeOut );

   if( lRet < LETO_MSGSIZE_LEN )
   {
      printf( "hard fail after only <%ld> bytes\n", lRet ); fflush( stdout );
      return 0;
   }

   pConnection->uiProto = 3;
   pConnection->uiTBufOffset = 7;

   ulMsgLen = HB_GET_LE_UINT32( szRet );
   if( ! ulMsgLen || ( ulMsgLen > pConnection->ulBufferLen ) )
   {
#ifdef LETO_CLIENTLOG
      leto_clientlog( NULL, 0, "ERROR intro server will transfer <%lu>, we offer: %lu",
                      ulMsgLen, pConnection->ulBufferLen );
#endif
      /* about ulMsgLen == 17; should be expected */
      return 0;
   }

   do
   {
      lRet = hb_socketRecv( hSocket, ptr, ulMsgLen, 0, pConnection->iTimeOut );

      if( lRet <= 0 )
         break;
      else
      {
         ptr += lRet;
         ulMsgLen -= lRet;
      }
   }
   while( ulMsgLen );

   *ptr = '\0';

   return ( long ) ( ptr - pConnection->szBuffer );
}

/* will get a 4 bytes ACK or NIO */
static HB_BOOL leto_RecvSecond( LETOCONNECTION * pConnection )
{
   long lRet;
   char sRet[ 5 ];

   /* '-' plus max. 4 digit error number or '++++' */
   lRet = hb_socketRecv( pConnection->hSocketErr, sRet, 4, 0, 2000 );

   /* we may in future analyse error number ... */
   if( lRet < 4 || sRet[ 0 ] != '+' )
      return HB_FALSE;
   else
      return HB_TRUE;
}

#if defined( HB_OS_WIN )
#  define LETO_SOCK_GETERROR()       WSAGetLastError()
#  define LETO_SOCK_IS_EINTR( err )  ( ( err ) == WSAEINTR )
#  define LETO_SOCK_IS_EAGAIN( err ) ( ( err ) == WSAEAGAIN )
#else
#  define LETO_SOCK_GETERROR()       errno
#  define LETO_SOCK_IS_EINTR( err )  ( ( err ) == EINTR )
#  define LETO_SOCK_IS_EAGAIN( err ) ( ( err ) == EAGAIN )
#endif

static int leto_socketSelectRead( HB_SOCKET hSocket, HB_MAXINT lTimeOut )
{
   int    iChange;
   HB_I64 llMilliSec;

#if 1 && defined( HB_HAS_POLL )
   struct pollfd pPoll[ 1 ];

   pPoll[ 0 ].fd = hSocket;
   pPoll[ 0 ].events = POLLIN | POLLRDNORM;

   if( lTimeOut > 0 )
      llMilliSec = leto_MilliSec();
   else
      llMilliSec = 0;

   do
   {
      iChange = poll( pPoll, 1, lTimeOut );
      if( iChange > 0 && pPoll[ 0 ].revents & ( POLLERR | POLLHUP | POLLNVAL ) )
      {
         iChange = -1;  /* socket closed or else serious .. */
         break;
      }
      else if( iChange <= 0 )
      {
         if( LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
            continue;
#ifndef __SOCKET_EAGAIN__
         else if( LETO_SOCK_GETERROR() )
            break;
#else
         else if( ! LETO_SOCK_IS_EAGAIN( LETO_SOCK_GETERROR() ) && LETO_SOCK_GETERROR() )
            break;
#endif
         else
         {
            if( llMilliSec != 0 )
            {
               HB_I64 llTmp = leto_MilliSec();

               lTimeOut -= llTmp - llMilliSec;
               if( lTimeOut <= 0 )
                  break;
               else
               {
                  llMilliSec = llTmp;
                  continue;
               }
            }
            else
               continue;
         }
      }
      else
         break;
   }
   while( 1 );

#else
   fd_set readfds;
   struct timeval MicroWait;

   if( lTimeOut > 0 )
      llMilliSec = leto_MilliSec();
   else
      llMilliSec = 0;

   do
   {
      if( lTimeOut > 0 )
      {
         MicroWait.tv_sec = ( long ) ( lTimeOut / 1000 );
         MicroWait.tv_usec = ( long ) ( ( lTimeOut % 1000 ) * 1000 );
      }

      FD_ZERO( &readfds );
      FD_SET( hSocket, &readfds );

      iChange = select( ( int ) hSocket + 1, &readfds, NULL, NULL, lTimeOut > 0 ? &MicroWait : NULL );
      if( iChange <= 0 )
      {
         char c;

         if( LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
            continue;
#ifndef __SOCKET_EAGAIN__
         else if( LETO_SOCK_GETERROR() )
            break;
#else
         else if( ! LETO_SOCK_IS_EAGAIN( LETO_SOCK_GETERROR() ) && LETO_SOCK_GETERROR() )
            break;
#endif
         else if( recv( hSocket, &c, 1, MSG_PEEK ) <= 0 )  /* test read fails in case of closed socket */
         {
            iChange = -1;
            break;
         }
         else
         {
            if( llMilliSec != 0 )
            {
               HB_I64 llTmp = leto_MilliSec();

               lTimeOut -= llTmp - llMilliSec;
               if( lTimeOut <= 0 )
                  break;
               else
               {
                  llMilliSec = llTmp;
                  continue;
               }
            }
            else
               continue;
         }
      }
      else
         break;
   }
   while( HB_TRUE );

#endif

   return iChange;
}

/*
 * -1 == timed out or socket err
 * -2 == received less bytes as 4 ( LETO_MSGSIZE_LEN )
 * -3 == invalid LETO_MSGSIZE_LEN
 * -4 == received less than expected
 */
static long leto_Recv( LETOCONNECTION * pConnection )
{
   HB_ULONG ulMsgLen, ulRead = 0;
   long     lRet;
#ifdef USE_LZ4
   HB_BOOL  fCompressed = HB_FALSE;
#endif
#ifdef __SOCKET_EAGAIN__
   HB_I64   llMilliSec;

   if( pConnection->iTimeOut > 0 )
      llMilliSec = leto_MilliSec();
   else
      llMilliSec = 0;
   pConnection->iTimeOut
#endif

   hb_vmUnlock();
   if( ( lRet = leto_socketSelectRead( pConnection->hSocket, pConnection->iTimeOut ) ) <= 0 )
   {
      hb_vmLock();
      return lRet;
   }

   do
   {
#ifndef USE_LZ4
      if( pConnection->zstream )
         lRet = hb_znetRead( pConnection->zstream, pConnection->hSocket, pConnection->szBuffer + ulRead, LETO_MSGSIZE_LEN - ulRead, -1 );
      else
#endif
      {
         lRet = recv( pConnection->hSocket, pConnection->szBuffer + ulRead, LETO_MSGSIZE_LEN - ulRead, 0 );
         if( lRet < 0 )
         {
            if( LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
               continue;
         }
      }
      if( lRet > 0 )
         ulRead += lRet;
#ifndef __SOCKET_EAGAIN__
      else if( ! LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
         break;
#else
      else if( ! LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) && ! LETO_SOCK_IS_EAGAIN( LETO_SOCK_GETERROR() ) )
         break;
      else if( llMilliSec != 0 && leto_MilliSec() - llMilliSec > pConnection->iTimeOut )
         break;
#endif
   }
   while( ulRead < LETO_MSGSIZE_LEN );

   if( ulRead < LETO_MSGSIZE_LEN )
   {
      hb_vmLock();
      return -2;
   }

   ulMsgLen = HB_GET_LE_UINT32( pConnection->szBuffer );
#ifdef USE_LZ4
   if( pConnection->zstream )
   {
      fCompressed = ( ulMsgLen & 0x80000000 );
      if( fCompressed )
         ulMsgLen &= 0x7FFFFFFF;
   }
#endif
   if( ! ulMsgLen || ulMsgLen > LETO_MAX_RECV_BLOCK )
   {
      hb_vmLock();
      return -3;
   }

   /* ensure space enough for possible blowfish decrypt in same buffer + termination char */
   if( ulMsgLen + 9 > pConnection->ulBufferLen )
   {
      hb_vmLock();
      pConnection->ulBufferLen = HB_MAX( ulMsgLen + 9, LETO_SENDRECV_BUFFSIZE );
      pConnection->szBuffer = ( char * ) hb_xrealloc( pConnection->szBuffer, pConnection->ulBufferLen + 1 );
      hb_vmUnlock();
   }

   ulRead = 0;
   do
   {
#ifndef USE_LZ4
      if( pConnection->zstream )
         lRet = hb_znetRead( pConnection->zstream, pConnection->hSocket, pConnection->szBuffer + ulRead, ulMsgLen - ulRead, -1 );
      else
#endif
      {
         lRet = recv( pConnection->hSocket, pConnection->szBuffer + ulRead, ulMsgLen - ulRead, 0 );
         if( lRet < 0 )
         {
            if( LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
               continue;
         }
      }

      if( lRet > 0 )
         ulRead += lRet;
#ifndef __SOCKET_EAGAIN__
      else if( ! LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
         break;
#else
      else if( ! LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) && ! LETO_SOCK_IS_EAGAIN( LETO_SOCK_GETERROR() ) )
         break;
      else if( llMilliSec != 0 && leto_MilliSec() - llMilliSec > pConnection->iTimeOut )
         break;
#endif
      else
      {
         if( leto_socketSelectRead( pConnection->hSocket, 1000 ) <= 0 )  // come on !, one ! tiny pause ...
            break;
      }
   }
   while( ulRead < ulMsgLen );

   *( pConnection->szBuffer + ulRead ) = '\0';

   hb_vmLock();

   if( ulRead != ulMsgLen )
      return -4;

#ifdef USE_LZ4
   if( fCompressed || pConnection->fZipCrypt )  /* means compressed and/or encrypted */
      ulRead = hb_lz4netDecrypt( pConnection->zstream, &pConnection->szBuffer, ulRead, &pConnection->ulBufferLen, fCompressed );
#endif

#ifdef LETO_CLIENTLOG
   leto_clientlog( NULL, 0, "read (%d):%s", ulRead, pConnection->szBuffer  );
#endif

   return ( long ) ulRead;
}

/* non-blocking socket will not fail of EINTR  ?? */
/* http://stackoverflow.com/questions/14134440/eintr-and-non-blocking-calls */

/* MSG_MORE: a unix socket flag: data will *NOT* immediately send out, even not in NoDelay mode.
 * => no need to memcpy() data in a temp buffer to add data-length beforehand
 *    only use such buffer always for hb_znet connection
 * EXCEPTION for zipped traffic, here to do it 'old style' using receive buffer */

/* this are 4! variants in one: LZ4 or ZLib compression (hb_znet) --- with and without MSG_MORE flag */
static long leto_Send( LETOCONNECTION * pConnection, const char * szData, unsigned long ulLen )
{
   HB_ULONG ulSent;
   long     lTmp;
   int      iFlags, iErr = 0;
#ifndef USE_LZ4
   long     lLast = 1;
   int      iTimeOut = pConnection->iTimeOut;
#else
   HB_BOOL  fUseLZ4Buffer;
#endif

#if defined( MSG_NOSIGNAL )
   iFlags = MSG_NOSIGNAL;
#else
   iFlags = 0;
#endif

#ifndef USE_LZ4
   ulSent = LETO_MSGSIZE_LEN + ulLen + 9;
#else
   ulSent = LZ4_COMPRESSBOUND( ulLen ) + 24;
#endif
   if( ulSent > pConnection->ulBufferLen )
   {
      pConnection->ulBufferLen = ulSent;
      pConnection->szBuffer = ( char * ) hb_xrealloc( pConnection->szBuffer, pConnection->ulBufferLen + 1 );
   }

#ifdef MSG_MORE
   #ifndef USE_LZ4
   if( pConnection->zstream )
   {
      HB_PUT_LE_UINT32( pConnection->szBuffer, ulLen );
      memcpy( pConnection->szBuffer + LETO_MSGSIZE_LEN, szData, ulLen );
      ulLen += LETO_MSGSIZE_LEN;
      pConnection->szBuffer[ ulLen ] = '\0';
   }
   else
   #else
   fUseLZ4Buffer = hb_lz4netEncryptTest( pConnection->zstream, ulLen );
   if( ! fUseLZ4Buffer )
   #endif
   {
      char szMsgSize[ LETO_MSGSIZE_LEN ];

      ulSent = 0;
      HB_PUT_LE_UINT32( szMsgSize, ulLen );
      do
      {
         lTmp = send( pConnection->hSocket, szMsgSize + ulSent, LETO_MSGSIZE_LEN - ulSent, iFlags | MSG_MORE );
         if( lTmp > 0 )
            ulSent += lTmp;
         else
         {
            iErr = LETO_SOCK_GETERROR();
            if( LETO_SOCK_IS_EINTR( iErr ) )
            {
               iErr = 0;
               continue;
            }
            else if( iErr )
               return 0;
         }
      }
      while( ulSent < LETO_MSGSIZE_LEN );
   }

#else  /* _!_ MSG_MORE */

   #ifdef USE_LZ4
   fUseLZ4Buffer = hb_lz4netEncryptTest( pConnection->zstream, ulLen );
   if( ! fUseLZ4Buffer )
   #endif
   {
      HB_PUT_LE_UINT32( pConnection->szBuffer, ulLen );
      memcpy( pConnection->szBuffer + LETO_MSGSIZE_LEN, szData, ulLen );
      ulLen += LETO_MSGSIZE_LEN;
      pConnection->szBuffer[ ulLen ] = '\0';
   }
#endif  /* ! MSG_MORE */

#ifdef USE_LZ4
   if( fUseLZ4Buffer )
      ulLen = hb_lz4netEncrypt( pConnection->zstream, &pConnection->szBuffer, ulLen, &pConnection->ulBufferLen, szData );
#endif

   /* data length send/ processed, now main data */
   ulSent = 0;
   do
   {
#ifndef USE_LZ4
      if( pConnection->zstream )
      {
         lLast = 1;  /* lLast == 1 as start value -- EXTREME important else no send */
         lTmp = hb_znetWrite( pConnection->zstream, pConnection->hSocket, pConnection->szBuffer + ulSent, ulLen - ulSent, -1, &lLast );
      }
      else
#endif
#ifdef MSG_MORE
   #ifdef USE_LZ4
         if( fUseLZ4Buffer )
            lTmp = send( pConnection->hSocket, pConnection->szBuffer + ulSent, ulLen - ulSent, iFlags );
         else
   #endif
            lTmp = send( pConnection->hSocket, szData + ulSent, ulLen - ulSent, iFlags );
#else  /* _!_ MSG_MORE */
         lTmp = send( pConnection->hSocket, pConnection->szBuffer + ulSent, ulLen - ulSent, iFlags );
#endif

      /* ToDo -- add a timer here for the former iTimeCount and break after that */
      if( lTmp > 0 )
         ulSent += lTmp;
      else
      {
#ifndef USE_LZ4
         if( pConnection->zstream )
            iErr = hb_socketGetError();
         else
            iErr = LETO_SOCK_GETERROR();
#else
         iErr = LETO_SOCK_GETERROR();
#endif
         if( LETO_SOCK_IS_EINTR( iErr ) )
         {
            iErr = 0;
            continue;
         }
#ifndef USE_LZ4
         else if( iErr || hb_vmRequestQuery() != 0 || lLast <= 0 )
            break;
#else
         else if( iErr )
            break;
#endif
      }
   }
   while( ulSent < ulLen );

#ifndef USE_LZ4
   if( pConnection->zstream && lLast > 0 && ! iErr )
   {
   #if defined( __HARBOUR30__ )  /* 26.08.2015 new param --> hb_sockexFlush() */
      if( ( lTmp = hb_znetFlush( pConnection->zstream, pConnection->hSocket, iTimeOut ) ) != 0 )
   #else
      if( ( lTmp = hb_znetFlush( pConnection->zstream, pConnection->hSocket, iTimeOut, HB_FALSE ) ) != 0 )
   #endif
      {
   #ifdef LETO_CLIENTLOG
         leto_clientlog( NULL, 0, "ERROR leto_Send() not flushed: %ld", lTmp );
   #endif
         iErr = hb_socketGetError();
      }
   }
#endif

   if( iErr || ulSent < ulLen )
   {
#if ! defined( __HARBOUR30__ )  /* new function since 2015/08/17 */
      if( iErr )
         hb_socketSetError( iErr );
#endif
      ulSent = 0;
   }

   return ( long ) ulSent;
}

/* splitted send and receive -- this func calls both after another */
long leto_DataSendRecv( LETOCONNECTION * pConnection, const char * szData, unsigned long ulLen )
{
   long lRecv;

   if( ! ulLen )
      ulLen = strlen( szData );

   lRecv = leto_Send( pConnection, szData, ulLen );
   pConnection->iError = delayedError();
   if( pConnection->iError )
      lRecv = 0;
   else if( ! lRecv )
   {
#if ! defined( __HARBOUR30__ )  /* new function since 2015/08/17 */
      hb_socketSetError( LETO_SOCK_GETERROR() );
#endif
      pConnection->fMustResync = HB_TRUE;
      pConnection->iError = 1000;
   }

   if( lRecv > 0 )  /* only receive in case of no above send errors */
   {
      lRecv = leto_Recv( pConnection );
      if( lRecv <= 0 )
      {
#if ! defined( __HARBOUR30__ )  /* new function since 2015/08/17 */
         hb_socketSetError( LETO_SOCK_GETERROR() );
#endif
         lRecv = 0;
         pConnection->fMustResync = HB_TRUE;
         pConnection->iError = 1000;
      }
   }

   return lRecv;
}

/* if no second socket for asynchronous error is available send it the over first socket, wait for response */
unsigned long leto_SendRecv2( LETOCONNECTION * pConnection, const char * szData, unsigned long ulLen, int iErr )
{
   unsigned long ulRet;

   if( ! ulLen )
      ulLen = strlen( szData );

   if( pConnection->hSocketErr != HB_NO_SOCKET )
   {
      ulRet = leto_Send( pConnection, szData, ulLen );

      /* also check for deleayed error */
      pConnection->iError = delayedError();
      if( pConnection->iError )
         ulRet = 0;
      else if( ! ulRet )
      {
         pConnection->fMustResync = HB_FALSE;
         pConnection->iError = 1000;
      }
   }
   else
   {
      ulRet = leto_DataSendRecv( pConnection, szData, ulLen );

      if( ulRet && *( pConnection->szBuffer ) == '-' && iErr )
      {
         pConnection->iError = iErr;
         ulRet = 0;
      }
   }

   return ulRet;
}

static _HB_INLINE_ unsigned long leto_SendRecv( LETOCONNECTION * pConnection, const char * szData, unsigned long ulLen, int iErr )
{
   unsigned long ulRet;

   ulRet = leto_DataSendRecv( pConnection, szData, ulLen );
   if( ulRet && iErr && *( pConnection->szBuffer ) == '-' )
   {
      pConnection->iError = iErr;
      ulRet = 0;
   }

   return ulRet;
}

HB_EXPORT HB_ERRCODE LetoSet( LETOCONNECTION * pConnection, int iCommand, const char * szCommand )
{
   char          szData[ 32 ];
   unsigned long ulLen;

   ulLen = eprintf( szData, "%c;%d;%s;", LETOCMD_set, iCommand, szCommand );
   if( ! leto_SendRecv2( pConnection, szData, ulLen, 0 ) )
      return HB_FAILURE;

   return HB_SUCCESS;
}

#ifdef LETO_MT
LETOCONNECTION * leto_ConnectionFind( const char * szAddr, int iPort )
{
   LETOPOOL *   pLetoPool = ( LETOPOOL * ) hb_stackGetTSD( &s_TSData );
   unsigned int ui;

   for( ui = 0; ui < pLetoPool->uiConnCount; ui++ )
   {
      if( pLetoPool->letoConnPool[ ui ].pAddr &&
          ! strcmp( pLetoPool->letoConnPool[ ui ].pAddr, szAddr ) &&
          pLetoPool->letoConnPool[ ui ].iPort == iPort )
      {
         pLetoPool->pCurrentConn = pLetoPool->letoConnPool + ui;
         return pLetoPool->pCurrentConn;
      }
   }
   return NULL;
}

#else  /* ! LETO_MT */

LETOCONNECTION * leto_ConnectionFind( const char * szAddr, int iPort )
{
   HB_USHORT i;

   for( i = 0; i < s_uiConnCount; i++ )
   {
      if( s_letoConnPool[ i ].pAddr &&
          ! strcmp( s_letoConnPool[ i ].pAddr, szAddr ) &&
          s_letoConnPool[ i ].iPort == iPort )
      {
         s_pCurrentConn = s_letoConnPool + i;
         return s_pCurrentConn;
      }
   }
   return NULL;
}
#endif

static const char * leto_GetServerCdp( LETOCONNECTION * pConnection, const char * szCdp )
{
   PCDPSTRU pCdps = pConnection->pCdpTable;

   while( szCdp && pCdps )
   {
      if( ! strcmp( szCdp, pCdps->szClientCdp ) )
      {
         szCdp = pCdps->szServerCdp;
         break;
      }
      pCdps = pCdps->pNext;
   }
   return szCdp;
}

#if 0  /* do not remove -- for future use */
int LetoCheckServerVer( LETOCONNECTION * pConnection, HB_USHORT uiVer )
{
   return ( int ) ( pConnection->uiMajorVer * 100 + pConnection->uiMinorVer ) >= uiVer;
}
#endif

const char * leto_RemoveIpFromPath( const char * szPath )
{
   if( szPath && strlen( szPath ) >= 5 && szPath[ 0 ] == '/' && szPath[ 1 ] == '/' )
   {
      const char * ptr = strchr( szPath + 2, '/' );

      if( ptr &&  ptr - szPath <= 71 )  /* "//123.123.123.123:12345/" or //DNS63:12345/ */
         return ptr + 1;
   }
   return szPath;
}

/* remove drive letter and duplicated path separator at beginning */
void leto_BeautifyPath( char * szPath )
{
   char * ptr = szPath;

   if( *ptr && ptr[ 1 ] == ':' )  /* C:... */
      memmove( szPath, ptr + 2, strlen( ptr + 2 ) + 1 );  /* including '\0' */

   if( *ptr == '/' || *ptr == '\\' )
   {
      ptr++;
      while( *ptr == '/' || *ptr == '\\' )
      {
         memmove( ptr, ptr + 1, strlen( ptr + 1 ) + 1 );  /* including '\0' */
      }
   }
}

HB_BOOL leto_getIpFromPath( const char * szSource, char * szAddr, int * piPort, char * szPath )
{
   const char * ptrPort = szSource;
   const char * ptr = szSource;
   int          iLen = ptr ? strlen( ptr ) : 0;
   HB_BOOL      fWithPort = HB_TRUE, fWithIP = iLen >= 5 ? HB_TRUE : HB_FALSE;

   szAddr[ 0 ] = '\0';
   while( iLen >= 5 )  /* "//1.1.1.1/" len 10-18 or "//lh/" len 5-66 */
   {
      if( ptr[ 0 ] != '/' || ptr[ 1 ] != '/' )
      {
         fWithIP = HB_FALSE;
         break;
      }
      ptr += 2;
      if( ( ptrPort = strchr( ptr, ':' ) ) == NULL )  /* no port */
      {
         if( ( ptrPort = strchr( ptr, '/' ) ) == NULL )
         {
            fWithIP = HB_FALSE;
            break;
         }
         else if( ptrPort - ptr > 63 || ptrPort - ptr < 2 )
         {
            fWithIP = HB_FALSE;
            break;
         }
         fWithPort = HB_FALSE;
         if( ! *piPort )
            *piPort = LETO_DEFAULT_PORT;
      }
      else if( strchr( ptrPort, '/' ) == NULL )  /* no ... / */
      {
         fWithIP = HB_FALSE;
         break;
      }
      else if( ptrPort - ptr < 2 )  /* address or hostname min len */
      {
         fWithIP = HB_FALSE;
         break;
      }
      else
         *piPort = atoi( ptrPort + 1 );

      memcpy( szAddr, ptr, ptrPort - ptr );
      szAddr[ ptrPort - ptr ] = '\0';
      if( atoi( szAddr ) == 0 )  /* instead a valid hostname ? */
      {
         char * szIP = hb_socketResolveAddr( szAddr, HB_SOCKET_AF_INET );

         if( szIP )
         {
            if( atoi( szIP ) == 0 )
            {
               szAddr[ 0 ] = '\0';
               fWithIP = HB_FALSE;
               hb_xfree( szIP );
               break;
            }
            else
            {
               strcpy( szAddr, szIP );
               hb_xfree( szIP );
            }
         }
      }

      if( fWithPort )
      {
         ptrPort++;
         while( *ptrPort && *ptrPort >= '0' && *ptrPort <= '9' )
            ptrPort++;
      }
      ptrPort++;

      break;
   }

   /* ptrPort positioned after '//IP:port/' or at start of szSource */
   if( szPath && ptrPort )
   {
      if( *ptrPort )
         strcpy( szPath, ptrPort );
   }

   return fWithIP;
}

void leto_getFileFromPath( const char * szSource, char * szFile, HB_USHORT uLenMax )
{
   const char * ptrEnd = szSource + strlen( szSource ) - 1;
   const char * ptr = ptrEnd;
   HB_USHORT    uLen;

   while( ptr >= szSource && *ptr != '/' && *ptr != '\\' )
   {
      if( ptr > szSource )
         ptr--;
      else
         break;
   }
   if( *ptr == '/' || *ptr == '\\' )
      ptr++;
   uLen = ( HB_USHORT ) ( ptrEnd - ptr + 1 );
   if( uLen > uLenMax - 1 )
      uLen = uLenMax - 1;
   strncpy( szFile, ptr, uLen );
   szFile[ uLen ] = '\0';
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

const char * leto_DecryptText( LETOCONNECTION * pConnection, HB_ULONG * pulLen, char * ptr )
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
      if( *pulLen > pConnection->ulBufCryptLen )
      {
         if( ! pConnection->ulBufCryptLen )
            pConnection->pBufCrypt = ( char * ) hb_xgrab( *pulLen + 1 );
         else
            pConnection->pBufCrypt = ( char * ) hb_xrealloc( pConnection->pBufCrypt, *pulLen + 1 );
         pConnection->ulBufCryptLen = ( HB_ULONG ) *pulLen;
      }

#ifdef USE_LZ4
      leto_lz4Uncompress( pConnection->pBufCrypt, ( HB_SIZE * ) pulLen, ( const char * ) ptr, nSize );
#else
      hb_zlibUncompress( pConnection->pBufCrypt, ( HB_SIZE * ) pulLen, ( const char * ) ptr, nSize );
#endif
      ptr = pConnection->pBufCrypt;
   }

   ptr[ *pulLen ] = '\0';
   return ptr;
}

HB_ULONG leto_CryptText( LETOCONNECTION * pConnection, const char * pData, HB_ULONG ulLen, HB_ULONG ulPrelead )
{
   HB_ULONG ulBufLen;
   HB_SIZE  nDest;
   HB_BOOL  fCompress;

#ifdef USE_LZ4
   fCompress = ( pConnection->iZipRecord < 1 && ulLen > LETO_LZ4_COMPRESS_MIN ) ? HB_TRUE : HB_FALSE;
   if( fCompress )
   {
      nDest = ( HB_SIZE ) LZ4_COMPRESSBOUND( ulLen );
      if( ! nDest )  /* too big > 0x7E000000 */
      {
         pConnection->iError = 1021;
         ulLen = 0;
         ulBufLen = 5 + ulPrelead;
      }
      else
         ulBufLen = nDest + 21 + ulPrelead;  /* encrypt +8, zip-lengths +8, length +4, termination + 1  */
   }
#else
   fCompress = ( pConnection->iZipRecord < 1 && ulLen > LETO_ZIP_MINLENGTH ) ? HB_TRUE : HB_FALSE;
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

   if( ulBufLen > pConnection->ulBufCryptLen )
   {
      if( ! pConnection->ulBufCryptLen )
         pConnection->pBufCrypt = ( char * ) hb_xgrab( ulBufLen + 1 );
      else
         pConnection->pBufCrypt = ( char * ) hb_xrealloc( pConnection->pBufCrypt, ulBufLen + 1 );
      pConnection->ulBufCryptLen = ( HB_ULONG ) ulBufLen;
   }

   if( ulLen )
   {
      /* compress here, if not later done by compressed traffic */
      if( fCompress )
      {
#ifdef USE_LZ4
         leto_lz4Compress( ( char * ) pConnection->pBufCrypt + 4 + ulPrelead, &nDest, ( const char * ) pData, ulLen,
                          HB_ZLIB_COMPRESSION_SPEED );
#else
         hb_zlibCompress( ( char * ) pConnection->pBufCrypt + 4 + ulPrelead, &nDest, ( const char * ) pData, ulLen,
                          HB_ZLIB_COMPRESSION_SPEED );
#endif
         HB_PUT_LE_UINT32( pConnection->pBufCrypt + 4 + nDest + ulPrelead, nDest );
         HB_PUT_LE_UINT32( pConnection->pBufCrypt + 4 + nDest + 4 + ulPrelead, ulLen );
         ulLen = ( nDest + 8 );
         HB_PUT_LE_UINT32( pConnection->pBufCrypt + ulPrelead, ulLen | 0x80000000 );
      }
      else
      {
         memcpy( pConnection->pBufCrypt + 4 + ulPrelead, pData, ulLen );
         HB_PUT_LE_UINT32( pConnection->pBufCrypt + ulPrelead, ulLen );
      }
   }
   else  /* ulLen == 0 */
      memset( pConnection->pBufCrypt + ulPrelead, '\0', 4 );

   ulLen += 4;
   pConnection->pBufCrypt[ ulLen + ulPrelead ] = '\0';

   return ulLen;
}

static _HB_INLINE_ unsigned int leto_IsBinaryField( unsigned int uiType, unsigned int uiLen )
{
   return ( ( uiType == HB_FT_MEMO || uiType == HB_FT_BLOB ||
              uiType == HB_FT_IMAGE || uiType == HB_FT_OLE ) && uiLen != 10 ) ||
          ( uiType == HB_FT_DATE && uiLen != 8 ) ||
          uiType == HB_FT_TIME ||
          uiType == HB_FT_TIMESTAMP ||
          uiType == HB_FT_MODTIME ||
          uiType == HB_FT_ANY ||
          uiType == HB_FT_INTEGER ||
          uiType == HB_FT_ROWVER ||
          uiType == HB_FT_AUTOINC ||
          uiType == HB_FT_DOUBLE ||
          uiType == HB_FT_CURDOUBLE ||
          uiType == HB_FT_VARLENGTH ||
          uiType == HB_FT_CURRENCY;
}

/* blanks all fields content to ' ', then only binary fields to '\0' */
static void leto_SetBlankRecord( LETOTABLE * pTable )
{
   /* set all to white space, revert later for binary fields to '\0' */
   memset( pTable->pRecord, ' ', pTable->uiRecordLen );
   if( pTable->fHaveBinary )
   {
      LETOFIELD * pField = pTable->pFields;
      HB_USHORT   uiCount;

      for( uiCount = 0; uiCount < pTable->uiFieldExtent; uiCount++, pField++ )
      {
         if( pTable->pFieldIsBinary[ uiCount ] )
            memset( pTable->pRecord + pTable->pFieldOffset[ uiCount ], '\0', pField->uiLen );
#if 0  /* ToDo ? unicode support */
         else if( pField->uiFlags & HB_FF_UNICODE )
         {
            HB_USHORT uiLen = pField->uiLen;

            while( uiLen-- )
            {
               HB_PUT_LE_UINT16( pPtr, 0x0020 );
               pPtr += 2;
            }
         }
#endif
      }
   }
}

/* optimized: hb_setGetDeleted() change checked with LETO_SET() */
static _HB_INLINE_ HB_BOOL leto_HotBuffer( LETOTABLE * pTable )
{
   return ( LETO_CENTISEC() - pTable->llCentiSec < pTable->iBufRefreshTime || pTable->iBufRefreshTime == 0 );
}

static _HB_INLINE_ HB_BOOL leto_OutBuffer( LETOBUFFER * pLetoBuf, char * ptr )
{
   return ( ( unsigned long ) ( ptr - ( char * ) pLetoBuf->pBuffer ) ) >= pLetoBuf->ulBufDataLen - 1;
}

static _HB_INLINE_ void leto_setSkipBuf( LETOTABLE * pTable, const char * ptr, unsigned long ulDataLen )
{
   LETOBUFFER * pLetoBuf = &pTable->Buffer;

   if( ulDataLen > pLetoBuf->ulBufLen )
   {
      if( ! pLetoBuf->ulBufLen )
         pLetoBuf->pBuffer = ( HB_BYTE * ) hb_xgrab( ulDataLen );
      else
         pLetoBuf->pBuffer = ( HB_BYTE * ) hb_xrealloc( pLetoBuf->pBuffer, ulDataLen );
      pLetoBuf->ulBufLen = ulDataLen;
   }
   pLetoBuf->ulBufDataLen = ulDataLen;

   memcpy( ( char * ) pTable->Buffer.pBuffer, ptr, ulDataLen );
   pTable->ptrBuf = pTable->Buffer.pBuffer;
   pTable->uiRecInBuf = 0;
   pTable->llCentiSec = LETO_CENTISEC();
}

/* pTable->ptrBuf must be pre-checked to be not NULL */
static void leto_refrSkipBuf( LETOTABLE * pTable )
{
   HB_ULONG ulRemove = pTable->ptrBuf - pTable->Buffer.pBuffer;

   if( ulRemove )
   {
      memmove( pTable->Buffer.pBuffer, pTable->ptrBuf, pTable->Buffer.ulBufDataLen - ulRemove );
      pTable->Buffer.ulBufDataLen -= ulRemove;
      pTable->ptrBuf = pTable->Buffer.pBuffer;
#ifdef LETO_CLIENTLOG
      leto_clientlog( NULL, 0, "DEBUG leto_refrSkipBuf new record %lu restlen %lu (%lu)",
                      HB_GET_LE_UINT32( pTable->ptrBuf + 4 ), pTable->Buffer.ulBufDataLen, ulRemove );
#endif
   }
   pTable->uiRecInBuf = 0;
}

static _HB_INLINE_ HB_ULONG leto_TransBlockLen( LETOCONNECTION * pConnection, HB_ULONG ulLen )
{
   return pConnection->ulTransBlockLen ? pConnection->ulTransBlockLen : ( ulLen < 512 ) ? 8192 : ulLen * 16;
}

static void leto_AddTransBuffer( LETOCONNECTION * pConnection, const char * pData, HB_ULONG ulLen )
{
   if( ! pConnection->szTransBuffer )
   {
      pConnection->ulTransBuffLen = leto_TransBlockLen( pConnection, ulLen );
      pConnection->szTransBuffer = ( HB_BYTE * ) hb_xgrab( pConnection->ulTransBuffLen );
      pConnection->szTransBuffer[ 0 ] = LETOCMD_ta;
      pConnection->szTransBuffer[ 1 ] = ';';
   }
   if( ! pConnection->ulTransDataLen )
      pConnection->ulTransDataLen = pConnection->uiTBufOffset;

   if( pConnection->ulTransBuffLen - pConnection->ulTransDataLen <= ulLen )
   {
      pConnection->ulTransBuffLen = pConnection->ulTransDataLen + leto_TransBlockLen( pConnection, ulLen );
      pConnection->szTransBuffer = ( HB_BYTE * ) hb_xrealloc( pConnection->szTransBuffer, pConnection->ulTransBuffLen );
   }
   memcpy( pConnection->szTransBuffer + pConnection->ulTransDataLen, pData, ulLen );
   pConnection->ulTransDataLen += ulLen;
   pConnection->ulRecsInTrans++;
}

static HB_BOOL leto_SearchTransList( LETOCONNECTION * pConnection, HB_ULONG hTable, HB_ULONG ulRecNo )
{
   /* recently searched to be not in list */
   if( pConnection->pRecsNotList.ulRecNo != ulRecNo &&
       pConnection->pRecsNotList.hTable != hTable )
   {
      HB_ULONG ul = 0;

      while( ul < pConnection->ulRecsInList )
      {
         if( pConnection->pTransList[ ul ].ulRecNo == ulRecNo &&
             pConnection->pTransList[ ul ].hTable == hTable )
            return HB_TRUE;
         ul++;
      }
      pConnection->pRecsNotList.ulRecNo = ulRecNo;
      pConnection->pRecsNotList.hTable = hTable;
   }

   return HB_FALSE;
}

static void leto_AddTransList( LETOCONNECTION * pConnection, LETOTABLE * pTable )
{
   if( ! leto_SearchTransList( pConnection, pTable->hTable, pTable->ulRecNo ) )
   {
      if( ! pConnection->pTransList )
      {
         pConnection->pTransList = ( TRANSACTLIST * ) hb_xgrab( sizeof( TRANSACTLIST ) * 32 );
         pConnection->ulTransListLen = 32;
      }
      else if( pConnection->ulRecsInList >= pConnection->ulTransListLen )
      {
         pConnection->ulTransListLen += 32;
         pConnection->pTransList = ( TRANSACTLIST * ) hb_xrealloc( pConnection->pTransList,
                                                                   sizeof( TRANSACTLIST ) * pConnection->ulTransListLen );
      }
#if 0  /* experimental verify */
      if( ! ( ! pTable->fShared || pTable->fFLocked || leto_IsRecLocked( pTable, pTable->ulRecNo ) ) )
         pConnection->ulTransLockErr++;
#endif

      pConnection->pTransList[ pConnection->ulRecsInList ].hTable = pTable->hTable;
      pConnection->pTransList[ pConnection->ulRecsInList ].ulRecNo = pTable->ulRecNo;
      pConnection->ulRecsInList++;

      pConnection->pRecsNotList.ulRecNo = pConnection->pRecsNotList.hTable = 0;
   }
}

static void leto_AddTransAppend( LETOCONNECTION * pConnection, LETOTABLE * pTable )
{
   HB_USHORT ui = 0;

   while( ui < pConnection->uiTransAppend )
   {
      if( pConnection->pTransAppend[ ui ].pTable == pTable )
         break;
      ui++;
   }

   if( ui >= pConnection->uiTransAppend )
   {
      if( ! pConnection->pTransAppend )
      {
         pConnection->pTransAppend = ( TRANSACTWA * ) hb_xgrab( sizeof( TRANSACTWA ) * 16 );
         pConnection->uiTransAppLen = 16;
      }
      else if( pConnection->uiTransAppend >= pConnection->uiTransAppLen )
      {
         pConnection->uiTransAppLen += 16;
         pConnection->pTransAppend = ( TRANSACTWA * ) hb_xrealloc( pConnection->pTransAppend,
                                                                   sizeof( TRANSACTWA ) * pConnection->uiTransAppLen );
      }

      pConnection->pTransAppend[ ui ].pTable = pTable;
      pConnection->pTransAppend[ ui ].ulRecNo = 0;
      pConnection->uiTransAppend++;
   }

   if( ! pConnection->pTransAppend[ ui ].ulRecNo )
   {
      if( ! pTable->fShared || pTable->fFLocked )
         pConnection->pTransAppend[ ui ].ulRecNo = 999999999;
      else if( pTable->ulLocksMax )
         pConnection->pTransAppend[ ui ].ulRecNo = pTable->pLocksPos[ pTable->ulLocksMax - 1 ];
   }
}

HB_EXPORT void LetoDbFreeTag( LETOTAGINFO * pTagInfo )
{
   if( pTagInfo->BagName )
      hb_xfree( pTagInfo->BagName );
   if( pTagInfo->TagName )
      hb_xfree( pTagInfo->TagName );
   if( pTagInfo->KeyExpr )
      hb_xfree( pTagInfo->KeyExpr );
   if( pTagInfo->ForExpr )
      hb_xfree( pTagInfo->ForExpr );
   if( pTagInfo->pTopScopeAsString )
      hb_xfree( pTagInfo->pTopScopeAsString );
   if( pTagInfo->pBottomScopeAsString )
      hb_xfree( pTagInfo->pBottomScopeAsString );

   hb_xfree( pTagInfo );
}

static unsigned int leto_checkLockError( LETOCONNECTION * pConnection )
{
   char * ptr = pConnection->szBuffer;

   if( *ptr == '-' )
   {
      if( *( ptr + 3 ) != '4' )
         pConnection->iError = 1038;
      return 1;
   }
   return 0;
}

const char * leto_ParseTagInfo( LETOTABLE * pTable, const char * pBuffer )
{
   HB_USHORT    uiOrders = ( HB_USHORT ) atoi( pBuffer );
   const char * ptr = pBuffer;

   while( *ptr && *ptr++ != ';' )
      ;
   if( pTable->pTagInfo && pTable->uiOrders > uiOrders )  /* unregister all tags */
      leto_ClearTagInfos( pTable );
   if( ! pTable->pTagInfo || ! uiOrders )
      pTable->pTagCurrent = NULL;

   if( uiOrders )
   {
      LETOTAGINFO * pTagInfo = pTable->pTagInfo;
      const char *  ptr2;
      HB_USHORT     uiCount, uiActive;
      int           iLen;

      uiActive = ( HB_USHORT ) atoi( ptr );
      if( ! uiActive )
         pTable->pTagCurrent = NULL;
      while( *ptr && *ptr++ != ';' )
         ;

      for( uiCount = 1; uiCount <= uiOrders; uiCount++ )
      {
         if( ! pTagInfo )  /* new Tag to register -- ToDo : check also TagName and delete before */
         {
            LETOTAGINFO * pTagNew = ( LETOTAGINFO * ) hb_xgrabz( sizeof( LETOTAGINFO ) );

            if( uiCount == 1 )
            {
               pTable->pTagInfo = pTagNew;
               pTagInfo = pTable->pTagInfo;
               if( uiActive == uiCount )
                  pTable->pTagCurrent = pTagInfo;
            }
            else
            {
               pTagInfo = pTable->pTagInfo;
               while( pTagInfo->pNext )
                  pTagInfo = pTagInfo->pNext;
               pTagInfo->pNext = pTagNew;
               pTagInfo = pTagInfo->pNext;
            }
            pTagInfo->uiTag = uiCount;
            if( uiActive == uiCount )
               pTable->pTagCurrent = pTagInfo;
         }
         else  /* Tag is registered, skip like in leto_SkipTagInfo() */
         {
            int i;

            if( uiActive == uiCount )
               pTable->pTagCurrent = pTagInfo;
            for( i = 0; i < 9; i++ )
            {
               while( *ptr && *ptr++ != ';' )
                  ;
            }
            pTagInfo = pTagInfo->pNext;
            continue;
         }

         ptr2 = LetoParseItemEnd( ptr );
         if( ( iLen = ptr2 - ptr ) > 1 )  /* pre-leading '*' for production index */
         {
            char * ptrTran;

            pTagInfo->fProduction = ( *ptr == '*' );
            pTagInfo->BagName = ( char * ) hb_xgrab( iLen );
            memcpy( pTagInfo->BagName, ptr + 1, iLen - 1 );
            pTagInfo->BagName[ iLen - 1 ] = '\0';
            ptrTran = pTagInfo->BagName;
            while( *ptrTran )
            {
               if( *ptrTran == DEF_CH_SEP )
                  *ptrTran = DEF_SEP;
               ptrTran++;
            }
         }
         else
            pTagInfo->BagName = NULL;

         ptr = ptr2 + 1;
         ptr2 = LetoParseItemEnd( ptr );
         if( ( iLen = ptr2 - ptr ) > 0 )
         {
            if( iLen > LETO_MAX_TAGNAME )
               iLen = LETO_MAX_TAGNAME;
            pTagInfo->TagName = ( char * ) hb_xgrab( iLen + 1 );
            memcpy( pTagInfo->TagName, ptr, iLen );
            pTagInfo->TagName[ iLen ] = '\0';
         }
         else
            pTagInfo->TagName = NULL;

         ptr = ptr2 + 1;
         ptr2 = LetoParseItemEnd( ptr );
         iLen = ptr2 - ptr;
         pTagInfo->KeyExpr = ( char * ) hb_xgrab( iLen + 1 );
         if( iLen )
            memcpy( pTagInfo->KeyExpr, ptr, iLen );
         pTagInfo->KeyExpr[ iLen ] = '\0';

         ptr = ptr2 + 1;
         ptr2 = LetoParseItemEnd( ptr );
         if( ( iLen = ptr2 - ptr ) > 0 )
         {
            pTagInfo->ForExpr = ( char * ) hb_xgrab( iLen + 1 );
            memcpy( pTagInfo->ForExpr, ptr, iLen );
            pTagInfo->ForExpr[ iLen ] = '\0';
         }
         else
            pTagInfo->ForExpr = NULL;

         ptr = ptr2 + 1;
         pTagInfo->cKeyType = *ptr;
         ptr += 2;

         if( pTagInfo->cKeyType == 'T' )  /* T-imestamp, 'N|C|L|D' */
            pTagInfo->uiKeySize = 18;
         else
            pTagInfo->uiKeySize = ( HB_USHORT ) atoi( ptr );
         while( *ptr && *ptr++ != ';' )
            ;

         pTagInfo->fUsrAscend = ! ( *ptr == 'T' );
         ptr += 2;

         pTagInfo->fUniqueKey = ( *ptr == 'T' );
         ptr += 2;

         pTagInfo->fCustom = ( *ptr == 'T' );
         ptr += 2;

         if( uiCount < uiOrders )
            pTagInfo = pTagInfo->pNext;
      }
   }
   pTable->uiOrders = uiOrders;

   return ptr;
}

void leto_SetUpdated( LETOTABLE * pTable, HB_USHORT uiUpdated )
{
   pTable->uiUpdated = uiUpdated;
   memset( pTable->pFieldUpd, 0, pTable->uiFieldExtent * sizeof( HB_UCHAR ) );
}

void leto_ParseRecord( LETOCONNECTION * pConnection, LETOTABLE * pTable, const char * szData )
{
   const char * ptr = szData + 3;  /* after leading UINT24 */

   if( *ptr == 0x40 )
      pTable->fBof = pTable->fEof = pTable->fRecLocked = pTable->fDeleted = pTable->fFound = HB_FALSE;
   else
   {
      pTable->fBof = ( *ptr & LETO_FLG_BOF );
      pTable->fEof = ( *ptr & LETO_FLG_EOF );
      pTable->fRecLocked = ( *ptr & LETO_FLG_LOCKED );
      pTable->fDeleted = ( *ptr & LETO_FLG_DEL );
      pTable->fFound = ( *ptr & LETO_FLG_FOUND );
   }
   pTable->ulRecNo = HB_GET_LE_UINT32( ( const HB_BYTE * ) ( ptr + 1 ) );
   ptr += 6;  /* data above + ';' */

   if( pConnection->iZipRecord >= 0 )
   {
      memcpy( pTable->pRecord, ptr, pTable->uiRecordLen );   /* new: WITH delete flag */
      ptr += pTable->uiRecordLen;
   }
   else if( pTable->fEof )
   {
      leto_SetBlankRecord( pTable );
      ptr += pTable->uiFieldExtent;
   }
   else
   {
      LETOFIELD * pField = pTable->pFields;
      HB_UCHAR    uLenLen;
      HB_USHORT   uiLen, uiCount;
      char *      ptrRec;

      /* blank whole record with most common ' ' to spare these tiny memset's in loop */
      memset( pTable->pRecord, ' ', pTable->uiRecordLen );
      if( pTable->fDeleted )
         pTable->pRecord[ 0 ] = '*';

      for( uiCount = 0; uiCount < pTable->uiFieldExtent; uiCount++, pField++ )
      {
         ptrRec = ( char * ) pTable->pRecord + pTable->pFieldOffset[ uiCount ];
         uLenLen = ( ( HB_UCHAR ) *ptr ) & 0xFF;

         if( ! uLenLen && ! pTable->pFieldIsBinary[ uiCount ] )
         {
            if( pField->uiType == HB_FT_LOGICAL )
               *ptrRec = 'F';

            ptr++;
            if( pField->uiType == HB_FT_STRING && pField->uiLen > 255 )
               ptr++;
         }
         else  /* not empty field or binary type */
         {
            switch( pField->uiType )
            {
               case HB_FT_STRING:
                  ptr++;
                  if( pField->uiLen < 256 )
                  {
                     memcpy( ptrRec, ptr, uLenLen );
                     ptr += uLenLen;
                  }
                  else
                  {
                     uiLen = ( HB_USHORT ) leto_b2n( ptr, uLenLen );
                     ptr += uLenLen;
                     memcpy( ptrRec, ptr, uiLen );
                     ptr += uiLen;
                  }
                  break;

               case HB_FT_LONG:
               case HB_FT_FLOAT:
                  ptr++;
                  ptrRec += ( pField->uiLen - uLenLen );
                  memcpy( ptrRec, ptr, uLenLen );
                  ptr += uLenLen;
                  break;

               case HB_FT_DATE:
                  memcpy( ptrRec, ptr, pField->uiLen );
                  ptr += pField->uiLen;
                  break;

               case HB_FT_LOGICAL:
                  *ptrRec = *ptr++;
                  break;

               case HB_FT_MEMO:
               case HB_FT_BLOB:
               case HB_FT_PICTURE:
               case HB_FT_OLE:
                  /* empty memo field allready sorted out */
                  if( pField->uiLen == 4 )
                  {
                     memcpy( ptrRec, ptr, pField->uiLen );
                     ptr += pField->uiLen;
                  }
                  else  /* changed to be conform with ZIP traffic: using rightmost char */
                  {
                     ptrRec[ pField->uiLen - 1 ] = '1';
                     ptr++;
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
                  memcpy( ptrRec, ptr, pField->uiLen );
                  ptr += pField->uiLen;
                  break;

               case HB_FT_ANY:
                  if( pField->uiLen == 3 || pField->uiLen == 4 )
                  {
                     memcpy( ptrRec, ptr, pField->uiLen );
                     ptr += pField->uiLen;
                  }
                  else
                  {
                     *ptrRec++ = *ptr;
                     switch( *ptr++ )
                     {
                        case 'D':
                           memcpy( ptrRec, ptr, 8 );  /* reverse for hb_itemGetDS() */
                           ptr += 8;
                           break;

                        case 'L':
                           *ptrRec = *ptr++;
                           break;

                        case 'N':
                           uiLen = ( ( HB_UCHAR ) *ptr ) & 0xFF;
                           memcpy( ptrRec + ( pField->uiLen - uiLen ), ptr, uiLen );
                           ptr += uiLen + 1;
                           break;

                        case 'C':
                           uiLen = ( HB_USHORT ) leto_b2n( ptr, 2 );
                           memcpy( ptrRec, ptr, uiLen + 2 );
                           ptr += uiLen + 2;
                           break;
                     }
                  }
                  break;
            }
         }
      }
   }

   pTable->ulRecCount = HB_GET_LE_UINT32( ( const HB_BYTE * ) ptr + 1 );  /* after a ';' */

   if( pTable->pTagCurrent )
   {
      ptr += 5;  /* see above: behind ulRecCount */
      if( *ptr == '%' )
      {
         pTable->pTagCurrent->ulKeyNo = HB_GET_LE_UINT32( ( const HB_BYTE * ) ptr + 1 );
         ptr += 5;
      }
      else
         pTable->pTagCurrent->ulKeyNo = 0;

      if( *ptr == '$' )
         pTable->pTagCurrent->ulKeyCount = HB_GET_LE_UINT32( ( const HB_BYTE * ) ptr + 1 );
      else
         pTable->pTagCurrent->ulKeyCount = 0;
   }

   /* check if 'old' server data must be refreshed with buffered transaction data */
   if( pConnection->fTransActive && leto_SearchTransList( pConnection, pTable->hTable, pTable->ulRecNo ) )
   {
      HB_ULONG  ulLen, ulIndex;
      HB_UCHAR  uLenLen;
      HB_USHORT uiSpace, uiLen;
      char *    ptrPar;

      ptr = ( char * ) ( pConnection->szTransBuffer + pConnection->uiTBufOffset );
      for( ulIndex = 0; ulIndex < pConnection->ulRecsInTrans; ulIndex++ )
      {
         if( ( uLenLen = ( ( ( HB_UCHAR ) *ptr ) & 0xFF ) ) < 10 )
         {
            ulLen = leto_b2n( ++ptr, uLenLen );
            ptr += uLenLen;

            /* first: check for update action in transaction buffer */
            if( ptr[ 0 ] == LETOCMD_upd )
            {
               /* second: verify the same WA */
               if( strtoul( ptr + 2, &ptrPar, 10 ) == pTable->hTable )
               {
                  /* third: check for RecNo behind WA + ';' */
                  if( strtoul( ++ptrPar, &ptrPar, 10 ) == pTable->ulRecNo )
                  {
                     LETOFIELD *  pField;
                     HB_UCHAR     n255 = ( pTable->uiFieldExtent > 255 ? 2 : 1 );
                     HB_USHORT    uiField;
                     long         lUpd, l;
                     char *       ptrRec;

                     lUpd = strtol( ++ptrPar, &ptrPar, 10 );
#ifdef LETO_CLIENTLOG
                     leto_clientlog( NULL, 0, "leto_ParseRecord transaction recno %lu WA %lu fields %ld", pTable->ulRecNo, pTable->hTable, lUpd );
#endif
                     ptrPar++;
                     if( *ptrPar == '1' )
                        pTable->fDeleted = HB_TRUE;
                     else if( *ptrPar == '2' )
                        pTable->fDeleted = HB_FALSE;
                     ptrPar += 2;  /* followed by a ';' */

                     for( l = 0; l < lUpd; l++ )
                     {
                        uiField = ( HB_USHORT ) leto_b2n( ptrPar, n255 );
                        if( ! uiField || uiField > pTable->uiFieldExtent )
                           break;  /* malicious data, ToDo: we shell leave a note */
                        pField = pTable->pFields + --uiField;
                        ptrPar += n255;

                        ptrRec = ( char * ) pTable->pRecord + pTable->pFieldOffset[ uiField ];

                        switch( pField->uiType )
                        {
                           case HB_FT_STRING:
                              if( pField->uiLen < 256 )
                              {
                                 uiLen = ( ( HB_UCHAR ) *ptrPar ) & 0xFF;
                                 ptrPar++;
                              }
                              else
                              {
                                 uLenLen = ( ( HB_UCHAR ) *ptr ) & 0xFF;
                                 uiLen = ( HB_USHORT ) leto_b2n( ptrPar, ( HB_UCHAR ) uLenLen );
                                 ptrPar += uLenLen + 1;
                              }
                              memcpy( ptrRec, ptrPar, uiLen );
                              ptrPar += uiLen;
                              if( uiLen < pField->uiLen )
                                 memset( ptrRec + uiLen, ' ', pField->uiLen - uiLen );
                              break;

                           case HB_FT_LOGICAL:
                              *ptrRec = *ptrPar++;
                              break;

                           case HB_FT_LONG:
                           case HB_FT_FLOAT:
                              uiLen = ( ( HB_UCHAR ) *ptrPar ) & 0xFF;
                              ptrPar++;

                              if( ( uiSpace = ( pField->uiLen - uiLen ) ) > 0 )
                              {
                                 memset( ptrRec, ' ', uiSpace );
                                 ptrRec += uiSpace;
                              }
                              memcpy( ptrRec, ptrPar, uiLen );
                              ptrPar += uiLen;
                              break;

                           case HB_FT_INTEGER:
                           case HB_FT_CURRENCY:
                           case HB_FT_DOUBLE:
                           case HB_FT_CURDOUBLE:
                           case HB_FT_TIME:
                           case HB_FT_MODTIME:
                           case HB_FT_TIMESTAMP:
                           case HB_FT_DATE:
                           case HB_FT_AUTOINC:
                           case HB_FT_ROWVER:
                              memcpy( ptrRec, ptrPar, pField->uiLen );
                              ptrPar += pField->uiLen;
                              break;

                           case HB_FT_ANY:
                              if( pField->uiLen == 3 || pField->uiLen == 4 )
                              {
                                 memcpy( ptrRec, ptrPar, pField->uiLen );
                                 ptrPar += pField->uiLen;
                              }
                              else
                              {
                                 *ptrRec++ = *ptrPar;
                                 switch( *ptrPar++ )
                                 {
                                    case 'D':
                                       memcpy( ptrRec, ptrPar, 8 );
                                       ptrPar += 8;
                                       break;

                                    case 'L':
                                       *ptrRec = *ptrPar++;
                                       break;

                                    case 'N':
                                       uiLen = ( ( HB_UCHAR ) *ptr ) & 0xFF;
                                       memset( ptrRec, ' ', pField->uiLen - uiLen );
                                       memcpy( ptrRec + ( pField->uiLen - uiLen ), ptrPar, uiLen );
                                       ptrPar += uiLen + 1;
                                       break;

                                    case 'C':
                                       uiLen = ( HB_USHORT ) leto_b2n( ptr, 2 );
                                       memcpy( ptrRec, ptrPar, uiLen + 2 );
                                       ptrPar += uiLen + 2;
                                       break;
                                 }
                              }
                              break;
                        }
                     }
                  }
               }
            }
            ptr += ulLen;
         }
         else
            break;  /* malicious data, shell leave a note */
      }
   }
}

/* counterpart to leto_MemoInfo() at server */
static const char * leto_ReadMemoInfo( LETOTABLE * pTable, const char * ptr )
{
   const char * ptr2 = LetoFindCmdItem( ptr );

   if( ptr2 != NULL )
   {
      if( ptr2 - ptr )
         memcpy( pTable->szOrderExt, ptr, ptr2 - ptr );
      pTable->szOrderExt[ ptr2 - ptr ] = '\0';

      ptr = ptr2 + 1;
      ptr2 = LetoParseItemEnd( ptr );
      if( ptr2 - ptr )
         memcpy( pTable->szMemoExt, ptr, ptr2 - ptr );
      pTable->szMemoExt[ ptr2 - ptr ] = '\0';

      ptr = ptr2 + 1;
      pTable->uiMemoType = ( HB_BYTE ) atoi( ptr );
      ptr += 2;

      pTable->uiMemoVersion = ( HB_USHORT ) atoi( ptr );
      ptr += 2;

      ptr2 = LetoParseItemEnd( ptr );
      pTable->uiMemoBlocksize = ( HB_USHORT ) atoi( ptr );

      ptr = ptr2 + 1;
      pTable->uiLockScheme = ( HB_USHORT ) atoi( ptr );
      ptr += 2;
   }

   return ptr;
}

static const char * leto_SkipTagInfo( const char * ptr )
{
   int iOrders = 9 * atoi( ptr ), i;

   while( *ptr && *ptr++ != ';' )
      ;
   for( i = 0; i < iOrders; i++ )
   {
      while( *ptr && *ptr++ != ';' )
         ;
   }

   return ptr;
}

static char * leto_NetName( void )
{
#if defined( HB_OS_UNIX ) || ( defined( HB_OS_OS2 ) && defined( __GNUC__ ) )

   #define MAXGETHOSTNAME  256

   char szValue[ MAXGETHOSTNAME + 1 ], * szRet;
   unsigned int uiLen;

   szValue[ 0 ] = '\0';
   gethostname( szValue, MAXGETHOSTNAME );

#elif defined( HB_OS_WIN_32 ) || defined( HB_OS_WIN )

   DWORD uiLen = MAX_COMPUTERNAME_LENGTH + 1;
   char  szValue[ MAX_COMPUTERNAME_LENGTH + 1 ], * szRet;

   szValue[ 0 ] = '\0';
   GetComputerName( szValue, &uiLen );

#else

   return NULL;

#endif

   uiLen = strlen( szValue );
   szRet = ( char * ) hb_xgrab( uiLen + 1 );
   memcpy( szRet, szValue, uiLen );
   szRet[ uiLen ] = '\0';
   return szRet;
}

static HB_THREAD_STARTFUNC( leto_elch )
{
   LETOCONNECTION * pConnection = ( LETOCONNECTION * ) Cargo;
   HB_SOCKET        hSocket = pConnection->hSocketErr;
   int              iChange;
   char             szBuffer[ 256 ];
   fd_set           readfds;
   int              nfds = 0;

   /* without HVM ! */

   szBuffer[ 0 ] = '\0';

   while( pConnection->hSockPipe[ 0 ] != FS_ERROR && pConnection->hSocketErr != HB_NO_SOCKET )
   {
      FD_ZERO( &readfds );
      FD_SET( hSocket, &readfds );
      if( pConnection->hSockPipe[ 0 ] != FS_ERROR )
      {
         if( ( int ) pConnection->hSockPipe[ 0 ] > ( int ) pConnection->hSocketErr )
            nfds = ( int ) pConnection->hSockPipe[ 0 ];
         else
            nfds = ( int ) pConnection->hSocketErr;
         FD_SET( ( unsigned int ) pConnection->hSockPipe[ 0 ], &readfds );
      }
      hb_vmUnlock();
      iChange = select( nfds + 1, &readfds, NULL, NULL, NULL );
      hb_vmLock();
      if( iChange <= 0 )
      {
         if( iChange == 0 )
            continue;
         else
            break;
      }
      else /* if( iChange > 0 ) */
      {
         char *   ptr = szBuffer;
         HB_ULONG ulRead = 0;
         HB_ULONG ulMsgLen;
         long     lRet;

         /* each action at pipe means shell end this thread */
         if( pConnection->hSockPipe[ 0 ] == FS_ERROR )
            break;
         else if( FD_ISSET( ( int ) pConnection->hSockPipe[ 0 ], &readfds ) )
         {
            char c[ 1 ];

            hb_fsPipeRead( pConnection->hSockPipe[ 0 ], &c, 1, 0 );
            break;
         }

         do
         {
            lRet = hb_socketRecv( pConnection->hSocketErr, ptr + ulRead, LETO_MSGSIZE_LEN - ulRead, 0, -1 );

            if( lRet > 0 )
               ulRead += lRet;
#ifndef __SOCKET_EAGAIN__
            else if( ! LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
               break;
#else
            else if( ! LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) &&
                     ! LETO_SOCK_IS_EAGAIN( LETO_SOCK_GETERROR() ) )
               break;
#endif
         }
         while( ulRead < LETO_MSGSIZE_LEN );

         if( ulRead == 0 )
            break;
         else if( ulRead != LETO_MSGSIZE_LEN )
            continue;  // ToDo = also break ?

         ulMsgLen = HB_GET_LE_UINT32( ptr );
         if( ! ulMsgLen )
            continue;

         ulRead = 0;
         do
         {
            hb_vmUnlock();
            lRet = hb_socketRecv( pConnection->hSocketErr, ptr + ulRead, ulMsgLen - ulRead, 0, -1 );
            hb_vmLock();

            if( lRet > 0 )
               ulRead += lRet;
#ifndef __SOCKET_EAGAIN__
            else if( lRet < 0 && ! LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) )
               break;
#else
            else if( ! LETO_SOCK_IS_EINTR( LETO_SOCK_GETERROR() ) &&
                     ! LETO_SOCK_IS_EAGAIN( LETO_SOCK_GETERROR() ) )
               break;
#endif
            else if( lRet == 0 )
            {
               iChange = leto_socketSelectRead( hSocket, 1000 ); // come on !, one ! tiny pause ...
               if( iChange <= 0 )
                  break;
            }
         }
         while( ulRead < ulMsgLen );

         *( ptr + ulRead ) = '\0';
         if( ulRead != ulMsgLen || ulRead < 2 )
            continue;
         else if( *ptr == '+' && *( ptr + 1 ) == LETOCMD_ping )
         {
            szBuffer[ 4 ] = '+';
            szBuffer[ 5 ] = '@';
            szBuffer[ 6 ] = ';';
            szBuffer[ 7 ] = '\0';
            HB_PUT_LE_UINT32( szBuffer, 3 );
            if( hb_socketSend( hSocket, szBuffer, 7, 0, -1 ) == 7 )
               continue;
            else
               break;
         }
         else if( *ptr == '-' )  /* should be an error */
         {
            int    iError = 0;
            char * pTmp, * pTmp2;
            char * pp[ 9 ];
            int    i, iCount = 0;

            for( i = 0; i <= 8; ++i )
               pp[ i ] = NULL;
            pTmp = strchr( ptr, ';' );

            while( pTmp )
            {
               pTmp++;
               if( *pTmp != ';' && *pTmp != '\0' )
               {
                  pTmp2 = strchr( pTmp, ';' );
                  if( pTmp2 )
                  {
                     *pTmp2 = '\0';
                     pp[ iCount++ ] = pTmp;
                     pTmp = pTmp2;
                  }
                  else
                     break;
               }
               else if( *pTmp == '\0' )
                  break;
               else
                  iCount++;
            }

            HB_GC_LOCKE();

            if( iCount >= 4 )
               s_pError = hb_errNew();
            else
               continue;

            for( i = 0; i <= 8; ++i )
            {
               switch( i )
               {
                  case 0:
                     if( pp[ i ] )
                        hb_errPutSeverity( s_pError, ( HB_USHORT ) atoi( pp[ i ] ) );
                     break;

                  case 1:
                     if( pp[ i ] )
                        hb_errPutSubSystem( s_pError, ( const char * ) pp[ i ] );
                     break;

                  case 2:
                     if( pp[ i ] )
                        hb_errPutGenCode( s_pError, atoi( pp[ i ] ) );
                     break;

                  case 3:
                     if( pp[ i ] )
                     {
                        iError = atoi( pp[ i ] );
                        hb_errPutSubCode( s_pError, iError );
                     }
                     break;

                  case 4:
                     if( pp[ i ] )
                        hb_errPutDescription( s_pError, ( const char * ) pp[ i ] );
                     break;

                  case 5:
                     if( pp[ i ] )
                        hb_errPutOperation( s_pError, ( const char * ) pp[ i ] );
                     break;

                  case 6:
                     if( pp[ i ] )
                        hb_errPutOsCode( s_pError, atoi( pp[ i ] ) );
                     break;

                  case 7:
                     if( pp[ i ] )
                        hb_errPutFlags( s_pError, ( HB_USHORT ) atoi( pp[ i ] ) );
                     break;

                  case 8:
                     if( pp[ i ] )
                        hb_errPutFileName( s_pError, ( const char * ) pp[ i ] );
                     break;
               }
            }

            if( ! iError )
            {
               hb_errRelease( s_pError );
               s_pError = NULL;
#ifdef LETO_CLIENTLOG
               leto_clientlog( NULL, 0, "%s -- %d\n", "non valid error send (%d) ...", iError );
#endif
            }

            HB_GC_UNLOCKE();
         }
      }
   }

#ifdef LETO_CLIENTLOG
   leto_clientlog( NULL, 0, "%s", "DEBUG thread leto_elch() ended" );
#endif

   HB_THREAD_END
}

HB_EXPORT LETOCONNECTION * LetoConnectionNew( const char * szAddr, int iPort, const char * szUser, const char * szPass, int iTimeOut, HB_BOOL fZombieCheck )
{
#ifdef LETO_MT
   LETOPOOL *       pLetoPool = ( LETOPOOL * ) hb_stackGetTSD( &s_TSData );
#endif
   LETOCONNECTION * pConnection;
   HB_SOCKET        hSocket;
   char             szData[ 300 ];
   unsigned int     ui;
   HB_ULONG         ulLen;

#ifdef LETO_MT

   for( ui = 0; ui < pLetoPool->uiConnCount; ui++ )
   {
      if( ! pLetoPool->letoConnPool[ ui ].pAddr )
         break;
   }
   if( ui == pLetoPool->uiConnCount )
   {
      pLetoPool->letoConnPool = hb_xrealloc( pLetoPool->letoConnPool, sizeof( LETOCONNECTION ) * ( pLetoPool->uiConnCount + 1 ) );
      memset( pLetoPool->letoConnPool + ui, 0, sizeof( LETOCONNECTION ) );
      pLetoPool->uiConnCount++;
   }
   pConnection = pLetoPool->letoConnPool + ui;
   memset( pConnection, 0, sizeof( LETOCONNECTION ) );
   pLetoPool->pCurrentConn = pConnection;

#else

   for( ui = 0; ui < s_uiConnCount; ui++ )
   {
      if( ! s_letoConnPool[ ui ].pAddr )
         break;
   }

   if( ui == s_uiConnCount )
   {
      s_letoConnPool = hb_xrealloc( s_letoConnPool, sizeof( LETOCONNECTION ) * ( s_uiConnCount + 1 ) );
      s_uiConnCount++;
   }
   pConnection = s_letoConnPool + ui;
   memset( pConnection, 0, sizeof( LETOCONNECTION ) );
   s_pCurrentConn = pConnection;

#endif

   if( iTimeOut == 0 )
      iTimeOut = LETO_DEFAULT_TIMEOUT;  /* two minutes */

   hSocket = leto_ipConnect( szAddr, iPort, iTimeOut, pConnection );
   if( hSocket != HB_NO_SOCKET && ! pConnection->iErrorCode )
   {
      unsigned int uiAddrLen = strlen( szAddr );

      pConnection->iConnection = ui;
      pConnection->iConnectSrv = -1;
      pConnection->hSocket = hSocket;
      pConnection->hSocketErr = HB_NO_SOCKET;
      pConnection->iPort = iPort;
      pConnection->iTimeOut = iTimeOut;
      pConnection->pAddr = ( char * ) hb_xgrab( uiAddrLen + 1 );
      memcpy( pConnection->pAddr, szAddr, uiAddrLen );
      pConnection->pAddr[ uiAddrLen ] = '\0';
      pConnection->iZipRecord = -1;
      pConnection->fRefreshCount = HB_TRUE;
      pConnection->iBufRefreshTime = 100;
      memset( pConnection->cDopcode, 0, LETO_DOPCODE_LEN );
      pConnection->hSockPipe[ 0 ] = FS_ERROR;
      pConnection->hSockPipe[ 1 ] = FS_ERROR;
      pConnection->ulBufferLen = LETO_SENDRECV_BUFFSIZE;
      pConnection->szBuffer = ( char * ) hb_xgrabz( pConnection->ulBufferLen + 1 );

      if( leto_RecvFirst( pConnection ) )
      {
         char *       ptr, * pName;
         unsigned int uiSizeLen = ( pConnection->uiProto == 1 ) ? 0 : LETO_MSGSIZE_LEN;
         char *       szModName;
         int          iMod;

         ptr = strchr( pConnection->szBuffer, ';' );
         if( ptr )
         {
            memcpy( pConnection->szVersion, pConnection->szBuffer, ptr - pConnection->szBuffer );
            pConnection->szVersion[ ptr - pConnection->szBuffer ] = '\0';
            ptr += 2;

            ulLen = strlen( ptr );
            if( ulLen )
            {
               char * szPassTmp = ( char * ) hb_xgrab( ulLen );

               leto_hexchar2byte( ptr, ulLen, szPassTmp );
               leto_decrypt( szPassTmp, ulLen / 2, szPassTmp, &ulLen, LETO_PASSWORD, HB_TRUE );
               if( ulLen == LETO_DOPCODE_LEN )  /* we urgent need that for encryption of user password */
                  memcpy( pConnection->cDopcode, szPassTmp, LETO_DOPCODE_LEN );
               hb_xfree( szPassTmp );
            }
         }
         else  /* should never happen */
         {
            memcpy( pConnection->szVersion, pConnection->szBuffer, strlen( pConnection->szBuffer ) );
            pConnection->szVersion[ strlen( pConnection->szBuffer ) ] = '\0';
         }

         ptr = pConnection->szVersion;
         while( ptr && ! HB_ISDIGIT( *ptr ) )
            ptr++;
         if( ptr != NULL )
            sscanf( ptr, "%u.%u", &pConnection->uiMajorVer, &pConnection->uiMinorVer );
         else
            pConnection->uiMajorVer = pConnection->uiMinorVer = 0;

#if defined( __XHARBOUR__ ) || defined( __HARBOUR30__ )
         szModName = s_szModName;
#else
         szModName = hb_cmdargProgName();
#endif
         if( szModName )
         {
            iMod = strlen( szModName ) - 1;

            while( iMod >= 0 && szModName[ iMod ] != '\\' && szModName[ iMod ] != '/' )
               iMod--;
            if( iMod < 0 )
               iMod = 0;
            if( szModName[ iMod ] == '\\' || szModName[ iMod ] == '/' )
               iMod++;
         }
         else
            iMod = 0;
         pName = leto_NetName();
         ptr = szData + uiSizeLen;
         ptr += eprintf( ptr, "%c;%s;%s;%s;", LETOCMD_intro,
                         LETO_VERSION_STRING, ( ( pName ) ? pName : "" ),
                         ( ( szModName ) ? szModName + iMod : "LetoClient" ) );
#if ! ( defined( __XHARBOUR__ ) || defined( __HARBOUR30__ ) )
         if( szModName )
            hb_xfree( szModName );
#endif
         if( pName )
            hb_xfree( pName );

         if( szUser )
         {
            if( ( ulLen = strlen( szUser ) ) > LETO_MAX_USERNAME )
               ulLen = LETO_MAX_USERNAME;
            memcpy( ptr, szUser, ulLen );
            ptr += ulLen;
         }
         *ptr++ = ';';

         if( szPass && ( ulLen = strlen( szPass ) ) > 0 )
         {
            char * szKey = leto_localKey( pConnection->cDopcode, LETO_DOPCODE_LEN );
            char * szBuf = ( char * ) hb_xgrab( ( strlen( szPass ) + 9 ) * 2 );

            leto_encrypt( szPass, ulLen, szBuf, &ulLen, szKey, HB_FALSE );
            leto_byte2hexchar( szBuf, ( int ) ulLen, ptr );
            if( szKey )
               hb_xfree( szKey );
            hb_xfree( szBuf );
            ptr += ulLen * 2;
         }
         *ptr++ = ';';
         eprintf( ptr, "%s;%c%c%c%d;%s;%d;%s;%s", leto_GetServerCdp( pConnection, HB_CDP_PAGE() ? HB_CDP_PAGE()->id : "" ),
                       hb_setGetSoftSeek() ? 'T' : 'F', hb_setGetDeleted() ? 'T' : 'F',
                       hb_setGetAutOpen() ? 'T' : 'F', hb_setGetAutOrder(),
                       hb_setGetDateFormat(), hb_setGetEpoch(), hb_setGetDefault(), hb_setGetPath() );

         ulLen = strlen( szData + LETO_MSGSIZE_LEN );
         HB_PUT_LE_UINT32( szData, ulLen );

#ifdef LETO_CLIENTLOG
         leto_clientlog( NULL, 0, "INTRO :%s", szData + 4 );
#endif
         if( hb_socketSend( hSocket, szData, LETO_MSGSIZE_LEN + ulLen, 0, -1 ) <= 0 )
         {
            pConnection->iConnectRes = LETO_ERR_SEND;
            LetoConnectionClose( pConnection );
            return NULL;
         }
         if( ! leto_Recv( pConnection ) )
         {
            if( szPass )
               pConnection->iConnectRes = LETO_ERR_LOGIN;
            else
               pConnection->iConnectRes = LETO_ERR_RECV;
            LetoConnectionClose( pConnection );
            return NULL;
         }

         ptr = leto_firstchar( pConnection );
         if( ! strncmp( ptr, "ACC", 3 ) )
            pConnection->iConnectRes = LETO_ERR_ACCESS;
         else if( ! strncmp( ptr, "LCK", 3 ) )
            pConnection->iConnectRes = LETO_ERR_LOCKED;
         else if( ! strncmp( ptr, "ERR", 3 ) )
            pConnection->iConnectRes = LETO_ERR_MANY_CONN;
         else
         {
            if( ( pName = strchr( ptr, ';' ) ) != NULL && ( pName - ptr ) >= 3 )
            {
               memcpy( pConnection->szAccess, ptr, ( pName - ptr > 3 ) ? 3 : pName - ptr );
               ptr = pName + 1;
               if( ( pName = strchr( ptr, ';' ) ) != NULL )
               {
                  pConnection->szVerHarbour = ( char * ) hb_xgrab( pName - ptr + 1 );
                  memcpy( pConnection->szVerHarbour, ptr, pName - ptr );
                  pConnection->szVerHarbour[ pName - ptr ] = '\0';

                  if( *( ptr = pName + 1 ) != '\0' )
                  {
                     unsigned int uiMemoType, uiMemoBlocksize, uiLockScheme;
                     unsigned int uiServerPort, uiSrvMode, uiLowerCase, uiAutOrder;
                     char *       ptr2 = pName + 1;

                     while( *ptr2 != ';' )
                        ptr2++;
                     *ptr2++ = '\0';

                     memcpy( pConnection->szDriver, pName + 1, ptr2 - ( pName + 2 ) );
                     pConnection->szDriver[ ptr2 - ( pName + 2 ) ] = '\0';
                     pConnection->uiDriver = ( strstr( pConnection->szDriver, "NTX" ) != NULL ) ? 1 : 0;

                     sscanf( ptr2, "%u;%u;%u;%u;%d;%u;%u",
                             &uiMemoType, &uiMemoBlocksize, &uiLockScheme,
                             &uiServerPort, &uiAutOrder, &uiSrvMode, &uiLowerCase );

                     pConnection->uiLockSchemeExtend = ( HB_USHORT ) uiLockScheme;  /* extended or normal */
                     pConnection->uiMemoType = ( HB_USHORT ) uiMemoType;
                     pConnection->uiMemoBlocksize = ( HB_USHORT ) uiMemoBlocksize;
                     pConnection->iServerPort = uiServerPort;  /* port at server side, after accept() */
                     pConnection->uiServerMode = ( HB_USHORT ) uiSrvMode;
                     pConnection->fLowerCase = uiLowerCase ? HB_TRUE : HB_FALSE;

                     /* now the second socket for errors if MT is supported */
                     if( fZombieCheck && hb_vmIsMt() )
                     {
                        pConnection->hSocketErr = leto_ipConnect( szAddr, iPort + 1, 2000, NULL );
#ifdef LETO_CLIENTLOG
                        if( pConnection->hSocketErr == HB_NO_SOCKET )
                           leto_clientlog( NULL, 0, "%s", "ERROR: creating second socket failed" );
#endif
                     }
                     else
                        pConnection->hSocketErr = HB_NO_SOCKET;

                     if( pConnection->hSocketErr != HB_NO_SOCKET )
                     {
                        /* intro for error socket is different */
                        ulLen = eprintf( szData + LETO_MSGSIZE_LEN, "%c;%d;%s:%d",
                                         LETOCMD_intro,
                                         pConnection->iServerPort,
                                         pConnection->pAddr,
                                         pConnection->iPort );
                        HB_PUT_LE_UINT32( szData, ulLen );
                        if( hb_socketSend( pConnection->hSocketErr, szData, ulLen + LETO_MSGSIZE_LEN, 0, -1 ) > 0 )
                        {
                           if( ! leto_RecvSecond( pConnection ) )
                           {
                              hb_socketShutdown( pConnection->hSocketErr, HB_SOCKET_SHUT_RDWR  );
                              hb_socketClose( pConnection->hSocketErr );
                              pConnection->hSocketErr = HB_NO_SOCKET;
#ifdef LETO_CLIENTLOG
                              leto_clientlog( NULL, 0, "%s", "leto_elch() error after errsock intro send" );
#endif
                           }
#if ! defined( HB_OS_WIN )
                           else if( hb_fsPipeCreate( pConnection->hSockPipe ) )
#else
                           else
#endif
                           {
                              pConnection->hThread = hb_threadCreate( &pConnection->hThreadID, leto_elch, ( void * ) pConnection );
                              if( pConnection->hThread )
                                 hb_threadDetach( pConnection->hThread );
                           }
                        }
                        else
                        {
                           hb_socketShutdown( pConnection->hSocketErr, HB_SOCKET_SHUT_RDWR  );
                           hb_socketClose( pConnection->hSocketErr );
                           pConnection->hSocketErr = HB_NO_SOCKET;
#ifdef LETO_CLIENTLOG
                           leto_clientlog( NULL, 0, "%s", "ERROR with send to error-socket" );
#endif
                        }
                     }
                  }
                  else
                     pConnection->iConnectRes = LETO_ERR_PROTO;
               }
               else
                  pConnection->iConnectRes = LETO_ERR_PROTO;
            }
            else
               pConnection->iConnectRes = LETO_ERR_PROTO;
         }
      }
      else
         pConnection->iConnectRes = LETO_ERR_RECV;
   }
   else
      pConnection->iConnectRes = LETO_ERR_SOCKET;

   if( pConnection->iConnectRes )
   {
#ifdef LETO_CLIENTLOG
      leto_clientlog( NULL, 0, "LETODB CONNECT ERROR %d", pConnection->iConnectRes );
#endif
      LetoConnectionClose( pConnection );
      return NULL;
   }
   else
      return pConnection;
}

HB_EXPORT int LetoCloseAll( LETOCONNECTION * pConnection )
{
   char szData[ 4 ];

   eprintf( szData, "%c;", LETOCMD_closall );
   if( leto_SendRecv2( pConnection, szData, 3, 1021 ) )
      return 1;
   else
      return 0;
}

/* called from both version with 'LETO_MT' and without MT */
HB_EXPORT void LetoConnectionClose( LETOCONNECTION * pConnection )
{
   if( pConnection->pAddr )
   {
      hb_xfree( pConnection->pAddr );
      pConnection->pAddr = NULL;
   }
   if( pConnection->szVerHarbour )
      hb_xfree( pConnection->szVerHarbour );
   if( pConnection->hSocketErr != HB_NO_SOCKET )
   {
      if( pConnection->hSockPipe[ 1 ] != FS_ERROR )  /* wake up leto_elch() */
      {
         const char cToPipe[ 1 ] = { ' ' };

         hb_fsPipeWrite( pConnection->hSockPipe[ 1 ], cToPipe, 1, 0 );
      }
   }

   leto_cryptReset( HB_FALSE );

   if( pConnection->hSocket != HB_NO_SOCKET )
   {
      hb_socketShutdown( pConnection->hSocket, HB_SOCKET_SHUT_RDWR );
      hb_socketClose( pConnection->hSocket );
      pConnection->hSocket = HB_NO_SOCKET;
   }

   if( pConnection->zstream )
   {
#ifdef USE_LZ4
      hb_lz4netClose( pConnection->zstream );
#else
      hb_znetClose( pConnection->zstream );
#endif
      pConnection->zstream = NULL;
   }

   if( pConnection->szBuffer )
   {
      hb_xfree( pConnection->szBuffer );
      pConnection->szBuffer = NULL;
   }

   if( pConnection->pCdpTable )
   {
      PCDPSTRU pNext = pConnection->pCdpTable, pCdps;

      while( pNext )
      {
         pCdps = pNext;
         hb_xfree( pCdps->szClientCdp );
         hb_xfree( pCdps->szServerCdp );
         pNext = pCdps->pNext;
         hb_xfree( pCdps );
      }
      pConnection->pCdpTable = NULL;
   }

   if( pConnection->szTransBuffer )
   {
      hb_xfree( pConnection->szTransBuffer );
      pConnection->szTransBuffer = NULL;
   }
   if( pConnection->pTransList )
   {
      hb_xfree( pConnection->pTransList );
      pConnection->pTransList = NULL;
   }
   if( pConnection->pTransAppend )
   {
      hb_xfree( pConnection->pTransAppend );
      pConnection->pTransAppend = NULL;
   }
   if( pConnection->pBufCrypt )
   {
      hb_xfree( pConnection->pBufCrypt );
      pConnection->pBufCrypt = NULL;
   }

   if( pConnection->hSocketErr != HB_NO_SOCKET )
   {
      hb_socketShutdown( pConnection->hSocketErr, HB_SOCKET_SHUT_RDWR  );
      hb_socketClose( pConnection->hSocketErr );
      pConnection->hSocketErr = HB_NO_SOCKET;
   }

   if( pConnection->hSockPipe[ 1 ] != FS_ERROR )  /* wake up leto_elch() */
   {
      hb_fsClose( pConnection->hSockPipe[ 1 ] );
      pConnection->hSockPipe[ 1 ] = FS_ERROR;
   }
   if( pConnection->hSockPipe[ 0 ] != FS_ERROR )
   {
      hb_fsClose( pConnection->hSockPipe[ 0 ] );
      pConnection->hSockPipe[ 0 ] = FS_ERROR;
   }

   letoClearCurrConn( pConnection );
}

static const char * leto_AddFields( LETOTABLE * pTable, HB_USHORT uiFields, const char * szFields )
{
   HB_USHORT    uiCount;
   unsigned int uiLen;
   const char * ptr = szFields;
   const char * ptrStart;
   LETOFIELD *  pField;

   pTable->uiFieldExtent = uiFields;

   pTable->pFieldOffset = ( unsigned int * ) hb_xgrab( uiFields * sizeof( unsigned int ) );
   pTable->pFieldUpd = ( HB_BYTE * ) hb_xgrabz( uiFields * sizeof( HB_BYTE ) );
   pTable->pFieldIsBinary = ( HB_BYTE * ) hb_xgrabz( uiFields * sizeof( HB_BYTE ) );

   /* no hb_xgrabz() as every item is guaranteed filled below */
   pTable->pFields = ( LETOFIELD * ) hb_xgrab( sizeof( LETOFIELD ) * uiFields );
   pTable->uiRecordLen = 1;

   for( uiCount = 0; uiCount < uiFields; uiCount++ )
   {
      pField = pTable->pFields + uiCount;
      ptrStart = ptr;
      while( *ptr && *ptr++ != ';' )
         ;
      uiLen = ptr - ptrStart - 1;
      if( uiLen > 10 )
         uiLen = 10;
      memcpy( pField->szName, ptrStart, uiLen );
      pField->szName[ uiLen ] = '\0';

      switch( *ptr )
      {
         case 'C':
            pField->uiType = HB_FT_STRING;
            break;
         case 'N':
            pField->uiType = HB_FT_LONG;
            break;
         case 'L':
            pField->uiType = HB_FT_LOGICAL;
            break;
         case 'D':
            pField->uiType = HB_FT_DATE;
            break;
         case 'M':
            pField->uiType = HB_FT_MEMO;
            pTable->fHaveMemo = HB_TRUE;
            break;
         case 'W':
            pField->uiType = HB_FT_BLOB;
            pTable->fHaveMemo = HB_TRUE;
            break;
         case 'P':
            pField->uiType = HB_FT_IMAGE;
            pTable->fHaveMemo = HB_TRUE;
            break;
         case 'G':
            pField->uiType = HB_FT_OLE;
            pTable->fHaveMemo = HB_TRUE;
            break;
         case 'V':
            pField->uiType = HB_FT_ANY;
            break;
         case 'I':
         case '2':
         case '4':
            pField->uiType = HB_FT_INTEGER;
            break;
         case 'F':
            pField->uiType = HB_FT_FLOAT;
            break;
         case 'Y':
            pField->uiType = HB_FT_CURRENCY;
            break;
         case '8':
         case 'B':
            pField->uiType = HB_FT_DOUBLE;
            break;
         case '=':
            pField->uiType = HB_FT_MODTIME;
            break;
         case '@':
            pField->uiType = HB_FT_TIMESTAMP;
            break;
         case 'T':
            pField->uiType = HB_FT_TIME;
            break;
         case '^':
            pField->uiType = HB_FT_ROWVER;
            pTable->fHaveAutoinc = HB_TRUE;
            break;
         case '+':
            pField->uiType = HB_FT_AUTOINC;
            pTable->fHaveAutoinc = HB_TRUE;
            break;
         case 'Q':
            pField->uiType = HB_FT_VARLENGTH;
            break;
         case 'Z':
            pField->uiType = HB_FT_CURDOUBLE;
            break;
         default:  /* ToDo -- we should throw an error */
            pField->uiType = HB_FT_NONE;
            break;
      }

      pField->uiFlags = 0;
      /* extended field attributes after a ':' */
      if( *( ++ptr ) == ':' )
      {
         ptr++;
         while( *ptr != ';' )
         {
            switch( *ptr++ )
            {
#if ! defined( __HARBOUR30__ )
               case '+':
                  pField->uiFlags |= HB_FF_AUTOINC;
                  pTable->fHaveAutoinc = HB_TRUE;
                  break;
#endif
               case 'B':
                  pField->uiFlags |= HB_FF_BINARY;
                  break;

               case 'E':
                  pField->uiFlags |= HB_FF_ENCRYPTED;
                  break;

               case 'N':
                  pField->uiFlags |= HB_FF_NULLABLE;
                  break;

               case 'U':
                  pField->uiFlags |= HB_FF_UNICODE;
#if 0  /* ToDo ? unicode support */
                  pTable->fHaveBinary = HB_TRUE;
#endif
                  break;

               case 'Z':
                  pField->uiFlags |= HB_FF_COMPRESSED;
                  break;
            }
         }
      }
      else
      {
         while( *ptr != ';' )
            ptr++;
      }
      ptr++;
      pField->uiLen = ( HB_USHORT ) atoi( ptr );
      if( leto_IsBinaryField( pField->uiType, pField->uiLen ) )
      {
         pTable->fHaveBinary = HB_TRUE;
         pTable->pFieldIsBinary[ uiCount ] = HB_TRUE;
      }

      pTable->pFieldOffset[ uiCount ] = pTable->uiRecordLen;
      pTable->uiRecordLen += pField->uiLen;
      while( *ptr++ != ';' )
         ;
      pField->uiDec = ( HB_USHORT ) atoi( ptr );
      while( *ptr++ != ';' )
         ;
   }

   return ptr;
}

HB_EXPORT LETOTABLE * LetoDbCreateTable( LETOCONNECTION * pConnection, const char * szFile, const char * szAlias, const char * szFields, unsigned int uiArea, const char * szCdpage )
{
   LETOTABLE *   pTable;
   char *        szData;
   const char *  ptr = szFields;
   char *        ptrTmp;
   HB_USHORT     uiFields = 0;
   unsigned long ulLen;

   hb_rddSetNetErr( HB_FALSE );
   /* count and low verify field definitions */
   while( *ptr )
   {
      if( *ptr++ == ';' )
         ++uiFields;
   }
   if( uiFields < 4 || uiFields % 4 )
   {
      pConnection->iError = 1;
      return NULL;
   }
   else
      uiFields /= 4;

   szData = ( char * ) hb_xgrab( strlen( szFields ) + 30 +        /* uiFields * 24 --> strlen() */
                                 HB_PATH_MAX + HB_RDD_MAX_ALIAS_LEN + HB_RDD_MAX_DRIVERNAME_LEN );

   ulLen = eprintf( szData, "%c;%s;%s;%s;%d;%s;%d;%d;%s;%d;%s;%s;", LETOCMD_creat, szFile,
                    szAlias ? szAlias : "", pConnection->szDriver,
                    pConnection->uiMemoType,  pConnection->szMemoExt, pConnection->uiMemoBlocksize,
                    uiFields, szFields, uiArea, szCdpage ? szCdpage : "", hb_setGetDateFormat() );
   if( ! leto_DataSendRecv( pConnection, szData, ulLen ) )
   {
      hb_xfree( szData );
      return NULL;
   }
   hb_xfree( szData );

   ptr = pConnection->szBuffer;
   if( *ptr == '-' )
   {
      unsigned int  ui;

      if( ptr[ 4 ] == ':' )
      {
         sscanf( ptr + 5, "%u-%u-", &ui, &pConnection->iError );
         if( pConnection->iError == 1023 )
            hb_rddSetNetErr( HB_TRUE );
      }
      else
         pConnection->iError = 1021;
      return NULL;
   }

   pTable = ( LETOTABLE * ) hb_xgrabz( sizeof( LETOTABLE ) );
   pTable->uiConnection = pConnection->iConnection;
   pTable->iBufRefreshTime = pConnection->iBufRefreshTime;
   pTable->fShared = pTable->fReadonly = pTable->fEncrypted = HB_FALSE;

   pTable->pLocksPos = NULL;
   pTable->ulLocksMax = pTable->ulLocksAlloc = 0;
   strcpy( pTable->szDriver, pConnection->szDriver );

   leto_AddFields( pTable, uiFields, szFields );

   pTable->pRecord = ( HB_UCHAR * ) hb_xgrab( pTable->uiRecordLen + 1 );

   ptr = leto_firstchar( pConnection );
   pTable->hTable = strtoul( ptr, &ptrTmp, 10 );
   ptr = ++ptrTmp;
   if( *ptr == '1' )
      pTable->uiDriver = LETO_NTX;
   ptr += 2;

   /* update MemoType, MemoFileExt, MemoVersion, LockScheme .. */
   ptr = leto_ReadMemoInfo( pTable, ptr );

   /* for DBI_LASTUPDATE */
   pTable->lLastUpdate = strtoul( ptr, &ptrTmp, 10 );
   ptr = ++ptrTmp;

   leto_ParseRecord( pConnection, pTable, ptr );

   return pTable;
}

HB_EXPORT HB_BOOL LetoProdSupport( void )
{
   LPRDDNODE pRDDNode;
   HB_USHORT uiRddID;
   HB_BOOL   fSupportStruct = HB_FALSE;

   pRDDNode = hb_rddFindNode( "LETO", &uiRddID );
   if( pRDDNode )
   {
      PHB_ITEM pItem = NULL;

      pItem = hb_itemPutC( pItem, NULL );
      if( SELF_RDDINFO( pRDDNode, RDDI_STRUCTORD, 0, pItem ) == HB_SUCCESS )
         fSupportStruct = hb_itemGetL( pItem );
      hb_itemRelease( pItem );
   }

   return fSupportStruct;
}

HB_EXPORT LETOTABLE * LetoDbOpenTable( LETOCONNECTION * pConnection, const char * szFile, const char * szAlias,
                                       HB_BOOL fShared, HB_BOOL fReadOnly, const char * szCdp, unsigned int uiArea )
{
   char         szData[ HB_PATH_MAX + HB_RDD_MAX_ALIAS_LEN + HB_RDD_MAX_DRIVERNAME_LEN + 25 ];
   const char * ptr;
   char *       ptrTmp;
   LETOTABLE *  pTable;
   HB_ULONG     ulLen;

   hb_rddSetNetErr( HB_FALSE );
   ulLen = eprintf( szData, "%c;%s;%s;%c%c;%s;%s;%d;%s;", LETOCMD_open, szFile, szAlias,
                    ( fShared ) ? 'T' : 'F', ( fReadOnly ) ? 'T' : 'F',
                    szCdp, pConnection->szDriver, uiArea, hb_setGetDateFormat() );
   if( ! leto_DataSendRecv( pConnection, szData, ulLen ) )
      return NULL;

   ptr = pConnection->szBuffer;
   if( *ptr == '-' )
   {
      if( *( ptr + 3 ) == '4' )
      {
         hb_rddSetNetErr( HB_TRUE );  /* we set it only for access violation ? */
         pConnection->iError = 103;
      }
      else if( ptr[ 4 ] == ':' )
      {
         unsigned int uErr;

         sscanf( ptr + 5, "%u-%u-", &uErr, &pConnection->iError );
      }
      else
         pConnection->iError = 1021;
      return NULL;
   }
   ptr++;  /* ptr = leto_firstchar(); */

   pTable = ( LETOTABLE * ) hb_xgrabz( sizeof( LETOTABLE ) );
   pTable->uiConnection = pConnection->iConnection;

   pTable->fShared = fShared;
   pTable->fReadonly = fReadOnly;
   pTable->fEncrypted = HB_FALSE;
   pTable->iBufRefreshTime = pConnection->iBufRefreshTime;

   pTable->pLocksPos = NULL;
   pTable->ulLocksMax = pTable->ulLocksAlloc = 0;
   strcpy( pTable->szDriver, pConnection->szDriver );

   pTable->hTable = strtoul( ptr, &ptrTmp, 10 );
   ptr = ++ptrTmp;
   pTable->uiDriver = *ptr - '0';  /* LETO_CDX = '0', LETO_NTX = '1' */
   ptr += 2;

   /* update MemoType, MemoFileExt, MemoVersion, LockScheme .. */
   ptr = leto_ReadMemoInfo( pTable, ptr );

   /* for DBI_LASTUPDATE */
   pTable->lLastUpdate = strtoul( ptr, &ptrTmp, 10 );  /* long Julian type */
   ptr = ++ptrTmp;

   ptr = leto_AddFields( pTable, ( HB_USHORT ) atoi( ptr ), LetoParseItemEnd( ptr ) + 1 );
   pTable->pRecord = ( HB_UCHAR * ) hb_xgrab( pTable->uiRecordLen + 1 );

   if( hb_setGetAutOpen() && LetoProdSupport() )
      ptr = leto_ParseTagInfo( pTable, ptr );
   else
      ptr = leto_SkipTagInfo( ptr );

   leto_ParseRecord( pConnection, pTable, ptr );

   return pTable;
}

HB_EXPORT HB_ERRCODE LetoDbCloseTable( LETOTABLE * pTable )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

   if( pConnection->fTransActive )
   {
      pConnection->iError = 1031;
      return 1;
   }

   if( pTable->hTable )
   {
      char szData[ 32 ];

      if( ! pConnection->fCloseAll )
      {
         HB_ULONG ulLen = eprintf( szData, "%c;%lu;", LETOCMD_close, pTable->hTable );

         if( ! leto_SendRecv2( pConnection, szData, ulLen, 1021 ) )
            return 1;
      }
      pTable->hTable = 0;
   }

   if( pTable->pFieldOffset )
   {
      hb_xfree( pTable->pFieldOffset );
      pTable->pFieldOffset = NULL;
   }
   if( pTable->pFieldIsBinary )
   {
      hb_xfree( pTable->pFieldIsBinary );
      pTable->pFieldIsBinary = NULL;
   }
   if( pTable->pFieldUpd )
   {
      hb_xfree( pTable->pFieldUpd );
      pTable->pFieldUpd = NULL;
   }

   if( pTable->pRecord )
   {
      hb_xfree( pTable->pRecord );
      pTable->pRecord = NULL;
   }

   if( pTable->pFields )
   {
      hb_xfree( pTable->pFields );
      pTable->pFields = NULL;
   }
   if( pTable->szTags )
   {
      hb_xfree( pTable->szTags );
      pTable->szTags = NULL;
   }
   if( pTable->Buffer.pBuffer )
   {
      hb_xfree( pTable->Buffer.pBuffer );
      pTable->Buffer.pBuffer = NULL;
   }
   if( pTable->pLocksPos )
   {
      hb_xfree( pTable->pLocksPos );
      pTable->pLocksPos = NULL;
      pTable->ulLocksMax = pTable->ulLocksAlloc = 0;
   }
   if( pTable->pTagInfo )
      leto_ClearTagInfos( pTable );

   hb_xfree( pTable );
   return 0;
}

HB_EXPORT HB_ERRCODE LetoRddInfo( LETOCONNECTION * pConnection, HB_USHORT uiIndex, const char * szNewSet )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   if( uiIndex )
   {
      char          szData[ 32 ];
      unsigned long ulLen;

      ulLen = eprintf( szData, "%c;%s;%d;", LETOCMD_rddinfo, pConnection->szDriver, uiIndex );
      if( szNewSet )
         ulLen += eprintf( szData + ulLen, "%s;", szNewSet );
      if( ! leto_SendRecv( pConnection, szData, ulLen, 1021 ) )
         errCode = HB_FAILURE;
   }
   else
      errCode = HB_FAILURE;

   return errCode;
}

/* unused */
HB_EXPORT unsigned int LetoDbBof( LETOTABLE * pTable )
{
   return pTable->fBof;
}

/* unused */
HB_EXPORT unsigned int LetoDbEof( LETOTABLE * pTable )
{
   return pTable->fEof;
}

/* unused */
HB_EXPORT unsigned int LetoDbGetField( LETOTABLE * pTable, HB_USHORT uiIndex, char * szRet, HB_USHORT * uiLen )
{
   HB_USHORT uiFldLen;

   if( ! uiIndex || uiIndex > pTable->uiFieldExtent )
      return 1;

   uiIndex--;
   if( ! uiLen )
      uiFldLen = ( pTable->pFields + uiIndex )->uiLen;
   else if( ! *uiLen || *uiLen > ( pTable->pFields + uiIndex )->uiLen )
      *uiLen = uiFldLen = ( pTable->pFields + uiIndex )->uiLen;
   else
      uiFldLen = *uiLen;

   memcpy( szRet, pTable->pRecord + pTable->pFieldOffset[ uiIndex ], uiFldLen );
   szRet[ uiFldLen ] = '\0';

   return 0;
}

HB_EXPORT const char * LetoDbGetMemo( LETOTABLE * pTable, unsigned int uiIndex, unsigned long * ulLenMemo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char             szData[ 32 ];
   unsigned long    ulLen;

   if( ! pTable->ulRecNo )
   {
      *ulLenMemo = 0;
      return "";
   }

   if( pConnection->fTransActive && leto_SearchTransList( pConnection, pTable->hTable, pTable->ulRecNo ) )
   {
      HB_ULONG ulTransex;
      HB_UCHAR uLenLen;
      char *   ptr, * ptrPar;

      ptr = ( char * ) ( pConnection->szTransBuffer + pConnection->uiTBufOffset );
      for( ulTransex = 0; ulTransex < pConnection->ulRecsInTrans; ulTransex++ )
      {
         if( ( uLenLen = ( ( ( HB_UCHAR ) *ptr ) & 0xFF ) ) < 10 )
         {
            ulLen = leto_b2n( ++ptr, uLenLen );
            ptr += uLenLen;

            /* first: check for memo update action in transaction buffer */
            if( ptr[ 0 ] == LETOCMD_memo )
            {
               /* second: verify the same WA */
               if( strtoul( ptr + 2, &ptrPar, 10 ) == pTable->hTable )
               {
                  /* third: check for RecNo behind WA + ';p;' */
                  if( strtoul( ptrPar + 3, &ptrPar, 10 ) == pTable->ulRecNo )
                  {
                     /* forth: check field index the same */
                     if( ( unsigned int ) atoi( ++ptrPar ) == uiIndex + 1 )
                     {
                        ptrPar = strchr( ++ptrPar, ';' );
                        ptrPar++;
                        if( ( uLenLen = ( ( ( HB_UCHAR ) *ptrPar ) & 0xFF ) ) < 10 )
                        {
                           *ulLenMemo = leto_b2n( ++ptrPar, uLenLen );
                           ptrPar += uLenLen;
#ifdef LETO_CLIENTLOG
                           leto_clientlog( NULL, 0, "LetoDbGetMemo transaction recno %lu WA %lu field %ld len %lu",
                                                    pTable->ulRecNo, pTable->hTable, uiIndex + 1, *ulLenMemo );
#endif
                           return ptrPar;  /* found, sigh ... */
                        }
                     }
                  }
               }
            }
            ptr += ulLen;
         }
         else
            break;  /* malicious data, shell leave a note */
      }
   }

   ulLen = eprintf( szData, "%c;%lu;%c;%lu;%d;",
                    LETOCMD_memo, pTable->hTable, LETOSUB_get, pTable->ulRecNo, uiIndex + 1 );
   if( leto_SendRecv( pConnection, szData, ulLen, 0 ) )
      return leto_DecryptText( pConnection, ulLenMemo, pConnection->szBuffer );
   else
      return NULL;
}

/* unused */
HB_EXPORT unsigned int LetoDbPutField( LETOTABLE * pTable, HB_USHORT uiIndex, char * szValue, HB_USHORT uiLen )
{
   LETOFIELD * pField;
   HB_UCHAR *  ptr;

   if( pTable->fReadonly )
      return 10;

   if( ! uiIndex || uiIndex > pTable->uiFieldExtent )
      return 1;

   uiIndex--;
   pField = pTable->pFields + uiIndex;
   if( ! uiLen || uiLen > pField->uiLen || uiLen > ( HB_USHORT ) strlen( szValue ) )
      return 2;

   ptr = pTable->pRecord + pTable->pFieldOffset[ uiIndex ];

   if( pField->uiType == HB_FT_DATE )
   {
      char * ptrv = szValue;

      /* removed leto_dateEncStr( szValue ) -- no verification for valid date */
      if( uiLen != pField->uiLen || strtoul( szValue, &ptrv, 10 ) < 101 || ptrv - szValue != pField->uiLen )
         return 2;
      memcpy( ptr, szValue, uiLen );
   }
   else if( pField->uiType == HB_FT_LOGICAL )
   {
      if( *szValue != 'T' && *szValue != 'F' )
         return 2;
      memcpy( ptr, szValue, uiLen );
   }
   else if( pField->uiType == HB_FT_LONG )
   {
      unsigned int uiDot = 0;
      char *       ptrv = szValue;

      while( ( ( HB_USHORT ) ( ptrv - szValue ) ) < uiLen && *ptrv == ' ' )
         ptrv++;
      if( *ptrv == '-' || *ptrv == '+' )
         ptrv++;
      if( ( ( HB_USHORT ) ( ptrv - szValue ) ) < uiLen )
      {
         while( ( ( HB_USHORT ) ( ptrv - szValue ) ) < uiLen )
         {
            if( *ptrv == '.' )
            {
               if( uiDot || ! pField->uiDec || szValue + uiLen - pField->uiDec - 1 != ptrv )
                  return 2;
               uiDot = 1;
            }
            else if( *ptrv < '0' || *ptrv > '9' )
               return 2;
            ptrv++;
         }
         if( pField->uiDec && ! uiDot )
            return 2;
      }
      if( uiLen < pField->uiLen )
      {
         memset( ptr, ' ', pField->uiLen - uiLen );
         ptr += ( pField->uiLen - uiLen );
      }
      memcpy( ptr, szValue, uiLen );
   }
   else if( pField->uiType == HB_FT_STRING )
   {
      memcpy( ptr, szValue, uiLen );
      if( uiLen < pField->uiLen )
      {
         ptr += uiLen;
         memset( ptr, ' ', pField->uiLen - uiLen );
      }
   }

   pTable->uiUpdated |= LETO_FLAG_UPD_CHANGE;
   *( pTable->pFieldUpd + uiIndex ) = 1;

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbPutMemo( LETOTABLE * pTable, unsigned int uiIndex, const char * szMemoText, unsigned long ulLenMemo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char *           szData = ( char * ) hb_xgrab( ulLenMemo + 42 ), * ptr;
   HB_ULONG         ulLen = ulLenMemo;
   LETOFIELD *      pField = pTable->pFields + uiIndex;
   HB_BOOL          fAppend = ( pTable->uiUpdated & LETO_FLAG_UPD_APPEND );

   if( pTable->fReadonly )
      return HB_FAILURE;
   if( pField->uiLen == 4 )
      HB_PUT_LE_UINT32( &pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] ], ( ulLenMemo ) ? 1 : 0 );
   else
      pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] + pField->uiLen - 1 ] = ( ulLenMemo ) ? '1' : ' ';

   if( pConnection->fTransActive && fAppend )
   {
      pTable->uiUpdated &= ~LETO_FLAG_UPD_APPEND;  /* fixed, was : &= ( ! 1 ) ==> 0 */
      ptr = szData + 4 + eprintf( szData + 4, "%c;%lu;%c;%lu;%d;", LETOCMD_memo, pTable->hTable, LETOSUB_add,
                                  pTable->ulRecNo, uiIndex + 1 );
   }
   else
      ptr = szData + 4 + eprintf( szData + 4, "%c;%lu;%c;%lu;%d;", LETOCMD_memo, pTable->hTable, LETOSUB_put,
                                  pTable->ulRecNo, uiIndex + 1 );

   ptr = leto_AddLen( ptr, &ulLen, HB_FALSE );  /* after eprintf, before szMemoText */
   memcpy( ptr, szMemoText, ulLenMemo );
   ptr += ulLenMemo;
   *ptr = '\0';
   ulLen = ptr - ( szData + 4 );

   if( pConnection->fTransActive )
   {
      ptr = leto_AddLen( ( szData + 4 ), &ulLen, HB_TRUE );  /* before */
      leto_AddTransBuffer( pConnection, ptr, ulLen );
      if( ! fAppend && pTable->ulRecNo )
         leto_AddTransList( pConnection, pTable );
      else
         leto_AddTransAppend( pConnection, pTable );
   }
   else
   {
      if( pTable->ptrBuf )
         leto_refrSkipBuf( pTable );

      /* send without pre-leading length */
      if( ! leto_SendRecv2( pConnection, szData + 4, ulLen, 1021 ) )
      {
         hb_xfree( szData );
         return 1;
      }
   }
   hb_xfree( szData );
   return 0;
}

/* unused */
HB_EXPORT unsigned int LetoDbRecNo( LETOTABLE * pTable, unsigned long * ulRecNo )
{
   *ulRecNo = pTable->ulRecNo;
   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbRecCount( LETOTABLE * pTable, unsigned long * ulCount )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

   if( pConnection->fRefreshCount || ! pTable->ulRecCount )
   {
      char          szData[ 32 ];
      unsigned long ulLen;

      ulLen = eprintf( szData, "%c;%lu;", LETOCMD_rcou, pTable->hTable );
      if( ! leto_SendRecv( pConnection, szData, ulLen, 1021 ) )
         return 1;

      pTable->ulRecCount = *ulCount = ( unsigned long ) atol( leto_firstchar( pConnection ) );
   }
   else
      *ulCount = pTable->ulRecCount;

   return 0;
}

/* unused */
HB_EXPORT unsigned int LetoDbFieldCount( LETOTABLE * pTable, unsigned int * uiCount )
{
   *uiCount = ( unsigned int ) pTable->uiFieldExtent;
   return 0;
}

/* unused */
HB_EXPORT unsigned int LetoDbFieldName( LETOTABLE * pTable, HB_USHORT uiIndex, char * szName )
{
   if( ! szName || ! uiIndex || uiIndex > pTable->uiFieldExtent )
      return 1;

   strcpy( szName, ( pTable->pFields + uiIndex - 1 )->szName );
   return 0;
}

/* unused */
HB_EXPORT unsigned int LetoDbFieldType( LETOTABLE * pTable, HB_USHORT uiIndex, unsigned int * uiType )
{
   if( ! uiType || ! uiIndex || uiIndex > pTable->uiFieldExtent )
      return 1;

   *uiType = ( pTable->pFields + uiIndex - 1 )->uiType;
   return 0;
}

/* unused */
HB_EXPORT unsigned int LetoDbFieldLen( LETOTABLE * pTable, HB_USHORT uiIndex, unsigned int * uiLen )
{
   if( ! uiLen || ! uiIndex || uiIndex > pTable->uiFieldExtent )
      return 1;

   *uiLen = ( pTable->pFields + uiIndex - 1 )->uiLen;
   return 0;
}

/* unused */
HB_EXPORT unsigned int LetoDbFieldDec( LETOTABLE * pTable, HB_USHORT uiIndex, unsigned int * uiDec )
{
   if( ! uiDec || ! uiIndex || uiIndex > pTable->uiFieldExtent )
      return 1;

   *uiDec = ( pTable->pFields + uiIndex - 1 )->uiDec;
   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbGoTo( LETOTABLE * pTable, unsigned long ulRecNo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   HB_BOOL          fFound = HB_FALSE;

   if( pTable->pFilterVar )
      Leto_VarExprSync( pConnection, pTable->pFilterVar, HB_FALSE );

   /* check hotbuffer also for same ulRecno */
   if( ulRecNo && pTable->ptrBuf && leto_HotBuffer( pTable ) )
   {
      HB_UCHAR * ptrBuf = pTable->Buffer.pBuffer;
      HB_ULONG   ulRecLen;

      do
      {
         if( HB_GET_LE_UINT32( ptrBuf + 4 ) == ulRecNo )  /* fix: + 4 */
         {
            if( ptrBuf != pTable->ptrBuf )  /* not re-read active record */
            {
               pTable->ptrBuf = ptrBuf;
               leto_refrSkipBuf( pTable );
               leto_ParseRecord( pConnection, pTable, ( char * ) pTable->ptrBuf );
            }
            else
               pTable->fBof = pTable->fEof = pTable->fFound = HB_FALSE;
#ifdef LETO_CLIENTLOG
            leto_clientlog( NULL, 0, "LetoDbGoTo found record %lu in skip buffer", pTable->ulRecNo );
#endif
            pTable->Buffer.ulShoots++;
            fFound = HB_TRUE;
            break;
         }
         ulRecLen = HB_GET_LE_UINT24( ptrBuf );
         if( ! ulRecLen || ulRecLen > 0xFFFF )  /* malicious data in buffer */
            break;
         ptrBuf += ulRecLen + 3;
      }
      while( ! leto_OutBuffer( &pTable->Buffer, ( char * ) ptrBuf ) );

   }

   if( ! fFound )
   {
      char     szData[ 32 ];
      HB_ULONG ulLen;

      ulLen = eprintf( szData, "%c;%lu;%lu;%c;", LETOCMD_goto, pTable->hTable, ulRecNo,
                               ( char ) ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 ) );
      if( ! leto_SendRecv( pConnection, szData, ulLen, 1021 ) )
         return 1;
      leto_ParseRecord( pConnection, pTable, leto_firstchar( pConnection ) );
      pTable->ptrBuf = NULL;
      if( pTable->fAutoRefresh )
         pTable->llCentiSec = LETO_CENTISEC();
   }

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbGoBottom( LETOTABLE * pTable )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char          sData[ 32 ];
   unsigned long ulLen;

   if( pTable->pFilterVar )
      Leto_VarExprSync( pConnection, pTable->pFilterVar, HB_FALSE );

   ulLen = eprintf( sData, "%c;%lu;-2;%c;", LETOCMD_goto, pTable->hTable,
                           ( char ) ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 ) );
   if( ! leto_SendRecv( pConnection, sData, ulLen, 1021 ) )
      return 1;

   leto_ParseRecord( pConnection, pTable, leto_firstchar( pConnection ) );
   pTable->ptrBuf = NULL;
   if( pTable->fAutoRefresh )
      pTable->llCentiSec = LETO_CENTISEC();

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbGoTop( LETOTABLE * pTable )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char          sData[ 32 ];
   unsigned long ulLen;

   if( pTable->pFilterVar )
      Leto_VarExprSync( pConnection, pTable->pFilterVar, HB_FALSE );

   ulLen = eprintf( sData, "%c;%lu;-1;%c;", LETOCMD_goto, pTable->hTable,
                           ( char ) ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 ) );
   if( ! leto_SendRecv( pConnection, sData, ulLen, 1021 ) )
      return 1;

   leto_ParseRecord( pConnection, pTable, leto_firstchar( pConnection ) );
   pTable->ptrBuf = NULL;
   if( pTable->fAutoRefresh )
      pTable->llCentiSec = LETO_CENTISEC();

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbSkip( LETOTABLE * pTable, long lToSkip )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   HB_ULONG         ulDataLen;
   const char *     ptr;
   char             sData[ 42 ];

   if( pTable->pFilterVar )
      Leto_VarExprSync( pConnection, pTable->pFilterVar, HB_FALSE );

   if( pTable->ptrBuf && leto_HotBuffer( pTable ) )
   {
      if( ! lToSkip )
         return 0;
      else
      {
         HB_ULONG       ul = 0;
         const HB_ULONG ulSkips = ( HB_ULONG ) ( lToSkip > 0 ? lToSkip : -lToSkip );

         if( pTable->BufDirection == ( lToSkip > 0 ? 1 : -1 ) )
         {
            while( ul < ulSkips )
            {
               pTable->ptrBuf += HB_GET_LE_UINT24( pTable->ptrBuf ) + 3;
               if( leto_OutBuffer( &pTable->Buffer, ( char * ) pTable->ptrBuf ) )
               {
                  ul = 0;
                  break;
               }
               else
                  ul++;
            }
            pTable->uiRecInBuf += ul;
         }
         else if( pTable->uiRecInBuf >= ulSkips )
         {
            HB_BOOL fFound = HB_TRUE;

            pTable->ptrBuf = pTable->Buffer.pBuffer;
            while( ul < pTable->uiRecInBuf - ulSkips )
            {
               pTable->ptrBuf += HB_GET_LE_UINT24( pTable->ptrBuf ) + 3;
               if( leto_OutBuffer( &pTable->Buffer, ( char * ) pTable->ptrBuf ) )
               {
                  fFound = HB_FALSE;
                  break;
               }
               else
                  ul++;
            }
            pTable->uiRecInBuf -= ulSkips;
            if( fFound )
               ul = ulSkips;
            else
               ul = 0;
         }

         if( ul == ulSkips )
         {
            leto_ParseRecord( pConnection, pTable, ( char * ) pTable->ptrBuf );
            pTable->Buffer.ulShoots++;
            return 0;
         }
      }
   }

   /* if( lToSkip == 0 || pTable->iBufRefreshTime < 0 ) will fetch only one record */
   ulDataLen = eprintf( sData, "%c;%lu;%ld;%lu;%c;%c;", LETOCMD_skip, pTable->hTable, lToSkip,
                        pTable->ulRecNo, ( char ) ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 ),
                        pTable->iBufRefreshTime >= 0 ? 'T' : 'F'  );
   ulDataLen = leto_SendRecv( pConnection, sData, ulDataLen, 1020 );
   if( ! ulDataLen-- )  /* first char is '+' */
      return 1;

   ptr = leto_firstchar( pConnection );
   leto_ParseRecord( pConnection, pTable, ptr );
   if( lToSkip )
   {
      leto_setSkipBuf( pTable, ptr, ulDataLen );
      pTable->BufDirection = ( lToSkip > 0 ? 1 : -1 );
      pTable->uiRecInBuf = 0;
   }
   else
   {
      pTable->ptrBuf = NULL;
      if( pTable->fAutoRefresh )
         pTable->llCentiSec = LETO_CENTISEC();
   }

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbSeek( LETOTABLE * pTable, const char * szKey, HB_USHORT uiKeyLen, HB_BOOL fSoftSeek, HB_BOOL fFindLast )
{
   if( szKey )
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
      char          szData[ LETO_MAX_KEY + LETO_MAX_TAGNAME + 56 ];
      unsigned long ulLen;

      if( pTable->pFilterVar )
         Leto_VarExprSync( pConnection, pTable->pFilterVar, HB_FALSE );

      ulLen = eprintf( szData, "%c;%lu;%c;", LETOCMD_seek, pTable->hTable,
                       ( char ) ( ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 )
                                  | ( fSoftSeek ? 0x10 : 0 )
                                  | ( fFindLast ? 0x20 : 0 ) ) );
      leto_AddKeyToBuf( szData, szKey, uiKeyLen, &ulLen );  /* trailing */
      if( ! leto_SendRecv( pConnection, szData, ulLen, 1021 ) )
         return 1;

      leto_ParseRecord( pConnection, pTable, leto_firstchar( pConnection ) );
      pTable->ptrBuf = NULL;
      if( pTable->fAutoRefresh )
         pTable->llCentiSec = LETO_CENTISEC();
   }

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbClearFilter( LETOTABLE * pTable )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char          szData[ 24 ];
   unsigned long ulLen;

   pTable->ptrBuf = NULL;
   ulLen = eprintf( szData, "%c;%lu;X;", LETOCMD_filt, pTable->hTable );
   if( ! leto_SendRecv2( pConnection, szData, ulLen, 1026 ) )
   {
      pConnection->iError = 1026;
      return 1;
   }

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbSetFilter( LETOTABLE * pTable, const char * szFilter, HB_BOOL fForceOpt )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char *        pData;
   unsigned long ulLen;

   if( szFilter && ( ulLen = strlen( szFilter ) ) > 0 )
   {
      pTable->ptrBuf = NULL;
      pData = ( char * ) hb_xgrab( ulLen + 36 );
      ulLen = eprintf( pData, "%c;%lu;%c;%s", LETOCMD_filt, pTable->hTable, fForceOpt ? 'T' : 'F', szFilter );
      if( ! leto_SendRecv( pConnection, pData, ulLen, 1026 ) )
      {
         hb_xfree( pData );
         return 1;
      }
      hb_xfree( pData );

      if( *( pConnection->szBuffer + 1 ) == '-' )  /* ok, '-' indicates not optimized */
         return 1;
   }
   else  /* also pre-checked in letoSetFilter */
   {
      pConnection->iError = 1026;
      return 1;
   }

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbCommit( LETOTABLE * pTable )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char          szData[ 32 ];
   unsigned long ulLen;

   if( pConnection->fTransActive || pTable->fReadonly )
   {
      pConnection->iError = 1;
      return HB_FAILURE;
   }

   ulLen = eprintf( szData, "%c;%lu;", LETOCMD_flush, pTable->hTable );
   if( ! leto_SendRecv2( pConnection, szData, ulLen, 1011 ) )
   {
      pConnection->iError = 1011;
      return 1;
   }

   return HB_SUCCESS;
}

HB_EXPORT HB_ERRCODE LetoDbPutRecord( LETOTABLE * pTable )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   HB_BOOL       fAppend = ( pTable->uiUpdated & LETO_FLAG_UPD_APPEND );
   HB_USHORT     ui, uiUpd = 0;
   char *        szData, * pData, * ptr;
   unsigned long ulLen;
   int           iRet = 0;

   if( pTable->fReadonly )
      return HB_FAILURE;
   hb_rddSetNetErr( HB_FALSE );

   for( ui = 0; ui < pTable->uiFieldExtent; ui++ )
   {
      if( pTable->pFieldUpd[ ui ] )
         uiUpd++;
   }
   szData = ( char * ) hb_xgrab( pTable->uiRecordLen + ( uiUpd * 8 ) + 20 );
   pData = szData + 4;

   if( fAppend )
      pData += eprintf( pData, "%c;%lu;%c%c;%d;", LETOCMD_add, pTable->hTable, pTable->fHaveAutoinc ? '0' : ' ',
                                                  ( pTable->uiUpdated & LETO_FLAG_UPD_UNLOCK ) ? '1' : '0', uiUpd );
   else
      pData += eprintf( pData, "%c;%lu;%lu;%d;", LETOCMD_upd, pTable->hTable, pTable->ulRecNo, uiUpd );

   *pData++ = ( pTable->uiUpdated & LETO_FLAG_UPD_DELETE ) ? ( ( pTable->fDeleted ) ? '1' : '2' ) : '0';
   *pData++ = ';';

   if( ( pTable->uiUpdated & LETO_FLAG_UPD_FLUSH ) && ! pConnection->fTransActive )
   {
      if( fAppend )
         szData[ 4 ] = LETOCMD_cmta;
      else
         szData[ 4 ] = LETOCMD_cmtu;
   }

   if( uiUpd )  /* updated fields, no memo */
   {
      LETOFIELD *  pField;
      HB_BOOL      fTwoBytes = ( pTable->uiFieldExtent > 255 );
      unsigned int uiLen, uiRealLen;
      char *       ptrEnd;

      for( ui = 0; ui < pTable->uiFieldExtent; ui++ )
      {
         if( pTable->pFieldUpd[ ui ] )
         {
            pField = pTable->pFields + ui;
            ptr = ( char * ) ( pTable->pRecord + pTable->pFieldOffset[ ui ] );
            if( ! fTwoBytes )
               *pData++ = ( HB_BYTE ) ( ui + 1 ) & 0xFF;
            else
            {
               uiLen = ui + 1;
               *pData++ = ( HB_BYTE ) uiLen & 0xFF;
               uiLen >>= 8;
               *pData++ = ( HB_BYTE ) uiLen & 0xFF;
            }

            switch( pField->uiType )
            {
               case HB_FT_STRING:
                  if( pField->uiFlags )  /* binary, compressed, encrypted, ... */
                     uiRealLen = pField->uiLen;
                  else  /* Trimmed field length */
                  {
                     ptrEnd = ptr + pField->uiLen - 1;
                     while( ptrEnd > ptr && *ptrEnd == ' ' )
                        ptrEnd--;
                     uiRealLen = ptrEnd - ptr + ( *ptrEnd == ' ' ? 0 : 1 );
                  }
                  if( pField->uiLen < 256 )
                  {
                     *pData = ( HB_BYTE ) uiRealLen & 0xFF;
                     uiLen = 1;
                  }
                  else
                  {
                     uiLen = leto_n2b( pData + 1, uiRealLen );
                     *pData = ( HB_BYTE ) uiLen & 0xFF;
                     uiLen++;
                  }
                  pData += uiLen;
                  memcpy( pData, ptr, uiRealLen );
                  pData += uiRealLen;
                  break;

               case HB_FT_LONG:
               case HB_FT_FLOAT:
                  ptrEnd = ptr + pField->uiLen - 1;
                  while( ptrEnd > ptr && *ptr == ' ' )
                     ptr++;
                  uiRealLen = ptrEnd - ptr + 1;
                  *pData = ( HB_BYTE ) uiRealLen & 0xFF;
                  pData++;
                  memcpy( pData, ptr, uiRealLen );
                  pData += uiRealLen;
                  break;

               case HB_FT_DATE:
                  memcpy( pData, ptr, pField->uiLen );
                  pData += pField->uiLen;
                  break;

               case HB_FT_LOGICAL:
                  *pData++ = *ptr;
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
                     *pData++ = *ptr;
                     switch( *ptr++ )
                     {
                        case 'D':
                           memcpy( pData, ptr, 8 );
                           pData += 8;
                           break;

                        case 'L':
                           *pData++ = *ptr;
                           break;

                        case 'N':
                           uiLen = ( ( HB_UCHAR ) *ptr ) & 0xFF;
                           memset( pData, ' ', pField->uiLen - uiLen );
                           memcpy( pData + ( pField->uiLen - uiLen ), ptr, uiLen );
                           pData += uiLen + 1;
                           break;

                        case 'C':
                           uiLen = leto_b2n( ptr, 2 );
                           memcpy( pData, ptr, uiLen + 2 );
                           pData += uiLen + 2;
                           break;
                     }
                  }
                  break;
            }
            if( ! --uiUpd )  /* done, skip remaining fields of table */
               break;
         }
      }
   }
   ulLen = pData - szData - 4;
   leto_SetUpdated( pTable, LETO_FLAG_UPD_NONE );

   if( pConnection->fTransActive )
   {
      pData = leto_AddLen( szData + 4, &ulLen, HB_TRUE );  /* before */
      leto_AddTransBuffer( pConnection, pData, ulLen );
      if( ! fAppend && pTable->ulRecNo )
         leto_AddTransList( pConnection, pTable );
      else
         leto_AddTransAppend( pConnection, pTable );
   }
   else  /* send without extra pre-leading length */
   {
      int iOk;

      if( pTable->ptrBuf )
         leto_refrSkipBuf( pTable );

      if( fAppend )  /* wait for answer */
         iOk = leto_SendRecv( pConnection, szData + 4, ulLen, 0 );
      else
         iOk = leto_SendRecv2( pConnection, szData + 4, ulLen, 1021 );

      ptr = pConnection->szBuffer;
      if( ! iOk )
      {
         if( *ptr == '-' )
         {
            unsigned int uErr;

            if( ptr[ 4 ] == ':' )
               sscanf( ptr + 5, "%u-%u-", &uErr, &pConnection->iError );
            else
               pConnection->iError = 1021;
         }
         iRet = 1;
      }
      else if( fAppend && *ptr == '+' )
      {
         ptr++;
         if( ! pTable->fHaveAutoinc )
            pTable->ulRecNo = strtoul( ptr, NULL, 10 );
         else
            leto_ParseRecord( pConnection, pTable, ptr );
      }
      else if( fAppend )
      {
         unsigned int uErr;

         if( ptr[ 4 ] == ':' )
            sscanf( ptr + 5, "%u-%u-", &uErr, &pConnection->iError );
         else
            pConnection->iError = 1021;
         iRet = 1;
      }
   }
   hb_xfree( szData );

   return iRet;
}

HB_EXPORT HB_ERRCODE LetoDbAppend( LETOTABLE * pTable, unsigned int fUnLockAll )
{
   if( fUnLockAll )
      leto_SetUpdated( pTable, LETO_FLAG_UPD_APPEND | LETO_FLAG_UPD_UNLOCK );
   else
      leto_SetUpdated( pTable, LETO_FLAG_UPD_APPEND );
   pTable->fBof = pTable->fEof = pTable->fFound = pTable->fDeleted = HB_FALSE;
   pTable->ptrBuf = NULL;
   pTable->ulRecCount++;
   pTable->ulRecNo = 0;  /* needed at server side for transactions to be zero*/

   /* if pTable->fHaveAutoinc, autoinc-values will be received in LetoDbPutRecord() */
   leto_SetBlankRecord( pTable );
   if( LetoDbPutRecord( pTable ) == HB_SUCCESS )
   {
      if( ! pTable->fFLocked && pTable->fShared && ( ! pTable->fHaveAutoinc || ! pTable->ulRecNo ) )
         pTable->fRecLocked = HB_TRUE;
      if( pTable->fAutoRefresh )
         pTable->llCentiSec = LETO_CENTISEC();
      return HB_SUCCESS;
   }

   return HB_FAILURE;
}

HB_EXPORT HB_ERRCODE LetoDbOrderCreate( LETOTABLE * pTable, const char * szBagName, const char * szTag,
                                        const char * szKey, unsigned int uiFlags,
                                        const char * szFor, const char * szWhile, unsigned long ulNext )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char          szData[ LETO_IDXINFOBLOCK ];
   unsigned long ulLen;
   const char *  ptr;

   if( ! ( uiFlags & ( LETO_INDEX_REST | LETO_INDEX_CUST | LETO_INDEX_FILT ) )
       && ( ! szFor || ! *szFor ) && ( ! szWhile || ! *szWhile ) && ! ulNext )
      uiFlags |= LETO_INDEX_ALL;

   ulLen = eprintf( szData, "%c;%lu;%s;%s;%s;%c;%s;%s;%c;%lu;%lu;%s;%c;%c;%c;%c;%c%c%c;", LETOCMD_creat_i,
                    pTable->hTable,
                    szBagName ? szBagName : "",
                    szTag ? szTag : "",
                    szKey,
                    ( uiFlags & LETO_INDEX_UNIQ ) ? 'T' : 'F',
                    szFor ? szFor : "",
                    szWhile ? szWhile : "",
                    ( uiFlags & LETO_INDEX_ALL )  ? 'T' : 'F',
                    pTable->ulRecNo,
                    ulNext,
                    "",
                    ( uiFlags & LETO_INDEX_REST ) ? 'T' : 'F',
                    ( uiFlags & LETO_INDEX_DESC ) ? 'T' : 'F',
                    ( uiFlags & LETO_INDEX_CUST ) ? 'T' : 'F',
                    ( uiFlags & LETO_INDEX_ADD )  ? 'T' : 'F',
                    ( uiFlags & LETO_INDEX_TEMP ) ? 'T' : 'F',
                    ( uiFlags & LETO_INDEX_EXCL ) ? 'T' : 'F',
                    ( uiFlags & LETO_INDEX_FILT ) ? 'T' : 'F' );
   if( ( uiFlags & LETO_INDEX_USEI ) && pTable->pTagCurrent )
      ulLen += eprintf( szData + ulLen, "%s;", pTable->pTagCurrent->TagName );

   if( ! leto_SendRecv( pConnection, szData, ulLen, 0 ) || *pConnection->szBuffer != '+' )
      return 1;

   ptr = leto_ParseTagInfo( pTable, leto_firstchar( pConnection ) );
   leto_ParseRecord( pConnection, pTable, ptr );
   pTable->ptrBuf = NULL;
   if( pTable->fAutoRefresh )
      pTable->llCentiSec = LETO_CENTISEC();

   return 0;
}

/* note: szTagName must be converted to UPPER case by caller */
HB_EXPORT HB_ERRCODE LetoDbOrderFocus( LETOTABLE * pTable, const char * szTagName, unsigned int uiOrder )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   LETOTAGINFO * pTagInfo = pTable->pTagInfo;
   char          szData[ 42 + LETO_MAX_TAGNAME ];
   unsigned long ulLen;

   if( pTable->pTagCurrent )
      pTable->pTagCurrent->ulKeyNo = pTable->pTagCurrent->ulKeyCount = 0;  /* leto_ClearTagBuf( pTable->pTagCurrent ); */

   if( szTagName )
   {
      if( ! *szTagName )
         pTable->pTagCurrent = NULL;
      else if( pTagInfo )
      {
         while( pTagInfo )
         {
            if( ! strcmp( pTagInfo->TagName, szTagName ) )
               break;
            pTagInfo = pTagInfo->pNext;
         }
         pTable->pTagCurrent = pTagInfo;
      }
   }
   else
   {
      if( ! uiOrder || uiOrder > ( unsigned int ) pTable->uiOrders )
         pTable->pTagCurrent = NULL;
      else if( pTagInfo )
      {
         while( --uiOrder )
         {
            pTagInfo = pTagInfo->pNext;
         }
         pTable->pTagCurrent = pTagInfo;
      }
   }

   ulLen = eprintf( szData, "%c;%lu;11;%s;%lu;", LETOCMD_ord, pTable->hTable,
                    pTable->pTagCurrent ? pTable->pTagCurrent->TagName : "", pTable->ulRecNo );
   if( ! leto_SendRecv( pConnection, szData, ulLen, 1021 ) )
      return 1;
   leto_ParseRecord( pConnection, pTable, leto_firstchar( pConnection ) );
   pTable->ptrBuf = NULL;
   if( pTable->fAutoRefresh )
      pTable->llCentiSec = LETO_CENTISEC();

   return pTagInfo ? 0 : 1;
}

HB_EXPORT HB_ERRCODE LetoDbIsRecLocked( LETOTABLE * pTable, unsigned long ulRecNo, unsigned int * uiRes )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

   pConnection->iError = 0;
   if( ulRecNo != 0 && ulRecNo != pTable->ulRecNo && ! pTable->fReadonly )
   {
      if( ! pTable->fShared || leto_IsRecLocked( pTable, ulRecNo ) )
         *uiRes = 1;
      else
      {
         char     szData[ 32 ], * ptr;
         HB_ULONG ulLen;

         ulLen = eprintf( szData, "%c;%lu;%lu;", LETOCMD_islock, pTable->hTable, ulRecNo );
         if( ! leto_SendRecv( pConnection, szData, ulLen, 0 ) || leto_checkLockError( pConnection ) )
            return 1;

         ptr = leto_firstchar( pConnection );
         *uiRes = ( *ptr == 'T' );
      }
   }
   else
      *uiRes = ! pTable->fReadonly && ( ! pTable->fShared || pTable->fRecLocked );

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbRecLock( LETOTABLE * pTable, unsigned long ulRecNo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char             szData[ 48 ];
   unsigned long    ulLen;

   if( pTable->fReadonly )
      return HB_FAILURE;
   else if( ! pTable->fShared || ( ulRecNo == pTable->ulRecNo ? pTable->fRecLocked : leto_IsRecLocked( pTable, ulRecNo ) ) )
      return HB_SUCCESS;

   if( pTable->fFLocked )  /* release file lock beforehand */
      LetoDbFileUnLock( pTable );

   ulLen = eprintf( szData, "%c;%lu;r;%lu;%d;", LETOCMD_lock, pTable->hTable, ulRecNo, pConnection->iLockTimeOut );
   if( ! leto_SendRecv( pConnection, szData, ulLen, 0 ) || leto_checkLockError( pConnection ) )
      return 1;

   leto_ParseRecord( pConnection, pTable, leto_firstchar( pConnection ) );
   if( pTable->ptrBuf )
      leto_refrSkipBuf( pTable );
   else if( pTable->fAutoRefresh )
      pTable->llCentiSec = LETO_CENTISEC();

   if( ulRecNo == pTable->ulRecNo )
      pTable->fRecLocked = HB_TRUE;

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbRecUnLock( LETOTABLE * pTable, unsigned long ulRecNo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char             szData[ 48 ];
   unsigned long    ulLen;

   if( pConnection->fTransActive )
   {
      pConnection->iError = 1031;
      return 1;
   }
   if( ! pTable->fShared || pTable->fFLocked || pTable->fReadonly ||
      ( ulRecNo == pTable->ulRecNo ? ! pTable->fRecLocked : ! leto_IsRecLocked( pTable, ulRecNo ) ) )
      return 0;

   ulLen = eprintf( szData, "%c;%lu;r;%lu;", LETOCMD_unlock, pTable->hTable, ulRecNo );

   if( ! leto_SendRecv2( pConnection, szData, ulLen, 1038 ) )
      return 1;
   if( ulRecNo == pTable->ulRecNo )
      pTable->fRecLocked = HB_FALSE;

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbFileLock( LETOTABLE * pTable )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char             szData[ 48 ];
   HB_ULONG         ulLen;
   const char *     ptr;

   if( pTable->fReadonly )
      return HB_FAILURE;
   else if( ! pTable->fShared || pTable->fFLocked  )
      return HB_SUCCESS;

   if( pTable->ulLocksMax )  /* release all our record locks */
      LetoDbFileUnLock( pTable );

   ulLen = eprintf( szData, "%c;%lu;f;%lu;%d;", LETOCMD_lock, pTable->hTable, pTable->ulRecNo, pConnection->iLockTimeOut );
   if( ! leto_SendRecv( pConnection, szData, ulLen, 0 ) || leto_checkLockError( pConnection ) )
      return 1;

   ptr = leto_firstchar( pConnection );
   if( *( ptr + 2 ) != '+' )  /* "+++" as ACK indicates no new record data to read, else it is a LE_UINT24 */
   {
      leto_ParseRecord( pConnection, pTable, ptr );
      if( pTable->ptrBuf )
         leto_refrSkipBuf( pTable );
      else if( pTable->fAutoRefresh )
         pTable->llCentiSec = LETO_CENTISEC();
   }
   pTable->fFLocked = HB_TRUE;

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbFileUnLock( LETOTABLE * pTable )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char             szData[ 48 ];
   unsigned long    ulLen;

   if( pConnection->fTransActive )
   {
      pConnection->iError = 1031;
      return 1;
   }
   if( ! pTable->fShared || pTable->fReadonly )
      return HB_SUCCESS;

   ulLen = eprintf( szData, "%c;%lu;f;", LETOCMD_unlock, pTable->hTable );
   if( ! leto_SendRecv2( pConnection, szData, ulLen, 1038 ) )
      return 1;

   pTable->fFLocked = pTable->fRecLocked = HB_FALSE;
   pTable->ulLocksMax = 0;  /* maybe better in letoUnLock() */

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbPack( LETOTABLE * pTable )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char          szData[ 32 ];
   unsigned long ulLen;

   if( pConnection->fTransActive || pTable->fReadonly || pTable->fShared )
   {
      pConnection->iError = 1;
      return 1;
   }

   ulLen = eprintf( szData, "%c;%lu;", LETOCMD_pack, pTable->hTable );
   if( ! leto_SendRecv( pConnection, szData, ulLen, 1021 ) )
      return 1; /* 2; */
   pTable->ptrBuf = NULL;

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbZap( LETOTABLE * pTable )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char          szData[ 32 ];
   unsigned long ulLen;

   if( pConnection->fTransActive || pTable->fReadonly || pTable->fShared )
   {
      pConnection->iError = 1;
      return 1;
   }

   ulLen = eprintf( szData, "%c;%lu;", LETOCMD_zap, pTable->hTable );
   if( ! leto_SendRecv( pConnection, szData, ulLen, 1021 ) )
      return 1;  /* 2; */
   pTable->ptrBuf = NULL;

   return 0;
}

HB_EXPORT HB_ERRCODE LetoDbReindex( LETOTABLE * pTable )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   char          szData[ 32 ];
   unsigned long ulLen;

   if( pConnection->fTransActive || pTable->fReadonly || pTable->fShared )
   {
      pConnection->iError = 1;
      return 1;
   }

   ulLen = eprintf( szData, "%c;%lu;03;", LETOCMD_ord, pTable->hTable );
   if( ! leto_SendRecv( pConnection, szData, ulLen, 1006 ) )
      return 1; /* 2; */
   pTable->ptrBuf = NULL;

   return 0;
}

HB_EXPORT const char * LetoMgGetInfo( LETOCONNECTION * pConnection )
{
   char szData[ 6 ];

   eprintf( szData, "%c;00;", LETOCMD_mgmt );
   if( leto_DataSendRecv( pConnection, szData, 5 ) )
      return leto_firstchar( pConnection );
   else
      return NULL;
}

HB_EXPORT const char * LetoMgSysInfo( LETOCONNECTION * pConnection )
{
   char szData[ 6 ];

   eprintf( szData, "%c;05;", LETOCMD_mgmt );
   if( leto_DataSendRecv( pConnection, szData, 5 ) )
      return leto_firstchar( pConnection );
   else
      return NULL;
}

HB_EXPORT const char * LetoMgGetUsers( LETOCONNECTION * pConnection, const char * szTable, const char * szList )
{
   unsigned long ulLen;
   char          szData[ HB_PATH_MAX + 64 ];

   if( szTable )
      ulLen = eprintf( szData, "%c;01;%s;", LETOCMD_mgmt, szTable );
   else
      ulLen = eprintf( szData, "%c;01;-1;", LETOCMD_mgmt );
   if( szList )
      ulLen = eprintf( szData + ulLen, "%s;", szList );

   if( leto_DataSendRecv( pConnection, szData, ulLen ) )
      return leto_firstchar( pConnection );
   else
      return NULL;
}

HB_EXPORT const char * LetoMgGetTables( LETOCONNECTION * pConnection, const char * szUser, const char * szList )
{
   unsigned long ulLen;
   char          szData[ HB_PATH_MAX + 64 ];

   if( szUser )
      ulLen = eprintf( szData, "%c;02;%s;", LETOCMD_mgmt, szUser );
   else
      ulLen = eprintf( szData, "%c;02;-1;", LETOCMD_mgmt );
   if( szList )
      ulLen += eprintf( szData + ulLen, "%s;", szList );

   if( leto_DataSendRecv( pConnection, szData, ulLen ) )
      return leto_firstchar( pConnection );
   else
      return NULL;
}

HB_EXPORT const char * LetoMgGetIndex( LETOCONNECTION * pConnection, const char * szUser, const char * szTable, const char * szList )
{
   unsigned long ulLen;
   char          szData[ HB_PATH_MAX + 64 ];

   if( szUser )
   {
      if( szTable )
         ulLen = eprintf( szData, "%c;06;%s;%s;", LETOCMD_mgmt, szUser, szTable );
      else
         ulLen = eprintf( szData, "%c;06;%s;;", LETOCMD_mgmt, szUser );
   }
   else
   {
      if( szTable )
         ulLen = eprintf( szData, "%c;06;-1;%s;", LETOCMD_mgmt, szTable );
      else
         ulLen = eprintf( szData, "%c;06;-1;;", LETOCMD_mgmt );
   }
   if( szList )
      ulLen += eprintf( szData + ulLen, "%s;", szList );

   if( leto_DataSendRecv( pConnection, szData, ulLen ) )
      return leto_firstchar( pConnection );
   else
      return NULL;
}

HB_EXPORT const char * LetoMgGetLocks( LETOCONNECTION * pConnection, const char * szUser, const char * szTable, const char * szList )
{
   unsigned long ulLen;
   char          szData[ HB_PATH_MAX + 64 ];

   if( szUser )
      ulLen = eprintf( szData, "%c;04;%s;", LETOCMD_mgmt, szUser );
   else
      ulLen = eprintf( szData, "%c;04;-1;", LETOCMD_mgmt );
   if( szTable )
      ulLen += eprintf( szData + ulLen, "%s", szTable );
   if( szList )
      ulLen += eprintf( szData + ulLen, ";%s;", szList );

   if( leto_DataSendRecv( pConnection, szData, ulLen ) )
      return leto_firstchar( pConnection );
   else
      return NULL;
}

HB_EXPORT int LetoMgKillUser( LETOCONNECTION * pConnection, const char * szUserId )
{
   unsigned long ulLen;
   char          szData[ 32 ];
   int           iRet = -2;

   ulLen = eprintf( szData, "%c;09;%s;", LETOCMD_mgmt, szUserId );
   if( leto_DataSendRecv( pConnection, szData, ulLen ) )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *ptr == '+' && *( ptr + 3 ) == ';' )
         iRet = atoi( ptr + 4 );
      else
         iRet = -2;
   }

   return iRet;
}

HB_EXPORT char * LetoMgGetTime( LETOCONNECTION * pConnection )
{
   char szData[ 6 ];

   eprintf( szData, "%c;03;", LETOCMD_mgmt );
   if( leto_DataSendRecv( pConnection, szData, 5 ) )
      return leto_firstchar( pConnection );
   else
      return NULL;
}

/* new param uilLength: set to 0 for auto determing by strlen(), > 0 for binary data */
HB_EXPORT int LetoVarSet( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar, char cType,
                          const char * pValue, unsigned int uiLength, unsigned int uiFlags, char ** pRetValue )
{
   unsigned long ulLen = 24 + strlen( szGroup ) + strlen( szVar ) + ( uiLength ? uiLength : strlen( pValue ) );
   char *        pData, * ptr;
   char          cFlag1 = ( ' ' | LETO_VCREAT ), cFlag2 = ' ';  /* LETO_VCREAT now default */
   unsigned int  uiRes;

   cFlag1 |= ( uiFlags & ( LETO_VCREAT | LETO_VOWN | LETO_VDENYWR | LETO_VDENYRD ) );
   if( ( uiFlags & LETO_VNOCREAT ) )
      cFlag1 &= ~LETO_VCREAT;
   if( pRetValue )
      cFlag2 |= LETO_VPREVIOUS;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;%c;%s;%s;%c%c%c;", LETOCMD_var, LETOSUB_set, szGroup, szVar, cType, cFlag1, cFlag2 );
   ptr = pData + ulLen;
   ulLen = uiLength;
   ptr = leto_AddLen( ptr, &ulLen, HB_FALSE );  /* after */
   memcpy( ptr, pValue, uiLength );
   ptr += uiLength;
   *ptr = '\0';
   uiRes = leto_DataSendRecv( pConnection, pData, ( ptr - pData ) );
   hb_xfree( pData );

   if( uiRes )
   {
      ptr = leto_firstchar( pConnection );
      if( *( ptr - 1 ) != '+' )
         uiRes = 0;
   }

   if( uiRes && pRetValue )
   {
      if( uiRes < 3 )
         uiRes = 0;
      else
         *pRetValue = ptr;
   }

   return uiRes;
}

/* new param pulLen receives length of [ possible binary char ] data */
HB_EXPORT const char * LetoVarGet( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar, unsigned long * pulLen )
{
   unsigned long ulLen = 24 + strlen( szGroup ) + strlen( szVar );
   char *        pData;
   unsigned int  uiRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;%c;%s;%s;", LETOCMD_var, LETOSUB_get, szGroup, szVar );

   uiRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( uiRes )
   {
      const char * ptr = pConnection->szBuffer;

      if( *ptr++ == '+' && uiRes >= 3 )
      {
         if( pulLen )
            *pulLen = ( HB_ULONG ) uiRes - 3;
         return ptr;
      }
      else
         return NULL;
   }

   return NULL;
}

HB_EXPORT long LetoVarIncr( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar, unsigned int uiFlags )
{
   unsigned long ulLen = 24 + strlen( szGroup ) + strlen( szVar );
   char *        pData;
   char          cFlag1 = ' ', cFlag2 = ' ';
   unsigned int  uiRes;

   cFlag1 |= ( uiFlags & ( LETO_VCREAT | LETO_VOWN | LETO_VDENYWR | LETO_VDENYRD ) );
   cFlag2 |= ( uiFlags & LETO_VPREVIOUS );

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;%c;%s;%s;2%c%c;", LETOCMD_var, LETOSUB_inc, szGroup, szVar, cFlag1, cFlag2 );

   uiRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( uiRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' )
         return strtol( ptr + 2, NULL, 10 );
      else
      {
         pConnection->iError = ( unsigned int ) atoi( ptr );
         return 0;
      }
   }
   else if( ! pConnection->iError )
      pConnection->iError = 1;

   return 0;
}

HB_EXPORT long LetoVarDecr( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar, unsigned int uiFlags )
{
   unsigned long ulLen = 24 + strlen( szGroup ) + strlen( szVar );
   char *        pData;
   char          cFlag1 = ' ', cFlag2 = ' ';
   unsigned int  uiRes;

   cFlag1 |= ( uiFlags & ( LETO_VCREAT | LETO_VOWN | LETO_VDENYWR | LETO_VDENYRD ) );
   cFlag2 |= ( uiFlags & LETO_VPREVIOUS );

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;%c;%s;%s;2%c%c;", LETOCMD_var, LETOSUB_dec, szGroup, szVar, cFlag1, cFlag2 );

   uiRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( uiRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' )
         return strtol( ptr + 2, NULL, 10 );
      else
      {
         pConnection->iError = ( unsigned int ) atoi( ptr );
         return 0;
      }
   }
   else if( ! pConnection->iError )
      pConnection->iError = 1;

   return 0;
}

HB_EXPORT int LetoVarDel( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar )
{
   unsigned long ulLen = 24 + strlen( szGroup ) + strlen( szVar );
   char *        pData;
   unsigned int  uiRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;%c;%s;%s;", LETOCMD_var, LETOSUB_del, szGroup, szVar );

   uiRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( uiRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' )
         return 1;
      else
         return 0;
   }

   return 0;
}

HB_EXPORT const char * LetoVarGetList( LETOCONNECTION * pConnection, const char * szGroup, HB_LONG lMaxLen )
{
   unsigned long ulLen = 32 + ( ( szGroup ) ? strlen( szGroup ) : 0 );
   char *        pData;
   unsigned int  uiRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;%c;%s;;%ld;", LETOCMD_var, LETOSUB_list, ( szGroup ) ? szGroup : "", lMaxLen );

   uiRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( uiRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' )
         return ptr;
      else
         return NULL;
   }

   return NULL;
}

HB_EXPORT HB_BOOL LetoFileExist( LETOCONNECTION * pConnection, const char * szFile )
{
   unsigned long ulLen = 24 + strlen( szFile );
   char *        pData;
   unsigned int  uiRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;01;%s;", LETOCMD_file, szFile );

   uiRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( uiRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' && *ptr == 'T' )
         return HB_TRUE;

      pConnection->iError = ( unsigned int ) atoi( ptr + 2 );
   }
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   return HB_FALSE;
}

HB_EXPORT HB_BYTE LetoFileErase( LETOCONNECTION * pConnection, const char * szFile )
{
   unsigned long ulLen = 24 + strlen( szFile );
   char *        pData;
   unsigned int  uiRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;02;%s;", LETOCMD_file, szFile );

   uiRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( uiRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' && ( *ptr == 'T' ) )
         return 1;

      pConnection->iError = ( unsigned int ) atoi( ptr + 2 );
   }
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   return 0;
}

HB_EXPORT HB_BYTE LetoFileRename( LETOCONNECTION * pConnection, const char * szFile, const char * szFileNew )
{
   unsigned long ulLen = 24 + strlen( szFile ) + strlen( szFileNew );
   char *        pData;
   unsigned int  uiRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;03;%s;%s;", LETOCMD_file, szFile, szFileNew );

   uiRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( uiRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' && ( *ptr == 'T' ) )
         return 1;

      pConnection->iError = ( unsigned int ) atoi( ptr + 2 );
   }
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   return 0;
}

HB_EXPORT HB_BYTE LetoFileCopy( LETOCONNECTION * pConnection, const char * szFile, const char * szFileNew )
{
   unsigned long ulLen = 24 + strlen( szFile ) + strlen( szFileNew );
   char *        pData;
   unsigned int  uiRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;17;%s;%s;", LETOCMD_file, szFile, szFileNew );

   uiRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( uiRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' && ( *ptr == 'T' ) )
         return 1;

      pConnection->iError = ( unsigned int ) atoi( ptr + 2 );
   }
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   return 0;
}

HB_EXPORT const char * LetoMemoRead( LETOCONNECTION * pConnection, const char * szFile, unsigned long * ulMemoLen )
{
   unsigned long ulLen = 24 + strlen( szFile );
   char *        pData;
   unsigned long ulRes;

   pConnection->iError = 0;

   pData = ( char * ) hb_xgrab( ulLen );
   ulRes = eprintf( pData, "%c;04;%s;", LETOCMD_file, szFile );

   ulRes = leto_DataSendRecv( pConnection, pData, ulRes );
   hb_xfree( pData );
   if( ulRes > 3 && ! memcmp( pConnection->szBuffer, "+F;", 3 ) )
      pConnection->iError = atoi( pConnection->szBuffer + 3 );
   else if( ulRes > 3 )
      return leto_DecryptText( pConnection, ulMemoLen, pConnection->szBuffer + 3 );
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   *ulMemoLen = 0;
   return NULL;
}

HB_EXPORT HB_BOOL LetoMemoWrite( LETOCONNECTION * pConnection, const char * szFile, const char * szValue, unsigned long ulLen )
{
   char *        pData;
   unsigned long ulRes;
   HB_ULONG      ulBufLen = 32 + strlen( szFile ) + ulLen;

   if( ulBufLen > pConnection->ulBufCryptLen )
   {
      if( ! pConnection->ulBufCryptLen )
         pConnection->pBufCrypt = ( char * ) hb_xgrab( ulBufLen + 1 );
      else
         pConnection->pBufCrypt = ( char * ) hb_xrealloc( pConnection->pBufCrypt, ulBufLen + 1 );
      pConnection->ulBufCryptLen = ( HB_ULONG ) ulBufLen;
   }
   pData = pConnection->pBufCrypt;

   pConnection->iError = 0;
   ulRes = eprintf( pData, "%c;13;%s;;0;", LETOCMD_file, szFile );
   ulRes += leto_CryptText( pConnection, szValue, ulLen, ulRes );
   pData = pConnection->pBufCrypt;

   if( pConnection->iError )
      ulRes = 0;
   else
      ulRes = leto_DataSendRecv( pConnection, pData, ulRes );
   if( ulRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' )
      {
         if( *ptr == 'T' )
            return HB_TRUE;
         else
            pConnection->iError = atoi( ptr + 2 );
      }
   }
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   return HB_FALSE;
}

HB_EXPORT const char * LetoFileRead( LETOCONNECTION * pConnection, const char * szFile, unsigned long ulStart, unsigned long * ulLen )
{
   char *        pData;
   unsigned long ulRes;

   pData = ( char * ) hb_xgrab( 32 + strlen( szFile ) );
   ulRes = eprintf( pData, "%c;10;%s;%lu;%lu;", LETOCMD_file, szFile, ulStart, *ulLen );

   ulRes = leto_DataSendRecv( pConnection, pData, ulRes );
   hb_xfree( pData );
   if( ulRes > 3 && ! memcmp( pConnection->szBuffer, "+F;", 3 ) )
      pConnection->iError = atoi( pConnection->szBuffer + 3 );
   else if( ulRes > 3 )
      return leto_DecryptText( pConnection, ulLen, pConnection->szBuffer + 3 );
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   *ulLen = 0;
   return NULL;
}

HB_EXPORT HB_BOOL LetoFileWrite( LETOCONNECTION * pConnection, const char * szFile, const char * szValue, unsigned long ulStart, unsigned long ulLen )
{
   char *        pData;
   unsigned long ulRes;
   HB_ULONG      ulBufLen = 48 + strlen( szFile ) + ulLen;

   if( ulBufLen > pConnection->ulBufCryptLen )
   {
      if( ! pConnection->ulBufCryptLen )
         pConnection->pBufCrypt = ( char * ) hb_xgrab( ulBufLen + 1 );
      else
         pConnection->pBufCrypt = ( char * ) hb_xrealloc( pConnection->pBufCrypt, ulBufLen + 1 );
      pConnection->ulBufCryptLen = ( HB_ULONG ) ulBufLen;
   }
   pData = pConnection->pBufCrypt;

   pConnection->iError = 0;
   ulRes = eprintf( pData, "%c;14;%s;%lu;", LETOCMD_file, szFile, ulStart );
   ulRes += leto_CryptText( pConnection, szValue, ulLen, ulRes );
   pData = pConnection->pBufCrypt;

   if( pConnection->iError )
      ulRes = 0;
   else
      ulRes = leto_DataSendRecv( pConnection, pData, ulRes );
   if( ulRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' )
      {
         if( *ptr == 'T' )
            return HB_TRUE;
         else
            pConnection->iError = atoi( ptr + 2 );
      }
      else
         pConnection->iError = -2;
   }
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   return HB_FALSE;
}

HB_EXPORT long LetoFileSize( LETOCONNECTION * pConnection, const char * szFile )
{
   unsigned long ulLen = 24 + strlen( szFile );
   char *        pData;
   unsigned long ulRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;11;%s;", LETOCMD_file, szFile );

   ulRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( ulRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' )
      {
         if( *ptr == 'T' )
         {
            ulLen = strtoul( ptr + 2, NULL, 10 );
            return ulLen;
         }
         else
            pConnection->iError = atoi( ptr + 2 );
      }
   }
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   return -1;
}

HB_EXPORT const char * LetoFileAttr( LETOCONNECTION * pConnection, const char * szFile, const char * szAttr )
{
   unsigned long ulLen = 25 + strlen( szFile ) + ( szAttr ? strlen( szAttr ) : 0 );
   char *        pData;
   unsigned long ulRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;15;%s;%s;", LETOCMD_file, szFile, ( szAttr ? szAttr : "" ) );

   ulRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( ulRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' )
      {
         if( *ptr == 'T' )
         {
            char * pEnd = strchr( ptr + 2, ';' );

            if( pEnd )
            {
               *pEnd = '\0';
               return ptr + 2;
            }
         }
         else
            pConnection->iError = atoi( ptr + 2 );
      }
   }
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   return "";
}

HB_EXPORT const char * LetoDirectory( LETOCONNECTION * pConnection, const char * szDir, const char * szAttr )
{
   unsigned long ulLen = 48 + strlen( szDir );
   char *        pData;
   unsigned long ulRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;12;%s;%s;", LETOCMD_file, szDir, ( ( szAttr ) ? szAttr : "" ) );

   ulRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( ulRes )
   {
      const char * ptr = pConnection->szBuffer;

      if( ! strncmp( ptr, "+F;", 3 ) )
         pConnection->iError = atoi( ptr + 3 );
      else
         return ptr + 3;
   }
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   return NULL;
}

HB_EXPORT HB_BYTE LetoDirMake( LETOCONNECTION * pConnection, const char * szFile )
{
   unsigned long ulLen = 24 + strlen( szFile );
   char *        pData;
   unsigned int  uiRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;05;%s;", LETOCMD_file, szFile );

   uiRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( uiRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' && *ptr == 'T' )
         return 1;

      pConnection->iError = ( unsigned int ) atoi( ptr + 2 );
   }
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   return 0;
}

HB_EXPORT HB_BYTE LetoDirExist( LETOCONNECTION * pConnection, const char * szFile )
{
   unsigned long ulLen = 24 + strlen( szFile );
   char *        pData;
   unsigned int  uiRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;06;%s;", LETOCMD_file, szFile );

   uiRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( uiRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' && *ptr == 'T' )
         return 1;

      pConnection->iError = ( unsigned int ) atoi( ptr + 2 );
   }
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   return 0;
}

HB_EXPORT HB_BYTE LetoDirRemove( LETOCONNECTION * pConnection, const char * szFile )
{
   unsigned long ulLen = 24 + strlen( szFile );
   char *        pData;
   unsigned int  uiRes;

   pData = ( char * ) hb_xgrab( ulLen );
   ulLen = eprintf( pData, "%c;07;%s;", LETOCMD_file, szFile );

   uiRes = leto_DataSendRecv( pConnection, pData, ulLen );
   hb_xfree( pData );
   if( uiRes )
   {
      const char * ptr = leto_firstchar( pConnection );

      if( *( ptr - 1 ) == '+' && *ptr == 'T' )
         return 1;

      pConnection->iError = ( unsigned int ) atoi( ptr + 2 );
   }
   else if( ! pConnection->iError )
      pConnection->iError = -1;

   return 0;
}

HB_EXPORT HB_BOOL leto_Ping( LETOCONNECTION * pConnection )
{
   HB_BOOL fPong = HB_FALSE;

   if( pConnection )
   {
      char szData[ 3 ];

      szData[ 0 ] = LETOCMD_ping;
      szData[ 1 ] = ';';
      szData[ 2 ] = '\0';
      if( leto_DataSendRecv( pConnection, szData, 2 ) && *( pConnection->szBuffer + 1 ) == '@' )
         fPong = HB_TRUE;
   }

   return fPong;
}

/* unused */
HB_EXPORT void LetoFreeStr( char * szStr )
{
   hb_xfree( szStr );
}
