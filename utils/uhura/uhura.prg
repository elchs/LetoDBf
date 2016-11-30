/*
 * why Uhura ? -- because Nyota was Lt. for communication ;-)
 * why these C-level functions ? -- just for my training ..
 *
 * Following is wished a bit compatible to hb_udpds_find() // hb_udpds_start()
 * see: contrib/hbmisc/udpds.prg -- Thanks Mindaugas, there my inspiration goes */
 * That means: you can use my UDPds server and ask with hb_udpds_Find()
 * or use in your project hb_udpds_Start() and ask with my detectServer()
 *
 * BUT you may prefer my detectServer(), as it acts more precise and extracts an IP address
 * out of the received answer -- and Uhura server can serve for multiple service names at one time,
 * and moreover is capable to run with multiple instances on specific interfaces.
 *
 *  you should adopt the SERVICE_NAMES #define below to your needs !
 *
 *  compile: ( only Windows user add:  hbwin.ch )
 *     hbmk2 uhura.prg -W3 -es0 -static -mt [ hbwin.ch ]
 *
 *  using  : WINDOWS user, according my tests, can NOT bind to a specific cServerIP/ interface ?
 *           also Windows does not really know interface names like "eth1" in Linux
 *           Others can use: cServerIP  ==  IP address -OR- a device name, e.g. "eth2"
 *           cServiceName(s) string EVER ends with a ';' plus optional after a ':' server IP address
 *           e.g.: "hbnetio:192.168.1.12;"
 
       *** Linux user must enclose cServiceNames in quotation marks! ***
 *
 *     uhura LIST                       --> lists found devices
 *
 *     uhura cServerIP STOP             --> stop server for IP or device ( regularly not needed )
 *     uhura STOP                       --> stop server for first found device
 *
 *     uhura START                      --> use first found interface, use #SERVICE_NAMES
 *     uhura "cServiceNames;"           --> use first found interface with cServiceNames
 *         Linux user are flexible to start for specific interface:
 *     uhura cServerIP                  --> use this IP/interface, use #SERVICE_NAMES
 *     uhura cServerIP "cServiceNames;" --> use this IP/interface, for given cServiceNames
 *
 *         Windows only:
 *     uhura i | u                      --> i-nstall or u-ninstall Uhura as Windows service
 *                                          no further param possible, but adopt #SERVICE_NAMES in source
 *                                          before recompiling
 *
 * MUCH fun -- Rolf 'elch'
 */


/*
 * list of identifiers, this server answers with an IP
 * !!! DO NOT MISS a SEMICOLON ';' AFTER EVERY AND ALSO AFTER LAST SERVICE NAME !!!
 * each identifier may optional be followed after a ':' by an IP address as answer
 */
#define SERVICE_NAMES    "hbnetio;letodb;znnetio;harbour:208.73.211.170;"


/*
 * WARNING: Windows seem can't bind to a specific interface !?? 
 * Linux user can outcomment following line, if want to force a specific IP address / interface
 * without given at commandline
 */
//#define FORCE_IP  "192.168.1.231"  /* IP or interface name */


#include "hbsocket.ch"


/* Windows service install function copy/ adopt from:
 * harbour/contrib/hbwin/tests/service.prg -- Copyright 2010 Jose Luis Capel */
#ifdef __PLATFORM__WINDOWS

#include "hbwin.ch"
#define _SERVICE_NAME "Uhura_service_discovery"

PROCEDURE Main( cMode, cAction )

   hb_default( @cMode, "S" ) /* NOTE: Must be the default action */

   SWITCH Upper( cMode )
   CASE "I"

      IF win_serviceInstall( _SERVICE_NAME, "Uhura Service Discovery", , WIN_SERVICE_AUTO_START )
         ? "Service has been successfully installed as autostart"
         ? ""
         HB_RUN( "net start Uhura_service_discovery" )
      ELSE
         ? "Error installing service:", hb_ntos( wapi_GetLastError() ), win_ErrorDesc()
      ENDIF
      EXIT

   CASE "U"

      IF win_serviceDelete( _SERVICE_NAME )
         ? "Service has been deleted"
      ELSE
         ? "Error deleting service:", hb_ntos( wapi_GetLastError() ), win_ErrorDesc()
      ENDIF
      EXIT

   CASE "S"

      /* NOTE: Used when starting up as service.
               Do not invoke the executable manually with this option */
      IF win_serviceStart( _SERVICE_NAME, @uhura() )
         // ? "Service has started OK"
      ELSE
         // ? "Service has had some problems:", hb_ntos( wapi_GetLastError() ), win_ErrorDesc()
         // uhura( "STOP" )
      ENDIF
      EXIT

   OTHERWISE
      uhura( cMode, cAction )
      EXIT

   ENDSWITCH
   
RETURN


FUNCTION win_ErrorDesc( nCode, nLangID )

   LOCAL cMsg := Space( 2048 )

   wapi_FormatMessage( ,, nCode, nLangID, @cMsg )

   cMsg := RTrim( cMsg )
   IF Right( cMsg, Len( hb_eol() ) ) == hb_eol()
      cMsg := hb_StrShrink( cMsg, Len( hb_eol() ) )
   ENDIF

   RETURN RTrim( cMsg )


#else  // non - Windows

PROCEDURE main( cServerIP, cAction )
   uhura( cServerIP, cAction )
RETURN

#endif


/* main function for Uhura service discovery */
PROCEDURE uhura( cServerIP, cAction )
 LOCAL cServiceNames
 LOCAL cBroadcastIP
 LOCAL cNetmask
 LOCAL cTmp
 LOCAL lErr := .F.
 LOCAL lUnixErr := .F.

  hb_default( @cServerIP, "" )

  ALTD()

#ifdef FORCE_IP
  IF ! EMPTY( cServerIP ) .AND. RIGHT( cServerIP, 1 ) == ";"
    cAction := cServerIP
  ENDIF
  cServerIP := FORCE_IP
#endif

/* --- start:  check for optional given IP/interface -and/or- service names --- */
  IF ( EMPTY( cAction ) .OR. UPPER( cAction ) != "STOP" ) .AND.;
     ( EMPTY( cServerIP ) .OR.;
       ( ! EMPTY( cServerIP ) .AND. UPPER( cServerIP ) != "STOP" .AND. UPPER( cServerIP ) != "LIST" ) ) .AND.;
     ( ! UPPER( cServerIp ) $ "-?/?-HELP/HELP" )

    cServiceNames := SERVICE_NAMES
    IF ! EMPTY( cServerIP ) .AND. RIGHT( cServerIP, 1 ) == ";"
      cServiceNames := cServerIP
      cServerIP := ""
    ELSEIF ! EMPTY( cAction ) .AND. RIGHT( cAction, 1 ) == ";"
      cServiceNames := cAction
      cAction := ""
    ENDIF

    /* an interface name given instead IP ? */
    IF ! EMPTY( cServerIP ) .AND. ! UPPER( cServerIP ) == "START" .AND. ! isValidIP4( cServerIP )
      cTmp := cServerIP
      cServerIP := IPForInterface( cServerIP )
      IF EMPTY( cServerIP )
        ? "WARNING: interface <" + cTmp + "> unknown !, using first found"
      ENDIF
    ENDIF

    /* calculate the broadcast IP -- if IP ok and netmask found */
    IF ! EMPTY( cServerIP ) .AND. isValidIP4( cServerIP ) .AND. isValidIP4( cNetmask := netmask4IP( cServerIP ) )
      cBroadcastIP := broadcastIP( cServerIP, cNetmask )
    ELSE
      cBroadcastIP := "0.0.0.0"
    ENDIF

#ifdef __PLATFORM__UNIX
#ifndef DEBUG
   IF unix_daemon( 0, 0 ) == -1
      ? "Could not become a daemon, where is the hell ;-( "
      lErr := .T.
   ENDIF
#endif
#endif

    /* now start Uhura and wait for end .. */
    IF ! lErr .AND. startserver( cServiceNames, cServerIP, cBroadcastIP  )
      ? "sercice discovery now stopped"
    ELSE
      IF ! lErr
         ? "sercice discovery not started because of an error"
      ENDIF
#ifndef __PLATFORM__UNIX
      lErr := .T.
#else
      /* we are daemon now -- cannot display more */
      lUnixErr := .T.
#endif
    ENDIF


/* --- stop the server for optional given IP/ interface --- */
  ELSEIF ! EMPTY( cServerIP ) .AND. ( UPPER( cServerIP ) == "STOP" .OR.;
               ( ! EMPTY( cAction ) .AND. UPPER( cAction ) == "STOP" ) )
    IF ! isValidIP4( cServerIP )
      /* an interface name given ? -- or first interface when STOP */
      IF UPPER( cServerIP ) == "STOP"
        cServerIP := ""
      ENDIF
      cServerIP := IPForInterface( cServerIP )
    ENDIF
    IF ! EMPTY( cServerIP )
      IF srv_end( cServerIP )
        ? "sercice discovery will be stopped soon .."
      ELSE
        ? "ERROR: sercice discovery for " + cServerIP + " not found  .. :-("
        lErr := .T.
      ENDIF
    ELSE
      ? "ERROR: interface: " + cServerIP + " unknown !"
      lErr := .T.
    ENDIF

  ELSE
     lErr := .T.
  ENDIF

/* --- invalid call: show info about use and list interfaces --- */
  IF lErr
     ? ""
     ? ""
     ? "uhura [ IP-adress ] [ servicenames; ]  -> start server, opt. IP and identifier"
     ? "uhura [ IP-adress ] STOP               -> stop server [ for IP-address ]"
     ? "found interfaces :"
     listInterfaces()
     ? ""
     CLEAR TYPEAHEAD
     WAIT
  ENDIF

  CLEAR TYPEAHEAD

  IF lErr .OR. lUnixErr
     ErrorLevel( 1 )
  ENDIF

RETURN


STATIC PROCEDURE listInterfaces()
 LOCAL aIFace := hb_socketGetIFaces( HB_SOCKET_AF_INET, .T. )
 LOCAL nLen   := LEN( aIFace )
 LOCAL nIFace := 1

  DO WHILE nIFace <= nLen
    IF ! EMPTY( aIFace[ nIFace,  HB_SOCKET_IFINFO_HWADDR ] ) .AND.;
       ! aIFace[ nIFace, HB_SOCKET_IFINFO_HWADDR ] == "00:00:00:00:00:00"
      ? PADR( aIFace[ nIFace, HB_SOCKET_IFINFO_NAME ], 16 ), " = ", aIFace[ nIFace, HB_SOCKET_IFINFO_ADDR ]
    ENDIF
    nIFace++
  ENDDO
RETURN 


/* try to find IP address for an interface name -- or return the first found with a MAC */
STATIC FUNCTION IPForInterface( cName )
 LOCAL aIFace := hb_socketGetIFaces( HB_SOCKET_AF_INET, .T. )
 LOCAL nIFace := 0
 LOCAL cIP    := ""

  IF LEN( aIFace ) > 0
    IF ! EMPTY( cName )
      cName := UPPER( cName )
      nIFace := ASCAN( aIFace, { | aItm | UPPER( aItm[ HB_SOCKET_IFINFO_NAME ] ) == cName } )
    ELSE
      /* first interface with guilty MAC address */
      nIFace := ASCAN( aIFace, { | aItm | ! EMPTY( aItm[ HB_SOCKET_IFINFO_HWADDR ] ) .AND.;
                                          ! aItm[ HB_SOCKET_IFINFO_HWADDR ] == "00:00:00:00:00:00" } )
    ENDIF
  ENDIF
  IF nIFace > 0
    cIP := aIFace[ nIFace, HB_SOCKET_IFINFO_ADDR ]
  ENDIF
RETURN cIP


/* try to find netmask for an interface given by IP address */
STATIC FUNCTION netmask4IP( cIP )
 LOCAL aIFace   := hb_socketGetIFaces( HB_SOCKET_AF_INET, .T. )
 LOCAL cNetmask := ""
 LOCAL nIFace

  IF ! EMPTY( cIP ) .AND. LEN( aIFace ) > 0
    nIFace := ASCAN( aIFace, { | aItm | ( aItm[ HB_SOCKET_IFINFO_ADDR ] ) == cIP } )
    IF nIFace > 0
      cNetmask := aIFace[ nIFace, HB_SOCKET_IFINFO_NETMASK ]
    ENDIF
  ENDIF
RETURN cNetmask


/* following two functions can be done at C-level very easy with inet_pton(),
 * but unfortunately need >= Win Vista -- so here my versions at prg level */

/* validate IP4: only digits, >= 1 digit between/ after dots, numbers <= 255, and 3 dots */
STATIC FUNCTION isValidIP4( cAddr )
 LOCAL nDot := 0
 LOCAL nNum := 0
 LOCAL cNUm := ""
 LOCAL nLen, cTmp, lValid
 
  IF ! EMPTY( cAddr )
    nLen := LEN( cAddr )
    lValid := .T.
  ELSE
    nLen := 0
    lValid := .F.
  ENDIF

  DO WHILE nLen > 0
    cTmp := LEFT( cAddr, 1 )
    IF ! cTmp $ "0123456789."
      lValid := .F.
      EXIT
    ENDIF
    IF cTmp == "."
      /* no digit between dots ? -or- value too big ? */
      IF nNum == 0 .OR. VAL( cNum ) > 255
        lValid := .F.
        EXIT
      ENDIF
      nDot++
      nNum := 0
      cNum := ""
    ELSE
      cNum += cTmp
      nNum++
    ENDIF
    cAddr := SUBSTR( cAddr, 2 )
    nLen--
  ENDDO

  IF lValid .AND. ( nDot != 3 .OR. nNum == 0 .OR. VAL( cNum ) > 255 )
    lValid := .F.
  ENDIF

RETURN lValid


/* translate address with netmask to a valid broadcast IP4:  broadcast = ip | ( ~ subnet ) */
STATIC FUNCTION broadcastIP( cAddr, cNetm )
 LOCAL cBroadcast := ""
 LOCAL nEndAddr, nEndNetm
 LOCAL nTuple := 1

  DO WHILE nTuple <= 4
    nEndAddr := AT( ".", cAddr )
    nEndNetm := AT( ".", cNetm )
    cBroadcast += hb_ntos( hb_bitOr( VAL( SUBSTR( cAddr, 1, nEndAddr - 1 ) ),;
                           hb_bitAnd( hb_bitNot( VAL( SUBSTR( cNetm, 1, nEndNetm - 1 ) ) ), 255 ) ) )
    IF nTuple < 4
      cBroadcast += "."
      cAddr := SUBSTR( cAddr, nEndAddr + 1 )
      cNetm := SUBSTR( cNetm, nEndNetm + 1 )
    ENDIF

    nTuple++
  ENDDO
RETURN cBroadcast



* --- the C-level server functions --- *
#pragma BEGINDUMP
#include "hbapi.h"
#include "hbvm.h"
#include "hbxvm.h"
#include "hbsocket.h"
#include "hbthread.h"


/* free ports !
 * http://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml
 */
#define UDP_PORT     4221                      /* for client requests */
#define SRV_PORT     4222                      /* the port for main thread to stop this server */

#define STOPCMD_UHURA "stop_uhura_discovery"   /* this as service name request stop all uhuras */


#if defined( HB_OS_UNIX ) && ! defined( HB_EOL_CRLF )
  #define HB_EOL "\n"
#else
  #define HB_EOL "\r\n"
#endif

#define MSGSIZE_LEN  4
#define UDP_TIMEOUT  5000
/* for some feedback during testing */
#define DEBUG        1 

static const char * s_szAddress = NULL;        /* used also for IP-address in answer string */
static HB_SOCKET s_udpSocket = HB_NO_SOCKET;   /* socket for UDP thread */
static const char * s_szServiceNames;          /* list of identifiers, divided + ends with ';' */
static int s_iErrorCode = 0;                   /* error for socket send/ receive */
static HB_BOOL s_bExit = HB_FALSE;              /* main loop runs as long as it is HB_FALSE */


/* --- functions to end the server --- */

static HB_ULONG client_SockSend( HB_SOCKET hSocket, char * pBuffer, HB_ULONG ulLen )
{
   HB_ULONG ulSent = 0;
   HB_LONG lTmp, lLast = 1;

   while( ulSent < ulLen )
   {
      lTmp = lLast = hb_socketSend( hSocket, pBuffer + ulSent, ulLen - ulSent, 0, 3000 );

      if( lTmp > 0 )
         ulSent += lTmp;
      else if( ( s_iErrorCode = hb_socketGetError() ) != HB_SOCKET_ERR_TIMEOUT
               || hb_vmRequestQuery() != 0 || lLast <= 0 )
         break;
   }

   return ulSent;
}

HB_SOCKET client_Connect( const char * szHost, int iPort, int iTimeOut )
{
   HB_SOCKET hSocket;
   char * pszIpAddres;

   pszIpAddres = hb_socketResolveAddr( szHost, HB_SOCKET_AF_INET );
   if( pszIpAddres == NULL )
      return HB_NO_SOCKET;

   hSocket = hb_socketOpen( HB_SOCKET_PF_INET, HB_SOCKET_PT_STREAM, 0 );
   if( hSocket != HB_NO_SOCKET )
   {
      void * pSockAddr;
      unsigned uiLen;

      if( hb_socketInetAddr( &pSockAddr, &uiLen, pszIpAddres, iPort ) )
      {
         hb_socketSetKeepAlive( hSocket, HB_TRUE );
         if( hb_socketConnect( hSocket, pSockAddr, uiLen, iTimeOut ) == 0 )
            hb_socketSetNoDelay( hSocket, HB_TRUE );
         else
         {
            s_iErrorCode = hb_socketGetError();
            hb_socketClose( hSocket );
            hSocket = HB_NO_SOCKET;
         }
      }
      else
      {
         s_iErrorCode = hb_socketGetError();
         hb_socketClose(hSocket );
         hSocket = HB_NO_SOCKET;
      }
      hb_xfree( pSockAddr );
   }
   else
      s_iErrorCode = hb_socketGetError();

   hb_xfree( pszIpAddres );

   return hSocket;
}

HB_FUNC( SRV_END )
{
   const char * szAddr = hb_parc( 1 );
   int iPort = SRV_PORT;
   HB_SOCKET hSocket;
   char * szBlock;
   HB_BOOL bRet;

   hb_socketInit();

   hSocket = client_Connect( szAddr, iPort, 1000 );

   if( hSocket != HB_NO_SOCKET )
   {
      szBlock = ( char * ) hb_xgrab( ( MSGSIZE_LEN ) + 1 );
      HB_PUT_LE_UINT32( szBlock, 42 );
      client_SockSend( hSocket, szBlock, MSGSIZE_LEN );
      hb_xfree( szBlock );
      hb_socketClose( hSocket );
      bRet = HB_TRUE;
   }
   else
      bRet = HB_FALSE;

   hb_idleSleep( 0.1 );
   hb_socketClose( hSocket );

   hb_socketCleanup();

   hb_retl( bRet );
}


/* --- main server functions --- */

/* help function to thread safe set exit condition for main loop */
static HB_BOOL bExit( HB_BOOL bSet )
{
   static HB_CRITICAL_NEW( mutex_Exit );
   HB_BOOL bExit;

   hb_threadEnterCriticalSection( &mutex_Exit );
   if( bSet )
      s_bExit = HB_TRUE;
   bExit = s_bExit;
   hb_threadLeaveCriticalSection( &mutex_Exit );

   return bExit;
}

/* UDP service request answer thread loop */
static HB_THREAD_STARTFUNC( udpds )
{
   ( void ) ( Cargo );
   char * szBuffer = ( char * ) hb_xgrab( 2001 );
   int iLenRcv;
   PHB_ITEM pError = NULL;
   void * pSockAddr;
   unsigned int uiLen;
   char * szServiceName = ( char * ) hb_xgrab( 120 );
   char * szTmp = ( char * ) hb_xgrab( 120 );
   const char * ptr;
   char * ptr2, * ptr3;
   char * szInfo = ( char * ) hb_xgrab( 120 );

   hb_vmThreadInit( NULL );

   memset( szTmp, 0, 120 );
   memset( szServiceName, 0, 120 );
   memset( szBuffer, 0, 2001 );

   while( HB_TRUE )
   {
      hb_xvmSeqBegin();
      iLenRcv = hb_socketRecvFrom( s_udpSocket, szBuffer, 2000, 0 , &pSockAddr, &uiLen, UDP_TIMEOUT );
      if( hb_xvmSeqEndTest() )
      {
         if( hb_xvmSeqRecover() )
            break;  // QUIT executed
         else
         {
            pError = hb_itemNew( hb_stackItemFromTop( -1 ) );
            hb_stackPop();
         }
      }
      if( pError )
      {
         hb_itemRelease( pError );
         hb_xfree( pSockAddr );
         break;
      }

      if( iLenRcv == -1 )
      {
         if( hb_socketGetError() != HB_SOCKET_ERR_TIMEOUT )
            break;
      }
      else if( iLenRcv > 0 )
      {
         HB_BOOL bAnswered = HB_FALSE;
         char * szSendToIP = hb_socketAddrGetName( pSockAddr, uiLen );
         int iLenAddr, iLenCmp;

         szBuffer[ iLenRcv ] = '\0';
         ptr = s_szServiceNames;
         while( ptr && ( ptr2 = strchr( ptr, ';' ) ) != NULL )
         {
            /* extract one service identifier out of list */
            iLenCmp = ptr2 - ptr;
            strncpy( szServiceName, ptr, iLenCmp );
            szServiceName[ iLenCmp ] = '\0';
            
            /* IP address option in szServiceName ? -> separate it */
            if( ( ptr3 = strchr( szServiceName, ':' ) ) != NULL )
            {
               iLenAddr = iLenCmp - 1 - ( ptr3 - szServiceName );
               iLenCmp = ( ptr3 - szServiceName );
               szServiceName[ iLenCmp ] = '\0';
               ptr3++;
            }
            else
               iLenAddr = 0;

            ptr = ptr2 + 1;  /* next idetifier in s_szServiceNames */
            iLenCmp += 2;

            if( iLenCmp != iLenRcv ) /* different length -> try next service identifier */
               continue;

            sprintf( szTmp, "%c%s%c", '\5', szServiceName, '\0' );

            /* compare request with actual service identifier -> send answer */
            if( strncmp( szBuffer, szTmp, iLenCmp ) == 0 )
            {
               szTmp[ 0 ] = '\6';
               if( iLenAddr > 0 )      /* add optional string (address) after ':' in szServiceName */
               {
                  strncpy( szTmp + iLenCmp, ptr3, iLenAddr );
                  szTmp[ iLenCmp + iLenAddr ] = '\0';
               }
               else if( s_szAddress )  /* Uhura started for specific interface - add that IP address */
               {
                  iLenAddr = strlen( s_szAddress );
                  strncpy( szTmp + iLenCmp, s_szAddress, iLenAddr );
                  szTmp[ iLenCmp + iLenAddr ] = '\0';
               }

               hb_xvmSeqBegin();
               hb_socketSendTo( s_udpSocket, szTmp, iLenCmp + iLenAddr, 0, pSockAddr, uiLen, 1000 );
               hb_xvmSeqEnd();
               bAnswered = HB_TRUE;
#ifdef DEBUG
               snprintf( szInfo, 120, "answer send to: %s for service request: %s (%s)%s",
                         szSendToIP, szServiceName, szTmp + iLenCmp, HB_EOL );
               hb_conOutStd( szInfo, strlen( szInfo ) );
#endif
            }
         }

         if( ! bAnswered )
         {
            const char * szStopCmd = STOPCMD_UHURA;
            int iLenStop = strlen( szStopCmd );
            
            if( iLenRcv == iLenStop + 2 && strncmp( szBuffer + 1, szStopCmd, iLenStop ) == 0 )
            {
#ifdef DEBUG
               snprintf( szInfo, 120, "discovery stop command from %s received (%s)%s", szSendToIP, szBuffer + 1, HB_EOL );
               hb_conOutErr( szInfo, strlen( szInfo ) );
#endif
               hb_xfree( pSockAddr );
               if( szSendToIP )
                   hb_xfree( szSendToIP );
               //s_bExit = HB_TRUE;
               bExit( HB_TRUE );
               break;
            }
            else
            {
#ifdef DEBUG
               snprintf( szInfo, 120, "no answer to: %s for unknown service request: %s%s",
                         szSendToIP, szBuffer, HB_EOL );
               hb_conOutErr( szInfo, strlen( szInfo ) );
#endif
            }
         }

         hb_xfree( pSockAddr );
         if( szSendToIP )
            hb_xfree( szSendToIP );
      }
   }

   hb_xfree( szBuffer );
   hb_xfree( szServiceName );
   hb_xfree( szTmp );
   hb_xfree( szInfo );

   hb_vmThreadQuit();
   HB_THREAD_END
}

static HB_ULONG srv_SockRecv( HB_SOCKET hSocket, char * pBuf, HB_ULONG ulLen )
{
   HB_ULONG ulRead = 0;
   HB_LONG lTmp;

   while( ulRead < ulLen )
   {
      lTmp = hb_socketRecv( hSocket, pBuf + ulRead, ulLen - ulRead, 0, 1000 );
      if( lTmp <= 0 )
      {
         if( hb_socketGetError() != HB_SOCKET_ERR_TIMEOUT || hb_vmRequestQuery() != 0 )
            break;
      }
      else
         ulRead += lTmp;
   }

   return ulRead;
}

/* the main thread - start UDP thread and wait for end .. */
static HB_BOOL StartServer( void )
{
   s_szServiceNames = hb_parc( 1 );
   s_szAddress = hb_parc( 2 );
   int iLen = hb_parclen( 2 );
   const char * szBroadcast = hb_parc( 3 );

   char * szInfo = ( char * ) hb_xgrab( 120 );

   HB_SOCKET hSocketIn;
   void * pSockAddr;
   unsigned int uiLen;

   HB_SOCKET hSocketMain = HB_NO_SOCKET;          // Initial server socket
   HB_ULONG ulRecvLen;

   //HB_BOOL bExit = HB_TRUE;
   HB_BOOL bError = HB_FALSE;

   char * pBuffer = NULL;
   HB_U32 uiRequest;

   hb_socketInit();

   if( iLen < 1 )
      s_szAddress = NULL;

   if( ( hSocketMain = hb_socketOpen( HB_SOCKET_AF_INET, HB_SOCKET_PT_STREAM, 0 ) ) != HB_NO_SOCKET )
   {
      hb_socketSetKeepAlive( hSocketMain, HB_TRUE );
      //hb_socketSetNoDelay( hSocketMain, HB_TRUE );  // we have a bit time ;-)

      hb_socketInetAddr( &pSockAddr, &uiLen, s_szAddress, SRV_PORT );
      if( hb_socketBind( hSocketMain, pSockAddr, uiLen ) != 0 || hb_socketListen( hSocketMain, 10 ) != 0 )
      {
         hb_socketClose( hSocketMain );
         hSocketMain = HB_NO_SOCKET;
      }
      hb_xfree( pSockAddr );
   }
   if( hSocketMain == HB_NO_SOCKET )
   {
      int iErrorCode = hb_socketGetError();

      snprintf( szInfo, 120, "could not open main socket (err: %d == %s)%s",
                iErrorCode, hb_socketErrorStr( iErrorCode ), HB_EOL );
      hb_conOutErr( szInfo, strlen( szInfo ) );
      hb_xfree( szInfo );

      return HB_FALSE;
   }

   /* UDP service thread start */
   HB_THREAD_ID hThreadID;

   if( ( s_udpSocket = hb_socketOpen( HB_SOCKET_AF_INET, HB_SOCKET_PT_DGRAM, 0 ) ) != HB_NO_SOCKET )
   {
      if( strncmp( szBroadcast, "0.0.0.0", 7 ) == 0 )
         hb_socketInetAddr( &pSockAddr, &uiLen, szBroadcast, UDP_PORT );
      else
/* Linux: socket is bound to broadcast IP, else no bcast packet is received
 * Windows: such throws error: EADDRNOTAVAIL -- here socket is bound to interface IP address */
#if defined( HB_OS_UNIX ) // || defined( HB_OS_LINUX )
         hb_socketInetAddr( &pSockAddr, &uiLen, szBroadcast, UDP_PORT );
#else
         hb_socketInetAddr( &pSockAddr, &uiLen, s_szAddress, UDP_PORT );
#endif

      hb_socketSetBroadcast( s_udpSocket, HB_TRUE );
      if( hb_socketBind( s_udpSocket, pSockAddr, uiLen ) == 0 )
      {
         hb_threadDetach( hb_threadCreate( &hThreadID, udpds, NULL ) );
         //s_bExit = HB_FALSE;
      }
      else
      {
         int iErr = hb_socketGetError();
         char * szIP = hb_socketAddrGetName( pSockAddr, uiLen );

         sprintf( szInfo, "UDP broadcasting socket (%s) NOT bound, err: %s ..%s", szIP, hb_socketErrorStr( iErr ), HB_EOL );
         hb_conOutErr( szInfo, strlen( szInfo ) );
         hb_socketClose( s_udpSocket );
         s_udpSocket = HB_NO_SOCKET;
         bError = HB_TRUE;
         if( szIP )
            hb_xfree( szIP );
         bExit( HB_TRUE );
      }
      hb_xfree( pSockAddr );
   }
   else
   {
      sprintf( szInfo, "no UDP broadcasting socket opened ..%s", HB_EOL );
      hb_conOutErr( szInfo, strlen( szInfo ) );
      bError = HB_TRUE;
   }
   //if( ! s_bExit )
   if( ! bExit( HB_FALSE ) )
   {
      snprintf( szInfo, 120, "UDP service request at: %s:%d now waiting for end ..%s", szBroadcast, UDP_PORT, HB_EOL );
      hb_conOutStd( szInfo, strlen( szInfo ) );
      
      snprintf( szInfo, 120, ".. for end send a stop command to: %s:%d %s", s_szAddress, SRV_PORT, HB_EOL );
      hb_conOutStd( szInfo, strlen( szInfo ) );
   }

   pBuffer = ( char * ) hb_xgrab( MSGSIZE_LEN * 2 );

   /*  main thread loop */
   //while( ! s_bExit )
   while( ! bExit( HB_FALSE ) )
   {
      hSocketIn = hb_socketAccept( hSocketMain, &pSockAddr, &uiLen, UDP_TIMEOUT );

      if( hSocketIn != HB_NO_SOCKET )
      {
         hb_xfree( pSockAddr );
         ulRecvLen = srv_SockRecv( hSocketIn, pBuffer, MSGSIZE_LEN );
         hb_socketClose( hSocketIn );
         if( ulRecvLen != MSGSIZE_LEN )
         {
            if( s_iErrorCode && s_iErrorCode != HB_SOCKET_ERR_CONNRESET )
            {
               snprintf( szInfo, 120, "wrong command received: %d <> %lu (err: %d == %s)%s",
                         MSGSIZE_LEN * 3, ulRecvLen, s_iErrorCode, hb_socketErrorStr( s_iErrorCode ), HB_EOL );
               hb_conOutErr( szInfo, strlen( szInfo ) );
            }
            if( s_iErrorCode != HB_SOCKET_ERR_TIMEOUT )
               break;
            else
            {
               //hb_socketClose( hSocketIn );
               continue;
            }
          }

          //hb_socketClose( hSocketIn );

          uiRequest = HB_GET_LE_UINT32( pBuffer );
          if( uiRequest == 42 )
          {
            //s_bExit = HB_TRUE;
            bExit( HB_TRUE );
            break;
          }
      }
      else if( hb_socketGetError() != HB_SOCKET_ERR_TIMEOUT )
         break;
   }

   if( hSocketMain != HB_NO_SOCKET )
      hb_socketClose( hSocketMain );

   if( s_udpSocket != HB_NO_SOCKET )
   {
      hb_socketClose( s_udpSocket );
      hb_idleSleep( ( UDP_TIMEOUT / 1000 ) + .5 );  /* wait for UDP thread to close */
   }

   hb_xfree( pBuffer );
   hb_xfree( szInfo );

   hb_socketCleanup();

   return  ( ! bError );
}

HB_FUNC( STARTSERVER )
{
   hb_retl( StartServer() );
}

#pragma ENDDUMP

