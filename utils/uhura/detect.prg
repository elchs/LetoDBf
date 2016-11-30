#include "hbsocket.ch"
/*
 * 4221 == free port
 * http://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml
 */
#define UDP_PORT  4221

REQUEST LETO

/* by using 'letodb.hbc', example:  hbmk2 basic.prg letodb.hbc
 * this will automatic include rdddleto.ch */
#include "rddleto.ch"

PROCEDURE main( cAddress )  /* pure IP-address */
ALTD()
   IF Empty( cAddress )
      cAddress := detectServer( "letodb" )
      cAddress := "//" + cAddress + ":2812/"
   ELSE
      cAddress := "//" + cAddress + IIF( ":" $ cAddress, "", ":2812" )
      cAddress += IIF( Right( cAddress, 1 ) == "/", "", "/" )
   ENDIF

   IF leto_Connect( cAddress ) < 0
      ALERT( "NO LETODB SERVER FOUND - ERROR: " + leto_Connect_Err( .T. ) )
      QUIT
   ELSE
      // RDDSETDEFAULT( "LETO" )  /* is automatic set by: REQUEST LETO */
      // LETO_DBDRIVER( "DBFCDX" )   /* to choose your DBF driver independent of the server default */ 
      LETO_TOGGLEZIP( 1 )         /* for compressed traffic */
   ENDIF

   // DbUseArea( .T., /* LETO */, "test", "TEST", .T. )
   // ...

   /* no special logout needed, all is automically done */
RETURN


*/ --- helper functions --- */

/* nNrOfPossible: optional, if a service is available at multiple interface/ server, take the n-th' */
FUNC detectServer( cService, nNrOfPossible )
 LOCAL cIP := "", aIP
 LOCAL cBroadcastIP
 LOCAL nIFace
 LOCAL aIFace := hb_socketGetIFaces( HB_SOCKET_AF_INET, .T. )
 LOCAL aBroadCasted := {}
 LOCAL nFound := 0
 
  hb_default( @nNrOfPossible, 1 )

  IF VALTYPE( aIFace ) == "A" .AND. LEN( aIFace ) > 0
    nIFace := 1
    DO WHILE nIFACE <= LEN( aIFace )
      /* outcomment last two conditions to use also interfaces without MAC address (e.g. loopback ) */
      IF ! EMPTY( aIFace[ nIFace, HB_SOCKET_IFINFO_ADDR ] ) .AND.;
           ! EMPTY( aIFace[ nIFace, HB_SOCKET_IFINFO_NETMASK ] ) .AND.;
             ! EMPTY( aIFace[ nIFace, HB_SOCKET_IFINFO_HWADDR ] ) .AND.;
               ! aIFace[ nIFace, HB_SOCKET_IFINFO_HWADDR ] == "00:00:00:00:00:00"
          
        cBroadcastIP := broadcastIP( aIFace[ nIFace, HB_SOCKET_IFINFO_ADDR ],;
                                     aIFace[ nIFace, HB_SOCKET_IFINFO_NETMASK ] )
        /* broadcast a identical subnet only one time */                             
        IF ASCAN( aBroadCasted, cBroadcastIP ) < 1
          AADD( aBroadcasted, cBroadcastIP )
          aIP := hb_udpds_Request( UDP_PORT, cService, cBroadcastIP, nNrOfPossible < 2 )
          IF LEN( aIP ) + nFound >= nNrOfPossible
            cIP := aIP[ nNrOfPossible - nFound ]
            EXIT
          ELSE
            nFound += LEN( aIP )
          ENDIF
        ENDIF

      ENDIF
      nIFace++
    ENDDO
  ENDIF
RETURN cIP


/* translate address with netmask to a valid broadcast IP4: broadcast = ip | ( ~ netmask ) */
FUNC broadcastIP( cAddr, cNetm )
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
RETU cBroadcast


/* validate IP: only digits, at least one digit between/ after dots, and 3 dots */
FUNCTION isValidIP4( cAddr )
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


/* send the broadcast packet and analyse possible answer to catch IP */
FUNCTION hb_udpds_Request( nPort, cService, cBroadcastIP, lOnlyFirstAnswer )
 LOCAL hSocket, cBuffer, nLen, aAddr
 LOCAL cFoundIP// := ""
 LOCAL aFoundIP := {}
 
  hb_default( @lOnlyFirstAnswer, .T. )

  IF ! Empty( hSocket := hb_socketOpen( HB_SOCKET_AF_INET, HB_SOCKET_PT_DGRAM ) )
    hb_socketSetBroadcast( hSocket, .T. )
    cService := hb_StrToUTF8( cService )
    IF hb_socketSendTo( hSocket, hb_BChar( 5 ) + cService + hb_BChar( 0 ), , , { HB_SOCKET_AF_INET, cBroadcastIP, nPort } ) == hb_BLen( cService ) + 2
      cBuffer := Space( 2000 )
      DO WHILE .T.
        nLen := hb_socketRecvFrom( hSocket, @cBuffer, , , @aAddr, 100 )  /* timeout 0.1 s enough ? */
        IF nLen > 0
          IF hb_BLeft( cBuffer, hb_BLen( cService ) + 2 ) == hb_BChar( 6 ) + cService + hb_BChar( 0 )
            /* an IP given in content ? --> if a valid IP4, prefer that ! */
            IF nLen >= hb_BLen( cService ) + 2 + 7  /* cService + 2 + min: "1.1.1.1" */
              cFoundIP := hb_BSubStr( cBuffer, hb_BLen( cService ) + 3, nLen - hb_BLen( cService ) - 2 )
              IF ! isvalidIP4( cFoundIP ) 
                cFoundIP := aAddr[ 2 ]
              ENDIF
              AADD( aFoundIP, cFoundIP )
              IF lOnlyFirstAnswer
                EXIT
              ENDIF
            ELSE
              AADD( aFoundIP, aAddr[ 2 ] )
              IF lOnlyFirstAnswer
                EXIT
              ENDIF
            ENDIF
          ENDIF
        ELSE
          EXIT
        ENDIF
      ENDDO
    ENDIF
    hb_socketClose( hSocket )
  ENDIF

RETURN aFoundIP