/*
 * RDD initialization
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 *           2017 Rolf 'elch' Beckmann; mostly all about auto_connecting
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

/* Standard Harbour RDDSys system */

ANNOUNCE RDDLETO

REQUEST LETORDD
REQUEST LETO_UDF
#ifndef USE_LZ4
   REQUEST HB_ZCOMPRESS
#endif

/* leto_std.ch */
REQUEST HB_DBCREATETEMP, __DBTOTAL
/* for LETO_DBEVAL */
REQUEST LETO_VARSET, LETO_VARGET, LETO_VARINCR
REQUEST FIELDGET, FIELDPOS, FIELDPUT, DBDELETE, DBRECALL
REQUEST LETO_RECLOCK, LETO_RECUNLOCK, LETO_TABLELOCK, LETO_TABLEUNLOCK

/* used in LETO_SETLOCKTIMEOUT */
REQUEST RDDINFO


#include "hbsocket.ch"
#include "rddleto.ch"

#ifdef __XHARBOUR__
   #include "hbcompat.ch"
   #define hb_BChar( x )          CHR( x )
   #define hb_BLeft( text, len )  LEFT( text, len )

   PROCEDURE hb_default( xRefPar, xDefault )
      IF VALTYPE( xRefPar ) != VALTYPE( xDefault )
	     xRefPar := xDefault
	  ENDIF
   RETURN
#endif

/* place holder to activate init functions here and in leto1.c */
PROCEDURE LETO

   rddSetDefault( "LETO" )

   RETURN


#ifndef LETO_NO_THREAD
INIT PROCEDURE LETO_INITERROR

#ifndef __XHARBOUR__
   IF hb_mtVM()
#else
   IF .T.
#endif
      hb_idleAdd( {|| leto_lookError() } )
   ENDIF

   RETURN
#endif

#ifndef __XHARBOUR__
INIT PROCEDURE LETO_CONNECTAUTO
   LOCAL hIni, nConnection, cServer, cService
   LOCAL cUser, cPW, nTimeOut, nBufRefr
   LOCAL cAppIni
   LOCAL nPort

   hb_FNameSplit( hb_ProgName(), /* @cPath */, @cAppIni, /* cExt */ )
   cAppIni := hb_FNameMerge( /* cPath */, cAppIni, ".ini" )
   IF ! FILE( cAppIni )
      cAppIni := "rddleto.ini"
   ENDIF

   IF FILE( cAppIni )
      hIni := hb_HGetDef( hb_iniRead( cAppIni, .F. ), "MAIN", { => } )
      cServer := hb_HGetDef( hIni, "SERVER", "" )

      IF UPPER( LEFT( cServer, 6 ) ) == "DETECT"
         IF LEN( cServer ) > 8 .AND. SUBSTR( cServer, 7, 1 ) == ":"
            cService := SUBSTR( cServer, 8 )
         ENDIF
         cServer := Leto_Detect( cService )  /* default 'letodb' */
      ENDIF
      IF ! EMPTY( cServer )
         /* user name can be used without PW */
         cUser       := hb_HGetDef( hIni, "USER", NIL )
         /* ToDO: interactive password entrance forced by e.g. "!",
          * makes sense only with LetoDBf authentication activated */
         cPW         := NIL

         IF hb_HPos( hIni, "TIMEOUT" ) > 0
            nTimeOut    := VAL( hb_HGet( hIni, "TIMEOUT" ) )
         ENDIF
         IF hb_HPos( hIni, "CACHE_TIMEOUT" ) > 0
            nBufRefr  := VAL( hb_HGet( hIni, "CACHE_TIMEOUT" ) )
         ENDIF

         IF hb_HPos( hIni, "DATE_FORMAT" ) > 0
            Set( _SET_DATEFORMAT, hb_HGet( hIni, "DATE_FORMAT" ) )
         ENDIF
         IF hb_HPos( hIni, "PATH_DEFAULT" ) > 0
            Set( _SET_DEFAULT, hb_HGet( hIni, "PATH_DEFAULT" ) )
         ENDIF
         IF hb_HPos( hIni, "PATH_SEARCH" ) > 0
            Set( _SET_PATH, hb_HGet( hIni, "PATH_SEARCH" ) )
         ENDIF

         OutStd( "connecting to LetoDBf " + cServer + " ..." )
         nConnection := Leto_Connect( cServer, cUser, cPW, nTimeOut, nBufRefr )
         IF nConnection < 0
            OutErr( "LetoDBf connect failed of: " + leto_Connect_Err( .T. ) + HB_EOL() )
            QUIT
         ENDIF
      ELSE
         nConnection := -1
      ENDIF

      cServer := hb_HGetDef( hIni, "SMB_SERVER", "" )
      IF ! EMPTY( cServer )
         nPort := VAL( hb_HGetDef( hIni, "SMB_PORT", "2814" ) )
         Leto_SMBServer( cServer, nPort, nConnection )
      ENDIF
   ENDIF

   RETURN
#endif

FUNCTION Leto_Detect( cService, nNrOfPossible, nPort )
   LOCAL cIP := "", aIP
   LOCAL cBroadcastIP
   LOCAL nIFace
   LOCAL aIFace := hb_socketGetIFaces( HB_SOCKET_AF_INET, .T. )
   LOCAL aBroadCasted := {}
   LOCAL nFound := 0

   hb_default( @cService, "letodb" )        /* "keyword" the wanted server configured to be responsible */
   hb_default( @nNrOfPossible, 1 )          /* if multiple server found in a network, take the n-TH */
   hb_default( @nPort, LETO_DEFAULT_PORT )  /* port to send the broadcast LETO_DEFAULT_PORT */

   IF VALTYPE( aIFace ) == "A" .AND. LEN( aIFace ) > 0
      nIFace := 1
      DO WHILE nIFACE <= LEN( aIFace )
         /* outcomment last two conditions to use also interfaces without MAC address (e.g. loopback ) */
#ifndef __XHARBOUR__
          IF ! EMPTY( aIFace[ nIFace, HB_SOCKET_IFINFO_ADDR ] ) .AND.;
               ! EMPTY( aIFace[ nIFace, HB_SOCKET_IFINFO_NETMASK ] ) .AND.;
                 ! EMPTY( aIFace[ nIFace, HB_SOCKET_IFINFO_HWADDR ] ) .AND.;
                   ! aIFace[ nIFace, HB_SOCKET_IFINFO_HWADDR ] == "00:00:00:00:00:00"
#else  /* misses the MAC address */
         IF ! EMPTY( aIFace[ nIFace, HB_SOCKET_IFINFO_ADDR ] ) .AND.;
              ! EMPTY( aIFace[ nIFace, HB_SOCKET_IFINFO_NETMASK ] ) .AND.;
			    LEFT( aIFace[ nIFace, HB_SOCKET_IFINFO_ADDR ], 5 ) != "127.0"
#endif

           cBroadcastIP := leto_broadcastIP( aIFace[ nIFace, HB_SOCKET_IFINFO_ADDR ],;
                                        aIFace[ nIFace, HB_SOCKET_IFINFO_NETMASK ] )
           /* broadcast a identical subnet only one time */
           IF ASCAN( aBroadCasted, cBroadcastIP ) < 1
              AADD( aBroadcasted, cBroadcastIP )
              aIP := leto_BCRequest( nPort, cService, cBroadcastIP, nNrOfPossible < 2 )
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

/* send the broadcast packet and analyse possible answer to catch IP */
STATIC FUNCTION leto_BCRequest( nPort, cService, cBroadcastIP, lOnlyFirstAnswer )
   LOCAL hSocket, cBuffer, nLen, aAddr
   LOCAL cFoundIP, aFoundIP := {}
   LOCAL nPos

   hb_default( @lOnlyFirstAnswer, .T. )

   IF ! Empty( hSocket := hb_socketOpen( HB_SOCKET_AF_INET, HB_SOCKET_PT_DGRAM ) )
      hb_socketSetBroadcast( hSocket, .T. )
      cService := hb_StrToUTF8( cService )
      IF hb_socketSendTo( hSocket, hb_BChar( 5 ) + cService + hb_BChar( 0 ),,,;
                          { HB_SOCKET_AF_INET, cBroadcastIP, nPort } ) == hb_BLen( cService ) + 2
         cBuffer := Space( 2000 )
         DO WHILE .T.
            nLen := hb_socketRecvFrom( hSocket, @cBuffer, , , @aAddr, 250 )  /* timeout 0.25 s enough ? */
            IF nLen > 0
               IF hb_BLeft( cBuffer, hb_BLen( cService ) + 2 ) == hb_BChar( 6 ) + cService + hb_BChar( 0 )
                  /* an IP given in content ? --> if a valid IP4, prefer that ! */
                  IF nLen >= hb_BLen( cService ) + 4  /* cService + 2 + min: ":x" */
                     cFoundIP := hb_BSubStr( cBuffer, hb_BLen( cService ) + 3, nLen - hb_BLen( cService ) - 2 )
                     IF ! leto_isvalidIP4( cFoundIP ) .AND. EMPTY( hb_socketResolveAddr( cFoundIP ) )
                        nPos := AT( ":", cFoundIP )
                        IF nPos < 3 .OR. ( ! leto_isvalidIP4( LEFT( cFoundIP, nPos - 1 ) ) .AND.;
                                           EMPTY( hb_socketResolveAddr( LEFT( cFoundIP, nPos - 1 ) ) ) )
                           /* no IP/DNS in response --> IP of sender plus opt. port-nr in response */
                           cFoundIP := aAddr[ 2 ] + IIF( nPos > 0, SUBSTR( cFoundIP, nPos ), "" )
                        ENDIF
                     ENDIF
                     AADD( aFoundIP, cFoundIP )
                     IF lOnlyFirstAnswer
                        EXIT
                     ENDIF
                  ELSE
                     IF VALTYPE( aAddr ) == "A"
                        AADD( aFoundIP, aAddr[ 2 ] )
                        IF lOnlyFirstAnswer
                          EXIT
                        ENDIF
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

