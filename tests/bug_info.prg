/*
 * bug_info[.exe] [ IP|name ] [ logfile ]
 *
 * Support function to report a bug:
 * function try to retrieve info from server <IP|name> and client
 * to only display it or to write this info into file <logfile>.
 * If file <logfile> exists, this info will be added at front of it.
 */

#include "leto_rev.ch"

REQUEST LETO


PROCEDURE main( cPar1, cPar2 )
   AltD()
   LETO_BUGREPORT( cPar1, cPar2 )
RETURN

FUNCTION LETO_BUGREPORT( cSrvAddr, cLogFile )
   LOCAL nTmp := LETO_CONNECT()
   LOCAL cInfo := ""
   LOCAL cTmp

#ifndef __XHARBOUR__   /* there is no useable LETO_UDF :-( */
   IF nTmp < 0 .AND. ! EMPTY( cSrvAddr )
      nTmp := LETO_CONNECT( cSrvAddr )
   ENDIF
#endif

   IF nTmp >= 0
      cTmp := leto_udf( "OS" )
   ELSE
      cInfo += "Server revision: " + __SRV_REVISION__ + HB_EOL() + HB_EOL()
   ENDIF
   IF ! EMPTY( cTmp ) .AND. VALTYPE( cTmp ) == "C"
      cInfo := "Server revision: " + __SRV_REVISION__ + HB_EOL() + cTmp + HB_EOL()

      cTmp := leto_udf( "VERSION" )        /* Harbour version */
      IF ! EMPTY( cTmp ) .AND. VALTYPE( cTmp ) == "C"
         cInfo += cTmp + HB_EOL()
      ENDIF
      cTmp := leto_udf( "HB_VERSION", 1 )  /* C-compiler aka HB_VERSION_COMPILER */
      IF ! EMPTY( cTmp ) .AND. VALTYPE( cTmp ) == "C"
         cInfo += cTmp + HB_EOL()
      ENDIF
      cInfo += HB_EOL()
   ENDIF

   cInfo += "Client revision: " + __RDD_REVISION__ + HB_EOL() + OS() + HB_EOL()
   cInfo += VERSION() + HB_EOL()
   cTmp := HB_VERSION( 1 )   /* C-compiler aka HB_VERSION_COMPILER */
   IF ! EMPTY( cTmp ) .AND. VALTYPE( cTmp ) == "C"
      cInfo += cTmp + HB_EOL()
   ENDIF
   cInfo += HB_EOL()

   IF ! EMPTY( cLogFile ) .AND. VALTYPE( cLogFile ) == "C"
      IF FILE( cLogFile )
         MEMOWRIT( cLogFile, cInfo + MEMOREAD( cLogFile )  )
      ELSE
         MEMOWRIT( cLogFile, cInfo )
      ENDIF
   ELSE
      ALERT( cInfo )
   ENDIF

RETURN cInfo
