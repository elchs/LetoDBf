/*
 * bug_info[.exe] [ IP|name ] [ logfile ]
 *
 * Support function to report a bug:
 * function try to retrieve info from server <IP|name> and client
 * to only display it or to write this info into file <logfile>.
 * If file <logfile> exists, this info will be added at front of it.
 */

#include "leto_rev.ch"
#include "rddleto.ch"

REQUEST LETO


PROCEDURE main( cPar1, cPar2, cPar3 )
   AltD()
   LETO_BUGREPORT( cPar1, cPar2, cPar3 )
RETURN

FUNCTION LETO_BUGREPORT( cSrvAddr, cLogFile, cOptions )
   LOCAL nConnect := LETO_CONNECT()
   LOCAL cInfo := ""
   LOCAL cTmp, nTmp

#ifndef __XHARBOUR__   /* there is no useable LETO_UDF :-( */
   IF nConnect < 0 .AND. ! EMPTY( cSrvAddr )
      nConnect := LETO_CONNECT( cSrvAddr )
   ENDIF
#endif

   IF nConnect >= 0
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

   IF nConnect >= 0 .AND. ! EMPTY( cOptions )
      IF VAL( cOptions ) >=0 .AND. LEFT( cOptions, 1 ) $ "0123456789"
         nTmp := VAL( cOptions )
         IF leto_mgID() != nTmp  /* ele we have just overwritten the log */
            cTmp := leto_MgLog( nConnect, nTmp )
         ELSE
            cTmp := ""
         ENDIF
      ELSE
         nTmp := -1
         cTmp := leto_MgLog( nConnect, -1 )
      ENDIF
      IF ! EMPTY( cTmp )
         cInfo += "- -[ " + STR( nTmp, 4, 0 ) + " ]" + REPL( "- ", 15 )
         cInfo +=  + HB_EOL() + cTmp + REPL( "- ", 42 ) + HB_EOL()
      ENDIF
   ENDIF

   cInfo += HB_EOL()

   IF ! EMPTY( cLogFile ) .AND. VALTYPE( cLogFile ) == "C"
      IF FILE( cLogFile )
         IF ".gz" $ LOWER( cLogFile )
            cTmp := HB_ZUNCOMPRESS( MEMOREAD( cLogFile ) )
         ELSE
            cTmp := MEMOREAD( cLogFile )
         ENDIF
         cInfo += cTmp
         IF ".gz" $ LOWER( cLogFile )
            cTmp := HB_GZCOMPRESS( cInfo )
            IF ! EMPTY( cTmp )
               MEMOWRIT( cLogFile, cTmp )
            ELSE
               MEMOWRIT( cLogFile + ".txt", cInfo )
            ENDIF
         ELSE
            MEMOWRIT( cLogFile, cInfo + MEMOREAD( cLogFile )  )
         ENDIF
      ELSE
         IF ".gz" $ LOWER( cLogFile )
            cTmp := HB_GZCOMPRESS( cInfo )
            IF ! EMPTY( cTmp )
               MEMOWRIT( cLogFile, cTmp )
            ELSE
               MEMOWRIT( cLogFile + ".txt", cInfo )
            ENDIF
         ELSE
            MEMOWRIT( cLogFile, cInfo )
         ENDIF
      ENDIF
   ELSE
      ALERT( cInfo )
   ENDIF

RETURN cInfo
