/* hbmk2 plugin to determine lastest filedate for LetoDBf server/ RDD sources */

#define REVISION_FILE   "../include/leto_rev.ch"
#define _EOL_           CHR( 10 )
#if defined( HB_OS_WIN )
   #define _FILE_MASK_  "*.*"
#else
   #define _FILE_MASK_  "*"
#endif

FUNCTION hbmk_plugin_rand( hbmk )

   IF VALTYPE( hbmk[ "cOUTPUTNAME" ] ) == "C" .AND. "bug_info" $ hbmk[ "cOUTPUTNAME" ]
      IF hbmk[ "cSTATE" ] == "pre_all"
         VersionString( .T. )
         VersionString( .F. )
      ENDIF
   ENDIF

RETURN NIL

STATIC FUNCTION VersionString( lServer )
   LOCAL dDate := DATE() - 36525
   LOCAL cTime := "00:00:00"
   LOCAL aDir, i, cTmp, cResult

   IF lServer
      aDir := Directory( "../source/server/" + _FILE_MASK_ )
   ELSE
      aDir := Directory( "../source/client/" + _FILE_MASK_ )
   ENDIF
   FOR i := 1 TO LEN( aDir )
      IF aDir[ i ][ 3 ] > dDate
         dDate := aDir[ i ][ 3 ]
         IF aDir[ i ][ 4 ] > cTime
            cTime := aDir[ i ][ 4 ]
         ENDIF
      ENDIF
   NEXT i

   aDir := Directory( "../source/common/" + _FILE_MASK_ )
   FOR i := 1 TO LEN( aDir )
      IF aDir[ i ][ 3 ] > dDate
         dDate := aDir[ i ][ 3 ]
         IF aDir[ i ][ 4 ] > cTime
            cTime := aDir[ i ][ 4 ]
         ENDIF
      ENDIF
   NEXT i

   IF ! EMPTY( dDate )
      IF FILE( REVISION_FILE )
         cTmp := MemoRead( REVISION_FILE )
         IF lServer
            i := AT( "#define __RDD_REVISION__", cTmp )
         ELSE
            i := AT( "#define __SRV_REVISION__", cTmp )
         ENDIF
         IF i > 0  /* fetch the other setting */
            cTmp := SUBSTR( cTmp, i )
            cTmp := LEFT( cTmp, AT( _EOL_, cTmp ) - 1 ) + _EOL_
         ENDIF
      ELSE
         cTmp := ""
      ENDIF

      cResult := "/* do not edit --- automated created file */" + _EOL_ + cTmp
      IF lServer
         cResult += "#define __SRV_REVISION__ "
      ELSE
         cResult += "#define __RDD_REVISION__ "
      ENDIF
      cResult += CHR( 34 ) + DTOS( dDate ) + " " + cTime + CHR( 34 ) + _EOL_

      MemoWrit( REVISION_FILE, cResult )

   ENDIF

RETURN cResult

