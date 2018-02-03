/*
 * this connect to a LetoDBf server and query for opened DBFs,
 * to compare if the DBF given by param is in the list and exclusive opened.
 * Address:port to connect is read of an .ini, below used with LETO_SMBSERVER()
 * --> set the OS ErrorLevel as return value
 *     0 means OK, 1 == fail, your <shared> open table is in <exclusive> usage.
 *
 * Most basic usage:
 * IF DbUseArea( ,, cFile,, lShared )
 *    IF lShared .AND. hb_ProcessRun( THIS_EXECUTABLE + " " + cFile,,,, .F. ) == 1
 *      DbCloseArea()
 *      NetErr( .T. )
 *    ENDIF
 * ENDIF
/*


/* --- no changes need below  - 2018 Rolf 'elch' Beckmann --- */

ANNOUNCE HB_GTSYS
REQUEST HB_GT_NUL_DEFAULT

PROCEDURE main( cFile )
   LOCAL nErrorLevel := 0
   LOCAL i, aArr, nLen, nTmp

   IF ! EMPTY( cFile ) .AND. Leto_Connect( LETO_SMBSERVER() ) >= 0

      /* beautify path and file */
      IF SUBSTR( cFile, 2, 1 ) == ":"
         cFile := SUBSTR( cFile, 3 )
      ENDIF
      IF LEFT( cFile, 1 ) == "."
         IF SUBSTR( cFile, 2, 1 ) == "."   /* a hiding elk  ".."  :-) */
            cFile := SUBSTR( cFile, 3 )
         ELSE
            cFile := SUBSTR( cFile, 2 )
         ENDIF
      ENDIF
      i := AT( "\", cFile )
      DO WHILE i > 0
         cFile := LEFT( cFile, i - 1 ) + "/" + SUBSTR( cFile, i + 1 )
         i := AT( "\", cFile )
      ENDDO
      DO WHILE LEFT( cFile, 1 ) == "/"
         cFile := SUBSTR( cFile, 2 )
      ENDDO
      cFile := LOWER( cFile )

      /* ask for the DataPath to (partly) extract of filename */
      aArr := Leto_MgGetInfo()
      IF VALTYPE( aArr ) == "A" .AND. LEN( aArr ) >= 11
         nTmp := 1
         DO WHILE ( i := AT( "/", SUBSTR( aArr[ 11 ], nTmp + 1 ) ) ) > 0
            nTmp += i
            nLen := LEN( SUBSTR( aArr[ 11 ], nTmp + 1 ) )
            IF LOWER( SUBSTR( aArr[ 11 ], nTmp + 1 ) ) == LEFT( cFile, nLen )
               cFile := SUBSTR( cFile, nLen + 2 )
               EXIT
            ENDIF
         ENDDO
      ENDIF

      /* ask the server for open files */
      aArr := leto_MgGetTables( -1 )
      IF VALTYPE( aArr ) == "A"
         i := 0
         DO WHILE i++ < LEN( aArr )
            IF cFile $ LOWER( aArr[ i, 2 ] )
               IF ! aArr[ i, 5 ]   /* exclusive */
                  nErrorLevel := 1
               ENDIF
               EXIT
            ENDIF
         ENDDO
      ENDIF

      Leto_Disconnect()
   ENDIF

   ERRORLEVEL( nErrorlevel )
RETURN
