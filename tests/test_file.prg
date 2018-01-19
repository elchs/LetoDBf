/*
 * This sample demonstrates how to use file functions with Leto db server
 * EnableFileFunc = 1 must be set in server's letodb.ini
 */
REQUEST LETO
#include "rddleto.ch"
#include "fileio.ch"

Function Main( cPath )
 LOCAL cBuf, arr, i, lTmp, nTmp, nHandle
 LOCAL nPort := 2812

   ALTD()
   IF Empty( cPath )
      cPath := "//127.0.0.1:2812/"
   ELSE
      cPath := "//" + cPath + IiF( ":" $ cPath, "", ":" + ALLTRIM( STR( nPort ) ) )
      cPath += Iif( Right(cPath,1) == "/", "", "/" )
   ENDIF

   ? "Hey, LetoDBf:", Leto_Ping()

   ? "Connect to " + cPath + " - "
   IF ( leto_Connect( cPath ) ) == -1
      ? "Error connecting to server:", leto_Connect_Err(), leto_Connect_Err( .T. )
      WAIT
      Return Nil
   ELSE
      ?? "Ok"
   ENDIF

   ? "Hey, LetoDBf:", Leto_Ping()

   /* No more cPath needed after Leto_connect() */

   ? 'leto_file( "test1.txt" ) - '
   lTmp := leto_file( "test1.txt" )
   ?? Iif( lTmp, "Ok", "No" )
   i := Leto_fError()
   ? 'file functions working ? - ' + Iif( i == 100, "disabled",;
      Iif( i == IIF( lTmp, 0, 2 ), "well well", "ups, error: " + STR( leto_fError(), 5, 0 ) ) )

   ? 'leto_memowrite( "test1.txt", "A test N1" ) - '
   ?? Iif( leto_memowrite( "test1.txt", "A test N1" ), "Ok", "Failure" )

   ? 'leto_file( "test1.txt" ) - '
   ?? Iif( leto_file( "test1.txt" ), "Ok", "No" )

   ? 'leto_memoread( "test1.txt" ) - '
   ?? leto_memoread( "test1.txt" )

   ? 'leto_frename( "test1.txt", "test2.txt" ) - '
   ?? Iif( leto_frename( "test1.txt","test2.txt" ) == 0, "Ok", "Failure" )
   ?? Iif( ! leto_file( "test1.txt" ), "!", " fail" )
   ?? Iif( leto_file( "test2.txt" ), "!", " fail" )

   ? 'leto_fcopy( "test2.txt", "test1.txt" ) - '
   ?? Iif( leto_fcopy( "test2.txt", "test1.txt" ) == 0, "Ok", "Failure" )
   ?? Iif( leto_file( "test1.txt" ), "!", " fail" )

   ? 'leto_fileread( "test2.txt", 7, 2 ) - '
   ?? Iif( leto_fileread( "test2.txt", 7, 2, @cBuf ) > 0, "'" + cBuf + "'", "Failure" )
   ?? Iif( cBuf == "N1", " - Ok", " - Failure" )

   ? 'leto_filewrite( "test2.txt", 7, "N2" ) - '
   ?? Iif( leto_filewrite( "test2.txt", 7, "N2" ), "Ok", "Failure" )

   ? 'leto_memoread( "test2.txt" ) - '
   cBuf := leto_memoread( "test2.txt" )
   ?? "'" + cBuf + "' - "
   ?? Iif( cBuf == "A test N2", "Ok", "Failure" )

   ? 'leto_filesize( "test2.txt" ) - '
   ?? leto_filesize( "test2.txt" )

   ? 'leto_filewrite( "test2.txt", 0, 2048 * "A" ) - '
   ?? Iif( leto_filewrite( "test2.txt", 0, REPLICATE( "A", 2048 ) ), "Ok", "Failure" )

   ? 'leto_filesize( "test2.txt" ) - '
   ?? leto_filesize( "test2.txt" )
   leto_fileread( "test2.txt", 0, 0, @cBuf )
   IF cBuf == REPLICATE( "A", 2048 )
      ?? " fine"
   ENDIF

#ifndef __XHARBOUR__
   ? 'leto_fCopyFromSrv( "test3.txt", "test2.txt" ) - '
   lTmp := leto_fCopyFromSrv( "test3.txt", "test2.txt", 1000 )
   ?? Iif( lTmp, "Ok", "Failure" )
   ?? Iif( FILE( "test3.txt" ), "!", "@" )
   IF ! lTmp
      ?? FError(), Leto_FError( .F. ), Leto_Ferror( .T. )
   ENDIF

   MemoWrit( "test3.txt", Replicate( "z", 123456 ) )
   ? 'leto_fCopyToSrv( "test3.txt", "test2.txt" ) - '
   lTmp := leto_fCopyToSrv( "test3.txt", "test2.txt", 1000 )
   ?? Iif( lTmp, "Ok", "Failure" )
   ?? Iif( leto_filesize( "test2.txt" ) == 123457, "!", "@" )  /* +1 for strg-z */
   IF ! lTmp
      ?? FError(), Leto_FError( .F. ), Leto_Ferror( .T. )
   ENDIF
   FErase( "test3.txt" )
#endif

   ? 'leto_memowrite( "test2.txt", 4095 * "B" ) - '
   ?? Iif( leto_memowrite( "test2.txt", REPLICATE( "B", 4095 ) ), "Ok", "Failure" )
   ?? Iif( leto_filesize( "test2.txt" ) == 4096, "!", "@" )  /* +1 for strg-z */
   ? 'leto_filesize( "test2.txt" ) - '
   ?? leto_filesize( "test2.txt" )
   cBuf := leto_memoread( "test2.txt" )
   IF cBuf == REPLICATE( "B", 4095 )
      ?? " fine"
   else
      ?? " wrong"
   ENDIF

#ifndef __XHARBOUR__
   ? 'leto_FCreate( "test3.txt" ) - '
   nHandle := leto_FCreate( "test3.txt" )
   ?? Iif( nHandle >= 0, "Ok", "Failure -- no further tests leto_F*() test" )
   IF nHandle >= 0
      ? "Press any key to continue..."
      Inkey( 0 )
      ? 'leto_FClose( nHandle ) - '
      ?? IiF( leto_FClose( nHandle ),  "Ok", "Failure" )
      ?? IiF( ! leto_FClose( nHandle ),  "!", "Fail" )
      ? 'leto_FOpen( "test3.txt", READWRITE ) - '
      nHandle := leto_FOpen( "test3.txt", FO_READWRITE )
      ?? Iif( nHandle >= 0, "Ok", "Failure" )
      IF nHandle >= 0
         ? 'leto_FWrite( nHandle, "testx12" ) - '
         nTmp := leto_FWrite( nHandle, "testx21" )
         ?? Iif( nTmp == 7, "Ok", "Failure" )
         ? 'leto_FSeek( nHandle, 4, 0 ) - '
         nTmp := leto_FSeek( nHandle, 4, 0 )
         ?? Iif( nTmp == 4, "Ok", "Failure" )
         leto_FWrite( nHandle, "3" )
         ? 'leto_FSeek( nHandle, 0, 2 ) - '
         nTmp := leto_FSeek( nHandle, 0, 2 )
         ?? Iif( nTmp == 7, "Ok", "Failure" )
         ?? " --- EOF - " + IiF( LETO_FEOF( nHandle ), "Ok", "Failure" )
         leto_FWrite( nHandle, "0" + CHR( 0 ) )
         cBuf := SPACE( 12 )
         leto_FSeek( nHandle, 0, 0 )
         ? 'leto_FRead( nHandle, @cBuf, 10 ) - '
         nTmp := leto_FRead( nHandle, @cBuf, 10 )
         ?? Iif( nTmp == 9, "Ok", "Failure" )
         ?? Iif( LEN( cBuf ) == 12 .AND. LEFT( cBuf, 9 ) == "test3210" + CHR( 0 ), " !!", "" )
         leto_FSeek( nHandle, 0, 0 )
         ? 'leto_FReadSTR( nHandle, 10 ) - '
         cBuf := leto_FReadStr( nHandle, 10 )
         ?? Iif( LEN( cBuf ) == 8 .AND. LEFT( cBuf, 8 ) == "test3210", "Ok", "Failure" )
         leto_FSeek( nHandle, 0, 0 )
         ? 'leto_FReadLEN( nHandle, 10 ) - '
         cBuf := leto_FReadLen( nHandle, 10 )
         ?? Iif( LEN( cBuf ) == 9 .AND. LEFT( cBuf, 9 ) == "test3210" + CHR( 0 ), "Ok", "Failure" )
         leto_FClose( nHandle )
         ?
      ENDIF
      ? "Press any key to continue..."
      Inkey( 0 )
      leto_FErase( "test3.txt" )
   ENDIF
#endif

   ? 'leto_DirMake( "TEST" ) - '
   ?? Iif( leto_DirMake( "TEST" ) == 0, "Ok", "Failure" )
   ?? Iif( leto_DirExist( "TEST" ), " verified", "!" )
   ? 'leto_DirRemove( "TEST" ) - '
   ?? Iif( leto_DirRemove( "TEST" ) == 0, "Ok", "Failure" )
   ?? Iif( ! leto_DirExist( "TEST" ), " verified", "!" )

   ? "Press any key to continue..."
   ?
   arr := leto_directory( "*" )
   ? 'leto_directory(): (' + Ltrim(Str(Len(arr))) + ")"
   Inkey( 0 )
   ? "found files: "
   FOR i := 1 TO Len( arr )
      ?? arr[i,1] + "; "
   NEXT
   ? "----------"
   ? "Press any key to continue..."

   ? 'leto_ferase( "test2.txt" ) - '
   ?? Iif( leto_fErase( "test2.txt" ) == 0, "Ok", "Failure" )
   leto_fErase( "test1.txt" )
   ?

   ? "Press any key to finish..."
   Inkey( 0 )

Return Nil

