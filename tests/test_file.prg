/*
 * This sample demonstrates how to use file functions with Leto db server
 * EnableFileFunc = 1 must be set in server's letodb.ini
 */
REQUEST LETO
#include "rddleto.ch"

REQUEST hb_vfExists, hb_vfOpen, hb_vfClose

Function Main( cPath )
 LOCAL cBuf, arr, i, lTmp
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

   ? 'leto_frename( "test1.txt","test2.txt" ) - '
   ?? Iif( leto_frename( "test1.txt","test2.txt" ) == 0, "Ok", "Failure" )

   ? 'leto_file( "test1.txt" ) - '
   lTmp := leto_file( "test1.txt" )
   ?? Iif( lTmp, "Ok", "No" )
   ?? Iif( ! lTmp, "!", " Failure" )

   ? 'leto_file( "test2.txt" ) - '
   lTmp := leto_file( "test2.txt" )
   ?? Iif( leto_file( "test2.txt" ), "Ok", "No" )
   ?? Iif( lTmp, "!", " Failure" )

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

   ? 'leto_fCopyFromSrv( "test3.txt", "test2.txt" ) - '
   ?? Iif( leto_fCopyFromSrv( "test3.txt", "test2.txt", 1000 ), "Ok", "Failure" )
   ?? Iif( FILE( "test3.txt" ), "!", "@" )

   MemoWrit( "test3.txt", Replicate( "z", 123456 ) )
   ? 'leto_fCopyToSrv( "test3.txt", "test2.txt" ) - '
   ?? Iif( leto_fCopyToSrv( "test3.txt", "test2.txt", 1000 ), "Ok", "Failure" )
   ?? Iif( leto_filesize( "test2.txt" ) == 123457, "!", "@" )  /* +1 for strg-z */
   FErase( "test3.txt" )

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
   ?

   ? "Press any key to finish..."
   Inkey( 0 )

Return Nil
