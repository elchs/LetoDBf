/*
 * This sample demonstrates how to use file functions with Leto db server
 * EnableFileFunc = 1 must be set in server's letodb.ini
 */
REQUEST LETO
#include "rddleto.ch"

REQUEST Leto_MakeDir, Leto_DirExist, Leto_DirRemove, Leto_FError

Function Main( cPath )
 LOCAL cBuf, arr, i
 LOCAL nPort := 2812

   RDDSETDEFAULT( "LETO" )

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

   /* No more IP prefix needed after Leto_connect() */
   cPath := ""

   ? 'leto_file( "test1.txt" ) - '
   ?? Iif( leto_file( cPath + "test1.txt" ), "Ok", "No" )
   i := Leto_fError()
   ? 'file functions working ? - ' + Iif( i == 100, "disabled",;
      Iif( i == 0, "well well", "ups, error: " + STR( leto_fError(), 5, 0 ) ) ) 

   ? 'leto_memowrite( "test1.txt", "A test N1" ) - '
   ?? Iif( leto_memowrite( cPath + "test1.txt", "A test N1" ), "Ok", "Failure" )

   ? 'leto_file( "test1.txt" ) - '
   ?? Iif( leto_file( cPath + "test1.txt" ), "Ok", "No" )

   ? 'leto_memoread( "test1.txt" ) - '
   ?? leto_memoread( cPath + "test1.txt" )

   ? 'leto_frename( "test1.txt","test2.txt" ) - '
   ?? Iif( leto_frename( cPath + "test1.txt","test2.txt" ) == 0, "Ok", "Failure" )

   ? 'leto_file( "test1.txt" ) - '
   ?? Iif( leto_file( cPath + "test1.txt" ), "Ok", "No" )

   ? 'leto_file( "test2.txt" ) - '
   ?? Iif( leto_file( cPath + "test2.txt" ), "Ok", "No" )

   ? 'leto_fileread( "test2.txt", 7, 2 ) - '
   ?? Iif( leto_fileread( cPath + "test2.txt", 7, 2, @cBuf ) > 0, cBuf, "Failure" )

   ? 'leto_filewrite( "test2.txt", 7, "N2" ) - '
   ?? Iif( leto_filewrite( cPath + "test2.txt", 7, "N2" ), "Ok", "Failure" )

   ? 'leto_memoread( "test2.txt" ) - '
   ?? leto_memoread( cPath + "test2.txt" )

   ? 'leto_filesize( "test2.txt" ) - '
   ?? leto_filesize( cPath + "test2.txt" )

   arr := leto_directory( "\" )
   ? 'leto_directory(): (' + Ltrim(Str(Len(arr))) + ")"
   ? "Press any key to continue..."
   Inkey( 0 )
   FOR i := 1 TO Len( arr )
      ? arr[i,1]
   NEXT
   ? "----------"

   ? 'leto_ferase( "test2.txt" ) - '
   ?? Iif( leto_fErase( cPath + "test2.txt" ) == 0, "Ok", "Failure" )
   ?

   ? "Press any key to finish..."
   Inkey( 0 )

Return Nil
