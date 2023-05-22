/*
 * This sample demonstrates how to use set/get variables functions with LetoDBf server
 */
REQUEST LETO

#ifdef __XHARBOUR__
   #include "hbcompat.ch"
   #define hb_milliseconds   LETO_MILLISEC
   #define hb_BLen( cTest )  LEN( cTest )
#endif

MEMVAR cPub, aPriv

Function Main( cPath )
 LOCAL aArr, cTest, lRes, nRes, i, xPrevious, dTest, cGroup
 PUBLIC cPub := "init"
 PRIVATE aPriv := { "first" }
 #ifdef __XHARBOUR__
   PRIV dDate := DATE() -1
 #endif

   IF Empty( cPath )
      cPath := "//127.0.0.1:2812/"
   ELSE
      cPath := "//" + cPath + IiF( ":" $ cPath, "", ":2812" )
      cPath += Iif( Right(cPath,1) == "/", "", "/" )
   ENDIF

   ? "Connect to " + cPath + " - "
   IF ( leto_Connect( cPath ) ) == -1
      nRes := leto_Connect_Err()
      IF nRes == LETO_ERR_LOGIN
         ?? "Login failed"
      ELSEIF nRes == LETO_ERR_RECV
         ?? "Recv Error"
      ELSEIF nRes == LETO_ERR_SEND
         ?? "Send Error"
      ELSE
         ?? "No connection"
      ENDIF
      Return Nil
   ELSE
      ?? "Ok"
   ENDIF
   ?
   altd()

   ? "Test: is a variable with attribute 'LETO_VOWN' from my last run deleted ?"
   ? IIF( EMPTY( leto_varGet( "main", "var_dec" ) ) .AND.;
          EMPTY( leto_varGet( "main","var_binary" ) ), ".. all fine deleted", "ups, BUG! alarm" )

   /* test error: missing LETO_VCREAT flag */
   ?  "Adding 'var_int' = 100 to [main] [Err (0)] "
   lRes := leto_varSet( "main", "var_int", 100, LETO_VNOCREAT )
   IF lRes
      ?? "Ok"
   ELSE
      ?? "Err (" + Ltrim( Str ( leto_ferror() ) ) + ")"
   ENDIF

   ?  "Adding 'var_int' = 100 to [main] [Ok] "
   lRes := leto_varSet( "main", "var_int", 99, LETO_VCREAT )
   IF lRes
      ?? "Ok"
      lRes := leto_varSet( "main", "var_int", 100,, @xPrevious )
      IF lRes .AND. VALTYPE( xPrevious ) == "N" .AND. xPrevious == 99
        ?? "!"
      ELSE
        ?? " failure LETO_VPREVIOUS"
      ENDIF
   ELSE
      ?? "Err (" + Ltrim( Str( leto_ferror() ) ) + ")"
   ENDIF

   ?  "Adding 'var_dec' = 123.456 to [main] [Ok] "
   lRes := leto_varSet( "main", "var_dec", 123.455, LETO_VCREAT + LETO_VOWN )
   IF lRes
      IF leto_varGet( "main", "var_dec" ) == 123.455
         ?? "Ok"
      ENDIF
      lRes := leto_varSet( "main", "var_dec", 123.456,, @xPrevious )
      IF lRes .AND. VALTYPE( xPrevious ) == "N" .AND. xPrevious == 123.455
        ?? "!"
      ELSE
        ?? " failure LETO_VPREVIOUS"
      ENDIF
   ELSE
      ?? "Err (" + Ltrim( Str( leto_ferror() ) ) + ")"
   ENDIF

   ?  "Adding 'var_log' = .T. to [main] [Ok] "
   lRes := leto_varSet( "main", "var_log", .T., LETO_VCREAT )
   IF lRes
      ?? "Ok"
   ELSE
      ?? "Err (" + Ltrim( Str( leto_ferror() ) ) + ")"
   ENDIF

   ?  "Adding 'var_char' = 'Just a test;' to [main] [Ok] "
   lRes := leto_varSet( "main", "var_char", "Just a test;", LETO_VCREAT )
   IF lRes
      ?? "Ok"
      lRes := leto_varGetCached()
      ? "Request again last set value:", leto_varGetCached(), IIF( lRes == 'Just a test;', 'Ok', 'failed' )
   ELSE
      ?? "Err (" + Ltrim( Str( leto_ferror() ) ) + ")"
   ENDIF

   ?  "Adding 'var_binary' containing: 'CHR(0);CHR(1);CHR(0)' to [main] [Ok] "
   lRes := leto_varSet( "main","var_binary", CHR(0) + ";" + CHR(1) + ";" + CHR(0), LETO_VCREAT + LETO_VOWN )
   IF lRes
      cTest := leto_varGet( "main", "var_binary" )
      IF cTest == CHR(0) + ";" + CHR(1) + ";" + CHR(0) .AND. hb_BLen( cTest ) == 5
         ?? 'OK'
      ELSE
         ?? 'failure'
      ENDIF
   ELSE
      ?? "Err (" + Ltrim(Str(leto_ferror())) + ")"
   ENDIF

   ?  "Adding 'var_arr' = { 1, 2, 3 } to [main] [Ok] "
   lRes := leto_varSet( "main", "var_arr", { 1, 2, 3 }, LETO_VCREAT + LETO_VOWN )
   IF lRes
      aArr = leto_varGet( "main","var_arr" )
      IF VALTYPE( aArr ) == "A" .AND. LEN( aArr ) == 3
         ?? 'OK', hb_BLen( hb_Serialize( aArr ) ), "bytes"
      ELSE
         ?? 'failure'
      ENDIF
   ELSE
      ?? "Err (" + Ltrim( Str( leto_ferror() ) ) + ")"
   ENDIF

   dTest := hb_SToD( "19690815" )
   ?  "Adding 'var_date' = " + DToC( dTest ) + " to [main] [Ok] "
   lRes := leto_varSet( "main", "var_date", dTest, LETO_VCREAT + LETO_VOWN )
   IF lRes
      dTest = leto_varGet( "main", "var_date" )
      IF VALTYPE( dTest ) == "D"
         ?? 'OK'
         IF dTest == hb_SToD( "19690815" )
            ?? ' !'
         ENDIF
      ELSE
         ?? 'failure'
      ENDIF
   ELSE
      ?? "Err (" + Ltrim( Str( leto_ferror() ) ) + ")"
   ENDIF

   ShowVars()

   ? "var_int = [100]", leto_varGet( "main","var_int" )
   ? "var_char = [Just a test;]", leto_varGet( "main","var_char" )
   ? "var_log = [.T.]", leto_varGet( "main","var_log" )

   ? "Press any key to continue..."
   Inkey( 0 )
   ?

   leto_varDecr( "main", "var_dec",, 2.135 )
   ? "Increment var_int, current value is [100]", leto_varIncr( "main", "var_int", LETO_VPREVIOUS )
   ? "Increment var_int, next    value is [102]", leto_varIncr( "main", "var_int" )
   ? "Decrement var_int, current value is [102]", leto_varDecr( "main", "var_int", LETO_VPREVIOUS )
   ? "Decrement var_int, next    value is [100]", leto_varDecr( "main", "var_int" )
   ? "Decrement var_dec, decimal decriment[121.32]", leto_varGet( "main", "var_dec" )

   ? "Press any key to continue..."
   Inkey( 0 )
   ?

   ? "Delete var_log [Ok] "
   lRes := leto_varDel( "main","var_log" )
   IF lRes
      ?? "Ok"
   ELSE
      ?? "Err (" + Ltrim( Str( leto_ferror() ) ) + ")"
   ENDIF

   ? "Delete var_char [Ok] "
   lRes := leto_varDel( "main","var_char" )
   IF lRes
      ?? "Ok"
   ELSE
      ?? "Err (" + Ltrim( Str( leto_ferror() ) ) + ")"
   ENDIF

   ? "Delete var_int [Ok] "
   lRes := leto_varDel( "main","var_int" )
   IF lRes
      ?? "Ok"
      lRes := leto_varDel( "main","var_int" )
      IF ! lRes
         ?? "!"
      ENDIF
   ELSE
      ?? "Err (" + Ltrim( Str( leto_ferror() ) ) + ")"
   ENDIF

   ShowVars()
   ? "Press any key to continue..."
   Inkey( 0 )
   ?

   ? "Test to add 99 variables to group 'group100': "
   lRes := .T.
   FOR i := 1 TO 99
      IF ! leto_varSet( "Group100","var_" + ALLTRIM( STR( i, 2, 0)), i, LETO_VCREAT )
         lRes := .F.
      ENDIF
   NEXT i
   ?? IIF( lRes, "Ok", "failure")

   IF lRes
      ? "Now delete these 99 variables one by one: "
      FOR i := 1 TO 99
         IF ! leto_varDel( "gRoUp100","var_" + ALLTRIM( STR( i, 2, 0 ) ) )
            lRes := .F.
         ENDIF
      NEXT i
      ?? IIF( lRes, "Ok", "failure")
   ENDIF

   ? "again add 99 variables to group 'group100': "
   lRes := .T.
   FOR i := 1 TO 99
      IF ! leto_varSet( "Group100","var_" + ALLTRIM( STR( i, 2, 0) ), i, LETO_VCREAT )
         lRes := .F.
      ENDIF
   NEXT i
   IF lRes
      ? "Now delete these 99 variables at once by just deleting whole group: "
      lRes := leto_varDel( "GRoUP100" )
      ?? IIF( lRes, "Ok", "failure" )
   ENDIF

   ?
   ?
#ifndef __XHARBOUR__
   cTest := "'exit' < cPub .AND. aPriv[ 1 ] == 'first'"
#else
   cTest := "'exit' < cPub .AND. dDate < DATE()"
#endif
   ? "test   expression: "
   IF Leto_VarExprTest( cTest )
      ?? "found memvar variables"
   ELSE
      ?? "failed to detect memvar"
   ENDIF

   ? "create expression: "
   ? cTest
   ? "==> "
   cTest := Leto_VarExprCreate( cTest, @aArr )
   cGroup := "My"  /* variables specific to this connection to server */
#ifndef __XHARBOUR__
   IF UPPER( cTest ) == UPPER( "'exit'<LETO_VARGET('" + cGroup + "','CPUB').AND.LETO_VARGET('" + cGroup + "','aPriv')[1]=='FIRST'" )
#else
   /* letodbf for xHarbour can't handle arrays because of complete missing hb_item[De]Serialize() in xHarbour */
   IF UPPER( cTest ) == UPPER( "'exit'<LETO_VARGET('" + cGroup + "','CPUB').AND.LETO_VARGET('" + cGroup + "','dDate')<DATE()" )
#endif
      ?? "ok"
   ELSE
      ?? "failed"
   ENDIF
   ? cTest

   ? "sync   expression: "
   IF EMPTY( aArr )
      ?? "failed to read memvar into array"
   ELSE
      cPub := "magic"
#ifndef __XHARBOUR__      
      AADD( aPriv, "second" )
      Leto_VarExprSync( aArr )
      IF Leto_VarGet( cGroup, "cPub" ) == "magic" .AND. LEN( Leto_VarGet( cGroup, "aPriv" ) ) == 2
         ?? "ok"
      ELSE
         ?? "failed ( magic, 2 ) :", Leto_VarGet( cGroup, "cPub" ), LEN( Leto_VarGet( cGroup, "aPriv" ) )
      ENDIF
#else
      dDate := DATE() - 5
      Leto_VarExprSync( aArr )
      IF Leto_VarGet( cGroup, "cPub" ) == "magic" .AND. Leto_VarGet( cGroup, "dDate" ) == DATE() - 5
         ?? "ok"
      ELSE
         ?? "failed ( magic, 2 ) :", Leto_VarGet( cGroup, "cPub" ), Leto_VarGet( cGroup, "dDate" )
      ENDIF
#endif

      /* benchmark of Leto_VarSet() with one change var */
      nRes := hb_milliseconds()
      i := 0
      DO WHILE i++ < 9999
         IF i % 2 == 1
            cPub := "Magical"
         ELSE
            cPub := "Miracle"
         ENDIF
         Leto_VarExprSync( aArr )
         IF Leto_VarGet( cGroup, "cPub" ) != cPub
            EXIT
         ENDIF
      ENDDO
      ? HB_NTOS( i ), "verified syncs :", STR( ( hb_milliseconds() - nRes ) / 1000, 7, 2 ), "s"

      /* benchmark of checks without changes */
      nRes := hb_milliseconds()
      FOR i := 1 TO 1000000
         Leto_VarExprSync( aArr )
      NEXT i
      ? " 1 M check:", STR( ( hb_milliseconds() - nRes ) / 1000, 7, 2 ), "s"

      ? "resync expression: "
      Leto_VarSet( cGroup, "cPub", "totally magic" )
#ifndef __XHARBOUR__
      Leto_VarSet( cGroup, "aPriv", { "first", "second", "third" } )
      Leto_VarExprSync( aArr, .T. )
      IF cPub == "totally magic" .AND. LEN( aPriv ) == 3
#else
      Leto_VarSet( cGroup, "dDate", DATE() + 42 )
      Leto_VarExprSync( aArr, .T. )
      IF cPub == "totally magic" .AND. dDate == DATE() + 42
#endif
         ?? "ok", cPub
      ELSE
         ?? "failed ( totally magic, 3  ) :", cPub
      ENDIF

   ENDIF

   ? "delete expression: "
   Leto_VarExprClear( cTest )
#ifndef __XHARBOUR__
   IF Leto_Varget( cGroup, "cPub" ) == NIL .OR. Leto_VarGet( cGroup, "aPriv" ) == NIL
#else
   IF Leto_Varget( cGroup, "cPub" ) == NIL .OR. Leto_VarGet( cGroup, "dDate" ) == NIL
#endif
      ?? "ok"
   ELSE
      ?? "failed",  Leto_VarGet( cGroup, "cPub" ), Leto_VarGet( cGroup, "aPriv" )
   ENDIF

   ?
   ShowVars()
   ?
   ? "No explicite need to delete var_dec, var_binary or var_date,"
   ? "<LETO_VOWN> variables will automatic deleted, this is checked in next run .."
   ?
   ? "Press any key to finish..."
   Inkey( 0 )

Return Nil

Static Function ShowVars()
Local i, j, arr, arr1

   ? "--- Vars list ---"
   IF ( arr := leto_varGetlist() ) != Nil
      FOR i := 1 TO Len( arr )
         ? arr[ i ] + " ("
         arr1 := leto_varGetlist( arr[ i ] )
         FOR j := 1 TO Len( arr1 )
            ?? arr1[ j ] + Iif( j == Len( arr1 ), ")-->", "," )
         NEXT
         arr1 := leto_varGetlist( arr[ i ], 12 )
         FOR j := 1 TO Len( arr1 )
            ? "   " + arr1[ j, 1 ] + ":", arr1[ j, 2 ]
         NEXT
      NEXT
   ELSE
      ? "Error reading list:", leto_ferror()
   ENDIF
   ? "----------"

Return Nil

