/*
 * This sample tests working with a dbf file in many threads as you have CPU cores
 * Just change the cPath value to that one you need.
 */

REQUEST LETO
REQUEST DBFCDX

/* outcomment to let slower running demo log into a file */
//#define LOG_IN_TXT_FILE  yes

PROCEDURE Main( cPath )

   LOCAL aNames := { "Petr", "Ivan", "Alexander", "Pavel", "Alexey", "Fedor", ;
      "Konstantin", "Vladimir", "Nikolay", "Andrey", "Dmitry", "Sergey" }
   LOCAL i, aStru // ,lUseLeto:=.f.
   LOCAL nHandle, ncounter
   LOCAL tStamp
   LOCAL nSrvMode
   FIELD NAME, NUM, INFO, DINFO, STAMP

   AltD()
   SET DATE FORMAT "yyyy.mm.dd"

   IF ValType( cPath ) == "C"
      rddSetDefault( "LETO" )
      cPath := "//" + cPath + iif( ":" $ cPath, "", ":2812" )
      cPath += iif( Right( cPath, 1 ) $ "/\", "", "/" )
   ELSE
      rddSetDefault( "DBFCDX" )
      cPath := ""
   ENDIF

   IF rddSetDefault() == "LETO"
      IF leto_Connect( cPath, "anonymous", "anonymous", 42000 /*timeout*/, /*hot buffer*/ ) == -1
         RETURN
      ENDIF
      LETO_DBDRIVER( "DBFCDX", "SMT", 1024 )
      nSrvMode := LETO_GETSERVERMODE()
   ELSE
      nSrvMode := 5
   ENDIF

   dbCreate( cPath + "test1", { { "NAME", "C", 10, 0 }, { "NUM", "N", 4, 0 }, { "INFO", "C", 32, 0 }, { "DINFO", "D", 8, 0 }, { "STAMP", "@", 8, 0 } } )
   SetColor( "w+/b" )
   CLS
   nHandle := FCreate( "testmt.log", 0 )
   FClose( nHandle )
   nHandle := FOpen( "testmt.log", 33 )
   IF nHandle > 0
      hb_DispOutAt( 1, 0, "File has been created" )
   ENDIF
   dbSelectArea( 1 )
   dbUseArea( .T., , cPath + "test1", , .T. )
   hb_DispOutAt( 2, 0, "File has been opened" )
   aStru := dbStruct()
   hb_DispOutAt( 3, 0, "Fields: " )
   FOR i := 1 TO Len( aStru )
      hb_DispOutAt( i + 3, 0, Str( i, 2 ) + "   " + aStru[ i, 1 ] )
      hb_DispOutAt( i + 3, 20, aStru[ i, 2 ] + "   " + Str( aStru[ i, 3 ], 3 ) + Str( aStru[ i, 4 ], 4 ) )
   NEXT

   FOR i := 1 TO Len( aNames )
      IF i == 5
         hb_idleSleep( 0.1 )
      ENDIF
      IF DbAppend()
         REPLACE NAME WITH aNames[ i ], NUM WITH i + 1000, ;
                 INFO WITH "This is a record number " + LTrim( Str( i ) ), ;
                 DINFO WITH Date() + i - 1, ;
                 STAMP WITH hb_DateTime()
      ENDIF
   NEXT
   dbUnlock()

   hb_DispOutAt( 10, 0, hb_nToS( RecCount() ) + " Records have been added" )
   INDEX ON NAME TAG NAME
   INDEX ON Stamp TAG STAMP
   INDEX ON Str( NUM, 4 ) TAG NUM
   hb_DispOutAt( 10, 30, ", File has been indexed" )
   IF nSrvMode >= 2 // share tables or No_Save_WA = 1
      IF dbUseArea( .T., , cPath + "test1", "TESTY", .T. )
         hb_DispOutAt( 11, 0, "DBF opend second time with different ALIAS " + Alias() )
      ENDIF
   ENDIF
   ordSetFocus( 2 )
   dbGoto( 5 )
   tStamp := STAMP
   dbGoTop()
   dbSeek( tStamp, .T. )
   IF RecNo() == 5
      hb_DispOutAt( 12, 0, "Timestamp seek successfull" )
   ENDIF
   ordSetFocus( 3 )
   ordScope( 1, "1001" )
   ordSetFocus( 1 )
   dbGoTop()
   nCounter := 1
   hb_DispOutAt( 13, 0, "Main thread running..." )
   DO WHILE LastKey() != 27
      hb_DispOutAt( 14, 0, name )
      testlog( nHandle, "Main thread at record " + AllTrim( Str( RecNo(), 4 ) ) )
      dbSkip()
      IF Eof()
         testlog( nHandle, "Main thread calling dbgoTop()" )
         dbGoTop()
         IF nSrvMode >= 2 // share tables or No_Save_WA = 1
            IF Alias() == "TEST1"
               dbSelectArea( "TESTY" )
            ELSE
               dbSelectArea( "TEST1" )
            ENDIF
            testlog( nHandle, "Primary thread changed to area " + Alias() )
         ENDIF
      ENDIF
#ifdef LOG_IN_TXT_FILE
      Inkey( 0.01 )
#else
      IF NextKey() != 0
         Inkey( 0.01 )
      ENDIF
#endif
      nCounter++
      IF nCounter <= IIF( Leto_CPUCores() < 2, 1, Leto_CPUCores() - 1 )  * 10 .AND. nCounter % 10 == 0
         hb_threadStart( @thFunc(), cPath, nHandle, Int( nCounter / 10 ) + 1 )
      ENDIF
   ENDDO
   FClose( nHandle )
   SET COLOR TO "w/n"

   WAIT
   CLS
   dbCloseAll()
   dbDrop( cPath + "test1" )
   QUIT

   RETURN


PROC thFunc( cPath, nHandle, nCounter )

   LOCAL nSrvMode, cThreadName

   IF nCounter == 2
      cThreadName := "Secondary"
   ELSE
      cThreadName := hb_ntos( nCounter ) + iif( nCounter < 4, "rd", "th" )
   ENDIF

   hb_DispOutAt( 13 + ( nCounter * 2 ), 0, cThreadName + " thread starting..." )
   IF rddSetDefault() == "LETO"
      IF leto_Connect( cPath, cThreadName, "anonymous", 42000 /*timeout*/, /*hot buffer*/ ) == -1
         RETURN
      ENDIF
      LETO_DBDRIVER( "DBFCDX", "SMT", 1024 )
      nSrvMode := LETO_GETSERVERMODE()
   ELSE
      nSrvMode := 5
   ENDIF

   testlog( nHandle, "CurrentConnectio: " + LETO_GETCURRENTCONNECTION() )
   dbUseArea( .T., , cPath + "test1", "TEST1", .T. )
   IF nSrvMode >= 2 // share tables or No_Save_WA = 1
      dbUseArea( .T., , cPath + "test1", "TESTX", .T. )
   ENDIF
   DO WHILE LastKey() != 27
      hb_DispOutAt( 14 + ( nCounter * 2 ), 0, field->name )
      testlog( nHandle, "Secondary thread at record " + AllTrim( Str( RecNo(), 4 ) ) )
      dbSkip( -1 )
      IF Bof()
         testlog( nHandle, cThreadName + " thread calling dbGoBottom" )
         dbGoBottom()
         IF nSrvMode >= 2 // share tables or No_Save_WA = 1
            IF Alias() == "TEST1"
               dbSelectArea( "TESTX" )
            ELSE
               dbSelectArea( "TEST1" )
            ENDIF
            testlog( nHandle, cThreadName + " thread changed to area " + Alias() )
         ENDIF
      ENDIF
#ifdef LOG_IN_TXT_FILE
      Inkey( 0.01 )
#else
      IF NextKey() != 0
         Inkey( 0.01 )
      ENDIF
#endif
   ENDDO

   RETURN


FUNCTION testlog( nHandle, cString )

#ifdef LOG_IN_TXT_FILE
   FWrite( nHandle, cString + hb_eol() )
#else
   HB_SYMBOL_UNUSED( nHandle )
   HB_SYMBOL_UNUSED( cString )
#endif

   RETURN NIL
