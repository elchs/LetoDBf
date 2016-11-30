 REQUEST LETO
 REQUEST DBFCDX
 EXTERNAL DBORDERINFO
 EXTERNAL hb_StrToTS
 EXTERNAL VALTYPE
 EXTERNAL LETO_HASH
 EXTERNAL MEMORY
 DYNAMIC DynsFun65535

Function Main( cPath, cUser, cPasswd )
 Local aNames := { "Petr", "Ivan", "Alexander", "Pavel", "Alexey", "Fedor", ;
   "Konstantin", "Vladimir", "Nikolay", "Andrey", "Dmitry", "Sergey" }
 Local i, ii, aStru //,lUseLeto:=.f.
 LOCAL iPause := 475, iMax := 50000
 LOCAL cVersion
 LOCAL cText := ""
 LOCAL pInit, pHrb, aFunc, cSig
 Field NAME, NUM, INFO, DINFO, STAMP

   //altd()
   SET DATE FORMAT "yyyy.mm.dd"
   altd()
   ? __DynsCount(), __DynsGetName( 1 ), hb_IsFunction( "test" ), __DynsVerify()
   if valtype(cPath)="C"
      RDDSETDEFAULT( "LETO" )
      cPath := "//" + cPath + Iif( Right(cPath,1) $ "/\", "", "/" )
   else
      RDDSETDEFAULT("DBFCDX")
      cPath:=""
   endif

   IF RDDSETDEFAULT() == "LETO"
      IF ( leto_Connect( cPath, cUser, cPasswd ) ) != -1
         cVersion := leto_GetServerVersion()
         cPath := ""
     ENDIF
     iMax := 450
   ELSE
     cVersion := "DBFCDX"
   ENDIF
   ? cVersion

   IF IIF( RDDSETDEFAULT() == "LETO", ! LETO_FILE( cPath+"test" + ".dbf" ), ! FILE( "test.dbf" ) )

      dbCreate( cPath+"test", { {"NAME","C",10,0}, {"NUM","N",4,0}, {"INFO","C",32,0}, {"DINFO","D",8,0} } )
      IF RDDSETDEFAULT() == "LETO"
         LETO_DBDRIVER( "DBFCDX", "SMT", 1024 )
      ENDIF
      dbCreate( cPath+"test", { {"NAME","C",10,0}, {"NUM","N",4,0}, {"INFO","C",32,0}, {"DINFO","D",8,0}, {"STAMP","@",8,0} } )
      SetColor( "w+/b" )
      CLS

      dbselectarea(1)

      dbusearea(.T., ,cPath+"test", ,.T.)
      hb_DispOutAt(2,0,"File has been opened")
      aStru := dbStruct()
      hb_DispOutAt(3,0,"Fields: ")
      FOR i := 1 TO Len( aStru )
         hb_DispOutAt(i+3,0,str(i,2)+"   "+aStru[i,1])
         hb_DispOutAt(i+3,20,aStru[i,2]+"   "+str(aStru[i,3],3)+str(aStru[i,4],4))
      NEXT

      FOR i := 1 TO Len( aNames )
         IF i == 5
            hb_idleSleep( 0.1 )
         ENDIF
         APPEND BLANK
         REPLACE NAME WITH aNames[i], NUM WITH i+1000, ;
            INFO WITH "This is a record number "+Ltrim(Str(i)), ;
            DINFO WITH Date()+i-1,;
            STAMP WITH hb_dateTime()
      NEXT
      DbUnlock()

      hb_DispOutAt(10,0,"Records have been added")
      INDEX ON NAME TAG NAME
      INDEX ON Stamp TAG STAMP
      INDEX ON Str(NUM,4) TAG NUM
      hb_DispOutAt(11,0,"File has been indexed")

      DbCloseAll()
   ENDIF

#if 0
   FOR i := 1 TO iMax
      dbusearea(.T., ,cPath+"test", "TEST"+hb_ntos(i) ,.T.)
      @ 1, 1 SAY i
      IF i == iPause
         altd()
      ENDIF
   NEXT i
#else
      IF RDDSETDEFAULT() == "LETO"
         LETO_DBDRIVER( "DBFCDX", "SMT", 1024 )
      ENDIF
      FOR i := 1 TO iMax
         IF i > 0 // .And. ! LETO_FILE( cPath+"test65500.dbf" )
            IF ! LETO_FILE( "test" + hb_ntos(i) + ".dbf" )
               leto_fcopy( "test.dbf", "/tmp/LETOS/test" + hb_ntos(i) + ".dbf" )
               leto_fcopy( "test.cdx", "/tmp/LETOS/test" + hb_ntos(i) + ".cdx" )
            ENDIF
         ENDIF

         dbusearea(.T., ,cPath+"test"+hb_ntos(i) ,.T.)
         @ 1, 1 SAY i
         IF i == iPause
           altd()
         ENDIF
      NEXT i
#endif

   wait

   @ 2,1 SAY __DynsCount()
   wait
   
#if 0 
   FOR i := 1 TO iMax
      cText += "FUNCTION DynsFun" + hb_nToS( i ) + "()" + hb_EOL()
      cText += "RETURN .T." + hb_EOL()
   NEXT i
   MemoWrit( "dynsdyns.prg", cText )

   hb_processRun( "hbmk2 -gh dynsdyns.prg"  , , , , .F. )
   IF FILE( hb_DirBase() + "dynsdyns.hrb" )
      pHrb := hb_hrbLoad( 0, hb_DirBase() + "dynsdyns.hrb" )  /* HB_HRB_BIND_DEFAULT */
      IF ! Empty( pHrb ) .AND. ! Empty( pInit := hb_hrbGetFunSym( pHrb, "DynsFun65535" ) )
         hb_ExecFromArray( pInit )
         aFunc := hb_hrbGetFunList( pHrb )
         ? LEN( aFunc )
         cSig := hb_hrbSignature()
         ? LEN( cSig )
      ENDIF
   ELSE
      ? "compile error"
   ENDIF
   wait

   ? __DynsCount()
   ? __DynsGetName( 1 )
   ? __DynsGetName( 65535 )
   ? __DynsGetName( 65536 )
   ? __DynsGetName( __DynsCount() )
   wait
#endif
   
   DbCloseAll()
   dbusearea(.T., ,cPath+"test", "TEST"+hb_ntos(i) ,.T.)
   wait
   CLS

   FOR ii := 1 TO 2

      FOR i := 1 TO iMax
         IF ! LETO_FILE( "test" + hb_ntos(i) + ".dbf" )
            leto_fcopy( "test.dbf", "/tmp/LETOS/test" + hb_ntos(i) + ".dbf" )
            leto_fcopy( "test.cdx", "/tmp/LETOS/test" + hb_ntos(i) + ".cdx" )
         ENDIF

         dbusearea(.T., ,cPath+"test"+hb_ntos(i) ,.T.)
         @ 1, 1 SAY i
      NEXT i

      DbCloseAll()
   
      FOR i := 1 TO iMax
         IF LETO_FILE( "test" + hb_ntos(i) + ".dbf" )
            DbDrop( cPath + "test" + hb_ntos(i) )
         ENDIF
      NEXT i

   NEXT ii

   wait
   DbDrop( cPath+"test" )
   quit
return NIL

FUNCTION stressDyns
RETURN {|| nothing() }

FUNCTION nothing
RETURN __DynsCount()
