/*
 * This sample tests working with a dbf file in two threads
 * Just change the cPath value to that one you need.
 */
 
 REQUEST LETO
 REQUEST DBFCDX
 EXTERNAL DBORDERINFO
 EXTERNAL hb_StrToTS
 EXTERNAL VALTYPE

PROCEDURE Main( cPath )
 Local aNames := { "Petr", "Ivan", "Alexander", "Pavel", "Alexey", "Fedor", ;
                  "Konstantin", "Vladimir", "Nikolay", "Andrey", "Dmitry", "Sergey" }
 Local i, aStru //,lUseLeto:=.f.
 local nHandle, ncounter
 local tStamp
 Local nSrvMode
 Field NAME, NUM, INFO, DINFO, STAMP

   altd()
   SET DATE FORMAT "yyyy.mm.dd"
   
   if valtype(cPath) == "C"
      RDDSETDEFAULT( "LETO" )
      cPath := "//" + cPath + IiF( ":" $ cPath, "", ":2812" )
      cPath += Iif( Right(cPath,1) $ "/\", "", "/" )
   else
      RDDSETDEFAULT("DBFCDX")
      cPath:=""
   endif

   IF RDDSETDEFAULT() == "LETO"
      IF leto_Connect( cPath, "anonymous", "anonymous", 42000 /*timeout*/, /*hot buffer*/ ) == -1
         return
      ENDIF
      LETO_DBDRIVER( "DBFCDX", "SMT", 1024 )
      nSrvMode := LETO_GETSERVERMODE()
   ENDIF

   dbCreate( cPath + "test1", { {"NAME","C",10,0}, {"NUM","N",4,0}, {"INFO","C",32,0}, {"DINFO","D",8,0}, {"STAMP","@",8,0} } )
   SetColor( "w+/b" )
   CLS
   nHandle:=fCreate("testmt.log",0)
   fclose(nHandle)
   nHandle:=fOpen("testmt.log",33)
   if nHandle>0
      hb_DispOutAt(1,0, "File has been created")
   endif
   dbselectarea(1)
   dbusearea(.T., ,cPath+"test1", ,.t.)
   IF nSrvMode == 2 .OR. nSrvMode == 4  // share tables
      dbusearea(.T., ,cPath + "test1", "TESTY",.t.)
   ENDIF
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
   ordsetfocus(2)
   DbGoto(5)
   tStamp := STAMP
   DbGoTop()
   DBSEEK( tStamp, .T. )
   IF RECNO() == 5
      hb_DispOutAt(12,0,"Timestamp seek successfull")
   ENDIF
   ordsetfocus(3)
   ordscope(1,"1001")
   ordsetfocus(1)
   dbgotop()
   nCounter:=0
   do while lastkey()!=27
      hb_DispOutAt(15,0,name)
      testlog(nHandle,"Main thread at record "+alltrim(str(recno(),4)))
      dbskip()
      if eof()
         testlog(nHandle,"Main thread calling dbgoTop()")
         dbgotop()
         IF nSrvMode == 2 .OR. nSrvMode == 4  // share tables
            IF ALIAS() == "TEST1"
               DbSelectArea("TESTY")
            ELSE
               DbSelectArea("TEST1")
            ENDIF
            testlog(nHandle,"Secondary thread changed to area" + ALIAS() )
         ENDIF
      endif
      inkey(.01)
      nCounter++
      if nCounter==10
         hb_threadStart( @thFunc(),cPath,nHandle )
      endif
   enddo
   fclose(nHandle)
   set color to "w/n"

   wait
   CLS
   DbCloseAll()
   DbDrop( cPath+"test1" )
   quit
return


PROC thFunc(cPath,nHandle)
 LOCAL nSrvMode

   hb_DispOutAt(19,0,"Secondary thread starting...")
   IF RDDSETDEFAULT() == "LETO"
      IF leto_Connect( cPath, "anonymous", "anonymous", 42000 /*timeout*/, /*hot buffer*/ ) == -1
         return
      ENDIF
      LETO_DBDRIVER( "DBFCDX", "SMT", 1024 )
      nSrvMode := LETO_GETSERVERMODE()
   ENDIF
   
   testlog(nHandle, "CurrentConnectio:" + STR(LETO_GETCURRENTCONNECTION(),3,0) )
   dbusearea(.T., ,cPath + "test1", "TEST1",.t.)
   IF nSrvMode == 2 .OR. nSrvMode == 4  // share tables
      dbusearea(.T., ,cPath + "test1", "TESTX",.t.)
   ENDIF
   do while lastkey() != 27
      hb_DispOutAt(20,0,field->name)
      testlog(nHandle,"Secondary thread at record "+alltrim(str(recno(),4)))
      dbskip(-1)
      if bof()
         testlog(nHandle,"Secondary thread calling dbGoBottom")
         dbGoBottom()
         IF nSrvMode == 2 .OR. nSrvMode == 4  // share tables
            IF ALIAS() == "TEST1"
               DbSelectArea("TESTX")
            ELSE
               DbSelectArea("TEST1")
            ENDIF
            testlog(nHandle,"Secondary thread changed to area" + ALIAS() )
         ENDIF
      endif
      inkey( 0.01 )
   enddo
return


function testlog(nHandle,cString)

fWrite(nHandle,cString+chr(13)+chr(10))
return nil
