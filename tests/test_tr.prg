/* do NOT use for xHarbour */
#include "dbinfo.ch"

/* pre-process commands COPY APPEND SORT and xtranslate __dbtrans ... */
/* missing in  letodb.hbc ? */
#ifndef LETO_STD_CH_
   #include "leto_std.ch"
#endif


#if defined( __PLATFORM__LINUX )
   REQUEST HB_GT_XWC
   REQUEST HB_GT_XWC_DEFAULT
#else
   REQUEST HB_GT_WVT
   REQUEST HB_GT_WVT_DEFAULT
#endif

REQUEST LETO, LETO_DBEVAL, DBINFO

Function main( cPath )
 FIELD CODE, NAME, TEXT
 MEMVAR nRecord

 LOCAL aStruct := { { 'Code', 'N', 2, 0 },{ 'Name', 'C', 10, 0 },{ 'Text', 'M', 10, 0 }, { 'AutoInc', '+', 4, 0 } }
 LOCAL nRec
 PRIVATE nRecord

   RDDSETDEFAULT( "LETO" )

   CLS
   ALTD()

   IF Empty( cPath )
      cPath := "//127.0.0.1:2812/"
   ELSE
      cPath := "//" + cPath + IiF( ":" $ cPath, "", ":2812" )
      cPath += Iif( Right( cPath, 1 ) == "/", "", "/" )
   ENDIF

   hb_dbDrop( cPath + "test.dbf" )
   hb_dbDrop( cPath + "test2.dbf" )
   hb_dbDrop( "mem:test4.dbf" )
   hb_dbDrop( "mem:test5.dbf",, "DBFNTX" )

   IF ! DbExists( cPath + "test" )
      DbCreate( cPath + 'test', aStruct,, .T. )
      DbAppend()
      Field->Code := 1
      Field->Name := "First"
      Field->Text := "Alex"
      DbAppend()
      Field->Code := 2
      Field->Name := "Second"
      Field->Text := "Elch"
      DbAppend()
      Field->Code := 3
      Field->Name := "Third"
      Field->Text := "Pavel"
      DbCommit()
      DbCloseArea()
      DbUseArea( .t.,, cPath + 'test',, .T. )
   ELSE
      DbUseArea( .t.,, cPath + 'test',, .T. )
      DbGoBottom()
   ENDIF

   IF FLOCK()
      IF ! DbExists( cPath + "test2" )
         IF DbCreate( cPath + 'test2', aStruct,, .T.,"TEST2" )
            DbSelectArea( "TEST" )
            __dbTrans( SELECT( "TEST2" ) )
            DbSelectArea( "TEST2" )
            DbCloseArea()
            DbSelectArea( "TEST" )
         ENDIF
      ENDIF
      DBUNLOCK()
   ENDIF

   DbUseArea( .t.,, cPath + 'test2',, .T. )
   ? "*          3  1 First               1 Alex"
   ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc, Field->Text
   ?

   DbSelectArea( "TEST" )
   DbGoTop()
   IF FLOCK()
      IF ! DbExists( cPath + "test3" )
         SORT TO test3 ON name /D
      ENDIF

      /* running as second instance ?? -- then only test transition and quit */
      IF DbExists( "mem:test4" )
         IF DbUseArea( .t.,, "mem:test4",, .T. )

            nRec := RecCount()       // a LOCAL var
            nRecord := 2             // a PRIVATE var
            DbSelectArea( "TEST4" )
            FLock()                  // advised to file-lock target, safe and faster as no RLock for each new needed

            DbSelectArea( "TEST" )
            ? Leto_dbTrans( SELECT( "TEST4" ),, { || Code == 2 } )      // executed locally
            ? Leto_dbTrans( SELECT( "TEST4" ),, "{ || Code == 2 }" )    // executed at server
            DbSelectArea( "TEST4" )
            ? "*", nRec + 2, " 2 Second              2"
            ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc
            ?

            DbSelectArea( "TEST" )
            ? Leto_dbTrans( SELECT( "TEST4" ),, "Code == nRecord" )      // executed locally because variable
            ? Leto_dbTrans( SELECT( "TEST4" ),, "{||Code == nRecord}" )  // executed locally because variable
            DbSelectArea( "TEST4" )
            ? "*", nRec + 4, " 2 Second              2"
            ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc
            ?

            DbSelectArea( "TEST" )
            SET FILTER TO Code == 2
            ? "filter active at server:", LETO_ISFLTOPTIM()
            __dbTrans( SELECT( "TEST4" ) )                               // executed at server
            SET FILTER TO
            DbSelectArea( "TEST4" )
            ? "*", nRec + 5, " 2 Second              2"
            ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc
            ?

            DbSelectArea( "TEST" )
            leto_varSet( "main", "dbtran_filt", nRecord, LETO_VCREAT + LETO_VOWN )
            SET FILTER TO Code == leto_varGetCached()                    // or slower: leto_varGet( "main", "dbtran_filt" )
            ? "filter active at server:", LETO_ISFLTOPTIM()
            __dbTrans( SELECT( "TEST4" ) )                               // executed at server
            SET FILTER TO                                                // clear filter before the var
            leto_varDel( "main","dbtran_filt" )
            DbSelectArea( "TEST4" )
            ? "*", nRec + 6, " 2 Second              2"
            ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc
            ?

            ? "!!! next OP intentional fails --> RTE !!!"
            WAIT

            /* codeblock is created at RUN-time, so it does not know about LOCALs at COMPILE-time */
            ? Leto_dbTrans( SELECT( "TEST4" ),, "RECNO() == nRec" )      // FAIL because no PRIVATE/ PUBLIC var

            DbSelectArea( "TEST" )
            DBUNLOCK()
            WAIT
            QUIT
         ELSE
            ? "Shared error using TEST4"
            WAIT
            QUIT
         ENDIF
      ELSE
         SORT TO mem:test4 ON name /D
         DBUNLOCK()
         DbUseArea( .t.,, "mem:test4",, .T. )
      ENDIF
   ENDIF
   DbUseArea( .t.,, cPath + 'test3' )
   ? "*          3  3 Third               3 Pavel"
   ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc, Field->Text

   DbSelectArea( "TEST" )
   DbGoTop()
   nRecord := 2
   /* stringifiyed! block fail at server because local memvar, so try afterwards at client */
   COPY to "test5" FOR { || Code == nRecord }
   IF DbExists( cPath + "test5" )
      IF DbUseArea( .t.,, cPath + 'test5',, .T. )
         ? "Copy Test"
         ? "*", 1, " 2 Second              2"
         ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc
         DbCloseArea()
      ENDIF
      hb_dbDrop( "test5" )
   ENDIF

   DbSelectArea( "TEST" )
   ? __DBTRANS( SELECT( "TEST2" ), { "Code", "Name", "Text" }, "Code==2" )
   DbSelectArea( "TEST2" )
   DbGoBottom()
   ? "Dbtrans Test"
   ? "*          4  2 Second              4 Elch"
   ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc, Field->Text
   IF DbAppend()
      REPLACE Field->Code WITH 5,;
              Field->Name WITH "Forth"
      DbUnlock()
   ENDIF

   ?
   WAIT

   DbSelectArea( "TEST" )
   ? __DBTRANS( SELECT( "TEST2" ), { "Code", "Name" }, {|| Code == 3 } )
   DbSelectArea( "TEST2" )
   ? "*          6  3 Third               6"
   ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc, Field->Text
   IF DbAppend()
      REPLACE Field->Code WITH 7,;
              Field->Name WITH "Forth"
      DbUnlock()
   ENDIF

   IF FLOCK()
      ? Leto_dbTrans( SELECT( "TEST4" ) )   // TEST2 --> TEST4
   ENDIF
   DbSelectArea( "TEST4" )
   ? "*         10  7 Forth               7"
   ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc, Field->Text
   ?

   /* HbMemIO from server to local HbMemIO */
   IF DbCreate( 'mem:test5',;
                { { 'Code', 'N', 2, 0 },{ 'Name', 'C', 10, 0 },{ 'Text', 'M', 10, 0 }, { 'AutoInc', '+', 4, 0 } },;
                "DBFNTX", .T., "TEST5" )
      DbSelectArea( "TEST4" )
      Leto_dbTrans( SELECT( "TEST5" ) )
      DbSelectArea( "TEST5" )
      ? "*         10  7 Forth               7"
      ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc, Field->Text
      ?
      DbCloseArea()
      hb_dbDrop( "mem:test5.dbf",, "DBFNTX" )   // DBFNTX to force local opposite to LETO
      IF DbExists( "mem:test5.dbf",, "DBFNTX" )
         ? "Fail to drop local file 'mem:test5.dbf"
      ENDIF
   ENDIF

   /* HbMemIO from server to local DBF */
   IF DbCreate( 'test6',;
                { { 'Code', 'N', 2, 0 },{ 'Name', 'C', 10, 0 },{ 'Text', 'M', 10, 0 }, { 'AutoInc', '+', 4, 0 } },;
                "DBFNTX", .T., "TEST6" )
      DbSelectArea( "TEST4" )
      __dbTrans( SELECT( "TEST6" ) )
      DbSelectArea( "TEST6" )
      ? "*         10  7 Forth               7"
      ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc, Field->Text
      DbCloseArea()
      hb_dbDrop( "test6.dbf",, "DBFNTX" )   // DBFNTX to force local opposite to LETO
      IF DbExists( "test6.dbf",, "DBFNTX" )
         ? "Fail to drop local file 'test6.dbf"
      ENDIF
   ENDIF

   /* HbMemIO from server to named temporary DBF */
   IF LETO_DBCREATETEMP( "test7", { { 'Code', 'N', 2, 0 },{ 'Name', 'C', 10, 0 },{ 'Text', 'M', 10, 0 }, { 'AutoInc', '+', 4, 0 } },;
                         "LETO", .T., "TEST7" )
      DbSelectArea( "TEST4" )
      __dbTrans( SELECT( "TEST7" ) )
      DbSelectArea( "TEST7" )
      ?
      ? "Named temporary table"
      ? "*         10  7 Forth               7"
      ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc, Field->Text
      DbCloseArea()
      IF DbExists( "test7.dbf",, "DBFNTX" )
         ? "Fail to drop local file 'test7.dbf"
      ELSE
         ? "Success to delete temporary table 'test7'"
      ENDIF
   ENDIF

   DbSelectArea( "TEST4" )
   nRec := Reccount()
   DbSelectArea( "TEST" )
   nRec *= Reccount()
   IF nRec > 0
      ?
      IF Leto_dbJoin( "test4", "test10", { "name", "test4->name", "name", "test4->name","name", "test4->name","name", "test4->name","name", "test4->name","name", "test4->name","name", "test4->name","name", "test4->name","name", "test4->name","name", "test4->name" },,,,, .T. )
         ? "leto_DbJoin() records", RecCount(), " == ", nRec
      ENDIF
      DbSelectArea( "TEST" )
   ENDIF

   ?
   ? "may start a second instance of this app, but END it before continue..."
   WAIT

   DbCloseAll()
   ? hb_dbDrop( cPath + "test.dbf" )
   ? hb_dbDrop( cPath + "test2.dbf" )
   ? hb_dbDrop( cPath + "test3.dbf" )
   ? hb_dbDrop( "mem:test4.dbf" )
   WAIT

RETURN nil

