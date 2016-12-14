/*
 * This sample tests working with dbf files
 * Just change the cPath value to that one you need.
 */
REQUEST LETO_CONNECTINFO
REQUEST DBORDERINFO, ORDLISTCLEAR, ORDBAGCLEAR, ORDDESTROY
REQUEST LETO
REQUEST DBFCDX

#include "dbinfo.ch"
#include "set.ch"

Function Main( cPath )
 LOCAL aNames := { "Petr", "Ivan", "Alexander", "Pavel", "Alexey", "Fedor", ;
                   "Konstantin", "Vladimir", "Nikolay", "Andrey", "Dmitry", "Sergey" }
 LOCAL i, aStru
 LOCAL nPort := 2812
 LOCAL nHotbuf := 100
 LOCAL nTimeOut := 60000
 LOCAL nKey
 FIELD NAME, NUM, INFO, DINFO, MINFO

   SET DATE FORMAT "dd/mm/yy"
   ALTD()

   IF Empty( cPath )
      cPath := ""
      RDDSETDEFAULT( "DBFCDX" )
   ELSE
      cPath := "//" + cPath + IiF( ":" $ cPath, "", ":" + ALLTRIM( STR( nPort ) ) )
      cPath += Iif( Right(cPath,1) == "/", "", "/" )
      RDDSETDEFAULT( "LETO" )
   ENDIF

   IF RDDSETDEFAULT() == "LETO"
      IF EMPTY( cPath ) .OR. leto_Connect( cPath, /*user*/, /*pass*/, nTimeOut /*timeout*/, nHotBuf /*hot buffer*/ ) == -1
         ALERT("NO LETODB SERVER FOUND - Fehler: " + leto_Connect_Err( .T. ) )
         QUIT
      ELSE
         ? LETO_GetServerVersion()
         // LETO_DBDRIVER( "DBFCDX" )
         // LETO_DBDRIVER( "DBFNTX" )
         ? "DBF DATABASE DRIVER        :", LETO_DBDRIVER()[ 1 ], "MEMOTYPE:", LETO_DBDRIVER()[ 2 ] 
         LETO_TOGGLEZIP( 1 )
         ? "NETWORK TRAFFIC COMPRESSION:", Iif( LETO_TOGGLEZIP() > 0, "ON", "OFF" )
      ENDIF
   ENDIF

   ? "DBF DATABASE EXTENSION     :", hb_rddInfo( RDDI_TABLEEXT )

   IF ! DbExists( "test1" )
      IF dbCreate( "test1", { { "NAME",  "C", 10, 0 },;
                              { "NUM",   "N",  4, 0 },;
                              { "INFO",  "C", 32, 0 },;
                              { "DINFO", "D",  8, 0 },;
                              { "TINFO", "T", 17, 0 },;
                              { "MINFO", "M", 10, 0 } } )
         ? "File has been created"
      ELSE
         ALERT( "DBF CREATE FAILED" + IIF( NetErr(), ", TABLE IN USE BY OTHER", "" ) )
         QUIT
      ENDIF
   ENDIF

   USE ( "test1" ) SHARED NEW
   IF ! NetErr() .AND. ! EMPTY( ALIAS() )
      ? "File has been opened"
   ELSE
      ? "ERROR opening database! -- press any key to quit"
      Inkey( 0 )
      QUIT
   ENDIF

   ? "Lockscheme    :", hb_rddInfo( RDDI_LOCKSCHEME ), dbinfo( DBI_LOCKSCHEME )
   ? "Memo extension:", hb_rddInfo( RDDI_MEMOEXT ), dbinfo( DBI_MEMOEXT )
   ? "     type     :", hb_rddInfo( RDDI_MEMOTYPE )
   ? "     blocksize:", hb_rddInfo( RDDI_MEMOBLOCKSIZE )

   aStru := dbStruct()
   ? "Fields:", Len( aStru )
   FOR i := 1 TO Len( aStru )
      ? i, aStru[ i, 1 ], aStru[ i, 2 ], aStru[ i, 3 ], aStru[ i, 4 ]
   NEXT

   ?
   ? "Press any key to continue..."
   Inkey( 0 )

   IF RecCount() == 0
      FOR i := 1 TO Len( aNames )
         APPEND BLANK
         REPLACE NAME  WITH aNames[ i ],;
                 NUM   WITH i + 1000, ;
                 INFO  WITH "This is a record number "+Ltrim(Str(i)), ;
                 DINFO WITH Date() + i - 1, ;
                 MINFO WITH "elk test" + STR( i, 10, 0)
      NEXT
      ? LEN( aNames ), "Records has been added"
      INDEX ON NAME TAG NAME
      ? "INDEX KEY 1:", IIF( indexord() == 1, "(ok)", "(fail)" ), indexkey( 1 )
      INDEX ON Str(NUM,4) TAG NUMS
      ? "INDEX KEY 2:", IIF( indexord() == 2, "(ok)", "(fail)" ), indexkey( 2 )
      INDEX ON NUM TAG NUMI TO test2
      ? "INDEX KEY 3:", IIF( indexord() == 3, "(ok)", "(fail)" ), indexkey( 3 )
      INDEX ON DINFO TAG DINFO
      ? "INDEX KEY 4:", IIF( indexord() == 3, "(ok)", "(fail)" ), indexkey( 3 )
      ? "Table now indexed"
      IF RDDINFO( RDDI_MULTITAG )
         ?? ", all in one file"
      ENDIF
      ?? " with extension: ", RDDInfo( RDDI_ORDEREXT ), ";"
   ELSE
      ? "File was indexed"
      DbSetIndex( "test2" )
   ENDIF
   ?? STR( DBORDERINFO( DBOI_ORDERCOUNT ), 2, 0 )
   ?? " orders active "
   ?? Iif( DBORDERINFO( DBOI_ORDERCOUNT ) == 3, "- Ok","- Failure" )
   DBSETORDER( 0 )
   ? "table locked", IIF( FLock(), "(Ok)", "Failure" )

   i := RecCount()
   ? "Reccount ", i, Iif( i == Len( aNames ), "- Ok","- Failure" )

   GO TOP
   ? "go top", NUM, NAME, DINFO, Iif( NUM == 1001, "- Ok","- Failure" )
   REPLACE INFO WITH "First", MINFO WITH "First"

   GO BOTTOM
   ? "go bottom", NUM, NAME, DINFO, Iif( NUM == 1012, "- Ok","- Failure" )
   REPLACE INFO WITH "Last", MINFO WITH "Last"

   ? "table unlocked", DbUnlock()
   ? 'ordSetFocus( "NAME" )'
   ordSetFocus( "NAME" )
   GO TOP
   ? "go top", NUM, NAME, DINFO, Iif( NUM == 1003, "- Ok","- Failure" )

   SKIP
   ? "skip", NUM, NAME, DINFO, Iif( NUM == 1005, "- Ok","- Failure" )

   GO BOTTOM
   ? "go bottom", NUM, NAME, DINFO, Iif( NUM == 1008, "- Ok","- Failure" )

   SKIP -1
   ? "skip -1", NUM, NAME, DINFO, Iif( NUM == 1012, "- Ok","- Failure" )

   SEEK "Petr"
   ? "seek", NUM, NAME, DINFO, Iif( NUM == 1001, "- Ok","- Failure" )
   SEEK "Andre"
   ? "seek", NUM, NAME, DINFO, Iif( NUM == 1010, "- Ok","- Failure" )

   SET FILTER TO NUM >= 1004 .AND. NUM <= 1010
   ?
   ? "SET FILTER TO NUM >= 1004 .AND. NUM <= 1010"
   GO TOP
   ? "go top", NUM, NAME, DINFO, Iif( NUM == 1005, "- Ok","- Failure" )

   SKIP
   ? "skip", NUM, NAME, DINFO, Iif( NUM == 1010, "- Ok","- Failure" )

   GO BOTTOM
   ? "go bottom", NUM, NAME, DINFO, Iif( NUM == 1008, "- Ok","- Failure" )

   SKIP -1
   ? "skip -1", NUM, NAME, DINFO, Iif( NUM == 1004, "- Ok","- Failure" )

   ? "Press any key to continue..."
   Inkey(0)

   ?
   ? "SET FILTER TO, SET ORDER TO 0"
   SET FILTER TO
   SET ORDER TO 0

   GO TOP
   ? "First record", Iif( ALLTRIM( INFO ) == "First" .AND. MINFO == "First", "- Ok","- Failure" )

   GO BOTTOM
   ? "Last record", Iif( ALLTRIM( INFO ) == "Last" .AND. MINFO == "Last", "- Ok","- Failure" )

   SET FILTER TO NUM >= 1009
   ?
   ? 'ordSetFocus( "NUMS" ), SET SCOPE TO "1009", "1011"'
   ordSetFocus( "NUMS" )
   SET SCOPE TO "1009", "1011"

   GO TOP
   ? "go top", NUM, NAME, DINFO, Iif( NUM == 1009, "- Ok","- Failure" )

   SKIP
   ? "skip", NUM, NAME, DINFO, Iif( NUM == 1010, "- Ok","- Failure" )

   SKIP
   ? "skip", NUM, NAME, DINFO, Iif( NUM == 1011, "- Ok","- Failure" )

   SKIP
   ? "skip", NUM, NAME, DINFO, Iif( Eof(), "- Ok","- Failure" )

   SET FILTER TO NUM >= 1009
   ?
   ? 'ordSetFocus( "NUMS" ), SET FILTER TO NUM >= 1009, SET SCOPE TO NIL, "1011"'
   ordSetFocus( "NUMS" )
   SET SCOPE TO
   SET SCOPE TO , "1011"

   GO TOP
   ? "go top", NUM, NAME, DINFO, Iif( NUM == 1009, "- Ok","- Failure" )

   SKIP
   ? "skip", NUM, NAME, DINFO, Iif( NUM == 1010, "- Ok","- Failure" )

   SKIP
   ? "skip", NUM, NAME, DINFO, Iif( NUM == 1011, "- Ok","- Failure" )

   SKIP
   ? "skip", NUM, NAME, DINFO, Iif( Eof(), "- Ok","- Failure" )

   dbCloseAll()
   ?
   ? "Press any key to continue..."
   Inkey( 0 )

   USE ( "test1" ) SHARED New
   i := 0
   ? "AutoOpened Tags:"
   DO WHILE ! Empty( Ordkey( ++i ) )
      ? i, ordKey( i )
   ENDDO
   i := DBORDERINFO( DBOI_ORDERCOUNT )
   IF RDDInfo( RDDI_STRUCTORD )
      ? "Active orders ", i, Iif( i == 3, "- Ok","- Failure" )
      ? "Focus set to ", indexord(), Iif( indexord() == SET( _SET_AUTORDER ), "- Ok","- Failure" )
   ELSE
      ? "Indextype does not support to auto-open index, found:", STR( i, 1, 0 ), " TAGs"
   ENDIF
   OrdListAdd( "test2" )
   ? "Destroy TAG <NUMI> in BAG <test2>", IIF( OrdDestroy( "NUMI" ) .AND. ! hb_dbExists( "test2.cdx" ), "- Ok","- Failure" )

   DBCLOSEALL()

   ?
   ? "Press ENTER to delete test DBF, any other key to finish, ..."
   nKey := Inkey( 0 )

   IF nKey == 13
      hb_dbDrop( "test1.dbf" )
      IF hb_dbexists( "test1.dbf" ) .OR.;
         hb_dbExists( "test1.cdx" ) .OR.;
         hb_dbexists( "test1.fpt" )
         ? "drop dbf: - Failure "
      ELSE
         ? "drop dbf: -  Ok"
      ENDIF

      ?
      ? "Press any key to finish ..."
      INKEY( 0 )
   ENDIF

Return Nil

