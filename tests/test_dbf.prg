/*
 * This sample tests working with dbf files
 * Just change the cPath value to that one you need.
 */

REQUEST LETO
REQUEST DBFCDX

#include "dbinfo.ch"

Function Main( cPath )
 LOCAL aNames := { "Petr", "Ivan", "Alexander", "Pavel", "Alexey", "Fedor", ;
                  "Konstantin", "Vladimir", "Nikolay", "Andrey", "Dmitry", "Sergey" }
 LOCAL i, aStru, aServerDriver
 LOCAL nPort := 2812
 FIELD NAME, NUM, INFO, DINFO, MINFO

   ALTD()
   SET DATE FORMAT "dd/mm/yy"

   IF Empty( cPath )
      //cPath := "//127.0.0.1:2812/temp/"
      cPath := ""
      RDDSETDEFAULT( "DBFCDX" )
   ELSE
      cPath := "//" + cPath + IiF( ":" $ cPath, "", ":" + ALLTRIM( STR( nPort ) ) )
      cPath += Iif( Right(cPath,1) == "/", "", "/" )
      RDDSETDEFAULT( "LETO" )
   ENDIF

   IF dbCreate( cPath + "test1", { { "NAME",  "C", 10, 0 },;
                                   { "NUM" ,  "N",  4, 0 },;
                                   { "INFO",  "C", 32, 0 },;
                                   { "DINFO", "D",  8, 0 },;
                                   { "MINFO", "M", 10, 0 } } )
      ? "File has been created"
   ENDIF

   USE ( cPath + "test1" ) NEW
   IF ! NetErr() .AND. ! EMPTY( ALIAS() )
      ? "File has been opened"
   ELSE
      ? "ERROR opening database! -- press any key to quit"
      Inkey( 0 )
      QUIT
   ENDIF
   aStru := dbStruct()
   ? "Fields:", Len( aStru )
   FOR i := 1 TO Len( aStru )
      ? i, aStru[i,1], aStru[i,2], aStru[i,3], aStru[i,4]
   NEXT

   FOR i := 1 TO Len( aNames )
      APPEND BLANK
      REPLACE NAME  WITH aNames[ i ],;
              NUM   WITH i + 1000,;
              INFO  WITH "This is a record number " + Ltrim( Str( i ) ),;
              DINFO WITH Date() + i - 1,;
              MINFO WITH aNames[ i ]
   NEXT
   ? LEN( aNames ), "Records has been added"
   INDEX ON NAME TAG NAME
   ? "INDEX KEY 1:", indexkey( 1 )
   INDEX ON Str(NUM,4) TAG NUM
   ? "INDEX KEY 2:", indexkey( 2 )
   ? "File has been indexed, "
   ?? DBORDERINFO( DBOI_ORDERCOUNT )
   ?? " active orders "
   ?? Iif( DBORDERINFO( DBOI_ORDERCOUNT ) == 2, "- Ok","- Failure" )

   ?
   ? "Press any key to continue..."
   Inkey( 0 )

   i := RecCount()
   ? "Reccount ", i, Iif( i == Len( aNames ), "- Ok","- Failure" )

   GO TOP
   ? "go top", NUM, NAME, DINFO, Iif( NUM == 1001, "- Ok","- Failure" )
   REPLACE INFO WITH "First", MINFO WITH "First"

   GO BOTTOM
   ? "go bottom", NUM, NAME, DINFO, Iif( NUM == 1012, "- Ok","- Failure" )
   REPLACE INFO WITH "Last", MINFO WITH "Last"

   ?
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

   ?
   ? 'ordSetFocus( "NUM" ), SET SCOPE TO "1009", "1011"'
   ordSetFocus( "NUM" )
   SET SCOPE TO "1009", "1011"

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

   IF RDDSETDEFAULT() == "LETO"
      aServerDriver := leto_DbDriver()
   ENDIF
   IF "CDX" $ RDDSETDEFAULT() .OR. ( VALTYPE( aServerDriver ) == "A" .AND. "CDX" $ aServerDriver[ 1 ] )
      USE ( cPath + "test1" ) NEW
      i := 0
      ? "auto opened index Tags:"
      DO WHILE ! Empty( Ordkey( ++i ) )
         ? i, ordKey( i )
      ENDDO
   ENDIF

   dbCloseAll()
   ?
   ? "dropping test DBF: "
   ?? Iif( DbDrop( cPath + "test1" ), "- Ok","- Failure" )

   ?
   ? "Press any key to finish ..."
   Inkey( 0 )

Return Nil
