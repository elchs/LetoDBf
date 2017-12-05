/*
 * This sample tests working with dbf files
 * Just change the cPath value to that one you need.
 */

REQUEST LETO
REQUEST DBFCDX

REQUEST leto_VarGet, leto_varSet, leto_varGetCached, leto_varDel
REQUEST DbSetIndex, DbClearIndex

#include "dbinfo.ch"
#include "ord.ch"

MEMVAR nNumTop, nNumBot

Function Main( cPath )
 LOCAL aNames := { "Petr", "Ivan", "Alexander", "Pavel", "Alexey", "Fedor", ;
                  "Konstantin", "Vladimir", "Nikolay", "Andrey", "Dmitry", "Sergey" }
 LOCAL i, ii, aStru
 LOCAL nPort := 2812
 LOCAL cName
 FIELD NAME, NUM, INFO, DINFO, MINFO
 PRIVATE nNumTop, nNumBot

   ALTD()
   SET DATE FORMAT "dd/mm/yy"

   IF Empty( cPath )  /* try it at local hard drive */
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
      ? i, PadR( aStru[i,1], 10 ), aStru[i,2], aStru[i,3], aStru[i,4]
   NEXT

   FOR ii := 1 TO 21
      FOR i := 1 TO Len( aNames )
         APPEND BLANK
         REPLACE NAME  WITH aNames[ i ],;
                 NUM   WITH i + 1000,;
                 INFO  WITH "This is a record number " + Ltrim( Str( i ) ),;
                 DINFO WITH Date() + i - 1,;
                 MINFO WITH STR( ii ) + STR( i )
      NEXT i
   NEXT ii
   ? LEN( aNames ) * 21, "Records has been added"
   INDEX ON NAME TAG NAME
   ? "INDEX KEY 1:", IndexKey( 1 )
   INDEX ON NUM TAG NUM ADDITIVE
   ? "INDEX KEY 2:", IndexKey( 2 )
   ? "File has been indexed; "

   ?? DBORDERINFO( DBOI_ORDERCOUNT )
   ?? " active orders, ", IIF( DBORDERINFO( DBOI_ORDERCOUNT ) == 2, "- Ok", "- Failure" )
   ? " active key:", IndexOrd(), IndexKey()

   ?
   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ? "Testing filtering for "
   IF RDDSETDEFAULT() == "LETO"
      ?? Leto_DBDriver()[ 1 ]
   ELSE
      ?? "DBFCDX"
   ENDIF
   ?
   ? 'ordSetFocus( "NAME" )'
   ordSetFocus( "NAME" )

   SEEK "Petr"
   ? "seek      ", NUM, NAME, DINFO, Iif( NUM == 1001, "- Ok","- Failure" )
   SEEK "Andre"
   ? "seek      ", NUM, NAME, DINFO, Iif( NUM == 1010, "- Ok","- Failure" )


   SET FILTER TO NUM >= 1004 .AND. NUM <= 1010
   ?
   ? DbFilter(), "; optimized:", LETO_ISFLTOPTIM()
   GO TOP
   ? "go top    ", NUM, NAME, DINFO, Iif( NUM == 1005, "- Ok","- Failure" )

   GO BOTTOM
   ? "go bottom ", NUM, NAME, DINFO, Iif( NUM == 1008, "- Ok","- Failure" )

   SEEK "Petr"
   ? "seek      ", NUM, NAME, DINFO, Iif( EOF(), "- Ok","- Failure" )
   SEEK "Andre"
   ? "seek      ", NUM, NAME, DINFO, Iif( NUM == 1010, "- Ok","- Failure" )

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   nNumTop := 1004
   nNumBot := 1010
   SET FILTER TO NUM >= nNumTop .AND. NUM <= nNumBot
   ? "with FORCEOPT = .F."
   ? DbFilter(), "; optimized:", LETO_ISFLTOPTIM()

   ?
#ifndef __XHARBOUR__  /* -> RTE cause of missing filter sync */
   SET( _SET_FORCEOPT, .T. )
#endif
   SET FILTER TO NUM >= nNumTop .AND. NUM <= nNumBot
   ? "with FORCEOPT = .T."
   ? DbFilter()
   ? "--> optimized:", LETO_ISFLTOPTIM()

   GO TOP
   ? "go top    ", NUM, NAME, DINFO, Iif( NUM == 1005, "- Ok","- Failure" )

   IF LETO_ISFLTOPTIM()  /* memvar nNumTop/ nNumBot will be synced with server */
      ? "change nNumTop to 1006"
      nNumTop := 1006
      GO TOP
      ? "go top    ", NUM, NAME, DINFO, Iif( NUM == 1010, "- Ok","- Failure" )
      ? "change nNumTop back to 1004"
      nNumTop := 1004
      DbSkip( -42 )
      ? "go top    ", NUM, NAME, DINFO, Iif( NUM == 1005, "- Ok","- Failure" )
   ENDIF

   GO BOTTOM
   ? "go bottom ", NUM, NAME, DINFO, Iif( NUM == 1008, "- Ok","- Failure" )

   SEEK "Petr"
   ? "seek      ", NUM, NAME, DINFO, Iif( EOF(), "- Ok","- Failure" )
   SEEK "Andre"
   ? "seek      ", NUM, NAME, DINFO, Iif( NUM == 1010, "- Ok","- Failure" )

   SET( _SET_FORCEOPT, .F. )
   ?
   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   cName := "Alex"
   SET FILTER TO cName $ NAME
   ?
   ? DbFilter(), "; '" + cName + "' $ NAME", " - optimized:", LETO_ISFLTOPTIM()
   GO TOP
   ? "go top    ", NUM, NAME, DINFO, Iif( ALLTRIM( NAME ) == "Alexander", "- Ok","- Failure" )

   GO BOTTOM
   ? "go bottom ", NUM, NAME, DINFO, Iif( ALLTRIM( NAME ) == "Alexey", "- Ok","- Failure" )

   SEEK "Petr"
   ? "seek      ", NUM, NAME, DINFO, Iif( EOF(), "- Ok","- Failure" )

   i := 0
   GO TOP
   DO WHILE ! EOF()
      i++
      SKIP
   ENDDO
   ? "count     ", i, Iif( i == 42, "- Ok","- Failure" )

   ?
   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ?
   SET FILTER TO
   ? "SET FILTER TO", DbFilter()

   GO TOP
   ? "go top    ", NUM, NAME, DINFO, Iif( NUM == 1003, "- Ok","- Failure" )

   GO BOTTOM
   ? "go bottom ", NUM, NAME, DINFO, Iif( NUM == 1008, "- Ok","- Failure" )

   SEEK "Petr"
   ? "seek      ", NUM, NAME, DINFO, Iif( NUM == 1001, "- Ok","- Failure" )
   SEEK "Andre"
   ? "seek      ", NUM, NAME, DINFO, Iif( NUM == 1010, "- Ok","- Failure" )


   SET SCOPE TO "Ivan", "Nikolay"
   ?
   ? "SCOPE >= ", OrdScope( TOPSCOPE ), " <= ", OrdScope( BOTTOMSCOPE )
   GO TOP
   ? "go top    ", NUM, NAME, DINFO, Iif( ALLTRIM( NAME ) == "Ivan", "- Ok","- Failure" )

   GO BOTTOM
   ? "go bottom ", NUM, NAME, DINFO, Iif( ALLTRIM( NAME ) == "Nikolay", "- Ok","- Failure" )

   SEEK "Konstantin"
   ? "seek      ", NUM, NAME, DINFO, Iif( ALLTRIM( NAME ) == "Konstantin", "- Ok","- Failure" )
   SEEK "Petr"
   ? "seek      ", NUM, NAME, DINFO, Iif( EOF(), "- Ok","- Failure" )

   ?
   ? "Press any key to continue..."
   Inkey( 0 )
   dbCloseAll()

   ?
   ? "dropping test DBF: "
   ?? Iif( DbDrop( cPath + "test1" ), "- Ok","- Failure" )

   ?
   ? "Press any key to finish ..."
   Inkey( 0 )

Return Nil
