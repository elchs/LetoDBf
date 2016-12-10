/*
 * This sample tests working with dbf files
 * Just change the cPath value to that one you need.
 */

#define __MEM_IO__ 1
#define FIELD_IDS  1    // 341 * 5000 nRounds = ~ 4GB

REQUEST DBORDERINFO, ORDLISTCLEAR, ORDBAGCLEAR, ORDDESTROY
REQUEST LETO
REQUEST DBFNTX
REQUEST hb_UTF8ToStr

#include "dbinfo.ch"
#include "set.ch"

Function Main( cPath )
 LOCAL aNames := { "Petr", "Ivan", "Alexander", "Pavel", "Alexey", "Fedor", ;
                   "Konstantin", "Vladimir", "Nikolay", "Andrey", "Dmitry", "Sergey" }
 LOCAL i, ii, aStru
 LOCAL nPort := 2812
 LOCAL nHotbuf := 100
 LOCAL nKey, nSec
 LOCAL cFile1 := "test1"
 LOCAL cFile2 := "test2"
 LOCAL nRounds := 10000
 LOCAL nDeleted
 LOCAL aStruct, aStructAll := {}
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
      IF EMPTY( cPath ) .OR. leto_Connect( cPath, /*user*/, /*pass*/, /*timeout*/, nHotBuf /*hot buffer*/ ) == -1
         ALERT("NO LETODB SERVER FOUND" )
         QUIT
      ELSE
         ? LETO_GetServerVersion()
#ifdef __MEM_IO__
         IF ! ".2." $ LETO_GetServerVersion()
            cFile1 := "mem:" + cFile1
            cFile2 := "mem:" + cFile2
         ENDIF
#endif
         // LETO_TOGGLEZIP( 1 )
         // ? "NETWORK TRAFFIC COMPRESSION:", Iif( LETO_TOGGLEZIP() > 0, "ON", "OFF" )
      ENDIF
   ELSE
#ifdef __MEM_IO__
         IF ! ".2." $ LETO_GetServerVersion()
            cFile1 := "mem:" + cFile1
            cFile2 := "mem:" + cFile2
         ENDIF
#endif
   ENDIF

   IF ! DbExists( cFile1 )

#if 0  // testing auto-incrementing fields, only LetoDBf can do
      IF dbCreate( cFile1, { { "NAME",    "C", 10, 0 },;
                             { "NUM",     "N",  4, 0 },;
                             { "INFO",    "C", 32, 0 },;
                             { "DINFO",   "D",  8, 0 },;
                             { "TINFO",   "T", 17, 0 },;
                             { "MINFO",   "M", 10, 0 },;
                             { "INTEGER", "I:+", 4, 0 },;
                             { "AUTOINC", "+",  4, 0 } } )
#else
      aStruct := { { "NAME",  "C", 10, 0 },;
                   { "NUM",   "N",  4, 0 },;
                   { "INFO",  "C", 64, 0 },;
                   { "DINFO", "D",  8, 0 },;
                   { "TINFO", "T", 17, 0 },;
                   { "MINFO", "M", 10, 0 } }
      FOR i := 1 TO FIELD_IDS
         FOR ii := 1 TO 6
            AADD( aStructAll, ACLONE( aStruct[ ii ] ) )
            IF i > 1
               aStructAll[ ( ( i - 1 )  * 6 ) + ii, 1 ] := ALLTRIM( aStruct[ ii, 1 ] ) + hb_ntos( i )
            ENDIF
         NEXT ii
      NEXT i
      IF dbCreate( cFile1, aStructAll )
#endif
         ? "File " + cFile1 + hb_rddInfo( RDDI_TABLEEXT ) + " has been created"
      ELSE
         ALERT( "DBF CREATE FAILED" + IIF( NetErr(), ", TABLE IN USE BY OTHER", "" ) )
         QUIT
      ENDIF
   ENDIF

   USE ( cFile1 ) SHARED NEW
   IF ! NetErr() .AND. ! EMPTY( ALIAS() )
      ? "File " + cFile1 + hb_rddInfo( RDDI_TABLEEXT ) + " have been opened shared"
   ELSE
      ? "ERROR opening database! -- press any key to quit"
      Inkey( 0 )
      QUIT
   ENDIF

   aStru := dbStruct()
   ? "Fields:", Len( aStru )
   FOR i := 1 TO Len( aStru )
      ? i, PadR( aStru[ i, 1 ], 10 ), aStru[ i, 2 ], STR( aStru[ i, 3 ], 5, 0 ), aStru[ i, 4 ]
   NEXT

   ?
   IF RecCount() == 0
      ? "please wait, appending " + hb_ntos( nRounds * LEN( aNames ) ) + " records ..."
      nSec := hb_milliseconds()

      IF FLock()
         FOR ii := 1 TO nRounds
            FOR i := 1 TO Len( aNames )
               IF DbAppend()
                  /* Harbour returns .T. if successful, else alike for: APPEND BLANK, check for NetErr() before proceeding */
                  REPLACE NAME  WITH aNames[ i ],;
                          NUM   WITH i + 1000, ;
                          INFO  WITH "This is a record number "+ Ltrim( Str( i ) ),;
                          DINFO WITH Date() + i - 1, ;
                          MINFO WITH "elk test" + STR( i, 10, 0 )
               ENDIF
            NEXT i
         NEXT ii
         DbUnlock()
      ENDIF

      ?? STR( ( hb_milliseconds() - nSec ) / 1000, 6, 2 ), "s -- "
      ?? STR( ( Len( aNames ) * nRounds ) / ( ( hb_milliseconds() - nSec ) / 1000 ), 7, 0 ), "/ s"
      DbUnLock()
      IF RDDSETDEFAULT() == "LETO"
         ? "file size: ", Leto_FileSize( cFile1 + hb_rddInfo( RDDI_TABLEEXT ) ) / 1024 / 1024, "MB; record length " +;
                          hb_ntos( dbInfo( DBI_GETRECSIZE ) )
      ELSE
         ? "file size: ", hb_FSize( cFile1 + hb_rddInfo( RDDI_TABLEEXT ) ) / 1024 / 1024, "MB"
      ENDIF
      nSec := hb_milliseconds()
      INDEX ON NAME TAG NAME TO ( cFile1 )
      ? ( hb_milliseconds() - nSec ) / 1000, "s, INDEX KEY 1:", IIF( indexord() == 1, "(ok)", "(fail)" ), indexkey( 1 )
      nSec := hb_milliseconds()
      INDEX ON Str(NUM,4) TAG NUMS TO ( cFile2 ) ADDITIVE
      ? ( hb_milliseconds() - nSec ) / 1000, "s, INDEX KEY 2:", IIF( indexord() == 2, "(ok)", "(fail)" ), indexkey( 2 )
      ? "File has been indexed"
   ELSE
      IF RDDSETDEFAULT() == "LETO"
         ? RecCount(), "records, file size: ", Leto_FileSize( cFile1 + hb_rddInfo( RDDI_TABLEEXT ) ) / 1024 / 1024, "MB"
      ELSE
         ? RecCount(), "records, file size: ", hb_FSize( cFile1 + hb_rddInfo( RDDI_TABLEEXT ) ) / 1024 / 1024, "MB"
      ENDIF
      IF ! DbExists( cFile1 + hb_rddInfo( RDDI_ORDBAGEXT ) )
         INDEX ON NAME TAG NAME TO ( cFile1 )
      ENDIF
      IF ! DbExists( cFile2 + hb_rddInfo( RDDI_ORDBAGEXT ) )
         INDEX ON Str(NUM,4) TAG NUMS TO ( cFile2 )
      ENDIF
      IF EMPTY( IndexKey( 1 ) )  /* auto open */
         DbSetIndex( cFile1 )
      ENDIF
      DbSetIndex( cFile2 )
   ENDIF

/* --- benchmarks --- */

   ? "skipping top to bottom ..."
   FOR i := 1 TO 5
     LETO_SETSKIPBUFFER( 21 * i * i )
     nSec := hb_milliseconds()
      DbGoTop()
      DO WHILE ! EOF()
        DbSkip( 1 )
      ENDDO
      ?? STR( ( hb_milliseconds() - nSec ) / 1000, 7,2 ), "s;"
   NEXT i
   LETO_SETSKIPBUFFER( 21 )

   ? "GoTop, seek ..."
   DbSetorder( 1 )
   nSec := hb_milliseconds()
   DbGoTop()
   ii := LEN( aNames )
   FOR i := 1 TO 10000
      DbGoTop()
      IF ! DbSeek( aNames[ ( i % ii ) + 1 ] )
         EXIT
      ENDIF
   NEXT i
   ?? STR( i - 1, 6, 0 ), "times, ", STR( ( hb_milliseconds() - nSec ) / 1000, 7, 2 ), "s"

   IF FLock()
      ? "FILE locked, skip through records, data change two fields ..."
      nSec := hb_milliseconds()
      DbGoTop()
      DO WHILE ! EOF()
         Replace INFO  WITH "This is a record number " + hb_ntos( RECNO() ),;
                 DINFO WITH DATE(),;
                 MINFO WITH "elk tested" + STR( RECNO(), 10, 0 )
         DbSkip( 1 )
      ENDDO
      ?? STR( ( hb_milliseconds() - nSec ) / 1000, 7, 2 ), "s"
      DbUnLock()
   ENDIF

   ? "record locking, skip through records, data change two fields ..."
   nSec := hb_milliseconds()
   DbGoTop()
   DO WHILE ! EOF()
      IF RLock()
         Replace INFO  WITH "This is a record number " + hb_ntos( RECNO() ),;
                 DINFO WITH DATE()
         DbUnLock()
      ENDIF
      DbSkip( 1 )
   ENDDO
   ?? STR( ( hb_milliseconds() - nSec ) / 1000, 7, 2 ), "s"

   ? "skipping from top to bottom, delete every 3rd record ..."
   nSec := hb_milliseconds()
   DbGoTop()
   nDeleted := 0
   DO WHILE ! EOF()
      IF RECNO() % 3 == 0 .AND. RLock()
         DELETE
         DbUnlock()
         nDeleted++
      ENDIF
      DbSkip( 1 )
   ENDDO
   ?? STR( ( hb_milliseconds() - nSec ) / 1000, 7, 2 ), "s"

   ? "verify deleted record ..."
   ii := 0
   LETO_SETSKIPBUFFER( 250 )
   nSec := hb_milliseconds()
   DbGoTop()
   DO WHILE ! EOF()
      IF RECNO() % 3 == 0 .AND. DELETED()
         ii++
      ENDIF
      DbSkip( 1 )
   ENDDO
   ?? STR( ( hb_milliseconds() - nSec ) / 1000, 7, 2 ), "seconds", IIF( ii == nDeleted, "( Ok )", "FAIL!" )
   LETO_SETSKIPBUFFER( 21 )

   ? "( amount of buffered SKIPs: " + STR( LETO_SETSKIPBUFFER(), 10, 0 ) + " )"
#ifdef __MEM_IO__
   IF RDDSETDEFAULT() == "LETO"
      ? "determine maximum request-rate ... "
      ii := 0
      nSec := hb_milliseconds()
      DO WHILE ii < 250000
         IF leto_Ping()
            ii++
         ENDIF
      ENDDO
      ?? STR( ii / ( ( hb_milliseconds() - nSec ) / 1000 ), 9, 2 ), "request/ s"
   ENDIF
#endif

   ?
   ? "Press ENTER to delete test DBF, any other key to finish, ..."
   nKey := Inkey( 0 )

   DBCLOSEALL()

   IF nKey == 13
      hb_dbDrop( cFile1 + hb_rddInfo( RDDI_TABLEEXT ) )
      hb_dbDrop( cFile1 + hb_rddInfo( RDDI_ORDBAGEXT ) )
      hb_dbDrop( cFile2 + hb_rddInfo( RDDI_ORDBAGEXT ) )
      IF hb_dbexists( cFile1 + hb_rddInfo( RDDI_TABLEEXT ) ) .OR.;
         hb_dbExists( cFile1 + hb_rddInfo( RDDI_ORDBAGEXT ) ) .OR.;
         hb_dbExists( cFile2 + hb_rddInfo( RDDI_ORDBAGEXT ) )
         ? "drop dbf: - Failure "
      ELSE
         ? "drop dbf: -  Ok"
      ENDIF

      ?
      ? "Press any key to finish ..."
      INKEY( 0 )
   ENDIF

Return Nil
