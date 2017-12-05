/* do NOT use for xHarbour */
#include "dbinfo.ch"

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
   ? Leto_dbTrans( SELECT( "TEST2" ), { "Code", "Name", "Text" }, "Code==2" )
   DbSelectArea( "TEST2" )
   DbGoBottom()
   ? "*          4  2 Second              4 Elch"
   ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc, Field->Text
   IF DbAppend()
      REPLACE Field->Code WITH 5,;
              Field->Name WITH "Forth"
      DbUnlock()
   ENDIF
   ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc, Field->Text

   DbSelectArea( "TEST" )
   ? Leto_dbTrans( SELECT( "TEST2" ), { "Code", "Name" }, {|| Code == 3 } )
   DbSelectArea( "TEST2" )
   ? "*          6  3 Third               6"
   ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc, Field->Text
   IF DbAppend()
      REPLACE Field->Code WITH 7,;
              Field->Name WITH "Forth"
      DbUnlock()
   ENDIF
   ? " ", Reccount(), Field->Code, Field->Name, Field->AutoInc, Field->Text

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
      __dbTrans( SELECT( "TEST5" ) )
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



/* C-level */
#pragma BEGINDUMP

/*
 * replicas of: __DBARRANGE(), __DBTRANS(), __DBAPP(), __DBCOPY()
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2004-2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * slightly modified  2017 Rolf 'elch' Beckmann
 */

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapirdd.h"
#include "funcleto.h"

/* replaces __DBARRANGE() by optional use of string expressions instead of codeblocks,
 * __dbArrange( nToArea, aStruct, bFor, bWhile, nNext, nRecord, lRest, aFields ) */
HB_FUNC( LETO_DBARRANGE )
{
   HB_ERRCODE errCode = HB_FAILURE;
   AREAP pSrcArea, pDstArea;

   pSrcArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   pDstArea = ( AREAP ) hb_rddGetWorkAreaPointer( ( HB_AREANO ) hb_parni( 1 ) );

   /* TODO: check what Clipper does when pDstArea == NULL or pSrcArea == pDstArea */
   if( pSrcArea && pDstArea && pSrcArea != pDstArea )
   {
      DBSORTINFO dbSortInfo;
      /* structure with fields copied copied from source WorkArea */
      PHB_ITEM pStruct = hb_param( 2, HB_IT_ARRAY );
      /* array with sorted fields in source WorkArea */
      PHB_ITEM pFields = hb_param( 8, HB_IT_ARRAY );

      memset( &dbSortInfo, 0, sizeof( dbSortInfo ) );
      errCode = hb_dbTransStruct( pSrcArea, pDstArea, &dbSortInfo.dbtri,
                                  NULL, pStruct );
      if( errCode == HB_SUCCESS )
      {
         PHB_ITEM pTransItm;

         dbSortInfo.dbtri.dbsci.itmCobFor   = hb_param( 3, HB_IT_BLOCK );
         dbSortInfo.dbtri.dbsci.lpstrFor    = hb_param( 3, HB_IT_STRING );
         dbSortInfo.dbtri.dbsci.itmCobWhile = hb_param( 4, HB_IT_BLOCK );
         dbSortInfo.dbtri.dbsci.lpstrWhile  = hb_param( 4, HB_IT_STRING );
         dbSortInfo.dbtri.dbsci.lNext       = hb_param( 5, HB_IT_NUMERIC );
         dbSortInfo.dbtri.dbsci.itmRecID    = HB_ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY );
         dbSortInfo.dbtri.dbsci.fRest       = hb_param( 7, HB_IT_LOGICAL );

         dbSortInfo.dbtri.dbsci.fIgnoreFilter     =
         dbSortInfo.dbtri.dbsci.fLast             =
         dbSortInfo.dbtri.dbsci.fIgnoreDuplicates =
         dbSortInfo.dbtri.dbsci.fBackward         =
         dbSortInfo.dbtri.dbsci.fOptimized        = HB_FALSE;
         dbSortInfo.dbtri.dbsci.fIncludeDeleted   = HB_TRUE;

         /* do not transfer record deleted flag to destination area */
         dbSortInfo.dbtri.uiFlags |= DBTF_RECALL;

         dbSortInfo.uiItemCount = pFields ? ( HB_USHORT ) hb_arrayLen( pFields ) : 0;
         if( dbSortInfo.uiItemCount > 0 )
         {
            HB_USHORT uiCount, uiDest;
            char * szFieldLine;
            HB_SIZE nSize = 0;

            dbSortInfo.lpdbsItem = ( LPDBSORTITEM ) hb_xgrab( dbSortInfo.uiItemCount * sizeof( DBSORTITEM ) );
            for( uiCount = 1; uiCount <= dbSortInfo.uiItemCount; ++uiCount )
            {
               HB_SIZE nLine = hb_arrayGetCLen( pFields, uiCount );
               if( nLine > nSize )
                  nSize = nLine;
            }
            szFieldLine = ( char * ) hb_xgrab( nSize + 1 );
            for( uiDest = 0, uiCount = 1; uiCount <= dbSortInfo.uiItemCount; ++uiCount )
            {
               char * szPos;
               dbSortInfo.lpdbsItem[ uiDest ].uiFlags = 0;
               hb_strncpyUpper( szFieldLine, hb_arrayGetCPtr( pFields, uiCount ),
                                hb_arrayGetCLen( pFields, uiCount ) );
               szPos = strchr( szFieldLine, '/' );
               if( szPos )
               {
                  *szPos++ = 0;
                  /* It's not Cl*pper compatible, Cl*pper checks only
                     for /D flag and ignores any /A flags [druzus] */
                  if( strchr( szPos, 'D' ) > strchr( szPos, 'A' ) )
                     dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_DESCEND;
                  else
                     dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_ASCEND;
                  if( strchr( szPos, 'C' ) != NULL )
                     dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_CASE;
               }
               else
                  dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_ASCEND;

               /* Cl*pper sorts records using field values from source
                  area only, destination area may not contain sorted
                  fields at all [druzus] */
               dbSortInfo.lpdbsItem[ uiDest ].uiField = hb_rddFieldExpIndex( pSrcArea, szFieldLine );
               /* Field found */
               if( dbSortInfo.lpdbsItem[ uiDest ].uiField != 0 )
                  ++uiDest;
            }
            dbSortInfo.uiItemCount = uiDest;
            hb_xfree( szFieldLine );
         }

         pTransItm = hb_dbTransInfoPut( NULL, &dbSortInfo.dbtri );
         errCode = SELF_INFO( dbSortInfo.dbtri.lpaDest, DBI_TRANSREC, pTransItm );
         if( errCode == HB_SUCCESS )
         {
            errCode = dbSortInfo.dbtri.uiItemCount == 0 ? HB_FAILURE :
                      ( dbSortInfo.uiItemCount == 0 ?
                        SELF_TRANS( pSrcArea, &dbSortInfo.dbtri ) :
                        SELF_SORT( pSrcArea, &dbSortInfo ) );
            SELF_INFO( dbSortInfo.dbtri.lpaDest, DBI_TRANSREC, pTransItm );
            if( errCode == HB_SUCCESS && ( dbSortInfo.dbtri.uiFlags & DBTF_CPYCTR ) )
               errCode = hb_dbTransCounters( &dbSortInfo.dbtri );
         }
         hb_itemRelease( pTransItm );
      }

      /* Free items */
      if( dbSortInfo.lpdbsItem )
         hb_xfree( dbSortInfo.lpdbsItem );
      if( dbSortInfo.dbtri.lpTransItems )
         hb_xfree( dbSortInfo.dbtri.lpTransItems );
   }

   hb_retl( errCode == HB_SUCCESS );
}


#if 0  /* LETO_DBTRANS integrated in LetoDBf client library */

/* replaces __DBTRANS() by optional use of string expressions instead of codeblocks,
 * Leto_dbTrans( cnDstArea, aFields, cFor, cWhile, nNext, nRecord, lRest ) */
HB_FUNC( LETO_DBTRANS )
{
   HB_UINT uiDstArea = 0;
   AREAP   pSrcArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( HB_ISCHAR( 1 ) )  /* ALIAS */
      hb_rddGetAliasNumber( hb_parc( 1 ), ( int * ) &uiDstArea );
   else if( HB_ISNUM( 1 ) )  /* WA number */
      uiDstArea = hb_parni( 1 );

   if( uiDstArea > 0 && pSrcArea )
   {
      HB_UINT uiSrcArea;
      AREAP   pDstArea;

      uiSrcArea = hb_rddGetCurrentWorkAreaNumber();
      hb_rddSelectWorkAreaNumber( uiDstArea );
      pDstArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

      if( pDstArea )
      {
         DBTRANSINFO dbTransInfo;
         PHB_ITEM pFields = hb_param( 2, HB_IT_ARRAY );
         HB_ERRCODE errCode;

         memset( &dbTransInfo, 0, sizeof( DBTRANSINFO ) );
         errCode = hb_dbTransStruct( pSrcArea, pDstArea, &dbTransInfo, NULL, pFields );
         if( errCode == HB_SUCCESS )
         {
            hb_rddSelectWorkAreaNumber( dbTransInfo.lpaSource->uiArea );

            dbTransInfo.dbsci.itmCobFor   = hb_param( 3, HB_IT_BLOCK );
            dbTransInfo.dbsci.lpstrFor    = hb_param( 3, HB_IT_STRING );
            dbTransInfo.dbsci.itmCobWhile = hb_param( 4, HB_IT_BLOCK );
            dbTransInfo.dbsci.lpstrWhile  = hb_param( 4, HB_IT_STRING );
            dbTransInfo.dbsci.lNext       = hb_param( 5, HB_IT_NUMERIC );
            dbTransInfo.dbsci.itmRecID    = HB_ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY );
            dbTransInfo.dbsci.fRest       = hb_param( 7, HB_IT_LOGICAL );

            /* different to Harbour: fIgnoreFilter = HB_FALSE */
            dbTransInfo.dbsci.fIgnoreFilter     = HB_TRUE;
            dbTransInfo.dbsci.fIncludeDeleted   = HB_TRUE;
            dbTransInfo.dbsci.fLast             = HB_FALSE;
            dbTransInfo.dbsci.fIgnoreDuplicates = HB_FALSE;
            dbTransInfo.dbsci.fBackward         = HB_FALSE;
            dbTransInfo.dbsci.fOptimized        = HB_FALSE;

            errCode = SELF_TRANS( dbTransInfo.lpaSource, &dbTransInfo );
         }

         if( dbTransInfo.lpTransItems )
            hb_xfree( dbTransInfo.lpTransItems );

         hb_retl( errCode == HB_SUCCESS );
      }
      else
         hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "LETO_DBTRANS" );

      hb_rddSelectWorkAreaNumber( uiSrcArea );
   }
   else
      hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, "LETO_DBTRANS" );
}

#endif  /* LETO_DBTRANS */


/* replaces __DBAPP() [ APPEND FROM ] by optional use of string expressions instead of codeblocks
 * Leto_dbApp( cFilename, aFields, cbFor, cbWhile, nNext, nRecord, lRest, cRDD, nConnection, cCDP, xDelimiter ) */
HB_FUNC( LETO_DBAPP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      hb_retl( HB_SUCCESS == hb_rddTransRecords( pArea,
               hb_parc( 1 ),                     /* file name */
               hb_parc( 8 ),                     /* RDD */
               hb_parnl( 9 ),                    /* connection */
               hb_param( 2, HB_IT_ARRAY ),       /* Fields */
               HB_FALSE,                         /* Export? */
               hb_param( 3, HB_IT_BLOCK ),       /* cobFor */
               hb_param( 3, HB_IT_STRING ),      /* lpStrFor */
               hb_param( 4, HB_IT_BLOCK ),       /* cobWhile */
               hb_param( 4, HB_IT_STRING ),      /* lpStrWhile */
               hb_param( 5, HB_IT_NUMERIC ),     /* Next */
               HB_ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY ),   /* RecID */
               hb_param( 7, HB_IT_LOGICAL ),     /* Rest */
               hb_parc( 10 ),                    /* Codepage */
               hb_param( 11, HB_IT_ANY ) ) );    /* Delimiter */
   else
#if 0
      commonError( ( LETOAREAP ) pArea, EG_NOTABLE, EDBCMD_NOTABLE, 0, NULL, 0, "LETO_DBAPP" );
#else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "LETO_DBAPP" );
#endif
}

/* replaces __DBCOPY() [ COPY TO ] by optional use of string expressions instead of codeblocks
 * Leto_dbCopy( cFilename, aFields, cbFor, cbWhile, nNext, xRecID, lRest, cRDD, nConnection, cCDP, xDelim ) */
HB_FUNC( LETO_DBCOPY )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      hb_retl( HB_SUCCESS == hb_rddTransRecords( pArea,
               hb_parc( 1 ),                     /* file name */
               hb_parc( 8 ),                     /* RDD */
               hb_parnl( 9 ),                    /* connection */
               hb_param( 2, HB_IT_ARRAY ),       /* Fields */
               HB_TRUE,                          /* Export? */
               hb_param( 3, HB_IT_BLOCK ),       /* cobFor */
               hb_param( 3, HB_IT_STRING ),      /* lpStrFor */
               hb_param( 4, HB_IT_BLOCK ),       /* cobWhile */
               hb_param( 4, HB_IT_STRING ),      /* lpStrWhile */
               hb_param( 5, HB_IT_NUMERIC ),     /* Next */
               HB_ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY ),   /* RecID */
               hb_param( 7, HB_IT_LOGICAL ),     /* Rest */
               hb_parc( 10 ),                    /* Codepage */
               hb_param( 11, HB_IT_ANY ) ) );    /* Delimiter */
   else
#if 0
      commonError( ( LETOAREAP ) pArea, EG_NOTABLE, EDBCMD_NOTABLE, 0, NULL, 0, "LETO_DBCOPY" );
#else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "LETO_DBCOPY" );
#endif
}

#pragma ENDDUMP
