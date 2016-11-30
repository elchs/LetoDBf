#include "dbinfo.ch"

#if defined( __PLATFORM__LINUX )
   REQUEST HB_GT_XWC
   REQUEST HB_GT_XWC_DEFAULT
#else
   REQUEST HB_GT_WVT
   REQUEST HB_GT_WVT_DEFAULT
#endif

REQUEST LETO

Function main( cPath )
 FIELD CODE, NAME, TEXT 

   RDDSETDEFAULT( "LETO" )

   CLS
   ALTD()

   IF Empty( cPath )
      cPath := "//127.0.0.1:2812/"
   ELSE
      cPath := "//" + cPath + IiF( ":" $ cPath, "", ":2812" )
      cPath += Iif( Right(cPath,1) == "/", "", "/" )
   ENDIF

   DbCreate(cPath + 'test', {{'Code','N',2,0},{'Name','C',20,0},{'Text','M',10,0}},, .T.)
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
   DbGoTop()

   leto_dbCopy( (cPath+"test2.dbf"), { }, 'Code==2',,,, .F.,,, )
   DbUseArea(.t.,, cPath + 'test2')
   ? "*          1  1 Second"
   ? " ", Reccount(), Field->Code, Field->Name
   ?

   DbSelectArea( "TEST" )
   DbGoTop()
   SORT TO test3 ON name /D
   DbUseArea(.t.,, cPath + 'test3')
   ? "*          3  3 Third"
   ? " ", Reccount(), Field->Code, Field->Name

   DbCloseAll()
   WAIT
   hb_dbDrop(cPath+"test.dbf")
   hb_dbDrop(cPath+"test2.dbf")
   hb_dbDrop(cPath+"test3.dbf")

RETURN nil


/* C-level game play */

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapirdd.h"
#include "funcleto.h"

/* Leto_dbTrans( nDstArea, aFieldsStru, cFor, cWhile, nNext, nRecord, lRest ) */
HB_FUNC( LETO_DBTRANS )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_UINT uiSrcArea, uiDstArea;
      AREAP pSrcArea, pDstArea;

      uiSrcArea = hb_rddGetCurrentWorkAreaNumber();
      pSrcArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      uiDstArea = hb_parni( 1 );
      hb_rddSelectWorkAreaNumber( uiDstArea );
      pDstArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

      if( pSrcArea && pDstArea )
      {
         DBTRANSINFO dbTransInfo;
         PHB_ITEM pFields = hb_param( 2, HB_IT_ARRAY );
         HB_ERRCODE errCode;

         memset( &dbTransInfo, 0, sizeof( DBTRANSINFO ) );
         errCode = hb_dbTransStruct( pSrcArea, pDstArea, &dbTransInfo,
                                     NULL, pFields );
         if( errCode == HB_SUCCESS )
         {
            hb_rddSelectWorkAreaNumber( dbTransInfo.lpaSource->uiArea );

            dbTransInfo.dbsci.itmCobFor   = NULL;
            dbTransInfo.dbsci.lpstrFor    = hb_param( 3, HB_IT_STRING );
            dbTransInfo.dbsci.itmCobWhile = NULL;
            dbTransInfo.dbsci.lpstrWhile  = hb_param( 4, HB_IT_STRING );
            dbTransInfo.dbsci.lNext       = hb_param( 5, HB_IT_NUMERIC );
            dbTransInfo.dbsci.itmRecID    = HB_ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY );
            dbTransInfo.dbsci.fRest       = hb_param( 7, HB_IT_LOGICAL );

            dbTransInfo.dbsci.fIgnoreFilter     = HB_TRUE;
            dbTransInfo.dbsci.fIncludeDeleted   = HB_TRUE;
            dbTransInfo.dbsci.fLast             = HB_FALSE;
            dbTransInfo.dbsci.fIgnoreDuplicates = HB_FALSE;
            dbTransInfo.dbsci.fBackward         = HB_FALSE;

            errCode = SELF_TRANS( dbTransInfo.lpaSource, &dbTransInfo );
         }

         if( dbTransInfo.lpTransItems )
            hb_xfree( dbTransInfo.lpTransItems );

         hb_retl( errCode == HB_SUCCESS );
      }
      else
         hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "__DBTRANS" );

      hb_rddSelectWorkAreaNumber( uiSrcArea );
   }
   else
      hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, "__DBTRANS" );
}

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
               NULL,                             /* cobFor */
               hb_param( 3, HB_IT_STRING ),      /* lpStrFor */
               NULL,                             /* cobWhile */
               hb_param( 4, HB_IT_STRING ),      /* lpStrWhile */
               hb_param( 5, HB_IT_NUMERIC ),     /* Next */
               HB_ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY ),   /* RecID */
               hb_param( 7, HB_IT_LOGICAL ),     /* Rest */
               hb_parc( 10 ),                    /* Codepage */
               hb_param( 11, HB_IT_ANY ) ) );    /* Delimiter */
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "APPEND FROM" );
}

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
               NULL,                             /* cobFor */
               hb_param( 3, HB_IT_STRING ),      /* lpStrFor */
               NULL,                             /* cobWhile */
               hb_param( 4, HB_IT_STRING ),      /* lpStrWhile */
               hb_param( 5, HB_IT_NUMERIC ),     /* Next */
               HB_ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY ),   /* RecID */
               hb_param( 7, HB_IT_LOGICAL ),     /* Rest */
               hb_parc( 10 ),                    /* Codepage */
               hb_param( 11, HB_IT_ANY ) ) );    /* Delimiter */
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "COPY TO" );
}

#pragma ENDDUMP

