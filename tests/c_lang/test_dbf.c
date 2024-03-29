
/* set it before ! */
#define __LETO_C_API__
#include "letocl.h"

#if defined( HB_OS_WIN )
   #define _EOL_  "\r\n"
#else
   #define _EOL_  "\n"
#endif

static void printRec( LETOTABLE * pTable )
{
   unsigned long ulRecNo, ulAlloc = 256, ulLen;
   char *        szRet = hb_xgrab( ulAlloc );

   LetoDbRecNo( pTable, &ulRecNo );
   ulLen = ulAlloc;
   LetoDbGetField( pTable, 1, szRet, &ulLen );
   printf( "N: %lu, %s", ulRecNo, szRet );

   ulLen = ulAlloc;
   LetoDbGetField( pTable, 2, szRet, &ulLen );
   printf( " %s", szRet );

   ulLen = ulAlloc;
   LetoDbGetField( pTable, 3, szRet, &ulLen );
   printf( " %s", szRet );

   ulLen = ulAlloc;
   LetoDbGetField( pTable, 4, szRet, &ulLen );
   printf( " %s", szRet );

   ulLen = ulAlloc;
   LetoDbGetField( pTable, 5, szRet, &ulLen );
   printf( " %s", szRet );

   /* alternative */
   /* printf( " %s /", LetoDbGetMemo( pTable, 5, &ulLen ) ); */

   printf( "%s", _EOL_ );

   hb_xfree( szRet );
}

int main( int argc, char *argv[] )
{
   LETOCONNECTION * pConnection;
   int iPort;
   char szAddr[128];

   LetoInit();
   LetoSetAddress( argc, argv, szAddr, &iPort );

   printf( "Connecting to %s:%d ..." _EOL_, szAddr, iPort );
   if( ( pConnection = LetoConnectionNew( szAddr, iPort, NULL, NULL, 0, 0 ) ) != NULL )
   {
      LETOTABLE * pTable;

      printf( "Connected!" _EOL_ );
      printf( "%s" _EOL_, LetoGetServerVer( pConnection ) );
      printf( "Compression level: %d" _EOL_, LetoToggleZip( pConnection, 1, NULL ) );

      pTable = LetoDbCreateTable( pConnection, "testCApi", "TEST1",
                                  "NAME;C;10;0;NUM;N;4;0;INFO;C;32;0;DINFO;D;8;0;MINFO;M;10;0;",
                                  1, NULL, HB_FALSE );
      if( pTable )
      {
         unsigned int ui, uiFields, uiIndex, uiRet;
         unsigned long ulRecCount;
         char szRet[ 64 ];
         char szOrdInfo[ 512 ];
         char * pNames[] = { "Petr", "Ivan", "Alexander", "Pavel", "Alexey", "Fedor",
                             "Rolf", "Vladimir", "Nikolay", "Andrey", "Dmitry", "Sergey" };

         printf( "test1.dbf has been created." _EOL_ );

         if( ! LetoDbRecCount( pTable, &ulRecCount ) )
            printf( "Records: %lu" _EOL_, ulRecCount );
         else
            printf( "LetoDbRecCount error" _EOL_ );

         LetoDbFieldCount( pTable, &uiFields );
         printf( "Fields number: %d" _EOL_, uiFields );
         for( ui = 1; ui <= uiFields; ui++ )
         {
            if( ! LetoDbFieldName( pTable, ui, szRet ) )
               printf( "   %-12s", szRet );
            if( ! LetoDbFieldType( pTable, ui, &uiRet ) )
            {
               switch( uiRet )
               {
                  case HB_FT_NONE:
                     printf( "ERR" );
                     break;
                  case HB_FT_STRING:
                     printf( "C" );
                     break;
                  case HB_FT_LOGICAL:
                     printf( "L" );
                     break;
                  case HB_FT_DATE:
                     printf( "D" );
                     break;
                  case HB_FT_LONG:
                     printf( "N" );
                     break;
                  case HB_FT_MEMO:
                     printf( "M" );
                     break;
               }
            }
            if( ! LetoDbFieldLen( pTable, ui, &uiRet ) )
               printf( "\t%d", uiRet );
            if( ! LetoDbFieldDec( pTable, ui, &uiRet ) )
               printf( "\t%d" _EOL_, uiRet );
         }

         printf( _EOL_ "Append record - " );

         for( ui = 1; ui <= 12; ui++ )
         {
            LetoDbAppend( pTable, 0 );

            LetoDbPutField( pTable, 1, pNames[ ui - 1 ], strlen( pNames[ ui - 1 ] ) );

            sprintf( szRet, "%d", ui + 2000 );
            LetoDbPutField( pTable, 2, szRet, strlen( szRet ) );

            sprintf( szRet, "A record number %d", ui );
            LetoDbPutField( pTable, 3, szRet, strlen( szRet ) );

            sprintf( szRet, "201801%d", ui + 10 );
            LetoDbPutField( pTable, 4, szRet, 8 );

            sprintf( szRet, "MEMO:<%s>", pNames[ ui - 1 ] );
            LetoDbPutField( pTable, 5, szRet, strlen( szRet ) );

            if( ! LetoDbPutRecord( pTable ) )
               printf( "%d ", ui );
            else
               printf( " error " );
         }

         printf( _EOL_ "Index creating (NAME) - " );
         if( ! LetoDbOrderCreate( pTable, NULL, "NAME", "NAME", 0, NULL, NULL, 0 ) )
            printf( "Ok" _EOL_ );
         else
            printf( "error" _EOL_ );
         printf( "Index creating (NUM) - " );
         if( ! LetoDbOrderCreate( pTable, NULL, "NUM", "NUM", 0, NULL, NULL, 0 ) )
            printf( "Ok" _EOL_ );
         else
            printf( "error" _EOL_ );
         printf( "Index creating DTOS(DINFO) - " );
         if( ! LetoDbOrderCreate( pTable, NULL, "DATE", "DTOS(DINFO)", 0, NULL, NULL, 0 ) )
            printf( "Ok" _EOL_ );
         else
            printf( "error" _EOL_ );
         printf( "Index creating UPPER(NAME) in extra bag - " );
         if( ! LetoDbOrderCreate( pTable, "testex", "UNAME", "UPPER(NAME)", LETO_INDEX_ADD, NULL, NULL, 0 ) )
            printf( "Ok" _EOL_ );
         else
            printf( "error" _EOL_ );
         printf( "4 index orders active - " );
         LetoDbOrdCount( pTable, &uiIndex );
         if( uiIndex == 3 )
            printf( "Ok" _EOL_ );
         else
            printf( "error" _EOL_ );
         printf( "active order 'DATE'   - %s" _EOL_, pTable->pTagCurrent ? pTable->pTagCurrent->TagName : "" );


         LetoDbOrdInfo( pTable, HB_FALSE, szOrdInfo, sizeof( szOrdInfo ) );
         printf( "Order Infos: %s "_EOL_, szOrdInfo );

         LetoDbOrderDestroy( pTable, "NUM", "testCApi" );
         LetoDbOrdCount( pTable, &uiIndex );
         if( uiIndex == 3 )
            printf( "Order 'NUM' destroyed from list" _EOL_ );
         LetoDbOrdInfo( pTable, HB_FALSE, szOrdInfo, sizeof( szOrdInfo ) );
         printf( "Order Infos: %s "_EOL_, szOrdInfo );

         LetoDbOrderListDelete( pTable, "testex" );
         LetoDbOrdCount( pTable, &uiIndex );
         if( uiIndex == 2 )
            printf( "Order 'UNAME' removed from list" _EOL_ );
         LetoDbOrdInfo( pTable, HB_FALSE, szOrdInfo, sizeof( szOrdInfo ) );
         printf( "Order Infos: %s "_EOL_, szOrdInfo );

         LetoDbOrderListClear( pTable );
         LetoDbOrdCount( pTable, &uiIndex );
         if( ! uiIndex )
            printf( "all Orders closed !" _EOL_ );
         LetoDbOrdInfo( pTable, HB_FALSE, szOrdInfo, sizeof( szOrdInfo ) );
         printf( "Order Infos: %s "_EOL_, szOrdInfo );

         LetoDbOrderListAdd( pTable, "testCApi" );
         LetoDbOrderListAdd( pTable, "testex" );
         LetoDbOrdCount( pTable, &uiIndex );
         if( uiIndex == 3 )
            printf( "%s production index + 'testex' re-opened" _EOL_, "testCApi" );
         LetoDbOrdInfo( pTable, HB_FALSE, szOrdInfo, sizeof( szOrdInfo ) );
         printf( "Order Infos: %s "_EOL_, szOrdInfo );

         LetoDbOrderDestroy( pTable, "UNAME", NULL );
         LetoDbOrdCount( pTable, &uiIndex );
         if( uiIndex == 2 )
            printf( "Order UNAME in testex destroyed " _EOL_ );
         LetoDbOrdInfo( pTable, HB_FALSE, szOrdInfo, sizeof( szOrdInfo ) );
         printf( "Order Infos: %s "_EOL_, szOrdInfo );

         LetoDbOrderFocus( pTable, "DATE", 0 );
         printf( "%s is active order " _EOL_,  LetoDbOrder( pTable, 0 ) );
         LetoDbOrdInfo( pTable, HB_FALSE, szOrdInfo, sizeof( szOrdInfo ) );
         printf( "Order Infos: %s "_EOL_, szOrdInfo );

         LetoDbOrderFocus( pTable, NULL, 1 );
         printf( "%s is active order " _EOL_,  LetoDbOrder( pTable, 0 ) );
         LetoDbOrdInfo( pTable, HB_FALSE, szOrdInfo, sizeof( szOrdInfo ) );
         printf( "Order Infos: %s "_EOL_, szOrdInfo );

         printf( "Go Top - " _EOL_ );
         LetoDbGoTop( pTable );
         printRec( pTable );

         printf( "Skip 1 record - " _EOL_ );
         LetoDbSkip( pTable, 1 );
         printRec( pTable );

         printf( "Seek \'Niko\'- " _EOL_ );
         LetoDbSeek( pTable, "Niko", 0, 1, 0 );
         printRec( pTable );

         printf( _EOL_ "Close table - " );
         if( ! LetoDbCloseTable( pTable ) )
            printf( "Ok" _EOL_ );
         else
            printf( "error" _EOL_ );

         printf( "Drop index & table %s - ", "testCApi" );   /* Harbour does not drop both at once */
         if( ! LetoDbDrop( pConnection, "", "testCApi" ) &&  /* index */
             ! LetoDbDrop( pConnection, "testCApi", "" ) )   /* table */
            printf( "Ok" _EOL_ );
         else
            printf( "error" _EOL_ );

      }
      else
         printf( "Can not create the test1.dbf" _EOL_ );

      printf( _EOL_ "logging out ..." _EOL_ );
      LetoConnectionClose( pConnection );
   }
   else
      printf( "Connection failure" _EOL_ );

   LetoExit( 1 );
}
