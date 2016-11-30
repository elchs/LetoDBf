
#include <stdio.h>
#ifdef __BORLANDC__
   #include <conio.h>
#endif
#include "letocl.h"

static void setAddress( int argc, char *argv[], char * szAddr, int * iPort )
{
   *iPort = 2812;
   if( argc < 2 )
      strcpy( szAddr, "127.0.0.1" );
   else
   {
      char * ptr = argv[1], * ptrPort;
      unsigned int uiLen;

      if( !strncmp( ptr, "//", 2 ) )
         ptr += 2;
      if( ( ptrPort = strchr( ptr, ':' ) ) != NULL )
      {
         uiLen = ptrPort - ptr;
         *iPort = atol( ptrPort+1 );
      }
      else
         uiLen = strlen( ptr );
      memcpy( szAddr, ptr, uiLen );
      ptr = szAddr + uiLen;
      if( *(ptr-1) == '/' || *(ptr-1) == '\\' )
        ptr --;
      *ptr = '\0';
   }
}

static void printRec( LETOTABLE * pTable )
{
   char szRet[48];
   unsigned long ulRecNo;

   LetoDbRecNo( pTable, &ulRecNo );
   LetoDbGetField( pTable, 1, szRet, NULL );
   printf( "N: %lu, %s /", ulRecNo, szRet );

   LetoDbGetField( pTable, 2, szRet, NULL );
   printf( " %s /", szRet );

   LetoDbGetField( pTable, 3, szRet, NULL );
   printf( " %s /", szRet );

   LetoDbGetField( pTable, 4, szRet, NULL );
   printf( " %s\r\n", szRet );

}

void main( int argc, char *argv[] )
{
   LETOCONNECTION * pConnection;
   int iPort;
   char szAddr[128];

   setAddress( argc, argv, szAddr, &iPort );

   LetoInit();

   printf( "Connect to %s port %d\r\n", szAddr, iPort );
   if( ( pConnection = LetoConnectionNew( szAddr, iPort, NULL, NULL, 0, 0 ) ) != NULL )
   {
      char * ptr, szData[64];
      LETOTABLE * pTable;

      printf( "Connected!\r\n" );
      printf( "%s\r\n", LetoGetServerVer( pConnection ) );

      pTable = LetoDbCreateTable( pConnection, "/test1", "test1", 
         "NAME;C;10;0;NUM;N;4;0;INFO;C;32;0;DINFO;D;8;0;", 0, NULL );
      if( pTable )
      {
         unsigned int ui, uiFields, uiRet;
         unsigned long ulRecCount, ulRecNo;
         char szRet[48];
         char * pNames[] = { "Petr", "Ivan", "Alexander", "Pavel", "Alexey", "Fedor",
            "Konstantin", "Vladimir", "Nikolay", "Andrey", "Dmitry", "Sergey" };

         printf( "test1.dbf has been created.\r\n" );

         if( !LetoDbRecCount( pTable, &ulRecCount ) )
            printf( "Records: %d\r\n", ulRecCount );
         else
            printf( "LetoDbRecCount error\r\n" );

         LetoDbFieldCount( pTable, &uiFields );
         printf( "Fields number: %d\r\n", uiFields );
         for( ui=1; ui <= uiFields; ui++ )
         {
            if( !LetoDbFieldName( pTable, ui, szRet ) )
               printf( "   %-12s", szRet );
            if( !LetoDbFieldType( pTable, ui, &uiRet ) )
               printf( "%d", uiRet );
            if( !LetoDbFieldLen( pTable, ui, &uiRet ) )
               printf( "\t%d", uiRet );
            if( !LetoDbFieldDec( pTable, ui, &uiRet ) )
               printf( "\t%d\r\n", uiRet );
         }

         printf( "\r\n" );
         for( ui=1; ui <= 12; ui++ )
         {
            printf( "Append record - " );
            LetoDbAppend( pTable, 0 );

            LetoDbPutField( pTable, 1, pNames[ui-1], strlen(pNames[ui-1]) );

            sprintf( szRet, "%d", ui+2000 );
            LetoDbPutField( pTable, 2, szRet, strlen(szRet) );

            sprintf( szRet, "A record number %d", ui );
            LetoDbPutField( pTable, 3, szRet, strlen(szRet) );

            sprintf( szRet, "201312%d", ui+10 );
            LetoDbPutField( pTable, 4, szRet, 8 );

            if( !LetoDbPutRecord( pTable, 0 ) )
               printf( "%d\r\n", ui );
            else
               printf( "error\r\n" );
         }

         printf( "\r\nIndex creating (NAME) - " );
         if( ! LetoDbOrderCreate( pTable, NULL, "NAME", "NAME", 0, NULL, NULL, 0 ) )
            printf( "Ok\r\n" );
         else
            printf( "error\r\n" );
         printf( "Index creating (NUM) - " );
         if( !LetoDbOrderCreate( pTable, NULL, "NUM", "Str(NUM,4)", 0, NULL, NULL, 0 ) )
            printf( "Ok\r\n" );
         else
            printf( "error\r\n" );

         printf( "Go Top - " );
         LetoDbGoTop( pTable, "NAME" );
         printRec( pTable );

         printf( "Skip 1 record - " );
         LetoDbSkip( pTable, 1, "NAME" );
         printRec( pTable );

         printf( "Seek \'Niko\'- " );
         LetoDbSeek( pTable, "Niko", 0, 0, 0 );
         printRec( pTable );

         printf( "\r\nClose table - " );
         if( !LetoDbCloseTable( pTable ) )
            printf( "Ok\r\n" );
         else
            printf( "error\r\n" );

         getch();
      }
      else
         printf( "Can not create the test1.dbf\r\n" );

      LetoConnectionClose( pConnection );
   }
   else
      printf( "Connection failure\r\n" );

   LetoExit( 1 );
}
