
#include <stdio.h>
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

      printf( "Connected!\r\n" );
      printf( "%s\r\n", LetoGetServerVer( pConnection ) );
      if( ( ptr = LetoMgGetInfo( pConnection ) ) != NULL && *(ptr-1) == '+' )
      {
         LetoGetCmdItem( &ptr, szData ); ptr ++;
         printf( "Users current:  %s\t\t", szData );
         LetoGetCmdItem( &ptr, szData ); ptr ++;
         printf( "max: %s\r\n", szData );

         LetoGetCmdItem( &ptr, szData ); ptr ++;
         printf( "Tables current: %s\t\t", szData );
         LetoGetCmdItem( &ptr, szData ); ptr ++;
         printf( "max: %s\r\n", szData );

      }

      LetoConnectionClose( pConnection );
   }
   else
      printf( "Connection failure\r\n" );

   LetoExit( 1 );
}
