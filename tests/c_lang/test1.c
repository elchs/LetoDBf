
#include <stdio.h>
/* set it before ! */
#define __LETO_C_API__
#include "letocl.h"

#if defined( HB_OS_WIN )
   #define _EOL_  "\r\n"
#else
   #define _EOL_  "\n"
#endif

void main( int argc, char *argv[] )
{
   LETOCONNECTION * pConnection;
   int  iPort;
   char szAddr[ 128 ];

   LetoInit();
   LetoSetAddress( argc, argv, szAddr, &iPort );

   printf( "Connecting to %s:%d ..." _EOL_, szAddr, iPort );
   if( ( pConnection = LetoConnectionNew( szAddr, iPort, NULL, NULL, 0, 0 ) ) != NULL )
   {
      const char * ptr;
      char szData[ 64 ];

      printf( "Connected!" _EOL_ );
      printf( "%s" _EOL_, LetoGetServerVer( pConnection ) );
      if( ( ptr = LetoMgGetInfo( pConnection ) ) != NULL && *( ptr - 1 ) == '+' )
      {
         ptr = LetoGetCmdItem( ptr, szData ); ptr ++;
         printf( "Users current:  %s\t\t", szData );
         ptr = LetoGetCmdItem( ptr, szData ); ptr ++;
         printf( "max: %s" _EOL_, szData );

         ptr = LetoGetCmdItem( ptr, szData ); ptr ++;
         printf( "Tables current: %s\t\t", szData );
         LetoGetCmdItem( ptr, szData );
         printf( "max: %s" _EOL_, szData );
      }

      LetoConnectionClose( pConnection );
   }
   else
      printf( "Connection failure" _EOL_ );

   LetoExit( 1 );
}
