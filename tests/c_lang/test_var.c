
#include <stdio.h>
#include "letocl.h"
#include "rddleto.ch"

#define  LETO_VAR_LOG   '1'
#define  LETO_VAR_NUM   '2'
#define  LETO_VAR_CHAR  '3'

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
      char * ptr, szData[ 64 ];
      int iRes;

      printf( "Connected!\r\n" );
      printf( "%s\r\n", LetoGetServerVer( pConnection ) );

      printf( "Adding 'var_int' = 100 to [main] (Err (3)) " );
      iRes = LetoVarSet( pConnection, "main", "var_int", LETO_VAR_NUM, "100", 0, 0, NULL );
      if( iRes )
         printf( "Ok\r\n" );
      else
         printf( "Err( %d )\r\n", LetoGetError() );

      printf( "Adding 'var_int' = 100 to [main] (Ok) " );
      iRes = LetoVarSet( pConnection, "main", "var_int", LETO_VAR_NUM, "100", 0, LETO_VCREAT, NULL );
      if( iRes )
         printf( "Ok\r\n" );
      else
         printf( "Err( %d )\r\n", LetoGetError() );

      printf( "Adding 'var_log' = 1 to [main] (Ok) " );
      iRes = LetoVarSet( pConnection, "main", "var_log", LETO_VAR_LOG, "1", 0, LETO_VCREAT, NULL );
      if( iRes )
         printf( "Ok\r\n" );
      else
         printf( "Err( %d )\r\n", LetoGetError() );

      printf( "Adding 'var_char' = 'Just a test;' to [main] (Ok) " );
      iRes = LetoVarSet( pConnection, "main", "var_char", LETO_VAR_CHAR, "Just a test;", 0, LETO_VCREAT, NULL );
      if( iRes )
         printf( "Ok\r\n" );
      else
         printf( "Err( %d )\r\n", LetoGetError() );

      ptr = LetoVarGet( pConnection, "main", "var_int", NULL );
      printf( "\r\nvar_int = (100) %s\r\n", ( ptr )? ptr+2 : "Err" );
      if( ptr )
         free( ptr );

      ptr = LetoVarGet( pConnection, "main", "var_char", NULL );
      printf( "var_char = (Just a test;) %s\r\n", ( ptr )? ptr+2 : "Err" );
      if( ptr )
         free( ptr );

      ptr = LetoVarGet( pConnection, "main", "var_log", NULL );
      printf( "var_log = (1) %s\r\n", ( ptr )? ptr+2 : "Err" );
      if( ptr )
         free( ptr );

      printf( "\r\nDelete var_char (Ok) " );
      iRes = LetoVarDel( pConnection, "main", "var_char" );
      if( iRes )
         printf( "Ok\r\n" );
      else
         printf( "Err( %d )\r\n", LetoGetError() );

      printf( "Delete var_log (Ok) " );
      iRes = LetoVarDel( pConnection, "main", "var_log" );
      if( iRes )
         printf( "Ok\r\n" );
      else
         printf( "Err( %d )\r\n", LetoGetError() );

      printf( "Delete var_int (Ok) " );
      iRes = LetoVarDel( pConnection, "main", "var_int" );
      if( iRes )
         printf( "Ok\r\n" );
      else
         printf( "Err( %d )\r\n", LetoGetError() );

      LetoConnectionClose( pConnection );
   }
   else
      printf( "Connection failure\r\n" );

   LetoExit( 1 );
}
