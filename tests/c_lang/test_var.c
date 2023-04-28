
/* set it before ! */
#define __LETO_C_API__
#include "letocl.h"
#include "rddleto.ch"

#if defined( HB_OS_WIN )
   #define _EOL_  "\r\n"
#else
   #define _EOL_  "\n"
#endif

int main( int argc, char *argv[] )
{
   LETOCONNECTION * pConnection;
   int iPort;
   char szAddr[ 128 ];

   LetoInit();
   LetoSetAddress( argc, argv, szAddr, &iPort );

   printf( "Connecting to %s:%d ..." _EOL_, szAddr, iPort );
   if( ( pConnection = LetoConnectionNew( szAddr, iPort, NULL, NULL, 0, 0 ) ) != NULL )
   {
      const char *  ptr;
      char          szData[ 256 ], szDate[ 9 ];
      unsigned long ulLen;
      int           iRes;
      long          lData = 0;
      double        dData = 0.0;
      HB_BOOL       fData = HB_FALSE;

      printf( "Connected!" _EOL_ );
      printf( "%s\r\n", LetoGetServerVer( pConnection ) );


      /* adding vars */
      printf( "Adding 'var_int' = 100 to [main] [Err ( 0 )] " );
      iRes = LetoVarSet( pConnection, "main", "var_int", LETOVAR_NUM, "100", 0, LETO_VNOCREAT, NULL );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      printf( "Adding 'var_int' = 100 to [main] [Ok] " );
      iRes = LetoVarSet( pConnection, "main", "var_int", LETOVAR_NUM, "100", 3, LETO_VCREAT, NULL );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      printf( "Adding 'var_dec' = 123.456 to [main] [Ok] " );
      iRes = LetoVarSet( pConnection, "main", "var_dec", LETOVAR_NUM, "123.456", 0, LETO_VCREAT, NULL );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      printf( "Adding 'var_log' = 1 to [main] [Ok] " );
      iRes = LetoVarSet( pConnection, "main", "var_log", LETOVAR_LOG, "1", 0, LETO_VCREAT, NULL );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      printf( "Adding 'var_char' = 'Just a test;' to [main] [Ok] " );
      iRes = LetoVarSet( pConnection, "main", "var_char", LETOVAR_STR, "Just a test;", 0, LETO_VCREAT, NULL );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      printf( "Adding 'var_binary' containing: 'CHR(0);CHR(1);CHR(0)' to [main] [Ok] " );
      iRes = LetoVarSet( pConnection, "main", "var_binary",  LETOVAR_STR, "\0;\1;\0", 5, LETO_VCREAT + LETO_VOWN, NULL );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      printf( "Adding 'var_date' containing: '20230102' to [main] [Ok] " );
      iRes = LetoVarSet( pConnection, "main", "var_date",  LETOVAR_DAT, "20230102", 0, LETO_VCREAT + LETO_VOWN, NULL );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      /* retrieve vars */
      printf( _EOL_ );

      ptr = LetoVarGet( pConnection, "main", "var_int", NULL );
      printf( "var_int = [100] %s" _EOL_, ( ptr ) ? ptr + 2 : "Err" );
      iRes = LetoVarGetC( pConnection, "main", "var_int", &lData, NULL );

      ptr = LetoVarGet( pConnection, "main", "var_dec", NULL );
      printf( "var_dec = [123.345] %s" _EOL_, ( ptr ) ? ptr + 2 : "Err" );
      iRes = LetoVarGetC( pConnection, "main", "var_dec", &dData, NULL );

      printf( "LetoVarGetC into long: %ld, double: %f " _EOL_, lData, dData );

      ptr = LetoVarGet( pConnection, "main", "var_char", NULL );
      printf( "var_char = (Just a test;) %s" _EOL_, ( ptr ) ? ptr + 2 : "Err" );
      ulLen = 256;
      iRes = LetoVarGetC( pConnection, "main", "var_char", szData, &ulLen );
      printf( "var_char = (Just a test;) %s  (len: %lu)" _EOL_, szData, ulLen );

      ptr = LetoVarGet( pConnection, "main", "var_binary", NULL );
      if( ptr && *( ptr + 2 ) == '\0' && *( ptr + 3 ) == ';' &&
                 *( ptr + 4 ) == '\1' && *( ptr + 5 ) == ';' &&
                 *( ptr + 6 ) == '\0' )
         printf( "var_binary = CHR(0);CHR(1);CHR(0) %s" _EOL_, "Ok" );
      else
         printf( "var_binary != CHR(0);CHR(1);CHR(0) %s" _EOL_, "Err" );

      ulLen = 256;
      iRes = LetoVarGetC( pConnection, "main", "var_binary", szData, &ulLen );
      ptr = szData;
      if( iRes && *( ptr + 2 ) == '\0' && *( ptr + 3 ) == ';' &&
                  *( ptr + 4 ) == '\1' && *( ptr + 5 ) == ';' &&
                  ulLen == 5 )
         printf( "var_binary = CHR(0);CHR(1);CHR(0) %s  (len: %lu)" _EOL_, "Ok", ulLen );
      else
         printf( "var_binary != CHR(0);CHR(1);CHR(0) %s  (len: %lu)" _EOL_, "Err", ulLen );

      ptr = LetoVarGet( pConnection, "main", "var_log", NULL );
      iRes = LetoVarGetC( pConnection, "main", "var_log", &fData, NULL );
      printf( "var_log = [1] %s true: %s" _EOL_, ( ptr ? ptr + 2 : "Err" ), fData ? "true" : "false" );

      ptr = LetoVarGet( pConnection, "main", "var_date", NULL );
      /* min length 9 must be ensured ! */
      iRes = LetoVarGetC( pConnection, "main", "var_date", szDate, NULL );
      printf( "var_date = (20230102) %s  (%s)" _EOL_, ptr + 2, szDate );


      /* delete vars */
      printf( _EOL_ );
      printf( "Delete var_char [Ok] " );
      iRes = LetoVarDel( pConnection, "main", "var_char" );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      printf( "Delete var_binary [Ok] " );
      iRes = LetoVarDel( pConnection, "main", "var_binary" );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      printf( "Delete var_log [Ok] " );
      iRes = LetoVarDel( pConnection, "main", "var_log" );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      printf( "Delete var_int [Ok] " );
      iRes = LetoVarDel( pConnection, "main", "var_int" );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      printf( "Delete var_dec [Ok] " );
      iRes = LetoVarDel( pConnection, "main", "var_dec" );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      printf( "Delete var_date [Ok] " );
      iRes = LetoVarDel( pConnection, "main", "var_date" );
      if( iRes )
         printf( "Ok" _EOL_ );
      else
         printf( "Err( %d )" _EOL_, LetoGetError() );

      printf( _EOL_ "logging out ..." _EOL_ );
      LetoConnectionClose( pConnection );
   }
   else
      printf( "Connection failure" _EOL_ );

   LetoExit( 1 );
}
