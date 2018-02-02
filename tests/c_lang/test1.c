
/* set it before ! */
#define __LETO_C_API__
#include "letocl.h"

/* needed for LetoUdf() */
#include "hbapi.h"
#include "hbapiitm.h"

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
      char         szData[ 64 ];
      PHB_ITEM     pItem = hb_itemNew( NULL );  /* at least empty item, or hb_itemArrayNew( 0 ) */

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

      printf( _EOL_ "remote execution test" _EOL_ );

      /* kidding a bit with LetoDBf server */
      if( ! LetoUdf( pConnection, NULL, HB_FALSE, "Truly_Nonsense", NULL ) )
         printf( "Executed safely a not existing function" _EOL_ );

      /* without function params and no return value */
      if( LetoUdf( pConnection, NULL, HB_FALSE, "HB_MILLISECONDS", NULL ) )
         printf( "hb_MilliSeconds() successful ( not fetching result )" _EOL_ );

      /* without function params but return value */
      hb_arrayNew( pItem, 0 );
      if( LetoUdf( pConnection, NULL, HB_FALSE, "HB_MILLISECONDS", &pItem ) )
         printf( "hb_MilliSeconds(): <%lu>" _EOL_, hb_itemGetNL( pItem ) );

      hb_arrayNew( pItem, 0 );
      if( LetoUdf( pConnection, NULL, HB_FALSE, "DATE", &pItem ) )
      {
         char szDate[ 11 ];

         hb_itemGetDS( pItem, szDate );
         printf( "Date(): <%s>" _EOL_, szDate );
      }

      hb_arrayNew( pItem, 1 );  /* one param for the remote func */
      hb_arraySetNI( pItem, 1, 666 );
      if( LetoUdf( pConnection, NULL, HB_FALSE, "STR", &pItem ) )
         printf( "STR(666): <%s>" _EOL_, hb_itemGetCPtr( pItem ) );

      hb_arrayNew( pItem, 3 );  /* three params for the remote func */
      hb_arraySetC( pItem, 1, "xxxelchxxx" );
      hb_arraySetNI( pItem, 2, 4 );
      hb_arraySetNI( pItem, 3, 4 );
      if( LetoUdf( pConnection, NULL, HB_FALSE, "SUBSTR", &pItem ) )
         printf( "SUBSTR('xxxelchxxx',4,4): <%s>" _EOL_, hb_itemGetCPtr( pItem ) );

      /* and don't forget ! */
      hb_itemRelease( pItem );

      printf( _EOL_ "logging out ..." _EOL_ );
      LetoConnectionClose( pConnection );
   }
   else
      printf( "Connection failure" _EOL_ );

   LetoExit( 1 );
}
