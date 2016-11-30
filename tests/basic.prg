/* a snippet to show the basic structure of an application using LetoDBf */

REQUEST LETO

/* by using 'letodb.hbc', example:  hbmk2 basic.prg letodb.hbc
 * this will automatic include rdddleto.ch */
#include "rddleto.ch"

PROCEDURE main( cAddress )  /* pure IP-address */

   IF Empty( cAddress )
      cAddress := "//127.0.0.1:2812/"
   ELSE
      cAddress := "//" + cAddress + IIF( ":" $ cAddress, "", ":2812" )
      cAddress += IIF( Right( cAddress, 1 ) == "/", "", "/" )
   ENDIF

   IF leto_Connect( cAddress ) < 0
      ALERT( "NO LETODB SERVER FOUND - ERROR: " + leto_Connect_Err( .T. ) )
      QUIT
   ELSE
      // RDDSETDEFAULT( "LETO" )     /* automatic set by: REQUEST LETO */
      // LETO_DBDRIVER( "DBFCDX" )   /* to choose your DBF driver independent of the server default */ 
      LETO_TOGGLEZIP( 1 )         /* switch compressed network traffic */
   ENDIF

   // DbUseArea( .T., /* LETO */, "test", "TEST", .T. )
   // ...

   /* no special logout needed, all is automically done */
RETURN
