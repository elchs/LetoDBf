
FUNCTION hbmk_plugin_rand( hbmk )

   LOCAL tmp

   SWITCH hbmk[ "cSTATE" ]

   CASE "pre_all"
      tmp := RandomString( 42 )
      hbmk_AddOption_PRG( hbmk, "-D__RANDOM_STRING__=" + CHR( 34 ) + tmp + CHR( 34 ) )
      hbmk_AddOption_C( hbmk, "-D__RANDOM_STRING__='" + CHR( 34 ) + tmp + CHR( 34 ) + "'" )
      EXIT

   ENDSWITCH

RETURN NIL

STATIC FUNCTION RandomString( nLen )
   LOCAL tmp := ""
   LOCAL i := 1
   LOCAL aInvalid := { 92, 96, 239 }
   LOCAL n

   hb_randomSeed( hb_milliseconds() )
   DO WHILE i < nLen
      n := INT( hb_random() * 254 )
      If n > 39 .AND. ASCAN( aInvalid, n ) == 0
         tmp += CHR( n )
         i++
      ENDIF
   ENDDO
RETURN tmp
