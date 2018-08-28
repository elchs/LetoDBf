/*
 * Ini files reading procedure
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "fileio.ch"
#define STR_BUFLEN  4096

FUNCTION RDINI( fname )

   LOCAL han, stroka, strfull, poz1, vname, arr
   LOCAL strbuf := Space( STR_BUFLEN ), poz := STR_BUFLEN + 1

   IF ( han := FOpen( fname, FO_READ + FO_SHARED ) ) != - 1
      arr := {}
      strfull := ""
      DO WHILE .T.
         IF Len( stroka := RDSTR( han, @strbuf, @poz, STR_BUFLEN ) ) == 0
            EXIT
         ENDIF
         IF Right( stroka, 1 ) == '&'
            strfull += Left( stroka, Len( stroka ) -1 )
            LOOP
         ELSE
            IF !Empty( strfull )
               stroka := strfull + stroka
            ENDIF
            strfull := ""
         ENDIF

         IF Left( stroka, 1 ) == "["
            stroka := Upper( SubStr( stroka, 2, At( "]", stroka ) - 2 ) )
            AAdd( arr, { stroka, {} } )
         ELSEIF Left( stroka, 1 ) <> ";"
            poz1 := At( "=", stroka )
            IF poz1 != 0
               IF Empty( arr )
                  AAdd( arr, { "MAIN", {} } )
               ENDIF
               vname  := AllTrim( Left( stroka, poz1 - 1 ) )
               stroka := AllTrim( SubStr( stroka, poz1 + 1 ) )
               AAdd( arr[ Len( arr ), 2 ], { Upper( vname ), stroka } )
            ENDIF
         ENDIF
      ENDDO
      FClose( han )
   ENDIF

   RETURN arr

STATIC FUNCTION RDSTR( han, strbuf, poz, buflen )

   LOCAL stro := "", rez, oldpoz, poz1

   oldpoz := poz
   poz    := At( Chr( 10 ), SubStr( strbuf, poz ) )
   IF poz = 0
      IF han <> Nil
         stro += SubStr( strbuf, oldpoz )
         rez  := FRead( han, @strbuf, buflen )
         IF rez = 0
            RETURN ""
         ELSEIF rez < buflen
            strbuf := SubStr( strbuf, 1, rez ) + Chr( 10 ) + Chr( 13 )
         ENDIF
         poz  := At( Chr( 10 ), strbuf )
         stro += SubStr( strbuf, 1, poz )
      ELSE
         stro += RTrim( SubStr( strbuf, oldpoz ) )
         poz  := oldpoz + Len( stro )
         IF Len( stro ) == 0
            RETURN ""
         ENDIF
      ENDIF
   ELSE
      stro += SubStr( strbuf, oldpoz, poz )
      poz  += oldpoz - 1
   ENDIF
   poz ++
   poz1 := Len( stro )
   IF poz1 > 2 .AND. Right( stro, 1 ) $ Chr( 13 ) + Chr( 10 )
      IF SubStr( stro, poz1 - 1, 1 ) $ Chr( 13 ) + Chr( 10 )
         poz1 --
      ENDIF
      stro := SubStr( stro, 1, poz1 - 1 )
   ENDIF

   RETURN stro

