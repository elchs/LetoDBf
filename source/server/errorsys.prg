/*
 * The default error handler
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 * updated for Leto db server
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

#include "common.ch"
#include "error.ch"

REQUEST WUsLog

PROCEDURE ErrorSys

   ErrorBlock( {| oError | DefError( oError ) } )

   RETURN

STATIC FUNCTION DefError( oError )

   LOCAL nFlags := 0

   // By default, division by zero results in zero
   IF ValType( oError ) == "O" .AND. oError:genCode == EG_ZERODIV .AND. oError:canSubstitute
      RETURN 0
   ENDIF

   // By default, retry on RDD lock error failure */
   IF oError:genCode == EG_LOCK .AND. oError:canRetry
      // oError:tries++
      RETURN .T.
   ENDIF

   // Set NetErr() of there was a database open error
   IF oError:genCode == EG_OPEN .AND. oError:osCode == 32 .AND. oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   // Set NetErr() if there was a lock error on dbAppend()
   IF oError:genCode == EG_APPENDLOCK .AND. oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   IF oError:canRetry
      nFlags += 1  /* EF_CANRETRY */
   ENDIF
   IF oError:canSubstitute
      nFlags += 2  /* EF_CANSUBSTITUTE */
   ENDIF
   IF oError:canDefault
      nFlags += 4  /* EF_CANDEFAULT */
   ENDIF

   wUsLog( Leto_ErrorMessage( oError ) + iif( Empty( oError:osCode ), "", ;
                 " (DOS Error " + LTrim( Str( oError:osCode ) ) + ")" ) )

   Leto_SetHrbError( oError:genCode, oError:subCode, oError:osCode, nFlags, IIF( Empty( oError:filename ), .F., .T. ),;
                     IIF( ! Empty( oError:filename ), oError:filename, oError:description + " " + oError:operation ) )
   IF .T.
      Break( oError )
   ENDIF

   RETURN .F.

/* used for letodb.log */
FUNCTION Leto_ErrorMessage( oError )

   LOCAL cMessage

   // start error message
   cMessage := iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "

   // add subsystem name if available
   cMessage += iif( ISCHARACTER( oError:subsystem ), oError:subsystem(), "???" )

   // add subsystem's error code if available
   cMessage += iif( ISNUMBER( oError:subCode ), "/" + LTrim( Str( oError:subCode ) ), "/???" )

   // add error description if available
   IF ISCHARACTER( oError:description )
      cMessage += "  " + oError:description
   ENDIF

   // add either filename or operation

   IF ! Empty( oError:filename )
      cMessage += ": " + oError:filename
   ELSEIF ! Empty( oError:operation )
      cMessage += ": " + oError:operation
   ENDIF

   IF ValType( oError:Args ) == "A"
      cMessage += ( hb_eol() + " Arguments: (" + Arguments( oError ) + ")" )
   ENDIF

   RETURN cMessage

STATIC FUNCTION Arguments( oErr )

   LOCAL xArg, cArguments := "", i

   IF ValType( oErr:Args ) == "A"
      FOR i := 1 TO  Len( oErr:Args )
         xArg := oErr:Args[ i ]
         cArguments += " [" + Str( i, 2 ) + "] = Type: " + ValType( xArg )
         IF xArg != NIL
            cArguments += " Val: " + hb_ValToStr( xArg )
         ENDIF
         IF i < Len( oErr:Args )
            cArguments += ","
         ENDIF
      NEXT
   ENDIF

   RETURN cArguments

FUNCTION WUsLog( cText )  // user log

   leto_wUsLog( "      " + cText + hb_eol() )

   RETURN .T.

FUNCTION WRLog( cText )   // server log

   leto_wRlog( "      " + cText + hb_eol() )

   RETURN .T.
