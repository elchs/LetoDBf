/*
 * Harbour Project source code:
 * Leto BMDBF* functions
 *
 * Copyright 2013 Pavel Tsarenko <tpe2 / at / mail.ru>
 * www - http://www.harbour-project.org
 *           2016 Rolf 'elch' Beckmann
 * return values, remove dangerous LETO_PARSEREC() --> DbGoTo()
 * removed array to string conversions
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

STATIC FUNCTION IsLeto
   RETURN Used() .AND. rddName() == "LETO"

FUNCTION LBM_DbGetFilterArray()

   LOCAL aFilterRec := {}

   IF IsLeto()
      aFilterRec := leto_UDF( "LBM_DbGetFilterArray" )
   ENDIF

   RETURN IIF( VALTYPE( aFilterRec ) == "A", aFilterRec, {} )

FUNCTION LBM_DbSetFilterArray( aFilterRec )

   LOCAL xRet := .F.

   IF IsLeto() .AND. ValType( aFilterRec ) == "A" .AND. LEN( aFilterRec ) > 0
      xRet := leto_UDF( "LBM_DbSetFilterArray", aFilterRec )
      IF VALTYPE( xRet ) == "L" .AND. xRet
         leto_SetBM()
      ENDIF
   ENDIF

   RETURN xRet

FUNCTION LBM_DbSetFilterArrayAdd( aFilterRec )

   LOCAL xRet := .F.

   IF IsLeto() .AND. ValType( aFilterRec ) == "A"  .AND. LEN( aFilterRec ) > 0
      xRet := leto_UDF( "LBM_DbSetFilterArrayAdd", aFilterRec )
      IF VALTYPE( xRet ) == "L" .AND. xRet
        leto_SetBM()
      ENDIF
   ENDIF

   RETURN xRet

FUNCTION LBM_DbSetFilterArrayDel( aFilterRec )

   LOCAL xRet := .F.

   IF IsLeto() .AND. ValType( aFilterRec ) == "A"  .AND. LEN( aFilterRec ) > 0
      xRet := leto_UDF( "LBM_DbSetFilterArrayDel", aFilterRec )
      IF VALTYPE( xRet ) == "L" .AND. xRet
         leto_SetBM()
      ENDIF
   ENDIF

   RETURN xRet

/*  ToDo much too dangerous, SCOPE and FILTER need pre-validation */
FUNCTION LBM_DbSetFilter( xScope, xScopeBottom, cFilter )

   LOCAL nRecno, xRet := .F.

   IF IsLeto()
      nRecno := leto_Udf( "LBM_DbSetFilter", xScope, xScopeBottom, OrdName( 0 ), cFilter, Set( _SET_DELETED ) )
      IF VALTYPE( nRecno ) == "N" .AND. nRecno > 0
         leto_SetBM()
         DBGOTO( nRecno )
         xRet := .T.
      ENDIF
   ENDIF

   RETURN xRet

