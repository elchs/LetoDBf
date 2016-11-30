/*
 * Harbour Project source code:
 * LetoDB COM server
 *
 * Copyright 2012 Pavel Tsarenko <tpe2 / at / mail.ru>
 * www - http://www.harbour-project.org
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

/*

 * To register LetoDB com server, type:

   regsvr32.exe letosrv.dll

   Connecting with LetoDB COM server:

   Harbour:
   oLeto := win_oleCreateObject( "LetoDB" )
   or xHarbour:
   oLeto := TOleAuto():new( "LetoDB" )

   cSrv := "//127.0.0.1:2812"
   ? oLeto:Connect( cSrv )
   ? oLeto:version
   oTable := oLeto:Open( cSrv + "/customer" )
   ? oTable:RecordCount()
   ? oTable:MoveFirst()
   ? oTable:FieldGet( "CODE" )
   oTable:Close()
   oLeto:Close()

 */

#define CLS_Name  "LetoDB"
#define CLS_ID    "{4C65746F-4442-3230-985F-8BA5E2AE8481}"

#include "error.ch"
#include "hbclass.ch"

PROCEDURE DllMain()

   REQUEST leto, dbfcdx
   rddSetDefault( "LETO" )
   set deleted off

   WIN_OleServerInit( CLS_ID, CLS_Name, {|| LetoDBSrv():new() } )

RETURN

CREATE CLASS LetoDBSrv

HIDDEN:

   VAR      nConn
   VAR      cAddress
   VAR      cVersion INIT ""

EXPORTED:

   METHOD   Connect( cAddress )
   METHOD   Close INLINE leto_disconnect()
   METHOD   Version INLINE leto_GetServerVersion()
   METHOD   Open( cName )

ENDCLASS

METHOD Connect( cAddress ) CLASS LetoDBSrv
   ::nConn := leto_Connect( cAddress )
RETURN ::nConn

METHOD Open( cName ) CLASS LetoDBSrv
RETURN letoTable():new( cName )


CREATE CLASS letoTable
HIDDEN:
   VAR      nConn
   VAR      cName
   VAR      cAlias
EXPORTED:

   METHOD   New( cName ) CONSTRUCTOR
   METHOD   Close INLINE (::cAlias)->( dbCloseArea() )
   METHOD   GetStruct INLINE (::cAlias)->( dbStruct() )
   METHOD   FieldCount INLINE (::cAlias)->( FCount() )

   METHOD   Eof INLINE (::cAlias)->( Eof() )
   METHOD   Bof INLINE (::cAlias)->( Eof() )
   METHOD   RecNo INLINE (::cAlias)->( RecNo() )
   METHOD   RecordCount INLINE (::cAlias)->( RecCount() )

   METHOD   FieldPos( cField ) INLINE (::cAlias)->( FieldPos( cField ) )
   METHOD   FieldGet( uParam )
   METHOD   FieldPut( uParam, uValue )

   METHOD   MoveFirst INLINE (::cAlias)->( dbGoTop() )
   METHOD   MoveLast INLINE (::cAlias)->( dbGoBottom() )
   METHOD   Move( nSkip ) INLINE (::cAlias)->( dbSkip( nSkip ) )
   METHOD   Seek( xKey, lPartial, lLast ) INLINE (::cAlias)->( dbSeek( xKey, lPartial, lLast ) )
   METHOD   Goto( nRecord ) INLINE (::cAlias)->( dbGoto( nRecord ) )

   ERROR HANDLER OnError( uParam )

ENDCLASS

METHOD New( cName ) CLASS letoTable
   LOCAL lSuccess := .T., oError
   BEGIN SEQUENCE WITH { |e|break( e ) }
      dbUseArea( .t.,, cName)
      ::cAlias := Alias()
   RECOVER USING oError
      lSuccess := .F.
   END SEQUENCE
RETURN if(lSuccess, Self, Nil)

METHOD FieldGet( uParam ) CLASS letoTable
   LOCAL uValue
   IF hb_isNumeric( uParam )
      uValue := (::cAlias)->(FieldGet( uParam ))
   ELSEIF hb_isString( uParam )
      uValue := (::cAlias)->(FieldGet( FieldPos( uParam ) ))
   ENDIF
RETURN uValue

METHOD FieldPut( uParam, uValue ) CLASS letoTable
   IF hb_isNumeric( uParam )
      uValue := (::cAlias)->(FieldPut( uParam, uValue ))
   ELSEIF hb_isString( uParam )
      uValue := (::cAlias)->(FieldPut( FieldPos( uParam ), uValue ))
   ENDIF
RETURN uValue

METHOD OnError( uParam ) CLASS letoTable

   LOCAL cMsg := __GetMessage()
   LOCAL nPos
   LOCAL uRet, oErr

   if uParam != nil .and. LEFT( cMsg, 1 ) == '_'
      cMsg := SubStr( cMsg, 2 )
   endif
   nPos := (::cAlias)->( FieldPos(cMsg) )

   if nPos != 0
      uRet := (::cAlias)->( if(uParam == nil, FieldGet(nPos), FieldPut(nPos, uParam)) )
   else

      oErr := ErrorNew()
      oErr:Args          := { Self, cMsg, uParam }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "Invalid class member"
      oErr:GenCode       := EG_NOVARMETHOD
      oErr:Operation     := "letoTable:" + cMsg
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := -1
      oErr:SubSystem     := "letoTable"
      uRet := Eval( ErrorBlock(), oErr )

   endif

   RETURN uRet

ANNOUNCE GT_SYS
REQUEST HB_GT_GUI_DEFAULT
