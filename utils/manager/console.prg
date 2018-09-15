/*
 * Console management utility for Leto db server
 *
 * Copyright 2008 Alexander S.Kresin <alex@belacy.belgorod.su>
 *
 * nearly complete new written and re-designed, for use with LetoDBf
 * Copyright 2016 Rolf 'elch' Beckmann
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

#include "hbclass.ch"
#include "rddleto.ch"
#include "cmdleto.h"

#include "inkey.ch"
#include "box.ch"
#include "dbinfo.ch"
#include "achoice.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"
#include "memoedit.ch"

#define CHR_HEADSEP   CHR( 205 ) + CHR( 203 ) + CHR( 205 )
#define CHR_FOOTSEP   CHR( 205 ) + CHR( 202 ) + CHR( 205 )
#define CHR_COLSEP    " " + CHR( 186 ) + " "

REQUEST LETO
REQUEST RDDInfo

//#define SECURED    1    /* activate for netork traffic encryption */


STATIC s_nLastKeyType
STATIC s_nRefresh := 1000  /* 1 seconds default */
STATIC s_myConn := -1

FUNCTION Main( cAddress, cUser, cPasswd )
 LOCAL aBrows := {}
 LOCAL aBlocks := {}
 LOCAL aPos := {}
 LOCAL aLastPos
 LOCAL nBrow := 1

 LOCAL nKey := 0
 LOCAL cIniName := "letodb.ini"
 LOCAL cDirBase := hb_dirBase()
 LOCAL aIni, ai
 LOCAL cPort := "2812"
 LOCAL cVersion
 LOCAL oColumn
 LOCAL nLastRefresh := 0
 LOCAL nLastLen, nNewLen
 LOCAL nBrowTmp
 LOCAL lHaveFocus := .T.
 LOCAL lReconfigure := .T.
 LOCAL nExtra, nAllExtra
 LOCAL mrow, mcol
 LOCAL cMode
 LOCAL nTmp
 LOCAL lResizing
 LOCAL nMaxRow, nMaxCol

   s_nLastKeyType := hb_MilliSeconds()
   IF VALTYPE( cAddress ) != "C"
      cAddress := ""
   ENDIF

   ALTD()
   CLS

   IF EMPTY( cAddress )
      IF File( cDirBase + cIniName )
         aIni := rdIni( cDirBase + cIniName )
#ifdef HB_OS_UNIX
      ELSEIF File( "/etc/" + cIniName )
         aIni := rdIni( "/etc/" + cIniName )
#endif
      ENDIF
   ENDIF

   if EMPTY( cAddress ) .AND. ! Empty(aIni)
      for each ai in aIni[1, 2]
         if ai[1] == "SERVER"
            cAddress := ai[2]
         elseif ai[1] == "PORT"
            cPort := ai[2]
         elseif ai[1] == "USER"
            cUser := ai[2]
         elseif ai[1] == "PASSWORD"
            cPasswd := ai[2]
         endif
      next
   endif

   IF LEFT( cAddress,2 ) != "//"
      cAddress := "//" + cAddress
   ENDIF
   IF ! ":" $ cAddress .AND. ! EMPTY( cPort )
      cAddress +=  ":" + cPort
   ENDIF
   IF RIGHT( cAddress,1 ) != "/"
      cAddress += "/"
   ENDIF

   RddSetDefault( "LETO" )
   SET SCOREBOARD OFF
   SET CONFIRM ON
   SET CURSOR OFF
   SET(_SET_EVENTMASK, INKEY_ALL - INKEY_MOVE + HB_INKEY_GTEVENT )   /* 234 ohne LbUP  RbUp */
   hb_GtInfo( HB_GTI_WINTITLE, "LetoDBf monitor" )
   hb_GtInfo( HB_GTI_RESIZABLE, .T.)
   hb_GtInfo( HB_GTI_RESIZEMODE,  HB_GTI_RESIZEMODE_ROWS )
   hb_GtInfo( HB_GTI_CLOSABLE, .T. )
   hb_GtInfo( HB_GTI_ALTENTER, .T.)
   IF HB_GtVersion() == "WVT"
#if ! defined( __HARBOUR30__ )
      hb_GtInfo( HB_GTI_FONTATTRIBUTE, HB_GTI_FONTA_FIXMETRIC + HB_GTI_FONTA_CLRBKG + HB_GTI_FONTA_DRAWBOX )
#endif
   ENDIF
   IF MPresent()
      MDblClk( 421 )
      MShow()
   ENDIF

   ? "Connecting to " + cAddress
   /* LETO_CONNECT( cAddress, [ cUserName ], [ cPassword ], [ nTimeOut ], [ nBufRefreshTime ], [ lZombieCheck ] ) */
   IF leto_Connect( cAddress, cUser, cPasswd, 21000, , .T. ) == -1
      TimedALERT( leto_Connect_Err( .T. ), 3 )
      RETURN Nil
   ELSE
      /* activate compression with encryption */
#ifdef SECURED
      leto_togglezip( 1, "encrypted" )
#else
      leto_togglezip( 1 )
#endif
   ENDIF
   cVersion := leto_GetServerVersion()
   cMode := Leto_MgSysInfo()[ 5 ]

   /* connections */
   AADD( aPos, 1 )
   AADD( aBrows, TBrowseNew( 9, 0, 12, MAXCOL() ) )
   ATAIL( aBrows ):colorSpec := "BG/N,W+/B"
   ATAIL( aBrows ):cargo := {}
   ATAIL( aBrows ):GoTopBlock := { || aPos[ 1 ] := 1 }
   ATAIL( aBrows ):GoBottomBlock := { || aPos[ 1 ] := IIF( EMPTY( aBrows[ 1 ]:cargo ), 1, LEN( aBrows[ 1 ]:cargo ) ) }
   ATAIL( aBrows ):SkipBlock := { | nSkip | ArrSkip( aBrows[ 1 ]:cargo, @aPos[ 1 ], nSkip ) }
   ATAIL( aBrows ):headSep   := CHR_HEADSEP
   /* ATAIL( aBrows ):footSep   := CHR_FOOTSEP */
   ATAIL( aBrows ):colSep    := CHR_COLSEP
   oColumn := TbColumnNew( "ID", ArrBlock( ATAIL( aBrows ), 1, @aPos[ 1 ] ) )
   oColumn:width := 4
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "IP address", ArrBlock( ATAIL( aBrows ), 2, @aPos[ 1 ] ) )
   oColumn:width := 15
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "system name", ArrBlock( ATAIL( aBrows ), 3, @aPos[ 1 ] ) )
   oColumn:width := 17
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "executable", ArrBlock( ATAIL( aBrows ), 4, @aPos[ 1 ] ) )
   oColumn:width := 15
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( " RDD ", ArrBlock( ATAIL( aBrows ), 7, @aPos[ 1 ] ) )
   oColumn:width := 5
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "last active", ArrBlock( ATAIL( aBrows ), 5, @aPos[ 1 ] ) )
   oColumn:width := 9
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "last action", ArrBlock( ATAIL( aBrows ), 6, @aPos[ 1 ] ) )
   oColumn:width := 64
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "username", ArrBlock( ATAIL( aBrows ), 8, @aPos[ 1 ] ) )
   oColumn:width := 12
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   ATAIL( aBrows ):freeze := 1
   AADD( aBlocks, { | oBrow | GetAllConnections( @oBrow ) } )

   /* databases */
   AADD( aPos, 1 )
   AADD( aBrows, TBrowseNew( 14, 0, 18, MAXCOL() - 20 ) )
   ATAIL( aBrows ):colorSpec := "G/N,W+/B"
   ATAIL( aBrows ):cargo := {}
   ATAIL( aBrows ):GoTopBlock := { || aPos[ 2 ] := 1 }
   ATAIL( aBrows ):GoBottomBlock := { || aPos[ 2 ] := IIF( EMPTY( aBrows[ 2 ]:cargo ), 1, LEN( aBrows[ 2 ]:cargo ) ) }
   ATAIL( aBrows ):SkipBlock := { | nSkip | ArrSkip( aBrows[ 2 ]:cargo, @aPos[ 2 ], nSkip ) }
   ATAIL( aBrows ):headSep   := CHR_HEADSEP
   ATAIL( aBrows ):colSep    := CHR_COLSEP
   oColumn := TbColumnNew( "Area", ArrBlock( ATAIL( aBrows ), 3, @aPos[ 2 ] ) )
   oColumn:width := 5
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "Alias", ArrBlock( ATAIL( aBrows ), 4, @aPos[ 2 ] ) )
   oColumn:width := 15
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "Mode", ArrBlock(ATAIL( aBrows ), 5, @aPos[ 2 ] ) )
   oColumn:width := 4
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "Filename", ArrBlock( ATAIL( aBrows ), 2, @aPos[ 2 ] ) )
   oColumn:width := MAXCOL() - 31 - 21
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "Memo", ArrBlock( ATAIL( aBrows ), 7, @aPos[ 2 ] ) )
   oColumn:width := 4
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "RDD", ArrBlock( ATAIL( aBrows ), 6, @aPos[ 2 ] ) )
   oColumn:width := 8
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   AADD( aBlocks, { | oBrow | oBrow:cargo := GetTables( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ) ) } )

   /* indexes */
   AADD( aPos, 1 )
   AADD( aBrows, TBrowseNew( 20, 0, MAXROW(), MAXCOL() - 20 ) )
   ATAIL( aBrows ):colorSpec := "RB/N,W+/B"
   ATAIL( aBrows ):cargo := {}
   ATAIL( aBrows ):GoTopBlock := { || aPos[ 3 ] := 1 }
   ATAIL( aBrows ):GoBottomBlock := { || aPos[ 3 ] := IIF( EMPTY( aBrows[ 3 ]:cargo ), 1, LEN( aBrows[ 3 ]:cargo ) ) }
   ATAIL( aBrows ):SkipBlock := { | nSkip | ArrSkip( aBrows[ 3 ]:cargo, @aPos[ 3 ], nSkip ) }
   ATAIL( aBrows ):headSep   := CHR_HEADSEP
   ATAIL( aBrows ):colSep    := CHR_COLSEP
   oColumn := TbColumnNew( "Tagname", ArrBlock( ATAIL( aBrows ), 3, @aPos[ 3 ] ) )
   oColumn:width := 11
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "Key", ArrBlock( ATAIL( aBrows ), 4, @aPos[ 3 ] ) )
   oColumn:width := MAXCOL() - 12 - 21
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "Filename", ArrBlock( ATAIL( aBrows ), 2, @aPos[ 3 ] ) )
   oColumn:width := MAXCOL() - 23
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   ATAIL( aBrows ):freeze := 1
   AADD( aBlocks, { | oBrow | oBrow:cargo := GetIndex( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ), ActiveDatabase( aBrows[ 2 ]:cargo, aPos[ 2 ] ) ) } )

   /* locks list */
   AADD( aPos, 1 )
   AADD( aBrows, TBrowseNew( 14, MAXCOL() - 18, MAXROW(), MAXCOL() ) )
   ATAIL( aBrows ):colorSpec := "R/N,W+/B"
   ATAIL( aBrows ):cargo := {}
   ATAIL( aBrows ):GoTopBlock := { || aPos[ 4 ] := 1 }
   ATAIL( aBrows ):GoBottomBlock := { || aPos[ 4 ] := IIF( EMPTY( aBrows[ 4 ]:cargo ), 1, LEN( aBrows[ 4 ]:cargo ) ) }
   ATAIL( aBrows ):SkipBlock := { |nSkip| ArrSkip( aBrows[ 4 ]:cargo, @aPos[ 4 ], nSkip ) }
   ATAIL( aBrows ):headSep   := CHR_HEADSEP
   ATAIL( aBrows ):colSep    := CHR_COLSEP
   oColumn := TbColumnNew( "    Record Lock    ", ArrBlock( ATAIL( aBrows ), 2, @aPos[ 4 ] ) )
   oColumn:width := 17
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "Filename", ArrBlock( ATAIL( aBrows ), 1, @aPos[ 4 ] ) )
   oColumn:width := 21
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   AADD( aBlocks, { | oBrow | oBrow:cargo := GetLocks( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ), ActiveDatabase( aBrows[ 2 ]:cargo, aPos[ 2 ] ) ) } )

   aLastPos := Aclone( aPos )
   EVAL( aBlocks[ 1 ], aBrows[ 1 ] )

   DO WHILE nKey <> K_ESC .AND. nKey <> HB_K_CLOSE

      lResizing := .F.

      IF lReconfigure .AND. lHaveFocus
         nMaxRow := MAXROW()
         nMaxCol := MAXCOL()
         IF MAXROW() < 24 .OR. MAXCOL() < 79
            SETMODE( 25, 80 )
         ENDIF
         ServerInfo( cAddress, cVersion, cMode )

         nExtra := nMaxRow - 21
         IF nExtra > 0
            nAllExtra = INT( nExtra / 3 )
            IF nAllExtra > 3
               nAllExtra = 3
            ENDIF
            IF nAllExtra > 0
               nExtra -= nAllExtra * 3
            ENDIF
         ELSE
            nExtra := 0
            nAllExtra := 0
         ENDIF

         @ 8, 0, nMaxRow, nMaxCol BOX SPACE( 9 ) COLOR aBrows[ 1 ]:colorSpec
         aBrows[ 1 ]:nRight := MAX( nMaxCol, 40 )
         aBrows[ 1 ]:nBottom := MAX( 11 + nAllExtra + INT( nExtra / 2 ), 12 )
         aBrows[ 1 ]:configure()
         aBrows[ 1 ]:freeze := 1

         aBrows[ 2 ]:nTop := 13 + nAllExtra + INT( nExtra / 2 )
         aBrows[ 2 ]:nRight := MAX( nMaxCol - 20, 20 )
         aBrows[ 2 ]:nBottom := MAX( nMaxRow - 5 - nAllExtra, 16 )
         oColumn := aBrows[ 2 ]:getColumn( 4 )
         oColumn:width := MAX( nMaxCol - 31 - 21, 1 )
         aBrows[ 3 ]:setColumn( 4, oColumn )
         aBrows[ 2 ]:configure()

         aBrows[ 3 ]:nTop := MAX( nMaxRow - 3 - nAllExtra, 17 )
         aBrows[ 3 ]:nRight := MAX( nMaxCol - 20, 20 )
         aBrows[ 3 ]:nBottom := MAX( nMaxRow, 20 )
         oColumn := aBrows[ 3 ]:getColumn( 2 )
         oColumn:width := MAX( nMaxCol - 12 - 21, 1 )
         aBrows[ 3 ]:setColumn( 2, oColumn )
         oColumn := aBrows[ 3 ]:getColumn( 3 )
         oColumn:width := MAX( nMaxCol - 23, 1 )
         aBrows[ 3 ]:setColumn( 3, oColumn )
         aBrows[ 3 ]:configure()
         aBrows[ 3 ]:freeze := 1

         aBrows[ 4 ]:nTop := 13 + nAllExtra + ( nExtra / 2 )
         aBrows[ 4 ]:nLeft := MAX( nMaxCol - 18, 21 )
         aBrows[ 4 ]:nBottom := MAX( nMaxRow, 20 )
         aBrows[ 4 ]:nRight := MAX( nMaxCol, 23 )
         aBrows[ 4 ]:configure()
         nLastRefresh := 1
         lReconfigure := .F.

         nBrowTmp := nBrow
         nBrow := 1
         DispBegin()
         DO WHILE nBrow <= LEN( aBrows )
            BrowseFrames( aBrows, nBrow, nBrowTmp )
            aBrows[ nBrow ]:refreshAll()
            DO WHILE .NOT. aBrows[ nBrow ]:Stabilize()
            ENDDO
            nBrow++
         ENDDO
         DispEnd()
         nBrow := nBrowTmp

      ENDIF

      IF hb_MilliSeconds() - nLastRefresh > s_nRefresh
         IF ! BasicInfo()  /* server down */
            EXIT
         ENDIF
      ENDIF

      nKey := 0
      IF ! aBrows[ nBrow ]:stable
         DO WHILE .NOT. aBrows[ nBrow ]:Stabilize()
            nKey := INKEY()
            IF nKey <> 0
               EXIT
            ENDIF
         ENDDO
      ENDIF

      IF aBrows[ nBrow ]:stable .AND. aPos[ nBrow ] != aLastPos[ nBrow ]
         nBrowTmp := nBrow
         DO WHILE nBrow < LEN( aBrows )
            nBrow++
            DispBegin()
            BrowseFrames( aBrows, nBrow, nBrowTmp )
            EVAL( aBlocks[ nBrow ], aBrows[ nBrow ] )
            nNewLen := IIF( EMPTY( aBrows[ nBrow ]:cargo ), 0, LEN( aBrows[ nBrow ]:cargo ) )
            IF aBrows[ nBrow ]:RowPos > nNewLen
               aBrows[ nBrow ]:RowPos := MAX( nNewLen, 1 )
               aPos[ nBrow ] := MAX( nNewLen, 1 )
            ENDIF
            aBrows[ nBrow ]:configure()
            IF nBrow == 1 .OR. nBrow == 3
               aBrows[ nBrow ]:freeze := 1
            ENDIF
            aBrows[ nBrow ]:refreshAll()

            DO WHILE .NOT. aBrows[ nBrow ]:Stabilize()
               IF nKey == 0
                  nKey := INKEY()
               ENDIF
               IF nKey <> 0
                  EXIT
               ENDIF
            ENDDO
            DispEnd()
            aLastPos[ nBrow ] := aPos[ nBrow ]
         ENDDO
         nBrow := nBrowTmp
         aLastPos[ nBrow ] := aPos[ nBrow ]
      ENDIF

      IF aBrows[ nBrow ]:stable .AND. nKey == 0
         nBrowTmp := nBrow
         nBrow := 1
         DO WHILE nBrow <= LEN( aBrows )
            DispBegin()
            BrowseFrames( aBrows, nBrow, nBrowTmp )
            IF hb_MilliSeconds() - nLastRefresh > s_nRefresh
               nLastLen := IIF( EMPTY( aBrows[ nBrow ]:cargo ), -1, LEN( aBrows[ nBrow ]:cargo ) )
               EVAL( aBlocks[ nBrow ], aBrows[ nBrow ] )
               nNewLen := IIF( EMPTY( aBrows[ nBrow ]:cargo ), -1, LEN( aBrows[ nBrow ]:cargo ) )
               IF aBrows[ nBrow ]:RowPos > nNewLen
                  aBrows[ nBrow ]:RowPos := MAX( nNewLen, 1 )
                  aPos[ nBrow ] := MAX( nNewLen, 1 )
                  aBrows[ nBrow ]:configure()
               ENDIF
               IF nLastLen != nNewLen
                  aBrows[ nBrow ]:configure()
                  IF nBrow == 1 .OR. nBrow == 3
                    aBrows[ nBrow ]:freeze := 1
                  ENDIF
               ENDIF
               aBrows[ nBrow ]:refreshAll()
            ENDIF

            DO WHILE .NOT. aBrows[ nBrow ]:Stabilize()
               IF nKey == 0
                  nKey := INKEY()
               ENDIF
               IF nKey <> 0
                  EXIT
               ENDIF
            ENDDO
            DispEnd()
            nBrow++
         ENDDO
         nBrow := nBrowTmp
         IF hb_MilliSeconds() - nLastRefresh > s_nRefresh
            nLastRefresh := hb_MilliSeconds()
         ENDIF
      ENDIF

      IF nKey == 0
         nKey := INKEY( ( s_nRefresh / 1000 ) )
         IF nKey == 0
            /* dynamical reduce number of request if constant running */
            nTmp := INT( ( hb_MilliSeconds() - s_nLastKeyType ) / 1000 / 60 )  /* minutes up time */
            IF nTmp > 720
               s_nRefresh := 60000  /* one per minute after 12 hours */
            ELSEIF nTmp > 60
               s_nRefresh := 30000
            ELSEIF nTmp > 15
               s_nRefresh := 10000
            ELSEIF nTmp > 1
               s_nRefresh := 3000
            ELSEIF nTmp > 0
               s_nRefresh := 2000
            ENDIF
         ELSE
            s_nLastKeyType := hb_MilliSeconds()
            s_nRefresh := 1000
         ENDIF
      ENDIF

      /* select browse with left click, right click == columnc change only if mouse on row */
      IF nKey == K_LBUTTONUP
         mRow := MROW()
         mCol := MCOL()
         IF mRow == 0 .AND. mCol >= 2 .AND. mCol <= 10
            nKey := 0
            IF Administrate( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ) )
               lResizing := .T.
            ENDIF
         ELSEIF mRow == 0 .AND. mCol >= MAXCOL() - 9 .AND. mCol <= MAXCOL() - 2
            IF HelpText()
               lResizing := .T.
            ENDIF
         ELSEIF mRow >= aBrows[ 1 ]:nTop - 1
            ai := ASCAN( aBrows, { | oBrow | oBrow:nTop - 1 <= mRow .AND. oBrow:nBottom >= mRow ;
                                      .AND. oBrow:nLeft <= mCol .AND. oBrow:nRight >= mCol } )
            IF ai > 0 .AND. ! EMPTY( aBrows[ ai ]: cargo ) .AND. ai != nBrow
               nBrow := ai
               nKey := 0
            ENDIF
            IF nBrow == ai
               IF mRow >= aBrows[ nBrow ]:nTop + 2 .AND. aBrows[ nBrow ]:RowPos + aBrows[ nBrow ]:nTop + 1 != mRow
                  aBrows[ nBrow ]:RowPos := mRow - aBrows[ nBrow ]:nTop - 1
                  aBrows[ nBrow ]:refreshAll()
                  nKey := 0
               ENDIF
            ELSE
               nKey := 0
            ENDIF
         ELSE
            nKey := 0
         ENDIF
      ELSEIF  nKey == K_RBUTTONUP
         mRow := MROW()
         mCol := MCOL()
         IF MRow >= aBrows[ 1 ]:nTop - 1
            ai := ASCAN( aBrows, { | oBrow | oBrow:nTop - 1 <= mRow .AND. oBrow:nBottom >= mRow ;
                                      .AND. oBrow:nLeft <= mCol .AND. oBrow:nRight >= mCol } )
            IF nBrow != ai
               nKey := 0
            ELSEIF mRow >= aBrows[ nBrow ]:nTop + 2 .AND. aBrows[ nBrow ]:RowPos + aBrows[ nBrow ]:nTop + 1 != mRow
               nKey := 0
            ENDIF
         ELSE
            nKey := 0
         ENDIF
      ELSEIF nKey == K_MWBACKWARD .OR. nKey == K_MWFORWARD
         mRow := MROW()
         mCol := MCOL()
         IF MRow >= aBrows[ 1 ]:nTop - 1
            ai := ASCAN( aBrows, { | oBrow | oBrow:nTop - 1 <= mRow .AND. oBrow:nBottom >= mRow ;
                                      .AND. oBrow:nLeft <= mCol .AND. oBrow:nRight >= mCol } )
            IF nBrow != ai
               nKey := 0
            ENDIF
         ENDIF
      ENDIF

      DO CASE
         CASE nKey == K_DOWN .OR. nKey == K_MWBACKWARD
            aBrows[ nBrow ]:Down()
         CASE nKey == K_UP .OR. nKey == K_MWFORWARD
            aBrows[ nBrow ]:Up()
         CASE nKey == K_RIGHT .OR. nKey == K_RBUTTONUP
            aBrows[ nBrow ]:Right()
         CASE nKey == K_LEFT .OR. nKey == K_LBUTTONUP
            aBrows[ nBrow ]:Left()
         CASE nKey == K_PGDN
            aBrows[ nBrow ]:PageDown()
         CASE nKey == K_PGUP
            aBrows[ nBrow ]:PageUp()
         CASE nKey == K_HOME
            aBrows[ nBrow ]:goTop()
         CASE nKey == K_END
            aBrows[ nBrow ]:goBottom()
         CASE nKey == K_TAB
            DO WHILE .T.
               IF ++nBrow > LEN( aBrows )
                  nBrow := 1
                  EXIT
               ENDIF
               IF EMPTY( aBrows[ nBrow ]:cargo )
                  nBrow++
               ELSE
                  EXIT
               ENDIF
            ENDDO
         CASE nKey == K_SH_TAB
            DO WHILE .T.
               IF --nBrow < 1
                  nBrow := LEN( aBrows )
               ENDIF
               IF ! EMPTY( aBrows[ nBrow ]:cargo )
                  EXIT
               ENDIF
            ENDDO
         CASE nKey == HB_K_LOSTFOCUS
            lHaveFocus := .F.
         CASE nKey == HB_K_GOTFOCUS
            lHaveFocus := .T.
         CASE nKey == HB_K_RESIZE
            lHaveFocus := .T.
            lResizing := .T.
         CASE nKey == K_ALT_H
            IF HelpText()
               lResizing := .T.
            ENDIF
         CASE nKey == K_ALT_U .AND. ! EMPTY( aBrows[ 2 ]:cargo )
            IF GetUserList( ActiveDatabase( aBrows[ 2 ]:cargo, aPos[ 2 ] ), aBrows[ 3 ] )
               lResizing := .T.
            ENDIF
         CASE nKey == K_ALT_M
            IF Administrate( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ) )
               lResizing := .T.
            ENDIF
         CASE nKey == K_ALT_L
            IF ViewLogs( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ) )
               lResizing := .T.
            ENDIF
         CASE nKey == K_ALT_K
            KillActiveUsers( aBrows[ 1 ]:cargo, aPos[ 1 ] )
      ENDCASE

      IF lResizing
         nLastRefresh := 0
         lReconfigure := .T.
      ENDIF
   ENDDO

RETURN Nil

STATIC FUNCTION BrowseFrames( aBrows, nBrow, nBrowTmp )
   @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft TO;
     aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nRight DOUBLE COLOR aBrows[ nBrow ]:colorSpec
   DO CASE
      CASE nBrowTmp == 1 .AND. nBrow == 1
         @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft + 2 SAY "[ Connections ]" COLOR aBrows[ nBrow ]:colorSpec
      CASE nBrowTmp == 2 .AND. nBrow == 2
         @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft + 2 SAY "[ Databases ]" COLOR aBrows[ nBrow ]:colorSpec
      CASE nBrowTmp == 3 .AND. nBrow == 3
         @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft + 2 SAY "[ Indexes ]" COLOR aBrows[ nBrow ]:colorSpec
      CASE nBrowTmp == 4 .AND. nBrow == 4
         @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft + 2 SAY "[ Locks ]" COLOR aBrows[ nBrow ]:colorSpec
   ENDCASE
   IF nBrow == 4
      @ aBrows[ nBrow ]:nTop, aBrows[ nBrow ]:nLeft - 1 TO;
        aBrows[ nBrow ]:nBottom + 1, aBrows[ nBrow ]:nLeft - 1 DOUBLE COLOR aBrows[ nBrow ]:colorSpec
      @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft - 1 SAY CHR( 201 ) COLOR aBrows[ nBrow ]:colorSpec
   ENDIF
RETURN NIL

STATIC FUNCTION SecToTimestring( nSec )
 LOCAL cTime

   cTime := Padl( Ltrim( Str( Int( ( nSec % 86400 ) / 3600 ) ) ), 3, '0') + ":" +;
            Padl( Ltrim( Str( Int( ( nSec % 3600 ) / 60 ) ) ), 2, '0') + ":" + ;
            Padl( Ltrim( Str( Int( nSec % 60 ) ) ), 2, '0' )
RETURN cTime

STATIC FUNCTION ActionDecode( cAction )
 LOCAL cCode := LEFT( cAction, 1 )
 LOCAL cHaveDone

   DO CASE
      CASE cCode == LETOCMD_admin
         cHaveDone := "admin    "
      CASE cCode == LETOCMD_creat_i
         cHaveDone := "ordcreat "
      CASE cCode == LETOCMD_close
         cHaveDone := "close    "
      CASE cCode == LETOCMD_drop
         cHaveDone := "drop     "
      CASE cCode == LETOCMD_exists
         cHaveDone := "exists   "
      CASE cCode == LETOCMD_file
         cHaveDone := "file     "
      CASE cCode == LETOCMD_open_i
         cHaveDone := "ordadd   "
      CASE cCode == LETOCMD_intro
         cHaveDone := "intro    "
      CASE cCode == LETOCMD_udf_rel
         cHaveDone := "relation "
      CASE cCode == LETOCMD_closall
         cHaveDone := "closeall "
      CASE cCode == LETOCMD_mgmt
         cHaveDone := "manage   "
      CASE cCode == LETOCMD_creat
         cHaveDone := "create   "
      CASE cCode == LETOCMD_open
         cHaveDone := "dbopen   "
      CASE cCode == LETOCMD_ping
         cHaveDone := "ping     "
      CASE cCode == LETOCMD_quit
         cHaveDone := "quit     "
      CASE cCode == LETOCMD_rename
         cHaveDone := "rename   "
      CASE cCode == LETOCMD_sql
         cHaveDone := "SQL      "
      CASE cCode == LETOCMD_ta
         cHaveDone := "transact "
      CASE cCode == LETOCMD_udf_fun
         cHaveDone := "udf-fun  "
      CASE cCode == LETOCMD_var
         cHaveDone := "variable "
      CASE cCode == LETOCMD_set
         cHaveDone := "set      "
      CASE cCode == LETOCMD_stop
         cHaveDone := "stop     "
      CASE cCode == LETOCMD_rddinfo
         cHaveDone := "rddinfo  "
      CASE cCode == LETOCMD_zip
         cHaveDone := "togglzip "

      CASE cCode == LETOCMD_add
         cHaveDone := "append   "
      CASE cCode == LETOCMD_dbi
         cHaveDone := "dbinfo   "
      CASE cCode == LETOCMD_dboi
         cHaveDone := "ordinfo  "
      CASE cCode == LETOCMD_scop
         cHaveDone := "scope    "
      CASE cCode == LETOCMD_seek
         cHaveDone := "seek     "
      CASE cCode == LETOCMD_flush
         cHaveDone := "dbcommit "
      CASE cCode == LETOCMD_goto
         cHaveDone := "goto     "
      CASE cCode == LETOCMD_group
         cHaveDone := "group    "
      CASE cCode == LETOCMD_filt
         cHaveDone := "dbfilter "
      CASE cCode == LETOCMD_sum
         cHaveDone := "sum      "
      CASE cCode == LETOCMD_unlock
         cHaveDone := "unlock   "
      CASE cCode == LETOCMD_lock
         cHaveDone := "lock     "
      CASE cCode == LETOCMD_memo
         cHaveDone := "memo     "
      CASE cCode == LETOCMD_udf_dbf
         cHaveDone := "udf      "
      CASE cCode == LETOCMD_ord
         cHaveDone := "ordfocus "
      CASE cCode == LETOCMD_pack
         cHaveDone := "pack     "
      CASE cCode == LETOCMD_sort
         cHaveDone := "sort     "
      CASE cCode == LETOCMD_islock
         cHaveDone := "islocked "
      CASE cCode == LETOCMD_skip
         cHaveDone := "skip     "
      CASE cCode == LETOCMD_cmta
         cHaveDone := "append+  "
      CASE cCode == LETOCMD_upd
         cHaveDone := "replace  "
      CASE cCode == LETOCMD_rela
         cHaveDone := "relation "
      CASE cCode == LETOCMD_cmtu
         cHaveDone := "replace+ "
      CASE cCode == LETOCMD_rcou
         cHaveDone := "reccount "
      CASE cCode == LETOCMD_zap
         cHaveDone := "zap      "
      CASE cCode == LETOCMD_trans
         cHaveDone := "dbcopy   "
      OTHERWISE
         cHaveDone := "???      "
   ENDCASE
RETURN cHaveDone + cAction

STATIC FUNCTION DriverName( nID )
 LOCAL cDriver

   DO CASE
      CASE nId == 0
         cDriver := " CDX "
      CASE nId == 1
         cDriver := " NTX "
      CASE nId == 2
         cDriver := " NSX "
      CASE nId == 3
         cDriver := " FPT "
      CASE nId == 4
         cDriver := " SIX "
      CASE nId == 10
         cDriver := "BMCDX"
      CASE nId == 11
         cDriver := "BMNTX"
      CASE nId == 12
         cDriver := "BMNSX"
      OTHERWISE
         cDriver := " ??? "
   ENDCASE
RETURN cDriver

STATIC FUNCTION GetAllConnections( oBrow )
   oBrow:cargo := leto_MgGetUsers()
   IF EMPTY( oBrow:cargo )
      QUIT  /* there must be at least myself */
   ELSE
      IF LEN( oBrow:cargo[ 1 ] ) < 9
         QUIT
      ENDIF
      AEVAL( @oBrow:cargo, { | aConn | aConn[ 7 ] := DriverName( VAL( aConn[ 7 ] ) ) } )
      AEVAL( @oBrow:cargo, { | aConn | aConn[ 5 ] := SecToTimestring( Val( aConn[ 5 ] ) ) } )
      AEVAL( @oBrow:cargo, { | aConn | aConn[ 6 ] := ActionDecode( aConn[ 6 ] ) } )
   ENDIF
RETURN .T.

STATIC FUNCTION GetTables( nConnection )
 LOCAL aTables := leto_MgGetTables( nConnection )

   IF ! EMPTY( aTables )
      IF LEN( aTables[ 1 ] ) < 7 .OR. VALTYPE( aTables[ 1, 5 ] ) != "L" 
         QUIT
      ENDIF
      AEVAL( aTables, {| aData | aData[ 5 ] := IIF( aData[ 5 ] , "shar", "excl" ) } )
      AEVAL( aTables, {| aData | aData[ 7 ] := IIF( aData[ 7 ] == "1" , "DBT", IIF( aData[ 7 ] == "2", "FPT", "SMT" ) ) } )
   ELSE
      aTables := {}
   ENDIF
RETURN aTables

STATIC FUNCTION GetIndex( nConnection, cTable )
 LOCAL aIndex := leto_MgGetIndex( nConnection, cTable )

   IF EMPTY( aIndex )
      aIndex := {}
   ELSEIF LEN( aIndex[ 1 ] ) < 4
      QUIT
   ENDIF
RETURN aIndex

STATIC FUNCTION GetLocks( nConnection, cTable )
 LOCAL aRaw := leto_MgGetLocks( nConnection, cTable, "<100" )  /* max 100 locks */
 LOCAL aLocks := {}
 LOCAL cTmp, cLock

   IF ! EMPTY( aRaw )
      cTmp := aRaw[ 1, 2 ]
      TOKENINIT( @cTmp, ",", 1 )
      DO WHILE .NOT. TOKENEND()
         cLock := TOKENNEXT( cTmp )
         IF .NOT. EMPTY( cLock )
            AADD( aLocks, { "", cLock } )
         ENDIF
      ENDDO
   ENDIF
RETURN aLocks

STATIC FUNCTION GetUserList( cTable, oRefBrow )
 LOCAL aUsers := leto_MgGetUsers( cTable )
 LOCAL oBrow, oColumn, nRowPos := 1
 LOCAL aQuitKeys := { K_ESC, HB_K_CLOSE, K_TAB, K_SH_TAB, K_LBUTTONDOWN, K_RBUTTONDOWN, HB_K_RESIZE }
 LOCAL nKey := 0
 LOCAL lResize := .F.
 LOCAL nLastRefresh := 0

   IF ! EMPTY( aUsers )
      oBrow := TBrowseNew( oRefBrow:nTop, 0, oRefBrow:nBottom, oRefBrow:nRight )
      oBrow:cargo := aUsers
      oBrow:colorSpec := oRefBrow:colorSpec
      oBrow:GoTopBlock := { || nRowPos := 1 }
      oBrow:GoBottomBlock := { || nRowPos := IIF( EMPTY( oBrow:cargo ), 1, LEN( oBrow:cargo ) ) }
      oBrow:SkipBlock := { | nSkip | ArrSkip( oBrow:cargo, @nRowPos, nSkip ) }
      oBrow:headSep := CHR_HEADSEP
      oBrow:colSep := CHR_COLSEP
      oColumn := TbColumnNew( "ID", { || oBrow:cargo[ nRowPos, 1  ] } )
      oColumn:width := 4
      oColumn:defcolor := { 1, 2 }
      oBrow:addColumn( oColumn )
      oColumn := TbColumnNew( "IP address", { || oBrow:cargo[ nRowPos, 2  ] } )
      oColumn:width := 15
      oColumn:defcolor := { 1, 2 }
      oBrow:addColumn( oColumn )
      oColumn := TbColumnNew( "system name", { || oBrow:cargo[ nRowPos, 3  ] } )
      oColumn:width := 17
      oColumn:defcolor := { 1, 2 }
      oBrow:addColumn( oColumn )
      oColumn := TbColumnNew( "executable", { || oBrow:cargo[ nRowPos, 4  ] } )
      oColumn:width := 15
      oColumn:defcolor := { 1, 2 }
      oBrow:addColumn( oColumn )
      oColumn := TbColumnNew( " RDD ", { || oBrow:cargo[ nRowPos, 7  ] } )
      oColumn:width := 5
      oColumn:defcolor := { 1, 2 }
      oBrow:addColumn( oColumn )
      oColumn := TbColumnNew( "last active", { || oBrow:cargo[ nRowPos, 5  ] } )
      oColumn:width := 9
      oColumn:defcolor := { 1, 2 }
      oBrow:addColumn( oColumn )
      oColumn := TbColumnNew( "last action", { || oBrow:cargo[ nRowPos, 6  ] } )
      oColumn:width := 64
      oColumn:defcolor := { 1, 2 }
      oBrow:addColumn( oColumn )
      oColumn := TbColumnNew( "username", { || oBrow:cargo[ nRowPos, 8  ] } )
      oColumn:width := 12
      oColumn:defcolor := { 1, 2 }
      oBrow:addColumn( oColumn )
      oBrow:freeze := 1
      oBrow:configure()
      @ oBrow:nTop - 1, oBrow:nLeft TO oBrow:nTop - 1, oBrow:nRight DOUBLE COLOR oBrow:colorSpec
      @ oBrow:nTop - 1, oBrow:nLeft + 2 SAY "[ Users ]" COLOR oBrow:colorSpec
      DO WHILE ASCAN( aQuitKeys, nKey ) == 0
         DO CASE
            CASE nKey == HB_K_RESIZE
               lResize := .T.
               EXIT
            CASE nKey == K_DOWN .OR. nKey == K_MWBACKWARD
               oBrow:Down()
            CASE nKey == K_UP .OR. nKey == K_MWFORWARD
               oBrow:Up()
            CASE nKey == K_RIGHT
               oBrow:Right()
            CASE nKey == K_LEFT
               oBrow:Left()
            CASE nKey == K_PGDN
               oBrow:PageDown()
            CASE nKey == K_PGUP
               oBrow:PageUp()
            CASE nKey == K_HOME
               oBrow:goTop()
            CASE nKey == K_END
               oBrow:goBottom()
         ENDCASE

         IF hb_MilliSeconds() - nLastRefresh > s_nRefresh
            IF ! BasicInfo()  /* server down */
               EXIT
            ENDIF
            oBrow:cargo := leto_MgGetUsers( cTable )
            IF EMPTY( oBrow:cargo )
               EXIT
            ELSEIF oBrow:rowPos > LEN( oBrow:cargo )
               oBrow:rowPos := LEN( oBrow:cargo )
               oBrow:rowPos:configure()
            ENDIF
            oBrow:refreshAll()
            nLastRefresh := hb_MilliSeconds()
         ENDIF

         IF ! oBrow:stable
            DispBegin()
            DO WHILE .NOT. oBrow:Stabilize()
               IF NextKey() <> 0
                  EXIT
               ENDIF
            ENDDO
            DispEnd()
         ENDIF

         nKey := INKEY( s_nRefresh / 1000 )
      ENDDO
   ENDIF
RETURN lResize

STATIC FUNCTION ArrSkip( aArr, nCurrent, nSkip )
 LOCAL nSkipped
   IF nCurrent + nSkip < 1
      nSkipped := -nCurrent + 1
      nCurrent := 1
      RETURN nSkipped
   ELSEIF nCurrent + nSkip > IIF( EMPTY( aArr ), 0, LEN( aArr ) )
      nSkipped := IIF( EMPTY( aArr ), 0, LEN( aArr ) ) - nCurrent
      nCurrent := IIF( EMPTY( aArr ), 1, LEN( aArr ) )
      RETURN nSkipped
   ENDIF
   nCurrent += nSkip
RETURN nSkip

STATIC FUNCTION ArrBlock( oBrowse, nCol, nPos )
RETURN ( {|| IIF( ! EMPTY( oBrowse:cargo ) .AND. LEN( oBrowse:cargo ) >= nPos,;
             oBrowse:cargo[ nPos, nCol ], NIL) } )

STATIC FUNCTION ActiveConnection( aArr, nPos )
 LOCAL nConn := -1

   IF s_myConn < 0
      s_myConn := leto_mgID()
   ENDIF

   IF ! EMPTY( aArr )
      IF ! VAL( aArr[ nPos, 1 ] ) == s_myConn
         nConn := VAL( aArr[ nPos, 1 ] )
      ENDIF
   ENDIF
RETURN nConn

STATIC FUNCTION ActiveDatabase( aArr, nPos )
 LOCAL cBase := NIL
   IF ! EMPTY( aArr )
      cBase := aArr[ nPos, 2 ]
   ENDIF
RETURN cBase

STATIC FUNCTION ServerInfo( cAddress, cVersion, cMode )
 LOCAL cText

   SETCOLOR( "W+/R" )
   @ 0, 0 CLEAR TO 0, MAXCOL()
   cText := cVersion + " mode " + cMode + " at " + cAddress
   @ 0, ( MAXCOL() - LEN( cText ) ) / 2 SAY cText

   SETCOLOR( "W+/G" )
   @ 0, 2 SAY "[ Menu ]"
   @ 0, MAXCOL() - 9 SAY "[ Help ]"
   SETCOLOR( "GR+/G" )
   @ 0, 4 SAY "M"
   @ 0, MAXCOL() - 7 SAY "H"

RETURN .T.

STATIC FUNCTION BasicInfo()
 STATIC aCpuLoads := {}
 LOCAL nSec, nDay, nHour, nMin, oldc, nTransAll, nTransBad
 LOCAL aInfo := leto_MgGetInfo()
 LOCAL aInfo3 := Leto_MgGetTime()
 LOCAL aInfo4 := Leto_MgSysInfo()
 LOCAL cTmp, nTmp

   IF EMPTY( aInfo ) .OR. LEN( aInfo ) < 18 .OR. EMPTY( aInfo4 ) .OR. LEN( aInfo4 ) < 9
      RETURN .F.
   ENDIF

   DispBegin()

   oldc := SETCOLOR( "W+/B,B/BG,,,W+/G" )
   @ 1, 0, 7, MAXCOL() BOX B_DOUBLE + " "
   @ 1, MAXCOL() / 2 SAY "Ë"
   @ 7, MAXCOL() / 2 SAY "Ê"
   @ 2, MAXCOL() / 2 TO 6,MAXCOL() / 2 DOUBLE
   @ 1, 2 SAY "[ Statistics ]"

   @ 2, MAXCOL() / 2 + 25 SAY "Load: " + STR( VAL( aInfo[ 18 ] ), 3, 0 ) + " %"

   IF LEN( aInfo3 ) >= 3
      @ 2,  2 SAY "Server MB disk:" + STR( VAL( aInfo4[ 1 ] ) / 1024 / 1024, 8, 0 )
      @ 2, 26 SAY "RAM: " + STR( VAL( aInfo4[ 4 ] ) / 1024, 7, 0)
   ELSE
      @ 2,  2 SAY "Server memory :" + STR( VAL( aInfo[16] ) / 1024, 7, 0 )
   ENDIF
   @ 3,  2 SAY "Users  current: " + Padl( aInfo[ 1 ],7 )
   @ 3, 26 SAY "Max: " + Padl( aInfo[ 2 ], 7 )
   @ 4,  2 SAY "Tables current: " + Padl( aInfo[ 3 ],7 )
   @ 4, 26 SAY "Max: " + Padl( aInfo[ 4 ], 7 )
   @ 5,  2 SAY "Indexs current: " + Padl( aInfo[ 9 ],7 )
   @ 5, 26 SAY "Max: " + Padl( aInfo[ 10 ], 7 )

   IF LEN( aInfo3 ) >= 3
      nSec := aInfo3[ 3 ]
      nDay := Int( nSec / 86400 )
      nHour := Int( ( nSec % 86400 ) / 3600 )
      nMin := Int( ( nSec % 3600 ) / 60 )
      nSec -= ( nDay * 86400 ) + ( nHour * 3600 ) + ( nMin * 60 )
   ELSE
      nSec := Val( aInfo[ 5 ] )
      nDay := Int( nSec / 86400 )
      nHour := Int( ( nSec % 86400) / 3600 )
      nMin := Int( ( nSec % 3600 ) / 60 )
      nSec -= ( nDay * 86400 ) + ( nHour * 3600 ) + ( nMin * 60 )
   ENDIF

   @ 2, MAXCOL() / 2 + 2 SAY "Up  Time: "
   cTmp := Ltrim( Str( nDay, 4, 0  ) ) + "." +;
           IIF( nHour < 10, "0", "" ) + Ltrim( Str( nHour, 2, 0 ) ) + ":" +;
           IIF( nMin < 10, "0", "" ) + Ltrim( Str( nMin, 2, 0 ) ) + ":" +;
           IIF( nSec < 10, "0", "" ) + Ltrim( Str( nSec, 2, 0 ) )
   @ 2, MAXCOL() / 2 + 24 - LEN( cTmp ) SAY cTmp
   IF LEN( aInfo[ 6 ] ) > 12
      nTmp := VAL( aInfo[ 6 ] ) / 1000000
      @ 3, MAXCOL() / 2 + 2 SAY "Requests: " + STR( nTmp, 12, 0 ) + " x 1M"
   ELSE
      @ 3, MAXCOL() / 2 + 2 SAY "Requests: " + Padl( aInfo[ 6 ], 12 )
   ENDIF

   nTmp := Int( Val( aInfo[ 7 ] ) / 1024 )
   IF nTmp > 1024 * 10
      cTmp := "M"
      nTmp /= 1024
      IF nTmp > 1024 * 10
         cTmp := "G"
         nTmp /= 1024
#if 0
         IF nTmp > 1024 * 10
            cTmp := "T"
            nTmp /= 1024
         ENDIF
#endif
      ENDIF
   ELSE
      cTmp := "K"
   ENDIF
   @ 4, MAXCOL() / 2 + 2 SAY cTmp + "B  sent: " + Padl( Int( nTmp ), 12 )

   nTmp := Int( Val( aInfo[ 7 ] ) / 1024 ) / aInfo3[ 3 ]
   IF nTmp >= 1024
      nTmp /= 1024
      @ 4, MAXCOL() / 2 + 25 SAY "Rate: " + STR( nTmp, 3, 0 )  + " MB/s"
   ELSE
      @ 4, MAXCOL() / 2 + 25 SAY "Rate:" + STR( nTmp, 4, 0 )  + " KB/s"
   ENDIF

   nTmp := Int( Val( aInfo[ 8 ] ) / 1024 )
   IF nTmp > 1024 * 10
      cTmp := "M"
      nTmp /= 1024
      IF nTmp > 1024 * 10
         cTmp := "G"
         nTmp /= 1024
#if 0
         IF nTmp > 1024 * 10
            cTmp := "T"
            nTmp /= 1024
         ENDIF
#endif
      ENDIF
   ELSE
      cTmp := "K"
   ENDIF
   @ 5, MAXCOL() / 2 + 2 SAY cTmp + "B  read: " + Padl( Int( nTmp ), 12 )

   nTmp := Int( Val( aInfo[ 8 ] ) / 1024 ) / aInfo3[ 3 ]
   IF nTmp >= 1024
      nTmp /= 1024
      @ 5, MAXCOL() / 2 + 25 SAY "Rate: " +  STR( nTmp, 3, 0 ) + " MB/s"
   ELSE
      @ 5, MAXCOL() / 2 + 25 SAY "Rate:" +  STR( nTmp, 4, 0 ) + " KB/s"
   ENDIF

   IF ! Empty( aInfo[ 14 ] )
      nTransAll := Val( aInfo[ 14 ] )
      nTransBad := nTransAll - Val( aInfo[ 15 ] )
      @ 6,  2 SAY "Transact   All: " + Str( nTransAll, 7 )
      @ 6, 26 SAY "Bad: " + Str( nTransBad, 7 )
   ENDIF

   nSec := VAL( aInfo[ 12 ] )
   nDay := Int( nSec / 86400 )
   nHour := Int( ( nSec % 86400) / 3600 )
   nMin := Int( ( nSec % 3600 ) / 60 )
   nSec -= ( nDay * 86400 ) + ( nHour * 3600 ) + ( nMin * 60 )
   cTmp := Ltrim( Str( nDay, 4, 0 ) ) + "." +;
           IIF( nHour < 10, "0", "" ) + Ltrim( Str( nHour, 2, 0 ) ) + ":" +;
           IIF( nMin < 10, "0", "" ) + Ltrim( Str( nMin, 2, 0 ) ) + ":" + ;
           IIF( nSec < 10, "0", "" ) + Ltrim( Str( nSec, 2, 0 ) )
   @ 6, MAXCOL() / 2 + 2 SAY "CPU time: "
   @ 6, MAXCOL() / 2 + 24 - LEN( cTmp ) SAY cTmp
   @ 6, MAXCOL() / 2 + 25 SAY "Core: " + Padl( aInfo[ 13 ], 3 )
   SETCOLOR( oldc )

   DispEnd()

RETURN .T.

STATIC FUNCTION KillActiveUsers( nConnection )
 LOCAL cSave := SAVESCREEN( 2, 1, 6, MAXCOL() - 1 )
 LOCAL oldcolor := SETCOLOR( "W+/B,W+/G,,,W/N" )
 LOCAL oldcurs := SETCURSOR( SC_NORMAL )
 LOCAL getlist := {}
 LOCAL cConfirm := "NON"
 LOCAL aInfo, i
 LOCAL aUser, lOK := .F.
 LOCAL lLockConn := .F.
 LOCAL nKillID, nKilled
 LOCAL lOpen

   @ 2, 1, 6, MAXCOL() - 1 BOX SPACE( 9 )

   IF ! EMPTY( leto_MgGetInfo() )
      lOk := .T.
   ENDIF

   IF lOk
     @ 2, 1 SAY "? close *ALL* active user [ *USE* with open tables ] or only *ONE* connection ?"
     @ 3, 2 SAY "type: ALL, USE or ONE" GET cConfirm PICT "@! XXX" VALID ALLTRIM( cConfirm ) $ "ALL-ONE-USE-NON"
     READ
     lOk := LASTKEY() != K_ESC
     IF cConfirm == "NON"
        lOk := .F.
     ENDIF
     lOpen := cConfirm == "USE"
   ENDIF

   IF lOk
      lLockConn := LETO_LOCKCONN( .T. )
      IF lLockConn
         @ 3, 2 SAY "Server locked ..." + SPACE( 10 )
      ELSE
         lOk := .F.
         @ 3, 2 SAY "Server lock problems .. "
      ENDIF
   ENDIF

   IF lOk .AND. ( cConfirm == "ALL" .OR. cConfirm == "USE" )
      nKilled := 0
      DO WHILE lOk
         IF lOpen .AND. ( VALTYPE( aInfo := leto_MgGetTables() ) != "A" .OR. EMPTY( aInfo ) )
            EXIT
         ENDIF
         IF lOpen
            @ 4, 2 SAY " Open Tables : " + hb_ntos( LEN( aInfo ) )
            IF VALTYPE( aUser := leto_MgGetUsers( VAL( aInfo[1, 1] ) ) ) != "A" .OR. EMPTY( aUser )
               EXIT  // should be impossible
            ENDIF
         ELSE
            IF VALTYPE( aUser := leto_MgGetUsers() ) != "A" .OR. LEN( aUser ) < 2
               EXIT  // only console
            ENDIF
         ENDIF
         FOR i := 1 TO LEN( aUser )
            nKillId := VAL( aUser[ i, 1 ] )
            IF ! lOpen .AND. nKillId == s_myConn  /* can not kill myself ;-) */
               LOOP
            ENDIF
            IF leto_MgKill( nKillID ) == nKillID
               @ 5, 2 SAY " closed connection: " + aUser[ i, 1 ] + " " +;
                          ALLTRIM( aUser[ i, 4 ] ) + "@" + ALLTRIM( aUser[ i, 3 ] ) + SPACE( 10 )
               nKilled++
            ELSE
               @ 5, 2 SAY " closing connection: " + aUser[ i, 1 ] + " failed"
            ENDIF
         NEXT i
         INKEY( 2 )  /* wait for the connection(s) to close */
      ENDDO
   ELSEIF lOk .AND. cConfirm == "ONE"
      IF nConnection == -1
         timedAlert( " can not close me myself this way", 3, "W+/R" )
         lOk := .F.
      ELSEIF leto_MgKill( nConnection ) == nConnection
         timedAlert( " successful closed connection : " + hb_nTos( nConnection ), 2, "W+/G" )
         nKilled := 1
         lOk := .T.
      ELSE
         timedAlert( " failure closing connection: " + hb_nTos( nConnection ), 3, "W+/R" )
         lOk := .F.
      ENDIF
   ENDIF

   IF lOk .AND. nKilled > 0
      @ 6, 2 SAY "Bill killed ..."
   ELSE
      @ 6, 2 SAY "aborted ..."
   ENDIF
   INKEY( 1 )
   IF lLockConn
      IF LETO_LOCKCONN( .F. )
         @ 3, 2 SAY "Server unlocked !  "
      ENDIF
   ENDIF

   SETCOLOR( oldcolor )
   SETCURSOR( oldcurs )
   RESTSCREEN( 2, 1, 6, MAXCOL() - 1, cSave )
RETURN .T.

FUNCTION memologger( nMode, nLine, nCol )
 STATIC nUpTime
 LOCAL nRet := ME_DEFAULT
 LOCAL nKey

   HB_SYMBOL_UNUSED( nLine )
   HB_SYMBOL_UNUSED( nCol )

   DO CASE
      CASE nMode == ME_IDLE
         nKey := INKEY( ( s_nRefresh / 1000 ) )
         IF nKey == 0 .AND. hb_MilliSeconds() - nUpTime > s_nRefresh
            hb_keyPut( K_CTRL_W )
         ELSE
            hb_keyIns( nKey )
         ENDIF
      CASE nMode == ME_INIT
         nUpTime := hb_MilliSeconds()
         /* hb_keyPut( K_CTRL_PGDN ) */
      CASE nMode == ME_UNKEY
         nKey := LASTKEY()
         IF nKey == K_MWBACKWARD
            hb_keyIns( K_DOWN )
         ELSEIF nKey == K_MWFORWARD
            hb_keyIns( K_UP )
         ELSEIF nKey == K_RBUTTONUP
            hb_keyIns( K_ESC )
         ELSEIF nKey == HB_K_RESIZE
            hb_keyPut( K_ESC )
            hb_keyPut( HB_K_RESIZE )
         ENDIF
   ENDCASE
RETURN nRet

STATIC FUNCTION ViewLogs( nConnection )
 LOCAL oldcolor := SetColor( "W+/G,W+/B" )
 LOCAL cText := leto_MgLog( nConnection, 0 )
 LOCAL cSave
 LOCAL nLines
 LOCAL lResize := .F.

   IF EMPTY( cText )
      timedAlert( " no LOG file for connection: " + hb_nTos( nConnection ), 1, "W+/R" )
   ELSE
      cSave := SAVESCREEN( 8, 0, MAXROW(), MAXCOL() )
      nLines := LEN( cText )
      IF nLines > 65535 * 5
         timedAlert( " LARGE log " + hb_nTos( INT( nLines / 1024 ) ) + " KB, expect delays ", 3, "W+/R" )
         s_nRefresh := MAX( 10000, s_nRefresh )
      ENDIF
      DO WHILE .T.
         nLines := MLCOUNT( cText, 1024 )
         memoedit( cText, 8, 0, MAXROW(), MAXCOL(), .F.,"memologger", 1024,, MAX( 0, nLines - ( MAXROW() - 8 ) ) )
         IF LASTKEY() != K_ESC
            IF ! BasicInfo()  /* server down */
               EXIT
            ENDIF
            cText := leto_MgLog( nConnection, 0 )
         ELSE
            EXIT
         ENDIF
      ENDDO
      RESTSCREEN(  8, 0, MAXROW(), MAXCOL(), cSave )
      IF INKEY() == HB_K_RESIZE
         lResize := .T.
      ENDIF
   ENDIF
   SetColor( oldcolor )
RETURN lResize

STATIC FUNCTION Administrate( nConnection )
 LOCAL nMenu := 0
 LOCAL oldcolor := SetColor( "W+/G,W+/B" )
 LOCAL bLocked := leto_LockConn()  /* only query for active state */
 LOCAL aMenu := {;
      "1 Add    User",;
      "2 Delete User",;
      "3 Change Passwrd",;
      "4 Change Access",;
      "5 Flush  Changes",;
      "6 Close  Connect",;
      "7 Change Refresh",;
      IIF( bLocked, "8 UNlock Server", "8 Lock   Server" ),;
      "9 View   Logfile",;
      "D Change Debug",;
      "0 QUIT ! Console" }
 LOCAL cSave := SAVESCREEN( 1, 2, 2 + LEN( aMenu ), 19 )
 LOCAL arr
 LOCAL lResize := .F.

   DO WHILE nMenu < 6
      @ 1, 2, 2 + LEN( aMenu ), 19 BOX B_SINGLE + " "
      nMenu := Achoice( 2, 3, 2 + LEN( aMenu ), 18, aMenu, "MyChoice" )
      IF INKEY() == HB_K_RESIZE
         lResize := .T.
      ENDIF
      DO WHILE INKEY() != 0
      ENDDO
      RESTSCREEN( 1, 2, 2 + LEN( aMenu ), 19, cSave )
      IF lResize
         EXIT
      ENDIF

      DO CASE
         CASE nMenu == 1
            IF ( arr := GetUser( .T., .T. ) ) != Nil
               IF leto_useradd( arr[1], arr[2], arr[3] )
                  leto_userflush()
                  TimedAlert( "User is added", 2, "GR+/N" )
               ELSE
                  TimedAlert( "User is NOT added", 2, "R+/N" )
               ENDIF
            ELSE
               TimedAlert( "Operation canceled", 2, "R+/N" )
            ENDIF

         CASE nMenu == 2
            IF ( arr := GetUser( .F., .F. ) ) != Nil
               IF leto_userdelete( arr[1], arr[2], arr[3] )
                  leto_userflush()
                  TimedAlert( "User is deleted", 2, "GR+/N" )
               ELSE
                  TimedAlert( "User is NOT deleted", 2, "R+/N" )
               ENDIF
            ELSE
               TimedAlert( "Operation canceled", 2, "R+/N" )
            ENDIF

         CASE nMenu == 3
            IF ( arr := GetUser( .T.,.F. ) ) != Nil
               IF leto_userpasswd( arr[1], arr[2] )
                  leto_userflush()
                  IF EMPTY( arr[2] )
                     TimedAlert( "Password is cleared", 2, "GR+/N" )
                  ELSE
                     TimedAlert( "Password is changed", 2, "GR+/N" )
                  ENDIF
               ELSE
                  TimedAlert( "Password is NOT changed", 2, "R+/N" )
               ENDIF
            ELSE
               TimedAlert( "Operation canceled", 2, "R+/N" )
            ENDIF

         CASE nMenu == 4
            IF ( arr := GetUser( .F., .T. ) ) != Nil
               IF leto_userrights( arr[1], arr[3] )
                  leto_userflush()
                  TimedAlert( "Rights are changed", 2, "GR+/N" )
               ELSE
                  TimedAlert( "Rights are NOT changed", 2, "R+/N" )
               ENDIF
            ELSE
               TimedAlert( "Operation canceled", 2, "R+/N" )
            ENDIF

         CASE nMenu == 5
            leto_userflush()
            TimedAlert( "OK", 2, "GR+/N" )

         CASE nMenu == 6
            KillActiveUsers( nConnection )
         CASE nMenu == 7
            changeRefresh()
         CASE nMenu == 8
            TimedAlert( "LetoDBf server try to : " + IIF( bLocked, "unlock", "lock" ), 2, "W+/B" )
            IF bLocked
               bLocked := leto_LockConn( .F. )
            ELSE
               bLocked := leto_LockConn( .T. )
            ENDIF
            TimedAlert( "LetoDBf server lock/ unlock: " + IIF( ! bLocked, "failed", "successful" ), 2,;
                         IIF( bLocked,"W+/G", "W+/R" ) )
         CASE nMenu == 9
            ViewLogs( nConnection )
         CASE nMenu == 10
            ChangeDebug()
         CASE nMenu == 0
            EXIT
         CASE nMenu == LEN( aMenu )
            hb_keyPut( K_ESC )
            hb_keyPut( K_ESC )
            EXIT
      ENDCASE
   ENDDO
   SetColor( oldcolor )
RETURN .T.


FUNCTION MyChoice( nStatus )  /* must be a public FUNCTION */
 LOCAL nKey, cKey
 LOCAL mRow, mCol

   nKey := LastKey()
   IF nKey == K_LBUTTONUP
      mRow := MROW()
      mCol := MCOL()
      IF mRow < 1 .OR. mRow > 12 .OR. mCol < 2 .AND. mCol > 19
         hb_keyPut( K_ESC )
      ENDIF
   ENDIF
   DO CASE
      CASE nStatus == AC_EXCEPT
         cKey := Upper( CHR( nKey ) )  /* hb_keyChar( nKey ) */
         DO CASE
            CASE ( cKey >= "0" .AND. cKey <= "9" ) .OR. cKey == "D"
               hb_keyPut( K_ENTER )
               RETURN AC_GOTO
            CASE nKey == K_HOME
               hb_keyPut( K_PGUP )
            CASE nKey == K_END
               hb_keyPut( K_PGDN )
            CASE nKey == K_ENTER
               RETURN AC_SELECT
            CASE nKey == K_ESC .OR. nKey == K_RBUTTONUP
               RETURN AC_ABORT
            CASE nKey == HB_K_RESIZE
               hb_keyPut( HB_K_RESIZE )
               RETURN AC_ABORT
         ENDCASE
      CASE nStatus == AC_NOITEM
         RETURN AC_ABORT
   ENDCASE

RETURN AC_CONT

STATIC FUNCTION ChangeRefresh
 LOCAL cSave := SAVESCREEN( 2, 1, 6, MAXCOL() - 1 )
 LOCAL oldcolor := SETCOLOR( "W+/B,W+/G,,,W/N" )
 LOCAL oldcurs := SETCURSOR( SC_NORMAL )
 LOCAL getlist := {}

   @ 2, 1, 6, MAXCOL() - 1 BOX SPACE( 9 )
   @ 3, 2 SAY "Refresh rate ( 1/ 1000 ) second :" GET s_nRefresh PICTURE "@K 999999" ;
                                                  VALID s_nRefresh >= 100 .AND. s_nRefresh <= 360000
   READ
   SETCOLOR( oldcolor )
   SETCURSOR( oldcurs )
   RESTSCREEN( 2, 1, 6, MAXCOL() - 1, cSave )
RETURN .T.

STATIC FUNCTION ChangeDebug
 LOCAL cSave := SAVESCREEN( 2, 1, 6, MAXCOL() - 1 )
 LOCAL oldcolor := SETCOLOR( "W+/B,W+/G,,,W/N" )
 LOCAL oldcurs := SETCURSOR( SC_NORMAL )
 LOCAL getlist := {}
 LOCAL nLevel := RDDInfo( RDDI_DEBUGLEVEL )

   @ 2, 1, 6, MAXCOL() - 1 BOX SPACE( 9 )
   @ 2, 2 SAY "DebugLevel: 0: non debug feedback, only error"
   @ 3, 2 GET nLevel PICTURE "@K 99" VALID nLevel >= 0 .AND. nLevel <= 99
   @ 3, 2 SAY "            1: the very minimal info messages"
   @ 4, 2 SAY "           10: a lot feedback"
   @ 5, 2 SAY "           15: plus partly traffic"
   @ 6, 2 SAY "           21: full traffic log -- ! log files will very fast grow huge !"

   READ
   IF LASTKEY() != K_ESC
      RDDInfo( RDDI_DEBUGLEVEL, nLevel )
   ENDIF
   SETCOLOR( oldcolor )
   SETCURSOR( oldcurs )
   RESTSCREEN( 2, 1, 6, MAXCOL() - 1, cSave )
RETURN .T.

STATIC FUNCTION HelpText
 LOCAL oldcolor := SETCOLOR( "W+/B,W+/G,,,W/N" )
 LOCAL cSave := SAVESCREEN( 8, 0, MAXROW(), MAXCOL() )
 LOCAL nKey
 LOCAL lResize := .F.

   @ 8, 0 CLEAR TO MAXROW(), MAXCOL()
   DO WHILE .T.
      @  8, 0 SAY CENTER( "LetoDBf console navigation", MAXCOL( .T. ) + 1 ," ", .T. )
      @  9, 1 SAY "in browses: K_TAB/ K_SHIFT_TAB/ Left-click into area: toggle between browses"
      @ 10, 1 SAY "            K_DOWN/ K_UP/ mouse wheel ( mouse in browse area ): row up/ down"
      @ 11, 1 SAY "            left-click onto a row in browse: change to that row"
      @ 12, 1 SAY "            K_LEFT/ K_RIGHT/ left-click/ right click active row: change column"
      @ 13, 1 SAY "            K_ALT_M = menu; K_ALT_H = help, K_ALT_L = log; K_ALT_K = close C."
      @ 15, 1 SAY "in Menu   : K_DOWN/ K_UP/ left-click: change to that menu option"
      @ 16, 1 SAY "            K_ESC/ right-click: end menu"
      @ 17, 1 SAY "            K_ENTER/ left-double-click: choose menu option"
      @ 19, 1 SAY "in Logview: K_UP/ K_DOWN/ K_PG_UP/ K_PGDN/ K_HOME/ K_END/ mouse wheel: textpos"
      @ 20, 1 SAY "            K_ESC/ right-click: end log view"
      @ 21, 1 SAY "            not typing any key for refresh rate: reposition cursor last page  "
      @ 23, 1 SAY "this help : end in 30 seconds without typing any key, with any key immideately"
      @ 24, 1 SAY "            K_SPACE/ left-click will leave it 30 sec longer viewable"
      nKey := INKEY( 30000 )
      IF nKey == HB_K_RESIZE
         lResize := .T.
         EXIT
      ELSEIF nKey == K_SPACE .OR. nKey == K_LBUTTONUP .OR. nKey == K_LBUTTONDOWN .OR. ;
             nKey == HB_K_LOSTFOCUS .OR. nKey == HB_K_GOTFOCUS
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
   RESTSCREEN(  8, 0, MAXROW(), MAXCOL(), cSave )
   SetColor( oldcolor )
RETURN lResize

STATIC FUNCTION GetUser( lPass, lRights )
 LOCAL cUser := SPACE( 16 )
 LOCAL cPass, cCheck, cRights
 LOCAL cSave := SAVESCREEN( 2, 1, 6, MAXCOL() - 1 )
 LOCAL oldcolor := SETCOLOR( "W+/B,W+/G,,,W/N" )
 LOCAL oldcurs := SETCURSOR( SC_NORMAL )
 LOCAL getlist := {}
 LOCAL lOk := .T.
 LOCAL i

   @ 2, 1, 6, MAXCOL() - 1 BOX SPACE( 9 )
   @ 2, 2 SAY "User name :" GET cUser
   READ
   cUser := ALLTRIM( cUser )
   IF LASTKEY() == K_ESC  .OR. EMPTY( cUser )
      lOk := .F.
   ELSEIF Len( cUser ) > 16
      TimedAlert( "Username too long", 2, "R+/N" )
      lOk := .F.
   ENDIF

   IF lOk .AND. lPass
      cPass := RTRIM( GETSECRET( SPACE( 32 ), 3, 2, .F., "Password  :" ) )
      IF LASTKEY() == K_ESC
         lOk := .F.
      ELSEIF Len( cPass ) > 32
         TimedAlert( "Password too long", 2, "R+/N" )
         lOk := .F.
      ELSE
         cCheck := RTRIM( GETSECRET( SPACE( 32 ), 4, 2, .F., "Re-type   :" ) )
         IF LASTKEY() == K_ESC
            lOk := .F.
         ELSEIF cPass != cCheck
            TimedAlert( "Password check failed", 2, "R+/N" )
            lOk := .F.
         ENDIF
      ENDIF
   ENDIF

   IF lOk .AND. lRights
      cRights := "NNN"
      @ 5,2 SAY "Access rights - Admin,Manage,Write (default - NNN) :" GET cRights PICTURE "@!"
      READ
      IF LASTKEY() == K_ESC
         lOk := .F.
      ELSEIF Len( cRights ) > 3
         TimedAlert( "Rights text too long", "R+/N" )
         lOk := .F.
      ELSE
         cRights := UPPER( cRights )
         FOR i := 1 TO Len(cRights)
            IF !( Substr(cRights,i,1) $ "NY" )
               TimedAlert( "Only 'N' and 'Y' chars are permitted", 2, "R+/N" )
               lOk := .F.
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF

   SETCOLOR( oldcolor )
   SETCURSOR( oldcurs )
   RESTSCREEN( 2, 1, 6, MAXCOL() - 1, cSave )
RETURN IIF( lOk, { cUser, cPass, cRights }, NIL )

STATIC FUNCTION TimedAlert( cText, nSec, cColor )
 LOCAL nX1 := ( ( MAXCOL() - LEN( cText ) ) / 2 ) - 2
 LOCAL nX2 := ( ( MAXCOL() - LEN( cText ) ) / 2 ) + LEN( cText ) + 2
 LOCAL nY1 := ( MAXROW() / 2 ) - 2
 LOCAL nY2 := ( MAXROW() / 2 ) + 2
 LOCAL cSave := SAVESCREEN( nY1, nX1, nY2, nX2 )
 LOCAL oldcolor := SETCOLOR( IIF( ! EMPTY( cColor ), cColor, "W+/R" ) )

   @ nY1, nX1, nY2, nX2 BOX B_DOUBLE + " "
   @ nY1 + 2, nX1 + 2 SAY cText
   DO WHILE Inkey( nSec ) >= K_MINMOUSE
   ENDDO
   RESTSCREEN( nY1, nX1, nY2, nX2, cSave )
   SETCOLOR( oldcolor )
RETURN .T.



