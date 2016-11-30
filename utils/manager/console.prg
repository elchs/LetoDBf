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

// ToDo -- menu change server debug level

#include "hbclass.ch"
#include "rddleto.ch"

#include "inkey.ch"
#include "box.ch"
#include "dbinfo.ch"
#include "achoice.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"
#include "memoedit.ch"

REQUEST LETO
REQUEST RDDInfo


STATIC s_nUpTime
STATIC s_nRefresh := 5000  /* 5 seconds default */
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
 LOCAL nLastRefresh := 0, nLastLen
 LOCAL nBrowTmp
 LOCAL lHaveFocus := .T.
 LOCAL lReconfigure := .T.
 LOCAL nExtra, nAllExtra
 LOCAL mrow, mcol
 LOCAL cMode
 LOCAL nTmp

   s_nUpTime := hb_MilliSeconds()
   IF VALTYPE( cAddress ) != "C"
      cAddress := ""
   ENDIF

   ALTD()
   CLS

   IF EMPTY( cAddress )
      IF File( cDirBase + cIniName )
         aIni := rdIni( cDirBase + cIniName )
#ifdef __LINUX__
      ELSEIF File( "/etc/" + cIniName )
         aIni := rdIni( "/etc/" + cIniName )
#endif
      ENDIF
   ENDIF

   if ! Empty(aIni)
      for each ai in aIni[1, 2]
         if ai[1] = "SERVER"
            cAddress := ai[2]
         elseif ai[1] = "PORT"
            cPort := ai[2]
         elseif ai[1] = "USER"
            cUser := ai[2]
         elseif ai[1] = "PASSWORD"
            cPasswd := ai[2]
         endif
      next
   endif

   IF LEFT( cAddress,2 ) != "//"
      cAddress := "//" + cAddress + IF( ! EMPTY(cPort) , ":" + cPort, "") + "/"
   endif

   RddSetDefault( "LETO" )
   SET SCOREBOARD OFF
   SET CONFIRM ON
   SET CURSOR OFF
   SET(_SET_EVENTMASK, INKEY_ALL - INKEY_MOVE + HB_INKEY_GTEVENT )   // 234 ohne LbUP  RbUp
   hb_GtInfo( HB_GTI_WINTITLE, "LetoDBf monitor" )
   hb_GtInfo( HB_GTI_RESIZABLE, .T.)
   hb_GtInfo( HB_GTI_RESIZEMODE,  HB_GTI_RESIZEMODE_ROWS )
   hb_GtInfo( HB_GTI_CLOSABLE, .T. )
   hb_GtInfo( HB_GTI_ALTENTER, .T.)
   IF HB_GtVersion() == "WVT"
      hb_GtInfo( HB_GTI_FONTATTRIBUTE, HB_GTI_FONTA_FIXMETRIC + HB_GTI_FONTA_CLRBKG + HB_GTI_FONTA_DRAWBOX )
   ENDIF
   IF MPresent()
      MDblClk( 421 )
      MShow()
   ENDIF

   ? "Connecting to " + cAddress
   /* LETO_CONNECT( cAddress, [ cUserName ], [ cPassword ], [ nTimeOut ], [ nBufRefreshTime ], [ lZombieCheck ] ) */
   IF leto_Connect( cAddress, cUser, cPasswd, 5000, , .T. ) == -1
      TimedALERT( leto_Connect_Err( .T. ), 3 )
      RETURN Nil
   ELSE
      /* activate compression */
      leto_togglezip( 1, "encrypted" )
   ENDIF
   cVersion := leto_GetServerVersion()
   cMode := Leto_MgSysInfo()[ 5 ]

   // std, hilight, rand,back,unselect
   /* connections */
   AADD( aPos, 1 )
   AADD( aBrows, TBrowseNew( 9, 0, 12, MAXCOL() ) )
   ATAIL( aBrows ):colorSpec := "BG/N,W+/B"
   ATAIL( aBrows ):cargo := {}
   ATAIL( aBrows ):GoTopBlock := { || aPos[ 1 ] := 1 }
   ATAIL( aBrows ):GoBottomBlock := { || aPos[ 1 ] := IIF( EMPTY( aBrows[ 1 ]:cargo ), 1, LEN( aBrows[ 1 ]:cargo ) ) }
   ATAIL( aBrows ):SkipBlock := { |nSkip| ArrSkip( aBrows[ 1 ]:cargo, @aPos[ 1 ], nSkip) }
   ATAIL( aBrows ):headSep   := "ÍËÍ"
   //ATAIL( aBrows ):footSep   := "ÍÊÍ"
   ATAIL( aBrows ):colSep    := " º "
   oColumn := TbColumnNew( "No", ArrBlock( ATAIL( aBrows ), 1, @aPos[ 1 ] ) )
   oColumn:width := 4
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "IP address", ArrBlock( ATAIL( aBrows ), 2, @aPos[ 1 ] ) )
   oColumn:width := 15
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "system name", ArrBlock( ATAIL( aBrows ), 3, @aPos[ 1 ] ) )
   oColumn:width := 18
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "executable", ArrBlock( aBrows[ 1 ], 4, @aPos[ 1 ] ) )
   oColumn:width := 15
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "cndx", ArrBlock( ATAIL( aBrows ), 7, @aPos[ 1 ] ) )
   oColumn:width := 4
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "last active", ArrBlock( ATAIL( aBrows ), 8, @aPos[ 1 ] ) )
   oColumn:width := 9
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "last action", ArrBlock( ATAIL( aBrows ), 6, @aPos[ 1 ] ) )
   oColumn:width := 64
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   ATAIL( aBrows ):freeze( 1 )
   AADD( aBlocks, { | oBrow | GetAllConnections( @oBrow ) } )

   /* databases */
   AADD( aPos, 1 )
   AADD( aBrows, TBrowseNew( 14, 0, 18, MAXCOL() - 20 ) )
   ATAIL( aBrows ):colorSpec := "G/N,W+/B"
   ATAIL( aBrows ):cargo := {}
   ATAIL( aBrows ):GoTopBlock := { || aPos[ 2 ] := 1 }
   ATAIL( aBrows ):GoBottomBlock := { || aPos[ 2 ] := IIF( EMPTY( aBrows[ 2 ]:cargo ), 1, LEN( aBrows[ 2 ]:cargo ) ) }
   ATAIL( aBrows ):SkipBlock := { |nSkip| ArrSkip( aBrows[ 2 ]:cargo, @aPos[ 2 ], nSkip) }
   ATAIL( aBrows ):headSep   := "ÍËÍ"
   ATAIL( aBrows ):colSep    := " º "
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
   oColumn:width := MAXCOL() - 33 - 21
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   AADD( aBlocks, {|oBrow| IIF( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ) >= -1,;
                           ( oBrow:cargo := leto_MgGetTables( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ) ) ), ( oBrow:cargo := {} ) ),;
                    AEVAL( @oBrow:cargo, {|aData| aData[ 5 ] := IIF( aData[ 5 ] , "shar", "excl" ) } ) } )

   /* indexes */
   AADD( aPos, 1 )
   AADD( aBrows, TBrowseNew( 20, 0, MAXROW(), MAXCOL() - 20 ) )
   ATAIL( aBrows ):colorSpec := "RB/N,W+/B"
   ATAIL( aBrows ):cargo := {}
   ATAIL( aBrows ):GoTopBlock := { || aPos[ 3 ] := 1 }
   ATAIL( aBrows ):GoBottomBlock := { || aPos[ 3 ] := IIF( EMPTY( aBrows[ 3 ]:cargo ), 1, LEN( aBrows[ 3 ]:cargo ) ) }
   ATAIL( aBrows ):SkipBlock := { |nSkip| ArrSkip( aBrows[ 3 ]:cargo, @aPos[ 3 ], nSkip) }
   ATAIL( aBrows ):headSep   := "ÍËÍ"
   ATAIL( aBrows ):colSep    := " º "
   oColumn := TbColumnNew( "Tagname", ArrBlock( ATAIL( aBrows ), 3, @aPos[ 3 ] ) )
   oColumn:width := 11
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "Key", ArrBlock( ATAIL( aBrows ), 4, @aPos[ 3 ] ) )
   oColumn:width := MAXCOL() - 14 - 21
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "Filename", ArrBlock( ATAIL( aBrows ), 2, @aPos[ 3 ] ) )
   oColumn:width := MAXCOL() - 23
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   ATAIL( aBrows ):freeze( 1 )
   AADD( aBlocks, {|oBrow| IIF( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ) >= -1,;
                           ( oBrow:cargo := leto_MgGetIndex( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ), ActiveDatabase( aBrows[ 2 ]:cargo, aPos[ 2 ] ) ) ), ( oBrow:cargo := {} ) ) } )

   /* locks list */
   AADD( aPos, 1 )
   AADD( aBrows, TBrowseNew( 14, MAXCOL() - 18, MAXROW() - 1, MAXCOL() ) )
   ATAIL( aBrows ):colorSpec := "R/N,W+/B"
   ATAIL( aBrows ):cargo := {}
   ATAIL( aBrows ):GoTopBlock := { || aPos[ 4 ] := 1 }
   ATAIL( aBrows ):GoBottomBlock := { || aPos[ 4 ] := IIF( EMPTY( aBrows[ 4 ]:cargo ), 1, LEN( aBrows[ 4 ]:cargo ) ) }
   ATAIL( aBrows ):SkipBlock := { |nSkip| ArrSkip( aBrows[ 4 ]:cargo, @aPos[ 4 ], nSkip) }
   ATAIL( aBrows ):headSep   := "ÍËÍ"
   ATAIL( aBrows ):colSep    := " º "
   oColumn := TbColumnNew( "Record Lock", ArrBlock( ATAIL( aBrows ), 2, @aPos[ 4 ] ) )
   oColumn:width := 11
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   oColumn := TbColumnNew( "Filename", ArrBlock( ATAIL( aBrows ), 1, @aPos[ 4 ] ) )
   oColumn:width := 21
   oColumn:defcolor := { 1, 2 }
   ATAIL( aBrows ):addColumn( oColumn )
   AADD( aBlocks, {|oBrow| IIF( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ) >= -1,;
                           ( oBrow:cargo := leto_MgGetLocks( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ), ActiveDatabase( aBrows[ 2 ]:cargo, aPos[ 2 ] ) ) ), ( oBrow:cargo := {} ) ) } )

   aLastPos := Aclone( aPos )
   EVAL( aBlocks[ 1 ], aBrows[ 1 ] )

   DO WHILE nKey <> K_ESC .AND. nKey <> HB_K_CLOSE

      IF lReconfigure .AND. lHaveFocus
         IF MAXROW() < 21 .OR. MAXCOL() < 79
            SETMODE( 22, 80 )
         ENDIF
         ServerInfo( cAddress, cVersion, cMode )

         nExtra := MAXROW() - 21
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

         @ 8, 0, MAXROW(), MAXCOL() BOX SPACE( 9 ) COLOR aBrows[ 1 ]:colorSpec
         aBrows[ 1 ]:nRight := MAXCOL()
         aBrows[ 1 ]:nBottom := 11 + nAllExtra + INT( nExtra / 2 )
         aBrows[ 1 ]:configure()
         aBrows[ 1 ]:freeze( 1 )

         aBrows[ 2 ]:nTop := 13 + nAllExtra + INT( nExtra / 2 )
         aBrows[ 2 ]:nRight := MAXCOL() - 20
         aBrows[ 2 ]:nBottom := MAXROW() - 5 - nAllExtra
         oColumn := aBrows[ 2 ]:getColumn( 4 )
         oColumn:width := MAXCOL() - 33 - 21
         aBrows[ 3 ]:setColumn( 4, oColumn )
         aBrows[ 2 ]:configure()

         aBrows[ 3 ]:nTop := MAXROW() - 3 - nAllExtra
         aBrows[ 3 ]:nRight := MAXCOL() - 20
         aBrows[ 3 ]:nBottom := MAXROW()
         oColumn := aBrows[ 3 ]:getColumn( 2 )
         oColumn:width := MAXCOL() - 14 - 21
         aBrows[ 3 ]:setColumn( 2, oColumn )
         oColumn := aBrows[ 3 ]:getColumn( 3 )
         oColumn:width := MAXCOL() - 23
         aBrows[ 3 ]:setColumn( 3, oColumn )
         aBrows[ 3 ]:configure()
         aBrows[ 3 ]:freeze( 1 )

         aBrows[ 4 ]:nTop := 13 + nAllExtra + ( nExtra / 2 )
         aBrows[ 4 ]:nLeft := MAXCOL() - 18
         aBrows[ 4 ]:nBottom := MAXROW()
         aBrows[ 4 ]:nRight := MAXCOL()
         aBrows[ 4 ]:configure()
         nLastRefresh := 1
         lReconfigure := .F.
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
            @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft TO;
              aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nRight DOUBLE COLOR aBrows[ nBrow ]:colorSpec
            DO CASE
               CASE nBrowTmp == 1 .AND. nBrow == 1
                  @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft + 2 SAY "[ Connentions ]" COLOR aBrows[ nBrow ]:colorSpec
               CASE nBrowTmp == 2 .AND. nBrow == 2
                  @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft + 2 SAY "[ Databases ]" COLOR aBrows[ nBrow ]:colorSpec
               CASE nBrowTmp == 3 .AND. nBrow == 3
                  @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft + 2 SAY "[ Indexes ]" COLOR aBrows[ nBrow ]:colorSpec
               CASE nBrowTmp == 4 .AND. nBrow == 4
                  @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft + 2 SAY "[ Locks ]" COLOR aBrows[ nBrow ]:colorSpec
            ENDCASE

            EVAL( aBlocks[ nBrow ], aBrows[ nBrow ] )
            aBrows[ nBrow ]:configure()

            DO WHILE .NOT. aBrows[ nBrow ]:Stabilize()
               nKey := INKEY()
               IF nKey <> 0
                  EXIT
               ENDIF
            ENDDO
            aLastPos[ nBrow ] := aPos[ nBrow ]
         ENDDO
         nBrow := nBrowTmp
         aLastPos[ nBrow ] := aPos[ nBrow ]
      ENDIF

      IF aBrows[ nBrow ]:stable .AND. nKey == 0  // .AND. lHaveFocus
         nBrowTmp := nBrow
         nBrow := 1
         DO WHILE nBrow <= LEN( aBrows )
            @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft TO;
              aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nRight DOUBLE COLOR aBrows[ nBrow ]:colorSpec
            DO CASE
               CASE nBrowTmp == 1 .AND. nBrow == 1
                  @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft + 2 SAY "[ Connentions ]" COLOR aBrows[ nBrow ]:colorSpec
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
               @ aBrows[ nBrow ]:nTop - 1, aBrows[ nBrow ]:nLeft - 1 SAY "É" COLOR aBrows[ nBrow ]:colorSpec
            ENDIF

            IF hb_MilliSeconds() - nLastRefresh > s_nRefresh
               nLastLen := IIF( EMPTY( aBrows[ nBrow ]:cargo ), -1, LEN( aBrows[ nBrow ]:cargo ) )
               EVAL( aBlocks[ nBrow ], aBrows[ nBrow ] )
               IF nLastLen != IIF( EMPTY( aBrows[ nBrow ]:cargo ), 0, LEN( aBrows[ nBrow ]:cargo ) )
                  aBrows[ nBrow ]:configure()
               ELSE
                  aBrows[ nBrow ]:refreshAll()
               ENDIF
            ENDIF

            DO WHILE .NOT. aBrows[ nBrow ]:Stabilize()
               nKey := INKEY()
               IF nKey <> 0
                  EXIT
               ENDIF
            ENDDO
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
            nTmp := ( hb_MilliSeconds() - s_nUpTime ) / 1000 / 60  /* minutes up time */
            IF nTmp > 15
               s_nRefresh := 10000
            ELSEIF nTmp > 60
               s_nRefresh := 30000
            ELSEIF nTmp > 720
               s_nRefresh := 60000  /* one per minute after 12 hours */
            ENDIF
         ENDIF
      ENDIF

      /* select browse with left click, right click == columnc change only if mouse on row */
      IF nKey == K_LBUTTONUP
         mRow := MROW()
         mCol := MCOL()
         IF mRow == 0 .AND. mCol >= 2 .AND. mCol <= 10
            nKey := 0
            Administrate( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ) )
            IF NEXTKEY() > 0
               nKey := NEXTKEY()
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
               nKey = 0
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
               nKey = 0
            ELSEIF mRow >= aBrows[ nBrow ]:nTop + 2 .AND. aBrows[ nBrow ]:RowPos + aBrows[ nBrow ]:nTop + 1 != mRow
               nKey = 0
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
               nKey = 0
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
            nLastRefresh := 0
            lReconfigure := .T.
         CASE nKey == K_ALT_M
            Administrate( ActiveConnection( aBrows[ 1 ]:cargo, aPos[ 1 ] ) )
      ENDCASE

   ENDDO

RETURN Nil

STATIC FUNCTION GetAllConnections( oBrow )
   oBrow:cargo := leto_MgGetUsers()
   IF EMPTY( oBrow:cargo )
      oBrow:cargo := {}  /* can be NIL */
   ELSE
      AEVAL( @oBrow:cargo, {|aConn| aConn[ 7 ] := IIF( aConn[ 7 ] == "0", "CDX", "NTX" ) } )
      AEVAL( @oBrow:cargo, {|aConn| aConn[ 8 ] := Padl(Ltrim(Str(Int((Val(aConn[5])%86400)/3600))),3,'0') + ":";
                                                  + Padl(Ltrim(Str(Int((Val(aConn[5])%3600)/60))),2,'0') + ":";
                                                  + Padl(Ltrim(Str(Int(Val(aConn[5])%60))),2,'0') } )
   ENDIF
RETURN .T.

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
 //LOCAL aInfo

   //IF ( aInfo := leto_MgGetInfo() ) == Nil
   //   RETURN .F.
   //ENDIF

   SETCOLOR( "W+/R" )
   @ 0, 0 CLEAR TO 0, MAXCOL()
   cText := cVersion + " mode " + cMode + " at " + cAddress
   // cText := aInfo[13] + " CPU core " + cVersion + " at " + cAddress
   @ 0, ( MAXCOL() - LEN( cText ) ) / 2 SAY cText

   SETCOLOR( "W+/G" )
   @ 0, 2 SAY "[ Menu ]"
   SETCOLOR( "GR+/G" )
   @ 0, 4 SAY "M"

RETURN .T.

STATIC FUNCTION BasicInfo()
 STATIC aCpuLoads := {}
 LOCAL aInfo, nSec, nDay, nHour, nMin, oldc, nTransAll, nTransBad
 LOCAL aInfo3 := Leto_MgGetTime()
 LOCAL aInfo4 := Leto_MgSysInfo()
 LOCAL cTmp, nTmp

   IF EMPTY( ( aInfo := leto_MgGetInfo() ) )
      RETURN .F.
   ENDIF

   oldc := SETCOLOR( "W+/B,B/BG,,,W+/G" )
   @ 1, 0, 7, MAXCOL() BOX B_DOUBLE + " "
   @ 1, MAXCOL() / 2 SAY "Ë"
   @ 7, MAXCOL() / 2 SAY "Ê"
   @ 2, MAXCOL() / 2 TO 6,MAXCOL() / 2 DOUBLE
   @ 1, 2 SAY "[ Statistics ]"

#if 0
   IF EMPTY( aCpuLoads )
      aCpuLoads := hb_CPULoad( .T. )
   ELSE
      aCpuLoads := hb_CPULoad( aCpuLoads )
      IF ! EMPTY( aCpuLoads ) .AND. LEN( aCpuLoads ) > 4
         @ 6, MAXCOL() / 2 + 27 SAY STR( aCpuLoads[ 5 ] * 100, 3, 0 ) + " %"
      ENDIF
   ENDIF
#else
   // ToFix VAL() ????
   @ 2, MAXCOL() / 2 + 25 SAY "Load: " + STR( VAL( aInfo[ 18 ] ), 3, 0 ) + " %"
#endif

#if 0
      // LOCAL aInfo2 := leto_ConnectInfo()
      IIF( aInfo2[1] == LETO_CDX, "DBFCDX", "DBFNTX" ) + " LockScheme: " + STR( aInfo2[ 2 ], 1,0 )
#endif

   IF LEN( aInfo3) >= 3
      @ 2,  2 SAY "Server MB disk:" + STR( VAL( aInfo4[1] ) / 1024 / 1024, 8, 0 )
      @ 2, 26 SAY "RAM: " + STR( VAL( aInfo4[4] ) / 1024, 7, 0)
   ELSE
      @ 2,  2 SAY "Server memory :" + STR( VAL( aInfo[16] ) / 1024, 7, 0 )
   ENDIF
   @ 3,  2 SAY "Users  current: " + Padl( aInfo[1],7 )
   @ 3, 26 SAY "Max: " + Padl( aInfo[2], 7 )
   @ 4,  2 SAY "Tables current: " + Padl( aInfo[3],7 )
   @ 4, 26 SAY "Max: " + Padl( aInfo[4], 7 )
   @ 5,  2 SAY "Indexs current: " + Padl( aInfo[9],7 )
   @ 5, 26 SAY "Max: " + Padl( aInfo[10], 7 )

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
   // @ 3, MAXCOL() / 2 + 25 SAY "Rate: " + STR( VAL( aInfo[ 6 ] ) / aInfo3[ 3 ], 3, 0 )
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
      //@ 4, MAXCOL() / 2 + 25 SAY "Rate: " + STR( ( VAL( aInfo[ 7 ] ) / ( 1024 * 1024 ) ) / aInfo3[ 3 ], 3, 0 )  + " MB/s"
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
      //@ 5, MAXCOL() / 2 + 25 SAY "Rate: " +  STR( ( VAL( aInfo[ 8 ] ) / ( 1024 * 1024 ) ) / aInfo3[ 3 ], 3, 0 ) + " MB/s"
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
     @ 2, 1 SAY "? kill *ALL* active user [ *USE* with open tables ] or only *ONE* connection ?"
     @ 3, 2 SAY "type: ALL, USE or ONE" GET cConfirm PICT "@! XXX" VALID ALLTRIM( cConfirm ) $ "ALL-ONE-USE"
     READ
     lOk := LASTKEY() != K_ESC
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

   IF lOk .AND. cConfirm $ "ALL-USE"
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
            IF ! lOpen .AND. nKillId == s_myConn  // myself
               LOOP
            ENDIF
            IF( leto_MgKill( nKillID ) == nKillID )
               @ 5, 2 SAY " closed connection: " + aUser[ i, 1 ] + " " +;
                          ALLTRIM( aUser[ i, 4 ] ) + "@" + ALLTRIM( aUser[ i, 3 ] ) + SPACE( 10 )
               nKilled++
            ELSE
               @ 5, 2 SAY " closing connection: " + aUser[ i, 1 ] + " failed"
            ENDIF
         NEXT i
         INKEY( 2 )  // let the connection(s) close
      ENDDO
   ELSEIF lOk
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
         IF hb_MilliSeconds() - nUpTime > s_nRefresh
            hb_keyPut( K_CTRL_W )
         ELSE
            hb_keyPut( nKey )
         ENDIF
      CASE nMode == ME_INIT
         nUpTime := hb_MilliSeconds() 
         hb_keyPut( K_CTRL_PGDN )
      CASE nMode == ME_UNKEY
         nKey := LASTKEY()
         IF nKey == K_MWBACKWARD
            hb_keyPut( K_DOWN )
         ELSEIF nKey == K_MWFORWARD
            hb_keyPut( K_UP )
         ELSEIF nKey == K_RBUTTONUP
            hb_keyPut( K_ESC )
         ENDIF
   ENDCASE
RETURN nRet

STATIC FUNCTION ViewLogs( nConnection )
 LOCAL cText := leto_MgLog( nConnection, 0 )
 LOCAL cSave

   IF EMPTY( cText )
      timedAlert( " no LOG file for connection: " + hb_nTos( nConnection ), 1, "W+/R" )
   ELSE
      cSave := SAVESCREEN( 8, 0, MAXROW(), MAXCOL() )
      DO WHILE .T.
         memoedit( cText, 8, 0, MAXROW(), MAXCOL(), .F.,"memologger",1024 )
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
   ENDIF
RETURN NIL

STATIC FUNCTION Administrate( nConnection )
 LOCAL nMenu := 0
 LOCAL oldcolor := SetColor( "W+/G,W+/B" )
 LOCAL bLocked := leto_LockConn()  /* only query for active state */
 LOCAL aMenu := {;
      "1 Add    User",;
      "2 Delete User",;
      "3 Change Password",;
      "4 Change Access",;
      "5 Flush  Changes",;
      "6 Kill   Connecti",;
      "7 Change Refresh",;
      IIF( bLocked, "8 UNlock Server", "8 Lock   Server" ),;
      "9 View   Logfile",;
      "0 QUIT ! Console" }
 LOCAL cSave := SAVESCREEN( 1, 2, 2 + LEN( aMenu ), 19 )
 LOCAL arr

   DO WHILE nMenu < 6
      @ 1, 2, 2 + LEN( aMenu ), 19 BOX B_SINGLE + " "
      nMenu := Achoice( 2, 3, 2 + LEN( aMenu ), 18, aMenu, "MyChoice" )
      DO WHILE INKEY() != 0
      ENDDO
      RESTSCREEN( 1, 2, 2 + LEN( aMenu ), 19, cSave )

      DO CASE
         CASE nMenu == 1
            IF( arr := GetUser( .t.,.t. ) ) != Nil
               IF leto_useradd( arr[1], arr[2], arr[3] )
                  TimedAlert( "User is added", 2, "GR+/N" )
               ELSE
                  TimedAlert( "User is NOT added", 2, "R+/N" )
               ENDIF
            ELSE
               TimedAlert( "Operation canceled", 2, "R+/N" )
            ENDIF

         CASE nMenu == 2
            IF( arr := GetUser( .f.,.f. ) ) != Nil
               IF leto_userdelete( arr[1], arr[2], arr[3] )
                  TimedAlert( "User is deleted", 2, "GR+/N" )
               ELSE
                  TimedAlert( "User is NOT deleted", 2, "R+/N" )
               ENDIF
            ELSE
               TimedAlert( "Operation canceled", 2, "R+/N" )
            ENDIF

         CASE nMenu == 3
            IF( arr := GetUser( .t.,.f. ) ) != Nil
               IF leto_userpasswd( arr[1], arr[2] )
                  TimedAlert( "Password is changed", 2, "GR+/N" )
               ELSE
                  TimedAlert( "Password is NOT changed", 2, "R+/N" )
               ENDIF
            ELSE
               TimedAlert( "Operation canceled", 2, "R+/N" )
            ENDIF

         CASE nMenu == 4
            IF( arr := GetUser( .f.,.t. ) ) != Nil
               IF leto_userrights( arr[1], arr[3] )
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
         CASE nMenu == 0
            EXIT
         CASE nMenu == 10
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
      IF mRow < 1 .OR. mRow > 11 .OR. mCol < 2 .AND. mCol > 19
         hb_keyPut( K_ESC )
      ENDIF
   ENDIF
   DO CASE
      CASE nStatus == AC_EXCEPT
         cKey := Upper( hb_keyChar( nKey ) )
         DO CASE
            CASE cKey >= "0" .AND. cKey <= "9"
               hb_keyPut( K_ENTER )
               RETURN AC_GOTO
            CASE nKey == K_ENTER
               RETURN AC_SELECT
            CASE nKey == K_ESC .OR. nKey == K_RBUTTONUP
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
   @ 2, 2 SAY "Refresh rate ( 1/ 1000 ) second :" GET s_nRefresh PICTURE "@K 999999" ;
                                                  VALID s_nRefresh >= 100 .AND. s_nRefresh <= 360000
   READ
   SETCOLOR( oldcolor )
   SETCURSOR( oldcurs )
   RESTSCREEN( 2, 1, 6, MAXCOL() - 1, cSave )
RETURN .T.

STATIC FUNCTION GetUser( lPass, lRights )
 LOCAL cUser := SPACE( 21 )
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
   ELSEIF Len( cUser ) > 21
      TimedAlert( "Username too long", 2, "R+/N" )
      lOk := .F.
   ENDIF

   IF lOk .AND. lPass
      cPass := GETSECRET( SPACE( 42 ), 3, 2, .F., "Password  :" )
      IF LASTKEY() == K_ESC
         lOk := .F.
      ELSEIF ! EMPTY( cPass ) .AND. Len( cPass ) > 42
         TimedAlert( "Password too long", 2, "R+/N" )
         lOk := .F.
      ELSE
         cCheck := GETSECRET( SPACE( 42 ), 4, 2, .F., "Re-type   :" )
         IF LASTKEY() == K_ESC
            lOk := .F.
         ELSEIF cPass != cCheck
            TimedAlert( "Password check failed", 2, "R+/N" )
            lOk := .F.
         ENDIF
      ENDIF
   ENDIF

   IF lOk .AND. lRights
      cRights := SPACE( 3 )
      @ 5,2 SAY "Access rights - Admin,Manage,Write (default - NNN) :" GET cRights
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



#if 0
 a backup try ...
 LOCAL bLockLock := leto_LockLock()
      IIF( bLocked, "9 UNlock locks ", "8 Lock   locks " ),;

         CASE nMenu == 8
            TimedAlert( "LetoDBf server try 6 s to : " + IIF( bLockLock, "unlock", "lock" ) + " locking", 2, "W+/B" )
            IF bLockLock
               bLockLock := leto_LockLock( .F. )
            ELSE
               bLockLock := leto_LockLock( .T. )
            ENDIF
            TimedAlert( "LetoDBf server lock/ unlock locking: " + IIF( ! bLockLock, "failed", "successful" ), 2,;
                         IIF( bLockLock,"W+/G", "W+/R" ) )


      @ 6, 2 SAY "Server Lock Test wait"
      leto_udf( "LETO_BACKUPTABLES", "/tmp/PRO-CNC/DATEN" )
      hb_idleSleep( 30 )
      @ 6, 2 SAY "Server Lock Test done"
#endif


