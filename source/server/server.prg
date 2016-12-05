/*
 * Leto db server
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 *
 *           2015-2016 Rolf 'elch' Beckmann
 * removing nearly anything else the pure PRG level server start functions.
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
#include "dbstruct.ch"
#include "rddsys.ch"
#include "common.ch"
#include "dbinfo.ch"
#include "fileio.ch"
#include "error.ch"
#include "rddleto.ch"

#ifndef HB_HRB_BIND_DEFAULT
   #define HB_HRB_BIND_DEFAULT 0x0
#endif

REQUEST DBFNTX
REQUEST DBFCDX
REQUEST DBFFPT
REQUEST SIXCDX
REQUEST DBFNSX
REQUEST HB_MEMIO

#ifdef __BM
   REQUEST BMDBFNTX
   REQUEST BMDBFCDX
   REQUEST BM_DBSEEKWILD
#endif


MEMVAR oApp

#ifdef __LINUX__
   #ifndef __CONSOLE__
      ANNOUNCE HB_GTSYS
      REQUEST HB_GT_STD_DEFAULT
   #endif
   #define DEF_SEP      '/'
   #define DEF_CH_SEP   '\'
#else
   #ifndef __CONSOLE__
      ANNOUNCE HB_GTSYS
      REQUEST HB_GT_GUI_DEFAULT
   #endif
   #define DEF_SEP      '\'
   #define DEF_CH_SEP   '/'
#endif

/* comment out 2 lines for the complete Harbour core command/ function set */
// REQUEST __HB_EXTERN__
// #include "hbextern.ch"

/* comment out 2 lines for the complete hbct contrib */
// #define __HBEXTERN__HBCT__REQUEST 1
// #include "hbct.hbx"

/* else these selected functions are linked into server executable with REQUEST */
REQUEST ABS, ALLTRIM, AT, CHR, CTOD, DATE, DAY, DELETED, DESCEND, DTOC, DTOS, ;
        EMPTY, I2BIN, L2BIN, LEFT, LEN, LOWER, LTRIM, MAX, MIN, MONTH, OS, PAD, PADC, ;
        PADL, PADR, RAT, RECNO, RIGHT, ROUND, RTRIM, SPACE, STOD, STR, STRZERO, ;
        SUBSTR, REPLICATE, TIME, TRANSFORM, TRIM, UPPER, VAL, YEAR
REQUEST HB_DATETIME, HB_DTOT, HB_TTOD, HB_NTOT, HB_TTON, HB_CTOT, HB_TTOC, ;
        HB_TTOS, HB_STOT, HB_HOUR, HB_MINUTE, HB_SEC, HB_VALTOEXP, HB_ZCOMPRESS
REQUEST HB_HEXTONUM, HB_NUMTOHEX

REQUEST hb_ATokens, hb_tokenGet, hb_tokenCount, hb_WildMatch, hb_DiskSpace, hb_strformat
REQUEST FieldPos, FieldGet, FieldPut, Deleted, hb_FieldType, hb_FieldLen, hb_FieldDec

REQUEST Array, AClone, ASize, ADel, AIns, AEval, AScan, ASize, ASort
REQUEST hb_idleSleep, hb_milliSeconds

REQUEST dbGoTop, dbGoBottom, dbSkip, dbGoto, dbSeek, Bof, Eof, dbEval, dbInfo, dbStruct
REQUEST dbAppend, dbDelete, dbRecall, dbCommit
REQUEST __dbJoin
REQUEST ordKeyVal, dbOrderInfo, RDDinfo, Alias, Select, dbSelectArea

REQUEST LETO_VARSET, LETO_VARGET, LETO_VARINCR, LETO_VARDECR, LETO_VARDEL, LETO_VARGETLIST
REQUEST LETO_VARGETCACHED, LETO_BVALUE, LETO_BSEARCH

/* something elch special ;-) */
REQUEST MIXKEY

REQUEST LETO_GETUSTRUID, LETO_WUSLOG, LETO_GETAPPOPTIONS
REQUEST LETO_SELECT, LETO_SELECTAREA, LETO_ALIAS, LETO_AREAID, LETO_SELECTAREA, LETO_MAKEALIAS
REQUEST LETO_REC, LETO_DBEVAL
REQUEST LETO_RECLOCK, LETO_RECLOCKLIST, LETO_RECUNLOCK, LETO_TABLELOCK, LETO_TABLEUNLOCK
REQUEST LETO_TABLELOCK, LETO_TABLEUNLOCK
REQUEST LETO_DBUSEAREA, LETO_DBCLOSEAREA, LETO_ORDLISTADD
REQUEST LETO_DBCREATE, LETO_ORDCREATE
REQUEST LETO_BACKUPTABLES, LETO_IDLESLEEP
REQUEST LETO_UDFMUSTQUIT

#ifdef __BM
REQUEST LBM_DbGetFilterArray, LBM_DbSetFilterArray, LBM_DbSetFilterArrayAdd
REQUEST LBM_DbSetFilterArrayDel, LBM_DbSetFilter
REQUEST LETO_ATOC, LETO_CTOA
#endif

#ifdef __HB_EXT_CDP__
   #include "hbextcdp.ch"
#else
   /* only selected codepages */
   #include "letocdp.ch"

   /* ! all ! available codepages */
   // #define __HBEXTERN__HBCDPAGE__REQUEST__
   // #include "hbcpage.hbx"
#endif

#include "cmdleto.h"

STATIC s_cDirBase
STATIC s_pHrb

THREAD STATIC lOldDeleted
THREAD STATIC cOldFilter
THREAD STATIC nOldOrder
THREAD STATIC xOldScope
THREAD STATIC xOldScopeBottom

PROCEDURE Main( cCommand, cData )

   s_cDirBase := hb_DirBase()
   leto_setDirBase( s_cDirBase )

   IF cCommand != NIL .AND. Lower( cCommand ) == "stop"

      /* connect and send QUIT */
      PUBLIC oApp := HApp():New()

      IF leto_SendMessage( oApp:nPort, LETOCMD_stop, oApp:cAddr )
#ifdef __CONSOLE__
         ? "Send SToP to server..."
#else
         WrLog( "Have send STOP to server, soon should go down ..." )
#endif
      ELSE
#ifdef __CONSOLE__
         ? "Can't STOP the server (not started?)..."
#else
         WrLog( "Can't STOP the server (not started?)..." )
#endif
      ENDIF
      RETURN

   ELSEIF cCommand != NIL .AND. Left( Lower( cCommand ), 6 ) == "reload"

      /* send message to reload letoudf.hrb */
      PUBLIC oApp := HApp():New()

      IF ! leto_SendMessage( oApp:nPort, LETOCMD_udf_rel, , cData )
#ifdef __CONSOLE__
         ? "Can't reload letoudf.hrb"
#else
         WrLog( "Can't reload letoudf.hrb" )
#endif
      ENDIF
      RETURN

   ELSE

#ifdef __CONSOLE__

      AltD()
      CLS
      @ 1, 5 SAY "Server listening ..."
      @ 2, 5 SAY "Press [ESC] to terminate the program"
      StartServer()

#endif

#ifdef __WIN_DAEMON__

      StartServer()

#endif

#ifdef __WIN_SERVICE__

      IF cCommand != NIL
         IF Lower( cCommand ) == "install"
            IF leto_serviceInstall()
               WrLog( "LetoDB service has been successfully installed" )
            ELSE
               WrLog( "Error installing LetoDB service: " + Str( letowin_GetLastError() ) )
            ENDIF
            RETURN
         ELSEIF Lower( cCommand ) == "uninstall"
            IF leto_serviceDelete()
               WrLog( "LetoDB service has been deleted" )
            ELSE
               WrLog( "Error deleting LetoDB service: " + Str( letowin_GetLastError() )  )
            ENDIF
            RETURN
         ELSEIF Lower( cCommand ) == "test"
            StartServer()
            RETURN
         ELSE
            ? "LetoDB_mt { install | uninstall }"
         ENDIF
         RETURN
      ENDIF

      IF ! leto_serviceStart( "StartServer" )
         WrLog( "LetoDB service has had some problems: " + Str( letowin_GetLastError() ) )
         ErrorLevel( 1 )
      ENDIF

#endif

#ifdef __LINUX_DAEMON__

      IF ! leto_Daemon()
         WrLog( "Can't become a daemon" )
         ErrorLevel( 2 )
      ELSE
         StartServer()
      ENDIF

#endif

   ENDIF

   RETURN

PROCEDURE StartServer()

   PUBLIC oApp := HApp():New()

   /* verify datapath */
   IF ! Empty( oApp:DataPath )
      IF ! hb_DirExists( oApp:DataPath )
         ErrorLevel( 2 )
         WrLog( "Leto DB Server: DataPath '" + oApp:DataPath + "' does not exists .." )
         RETURN
      ENDIF
   ENDIF

   WrLog( "LetoDBf Server try to start ..." )
   leto_InitSet()

   leto_CreateData( oApp:cAddr, oApp:nPort )

   leto_HrbLoad()

#if 0
   /* elch Todo: that won't work if driver is later changed by client */
   IF ! Empty( oApp:cTrigger )
      hb_rddInfo( RDDI_TRIGGER, oApp:cTrigger, leto_DefaultDriver() )
   ENDIF
   IF ! Empty( oApp:cPendingTrigger )
      hb_rddInfo( RDDI_PENDINGTRIGGER, oApp:cPendingTrigger, leto_DefaultDriver() )
   ENDIF
#endif

   IF ! leto_Server( oApp:nPort, oApp:cAddr, oApp:nTimeOut, oApp:nZombieCheck )
#if __HARBOUR__ > 0x020100
      WrLog( "Socket error " + hb_socketErrorString( hb_socketGetError() ) )
      ? "error .. "
#else
      WrLog( "Socket error " )
#endif
      ErrorLevel( 1 )
   ENDIF

   WrLog( "Server has been closed." )

   RETURN

STATIC FUNCTION leto_hrbLoad( cData )

   LOCAL lUdfEnabled := leto_GetAppOptions( LETOOPT_lUdfEnable )
   LOCAL cHrbName, pInit
   LOCAL lDefault, pHrb, aFunc
   MEMVAR oErr

   IF VALTYPE( cData ) != "C"
      cHrbName := s_cDirBase + "letoudf.hrb"
      lDefault := .T.
   ELSE
      cHrbName := s_cDirBase + LEFT( cData, LEN( cData ) - 1 )
      lDefault := .F.
   ENDIF

   BEGIN SEQUENCE
      IF File( cHrbName )
         IF lUdfEnabled
            pHrb := hb_hrbLoad( HB_HRB_BIND_DEFAULT, cHrbName )
            IF ! Empty( pHrb )
               WrLog( "UDF file: " + cHrbName + " have been loaded." )

               IF lDefault
                  s_pHrb = pHrb
                  IF ! Empty( pInit := hb_hrbGetFunSym( s_pHrb, 'UDF_Init' ) )
                     hb_ExecFromArray( pInit )
                  ENDIF
               ELSE
                  aFunc := hb_hrbGetFunList( pHrb )
                  IF ! EMPTY( aFunc )
                     hb_hrbGetFunSym( s_pHrb, aFunc[ 1 ] )
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            WrLog( "using remote UDF is disabled." )
         ENDIF
      ELSE
         WrLog( "UDF file: " + cHrbName + " not present." )
      ENDIF

   RECOVER USING oErr
       WrLog( "UDF file : " + cHrbName + " contains error, not loaded." )
       WrLog( "UDF Error: " + Leto_ErrorMessage( oErr ) )
   END SEQUENCE

   RETURN NIL


FUNCTION hs_UdfReload( cData )

   LOCAL lUdfEnabled := leto_GetAppOptions( LETOOPT_lUdfEnable )

   IF lUdfEnabled
      IF VALTYPE( cData ) != "C" .AND. ! Empty( s_pHrb )
         hb_hrbUnload( s_pHrb )
         s_pHrb := nil
         WrLog( "letoudf.hrb has been unloaded." )
      ENDIF
      leto_hrbLoad( cData )
   ELSE
      WrLog( "using remote UDF is disabled." )
   ENDIF

   RETURN NIL


EXIT PROCEDURE EXITP

   LOCAL pExit

   IF ! Empty( s_pHrb ) .AND. ! Empty( pExit := hb_hrbGetFunSym( s_pHrb, 'UDF_Exit' ) )
      hb_ExecFromArray( pExit )
   ENDIF

   leto_ReleaseData()

   RETURN

CLASS HApp

   DATA cAddr     INIT NIL
   DATA nPort     INIT 2812
   DATA nTimeOut  INIT -1
   DATA DataPath  INIT ""
   DATA LogFile   INIT ""
   DATA lLower    INIT .F.
   DATA lFileFunc INIT .F.
   DATA lAnyExt   INIT .F.
   DATA lShare    INIT .F.      // .T. - new mode, which allows share tables with other processes
   DATA lNoSaveWA INIT .F.        // .T. - new mode, which forces dbUseArea() each time "open table" is demanded
   DATA nDriver   INIT 0
   DATA nBigLock  INIT 0
   DATA lPass4M   INIT .F.
   DATA lPass4L   INIT .F.
   DATA lPass4D   INIT .F.
   DATA cPassName INIT "leto_users"
   DATA lCryptTraffic INIT .F.
   DATA cTrigger
   DATA cPendingTrigger
   DATA nZombieCheck INIT 0

   METHOD New()

ENDCLASS

METHOD New() CLASS HApp

   LOCAL cIniName := "letodb.ini"
   LOCAL aIni, i, j, cTemp, cPath, nDriver
   LOCAL nPort
   LOCAL nMaxVars, nMaxVarSize
   LOCAL nCacheRecords := 10
   LOCAL nTables_max := NIL
   LOCAL nUsers_max := NIL
   LOCAL nDebugMode := 0
   LOCAL lHardCommit := .F.
   LOCAL nAutOrder
   LOCAL nMemoType
   LOCAL nMemoBlocksize := 0
   LOCAL lOptimize := .T.
   LOCAL lForceOpt := .F.
   LOCAL lUDFEnabled := .F.
   LOCAL lSetTrigger := .F.

#ifdef __LINUX__

   IF File( "/etc/" + cIniName )
      aIni := rdIni( "/etc/" + cIniName )
   ELSEIF File( s_cDirBase + cIniName )
      aIni := rdIni( s_cDirBase + cIniName )
   ENDIF

#else

   IF File( s_cDirBase + cIniName )
      aIni := rdIni( s_cDirBase + cIniName )
   ENDIF

#endif

   IF !Empty( aIni )
      FOR i := 1 TO Len( aIni )
         IF aIni[ i, 1 ] == "MAIN"
            FOR j := 1 TO Len( aIni[ i, 2 ] )
               IF aIni[ i, 2, j, 1 ] == "PORT"
                  IF ( nPort := Val( aIni[ i, 2, j, 2 ] ) ) >= 42
                     ::nPort := nPort
                  ENDIF
               ELSEIF aIni[ i, 2, j, 1 ] == "IP"
                  ::cAddr := aIni[ i, 2, j, 2 ]
               ELSEIF aIni[ i, 2, j, 1 ] == "TIMEOUT"
                  ::nTimeOut := Val( aIni[ i, 2, j, 2 ] )
               ELSEIF aIni[ i, 2, j, 1 ] == "DATAPATH"
                  ::DataPath := StrTran( aIni[ i, 2, j, 2 ], DEF_CH_SEP, DEF_SEP )
                  IF Right( ::DataPath, 1 ) $ DEF_SEP
                     ::DataPath := Left( ::DataPath, Len( ::DataPath ) - 1 )
                  ENDIF
               ELSEIF aIni[ i, 2, j, 1 ] == "LOGPATH"
                  ::LogFile := StrTran( aIni[ i, 2, j, 2 ], DEF_CH_SEP, DEF_SEP )
                  IF ! Empty( ::LogFile )
                     IF Right( ::LogFile, 1 ) != DEF_SEP
                        ::LogFile += DEF_SEP
                     ENDIF
                     leto_setDirBase( ::LogFile )
                  ENDIF
               ELSEIF aIni[ i, 2, j, 1 ] == "LOWER_PATH"
                  ::lLower := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "ENABLEFILEFUNC"
                  ::lFileFunc := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "ENABLEANYEXT"
                  ::lAnyExt := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "SHARE_TABLES"
                  ::lShare := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "NO_SAVE_WA"
                  ::lNoSaveWA := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "DEFAULT_DRIVER"
                  ::nDriver := iif( Lower( aIni[ i, 2, j, 2 ] ) == "ntx", LETO_NTX, 0 )
               ELSEIF aIni[ i, 2, j, 1 ] == "LOCK_SCHEME"
                  ::nBigLock := Val( aIni[ i, 2, j, 2 ] )
                  IF ::nBigLock < 0 .OR. ::nBigLock > 6
                     ::nBigLock := 0
                  ENDIF
               ELSEIF aIni[ i, 2, j, 1 ] == "PASS_FOR_LOGIN"
                  ::lPass4L := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "PASS_FOR_MANAGE"
                  ::lPass4M := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "PASS_FOR_DATA"
                  ::lPass4D := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "PASS_FILE"
                  ::cPassName := aIni[ i, 2, j, 2 ]
               ELSEIF aIni[ i, 2, j, 1 ] == "CRYPT_TRAFFIC"
                  ::lCryptTraffic := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "MAX_VARS_NUMBER"
                  nMaxVars := Val( aIni[ i, 2, j, 2 ] )
               ELSEIF aIni[ i, 2, j, 1 ] == "MAX_VAR_SIZE"
                  nMaxVarSize := Val( aIni[ i, 2, j, 2 ] )
               ELSEIF aIni[ i, 2, j, 1 ] == "CACHE_RECORDS"
                  IF ( nCacheRecords := Val( aIni[ i, 2, j, 2 ] ) ) <= 0
                     nCacheRecords := 10
                  ENDIF
               ELSEIF aIni[ i, 2, j, 1 ] == "TABLES_MAX"
                  IF ( nTables_max := Val( aIni[ i, 2, j, 2 ] ) ) <= 100 .OR. nTables_max > 200000
                     nTables_max := NIL
                  ENDIF
               ELSEIF aIni[ i, 2, j, 1 ] == "USERS_MAX"
                  IF ( nUsers_max := Val( aIni[ i, 2, j, 2 ] ) ) <= 10 .OR. nUsers_max > 100000
                     nUsers_max := NIL
                  ENDIF
               ELSEIF aIni[ i, 2, j, 1 ] == "DEBUG"
                  IF ( nDebugMode := Val( aIni[ i, 2, j, 2 ] ) ) <= 0
                     nDebugMode := 0
                  ENDIF
               ELSEIF aIni[ i, 2, j, 1 ] == "HARDCOMMIT"
                  lHardCommit := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "OPTIMIZE"
                  lOptimize := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "AUTORDER"
                  nAutOrder := Val( aIni[ i, 2, j, 2 ] )
               ELSEIF aIni[ i, 2, j, 1 ] == "MEMO_BSIZE"
                  nMemoBlocksize := Val( aIni[ i, 2, j, 2 ] )
                  IF nMemoBlocksize > 65536
                     nMemoBlocksize := 65536
                  ELSEIF nMemoBlocksize < 32
                     nMemoBlocksize := 0  // default for memotype
                  ELSE
                     nMemoBlockSize := Int( nMemoBlocksize / 32 ) * 32
                  ENDIF
               ELSEIF aIni[ i, 2, j, 1 ] == "MEMO_TYPE"
                  IF Lower( aIni[ i, 2, j, 2 ] ) $ 'dbt'
                     nMemoType := DB_MEMO_DBT
                  ELSEIF Lower( aIni[ i, 2, j, 2 ] ) $ 'fpt'
                     nMemoType := DB_MEMO_FPT
                  ELSEIF Lower( aIni[ i, 2, j, 2 ] ) $ 'smt'
                     nMemoType := DB_MEMO_SMT
                  ELSE
                     IF ::nDriver == LETO_NTX
                        nMemoType := DB_MEMO_DBT
                     ELSE
                        nMemoType := DB_MEMO_FPT
                     ENDIF
                  ENDIF
               ELSEIF aIni[ i, 2, j, 1 ] == "FORCEOPT"
                  lForceOpt := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "ALLOW_UDF"
                  lUDFEnabled := ( aIni[ i, 2, j, 2 ] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "ENABLESETTRIGGER"
                  lSetTrigger := ( aIni[ i, 2, j, 2] == '1' )
               ELSEIF aIni[ i, 2, j, 1 ] == "TRIGGER"
                  ::cTrigger := aIni[ i, 2, j, 2 ]
               ELSEIF aIni[ i, 2, j, 1 ] == "PENDINGTRIGGER"
                  ::cPendingTrigger := aIni[ i, 2, j, 2 ]
               ELSEIF aIni[ i, 2, j, 1 ] == "ZOMBIE_CHECK"
                  ::nZombieCheck := Val( aIni[ i, 2, j, 2 ] )
               ENDIF
            NEXT
         ELSEIF aIni[ i, 1 ] == "DATABASE"
            cPath := nDriver := Nil
            FOR j := 1 TO Len( aIni[ i, 2 ] )
               IF aIni[ i, 2, j, 1 ] == "DATAPATH"
                  cPath := StrTran( aIni[ i, 2, j, 2 ], DEF_CH_SEP, DEF_SEP )
                  IF Right( cPath, 1 ) $ DEF_SEP
                     cPath := Left( cPath, Len( cPath ) - 1 )
                  ENDIF
               ELSEIF aIni[ i, 2, j, 1 ] == "DRIVER"
                  nDriver := iif( ( cTemp := Lower( aIni[ i, 2, j, 2 ] ) ) == "cdx", ;
                     0, iif( cTemp == "ntx", LETO_NTX, Nil ) )
               ENDIF
            NEXT
            IF cPath != Nil
               cPath := StrTran( cPath, DEF_CH_SEP, DEF_SEP )
               IF Left( cPath, 1 ) != DEF_SEP
                  cPath := DEF_SEP + cPath
               ENDIF
               IF Right( cPath, 1 ) != DEF_SEP
                  cPath += DEF_SEP
               ENDIF
               leto_AddDataBase( cPath, iif( nDriver == Nil, ::nDriver, nDriver ) )
            ENDIF
         ENDIF
      NEXT
   ENDIF

   IF ::lLower
      SET( _SET_FILECASE, 1 )
      SET( _SET_DIRCASE, 1 )
      ENDI
#if 0
      IF ::lNoSaveWA
         ::lShare := .T.
      ENDIF
#endif

      leto_SetAppOptions( iif( Empty( ::DataPath ), "", ::DataPath ), ::nDriver, ::lFileFunc, ;
         ::lAnyExt, ::lPass4L, ::lPass4M, ::lPass4D, ::cPassName, ::lCryptTraffic, ;
         ::lShare, ::lNoSaveWA, nMaxVars, nMaxVarSize, nCacheRecords, nTables_max, nUsers_max, ;
         nDebugMode, lOptimize, nAutOrder, nMemoType, lForceOpt, ::nBigLock, lUDFEnabled, nMemoBlocksize,;
         ::lLower, ::cTrigger, ::cPendingTrigger, lSetTrigger, lHardCommit )

      RETURN Self

FUNCTION leto_SetEnv( xScope, xScopeBottom, xOrder, cFilter, lDeleted )

   IF ! Empty( xOrder )
      nOldOrder := OrdNumber()
      ordSetFocus( xOrder )
   ENDIF
   IF ValType( cFilter ) == "C" .AND. ! Empty( cFilter )
      cOldFilter := DbFilter()
      dbSetFilter( &( "{||" + cFilter + "}" ), cFilter )
   ENDIF
   IF lDeleted != Nil
      lOldDeleted := SET( _SET_DELETED )
      SET( _SET_DELETED, lDeleted )
   ENDIF

   IF xScope != Nil
      xOldScope := dbOrderInfo( DBOI_SCOPETOP )
      dbOrderInfo( DBOI_SCOPETOP, /*filename*/, /*iOrder*/, xScope )
   ENDIF
   IF xScopeBottom != Nil
      xOldScopeBottom := dbOrderInfo( DBOI_SCOPEBOTTOM )
      dbOrderInfo( DBOI_SCOPEBOTTOM,,, iif( xScopeBottom == Nil, xScope, xScopeBottom ) )
   ENDIF

   RETURN NIL

FUNCTION leto_ClearEnv( xScope, xScopeBottom, xOrder, cFilter )

   IF lOldDeleted != Nil
      SET( _SET_DELETED, lOldDeleted )
      lOldDeleted := NIL
   ENDIF

   IF ! Empty( xOrder )
      IF VALTYPE( nOldOrder ) == "N"
         ordSetFocus( nOldOrder )
      ENDIF
   ENDIF

   IF xScope != Nil
      IF xOldScope != Nil
         dbOrderInfo( DBOI_SCOPETOP,,, xOldScope )
         xOldScope := Nil
      ELSE
         dbOrderInfo( DBOI_SCOPETOPCLEAR )
      ENDIF
   ENDIF
   IF xScopeBottom != Nil
      IF xOldScope != Nil
         dbOrderInfo( DBOI_SCOPEBOTTOM,,, xOldScopeBottom )
         xOldScopeBottom := NIL
      ELSE
         dbOrderInfo( DBOI_SCOPEBOTTOMCLEAR )
      ENDIF
   ENDIF

   IF ValType( cFilter ) == "C"
      IF VALTYPE( cOldFilter ) == "C" .AND. ! Empty( cOldFilter )
         dbSetFilter( &( "{||" + cOldFilter + "}" ), cOldFilter )
         cOldFilter := NIL
      ELSE
         dbClearFilter()
      ENDIF
   ENDIF

   RETURN NIL


#if 0
/* useful for dynamic server address */
/* try to find IP address for an interface name -- or return the first found with a MAC */
STATIC FUNCTION IPForInterface( cName )

   LOCAL aIFace := hb_socketGetIFaces( HB_SOCKET_AF_INET, .T. )
   LOCAL nIFace := 0
   LOCAL cIP    := ""

   IF Len( aIFace ) > 0
      IF ! Empty( cName )
         cName := Upper( cName )
         nIFace := AScan( aIFace, {| aItm | Upper( aItm[ HB_SOCKET_IFINFO_NAME ] ) == cName } )
      ELSE
         /* first interface with guilty MAC address */
         nIFace := AScan( aIFace, {| aItm | ! Empty( aItm[ HB_SOCKET_IFINFO_HWADDR ] ) .AND. ;
            ! aItm[ HB_SOCKET_IFINFO_HWADDR ] == "00:00:00:00:00:00" } )
      ENDIF
   ENDIF
   IF nIFace > 0
      cIP := aIFace[ nIFace, HB_SOCKET_IFINFO_ADDR ]
   ENDIF

   RETURN cIP
#endif

/* elch mixkey */
#pragma BEGINDUMP
#include <extend.h>

HB_FUNC( MIXKEY )
{
   const char * ptr    = hb_parc( 1 );
   unsigned int iNLen  = hb_parni( 2 );
   unsigned int iZLen  = hb_parni( 3 );
   char * pcRet = ( char * ) hb_xgrab( iZLen );
   char * pNPart = ( char * ) hb_xgrab( iNLen );
   char * pZPart = ( char * ) hb_xgrab( iZLen );
   unsigned int iNPart = 0;
   unsigned int iZPart = 0;
   unsigned int i = 0;
   unsigned int y;

   while( i < iZLen )
      pcRet[ i++ ] = ' ';

   while( strlen( ptr ) > 0 )
   {
      switch( ptr[ 0 ] )
      {
         case '1':
         case '2':
         case '3':
         case '4':
         case '5':
         case '6':
         case '7':
         case '8':
         case '9':
            if( iNPart < iNLen )
               pNPart[ iNPart++ ] = ptr[ 0 ];
            break;

         case '0':
            if( iNPart < iNLen && iNPart > 0 )
               pNPart[ iNPart++ ] = ptr[ 0 ];
            break;

         case ' ':
            break;

         default :
            if( iZPart < iZLen )
               pZPart[ iZPart++ ] = ptr[ 0 ];
            break;
      }
      ptr++;
   }

   if( iNPart > 0 )
   {
      y = 0;
      i = iNLen - iNPart;
      while( i < iNLen )
         pcRet[ i++ ] = pNPart[ y++ ];
   }
   else
      pcRet[ iNLen - 1 ] = '0';

   if( iZPart > 0 )
   {
      y = 0;
      i = iNLen;
      while( i < iZLen )
         pcRet[ i++ ] = pZPart[ y++];
   }

   hb_retclen( pcRet, iZLen );

   hb_xfree( pcRet );
   hb_xfree( pNPart );
   hb_xfree( pZPart );
}

#pragma ENDDUMP
