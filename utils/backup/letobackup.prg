/*
 * a backup utility 'pattern' for LetoDBf server
 *
 * Copyright 2019 Rolf 'elch' Beckmann
 *           fully rewrite of a formerly same named file
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License FOR more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  IF not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, IF you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  IF you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * IF you write modifications of your own FOR Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * IF you do not wish that, delete this exception notice.
 *
 */


#include "fileio.ch"
#include "inkey.ch"

#ifdef __PLATFORM__WINDOWS
   #define DEF_SEP     '\'
   #define DEF_CH_SEP  '/'
   #define DEF_PATTERN '*:*'
#else
   #define DEF_SEP     '/'
   #define DEF_CH_SEP  '\'
   #define DEF_PATTERN '*'
#endif

#define LOG_STRIPLINE "-----"


STATIC cOutDir := ""
STATIC nHandle := -1


FUNCTION Main( cIniFile )

   LOCAL cAddress, cUser, cPasswd
   LOCAL cAppIni, aIni, aOpt, cPort
   LOCAL nWaitLock, nWaitDelay, nRetryLock, nRetryDelay
   LOCAL cLocalPath
   LOCAL cArcCmd, cArcFile, cExtraCmd, cLogFile
   LOCAL hRepl, cIncMask, cExcMask, aIncMask, aExcMask, cOneMask, cTmp, cRes
   LOCAL n1, n2, n3
   LOCAL nRow, nCol
   LOCAL nResult, cStdOut, cStdErr
   LOCAL lLocked := .F.
   LOCAL lShared := .F., lThirdParty := .F., lTrustThird := .T.
   LOCAL lSuccess := .T.

   AltD()
   ? hb_datetime(year(date()),month(date()),day(date()),hb_hour(hb_datetime()),hb_minute(hb_datetime()),hb_sec(hb_datetime())+120 )

   hb_default( @cAddress,   "//127.0.0.1:2812/" )
   hb_default( @cUser,      "" )
   hb_default( @cPasswd,    "" )
   hb_default( @nWaitLock,  30 )
   hb_default( @nWaitDelay, 20 )
   hb_default( @nRetryLock, 5 )
   hb_default( @nRetryDelay,120 )
   hb_default( @cArcFile,   "letoback.tar.gz" )
   hb_default( @cArcCmd,    "" )
   hb_default( @cExtraCmd,  "" )
   hb_default( @cLogFile,   "backup_%date%.log" )
   hb_default( @cExcMask,   "" )
   hb_default( @cLocalPath, "" )

   ErrorLevel( 1 )  /* config problem */

   /*
    * when there is a '.ini' same named as executable 'letobackup.ini',
    * or the 'rddleto.ini' valid for any LETO application,
    * we are here already 'auto-connected' to the server.
    * If not, then maybe because a needed user-name or password was missing.
    * Following search in the '.ini' file is to fetch some additional options
    * used for the backup.
    */

#ifdef __XHARBOUR__
   hb_FNameSplit( hb_CmdArgArgv(), /* @cPath */, @cAppIni, /* cExt */ )
#else
   hb_FNameSplit( hb_ProgName(), /* @cPath */, @cAppIni, /* cExt */ )
#endif
   cAppIni := hb_FNameMerge( /* cPath */, cAppIni, ".ini" )
   IF ! File( cAppIni )
      IF File( "rddleto.ini" )
         cAppIni := "rddleto.ini"
#if ! defined( __PLATFORM__WINDOWS )
      ELSEIF File( "/etc/letodb.ini" )
         cAppIni := "/etc/letodb.ini"
#endif
      ELSEIF File( "letodb.ini" )
         cAppIni := "letodb.ini"
      ELSEIF ! EMPTY( cIniFile ) .AND. FILE( cIniFile + IIF( RIGHT( cIniFile, 4 ) != ".ini", ".ini", "" ) )
         cAppIni := cIniFile + IIF( RIGHT( cIniFile, 4 ) != ".ini", ".ini", "" )
      ELSE
         Alert( "Error: missing configuration ( '.ini' file not found )" )
         QUIT
      ENDIF
   ENDIF

   aIni := RdIni( cAppIni )
   IF ! EMPTY( aIni )
      FOR EACH aOpt in aIni[ 1, 2 ]
         IF aOpt[ 1 ] == "SERVER"
            cAddress := aOpt[ 2 ]
         ELSEIF aOpt[ 1 ] == "PORT"
            cPort := aOpt[ 2 ]
         ELSEIF aOpt[ 1 ] == "USER"
            cUser := aOpt[ 2 ]
         ELSEIF aOpt[ 1 ] == "PASSWORD"  /* set it to "?" to enter it below */
            cPasswd := aOpt[ 2 ]

         /* backup specific options */
         ELSEIF aOpt[ 1 ] == "BACKUPPATH"
            cOutDir := aOpt[ 2 ]
         ELSEIF aOpt[ 1 ] == "MASKINCLUDE"
            cIncMask := aOpt[ 2 ]
         ELSEIF aOpt[ 1 ] == "MASKEXCLUDE"
            cExcMask := aOpt[ 2 ]
         ELSEIF aOpt[ 1 ] == "LOCKWAIT"  /* 0 to disable */
            nWaitLock := MAX( 0, Val( aOpt[ 2 ] ) )
         ELSEIF aOpt[ 1 ] == "LOCKDELAY"
            nWaitDelay := MIN( nWaitLock - 3, Val( aOpt[ 2 ] ) )
         ELSEIF aOpt[ 1 ] == "RETRYLOCK"
            nRetryLock := MAX( 1, Val( aOpt[ 2 ] ) )
         ELSEIF aOpt[ 1 ] == "RETRYDELAY"
            nRetryDelay := MAX( nWaitLock, Val( aOpt[ 2 ] ) )
         ELSEIF aOpt[ 1 ] == "LOGFILE"
            cLogFile := aOpt[ 2 ]
         ELSEIF aOpt[ 1 ] == "THIRDPARTY"
            lThirdParty := aOpt[ 2 ] == "1"
         ELSEIF aOpt[ 1 ] == "LOCALPATH"  /* '.' for directory of executable */
            cLocalPath := aOpt[ 2 ]
         ELSEIF aOpt[ 1 ] == "ARCFILE"
            cArcFile := aOpt[ 2 ]
         ELSEIF aOpt[ 1 ] == "ARCCMD"
            cArcCmd := aOpt[ 2 ]
         ELSEIF aOpt[ 1 ] == "EXTRACMD"
            cExtraCmd := aOpt[ 2 ]
         ENDIF
      NEXT
   ENDIF

   /* prepare include/ exclude list into arrays a[Inc|Exc]Mask */
   IF EMPTY( cIncMask )
      cIncMask := DEF_PATTERN
   ENDIF
   IF EMPTY( cExcMask )
      cExcMask := AddSlash( cOutDir ) + DEF_PATTERN
   ENDIF
   cIncMask := PrepareMaskList( cIncMask, @aIncMask )
   cExcMask := PrepareMaskList( cExcMask, @aExcMask )


   /* login to server if not already done by auto-login */
   ErrorLevel( 2 )  /* connection problem */
   IF EMPTY( Leto_GetCurrentConnection() )

      IF LEFT( cAddress, 2 ) != "//" .AND. ! ":" $ cAddress .AND. ! EMPTY( cPort )
         cAddress := "//" + cAddress + IF( ! EMPTY( cPort ), ":" + cPort, "" )
         IF RIGHT( cAddress ) != "/"
            cAddress += "/"
         ENDIF
      ENDIF

      /* secret read password */
      IF cPassWd == "?" .AND. ! EMPTY( cUser )
         cPasswd := SecretRead( "Password: " )
         IF EMPTY( cPasswd )
            ALERT( "Password not given" )
            RETURN NIL
         ENDIF
      ENDIF

      IF leto_Connect( cAddress, cUser, cPasswd ) == -1
         ALERT( "Fail to connect to LetoDBf at " + cAddress + ": " + leto_Connect_Err( .T. ) )
         RETURN NIL
      ENDIF
   ENDIF

   LogIt( "Connected to " + leto_GetServerVersion() + " at: " + cAddress )

   /* verify/ create directory FOR the archivate result at server */
   IF ! leto_DirExist( cOutDir )
      IF leto_DirMake( cOutDir ) < 0
         LogIt( "ERROR to create directory '" + cOutDir + "' FOR archive" )
         RETURN NIL
      ENDIF
   ENDIF

   IF nWaitLock > 0
      ErrorLevel( 3 )  /* server locking problem */

      LogIt( "Trying to lock server, please wait ..." )
      DO WHILE nRetryLock > 0
         n1 = Seconds()
         lLocked := leto_LockLock( .T., nWaitLock, nWaitDelay )
         LogIt( iif( lLocked, "Success", "Failure" ) + " after " + hb_ntos( Seconds() - n1 ) + " seconds" +;
                iif( nRetryLock > 0, ", retry in: " + hb_ntos( nRetryDelay ), "" ) )
         IF ! lLocked
            lSuccess := .F.
         ELSE
            EXIT
         ENDIF
         nRetryLock -= 1
         IF nRetryLock > 0
            lSuccess := .T.
            n1 := Seconds()
            nRow := ROW()
            nCol := COL() - LEN( hb_ntos( nRetryDelay ) )
            SET CURSOR OFF
            DO WHILE Seconds() - n1 < nRetryDelay
               SetPos( nRow, nCol )
               ?? hb_ntos( INT( nRetryDelay - ( Seconds() - n1 ) ) ) + SPACE( 5 )
               IF INKEY( 1 ) == K_ESC
                  lSuccess := .F.
                  nRetryLock := 0
                  EXIT
               ENDIF
            ENDDO
            SET CURSOR ON
         ENDIF
      ENDDO
   ENDIF

   IF LSuccess
      LogIt( "Backup start at " + DToS( Date() ) + " " + Time() )
   ENDIF

   IF LSuccess .AND. lThirdParty
      ErrorLevel( 4 )  /* third party problem */

      /* ask the server if its running in shared mode */
      IF leto_UDF( "leto_GetAppOptions", LETOOPT_SHARETABLES )
         lShared := .T.
      ENDIF

      /*
       * to verify that third party software do not disturb the backup,
       * *ANY* DBF at server is tried to open in SHARED mode plus applying a file-lock
       */
      IF lShared
         /* lTrustThird == .T. --> only report failures without break */
         lSuccess := OpenAllTable( lTrustThird )
      ENDIF
      IF lSuccess
         ErrorLevel( 0 )
      ENDIF
   ENDIF

   /* preparing the archivate command, fill wildcards with values */
   IF lSuccess .AND. ! EMPTY( cArcCmd )
      hRepl := { => }
      hRepl[ "%dst%" ]  := cOutDir
      hRepl[ "%date%" ] := DToS( Date() )
      hRepl[ "%time%" ] := Time()
      cArcFile := hb_StrReplace( cArcFile, hRepl )
      cLogFile := hb_StrReplace( cLogFile, hRepl )

      hRepl[ "%target%" ] := cArcFile
      cArcCmd := hb_StrReplace( cArcCmd, hRepl )
      cExtraCmd := hb_StrReplace( cExtraCmd, hRepl )

      /* cIncMask == "*.dbf, *.dbt";   cArcCmd == "do_with{ %mask%}"
       * ==> " do_with *.dbf do_with *.dbt"
       */

      /* aIncMask for including list %mask% */
      IF ( n3 := AT( "%mask%", cArcCmd ) ) > 0
         n1 := RAT( "{", LEFT( cArcCmd, n3 - 1 ) )
         n2 := AT( "}", SUBSTR( cArcCmd, n3 + 6 ) ) + n3 + 5
         IF n1 > 0 .AND. n2 > 0 .AND. n2 > n1 .AND. n3 > n1 .AND. n3 + 5 < n2
            cTmp := SUBSTR( cArcCmd, n1 + 1, n2 - n1 - 1 )
            cRes := ""
            FOR EACH cOneMask in aIncMask
               cRes += hb_StrReplace( cTmp, { "%mask%" => cOneMask } )
            NEXT
            cArcCmd := LEFT( cArcCmd, n1 - 1 ) + cRes + SUBSTR( cArcCmd, n2 + 1 )
         ENDIF
      ENDIF

      /* aExcMask for excluding list %xmask% */
      IF ( n3 := AT( "%xmask%", cArcCmd ) ) > 0
         n1 := RAT( "{", LEFT( cArcCmd, n3 - 1 ) )
         n2 := AT( "}", SUBSTR( cArcCmd, n3 + 6 ) ) + n3 + 5
         IF n1 > 0 .AND. n2 > 0 .AND. n2 > n1 .AND. n3 > n1 .AND. n3 + 5 < n2
            IF LEN( aExcMask ) < 1
               cArcCmd := LEFT( cArcCmd, n1 -1 ) + SUBSTR( cArcCmd, n2 + 1 )
            ELSE
               cTmp := SUBSTR( cArcCmd, n1 + 1, n2 - n1 - 1 )
               cRes := ""
               FOR EACH cOneMask in aExcMask
                  cRes += hb_StrReplace( cTmp, { "%xmask%" => cOneMask } )
               NEXT
               cArcCmd := LEFT( cArcCmd, n1 - 1 ) + cRes + SUBSTR( cArcCmd, n2 + 1 )
            ENDIF
         ENDIF
      ENDIF

      /* replace if not given in a repeated macro */
      cArcCmd := hb_StrReplace( cArcCmd, { "%mask%" => cIncMask } )
      cArcCmd := hb_StrReplace( cArcCmd, { "%xmask%" => cExcMask } )

      ErrorLevel( 4 )  /* archivator problem */

      LogIt( 'Runnung archivator:' + hb_eol() + cArcCmd )
      nResult := leto_ProcessRun( cArcCmd,, @cStdOut, @cStdErr )
      IF nResult != 0
         lSuccess := .F.
      ELSE
         /* if data-change logging for server is activated: rename old log to let a new created */
         Leto_LogToggle()
         ErrorLevel( 0 )
      ENDIF

      /* an extra command not logged or verified */
      IF ! EMPTY( cExtraCmd )
         LogIt( 'Extra action:' + hb_eol() + cExtraCmd )
         leto_ProcessRun( cExtraCmd,, @cStdOut, @cStdErr )
      ENDIF

      LogIt( "==> " + hb_ntos( nResult ) )
      IF ! EMPTY( cStdOut )
         LogIt( LOG_STRIPLINE + " StdOut:" )
         LogIt( cStdOut )
      ENDIF
      IF ! EMPTY( cStdErr )
         LogIt( LOG_STRIPLINE + " StdErr:" )
         LogIt( cStdErr )
      ENDIF
      LogIt( LOG_STRIPLINE )

   ENDIF

   IF lShared
      dbCloseAll()
   ENDIF

   IF lLocked
      IF leto_LockLock( .F. )
         LogIt( "Server unlocked successful" )
      ENDIF
   ENDIF

   /* move the archive from server to local */
   IF lSuccess .AND. Leto_File( AddSlash( cOutDir ) + cArcFile )
      IF ! EMPTY( cLocalPath )
         IF leto_FCopyFromSrv( AddSlash( cLocalPath ) + cArcFile, AddSlash( cOutDir ) + cArcFile )
            leto_FErase( AddSlash( cOutDir ) + cArcFile )
         ELSE
            LogIt( "Failed to copy archive" )
            ErrorLevel( 5 )  /* copy arcive to local problem */
            lSuccess := .F.
         ENDIF
      ENDIF
   ENDIF

   /* create log file at server in archivate directory */
   IF ! EMPTY( cLogFile )
      nHandle := Leto_FCreate( AddSlash( cOutDir ) + cLogFile, FC_NORMAL )
   ENDIF

   LogIt( "*****" + hb_eol() + "Backup " + iif( lSuccess, "finished", "FAILED END" ) + " at:" + DToS( Date() ) + Time() )

   IF nHandle >= 0
      Leto_FClose( nHandle )
   ENDIF

   RETURN NIL


STATIC FUNCTION OpenAllTable( lIgnoreFail )

   LOCAL lSuccess := .T.
   LOCAL aFiles := {}, aDirs := {}, aTable
   LOCAL aDbfMask
   LOCAL nOpened := 0, nFailed := 0, aFailed := {}
   LOCAL cAlias, lFail
   LOCAL nSize

   PrepareMaskList( "*.dbf, *.DBF", @aDbfMask )
   nSize := GetFiles( aFiles, DEF_SEP, aDbfMask, aDirs, cOutDir )

   IF ! EMPTY( aFiles )
      LogIt( LOG_STRIPLINE + hb_eol() + "Third Party Test, try to open and file-lock tables" )
      LogIt( hb_ntos( LEN( aFiles ) ) + " DBF found, size: " + hb_ntos( nSize / 1024 / 1024 ) + " MB" )
      FOR EACH aTable in aFiles

         hb_FNameSplit( aTable[ 1 ], /* @cPath */, @cAlias, /* cExt */ )
         cAlias := Upper( cAlias )
         IF LEFT( cAlias, 1 ) $ "0123456789"
            cAlias := "E" + cAlias
         ENDIF

         lFail := .F.
         BEGIN SEQUENCE WITH {| e | Break( e ) }
            IF ! dbUseArea( .T.,, aTable[ 1 ], cAlias, .T. ) .OR. ! FLock()
               AADD( aFailed, aTable[ 1 ] )
               nFailed++
               lFail := .T.
            ENDIF
         RECOVER
            AADD( aFailed, aTable[ 1 ] )
            nFailed++
            lFail := .T.
         END SQUENCE

         IF lFail .AND. ! lIgnoreFail
            lSuccess := .F.
            EXIT
         ELSEIF ! lFail
            nOpened++
         ENDIF

      NEXT

      LogIt( hb_ntos( nOpened ) + " DBF successful opened and locked" + hb_eol() + LOG_STRIPLINE )
      IF ! EMPTY( aFailed )
         LogIt( hb_ntos( nFailed ) + "DBF *FAILED* to open and FLock" )
         AEVAL( aFailed, {| aFile | LogIt( aFile[ 1 ] ) } )
         LogIt( LOG_STRIPLINE )
      ENDIF
   ELSE
      LogIt( "!! NO !! Third Party Test, as no DBF were found" )
   ENDIF

   RETURN lSuccess


/* --- */

STATIC FUNCTION AddSlash( cPath )

   IF ! RIGHT( cPath, 1 ) $ "/\"
      cPath += DEF_SEP
   ENDIF

   RETURN cPath

STATIC FUNCTION LogIt( cString, lDisplay )

   STATIC cText := ""

   IF VALTYPE( nHandle ) == "N" .AND. nHandle >= 0
      IF ! EMPTY( cText )
         Leto_FWrite( nHandle, cText )
         cText := ""
      ENDIF
      Leto_FWrite( nHandle, cString + hb_eol() )
   ELSE
      cText += cString + hb_eol()
   ENDIF
   IF VALTYPE( lDisplay ) != "L" .OR. lDisplay
      ? cString
   ENDIF

   RETURN NIL

STATIC FUNCTION GetFiles( aFiles, cDir, aMask, aDirs )

   LOCAL aDir, aFile
   LOCAL nSize := 0
   LOCAL cSep

   cDir := StrTran( cDir, DEF_CH_SEP, DEF_SEP )
   IF LEFT( cDir, 1 ) == DEF_SEP
      cDir := SUBSTR( cDir, 2 )
   ENDIF
   IF ! EMPTY( cDir )
      AADD( aDirs, cDir )
      cSep := DEF_SEP
   ELSE
      cSep := ""
   ENDIF

   aDir := leto_Directory( cDir, "D" )
   IF ! EMPTY( aDir )
      /* sort for name descending, then all files before directories */
      ASORT( aDir,,, {| aF1, aF2 | aF1[ 1 ] < aF2[ 1 ] } )
      ASORT( aDir,,, {| aF1, aF2 | AT( "D", aF1[ 5 ] ) < AT( "D", aF2[ 5 ] ) } )
   ENDIF

   FOR EACH aFile in aDir
      IF "D" $ aFile[ 5 ]
         IF aFile[ 1 ] != "." .AND. aFile[ 1 ] != ".." .AND. iif( VALTYPE( cOutDir ) == "C", aFile[ 1 ] != cOutDir, .T. )
            nSize += GetFiles( aFiles, cDir + cSep + aFile[ 1 ], aMask, aDirs )
         ENDIF
      ELSEIF Matching( aFile[ 1 ], aMask )
         AADD( aFiles, { cDir + cSep + aFile[ 1 ], aFile[ 2 ], aFile[ 3 ], aFile[ 4 ], aFile[ 5 ] } )
         nSize += aFile[ 2 ]
      ENDIF
   NEXT

   RETURN nSize

STATIC FUNCTION Matching( cFileName, aMask )

   LOCAL lMatch := .F., cMask

   FOR EACH cMask in aMask
      IF hb_WildMatch( cMask, cFileName, .T. )
         lMatch := .T.
         EXIT
      ENDIF
   NEXT

   RETURN lMatch

FUNCTION PrepareMaskList( cMask, aMask, cSep )

   IF EMPTY( cSep )
      cSep := ","
   ENDIF

   IF ! EMPTY( cMask )
      aMask := hb_ATokens( cMask, "," )
      AEVAL( aMask, {| cOne, n | aMask[ n ] := AllTrim( cOne ) } )
      /* write back trimmed */
      cMask := ""
      AEVAL( aMask, {| cOne, n | cMask += iif( n < 2, "", cSep ) + cOne } )
   ELSE
      cMask := ""
      aMask := {}
   ENDIF

   RETURN cMask

FUNCTION SecretRead( cText )

   LOCAL cPassWd := ""
   LOCAL nKey

   ? cText
   DO WHILE .T.
      nKey := INKEY( 0.1 )
      IF nKey == 0
         LOOP
      ELSEIF nKey == K_ESC
         cPasswd := ""
         EXIT
      ELSEIF nKey == K_ENTER
         EXIT
      ELSEIF nKey == K_BS
         IF LEN( cPasswd ) > 0
            SetPos( Row(), Col() -1 )
            QQOut( " " )
            SetPos( Row(), Col() -1 )
            cPasswd := LEFT( cPasswd, LEN( cPasswd ) -1 )
         ENDIF
      ELSE
         QQOut( "*" )
         cPasswd += CHR( nKey )
      ENDIF
   ENDDO

   RETURN cPassWd
