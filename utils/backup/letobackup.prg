/*
 * Backup utility for Leto db server
 *
 * Copyright 2012 Pavel Tsarenko <tpe2@mail.ru>
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

# letobackup.ini file sample

# Server connect string
Server   = //127.0.0.1:2812
# Database path at the server
DataPath = c:\database
# Database directory to backup
DataBase = /data1/
# Backup directory
Backup   = c:\backup\
# File masks to backup
Mask     = *.dbf,*.fpt,*.dbt
# If Lock=1, trying to lock server before backup
Lock     = 1
#Lock     = &if(hb_Hour(hb_DateTime())==12, "1", "0")
# Timeout in seconds
Seconds  = 30
#
# If Wait=1, it's need succesfull server lock to backup
Wait     = 1
# Command string to execute archivator after backup
ArcCmd   = "C:\Program files\7-zip\7z.exe" a -r c:\arc\data%dt%_%tm% c:\backup\*.*
#ArcCmd   = "C:\Program files\7-zip\7z.exe" a -r c:\arc\data[DTOS(Date())+if(hb_Hour(hb_DateTime())==7,"","_"+StrZero(hb_Hour(hb_DateTime()),2))] c:\backup\*.*
# eof letobackup.ini

*/

#include "dbinfo.ch"
#include "rddleto.ch"

#ifdef __PLATFORM__WINDOWS
   #define DEF_SEP "\"
#else
   #define DEF_SEP "/"
#endif

Static cDB := ""
Static cDir := ""
Static cOutDir := ""
Static cMask := ""
Static lLock := .t.
STATIC cDriver := "DBFNTX"

Function Main( cAddress, cUser, cPasswd )
   LOCAL nRes, lLocked := .f.
   LOCAL cDirBase := hb_dirBase()
   LOCAL aIni := RdIni( cDirBase + "letodb.ini" ), ai
   LOCAL nSecs := 30
   LOCAL lWait := .t.
   LOCAL cArc
   LOCAL lSuccess := .f.
   LOCAL nSec
   LOCAL lCopyTo
   LOCAL n1, n2
   LOCAL cPort := ""

   REQUEST leto, dbfntx, dbfcdx
   REQUEST StrZero
   REQUEST hb_DateTime, hb_Hour, hb_Minute, hb_Sec
   ALTD()

   rddSetDefault( "LETO" )
   set deleted off

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
         elseif ai[1] = "DATAPATH"
            cDB := ai[2]
         elseif ai[1] = "DATABASE"
            cDir := ai[2]
         elseif ai[1] = "DEFAULT_DRIVER"
            cDriver := "DBF" + ai[2]
         elseif ai[1] = "BACKUP"
            cOutDir := ai[2]
         elseif ai[1] = "MASK"
            cMask := ai[2]
         elseif ai[1] = "LOCK"
            lLock := ( IniParam(ai[2]) = "1")
         elseif ai[1] = "SECONDS"
            nSecs := Val(ai[2])
         elseif ai[1] = "WAIT"
            lWait := ( IniParam(ai[2]) = "1")
         elseif ai[1] = "ARCCMD"
            cArc := ai[2]
         endif
      next
   endif

   if cAddress == Nil
      ? "Server address is absent ..."
      Return Nil
   elseif Empty( cDir )
      ? "Source directory is absent ..."
      Return Nil
   elseif Empty( cOutDir )
      ? "Output directory is absent ..."
      Return Nil
   endif

   if ! ( LEFT( cAddress,2 ) == "//" )
      cAddress := "//" + cAddress + IF( ! EMPTY(cPort) , ":" + cPort, "") + "/"
   endif
   ? "Connecting to " + cAddress
   IF ( leto_Connect( cAddress, cUser, cPasswd ) ) == -1
      nRes := leto_Connect_Err()
      IF nRes == LETO_ERR_LOGIN
         ? "Login failed"
      ELSEIF nRes == LETO_ERR_RECV
         ? "Recv Error"
      ELSEIF nRes == LETO_ERR_SEND
         ? "Send Error"
      ELSE
         ? "No connection"
      ENDIF

      Return Nil
   ENDIF
   ? "Connected to " + leto_GetServerVersion()

   if lLock
      ? "Trying to lock server..."
      nSec := Seconds()
      lLocked := leto_LockLock( .t., nSecs)
      ? if( lLocked, "Success", "Failure" ), Seconds() - nSec, " seconds"
   endif

   if ! lLock .or. lLocked .or. ! lWait
      lCopyTo := ( cOutDir = StrTran( cDB + cDir, "/", DEF_SEP ) )
      Backup( cAddress, lCopyTo )
      lSuccess := .t.
   endif

   if lLocked
      ? "Unlocking server ..."
      leto_LockLock( .f. )
      ? if( leto_LockLock( .f. ), "Failure", "Success" )
   endif
   ? "Backup finished"

   if lSuccess .and. ! Empty(cArc)
      /* leto_udf("leto_backuptables","/tmp/bak/DATEN") */
      ? 'Runnung archivator'
      if (n1 := At("[", cArc)) != 0 .and. (n2 := At("]", cArc)) != 0 .and. n2 > n1
         cArc := Left( cArc, n1-1 ) + &(Substr(cArc, n1 + 1, n2 - n1 - 1)) + Substr( cArc, n2 + 1 )
      else
         cArc := StrTran(cArc, "%dt%", DTOS(Date()))
         cArc := StrTran(cArc, "%tm%", StrTran(Time(), ":", "_"))
      endif
      Run (cArc)
      ? "Done."
   endif

   INKEY( 12 )

Return Nil

Static function IniParam( cPar )
Return if( cPar = "&", &(Substr(cPar, 2)), cPar)

Static function Backup( cAddress, lCopyTo )
   LOCAL aFiles := {}, af, aDirs := {}, cd
   LOCAL aTables
   LOCAL nFiles := 0, nTables := 0
   LOCAL aMask
   LOCAL nSec := Seconds()

   if Right( cDir, 1) != "/"
      cDir += "/"
   endif
   if Right( cOutDir, 1) != DEF_SEP
      cOutDir += DEF_SEP
   endif
   if Right( cAddress, 1 ) = "/" .AND. cDir = "/"
      cAddress := Left( cAddress, len(cAddress) - 1 )
   endif
   aMask := hb_ATokens( Lower(cMask), "," )

   ? "Get list of opened tables..."
   aTables := leto_MgGetTables()
   AEval(aTables, {|a| a[2] := StrTran(a[2], DEF_SEP, "/")})
   ? LTrim(Str(len( aTables ))) + " tables found"

   ? "Scanning directory tree..."
   GetFiles( aFiles, cDir, aMask, aDirs )
   ? LTrim(Str(len( aFiles ))) + " files found"

   ? "Checking directories..."
   for each cd in aDirs
      cd := cOutDir + Substr( cd, len( cDir ) + 1 )
      if ! hb_dirExists( cd )
         hb_dirCreate( cd )
      endif
   next
   ? "Copying files..."
   for each af in aFiles
      MakeCopy( cAddress, cDB, aTables, cDir, cOutDir, af, @nFiles, @nTables, lCopyTo )
   next

   ? LTrim(Str( nFiles )) + " files and " + LTrim(Str( nTables )) + " tables copied", Seconds() - nSec, " seconds"

Return Nil

Static function IsMatch( aMask, cFileName )
   LOCAL lMatch := .f., cMask
   for each cMask in aMask
      if hb_WildMatch( cMask, cFileName, .t. )
         lMatch := .t.
         exit
      endif
   next
Return lMatch

Static function MakeCopy( cAddress, cDB, aTables, cDir, cOutDir, af, nFiles, nTables, lCopyTo )
   LOCAL cNewFile := cOutDir + Substr( StrTran(af[1], "/", DEF_SEP ), len(cDir) + 1 )
   LOCAL ad := Directory( cNewFile ), i
   LOCAL cTable

   if Empty(ad) .or. Abs(ad[1, 2] - af[2]) > 2 .or. ad[1, 3] != af[3] .or. ad[1, 4] != af[4]

      SetPos( Row(), 0 )
      if ASCAN(aTables, {|a| Lower(a[2]) == Lower(af[1])}) != 0

         ?? Padr( "Copying table " + af[1], MaxCol() )
         //LETO_BACKUP_TABLES
         dbUseArea( .t.,, cAddress + af[1], "leto_old", .t., .t. )
         if ( i := RAt( DEF_SEP, cNewFile ) ) # 0
            cNewFile := Left( cNewFile, i ) + Substr( Lower( cNewFile ), i + 1 )
         endif
         hb_rddInfo( RDDI_MEMOTYPE, dbInfo( DBI_MEMOTYPE ), cDriver )
         hb_rddInfo( RDDI_MEMOEXT, dbInfo( DBI_MEMOEXT ), cDriver )

         select leto_old
         set order to 0
         if lCopyTo

            cTable := cAddress + StrTran( Substr( cNewFile, len(cDB) + 1), DEF_SEP, "/")
            copy to (cTable) all

         else

            dbCreate( cNewFile, dbStruct(), cDriver, .t., "dbf_new")
            select leto_old
            leto_SetSkipBuffer( 1000 )
            go top
            while ! eof()
               dbf_new->(dbAppend())
               for i := 1 to FCount()
                  dbf_new->(FieldPut( i, leto_old->(FieldGet( i )) ))
               next
               if Deleted()
                  dbf_new->(dbDelete())
               endif
               skip
            enddo
            dbf_new->(dbCloseArea())

         endif
         
         close
         hb_FSetDateTime( cNewFile, af[3], af[4] )
         nTables ++

      elseif ! IsMemoFile( aTables, Lower(af[1]) )

         ?? Padr( "Copying file  " + af[1], MaxCol() )
         __CopyFile( cDB + af[1], cNewFile )
         hb_FSetDateTime( cNewFile, af[3], af[4] )
         nFiles ++

      elseif IsMemoFile( aTables, Lower(af[1]) )
         
         nFiles ++

      endif
   endif
Return nil

Static function GetFiles( aFiles, cDir, aMask, aDirs )
   LOCAL aDir := leto_Directory( cDir, "D" ), ad

   AADD( aDirs, StrTran( cDir, "/", DEF_SEP ) )
   for each ad in aDir
      if "D" $ ad[5]
         if ! ( ad[1] = "." .or. cDir = ad[1] .or. StrTran(cDB + cDir + ad[1] + "/", "/", DEF_SEP) == cOutDir )
            GetFiles( aFiles, cDir + ad[1] + "/", aMask, aDirs )
         endif
      elseif IsMatch( aMask, Lower( ad[1] ) )
         AADD( aFiles, {cDir + ad[1], ad[2], ad[3], ad[4], ad[5]} )
      endif
   next
Return nil

Static function IsMemoFile( aTables, cFileName )
Return (Right( cFileName, 4) = '.dbt' .or. Right( cFileName, 4) = '.fpt') .and.;
   ASCAN( aTables, {|a| Lower(a[2]) = Left(cFileName, len(cFileName)-3) }) # 0
