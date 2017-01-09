/*
 * Management utility for Leto db server
 *
 * Copyright 2008 Alexander S.Kresin <alex@belacy.belgorod.su>
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
#include "hwgui.ch"
#include "hxml.ch"
#include "rddleto.ch"

#ifndef __XHARBOUR__
ANNOUNCE HB_GTSYS
REQUEST HB_GT_GUI_DEFAULT
#endif

Static CRLF
Static aOpers[6], aSent[6], aRead[6]

Memvar oApp, oSayServer, cRes

Function Main( cAddress )
Local cIp := "", nPort := 2812, nRefr := 2, arr
Public oApp, oSayServer, cRes

   CRLF := Chr(13)+Chr(10)
   Afill( aOpers,0 )
   Afill( aSent,0 )
   Afill( aRead,0 )
   oApp := HApp():New()

   oApp:nItemCurr := 0
   IF cAddress != Nil
      IF ( arr := GetIpFromPath( "//" + cAddress + "/" ) ) != Nil
         cIp := arr[1]
         nPort := arr[2]
         oApp:nItemCurr := Ascan( oApp:aServers, cIp )
      ENDIF     
   ENDIF

   INIT WINDOW oApp:oMainWnd MAIN TITLE "Server management utility" ;
     AT 200,0 SIZE 600,400 FONT HFont():Add( "Georgia",0,-15,,204 )

   @ 0,0 PANEL oApp:oTool OF oApp:oMainWnd SIZE 0,56

   @ 10,2 COMBOBOX oApp:oCombo ITEMS oApp:aServers SIZE 140, 22 EDIT OF oApp:oTool ;
      ON CHANGE {||onComboChg()} STYLE CBS_DROPDOWN + CBS_AUTOHSCROLL ;
      TOOLTIP "Server ip address or name"
   @ 150,2 GET oApp:oGetPort VAR nPort SIZE 50,24 OF oApp:oTool PICTURE "9999" TOOLTIP "Port"
   @ 200,2 BUTTON oApp:oBtnGo CAPTION "Go" SIZE 40, 24 OF oApp:oTool ON CLICK {||GoConnect()}
   @ 250,10 GET UPDOWN oApp:oGetRefr VAR nRefr RANGE 1,60 OF oApp:oTool SIZE 50,32 TOOLTIP "Refresh time in seconds"

   @ 310,10 SAY oSayServer CAPTION "" SIZE 200,24 OF oApp:oTool FONT oApp:oMainWnd:oFont

   @ 10,30 BUTTON "Main" SIZE 70, 24 OF oApp:oTool ON CLICK {||BasicInfo()}
   @ 90,30 BUTTON "Users" SIZE 70, 24 OF oApp:oTool ON CLICK {||UsersInfo()}
   @ 170,30 BUTTON "Tables" SIZE 70, 24 OF oApp:oTool ON CLICK {||TablesInfo()}

   @ 520,12 BUTTON oApp:oBtnLock CAPTION "Lock" SIZE 55, 32 OF oApp:oTool ON CLICK {||LockConn()} ;
     TOOLTIP "Lock/Unlock new connections"
   @ 520,12 BUTTON oApp:oBtnKill CAPTION "Kill" SIZE 50, 32 OF oApp:oTool ON CLICK {||KillUser()} ;
     TOOLTIP "Disconnect current user"

   @ 0,56 BROWSE oApp:oBrw1 ARRAY SIZE 600,250 STYLE WS_BORDER + WS_VSCROLL ;
        ON SIZE {|o,x,y|o:Move(,,x)}
   @ 0,309 BROWSE oApp:oBrw2 ARRAY SIZE 600,110 STYLE WS_BORDER + WS_VSCROLL ;
        ON SIZE {|o,x,y|o:Move(,,x,y-oApp:oSplit:nTop-oApp:oSplit:nHeight)}

   @ 0,306 SPLITTER oApp:oSplit SIZE 600,3 DIVIDE {oApp:oBrw1} FROM {oApp:oBrw2} ;
        ON SIZE {|o,x,y|o:Move(,,x)}

   oApp:oBtnKill:Hide()
   IF oApp:nItemCurr > 0
      oApp:oCombo:SetItem( oApp:nItemCurr )
   ENDIF
   ACTIVATE WINDOW oApp:oMainWnd
Return Nil

Static Function onComboChg()
   oApp:nItemCurr := MIN( LEN( oApp:aParams ), hwg_SendMessage( oApp:oCombo:handle,CB_GETCURSEL,0,0 ) + 1 )
   IF oApp:nItemCurr > 0
      oApp:oGetPort:SetText( oApp:aParams[oApp:nItemCurr,1] )
   ELSE
      oApp:oGetPort:SetText( "2812" )
   ENDIF
Return .T.

Static Function LoginInfo( cu )
Local oDlg
Local cUser, cPassword := ""

   cUser := Iif( cu == Nil, "", cu )
   INIT DIALOG oDlg TITLE "" AT 20,20 SIZE 300,160 ;
        FONT oApp:oMainWnd:oFont CLIPPER

   @ 20,10 SAY "Login:" SIZE 100,22
   @ 120,10 GET cUser SIZE 260,26

   @ 20,40 SAY "Password:" SIZE 100,22
   @ 120,40 GET cPassword SIZE 160,26 STYLE ES_PASSWORD

   @ 100,120 BUTTON "Ok" ID IDOK SIZE 100,32

   ACTIVATE DIALOG oDlg

   IF oDlg:lResult
      Return { Iif( Empty(cUser),Nil,Trim(cUser) ), Iif( Empty(cUser),Nil,Trim(cPassword) ) }
   ELSE
      cRes := Nil
   ENDIF

Return cRes

Static Function GoConnect()
Local cIp, nPort, nRes, cErrText, nItem, aLogin

   IF oApp:oTimer == Nil
      cIp := hwg_GetEditText( oApp:oCombo:oParent:handle, oApp:oCombo:id )
      nPort := Val( oApp:oGetPort:GetText() )

      IF !Empty(cIp) .AND. !Empty(nPort)
         IF oApp:nItemCurr > 0 .AND. oApp:aServers[oApp:nItemCurr] == cIp .AND. ;
               Val(oApp:aParams[oApp:nItemCurr,1]) == nPort
            IF Empty( oApp:aParams[oApp:nItemCurr,2] )
               aLogin := { Nil, Nil }
            ELSE
               aLogin := LoginInfo( oApp:aParams[oApp:nItemCurr,2] )
            ENDIF
         ELSE
            aLogin := LoginInfo()
         ENDIF
         IF aLogin != Nil
            oSayServer:SetText( "" )
            oApp:oBtnGo:SetText("Go")
            leto_Disconnect()
            IF ( leto_Connect( "//"+cIp+":"+Ltrim(Str(nPort))+"/", aLogin[1], aLogin[2] ) ) == -1
               nRes := leto_Connect_Err()
               IF nRes == LETO_ERR_LOGIN
                  cErrText := "Login failed"
               ELSEIF nRes == LETO_ERR_RECV
                  cErrText := "Recv Error"
               ELSEIF nRes == LETO_ERR_SEND
                  cErrText := "Send Error"
               ELSE
                  cErrText := "No connection"
               ENDIF
                  oSayServer:SetText( cErrText )
               IF oApp:oTimer != Nil
                  oApp:oTimer:End()
                  oApp:oTimer := Nil
               ENDIF
               Return .F.
            ENDIF

            IF ( nItem := Ascan( oApp:aServers, cIp ) ) == 0
               Aadd( oApp:aServers, cIp )
               Aadd( oApp:aParams, { Ltrim(Str(nPort)), aLogin[1] } )
               oApp:lUpdList := .T.
            ELSEIF !( aLogin[1] == oApp:aParams[nItem,2] )
               oApp:aParams[nItem,2] := aLogin[1]
               oApp:lUpdList := .T.
            ENDIF

            IF !BasicInfo()
               Return .F.
            ENDIF

            oApp:oBtnGo:SetText("X")
            oSayServer:SetText( leto_GetServerVersion() )
            IF oApp:oTimer == Nil
               SET TIMER oApp:oTimer OF oApp:oMainWnd VALUE 1000 ACTION {||TimerFunc()}
            ENDIF
         ENDIF
      ELSE
         hwg_MsgStop( "Ip address or Port number is absent !" )
      ENDIF
   ELSE
      oApp:oBtnGo:SetText("Go")
      leto_Disconnect()
      oSayServer:SetText( "No connection" )
      oApp:oTimer:End()
      oApp:oTimer := Nil
   ENDIF

Return .T.

Static Function Disconnect()

   oApp:nInfoType := 0
   oApp:oBrw1:aArray := {}
   oApp:oBrw2:aArray := {}
   oApp:oBrw1:Refresh()
   oSayServer:SetText( "No connection" )
   IF oApp:oTimer != Nil
      oApp:oTimer:End()
      oApp:oTimer := Nil
   ENDIF

Return Nil

Static Function BasicInfo()
Local aInfo, nSec, nDay, nHour
Local nOpers, nSent, nRead
Local nSec1, nCpu
Local aInfo3
Local aInfo4
Static lReady := .F., nCurIndex := 1, nCurSec := 0, nLastOpers := 0, nLastSent := 0, nLastRead := 0

   oApp:oBtnKill:Hide()
   oApp:oBtnLock:Show()
   IF oApp:lSend
      oApp:nRequest = 1
      Return .T.
   ENDIF
   oApp:lSend := .T.
   IF ( aInfo := leto_MgGetInfo() ) == Nil
      oApp:lSend := .F.
      Disconnect()
      Return .F.
   ENDIF
   aInfo3 := Leto_MgGetTime()
   aInfo4 := Leto_MgSysInfo()
   oApp:lSend := .F.

   IF oApp:nInfoType != 1
      oApp:oBrw1:aArray := Array( 11,3 )
      oApp:oBrw1:aArray[1,1] := "Users"
      oApp:oBrw1:aArray[2,1] := "Tables"
      oApp:oBrw1:aArray[3,1] := "Indexes"
      oApp:oBrw1:aArray[4,1] := oApp:oBrw1:aArray[4,2] := oApp:oBrw1:aArray[4,3] := ""
      oApp:oBrw1:aArray[5,1] := "Time elapsed"
      oApp:oBrw1:aArray[6,1] := "Operations"
      oApp:oBrw1:aArray[7,1] := "KBytes sent"
      oApp:oBrw1:aArray[8,1] := "KBytes read"
      oApp:oBrw1:aArray[9,1] := "Transactions"
      oApp:oBrw1:aArray[10,1] := "CPU time"
      oApp:oBrw1:aArray[11,1] := "Mem HD/RAM"
      oApp:oBrw1:aArray[5,3] := ""
   ENDIF

   oApp:oBrw1:aArray[1,2] := aInfo[1]
   oApp:oBrw1:aArray[1,3] := aInfo[2]
   oApp:oBrw1:aArray[2,2] := aInfo[3]
   oApp:oBrw1:aArray[2,3] := aInfo[4]
   IF Len( aInfo ) >= 10 .AND. aInfo[9] != Nil .AND. aInfo[10] != Nil
      oApp:oBrw1:aArray[3,2] := aInfo[9]
      oApp:oBrw1:aArray[3,3] := aInfo[10]
   ELSE
      oApp:oBrw1:aArray[3,2] := oApp:oBrw1:aArray[3,3] := ""
   ENDIF
   IF LEN( aInfo3 ) >= 3
      nSec := aInfo3[3]
   ELSE
      nSec := Val( aInfo[5] )
   ENDIF
   nOpers := Val( aInfo[6] )
   nSent  := Val( aInfo[7] )
   nRead  := Val( aInfo[8] )
   nSec1 := Int( Seconds()/10 )
   IF nCurSec != nSec1
      nCurSec := nSec1
      IF ++nCurIndex > 6
         nCurIndex := 1
         lReady := .T.
      ENDIF
      aOpers[nCurIndex] := aSent[nCurIndex] := aRead[nCurIndex] := 0
   ENDIF
   aOpers[nCurIndex] += Iif( nLastOpers==0, 0, nOpers-nLastOpers )
   aSent[nCurIndex]  += Iif( nLastOpers==0, 0, nSent-nLastSent )
   aRead[nCurIndex]  += Iif( nLastOpers==0, 0, nRead-nLastRead )
   nLastOpers := nOpers
   nLastSent  := nSent
   nLastRead  := nRead

   oApp:oBrw1:aArray[6,2] := Str( aOpers[1]+aOpers[2]+aOpers[3]+aOpers[4]+aOpers[5]+aOpers[6]) + Iif(lReady,"","*")
   oApp:oBrw1:aArray[7,2] := Str( Int( ( aSent[1]+aSent[2]+aSent[3]+aSent[4]+aSent[5]+aSent[6] ) / 1000 ) ) + Iif(lReady,"","*")
   oApp:oBrw1:aArray[8,2] := Str( Int( ( aRead[1]+aRead[2]+aRead[3]+aRead[4]+aRead[5]+aRead[6] ) / 1000 ) ) + Iif(lReady,"","*")

   oApp:oBrw1:aArray[6,3] := aInfo[6]
   oApp:oBrw1:aArray[7,3] := Str(Int( nSent /1024 ))
   oApp:oBrw1:aArray[8,3] := Str(Int( nRead /1024 ))
   IF !Empty( aInfo[14] )
      oApp:oBrw1:aArray[9,2] := "All:  " + Ltrim( aInfo[14] )
      oApp:oBrw1:aArray[9,3] := "Bad:  " + Ltrim( Str( Val( aInfo[14] ) - Val( aInfo[15] ) ) )
   ELSE
      oApp:oBrw1:aArray[9,2] := oApp:oBrw1:aArray[9,3] := ""
   ENDIF
   nCpu := VAL( aInfo[12] )
   nDay := Int(nCpu/86400)
   nHour := Int((nCpu%86400)/3600)
   oApp:oBrw1:aArray[10,2] := Ltrim(Str(nDay)) + Iif(nDay==1," day "," days ") + ;
               Ltrim(Str(nHour))+Iif(nHour==1," hour "," hours ") + ;
               Ltrim(Str(Int((nCpu%3600)/60))) + " min"
   oApp:oBrw1:aArray[10,3] := aInfo[13]  // CPU Cores
   IF LEN( aInfo3 ) >= 3
      oApp:oBrw1:aArray[11,2] := STR( VAL( aInfo4[1] ) / 1024 / 1024, 8, 0 ) + " MB"
      oApp:oBrw1:aArray[11,3] := STR( VAL( aInfo4[4] ) / 1024, 7, 0) + " MB"
   ELSE
      oApp:oBrw1:aArray[11,2] := aInfo[16]
   ENDIF
   nDay := Int(nSec/86400)
   nHour := Int((nSec%86400)/3600)
   oApp:oBrw1:aArray[5,2] := Ltrim(Str(nDay)) + Iif(nDay==1," day "," days ") + ;
               Ltrim(Str(nHour))+Iif(nHour==1," hour "," hours ") + ;
               Ltrim(Str(Int((nSec%3600)/60))) + " min"

   IF oApp:nInfoType != 1
      oApp:nInfoType := 1
      oApp:oBrw1:aColumns := {}
      oApp:oBrw1:AddColumn( HColumn():New( "",{|v,o|o:aArray[o:nCurrent,1]},"C",15,0 ) )
      oApp:oBrw1:AddColumn( HColumn():New( "Current",{|v,o|o:aArray[o:nCurrent,2]},"C",24,0,,,DT_RIGHT ) )
      oApp:oBrw1:AddColumn( HColumn():New( "Max",{|v,o|o:aArray[o:nCurrent,3]},"C",12,0,,,DT_RIGHT ) )
      oApp:oBrw1:AddColumn( HColumn():New( "",{|v,o|" "},"C",1,0 ) )
      oApp:oBrw1:bPosChanged := Nil
      oApp:oBrw1:lChanged := .T.
      oApp:oBrw2:aColumns := Nil
      oApp:oBrw2:Refresh()
      oApp:oBrw1:nCurrent := oApp:oBrw1:rowPos := 1
   ENDIF
   oApp:oBrw1:Refresh()

Return .T.

Static Function UsersInfo()
Local aInfo, i, nUsers

   IF oApp:lSend
      oApp:nRequest = 2
      Return .T.
   ENDIF

   oApp:lSend := .T.
   IF ( aInfo := leto_MgGetUsers() ) == Nil
      oApp:lSend := .F.
      Disconnect()
      Return .F.
   ENDIF
   oApp:lSend := .F.

   oApp:oBtnLock:Hide()
   oApp:oBtnKill:Show()
   nUsers := Len( aInfo )
   oApp:oBrw1:aArray := Array( nUsers, 5 )
   FOR i := 1 TO nUsers
      oApp:oBrw1:aArray[i,4] := aInfo[i,1]
      oApp:oBrw1:aArray[i,1] := aInfo[i,2]
      oApp:oBrw1:aArray[i,2] := aInfo[i,3]
      oApp:oBrw1:aArray[i,3] := aInfo[i,4]
      oApp:oBrw1:aArray[i,5] := Padl(Ltrim(Str(Int((Val(aInfo[i,5])%86400)/3600))),2,'0') ;
         +":"+ Padl(Ltrim(Str(Int((Val(aInfo[i,5])%3600)/60))),2,'0') +":"+ ;
         Padl(Ltrim(Str(Int(Val(aInfo[i,5])%60))),2,'0')
   NEXT

   IF oApp:nInfoType != 2
      oApp:nInfoType := 2
      oApp:oBrw1:aColumns := {}
      oApp:oBrw1:AddColumn( HColumn():New( "Ip",{|v,o|o:aArray[o:nCurrent,1]},"C",16,0 ) )
      oApp:oBrw1:AddColumn( HColumn():New( "Host",{|v,o|o:aArray[o:nCurrent,2]},"C",18,0 ) )
      oApp:oBrw1:AddColumn( HColumn():New( "Module",{|v,o|o:aArray[o:nCurrent,3]},"C",18,0 ) )
      oApp:oBrw1:AddColumn( HColumn():New( "Timeout",{|v,o|o:aArray[o:nCurrent,5]},"C",15,0 ) )
      oApp:oBrw1:bPosChanged := Nil
      oApp:oBrw1:bPosChanged := {||Tables4User()}
      oApp:oBrw1:lChanged := .T.
      oApp:oBrw1:nCurrent := oApp:oBrw1:rowPos := 1
   ENDIF
   oApp:oBrw1:Refresh()
   Tables4User()

Return .T.

Static Function TablesInfo()
Local aInfo, i, nTables

   oApp:oBtnLock:Hide()
   oApp:oBtnKill:Hide()
   IF oApp:lSend
      oApp:nRequest = 3
      Return .T.
   ENDIF

   oApp:lSend := .T.
   IF ( aInfo := leto_MgGetTables() ) == Nil
      oApp:lSend := .F.
      Disconnect()
      Return .F.
   ENDIF

   oApp:lSend := .F.

   nTables := Len( aInfo )
   oApp:oBrw1:aArray := Array( nTables,2 )
   FOR i := 1 TO nTables
      oApp:oBrw1:aArray[i,2] := aInfo[i,1]
      oApp:oBrw1:aArray[i,1] := aInfo[i,2]
   NEXT

   IF oApp:nInfoType != 3
      oApp:nInfoType := 3
      oApp:oBrw1:aColumns := {}
      oApp:oBrw1:AddColumn( HColumn():New( "Name",{|v,o|o:aArray[o:nCurrent,1]},"C",32,0 ) )
      oApp:oBrw1:bPosChanged := {||Users4Table()}
      oApp:oBrw1:lChanged := .T.
      oApp:oBrw1:nCurrent := oApp:oBrw1:rowPos := 1
   ENDIF
   oApp:oBrw1:Refresh()
   Users4Table()

Return .T.

Static Function Users4Table()
Local aInfo, i, nUsers
  
   IF !Empty( oApp:oBrw1:aArray ) .AND. ;
         Len( oApp:oBrw1:aArray ) >= oApp:oBrw1:nCurrent .AND. ;
         Valtype( oApp:oBrw1:aArray[oApp:oBrw1:nCurrent] ) == "A" .AND. ;
         Len( oApp:oBrw1:aArray[oApp:oBrw1:nCurrent] ) >= 2
      oApp:lSend := .T.
      IF ( aInfo := leto_MgGetUsers( oApp:oBrw1:aArray[oApp:oBrw1:nCurrent,2] ) ) == Nil
         oApp:lSend := .F.
         Return .F.
      ENDIF
      oApp:lSend := .F.

      nUsers := Len( aInfo )
      oApp:oBrw2:aArray := Array( nUsers,3 )
      FOR i := 1 TO nUsers
         oApp:oBrw2:aArray[i,1] := aInfo[i,2]
         oApp:oBrw2:aArray[i,2] := aInfo[i,3]
         oApp:oBrw2:aArray[i,3] := aInfo[i,4]
      NEXT
   ELSE
      oApp:oBrw2:aArray := {}
   ENDIF

   IF oApp:oBrw2:aColumns == Nil .OR. Len(oApp:oBrw2:aColumns) == 1
      oApp:oBrw2:aColumns := {}
      oApp:oBrw2:AddColumn( HColumn():New( "Ip",{|v,o|o:aArray[o:nCurrent,1]},"C",18,0 ) )
      oApp:oBrw2:AddColumn( HColumn():New( "Host",{|v,o|o:aArray[o:nCurrent,2]},"C",18,0 ) )
      oApp:oBrw2:AddColumn( HColumn():New( "Module",{|v,o|o:aArray[o:nCurrent,3]},"C",18,0 ) )
      oApp:oBrw2:lChanged := .T.
   ENDIF
   oApp:oBrw2:Refresh()
Return Nil

Static Function Tables4User()
Local aInfo, i, nTables

   IF !Empty( oApp:oBrw1:aArray ) .AND. ;
         Len( oApp:oBrw1:aArray ) >= oApp:oBrw1:nCurrent .AND. ;
         Valtype( oApp:oBrw1:aArray[oApp:oBrw1:nCurrent] ) == "A" .AND. ;
         Len( oApp:oBrw1:aArray[oApp:oBrw1:nCurrent] ) >= 4
      oApp:lSend := .T.
      IF ( aInfo := leto_MgGetTables( oApp:oBrw1:aArray[oApp:oBrw1:nCurrent,4] ) ) == Nil
         oApp:lSend := .F.
         oApp:oBrw2:aArray := {}
      ELSE
         oApp:lSend := .F.

         nTables := Len( aInfo )
         oApp:oBrw2:aArray := Array( nTables,2 )
         FOR i := 1 TO nTables
            oApp:oBrw2:aArray[i,2] := aInfo[i,1]
            oApp:oBrw2:aArray[i,1] := aInfo[i,2]
         NEXT
      ENDIF
   ELSE
      oApp:oBrw2:aArray := {}
   ENDIF

   IF oApp:oBrw2:aColumns == Nil .OR. Len(oApp:oBrw2:aColumns) == 3
      oApp:oBrw2:aColumns := {}
      oApp:oBrw2:AddColumn( HColumn():New( "Name",{|v,o|o:aArray[o:nCurrent,1]},"C",24,0 ) )
      oApp:oBrw2:lChanged := .T.
   ENDIF
   oApp:oBrw2:Refresh()

Return Nil

Static Function LockConn()
   IF oApp:nInfoType == 1 .AND. hwg_MsgYesNo( IIF(oApp:lLocked, "Unlock server ?", "Lock server ?"))
      IF leto_LockConn( !oApp:lLocked )
         oApp:lLocked := !oApp:lLocked
         oApp:oBtnLock:SetText( IIF( oApp:lLocked, "Unlock", "Lock" ) )
      ENDIF
   ENDIF
Return Nil

Static Function KillUser()

   IF oApp:nInfoType == 2 .AND. hwg_MsgYesNo( "Really kill " + oApp:oBrw1:aArray[oApp:oBrw1:nCurrent,1] + " ?" )
      leto_mgKill( oApp:oBrw1:aArray[oApp:oBrw1:nCurrent,4] )
   ENDIF
Return Nil

Static Function TimerFunc()
Static nTM := 0

   nTM ++
   IF nTM >= Val( oApp:oGetRefr:GetText() )
      nTM := 0
      IF !oApp:lSend
         IF oApp:nInfoType == 1
            BasicInfo()
         ELSEIF oApp:nInfoType == 2
            UsersInfo()
         ELSEIF oApp:nInfoType == 3
            TablesInfo()
         ENDIF
      ENDIF
   ENDIF
Return Nil

Static Function GetCmdItem( cCommand,nPos1,nPos2 )
Local cSub := Substr( cCommand,nPos1 )

   IF ( nPos2 := At( ";",cSub ) ) != 0
      cSub := Left( cSub, nPos2 - 1 )
      nPos2 += nPos1 - 1
      Return cSub
   ENDIF
Return ""

Static Function GetIpFromPath( cPath )
Local nPos1, nPos2, nPos3, cSub

   IF Left( cPath,2 ) != "//" .OR. ( nPos1 := At( ":",cPath ) ) == 0
      Return Nil
   ENDIF
   cSub := Substr( cPath, nPos1 + 1 )
   nPos2 := At( "/",cSub )
   nPos3 := At( "\",cSub )
   IF nPos2 == 0 .AND. nPos3 == 0
      Return Nil
   ENDIF
   IF nPos2 == 0 .OR. ( nPos3 != 0 .AND. nPos3 < nPos2 )
      nPos2 := nPos3
   ENDIF
   nPos3 := Max( RAt( "/",cSub ), RAt( "\",cSub ) )

Return { Substr(cPath,3,nPos1-3), Val(Left(cSub,nPos2-1)), ;
   Iif(nPos3==0,"",Substr(cSub,nPos2,nPos3-nPos2+1)) }

Static Function ReadOptions( oApp )
Local oOptions := HXMLDoc():Read( "manage.xml" )
Local oNode, i1, cIp, cPort, cUser, cPass

   IF !Empty( oOptions ) .AND. !Empty( oOptions:aItems ) .AND. oOptions:aItems[1]:title == "init"
      FOR i1 := 1 TO Len( oOptions:aItems[1]:aItems )
         oNode := oOptions:aItems[1]:aItems[i1]
         IF oNode:title == "server"
            cIp := oNode:GetAttribute("ip")
            cPort := oNode:GetAttribute("port")
            cUser := oNode:GetAttribute("user")
            Aadd( oApp:aServers, cIp )
            Aadd( oApp:aParams, { Iif(cPort==Nil,"2812",cPort), cUser } )
         ENDIF
      NEXT
   ENDIF
   IF Empty( oApp:aServers )
      Aadd( oApp:aServers, "127.0.0.1" )
      Aadd( oApp:aParams, { "2812", Nil } )
   ENDIF
Return Nil

Static Function SaveOptions()
Local i, oXmlDoc := HXMLDoc():New( "windows-1251" ), oXMLNode, aAttr

   IF oApp:lUpdList
     oXMLNode := HXMLNode():New( "init" )
     oXmlDoc:Add( oXMLNode )
     FOR i := 1 TO Len( oApp:aServers )
        aAttr := { { "ip",oApp:aServers[i] }, { "port",oApp:aParams[i,1] } }
        IF !Empty( oApp:aParams[i,2] )
           Aadd( aAttr, { "user",oApp:aParams[i,2] } )
        ENDIF
        oXMLNode:Add( HXMLNode():New( "server", HBXML_TYPE_SINGLE, aAttr ) )
     NEXT
     oXmlDoc:Save( "manage.xml" )
   ENDIF
Return Nil

CLASS HApp

   DATA oMainWnd                     // Главное окно
   DATA oTool                        // Панель инструментов
   DATA oCombo
   DATA oGetPort
   DATA oGetRefr
   DATA oSayServer
   DATA oBtnGo
   DATA oBtnKill
   DATA oBtnLock
   DATA oBrw1
   DATA oBrw2
   DATA oSplit

   DATA aServers
   DATA aParams
   DATA nItemCurr  INIT 0
   DATA lUpdList   INIT .F.

   DATA oTimer
   DATA lSend      INIT .F.

   DATA nInfoType  INIT 0
   DATA nRequest   INIT 0

   DATA lLocked    INIT .F.

   METHOD New
ENDCLASS

METHOD New CLASS HApp

   ::aServers := {}
   ::aParams  := {}
   ReadOptions( Self )
Return Self

//#define SOCKET_BUFFER_SIZE  8192

Static Function WrLog( cText,cFile )
LOCAL nHand

  IF cFile == Nil
     cFile := "a.log"
  ENDIF
   
  IF !File( cFile )           
     nHand := Fcreate( cFile )
  ELSE                                     
     nHand := Fopen( cFile,1)
  ENDIF
  IF Ferror() != 0                           
     Return .F.
  ENDIF

  Fseek(nHand, 0,2 )
  Fwrite( nHand, cText + Chr(13) + Chr(10) )
  Fclose( nHand )

Return .T.

EXIT PROCEDURE EXIPROC

   IF oApp:lLocked
      leto_LockConn( .F. )
   ENDIF
   leto_DisConnect()
   IF oApp:oTimer != Nil
      oApp:oTimer:End()
   ENDIF
   SaveOptions()
RETURN
