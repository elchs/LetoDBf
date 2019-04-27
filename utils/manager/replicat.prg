#define SRV_TIMEOUT   42
/* #define __REPLICATE_DEBUG__   1 */

#include "inkey.ch"
#include "box.ch"


FUNCTION Main( cPrimary, cSecondary, cRestart )
   LOCAL lReset := .F.
   LOCAL nKey := 0
   LOCAL cSyncData, cSyncLog, nSyncSize, nSyncResult, nSynced, nBlocks, nErrors
   LOCAL cFile := "leto_syncdata.log"
   LOCAL nToggleMax, nToggler := 0
   LOCAL nPrimary, nSecondary
   LOCAL lLocked
   LOCAL cActions := "", cAct, nExcludeUser
   LOCAL i
   LOCAL nHnd

   ALTD()

   SET CURSOR OFF
   SETCOLOR( "W+/B,W+/G" )
   SetMode( 7, 42 )
   nToggleMax := MAXCOL()
   CLS
   IF EMPTY( cPrimary ) .OR. EMPTY( cSecondary )
      TimedALERT( " replicat[.exe] Primary_IP Secondary_IP", 5 )
      RETURN Nil
   ENDIF
   IF LEFT( cPrimary, 2 ) != "//"
      cPrimary := "//" + cPrimary
   ENDIF
   IF RIGHT( cPrimary,1 ) != "/"
      cPrimary += "/"
   ENDIF
   IF LEFT( cSecondary, 2 ) != "//"
      cSecondary := "//" + cSecondary
   ENDIF
   IF RIGHT( cSecondary,1 ) != "/"
      cSecondary += "/"
   ENDIF

   IF leto_Connect( cPrimary, /*cUser*/, /*cPasswd*/, SRV_TIMEOUT * 1000, , .T. ) == -1
      TimedALERT( "Primary Server: "  + leto_Connect_Err( .T. ), 3 )
      ErrorLevel( leto_Connect_Err( .F. ) )
      RETURN Nil
   ELSEIF leto_Connect( cSecondary, /*cUser*/, /*cPasswd*/, SRV_TIMEOUT * 1000, , .T. ) == -1
      TimedALERT( "Secondary Server: "  + leto_Connect_Err( .T. ), 3 )
      ErrorLevel( leto_Connect_Err( .F. ) )
      RETURN Nil
   ELSE
      Leto_SetCurrentConnection( cPrimary )
   ENDIF

   IF cPrimary == cSecondary
      lLocked := Leto_LockLock( .T., 10, 0 )
      IF ! lLocked
         TimedALERT( "Fail to lock primary server", 3 )
         QUIT
      ENDIF
   ENDIF

   IF ! EMPTY( cRestart )
      lReset := .T.
      IF VAL( cRestart ) > 0
         nExcludeUser := VAL( cRestart )
      ENDIF
      FOR i := 1 TO LEN( cRestart )
         cAct := SUBSTR( cRestart, i, 1 )
         IF cAct $ "+*ARMCDIOST"
            cActions += cAct
         ENDIF
      NEXT i
   ENDIF

   @ 0, 1 SAY "LetoDBf" COLOR "R/G"
   @ 0, 11 SAY "replication"
   @ 1, 1 SAY "Primary   " + cPrimary
   @ 2, 1 SAY "Secondary " + cSecondary

   nSynced := 0
   nBlocks := 0
   nErrors := 0

   DO WHILE nKey != K_ESC
      @ MAXROW(), nToggler++ SAY " "
      @ MAXROW(), IIF( nToggler > nToggleMax, nToggler := 0, nToggler ) SAY CHR( 64 )

      IF ( nPrimary := UserCount( cPrimary ) ) < 1
         TimedALERT( "primary server down" , 2 )
         EXIT
      ENDIF
      IF ( nSecondary := UserCount( cSecondary ) ) < 1
         TimedALERT( "secondary server down" , 2 )
         EXIT
      ENDIF
      @ 1, MAXCOL() - 7 SAY STR( nPrimary, 6, 0 )
      @ 2, MAXCOL() - 7 SAY STR( nSecondary, 6, 0 )

      cSyncData := Leto_LogRequest( IIF( lReset, 0, -1 ) )
      IF lReset
         lReset := .F.
      ENDIF
      nSyncSize := LEN( cSyncData )

      IF nSyncSize > 0
         @ MAXROW(), nToggler SAY CHR( 64 ) COLOR "G/B"
         nBlocks++
#ifdef __REPLICATE_DEBUG__
         nHnd := FCREATE( "good_syncdata." + hb_ntos( nBlocks ) )
         IF nHnd >= 0
            FWRITE( nHnd, cSyncData )
            FCLOSE( nHnd )
         ENDIF
#endif
         Leto_SetCurrentConnection( cSecondary )
         IF Leto_FileWrite( cFile, 0, cSyncData )
            nSyncResult := Leto_LogReplay( cFile, cActions,,, nExcludeUser )
            IF VALTYPE( nSyncResult ) != "N" .OR. nSyncResult >= 1
               IF VALTYPE( nSyncResult ) != "N"
                  TimedALERT( "TimeOut replicate secondary: " + hb_ntos( nSyncResult ), 2 )
                  nErrors++
               ELSE
                  TimedALERT( "Problem replicate secondary: " + hb_ntos( nSyncResult ), 2 )
                  nErrors += INT( nSyncResult )
                  cSyncLog := Leto_MgLog( NIL, 0, .T. )
                  nHnd := FCREATE( "bad_synclog." + hb_ntos( nBlocks ) )
                  IF nHnd >= 0
                     FWRITE( nHnd, cSyncLog )
                     FCLOSE( nHnd )
                  ENDIF
               ENDIF
#ifdef __REPLICATE_DEBUG__
               nHnd := FCREATE( "bad_syncdata." + hb_ntos( nBlocks ) )
               IF nHnd >= 0
                  FWRITE( nHnd, cSyncData )
                  FCLOSE( nHnd )
               ENDIF
#endif
            ELSE
               nSynced += nSyncSize
               @ 4, 3 SAY STR( nSynced, 15, 0 ) + " Bytes"
               @ 5, 3 SAY STR( nBlocks, 15, 0 ) + " Block"
#ifdef __REPLICATE_DEBUG__
               cSyncLog := Leto_MgLog( NIL, 0, .T. )
               IF LEN( cSyncLog ) > 0
                  nHnd := FCREATE( "good_synclog." + hb_ntos( nBlocks ) )
                  IF nHnd >= 0
                     FWRITE( nHnd, cSyncLog )
                     FCLOSE( nHnd )
                  ENDIF
               ENDIF
#endif
            ENDIF
            IF nErrors > 0
               @ 4, MAXCOL() - 11 SAY "Error: " + STR( nErrors, 3, 0 )
            ENDIF
            Leto_FErase( cFile )
         ENDIF
         Leto_SetCurrentConnection( cPrimary )
         IF nSyncSize < 8192  /* extra delay for less ongoing action */
            nKey := Inkey( 0.5 )
         ENDIF
      ELSE
         IF nPrimary < 2
            nKey := INKEY( 30 )
         ELSE
            nKey := INKEY( 3 )
         ENDIF
      ENDIF
      IF NEXTKEY() <> 0
         nKey := Inkey( 0.1 )
      ENDIF
   ENDDO

RETURN .T.


STATIC FUNCTION UserCount( cServer )
   LOCAL cOldConnect := Leto_SetCurrentConnection( cServer, .T. )
   LOCAL nUser := 0
   LOCAL aInfo := leto_MgGetInfo()

   IF VALTYPE( aInfo ) == "A"
      nUser := VAL( aInfo[ 1 ] )
   ENDIF
   Leto_SetCurrentConnection( cOldConnect )
RETURN nUser

STATIC FUNCTION TimedAlert( cText, nSec, cColor )
   LOCAL nX1 := ( ( MAXCOL() - LEN( cText ) ) / 2 ) - 2
   LOCAL nX2 := ( ( MAXCOL() - LEN( cText ) ) / 2 ) + LEN( cText ) + 2
   LOCAL nY1 := ( MAXROW() / 2 ) - 2
   LOCAL nY2 := ( MAXROW() / 2 ) + 2
   LOCAL cSave := SAVESCREEN( nY1, nX1, nY2, nX2 )
   LOCAL oldcolor := SETCOLOR( IIF( ! EMPTY( cColor ), cColor, "W+/R" ) )

   @ nY1, nX1, nY2, nX2 BOX B_DOUBLE + " "
   @ nY1 + 2, nX1 + 2 SAY cText
   IF nSec > 0
      DO WHILE Inkey( nSec ) >= K_MINMOUSE
      ENDDO
      RESTSCREEN( nY1, nX1, nY2, nX2, cSave )
   ENDIF
   SETCOLOR( oldcolor )
RETURN IIF( nSec > 0, .T., { nY1, nX1, nY2, nX2, cSave } )