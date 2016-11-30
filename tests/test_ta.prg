REQUEST LETO
REQUEST rddinfo
 
Function Main( cPath )
 LOCAL nRec
 LOCAL nTrans
 LOCAL lRet := .T.
 LOCAL nSec
 Field NORD, DORD, NPROD, SUMMA, NORM

   RDDSETDEFAULT( "LETO" )

   ALTD()
   IF Empty( cPath )
      cPath := "//127.0.0.1:2812/"
   ELSE
      cPath := "//" + cPath + IiF( ":" $ cPath, "", ":2812" )
      cPath += Iif( Right(cPath,1) == "/", "", "/" )
   ENDIF
   ? "Start"
   dbCreate( cPath + "nakl1", { {"NORD","N",10,0},{"DORD","D",8,0},{"SUMMA","N",12,2},{"NORM","M",10,0} } )
   dbCreate( cPath + "nakl2", { {"NORD","N",10,0},{"DORD","D",8,0},{"NPROD","N",3,0},{"SUMMA","N",12,2},{"NORM","M",10,0} } )
   ? "Files has been created"

   use ( cPath+"nakl1" ) Shared New
   index on Dtos(DORD)+Str(NORD,10,0) TAG DATA
   use ( cPath+"nakl2" ) Shared New
   index on Dtos(DORD)+Str(NORD,10,0)+Str(NPROD,3,0) TAG DATA
   ? "Files has been opened and indexed"

   // ? " compressed network traffic ", Leto_ToggleZip( 1 )

   ? "processing 500 transactions, 250 intentional rollbacks"
   ? ""
   nSec := hb_milliseconds()
   FOR nTrans := 1 TO 250
      nRec := AddNakl( nTrans, Date(), { 1400.5, 28632.28, 800.51 } )  /* 30833.29 */
      IF ! ChkNakl( nRec, nTrans, Date(), { 1400.5, 28632.28, 800.51 } )
         ? "internal Transaction problem "
         lRet := .F.
      ENDIF

      //? "Records has been added"
      ?? "+"

      select NAKL2
      if dbSeek( Dtos(Date())+Str(nTrans,10)+Str(2,3) )
         if ChangeNakl( 35688.24 )
            //? "Records has been changed"
            ?? "+"
         else
            ? "Failure - Rollback"
            lRet := .F.
         endif

         if NoChangeNakl( 35688.24 )
            //? "Rollback successfull"
            ?? "-"
         else
            ? "Failure in Rollback"
            lRet := .F.
         endif
      endif
      IF ! lRet
         EXIT
      ENDIF
   NEXT nTrans

   ? "Done", STR( ( hb_milliseconds() - nSec ) / 1000, 7, 2 ), "s"
   ?

   DbCloseAll()
   WAIT

   IF hb_dbdrop(cPath+"nakl1") .AND. hb_dbdrop(cPath+"nakl2")
      ? "files have been successful dropped"
   ELSE
      ? "Failure: files are NOT dropped"
      WAIT
   ENDIF

Return Nil

Function AddNakl( n_ord, d_ord, aSumm )
Local i, sumAll := 0.00
Local nRec := 0
Field NORD, DORD, SUMMA, NORM

   leto_BeginTransaction()

   select NAKL2

   for i := 1 to Len( aSumm )
      append blank
      replace NORD with n_ord, DORD with d_ord, NPROD with i, SUMMA with aSumm[i],;
              NORM with "elk-mtest" + STR( n_ord, 10, 0 )
      sumAll += aSumm[i]
      nRec++
   next

   select NAKL1
   append blank
   replace NORD with n_ord, DORD with d_ord, SUMMA with sumAll, NORM with "elk-test"  + STR( n_ord, 10, 0 )

   leto_CommitTransaction()

Return nRec


Function ChkNakl( nRec, n_ord, d_ord, aSumm )
Local i, sumAll := 0.00
Local lRet
Field NORD, DORD, SUMMA, NORM

   select NAKL2
   lRet := DbSeek( Dtos(DORD)+Str(NORD,10,0)+Str(1,3,0), .T. )
   IF lRet
      for i := 1 to nRec
         IF DoRlock( 3 )
            IF NORD != n_ord .OR. DORD != d_ord .OR. SUMMA != aSumm[ i ] .OR. NORM != "elk-mtest" + STR( n_ord, 10 ,0 )
               lRet := .F.
            ENDIF
            DBUNLOCK()
         ENDIF
         sumAll += aSumm[i]
         DbSkip(1)
      next
   ENDIF

   IF lRet
      select NAKL1
      lRet := DbSeek( Dtos(DORD)+Str(NORD,10,0) )
      IF lRet
        IF NORD != n_ord .OR. DORD != d_ord .OR. SUMMA != ROUND( sumAll, 2 ) .OR. NORM != "elk-test" + STR( n_ord, 10, 0 )
          lRet := .F.
        ENDIF
      ENDIF
   ENDIF

Return lRet


Function ChangeNakl( nSummNew )
Local nDelta
Local lRet := .T.
Field NORD, DORD, NPROD, SUMMA, NORM

   leto_BeginTransaction()

   select NAKL1
   IF ! dbSeek( Dtos( NAKL2->DORD ) + STR( NAKL2->NORD, 10, 0 ) ) .OR. ! DoRlock( 3 )
      leto_Rollback(.F.)
      Return .F.
   ENDIF

   select NAKL2
   /* test file lock, should keep set after commit */
   IF DoFlock( 3 )
      nDelta := nSummNew - SUMMA
      replace SUMMA with nSummNew, NORM with "elch tested"
   ELSE
      leto_Rollback()
      Return .F.
   ENDIF

   select NAKL1
   nSummNew := SUMMA
   replace SUMMA with SUMMA + nDelta, NORM with "elch tested"

   leto_CommitTransaction()

   IF SUMMA != nSummNew + nDelta .OR. NORM != "elch tested"
      lRet := .F.
   ENDIF
   DBUNLOCK()

   /* check existent file lock, will crash if not */
   select NAKL2
   replace NORM with "elch tested filelock"
   DBUNLOCK()

Return lRet


Function NoChangeNakl( nSummNew )
Local nDelta
Local lRet := .T.
Local nTmp
Field NORD, DORD, NPROD, SUMMA, NORM

   leto_BeginTransaction()

   select NAKL1
   if !dbSeek( Dtos(NAKL2->DORD)+STR(NAKL2->NORD,10,0) ) .or. !DoRlock( 3 )
      leto_Rollback(.F.)
      Return .F.
   endif

   select NAKL2
   if DoRlock( 3 )
      nTmp := SUMMA
      nDelta := nSummNew - SUMMA
      replace SUMMA with nSummNew, NORM with "no_elch"
   endif

   select NAKL1
   replace SUMMA with SUMMA + nDelta, NORM with "no_elch"

   leto_Rollback()

   select NAKL2
   IF DoRlock( 3 )
      if SUMMA != ROUND( nTmp, 2 ) .OR. NORM != "elch tested"
        lRet := .F.
      endif
      DbUnlock()
   ENDIF

Return lRet


Function DoRLock( n )
Local i := 0

   do while i < n
      if Rlock()
         exit
      endif
      Inkey( 0.1 )
      i ++
   enddo

Return ( i < n )

Function DoFLock( n )
Local i := 0

   do while i < n
      if Flock()
         exit
      endif
      Inkey( 0.1 )
      i ++
   enddo

Return ( i < n )

