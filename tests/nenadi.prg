
REQUEST RDDINFO, DBINFO, DBORDERINFO
REQUEST LETO_VARGET, LETO_VARGETCACHED
REQUEST LETO_BVALUE, LETO_BSEARCH
REQUEST LETO_UDF, LETO_UDFEXIST
REQUEST LETO_SETSEEKBUFFER, LETO_SETSKIPBUFFER
REQUEST LETO_ISFLTOPTIM


REQUEST LETO
REQUEST DBFNTX
#include "rddleto.ch"
#include "dbinfo.ch"

#define MAG_MAX  11

PROCEDURE main( cAddress )
 LOCAL i, ii, nTmp, cTmp, cMag, nSec
 LOCAL nTopScope, nBottomScope
 LOCAL aTmp := {}
 FIELD A_ID, K_ART

   ALTD()

   IF Empty( cAddress )
      cAddress := "//192.168.2.47:2812/"
   ELSE
      cAddress := "//" + cAddress + IIF( ":" $ cAddress, "", ":2812" )
      cAddress += IIF( Right( cAddress, 1 ) == "/", "", "/" )
   ENDIF

   // leto_Connect( cPath, /*user*/, /*pass*/, nTimeOut /*timeout*/, nHotBuf /*hot buffer*/ ) == -1
   IF leto_Connect( cAddress,,, ) < 0
      ALERT( "NO LETODB SERVER FOUND - ERROR: " + leto_Connect_Err( .T. ) )
      RDDSETDEFAULT( "DBFNTX" )
   ELSE
      IF leto_GetServerMode() < 3
         ALERT( "LETODBf NEED FOR THIS CONFIG OPTION: No_save_WA = 1" )
         QUIT
      ENDIF
      RDDSETDEFAULT( "LETO" )
      LETO_DBDRIVER( "DBFNTX" )  /* to choose your DBF driver independent of server default */
      LETO_TOGGLEZIP( 1 )        /* for commpressed traffic */
   ENDIF

/* --- create demo data --- */

   IF ! hb_dbexists( "Artikli" )
      IF DBCREATE( "Artikli",;
                   {;
                   {"A_AJM","C",3,0},;
                   {"A_AJMK","N",18,6},;
                   {"A_AKC","N",8,3},;
                   {"A_AMB","L",1,0},;
                   {"A_ATR","C",8,0},;
                   {"A_DOCTIP","N",10,0},;
                   {"A_DOPOBJ","N",10,0},;
                   {"A_DOPPAR","N",10,0},;
                   {"A_DOPREG","C",10,0},;
                   {"A_ECO","C",5,0},;
                   {"A_FIS","N",5,0},;
                   {"A_GAR","N",3,0},;
                   {"A_ID","N",10,0},;
                   {"A_JM","C",3,0},;
                   {"A_K1","N",12,6},;
                   {"A_K2","N",12,6},;
                   {"A_K3","N",12,6},;
                   {"A_KCENA","N",18,3},;
                   {"A_KON","C",10,0},;
                   {"A_KOSIR","N",6,0},;
                   {"A_KRIT","N",12,3},;
                   {"A_LNC","N",18,3},;
                   {"A_LOK","C",20,0},;
                   {"A_LPAR","N",10,0},;
                   {"A_LRAB","N",12,2},;
                   {"A_LT","N",5,2},;
                   {"A_MAXKOL","N",15,3},;
                   {"A_MAXR","N",6,2},;
                   {"A_MINKOL","N",15,3},;
                   {"A_MK","C",100,0},;
                   {"A_MKOD","C",10,0},;
                   {"A_MPC","N",12,2},;
                   {"A_MPOR","C",20,0},;
                   {"A_N","C",30,0},;
                   {"A_NA","C",7,0},;
                   {"A_NAB","L",1,0},;
                   {"A_NABCEN","N",18,3},;
                   {"A_NAD","N",10,0},;
                   {"A_NAM","C",30,0},;
                   {"A_NAR","N",15,3},;
                   {"A_NARDO","N",10,2},;
                   {"A_NAZIV","C",30,0},;
                   {"A_NAZIV2","C",60,0},;
                   {"A_NID","N",10,0},;
                   {"A_NKOL","N",12,3},;
                   {"A_NNABCEN","N",18,3},;
                   {"A_NPROCEN","N",18,3},;
                   {"A_NULA","L",1,0},;
                   {"A_OC","N",5,2},;
                   {"A_OIL","N",9,2},;
                   {"A_OJM","C",3,0},;
                   {"A_OJMK","N",12,4},;
                   {"A_OPERBEZ","N",12,3},;
                   {"A_OPERD1","D",8,0},;
                   {"A_OPERD2","D",8,0},;
                   {"A_OPERIZ","N",12,3},;
                   {"A_OPERMAX","N",12,3},;
                   {"A_OPERMIN","N",12,3},;
                   {"A_OPERPROS","N",12,3},;
                   {"A_OPERSA","N",12,3},;
                   {"A_OPERUL","N",12,3},;
                   {"A_OPTKOL","N",15,3},;
                   {"A_OZNDOB","C",20,0},;
                   {"A_PAK","N",18,3},;
                   {"A_PAKJM","C",3,0},;
                   {"A_PERBEZ","N",4,0},;
                   {"A_PERD1","D",8,0},;
                   {"A_PERD2","D",8,0},;
                   {"A_PERIZLAZ","N",12,3},;
                   {"A_PERMAX","N",12,3},;
                   {"A_PERMIN","N",12,3},;
                   {"A_PERPID","D",8,0},;
                   {"A_PERPROS","N",12,3},;
                   {"A_PERPU","N",12,3},;
                   {"A_PERPUD","D",8,0},;
                   {"A_PERSA","N",4,0},;
                   {"A_PERULAZ","N",12,3},;
                   {"A_PLAN","N",12,3},;
                   {"A_PMPC","N",12,2},;
                   {"A_POR","C",20,0},;
                   {"A_PPRO","N",6,2},;
                   {"A_PRO","N",10,0},;
                   {"A_PRO30","N",12,3},;
                   {"A_PRO7","N",12,3},;
                   {"A_PROCEN","N",18,3},;
                   {"A_REF","N",18,4},;
                   {"A_REFN","N",18,4},;
                   {"A_REFP","N",18,4},;
                   {"A_REZ","N",15,3},;
                   {"A_ROK","N",3,0},;
                   {"A_ROKT","D",8,0},;
                   {"A_SAS","L",1,0},;
                   {"A_SEL","L",1,0},;
                   {"A_SEZ","C",5,0},;
                   {"A_SIF","C",15,0},;
                   {"A_SKOL","N",12,3},;
                   {"A_SS","N",5,2},;
                   {"A_STAT","C",1,0},;
                   {"A_SUS","L",1,0},;
                   {"A_TA","C",1,0},;
                   {"A_TAKSA","N",18,6},;
                   {"A_TAR","C",15,0},;
                   {"A_TARID","N",10,0},;
                   {"A_TEZ","N",18,4},;
                   {"A_TEZB","N",12,4},;
                   {"A_TEZJM","C",3,0},;
                   {"A_TIP","C",5,0},;
                   {"A_TPAK","N",10,3},;
                   {"A_TRAZ","N",18,3},;
                   {"A_TTEZ","N",10,3},;
                   {"A_URL","C",100,0},;
                   {"A_UVO","N",10,0},;
                   {"A_VRS","N",10,0},;
                   {"A_WEB","L",1,0},;
                   {"A_ZAP","N",18,4},;
                   {"A_ZAPJM","C",3,0},;
                   {"A_ZEM","C",30,0},;
                   {"A_ZOSN","N",18,3};
                   },, .T., "ARTIKLI" )  // stay opened

         OrdCreate( "Art_Naz",, "Upper(A_NAZIV)" )
         OrdCreate( "Art_Id",,  "A_ID" )
         OrdCreate( "Art_Sif",, "Upper(A_SIF)" )
         OrdCreate( "Art_Vrs",, "STR(A_VRS) + Upper(A_NAZIV)" )
         OrdCreate( "Art_BK",,  "Upper(A_OZNDOB)" )
         OrdCreate( "Art_Nad",, "a_nad" )
         OrdListAdd( "Art_Id" )
         OrdSetFocus( "Art_Id" )  // select that order for following action in MagKol
      ENDIF
   ELSE  // open it just for creating MagKol
      dbUseArea( .T.,, "Artikli.dbf", "Artikli", .T., .F. )
      OrdListAdd( "Art_Id" )
   ENDIF

   IF ! hb_dbexists( "MagKol" )
      IF DBCREATE( "MagKol",;
                   {;
                   {"K_ART","N",10,0},;
                   {"K_JM","C",3,0},;
                   {"K_JMK","N",18,6},;
                   {"K_KOL","N",12,3},;
                   {"K_MAG","N",10,0},;
                   {"K_NABCEN","N",18,3},;
                   {"K_POR","C",20,0},;
                   {"K_PROCEN","N",18,3},;
                   {"K_REZ","N",18,3},;
                   {"K_REZ2","N",12,3};
                   },, .T., "MAGKOL" )
         OrdCreate( "Mag_M",,  "K_MAG" )
         OrdCreate( "Mag_AM",, "STR(K_ART,10,0) + STR(K_MAG,10,0)" )  // this be used as order during creating new
      ENDIF
   ENDIF

   IF ! hb_dbexists( "MAGACIN" )
      IF DBCREATE( "MAGACIN",;
                   {;
                   {"G_ADR1","C",30,0},;
                   {"G_ADR2","C",30,0},;
                   {"G_AJM","L",1,0},;
                   {"G_ALT","N",10,0},;
                   {"G_EXT","L",1,0},;
                   {"G_ID","N",10,0},;
                   {"G_JMBG","C",15,0},;
                   {"G_KEPU","C",10,0},;
                   {"G_KON","C",10,0},;
                   {"G_LK","C",12,0},;
                   {"G_NAP","C",35,0},;
                   {"G_NAZIV","C",30,0},;
                   {"G_NNC","L",1,0},;
                   {"G_OADR","C",25,0},;
                   {"G_OPS","N",10,0},;
                   {"G_PI","C",25,0},;
                   {"G_PP","L",1,0},;
                   {"G_PRO","N",10,0},;
                   {"G_RO","L",1,0},;
                   {"G_SEK","C",3,0},;
                   {"G_SKR","C",5,0},;
                   {"G_TEL","C",15,0},;
                   {"G_TIP","C",1,0},;
                   {"G_VAL","N",10,0};
                   },, .T., "MAGACIN" )
         OrdCreate( "Magacin",, "G_ID" )
         OrdCreate( "Mag_Naz",, "Upper(G_NAZIV)" )
      ENDIF
   ENDIF

   DbCloseAll()

   IF dbUseArea( .T.,, "MAGACIN.dbf", "MAGACIN", .T., .F. )
      OrdListAdd( "Magacin" )
      OrdListAdd( "Mag_Naz" )
      i := MAG_MAX - ( MAG_MAX - RECCOUNT() )
      nSec := hb_milliseconds()
      DbSetOrder( 2 )
      DO WHILE i < 11
         DbAppend()
         REPL G_ID    WITH RECNO(),;
              G_ADR1  WITH RString( 25 ),;
              G_ADR2  WITH RString( 20 ),;
              G_NAZIV WITH RString( 21 ) + STR( RECNO(), 4, 0 ),;
              G_TEL   WITH RNAD( 13 ),;
              G_VAL   WITH HB_RandomInt( 1, 10000 ),;
              G_RO    WITH .T.
         i++
      ENDDO
      ? "MAGACIN append", i, ( hb_milliseconds() - nSec ) / 1000
   ENDIF

   IF dbUseArea( .T.,, "Artikli.dbf", "Artikli", .T., .F. )
      OrdListAdd( "Art_Naz" )
      OrdListAdd( "Art_Id" )
      OrdListAdd( "Art_Sif" )
      OrdListAdd( "Art_Vrs" )
      OrdListAdd( "Art_BK" )
      OrdListAdd( "Art_Nad" )
      DbSetOrder( 2 )
      i := 50000 - ( 50000 - RECCOUNT() )

      nSec := hb_milliseconds()
      DbSetOrder( 2 )
      DO WHILE i < 50000
         DO WHILE .T.
            nTmp := HB_RandomInt( 1, 1000000 )
            IF ! DBSEEK( nTmp )
               EXIT
            ENDIF
         ENDDO
         AADD( aTmp, nTmp )

         DbAppend()
         REPL A_NAZIV  WITH RString( 21 ) + STR( RECNO(), 7,0 ),;
              A_NAZIV2 WITH RString( 30 ),;
              A_ID     WITH nTmp,;
              A_SIF    WITH RString( 15 ),;
              A_VRS    WITH HB_RandomInt( 1, 1000000 ),;
              A_OZNDOB WITH RString( 20 ),;
              A_MK     WITH RNAD( 50 ),;
              A_NAM    WITH RString( 20 ),;
              A_NAD    WITH HB_RandomInt( 1, 10000 ),;
              A_PERPUD WITH DATE(),;
              A_REF    WITH 1234567.34,;
              A_TAKSA  WITH 9876543.321
         i++
      ENDDO
      ?
      ? "Artikli append", i, ( hb_milliseconds() - nSec ) / 1000
      nSec := hb_milliseconds()
      DbGoTOp()
      DO WHILE ! EOF()
         DbSkip()
      ENDDO
      ? "        skip  ", i, ( hb_milliseconds() - nSec ) / 1000
      nSec := hb_milliseconds()
      FOR i := 1 TO 10
         OrdCreate( "Art_Vrs",, "STR(A_VRS) + Upper(A_NAZIV)" )
      NEXT
      ? " create index ", i - 1, ( hb_milliseconds() - nSec ) / 1000
      OrdListClear()
      OrdListAdd( "Art_Naz" )
      OrdListAdd( "Art_Id" )
      OrdListAdd( "Art_Sif" )
      OrdListAdd( "Art_Vrs" )
      OrdListAdd( "Art_BK" )
      OrdListAdd( "Art_Nad" )
      DbSetOrder( 2 )
   ENDIF

   IF dbUseArea( .T.,, "MagKol.dbf", "MagKol", .T., .F. )
      OrdListAdd( "Mag_AM" )
      OrdListAdd( "Mag_M" )
      i := 50000 - ( 50000 - ( RECCOUNT() / 5 ) )

      nSec := hb_milliseconds()
      DO WHILE i < 50000

         FOR ii := 1 TO 5
            DbAppend()

            REPL K_ART WITH aTmp[ i + 1 ],;
                 K_MAG WITH HB_RandomInt( 1, MAG_MAX ),;
                 K_KOL WITH HB_RandomInt( 0, 2 ),;
                 K_POR WITH RString( 10 ),;
                 K_JMK WITH HB_RandomInt( 0, 500000 )
         NEXT ii
         i++
      ENDDO
      ? "MagKol  append", INT( i * 5 ), ( hb_milliseconds() - nSec ) / 1000
      nSec := hb_milliseconds()
      DbGoTOp()
      DO WHILE ! EOF()
         DbSkip()
      ENDDO
      ? "        skip  ", INT( i * 5 ), ( hb_milliseconds() - nSec ) / 1000
      DBSETORDER( 1 )
      nSec := hb_milliseconds()
      FOR i := 1 TO 10
         OrdCreate( "Mag_M",,  "K_MAG" )
      NEXT
      ? " create index ", i - 1, ( hb_milliseconds() - nSec ) / 1000
      OrdListClear()
      OrdListAdd( "Mag_AM" )
      OrdListAdd( "Mag_M" )
   ENDIF

   
/* --- test data is filled up, now benchmark tests --- */


   aTmp := {}
   cTmp := ""
   cMag := ""
   nTopScope    := 9999999
   nBottomScope := 0

   // choose 3 random magacin, prepare a string for the filter
   
   FOR i := 1 TO 3
      DO WHILE .T.
         nTmp := HB_RandomInt( 1, MAG_MAX )
         IF ASCAN( aTmp, nTmp ) < 1
            AADD( aTmp, nTmp )
            IF nTmp < nTopScope
               nTopScope := nTmp
            ENDIF
            IF nTmp > nBottomScope
               nBottomScope := nTmp
            ENDIF
            EXIT
         ENDIF
      ENDDO

      cMag += IIF( EMPTY( cTmp ), "", "," ) + ALLTRIM( STR( nTmp ) )
      IF RDDSETDEFAULT() == "LETO"
         //cTmp += LETO_BVALUE( nTmp )
         cTmp += STR( nTmp )
      ELSE
         cTmp += STR( nTmp )
         //cTmp += IIF( EMPTY( cTmp ), "", "," ) + ALLTRIM( STR( nTmp ) )
      ENDIF
   NEXT i

   IF RDDSETDEFAULT() == "LETO"
      leto_VarSet( "FILTER", "MAGLIST", cTmp, LETO_VCREAT )
   ENDIF

   ?
   IF RDDSETDEFAULT() == "LETO"
      //? "MagKol YES/ NO filter:", "leto_BSearch( leto_VarGetCached(),K_MAG) .AND. K_KOL<>0"
      ? "MagKol YES/ NO filter:", "STR(K_MAG,10,0) $ leto_VarGetCached() .AND. K_KOL<>0"
   ELSE
      ? "MagKol YES/ NO filter:", "STR(K_MAG,10,0) $" + CHR(34) + cTmp + CHR(34) + " .AND. K_KOL<>0"
      //? "MagKol YES/ NO filter:", "ASCAN( {" + cTmp + "}, K_MAG ) > 0 .AND. K_KOL <> 0"
   ENDIF
   ? "            with MAGs:", cMag

   DbSelectArea( "Artikli" )
   OrdSetFocus( "Art_Id" )

   DbSelectArea( "MagKol" )
   DbSetOrder( 2 )
   DbSetRelation( "Artikli", {|| K_ART }, "{|| K_ART }" )
   SET( _SET_FORCEOPT, .T. )

   // try a filter in MagKol, 2 times without filter, one time solely the filter, one time filter *plus* scope 

   FOR i := 1 TO 4
      IF i % 2 == 0
         IF RDDSETDEFAULT() == "LETO"
            //DbSetFilter( ,"leto_BSearch( leto_VarGetCached(), K_MAG ) .AND. K_KOL<>0" )
            DbSetFilter( , "STR(K_MAG,10,0)$leto_VarGetCached() .AND. K_KOL<>0" )
         ELSE
            DbSetFilter( , "STR(K_MAG,10,0)$" + CHR(34) + cTmp + CHR(34) + " .AND. K_KOL<>0" )
            //DbSetFilter( ,"ASCAN({"+cTmp+"}, K_MAG )> 0 .AND. K_KOL<>0" )
         ENDIF
         IF i > 2
            ? "SCOPE"
            SET SCOPE TO nTopScope, nBottomScope
         ELSE
            ? "YES  "
         ENDIF
      ELSE
         DbClearFilter()
         IF i > 2
            SET SCOPE TO
         ENDIF
         ? " NO  "
      ENDIF
      nSec := hb_milliseconds()

      ii:= 0
      DBGOTOP()
      DO WHILE ! EOF()
         ii++
         DBSKIP()
      ENDDO

      ?? ii, ( hb_milliseconds() - nSec ) / 1000
   NEXT i

   // if this relation is not released here, we get a cyclic relation
   // shell be savely detected by LetoDBf --> RTE at client, but no server crash
   DbClearRelation()

   
/* --- version using a filter in two workareas, plus a relation -- full dynamic solution --- */

   /* default: server shell evaluate valid filter, but *without* ALIAS names */
   // RDDInfo( RDDI_OPTIMIZE, .T. )
   // SET( _SET_OPTIMIZE, .T. )   /* alternative */

   /* this will *force* the server to evaluate all valid filter *with* ALIAS names */  
   RDDInfo( RDDI_FORCEOPT, .T. )
   // SET( _SET_FORCEOPT, .T. )   /* alternative */

   DbSelectArea( "MagKol" )
   /* MagKol still have the filter active from test above */
   DbSetOrder( 2 )
#if 0  // version with OrdCreate()
   /* lAdditive (13.) == not close other orders, use this order to create new index */
   OrdCondSet( ,,,,,,,,,,,, .T. )
   /* new index order */
   OrdCreate( "Mag_A",, "K_ART" )
#else
   INDEX ON K_ART TO Mag_A ADDITIVE
#endif
   DbSetOrder( 3 )
   ?
   ? "MagKol active index key:", IndexKey()
   ? "       filter:", DbFilter()

   DbSelectArea( "Artikli" )
   /* the *forced to use* filter:  */
   SET FILTER TO MagKol->( DbSeek( Artikli->A_ID ) )
   /* this relation is needed to refresh workarea MagKol, because the active WA is Artikli */
   /* else evaluated filter at server will not refresh MagKol *at client side* */
   DbSetRelation( "MagKol", {|| A_ID } , "A_ID" )
   ? "Artikli Filter:", DbFilter(), "  FltOptim:", leto_IsFltOptim()
   ? "        relation: ", DbRelation( 1 )

   FOR i := 1 TO 2
      IF i % 2 == 0
         DbSetOrder( 1 )
         ? IndexKey()
      ELSE
         DbSetOrder( 2 )
         ? IndexKey()
      ENDIF

      nSec := hb_milliseconds()
      ii:= 0
      DBGOTOP()
      DO WHILE ! EOF()
         ii++
         DBSKIP()
      ENDDO
      ?? ii, ( hb_milliseconds() - nSec ) / 1000
   NEXT i

   ? "Press any key to browse ..."
   Inkey( 0 )

   DbGoTop()
   DbEdit( 1,0,MAXROW(),MAXCOL(), { "Artikli->A_ID", "Artikli->A_NAZIV", "MagKol->K_MAG","MagKolSum()","MagKol->K_ART" } )

   /* nicely reset all */
   DbClearRelation()
   DbClearFilter()
   RDDInfo( RDDI_FORCEOPT, .F. )



/* --- below a version using a conditional TEMPORARY index order plus a relation - not 'full dynamic --- */
/* --- the temporary index will only be actualized/ updated on data change in Artikli, not just by a K_KOL change */
/* --- convenient to use, as index automatically destroyed with workarea close, and exclusive! to the user who created it */

   ?
   ? "creating temporary conditional index"

   DbSelectArea( "MagKol" )
   /* relation need MagKol active with the new index order "Mag_A" */ 
   OrdSetFocus( "Mag_A" )  // A_ID

   DbSelectArea( "Artikli" )
   DbSetRelation( "MagKol", {|| A_ID } , "A_ID" )
   DbSetOrder( 2 )
   nSec := hb_milliseconds()
   /* lAdditive (13.) == not close other orders, lTemporary (18.) */ 
   OrdCondSet( "! MagKol->( EOF() )",,,,,,,,,,,, .T.,,,,, .T. )
   OrdCreate( "Art_Filter", "Art_Filter", "Upper(A_NAZIV)" )
   // INDEX ON Upper(A_NAZIV) TAG Art_Filter FOR ! MagKol->( EOF() ) ADDITIVE TEMPORARY
   OrdSetFocus( "Art_Filter" )
   ? DbOrderInfo( DBOI_KEYCOUNT ), ( hb_milliseconds() - nSec ) / 1000

   nSec := hb_milliseconds()
   DBGOTOP()
   DO WHILE ! EOF()
      DBSKIP()
   ENDDO
   ?? ( hb_milliseconds() - nSec ) / 1000

   WAIT
   CLS
   
   DbGoTop()
   DbEdit( 1,0, MAXROW(), MAXCOL(), { "Artikli->A_ID", "Artikli->A_NAZIV", "MagKol->K_MAG","MagKolSum()","MagKol->K_ART" } )

   /* reset relation */
   DbClearRelation()
   /* the temporary index "Art_Filter" ist automatically deleted with closing workarea */
   DbCloseArea( "Artikli" )

   WAIT
RETURN



/* --- some help functions --- */

FUNCTION MagKolSum
 LOCAL nArt := Artikli->A_ID
 LOCAL nSum := 0
 LOCAL nOldRec := 0

  DbSelectArea( "MagKol" )
  IF EOF()
     DbSeek( nArt )
     IF ! EOF()
        nOldRec := RecNo()
     ENDIF
  ELSE
     nOldRec := RecNo()
  ENDIF
  DO WHILE ! EOF() .AND. MagKol->K_ART == nArt
     nSum += MagKol->K_KOL
     DbSkip( 1 )
  ENDDO
  IF noldRec > 0
     DbGoto( nOldRec )
  ENDIF

  DbSelectArea( "Artikli" )
RETURN nSum


/* for random data */

STATIC FUNC RNAD( nLen )
   LOCAL cRet := ""
   LOCAL i

   FOR i := 1 TO nLen
      cRet += IIF( HB_RandomInt(0, 1) == 1, "1", " " )
   NEXT

RETU cRet

STATIC FUNC RString (nLen)
   STATIC cPool := ""
   LOCAL cRet
   LOCAL i

   IF EMPTY( cPool )
      FOR i := 1 TO 4096
         cPool += Chr( HB_RandomInt( 65, 126 ) )
      NEXT i
   ENDIF

   i := HB_RandomInt( 1, 4096 - nLen )
   cRet := SUBSTR( cPool, i, nLen )
RETURN cRet