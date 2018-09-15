/*
 *
 * modified PP rules for data processing commands:
 * # stringify! {for} {while} expressions
 * # pre-process __db*() to Leto_db*(), e.g. __dbEval => leto_DbEval(),
 *   partial using leto_Var() system for data exchange client <> server
 *
 * Copyright 2018 Rolf 'elch' Beckmann
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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


#ifndef LETO_STD_CH_
#define LETO_STD_CH_

#define LETO_DBEVAL_USE  1

#command COPY [TO <(f)>] [FIELDS <fields,...>] ;
              [FOR <for>] [WHILE <while>] [NEXT <next>] ;
              [RECORD <rec>] [<rest:REST>] [ALL] [VIA <rdd>] [CODEPAGE <cp>] => ;
         Leto_dbCopy( <(f)>, { <(fields)> }, ;
                      { <{for}>, <"for"> }, { <{while}>, <"while"> }, <next>, <rec>, <.rest.>, <rdd>,, <cp> )

#command APPEND [FROM <(f)>] [FIELDS <fields,...>] ;
                [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] [VIA <rdd>] ;
                [CODEPAGE <cp>] => ;
         Leto_dbApp( <(f)>, { <(fields)> }, ;
                     { <{for}>, <"for"> }, { <{while}>, <"while"> }, <next>, <rec>, <.rest.>, <rdd>,, <cp> )

#command SORT [TO <(f)>] [ON <fields,...>] ;
              [FOR <for>] [WHILE <while>] [NEXT <next>] ;
              [RECORD <rec>] [<rest:REST>] [ALL] [VIA <rdd>] ;
              [CODEPAGE <cp>] => ;
         Leto_dbSort( <(f)>, { <(fields)> }, ;
                      { <{for}>, <"for"> }, { <{while}>, <"while"> }, <next>, <rec>, <.rest.>, <rdd>,, <cp> )

#command JOIN [WITH <alias>] [TO <f>] [FIELDS <fields,...>] [FOR <for>] ;
              [VIA <rdd>] [CODEPAGE <cp>]  [<tmp: TEMPORARY>] => ;
         Leto_dbJoin( <(alias)>, <(f)>, { <(fields)> }, ;
                      { <{for}>, <"for"> }, [<rdd>],, [<cp>], [<.tmp.>] )

#command TOTAL [TO <(f)>] [ON <key>] [FIELDS <fields,...>] ;
               [FOR <for>] [WHILE <while>] [NEXT <next>] ;
               [RECORD <rec>] [<rest:REST>] [ALL] [VIA <rdd>] ;
               [CODEPAGE <cp>] => ;
         Leto_dbTotal( <(f)>, { <{key}>, <"key"> }, { <(fields)> }, ;
                       { <{for}>, <"for"> }, { <{while}>, <"while"> }, <next>, <rec>, <.rest.>, <rdd>,, <cp> )

/* need flocked or exclusive WA */
#command UPDATE FROM <(alias)> [ON <key>] [<rand:RANDOM>] ;
                [REPLACE <f1> WITH <x1> [, <fN> WITH <xN>]] => ;
         Leto_dbUpdate( <alias>, { <{key}>, <"key"> }, <.rand.>, ;
                        { #<x1> [, #<xN> ] }, { <"f1"> [, <"fN"> ] } )

/* note: also called from __dbTotal() */
#xtranslate __DBTRANS( [<x,...>] )  => LETO_DBTRANS( <x> )
#xtranslate __DBCOPY( [<x,...>] )   => LETO_DBCOPY( <x> )
#xtranslate __DBAPP( [<x,...>] )    => LETO_DBAPP( <x> )
#xtranslate __DBSORT( [<x,...>] )   => LETO_DBSORT( <x> )
/* note: positioned at new WA in case of success */
#xtranslate __DBJOIN( [<x,...>] )   => LETO_DBJOIN( <x> )
#xtranslate __DBTOTAL( [<x,...>] )  => LETO_DBTOTAL( <x> )

#xtranslate HB_DBCREATETEMP( [<cAlias>], <aStruct>, [<cRDD>], [<cCdp>], [<nConnection>] ) ;
            => Leto_DbCreateTemp( "", <aStruct>, "LETO", .T., <cAlias>, NIL, <cCdp>, <nConnection>  )

/* need to link occasionally called internal by above */
 * REQUEST HB_DBCREATETEMP, __DBTOTAL, RDDINFO */



#ifdef LETO_DBEVAL_USE

/* versions with given <for> or <while> stringyfied for leto_DbEval() */
 * if table is ! flocked/ not exclusive, needs RDDI_AUTOLOCK .T. for Rlock() */

/* REQUEST LETO_VARSET, LETO_VARGET, LETO_VARINCR */

#xtranslate DBEVAL( [<x,...>] )     => LETO_DBEVAL( <x> )

#command REPLACE [<f1> WITH <x1> [, <fN> WITH <xN>]] ;
                 [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                 [RECORD <rec>] [<rest:REST>] [ALL] [<descend:DESC,DESCENDING>] => ;
         leto_dbEval( "{|| FIELDPUT('"+<"f1">+"',"+<"x1">+")"[+", FIELDPUT('"+<"fN">+"',"+<"xN">+")"]+" }", ;
                      { <{for}>, <"for"> }, { <{while}>, <"while"> }, <next>, <rec>, <.rest.>,, .T., <.descend.>, .F. )
#command REPLACE <f1> WITH <v1>[, <fN> WITH <vN>] => ;
         _FIELD-><f1> := <v1> [; _FIELD-><fN> := <vN>]

#command DELETE [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] [<descend:DESC,DESCENDING>] [INTO <v>] => ;
         [ <v> := ];
         leto_dbEval( "{|n| dbDelete(), n }", { <{for}>, <"for"> }, { <{while}>, <"while"> }, <next>, <rec>, <.rest.>,, .T., <.descend.>, .F. )
#command DELETE =>  dbDelete()

#command RECALL [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] [<descend:DESC,DESCENDING>] [INTO <v>] => ;
         [ <v> := ];
         leto_dbEval( "{|n| dbRecall(), n }", { <{for}>, <"for"> }, { <{while}>, <"while"> }, <next>, <rec>, <.rest.>,, .T., <.descend.>, .T. )
#command RECALL =>  dbRecall()

#command COUNT [[INTO][TO] <v>] ;
               [FOR <for>] [WHILE <while>] [NEXT <next>] ;
               [RECORD <rec>] [<rest:REST>] [ALL] [<descend:DESC,DESCENDING>] => ;
         [ <v> := ];
         leto_dbEval( "{|n| n }", { <{for}>, <"for"> }, { <{while}>, <"while"> }, <next>, <rec>, <.rest.>,, .F., <.descend.> )


/* using leto_VarIncr() as private auto-create-delete '3' ( LETO_VCREAT | LETO_VOWN )
 * optional 'ON <result>' executed additional for array-values in <cVar>, i.e.: 'ON RecNo()' */
#command SUM [ <x1>[, <xN>]  TO  <v1> [, <vN>]] [ON <result>] ;
             [FOR <for>] [WHILE <while>] [NEXT <next>] ;
             [RECORD <rec>] [<rest:REST>] [ALL] [<descend:DESC,DESCENDING>] ;
             [INTO <cVar>] => ;
         [ <cVar> := ];
         Leto_dbEval( "{|| Leto_VarIncr( 'MySUM', '"+#<v1>+"', 3, "+<"x1">+" )"[+", Leto_VarIncr( 'MySUM', '"+#<vN>+"', 3, "+<"xN">+" )"][+", "+<"result">]+" }",;
                      { <{for}>, <"for"> }, { <{while}>, <"while"> }, <next>, <rec>, <.rest.>, <.cVar.>, .F., <.descend.> );;
         <v1> := Leto_VarGetSave( "MySUM", #<v1> , 0 );;
         [ <vN> := Leto_VarGetSave( "MySUM", #<vN>, 0 ) ; ];
         Leto_VarDel( "MySUM", #<v1> );;
         [ Leto_VarDel( "MySUM", #<vN> ) ]

#command AVERAGE [ <x1> [, <xN>]  TO  <v1>[, <vN>]] [ON <result>] ;
                 [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                 [RECORD <rec>] [<rest:REST>] [ALL] [<descend:DESC,DESCENDING>] ;
                 [INTO <cVar>]  => ;
         [ <cVar> := ];
         Leto_dbEval( "{|nRec| Leto_VarSet( 'MyAVG','__AVG', nRec, 3 ), Leto_VarIncr( 'MyAVG', '"+#<v1>+"', 3, "+<"x1">+" )"[+", Leto_VarIncr( 'MyAVG', '"+#<vN>+"', 3, "+<"xN">+" )"][+", "+<"result">]+" }", ;
                      { <{for}>, <"for"> }, { <{while}>, <"while"> }, <next>, <rec>, <.rest.>, <.cVar.>, .F., <.descend.> );;
         <v1> := Leto_VarGetSave( "MyAVG",#<v1>, 0 ) / Leto_VarGetSave( "MyAVG", "__AVG", 1 );;
         [ <vN> := Leto_VarGetSave( "MyAVG",#<vN>, 0 ) / Leto_VarGetSave( "MyAVG", "__AVG", 1 ) ; ];
         Leto_VarDel( "MyAVG", #<v1> );;
         [ Leto_VarDel( "MyAVG", #<vN> ) ; ];
         Leto_VarDel( "MySUM", '__AVG' )


#if 1
#command SELECT [<all:*>] [ <x1> [AS <v1>] [, <xN> [AS <vN>] ]] ;
                FROM [<alias1> [AS <xalias1>] [[, <aliasN> [AS <xaliasN>]]]] [ORDER BY <order1> [<descend:DESC,DESCENDING>][, <orderN>]] [WHERE <for1>[, <forN>]] ;
                [JOIN <chield1> [AS <calias1>] ON [<scoped:SCOPED> ] <relation1> [, <chieldN> [AS <caliasN>] ON <relationN>] ] ;
                [WHILE <while>] [LIMIT <records>] [OFFSET <offset>] ;
                [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] ;
                INTO <cVar>  => ;
         [ DbSelectArea( SELECT( <(aliasN)> ) );;
              OrdSetFocus( <(orderN)> );;
              DbSetFilter( <{forN}>, <"forN"> ); ];
         DbSelectArea( SELECT( <(alias1)> ) );;
              [ OrdSetFocus( <(order1)> ); ];
              [ DbSetFilter( <{for1}>, <"for1"> ); ];
              [ DbSetRelation( <"chield1">, <{relation1}>, <"relation1">, <.scoped.>, <(calias1)> ); ];
                 [ DbSetRelation( <"chieldN">,<{relationN}>, <"relationN">,, <(caliasN)> ); ];
         [ <cVar> := ];
         Leto_dbEval( "{|n| { "[+<"all">][+"'"+#<v1>+"' => "+<"x1">][+", '"+#<vN>+"' => "+<"xN">]+" } }",;
                       [{|,n| n \>= <offset> }], <"while">, <records><next>, <rec>, <.rest.>, <.cVar.>, .F., <.descend.> );;
         [ DbClearRelation( <"relation1"> ); ];
         [ DbClearFilter( <"for1">  ); ];
         [ DbSelectArea( SELECT( <(aliasN)> ) );;
              DbClearFilter( <"forN">  ); ]
#endif

#endif  /* LETO_DBEVAL_USE */


#endif  /* LETO_STD_CH_ */

