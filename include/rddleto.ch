/*
 * Header file for Leto RDD
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 *           2017 Rolf 'elch' Beckmann ( additions )
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


/* ! also used in C source ! */

#ifndef RDDLETO_CH_
#define RDDLETO_CH_


#ifndef LETO_DEFAULT_PORT
   #define LETO_DEFAULT_PORT  2812
#endif

/* login error conditions */
#define LETO_ERR_CONNECT      1
#define LETO_ERR_LOGIN        2
#define LETO_ERR_ACCESS       3
#define LETO_ERR_RECV         4
#define LETO_ERR_SEND         5
#define LETO_ERR_MANY_CONN    6
#define LETO_ERR_SOCKET       7
#define LETO_ERR_PROTO        8
#define LETO_ERR_LOCKED       9
#define LETO_ERR_RESTORE     10

#define LETO_DBF
#define LETO_CDX              0
#define LETO_NTX              1

/* LETO_VAR*() flags */
#define LETO_VCREAT           1
#define LETO_VOWN             2
#define LETO_VDENYWR          4
#define LETO_VDENYRD          8
#define LETO_VPREVIOUS       16
#define LETO_VNOCREAT        64

/* C-level var types */
#define LETOVAR_LOG          '1'
#define LETOVAR_NUM          '2'
#define LETOVAR_STR          '3'
#define LETOVAR_ARR          '4'
#define LETOVAR_DAT          '5'

#define LETO_VPREFIX          "S_"

/* LetoDBf specific rddinfo() */
#define RDDI_REFRESHCOUNT     101
#define RDDI_BUFKEYNO         102
#define RDDI_BUFKEYCOUNT      103
#define RDDI_VERSION          104
#define RDDI_DEBUGLEVEL       110
#define RDDI_LOCKTIMEOUT      111
#define RDDI_CLEARBUFFER      112
#define RDDI_DBEVALCOMPAT     113

#define DBI_BUFREFRESHTIME    1001
#define DBI_CLEARBUFFER       1002
#define DBI_DBS_COUNTER       1003
#define DBI_DBS_STEP          1004
#define DBI_AUTOREFRESH       1005
#define DBI_CHILDPARENT       1006

/* server config options */
#define LETOOPT_DATAPATH      1   // C
#define LETOOPT_INDEXTYPE     2   // N
#define LETOOPT_FILEFUNC      3   // L
#define LETOOPT_ANYEXT        4   // L
#define LETOOPT_PASS4L        5   // L
#define LETOOPT_PASS4M        6   // L
#define LETOOPT_PASS4D        7   // L
#define LETOOPT_ACCPATH       8   // C
#define LETOOPT_CRYPT         9   // L
#define LETOOPT_SHARETABLES  10   // L
#define LETOOPT_NOSAVEWA     11   // L
#define LETOOPT_VARMAX       12   // N
#define LETOOPT_VARLENMAX    13   // N
#define LETOOPT_CACHERECORDS 14   // N
#define LETOOPT_TABLEMAX     15   // N
#define LETOOPT_USERMAX      16   // N
#define LETOOPT_DEBUGLEVEL   17   // N
#define LETOOPT_OPTIMIZE     18   // L
#define LETOOPT_AUTOORDER    19   // N
#define LETOOPT_MEMOTYPE     20   // N
#define LETOOPT_FORCEOPT     21   // L
#define LETOOPT_LOCKCHEME    22   // N
#define LETOOPT_UDFENABLED   23   // L
#define LETOOPT_MEMOSIZE     24   // N
#define LETOOPT_LOWERPATH    25   // L
#define LETOOPT_TRIGGER      26   // P
#define LETOOPT_HARDCOMMIT   27   // L

/* determine if compiler behave case sensitive */
#define CASESENSITIVE
#ifdef CaseSensitive
   #undef CASESENSITIVE
#endif

/* redirect for 4 options handled in LETO_SET(), others forward to SET() */
#ifdef CASESENSITIVE
   #define set( _HB_SETTING, XSET )   LETO_SET( _HB_SETTING, XSET )
   #define Set( _HB_SETTING, XSET )   LETO_SET( _HB_SETTING, XSET )
#endif
#define SET( _HB_SETTING, XSET )   LETO_SET( _HB_SETTING, XSET )


#ifdef __XHARBOUR__
   #ifndef DBS_COUNTER
      #define DBS_COUNTER       102
   #endif
   #ifndef DBS_STEP
      #define DBS_STEP          103
   #endif
#endif

#endif /* RDDLETO_CH_ */
