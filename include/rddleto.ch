/*
 * Header file for Leto RDD
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
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

#ifndef LETO_CH_
#define LETO_CH_

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

#define LETO_CDX              0
#define LETO_NTX              1

/* LETO_VAR*() flags */
#define LETO_VCREAT           1
#define LETO_VOWN             2
#define LETO_VDENYWR          4
#define LETO_VDENYRD          8

#define LETO_VPREVIOUS        1

/* LetoDBf specific rddinfo() */
#define RDDI_REFRESHCOUNT     101
#define RDDI_BUFKEYNO         102
#define RDDI_BUFKEYCOUNT      103
#define RDDI_VERSION          104
#define RDDI_DEBUGLEVEL       110

/* server config options */
#define LETOOPT_cDataPath      1
#define LETOOPT_nIndexType     2
#define LETOOPT_lFilefunc      3
#define LETOOPT_lAnyExt        4
#define LETOOPT_lPass4l        5
#define LETOOPT_lPass4m        6
#define LETOOPT_lPass4d        7
#define LETOOPT_cAccPath       8
#define LETOOPT_lCrypt         9
#define LETOOPT_lShare        10
#define LETOOPT_lNoSaveWA     11
#define LETOOPT_nVarMax       12
#define LETOOPT_nVarlenmax    13
#define LETOOPT_nCacherecords 14
#define LETOOPT_nTableMax     15
#define LETOOPT_nUserMax      16
#define LETOOPT_nDebugLevel   17
#define LETOOPT_lOptimize     18
#define LETOOPT_nAutoOrder    19
#define LETOOPT_nMemotype     20
#define LETOOPT_lForceOpt     21
#define LETOOPT_nLockCheme    22
#define LETOOPT_lUdfEnable    23
#define LETOOPT_nMemoBSize    24
#define LETOOPT_bLowerPath    25
#define LETOOPT_pTrigger      26
#define LETOOPT_pPendTrigger  27
#define LETOOPT_bHardCommit   29

#endif /* LETO_CH_ */
