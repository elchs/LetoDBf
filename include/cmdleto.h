/*
 * Header file for Leto command set
 *
 * Copyright 2014-2016 Rolf 'elch' Beckmann
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


/* request marked with '+' does not send positive feedback if second socket available */

/* note: this .h include file also used for server.prg */

#define LETOCMD_OFFSET     64   /* offset base value */
#define LETOCMD_SETLEN     64   /* length of arrays with functions/ descriptions */

/* still not used char */
/*  [ \ ] ^ _  91-95   */
/* avoid: ` == 96      */


/* without ulAreaID    */
/* reserved for @A - Z */
/* 0x40-0x5A // 64-90  */
/* FREE:   G H  @      */
/* 'S' is PRESEVED !   */

#define LETOCMD_admin      'A'
#define LETOCMD_creat_i    'B'
#define LETOCMD_close      'C' // +  note: also transmits a 0x ulAreaID
#define LETOCMD_drop       'D'
#define LETOCMD_exists     'E'
#define LETOCMD_file       'F'
#define LETOCMD_open_i     'I'
#define LETOCMD_intro      'J'
#define LETOCMD_udf_rel    'K'
#define LETOCMD_closall    'L' // +
#define LETOCMD_mgmt       'M'
#define LETOCMD_creat      'N'
#define LETOCMD_open       'O'
#define LETOCMD_ping       'P'
#define LETOCMD_quit       'Q'
#define LETOCMD_rename     'R'
#define LETOCMD_sql        'S'
#define LETOCMD_ta         'T'
#define LETOCMD_udf_fun    'U'
#define LETOCMD_var        'V'
#define LETOCMD_set        'W' // +
#define LETOCMD_stop       'X'
#define LETOCMD_rddinfo    'Y'
#define LETOCMD_zip        'Z'


/* with ulAreaID        */
/* a - z    { | } ~     */
/* 0x61-0x7e   97 -126  */
/* FREE:  v | } ~       */

#define LETOCMD_add        'a'
#define LETOCMD_dbi        'b'
#define LETOCMD_dboi       'c'
#define LETOCMD_scop       'd'
#define LETOCMD_seek       'e'
#define LETOCMD_flush      'f' // +
#define LETOCMD_goto       'g'
#define LETOCMD_group      'h'
#define LETOCMD_filt       'i' // clear filter: +
#define LETOCMD_sum        'j'
#define LETOCMD_unlock     'k' // +
#define LETOCMD_lock       'l'
#define LETOCMD_memo       'm' // put/add +
#define LETOCMD_udf_dbf    'n'
#define LETOCMD_ord        'o'
#define LETOCMD_pack       'p'
#define LETOCMD_sort       'q'
#define LETOCMD_islock     'r'
#define LETOCMD_skip       's'
#define LETOCMD_cmta       't'
#define LETOCMD_upd        'u' // not bAppend, else +
#define LETOCMD_dbeval     'v'
#define LETOCMD_rela       'w'
#define LETOCMD_cmtu       'x'
#define LETOCMD_rcou       'y'
#define LETOCMD_zap        'z'
#define LETOCMD_trans      '{'


/* - sub command values -  */

/* for memo                */
#define LETOSUB_add        'a'
#define LETOSUB_get        'g'
#define LETOSUB_put        'p'

/* vars -- also use 'get'  */
#define LETOSUB_set        's'
#define LETOSUB_inc        'i'
#define LETOSUB_dec        'd'
#define LETOSUB_del        'r'
#define LETOSUB_list       'l'

