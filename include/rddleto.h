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

#include "hbapi.h"
#include "hbinit.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbapirdd.h"
#include "hbdbferr.h"
#include "hbdate.h"
#include "hbdefs.h"
#include "hbgtcore.h"
#include "hbset.h"
#include "hbsocket.h"
#include "hbstack.h"
#ifndef __XHARBOUR__
   #include "hbthread.h"
#endif
#include "hbatomic.h"
#include "hbvm.h"
#include "hbsetup.h"
#ifndef USE_LZ4
   #include "hbzlib.h"
#endif

#include "cmdleto.h"
#include "funcleto.h"
#include "letocl.h"

#include "dbinfo.ch"
#ifdef __HARBOUR30__
   #define HB_SERIALIZE_NUMSIZE   HB_TRUE
#else
   #include "hbserial.ch"
#endif
#include "rddsys.ch"

#include "rddleto.ch"

HB_EXTERN_BEGIN

typedef struct _LETOTAGEXTRAINFO
{
   PHB_ITEM   pKeyItem;
   PHB_ITEM   pTopScope;
   PHB_ITEM   pBottomScope;
   HB_USHORT  uiFCount;                /* index fields count */
   HB_USHORT * puiFields;               /* index fields array */

} LETOTAGEXTRAINFO;

/*
 *  LETO WORKAREA
 *  -------------
 *  The local Workarea Structure of Harbour remote Database Server RDD
 *
 */

typedef struct _LETOAREA_
{
   AREA area;

   /*
    *  LETO's additions to the workarea structure
    *
    *  Warning: The above section MUST match WORKAREA exactly!  Any
    *  additions to the structure MUST be added below, as in this
    *  example.
    */

   LETOTABLE * pTable;
   LPDBRELINFO lpdbPendingRel;         /* Pointer to parent rel struct */
   char *      szDataFileName;         /* case sensitive name of data file */
   HB_BOOL     fTransRec;              /* active target WA for transition */

} LETOAREA;

typedef LETOAREA * LETOAREAP;

HB_EXTERN_END

#if ! defined( HB_RDD_MAX_DRIVERNAME_LEN ) && defined( HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
   #define  HB_RDD_MAX_DRIVERNAME_LEN  HARBOUR_MAX_RDD_DRIVERNAME_LENGTH
   #define  HB_RDD_MAX_ALIAS_LEN       HARBOUR_MAX_RDD_ALIAS_LENGTH
#endif

