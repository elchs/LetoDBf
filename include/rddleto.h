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

#ifndef RDDLETO_H_
#define RDDLETO_H_


#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbdbferr.h"
#include "hbdate.h"
#include "hbinit.h"
#include "hbsetup.h"
#include "hbset.h"
#include "hbstack.h"
#if ! defined( __XHARBOUR__ )
   #include "hbthread.h"
#endif
#include "hbatomic.h"
#include "hbvm.h"
#ifndef USE_LZ4
   #include "hbzlib.h"
#endif

#include "dbinfo.ch"
#include "rddsys.ch"

#include "cmdleto.h"

#include "rddleto.ch"

#if ! defined( __XHARBOUR__ )

   #define HB_CDP_PAGE()  hb_vmCDP()

#else  /* __XHARBOUR__ */

   #include "hbfast.h"  /* for the 'hidden' hb_itemMove() */

   #define HB_CDP_PAGE()  hb_cdppage()

   #ifndef HB_LONG_LONG_OFF
      #define HB_LONG_LONG_OFF
   #endif
   #ifndef __HARBOUR30__
      #define __HARBOUR30__  1
   #endif
   #if ! defined( USE_LZ4 ) && ! defined( __BORLANDC__ )
      #define USE_LZ4        1
   #endif

   typedef unsigned char HB_BYTE;
   typedef int HB_BOOL;
   typedef unsigned short HB_USHORT;
   typedef signed char HB_SCHAR;
   #define HB_ULONG  ULONG

   #define HB_IT_TIMESTAMP  ( ( HB_TYPE ) 0x00040 )
   #define HB_FF_UNICODE    0x0040
   #if defined( HB_IS_NUMBER ) && defined ( HB_IS_NUMERIC )
      #undef HB_IS_NUMERIC
	  #define HB_IS_NUMERIC( p )  ( ( HB_ITEM_TYPE( p ) & HB_IT_NUMERIC ) != 0 )
   #endif

   /* hbznet & used zLib constants */
   #ifndef HB_ZLIB_STRATEGY_DEFAULT
      typedef void * PHB_ZNETSTREAM;

      #define HB_ZLIB_STRATEGY_DEFAULT    0
      #define HB_ZLIB_COMPRESSION_NONE    0
      #define HB_ZLIB_COMPRESSION_SPEED   1
      #define HB_ZLIB_COMPRESSION_SIZE    9

      #define hb_znetOpen( iLevel, nStrategy )                               NULL
      #define hb_znetRead( pStream, socket, pBuffer, len, timeout )          0
      #define hb_znetWrite( pStream, socket, pBuffer, len, timeout, pLast )  0
      #define hb_znetFlush( pStream, socket, timeout )                       0
      #define hb_znetEncryptKey( pStream, szPW, iKeyLen )                    do { } while( 0 )
      #define hb_znetClose( pStream )                                        do { } while( 0 )

      #define hb_zlibCompressBound( ulLen )                                  0
      #define hb_zlibCompress( pDst, pnDst, pSrc, nLen, iLevel )             0
      #define hb_zlibUncompress( pBufCrypt, pulLen, pPtr, nSize )            do { } while( 0 )

   #endif

   /* hbthread replace and dummy */
   #define hb_threadReleaseCPU()
   #ifndef LETO_NO_MT
      #define LETO_NO_MT                  1
   #endif
   /* No general disabling LETO_NO_THREAD here, because after correct setup it seem to work nice.
    * On the other hand, if a RTE send from server over second port, received by second ! thread,
    * is not leading to RTE in first main thread (aka  your app), totally kills the idea behind.
    * Test RTE in test_file.prg, if unsure disable it by setting -DLETO_NO_THREAD for C-compiler.
    * Especially test above for GUI applications, as i haven't done - elch */
   #if defined( HB_OS_WIN ) && ! defined( LETO_NO_THREAD )
      #ifndef __MT__  /* set before process.h to enable beginThreadex in header */
         #define __MT__
      #endif
	  #include <windows.h>
	  #include <process.h>
      typedef HANDLE   HB_THREAD_HANDLE;
      typedef unsigned HB_THREAD_ID;

      #define HB_THREAD_STARTFUNC( func )  unsigned __stdcall func( void * Cargo )
      #define HB_THREAD_END                _endthreadex( 0 ); return 0;
      #define hb_threadDetach( th_h )      CloseHandle( th_h )
   #else
      typedef int HB_THREAD_HANDLE;
      typedef int HB_THREAD_ID;
   #endif

   #ifndef DBI_LOCKTEST
      #define DBI_LOCKTEST  146
   #endif
   #ifdef DBI_ISTEMPORARY
       #undef DBI_ISTEMPORARY
       #define DBI_ISTEMPORARY 145
   #endif

   #define DB_DBFLOCK_DEFAULT      0
   #define DB_DBFLOCK_CLIPPER      DB_DBFLOCK_CLIP
   #define DB_DBFLOCK_COMIX        DB_DBFLOCK_CL53
   /* #define DB_DBFLOCK_VFP; same value */
   #define DB_DBFLOCK_HB32         DB_DBFLOCK_CL53EXT
   #define DB_DBFLOCK_HB64         DB_DBFLOCK_XHB64
   #define DB_DBFLOCK_CLIPPER2     6

   #define HB_AREANO               int
   #define hb_snprintf                           snprintf

   #define hb_arrayClone( pSource )              hb_itemClone( pSource )
   #define hb_arrayCloneTo( pDest, pSource )     hb_itemMove( pDest, hb_itemClone( pSource ) )
   /* a maybe too easy replace for the convenient harbour compare */
   #define hb_itemCompare( pA, pB, fBool,pRes )  hb_itemType( pA ) == hb_itemType( pB )
   #define hb_itemCloneTo( pDest, pSource )      hb_itemMove( pDest, hb_itemClone( pSource ) )

   #define hb_dynsymIsMemvar( pDyns )            ( hb_dynsymMemvarHandle( pDyns ) )
   #define hb_arraySetSymbol( pArr, n, pSym )    hb_itemPutSymbol( hb_arrayGetItemPtr( pArr, n ), pSym )
   #define hb_arrayGetSymbol( pArr, n )          hb_itemGetSymbol( hb_arrayGetItemPtr( pArr, n ) )

   #define hb_setGetForceOpt()                   ( HB_TRUE )
#endif

#include "funcleto.h"
#include "letocl.h"

#if ! defined( LETO_USE_THREAD )
   #ifdef LETO_NO_THREAD
      #define LETO_USE_THREAD    HB_FALSE
   #else
      #define LETO_USE_THREAD    HB_TRUE
   #endif
#endif

#ifdef __HARBOUR30__
   #define HB_SERIALIZE_NUMSIZE   0x01
#else
   #include "hbserial.ch"
#endif

#if ! defined( HB_RDD_MAX_DRIVERNAME_LEN ) && defined( HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
   #define  HB_RDD_MAX_DRIVERNAME_LEN  HARBOUR_MAX_RDD_DRIVERNAME_LENGTH
   #define  HB_RDD_MAX_ALIAS_LEN       HARBOUR_MAX_RDD_ALIAS_LENGTH
#endif

#ifndef RDDI_CONNECT
   #define RDDI_CONNECT     61
   #define RDDI_DISCONNECT  62
   #define RDDI_EXECUTE     63
#endif

#ifndef DB_DBF_SQL
   #define DB_DBF_SQL       5
#endif

HB_EXTERN_BEGIN

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
   HB_BOOL     fTemporary;             /* temporary table */
   PHB_ITEM    abFilterText;           /* the non-optimized filter expression */
   PHB_ITEM    pWhoCares;

} LETOAREA;

typedef LETOAREA * LETOAREAP;

HB_EXTERN_END

#endif  /* RDDLETO_H_ */
