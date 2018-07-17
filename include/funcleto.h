/*
 * Header file for Leto RDD and Server
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

#if ! defined( LETO_PASSWORD )
   /* !!! wild !!! password for initial decrypt received key from server with first connect */
   #if defined LETO_MYPASSWORD
      #define LETO_PASSWORD       HB_MACRO2STRING( LETO_MYPASSWORD )
   #else
      #define LETO_PASSWORD       "hE8Q,jy5+R#_~?0"
   #endif
#endif

/* !!! wild !!! password for server internally to decrypt user/password file */
#if defined( __MINGW32__ ) || defined( __MINGW64_VERSION_MAJOR )
   #ifdef __RANDOM_STRING__
      #undef __RANDOM_STRING__
   #endif
   #define __RANDOM_STRING__   "e.L!#-pcC@ZTa0&H"
#elif ! defined( __RANDOM_STRING__ )
   #define __RANDOM_STRING__   "e.L!#-pcC@ZTa0&H"
#endif

#if defined( HB_OS_UNIX )
   #define DEF_SEP               '/'
   #define DEF_SEPPATH           ':'
   #define DEF_CH_SEP            '\\'
#else
   #define DEF_SEP               '\\'
   #define DEF_SEPPATH           ';'
   #define DEF_CH_SEP            '/'
#endif

#if defined( __WATCOMC__ ) || defined( __LCC__ )
   #define _declspec( dllexport )  __declspec( dllexport )
#endif

#define LETO_MSGSIZE_LEN        4
#define LETO_SENDRECV_BUFFSIZE  65535

#define LETO_VERSION_STRING     "3.00"
#define LETO_RELEASE_STRING     "LetoDBf Server"
#define LETO_DEFAULT_PORT       2812

#define LETO_MAX_USERNAME       16
#define LETO_MAX_KEYLENGTH      32
#define LETO_DOPCODE_LEN        7

/* 0xFFFFFFF  268 435 455 */
#define LETO_MAX_RECV_BLOCK     0x7FFFFFFF  /* 2 147 483 647  ( 2GB - 1 ) */
/* In the absolute worst case of a single-byte input stream,
 * the overhead  is eleven bytes of overhead, includes one byte of actual data
 * plus LetoDBf 8 bytes for two 32 bit lengths  */
#define LETO_ZIP_MINLENGTH      18
#define LETO_LZ4_COMPRESS_MIN   512    /* 5 is logical minimum: min. +1 LZ4 overhead, +4 for uncomp. length */

#define LETO_RDD_MAX_ALIAS_LEN  7      /* server internal alias Exxxxx */
#define LETO_MAX_TAGNAME        10     /* ?? - CDX allows 10 ? */
#define LETO_MAX_KEY            256    /* 240 = CDX, 256 = NTX */
#define LETO_MAX_EXP            255
#define LETO_IDXINFOBLOCK       ( HB_PATH_MAX + LETO_MAX_TAGNAME + LETO_MAX_KEY + LETO_MAX_EXP + 42 )

#define LETO_CDX                0
#define LETO_NTX                1

#if ! ( defined( HB_FT_TIME ) )
   #define HB_FT_TIME         8
#endif
#if ! ( defined( HB_FT_TIMESTAMP ) )
   #define HB_FT_TIMESTAMP    9
#endif
#if ! ( defined( HB_FT_MODTIME ) )
   #define HB_FT_MODTIME      10
#endif
#if ! ( defined( HB_FT_PICTURE ) )
   #define HB_FT_PICTURE      18
#endif

#if ! defined( HB_PFS )
   #define HB_PFS          "l"
#endif

#if ! defined( HB_FALSE )
   #define HB_FALSE        0
   #define HB_TRUE         ( ! 0 )
#endif
#if ! defined( HB_SUCCESS )
   #define HB_SUCCESS      SUCCESS
   #define HB_FAILURE      FAILURE
#endif

#ifdef __HARBOUR30__
   #define DB_DBFLOCK_CLIPPER   DB_DBFLOCK_CLIP
   #define DB_DBFLOCK_COMIX     DB_DBFLOCK_CL53
   #define DB_DBFLOCK_HB32      DB_DBFLOCK_CL53EXT
   /* does not know DB_DBFLOCK_CLIPPER2 */
   #define DB_DBFLOCK_CLIPPER2     6
   #ifndef DB_DBFLOCK_VFP
      #define DB_DBFLOCK_VFP       3
   #endif
   #ifndef DB_DBFLOCK_HB64
      #define DB_DBFLOCK_HB64      5
   #endif

   #define PHB_MACRO  HB_MACRO_PTR
#endif

#if ! defined( HB_ISFIRSTIDCHAR )
   #define HB_ISDIGIT( c )        ( ( c ) >= '0' && ( c ) <= '9' )
   #define HB_ISFIRSTIDCHAR( c )  ( c >= 'A' && c <= 'Z' ) || c == '_' || ( c >= 'a' && c <= 'z' )
   #define HB_ISNEXTIDCHAR( c )   ( HB_ISFIRSTIDCHAR( c ) || HB_ISDIGIT( c ) )
#endif

/* flags for state of records */
#define LETO_FLG_BOF           0x01
#define LETO_FLG_EOF           0x02
#define LETO_FLG_LOCKED        0x04
#define LETO_FLG_DEL           0x08
#define LETO_FLG_FOUND         0x10

/* client flags for update state of tables */
#define LETO_FLAG_UPD_NONE     0x00
#define LETO_FLAG_UPD_APPEND   0x01
#define LETO_FLAG_UPD_CHANGE   0x02
#define LETO_FLAG_UPD_DELETE   0x04
#define LETO_FLAG_UPD_UNLOCK   0x08
#define LETO_FLAG_UPD_FLUSH    0x10
#define LETO_FLAG_UPD_ALL      0x20

extern HB_I64 leto_MilliSec( void );
extern HB_U64 leto_MicroSec( void );
extern int leto_CPUCores( void );
extern HB_UINT leto_CPULoad( void );
extern HB_UCHAR leto_n2b( char * s, HB_U32 n );
extern HB_ULONG leto_b2n( const char * s, const HB_UCHAR iLenLen );
extern int leto_stricmp( const char * s1, const char * s2 );
extern const char * leto_stristr( const char * s1, const char * s2 );
extern HB_BOOL leto_CbTrim( char * szCodeblock );
extern HB_U32 leto_hash( const char * str, int iLen );

extern void leto_writelog( const char * sFile, int n, const char * s, ... );
extern void leto_encrypt( const char * pSrc, HB_U32 ulLen, char * pDest, HB_ULONG * pLen, const char * szKey, HB_BOOL fGlobal );
extern void leto_decrypt( const char * pData, const HB_SIZE nSize, char * pszData, HB_ULONG * pLen, const char * szKey, HB_BOOL fGlobal );
extern char * leto_localKey( const char * sPart, HB_USHORT uLen );
extern void leto_random_block( char * sBuf, HB_USHORT uLen, HB_I32 llSeed );
extern void leto_cryptReset( HB_BOOL fGlobal );
extern void leto_byte2hexchar( const char * ptri, int iLen, char * ptro );
extern void leto_hexchar2byte( const char * ptri, int iLen, char * ptro );
extern HB_UINT ultostr( HB_U64 ulValue, char * ptr );
extern int eprintf( char * d, const char * fmt, ... );

#ifdef USE_LZ4
   #include "lz4.h"

   struct _HB_LZ4NET;
   typedef struct _HB_LZ4NET * PHB_LZ4NET;

   extern HB_BOOL hb_lz4netEncryptTest( const PHB_LZ4NET pStream, const HB_ULONG ulLen );
   extern HB_ULONG hb_lz4netEncrypt( PHB_LZ4NET pStream, char ** pData, HB_ULONG ulLen, HB_ULONG * pulDataLen, const char * szData );
   extern HB_ULONG hb_lz4netDecrypt( PHB_LZ4NET pStream, char ** pData, HB_ULONG ulLen, HB_ULONG * pulDataLen, HB_BOOL fCompressed );
   extern PHB_LZ4NET hb_lz4netOpen( int iLevel, int strategy );
   extern void hb_lz4netClose( PHB_LZ4NET pStream );
   extern void hb_lz4netEncryptKey( PHB_LZ4NET pStream, const char * keydata, int keylen );
#endif

