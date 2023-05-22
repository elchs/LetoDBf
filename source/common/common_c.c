/*
 * Leto db server functions
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 *           2014-16 Rolf 'elch' Beckmann <harbour / at / elchs.de>
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
#include "hbapifs.h"
#include "hbdefs.h"
#ifndef __XHARBOUR__
   #include "hbthread.h"
#else
   #if defined( HB_ARCH ) && ( HB_ARCH == 64 )
      #ifndef HB_ARCH_64BIT
         #define HB_ARCH_64BIT
      #endif
   #endif
#endif
#include "hbatomic.h"
#include "hbsocket.h"
#include "rddleto.ch"

#if defined( HB_OS_UNIX )
   #include <unistd.h>
   #include <sys/time.h>
   #include <time.h>
   #include <stdio.h>
   #include <stdlib.h>
#elif defined( HB_OS_WIN )
   #include <windows.h>
#endif

#ifdef USE_PMURHASH
   #include "PMurHash.h"
#endif

/* Fixed: older mingw32 than 5.3 ?? have a problem to correct pick up GetSystemTimes() in winbase.h
 * no comment on stoneage old compilers, even MsVc 2005 seem to be able to do */
#if ( defined( __BORLANDC__ ) || defined( __WATCOMC__ ) || defined( __MINGW32__ ) )
   #if defined( __MINGW32__ )
      #if ! ( __MINGW32_MAJOR_VERSION >= 5 && __MINGW32_MINOR_VERSION >= 3 )
         #define __NO_CPU_LOAD
      #endif
   #else
      #define __NO_CPU_LOAD
   #endif
#endif

#if ( ! ( defined( HB_OS_LINUX ) || defined( HB_OS_WIN ) ) && ! defined(  __NO_CPU_LOAD ) )
   #define __NO_CPU_LOAD
#elif defined( __XHARBOUR__ )
   #define __NO_CPU_LOAD
#endif

/* mutex for leto_CPULoad() */
#if ! defined( __NO_CPU_LOAD )
   #if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
      static HB_SPINLOCK_T s_CpuMtx = HB_SPINLOCK_INIT;
      #define HB_GC_LOCKC()    HB_SPINLOCK_ACQUIRE( &s_CpuMtx )
      #define HB_GC_UNLOCKC()  HB_SPINLOCK_RELEASE( &s_CpuMtx )
   #else
      static HB_CRITICAL_NEW( s_CpuMtx );
      #define HB_GC_LOCKC()    hb_threadEnterCriticalSection( &s_CpuMtx )
      #define HB_GC_UNLOCKC()  hb_threadLeaveCriticalSection( &s_CpuMtx )
   #endif
#endif

/* static's for leto_CPULoad() */
#if ! defined( __NO_CPU_LOAD )
   #if defined( HB_OS_LINUX )
      static long double a[ 4 ];
      static long double b[ 4 ] = { 0 };
   #elif defined( HB_OS_WIN )
      static HB_U64 s_ullPreviousTotalTicks = 0;
      static HB_U64 s_ullPreviousIdleTicks = 0;
   #endif
#endif


#ifdef __HARBOUR30__
HB_SIZE hb_fsPipeWrite( HB_FHANDLE hPipeHandle, const void * buffer, HB_SIZE nSize, HB_MAXINT nTimeOut )
{
#if defined( HB_OS_WIN )
   HANDLE hPipe = ( HANDLE ) hb_fsGetOsHandle( hPipeHandle );
   DWORD dwWritten, dwToWrite;

   HB_SYMBOL_UNUSED( nTimeOut );
   dwToWrite = ( DWORD ) nSize;
   if( WriteFile( hPipe, ( const HB_BYTE * ) buffer, dwToWrite, &dwWritten, NULL ) != 0 ) {};
#else
   HB_SYMBOL_UNUSED( nTimeOut );

   if( write( hPipeHandle, buffer, nSize ) ) {};
#endif
   return nSize;
}
#endif


int leto_CPUCores( void )
{
   int iCores;

#if defined( HB_OS_UNIX )

   #ifdef _SC_NPROCESSORS_ONLN      /* LINUX */
      iCores = ( int ) sysconf( _SC_NPROCESSORS_ONLN );

   #elif _SC_NPROC_ONLN             /* IRIX */
      iCores = ( int ) sysconf( _SC_NPROC_ONLN );

   #elif MPC_GETNUMSPUS             /* HPUX */
      iCores = mpctl( MPC_GETNUMSPUS, NULL, NULL );

   #elif HW_AVAILCPU                /* OSx, BSDs', ... */
      int nm[ 2 ];
      size_t len = 4;

      nm[ 0 ] = CTL_HW;
      nm[ 1 ] = HW_AVAILCPU;
      sysctl( nm, 2, ( HB_U32 * ) &iCores, &len, NULL, 0 );

   #elif HW_NCPU
      int nm[ 2 ];
      size_t len = 4;

      nm[ 0 ] = CTL_HW;
      nm[ 1 ] = HW_NCPU;
      sysctl( nm, 2, ( HB_U32 * ) &iCores, &len, NULL, 0 );

   #else
      iCores = 1;

   #endif

#elif defined( HB_OS_WIN )
   SYSTEM_INFO sysinfo;

   GetSystemInfo( &sysinfo );
   iCores = sysinfo.dwNumberOfProcessors;

#else  /* who else OS */
   iCores = 1;

#endif

   if( iCores < 1 )  /* at least one must be there ;-) */
      iCores = 1;

   return iCores;
}

#if defined( HB_OS_LINUX ) && ! defined( __NO_CPU_LOAD )
HB_UINT leto_CPULoad( void )
{
   HB_BOOL fInit;
   FILE * fp;
   int i;

   HB_GC_LOCKC();

   fInit = ( b[ 0 ] == 0 && b[ 1 ] == 0 && b[ 2 ] == 0 && b[ 3 ] == 0 );
   if( fInit )
   {
      if( ( fp = fopen( "/proc/stat", "r" ) ) != NULL )
      {
         char dump[ 42 ];

         i = fscanf( fp, "%s %Lf %Lf %Lf %Lf", dump, &b[ 0 ], &b[ 1 ], &b[ 2 ], &b[ 3 ] );
         fclose( fp );
      }
      else
         i = 0;
      HB_GC_UNLOCKC();

      return i ? 1 : 0;
   }
   else
   {
      long double cpuavg = 0;

      for( i = 0; i < 4; i++ )
      {
         a[ i ] = b[ i ];
      }

      if( ( fp = fopen( "/proc/stat", "r" ) ) != NULL )
      {
         char dump[ 42 ];

         i = fscanf( fp,"%s %Lf %Lf %Lf %Lf", dump, &b[ 0 ], &b[ 1 ], &b[ 2 ], &b[ 3 ] );
         fclose( fp );
      }
      else
         i = 0;

      if( i )
         cpuavg = ( ( b[ 0 ] + b[ 1 ] + b[ 2 ] ) - ( a[ 0 ] + a[ 1 ] + a[ 2 ] ) ) /
                  ( ( b[ 0 ] + b[ 1 ] + b[ 2 ] + b[ 3 ] ) - ( a[ 0 ] + a[ 1 ] + a[ 2 ] + a[ 3 ] ) );

      HB_GC_UNLOCKC();

      return i ? ( HB_UINT ) ( cpuavg * 100 ) : 0;
   }
}

#elif defined( HB_OS_WIN ) && ! defined( __NO_CPU_LOAD ) && ! defined( __XHARBOUR__ )
static HB_U64 leto_FtUint64( const FILETIME ft )
{
   return ( ( ( HB_U64 ) ( ft.dwHighDateTime ) ) << 32 ) | ( ( HB_U64 ) ft.dwLowDateTime );
}

static float CalcCPULoad( HB_U64 ullIdleTicks, HB_U64 ullTotalTicks)
{
   HB_U64 ullTotalTicksDiff = ullTotalTicks - s_ullPreviousTotalTicks;
   HB_U64 ullIdleTicksDiff  = ullIdleTicks - s_ullPreviousIdleTicks;
   float ret = 1.0f - ( ( ullTotalTicksDiff > 0 ) ?
               ( ( float ) ullIdleTicksDiff ) / ullTotalTicksDiff : 0 );

   s_ullPreviousTotalTicks = ullTotalTicks;
   s_ullPreviousIdleTicks  = ullIdleTicks;
   return ret;
}

HB_UINT leto_CPULoad( void )
{
   HB_UINT  uiRet;
   FILETIME ftIdle, ftKernel, ftUser;

   HB_GC_LOCKC();

   uiRet = ( HB_UINT ) ( GetSystemTimes( &ftIdle, &ftKernel, &ftUser ) ?
             100 * CalcCPULoad( leto_FtUint64( ftIdle ), leto_FtUint64( ftKernel ) + leto_FtUint64( ftUser ) ) : 0 );

   HB_GC_UNLOCKC();

   return uiRet;
}

#else
HB_UINT leto_CPULoad( void )
{
   return 0;
}

#endif  /* leto_CPULoad() */

HB_U64 leto_MicroSec( void )
{
#if defined( HB_OS_UNIX ) && ( defined( CLOCK_MONOTONIC ) || defined( CLOCK_MONOTONIC_RAW ) )
   struct timespec ts;

   #if defined( CLOCK_MONOTONIC )         /* only forwarding clock */
      clock_gettime( CLOCK_MONOTONIC, &ts );
   #elif defined( CLOCK_MONOTONIC_RAW )   /* without NTP changes, _RAW kernel >= 2.6.28 */
      clock_gettime( CLOCK_MONOTONIC_RAW, &ts );
   #endif

   return ( HB_U64 ) ( ts.tv_sec * 1000000 ) + ( ts.tv_nsec / 1000 );
#else
   return 0;
#endif
}

HB_U64 leto_MilliSec( void )
{
#if defined( HB_OS_WIN )
   return ( HB_U64 ) GetTickCount();  /* 32bit => overflow after 49.7 days */
#elif defined( HB_OS_UNIX ) && \
      ( defined( CLOCK_MONOTONIC_COARSE ) || defined( CLOCK_MONOTONIC_RAW ) || defined( CLOCK_MONOTONIC ) )
   struct timespec ts;

   #if defined( CLOCK_MONOTONIC_COARSE )  /* fast!, but not as precises as _RAW; kernel >= 2.6.32 */
      clock_gettime( CLOCK_MONOTONIC_COARSE, &ts );
   #elif defined( CLOCK_MONOTONIC_RAW )   /* without NTP changes, _RAW kernel >= 2.6.28 */
      clock_gettime( CLOCK_MONOTONIC_RAW, &ts );
   #elif defined( CLOCK_MONOTONIC )       /* only forwarding clock */
      clock_gettime( CLOCK_MONOTONIC, &ts );
   #endif

   return ( HB_U64 ) ( ts.tv_sec * 1000 ) + ( ts.tv_nsec / 1000000 );
#else
   return ( HB_U64 ) hb_dateMilliSeconds();  /* may go back and forth */
#endif
}


HB_U64 leto_MilliDiff( HB_U64 oldstamp )
{
#if defined( HB_OS_UNIX ) && \
      ( defined( CLOCK_MONOTONIC_COARSE ) || defined( CLOCK_MONOTONIC_RAW ) || defined( CLOCK_MONOTONIC ) )
   return leto_MilliSec() - oldstamp;
#else
   HB_U64 newstamp = leto_MilliSec();

   if( newstamp < oldstamp )
   #if defined( HB_OS_WIN )
      return ( 0xffffffff - ( HB_U32 ) oldstamp ) + ( HB_U32 ) newstamp;
   #else
      return 0;
   #endif
   else
      return newstamp - oldstamp;
#endif
}

/* removes all spaces of a literally codeblock, return HB_FALSE if wrong quotated */
HB_BOOL leto_CbTrim( char * szCodeblock )
{
   HB_BOOL   fSeqSingle = HB_FALSE;
   HB_BOOL   fSeqDouble = HB_FALSE;
   HB_USHORT uSrc, uTrg, uLen = ( HB_USHORT ) strlen( szCodeblock );

   for( uSrc = 0, uTrg = 0; uSrc < uLen; uSrc++ )
   {
      if( szCodeblock[ uSrc ] == '"' )
      {
         if( fSeqDouble )
            fSeqDouble = HB_FALSE;
         else if( ! fSeqSingle )
            fSeqDouble = HB_TRUE;
      }
      if( szCodeblock[ uSrc ] == '\'' )
      {
         if( fSeqSingle )
            fSeqSingle = HB_FALSE;
         else if( ! fSeqDouble )
            fSeqSingle = HB_TRUE;
      }
      if( szCodeblock[ uSrc ] == ' ' && ! fSeqSingle && ! fSeqDouble )
         continue;
      if( uSrc > uTrg )
         szCodeblock[ uTrg++ ] = szCodeblock[ uSrc ];
      else
         uTrg++;
   }
   if( uSrc > uTrg )
      szCodeblock[ uTrg ] = '\0';
   if( ! fSeqSingle && ! fSeqDouble )
      return HB_TRUE;
   else
      return HB_FALSE;
}

/* is like a length optimized HB_GET_LE_UINT32 */
/* convert a string number of length iLenLen to numeric value */
/* ever unsigned, minimum length 1 byte */
HB_ULONG leto_b2n( const char * s, const HB_UCHAR uLenLen )
{
   HB_U32   n = ( HB_U32 ) ( ( const unsigned char ) s[ 0 ] );
   HB_UCHAR ui = 1;

   while( ui < uLenLen )
   {
      n += ( HB_U32 ) ( ( const unsigned char ) s[ ui ] << ( 8 * ui ) );
      ui++;
   }

   return n;
}

/* convert a numeric value to string into <s>, return length of result */
HB_UCHAR leto_n2b( char * s, HB_U32 n )
{
   HB_UCHAR ui = 0;

   do
   {
      s[ ui++ ] = ( unsigned char ) ( n & 0xFF );
      n >>= 8;
   }
   while( n );

   return ui;
}

#ifdef USE_PMURHASH
/* fast 'official' PD hashing logic, with possible collisions -- for NON !! security tasks */
HB_U32 leto_hash( const char * key, int iLen )
{
   return ( HB_U32 ) PMurHash32( 0xABCD /* uint32_t seed */, ( const void * ) key, iLen);
}

#else  /* poor & silly ! but also fast green homebrew */

#define MULTIPLIER  37

HB_U32 leto_hash( const char * key, int iLen )
{
   HB_U32 h = 0;
   const unsigned char * p = ( const unsigned char * ) key;

   for( ; iLen-- > 0; p++ )
      h = MULTIPLIER * h + *p;
   return h;
}
#endif

/* char mapping table for leto_stricmp() */
static const unsigned char s_Upper[ 256 ] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                                     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                                     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                                     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
                                     40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
                                     50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
                                     60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
                                     70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
                                     80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
                                     90, 91, 92, 93, 94, 95, 96, 65, 66, 67,
                                     68, 69, 70, 71, 72, 73, 74, 75, 76, 77,
                                     78, 79, 80, 81, 82, 83, 84, 85, 86, 87,
                                     88, 89, 90, 123, 124, 125, 126, 127, 128, 129,
                                     130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
                                     140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
                                     150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
                                     160, 161, 162, 163, 164, 165, 166, 167, 168, 169,
                                     170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
                                     180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
                                     190, 191, 192, 193, 194, 195, 196, 197, 198, 199,
                                     200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
                                     210, 211, 212, 213, 214, 215, 216, 217, 218, 219,
                                     220, 221, 222, 223, 224, 225, 226, 227, 228, 229,
                                     230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
                                     240, 241, 242, 243, 244, 245, 246, 247, 248, 249,
                                     250, 251, 252, 253, 254, 255 };

/* hb_stricmp( s1, s2 ) replace, stops with first different char, not useable for sorting */
int leto_stricmp( const char * s1, const char * s2 )
{
   do
   {
      if( s_Upper[ ( const unsigned char ) *s1 ] != s_Upper[ ( const unsigned char ) *s2++ ] )
         return 1;
   }
   while( *s1++ );

   return 0;
}


const char * leto_stristr( const char * s1, const char * s2 )
{
   const char * s3, * s0 = s2;

   if( ! s1 || ! s2 || ! *s2 )
      return NULL;

   while( *s1 )
   {
      if( s_Upper[ ( const unsigned char ) *s1++ ] == s_Upper[ ( const unsigned char ) *s0 ] )
      {
         s0++;
         s3 = s1;
         while( *s0 && *s1 )
         {
            if( s_Upper[ ( const unsigned char ) *s1++ ] == s_Upper[ ( const unsigned char ) *s0 ] )
               s0++;
            else
            {
              if( *s1 )
              {
                 s0 = s2;
                 s1 = s3;
              }
              break;
            }
         }

         if( ! *s0 )
            return s1;
      }
   }

   return NULL;
}

void leto_byte2hexchar( const char * ptri, int iLen, char * ptro )
{
   int i, n;

   for( i = 0; i < iLen; i++, ptri++ )
   {
      n = ( ( ( unsigned char ) *ptri ) >> 4 ) & 0x0F;
      *ptro++ = ( char ) ( ( n < 10 ) ? n + 48 : n + 55 );
      n = ( ( unsigned char ) *ptri ) & 0x0F;
      *ptro++ = ( char ) ( ( n < 10 ) ? n + 48 : n + 55 );
   }
}

void leto_hexchar2byte( const char * ptri, int iLen, char * ptro )
{
   int i, n, nRes;

   iLen /= 2;
   for( i = 0; i < iLen; i++ )
   {
      n = ( unsigned char ) *ptri++;
      if( n > 64 && n < 71 )
         nRes = ( n - 55 ) * 16;
      else if( n > 47 && n < 58 )
         nRes = ( n - 48 ) * 16;
      else
         nRes = 0;

      n = ( unsigned char ) *ptri++;
      if( n > 64 && n < 71 )
         nRes += n - 55;
      else if( n > 47 && n < 58 )
         nRes += n - 48;
      else
         nRes = 0;
      *ptro++ = ( char ) nRes;
   }
}

#if ! defined( __LETO_C_API__ )

HB_FUNC( LETO_BVALUE )
{
#if defined( HB_ARCH_64BIT )
   char szSearch[ 8 ];
   int iLen = 8;
#else
   char szSearch[ 4 ];
   int iLen = 4;
#endif

#if defined( HB_ARCH_64BIT )
   HB_PUT_LE_UINT64( szSearch, ( HB_U64 ) hb_parnll( 1 ) );
#else
   HB_PUT_LE_UINT32( szSearch, ( HB_U32 ) hb_parnl( 1 ) );
#endif

   hb_retclen( szSearch, iLen );
}

HB_FUNC( LETO_BSEARCH )
{
   const char * szString = hb_parc( 1 );
   HB_ULONG     ulLen = hb_parclen( 1 );
   HB_ULONG     ulPos = 0;
   HB_BOOL      bFound = HB_FALSE;
#if defined( HB_ARCH_64BIT )
   HB_U64       ullSearch = hb_parnll( 2 );
   int          iLen = 8;
#else
   HB_U32       ulSearch = hb_parnl( 2 );
   int          iLen = 4;
#endif

   while( ulPos < ulLen )
   {
#if defined( HB_ARCH_64BIT )
      if( HB_GET_LE_UINT64( szString + ulPos ) == ullSearch )
         bFound = HB_TRUE;
#else
      if( HB_GET_LE_UINT32( szString + ulPos ) == ulSearch )
         bFound = HB_TRUE;
#endif
      if( bFound )
         break;
      ulPos += iLen;
   }
   hb_retl( bFound );
}

/* something like a strrstr() */
HB_FUNC( LETO_SSEARCH )
{
   const char * szString = hb_parc( 1 );
   const char * ptr = szString + hb_parclen( 1 ) - 1;
   char         szSearch[ 4 ];
   const char * ptr2 = szSearch + 3;
   const char * psc, * psc2;
   HB_BOOL      bFound = HB_FALSE;

   HB_PUT_BE_UINT32( szSearch, ( HB_U32 ) hb_parnl( 2 ) );
   while( ptr > szString + 2 )
   {
      for( psc = ptr, psc2 = ptr2;; )
      {
         if( *psc-- != *psc2 )
            break;
         if( psc2-- == szSearch )
         {
            bFound = HB_TRUE;
            break;
         }
      }
      if( bFound )
         break;
      ptr--;
   }
   hb_retl( bFound );
}

#endif  /* ! __LETO_C_API__  */

/* convert unsigned long to zero terminated string, returns length */
/* special version with fixed base 10 -- high technics, less divisions ;-) */
HB_UINT ultostr( HB_U64 ulValue, char * ptr )
{
   if( ulValue < 10 )
   {
      *ptr = ( char ) ( '0' + ulValue );
      *( ptr + 1 ) = '\0';
      return 1;
   }
   else
   {
      HB_U64  ulRemain;
      HB_UINT uiCount;
      char    buf[ 21 ];
      char *  ptrr = buf + 20;

      *ptrr = '\0';
      do
      {
         *--ptrr = ( char ) ( '0' + ( ulValue - 10 * ( ulRemain = ulValue / 10 ) ) );
      }
      while( ( ulValue = ulRemain ) > 0 );

      uiCount = buf + 20 - ptrr;
      memcpy( ptr, ptrr, uiCount + 1 );  /* incl. '\0' */
      return uiCount;
   }
}

/* > 3x faster sprintf replace, only valid for: %c, %s, %[l]d|u */
int eprintf( char * d, const char * fmt, ... )
{
   char *       dst = d;
   va_list      ap;
   const char * p;
   HB_I64       lValue;
   HB_U64       uValue;

   va_start( ap, fmt );
   for( ; *fmt; )
   {
      if( *fmt == '%' )
      {
         switch( *++fmt )
         {
            case 's':  /* NULL pointer results in nothing output */
               p = va_arg( ap, const char * );
               if( p && ( *d = *p ) != 0 )
               {
                  while( ( *++d = *++p ) != 0 );
               }
               break;

            case 'c':
               *d++ = ( char ) va_arg( ap, int );
               break;

            default:  /* all integer */
               if( *fmt == 'l' )
               {
                  if( *++fmt == 'u' )
                     uValue = va_arg( ap, unsigned long );
                  else
                  {
                     lValue = va_arg( ap, long );
                     if( lValue < 0 )
                     {
                        *d++ = '-';
                        lValue *= -1;
                     }
                     uValue = lValue;
                  }
               }
               else
               {
                  if( *fmt == 'u' )
                     uValue = va_arg( ap, unsigned int );
                  else
                  {
                     lValue = va_arg( ap, int );
                     if( lValue < 0 )
                     {
                        *d++ = '-';
                        lValue *= -1;
                     }
                     uValue = lValue;
                  }
               }

               /* a copy of ultostr(), without '\0' termination; value is ensured to be positive */
               if( uValue < 10 )
                  *d++ = ( char ) ( '0' + uValue );
               else
               {
                  HB_U64  uRemain;
                  HB_UINT uiCount;
                  char    buf[ 21 ];
                  char *  ptrr = buf + 20;

                  do
                  {
                     *--ptrr = ( char ) ( '0' + ( uValue - 10 * ( uRemain = uValue / 10 ) ) );
                  }
                  while( ( uValue = uRemain ) > 0 );

                  uiCount = buf + 20 - ptrr;
                  memcpy( d, ptrr, uiCount );
                  d += uiCount;
               }
               break;
         }
         fmt++;
      }
      else
         *d++ = *fmt++;
   }
   *d = '\0';
   va_end( ap );

   return ( d - dst );
}

#if ! defined( __LETO_C_API__ )

HB_FUNC( LETO_ISVALIDIP4 )
{
   HB_BOOL fValid;

   if( hb_parclen( 1 ) )
   {
      void *   pSockAddr;
      unsigned uiLen;
      int      iPort = HB_ISNUM( 2 ) ? hb_parni( 2 ) : LETO_DEFAULT_PORT;
      /* ToDo ? extract port of address after ':' */

      fValid = hb_socketInetAddr( &pSockAddr, &uiLen, hb_parc( 1 ), iPort );
      if( pSockAddr )
         hb_xfree( pSockAddr );
   }
   else
      fValid = HB_FALSE;

   hb_retl( fValid );
}

/* translate address with netmask to a valid broadcast IP4:  broadcast = ip | ( ~ subnet ) */
HB_FUNC( LETO_BROADCASTIP )
{
   char *  szBroadcast = ( char * ) hb_xgrab( HB_PATH_MAX );
   HB_SIZE nPosBC = 0;

   szBroadcast[ 0 ] = '\0';
   if( hb_parclen( 1 ) && hb_parclen( 2 ) )
   {
      HB_SIZE      nTuple = 1;
      HB_SIZE      nPosIP, nPosNM;
      const char * cAddr = hb_parc( 1 );
      const char * cNetm = hb_parc( 2 );
      const char * ptr;

      while( nTuple <= 4 )
      {
         ptr = strstr( cAddr, "." ) ;
         if( ! ptr && nTuple < 4 )  /* wrong IP */
            break;
         else
         {
            nPosIP = atoi( cAddr );
            if( ptr )
               cAddr += ( ptr - cAddr ) + 1;
         }

         ptr = strstr( cNetm, "." );
         if( ! ptr && nTuple < 4 )  /* wrong NM */
            break;
         else
         {
            nPosNM = atoi( cNetm );
            if( ptr )
               cNetm += ( ptr - cNetm ) + 1;
         }

         nPosBC += sprintf( szBroadcast + nPosBC, "%d", ( HB_UCHAR ) ( nPosIP | ( ~ nPosNM ) ) );
         if( nTuple++ < 4 )
            szBroadcast[ nPosBC++ ] = '.';
      }
      if( nTuple <= 4 )
         nPosBC = 0;
   }

   hb_retclen_buffer( szBroadcast, nPosBC );
}

#endif  /* ! __LETO_C_API__  */
