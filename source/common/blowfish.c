/*
 * LetoDb calling Harbour functions for Blowfish encryption
 *
 * Copyright 2015 Rolf 'elch' Beckmann
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
#include "hbbfish.h"

#include "funcleto.h"

/* keyglobal is valid key any time for server & client, e.g. to read password file
 * - creating keys for each thread avoids mutex locks; and key is needed by server AND client -
 * whereas keylocal is a temporary key only valid during a connection lifetime
 */
typedef struct
{
   HB_BOOL     fInitGlobal;
   HB_BOOL     fInitLocal;
   HB_BLOWFISH keyglobal;
   HB_BLOWFISH keylocal;
} LETO_BLOW2;


#ifndef LETO_NO_MT
   #include "hbstack.h"

   /* thread local BlowFish data init */
   static void leto_BFInit( void * cargo )
   {
      LETO_BLOW2 * pTSData = ( LETO_BLOW2 * ) cargo;

      pTSData->fInitGlobal = HB_FALSE;
      pTSData->fInitLocal = HB_FALSE;
   }

   static HB_TSD_NEW( s_BFData, sizeof( LETO_BLOW2 ), leto_BFInit, NULL );

#else
   static LETO_BLOW2 * s_BFData = NULL;

   /* BlowFish data init */
   static LETO_BLOW2 * leto_BFInit( void )
   {
      if( ! s_BFData )
      {
         s_BFData = ( LETO_BLOW2 * ) hb_xgrabz( sizeof( LETO_BLOW2 ) );
         s_BFData->fInitGlobal = HB_FALSE;
         s_BFData->fInitLocal = HB_FALSE;
      }

      return s_BFData;
   }

   void leto_BFExit( void )
   {
      if( s_BFData )
         hb_xfree( s_BFData );
   }
#endif


/* pSrc and pDest can be the same, but pDest must be long enough for padded bytes ( max + 8 ) */
void leto_encrypt( const char * pSrc, HB_U32 ulLen, char * pDest, HB_ULONG * pLen, const char * szKey, HB_BOOL fGlobal )
{
#ifndef LETO_NO_MT
   LETO_BLOW2 *        s_bf2 = ( LETO_BLOW2 * ) hb_stackGetTSD( &s_BFData );
#else
   LETO_BLOW2 *        s_bf2 = leto_BFInit();
#endif
   const HB_BLOWFISH * s_bf;

   if( fGlobal )
   {
      if( ! s_bf2->fInitGlobal && szKey )
      {
         hb_blowfishInit( &s_bf2->keyglobal, szKey, strlen( szKey ) );
         s_bf2->fInitGlobal = HB_TRUE;
      }
      if( s_bf2->fInitGlobal )
         s_bf = ( const HB_BLOWFISH * ) &s_bf2->keyglobal;
      else
         s_bf = NULL;
   }
   else
   {
      if( ! s_bf2->fInitLocal && szKey )
      {
         hb_blowfishInit( &s_bf2->keylocal, szKey, strlen( szKey ) );
         s_bf2->fInitLocal = HB_TRUE;
      }
      if( s_bf2->fInitLocal )
         s_bf = ( const HB_BLOWFISH * ) &s_bf2->keylocal;
      else
         s_bf = NULL;
   }

   if( s_bf && pSrc && pDest )
   {
      HB_SIZE nLen = ( HB_SIZE ) ulLen;

      if( nLen )
      {  /* binary data with possible 0 need ANSI X.923 padding */
         const HB_SIZE nSize = ( ( nLen >> 3 ) + 1 ) << 3;
         HB_U32 xl, xr;

         /* copy and padding of 8bytes last block, last byte == padded bytes */
         xr = nSize - nLen;  /* padded bytes in last block */
         if( pSrc != pDest && xr < 8 )  /* source and target NOT the same */
            memcpy( pDest + nSize - 8, pSrc + nLen - ( 8 - xr ), 8 - xr );
         if( xr - 1 > 0 )
            memset( pDest + nLen, '\0', xr - 1 );
         pDest[ nSize - 1 ] = ( char ) xr;
         *pLen = ( HB_ULONG ) nSize;

         /* except the last block */
         for( nLen = 0; nLen < nSize - 8; nLen += 8 )
         {
            xl = HB_GET_BE_UINT32( &pSrc[ nLen ] );
            xr = HB_GET_BE_UINT32( &pSrc[ nLen + 4 ] );
            hb_blowfishEncrypt( s_bf, &xl, &xr );
            HB_PUT_BE_UINT32( &pDest[ nLen ], xl );
            HB_PUT_BE_UINT32( &pDest[ nLen + 4 ], xr );
         }

         nLen = nSize - 8;
         xl = HB_GET_BE_UINT32( &pDest[ nLen ] );
         xr = HB_GET_BE_UINT32( &pDest[ nLen + 4 ] );
         hb_blowfishEncrypt( s_bf, &xl, &xr );
         HB_PUT_BE_UINT32( &pDest[ nLen ], xl );
         HB_PUT_BE_UINT32( &pDest[ nLen + 4 ], xr );
      }
      else
      {
         pDest[ 0 ] = '\0';
         *pLen = 0;
      }
   }
   else
      *pLen = 0;
}

/* pszSource and pszData can be the same, but pszData will be 0 terminated => + 1 length */
void leto_decrypt( const char * pSrc, const HB_SIZE nSize, char * pszData, HB_ULONG * pLen, const char * szKey, HB_BOOL fGlobal )
{
#ifndef LETO_NO_MT
   LETO_BLOW2 *        s_bf2 = ( LETO_BLOW2 * ) hb_stackGetTSD( &s_BFData );
#else
   LETO_BLOW2 *        s_bf2 = leto_BFInit();
#endif
   const HB_BLOWFISH * s_bf;

   if( fGlobal )
   {
      if( ! s_bf2->fInitGlobal && szKey )
      {
         hb_blowfishInit( &s_bf2->keyglobal, szKey, strlen( szKey ) );
         s_bf2->fInitGlobal = HB_TRUE;
      }
      if( s_bf2->fInitGlobal )
         s_bf = ( const HB_BLOWFISH * ) &s_bf2->keyglobal;
      else
         s_bf = NULL;
   }
   else
   {
      if( ! s_bf2->fInitLocal && szKey )
      {
         hb_blowfishInit( &s_bf2->keylocal, szKey, strlen( szKey ) );
         s_bf2->fInitLocal = HB_TRUE;
      }
      if( s_bf2->fInitLocal )
         s_bf = ( const HB_BLOWFISH * ) &s_bf2->keylocal;
      else
         s_bf = NULL;
   }

   if( s_bf && pSrc && pszData )
   {
      if( nSize >= 8 && ( nSize & 0x07 ) == 0 )
      {
         HB_SIZE nLen;
         HB_U32  xl, xr;

         for( nLen = 0; nLen < nSize; nLen += 8 )
         {
            xl = HB_GET_BE_UINT32( &pSrc[ nLen ] );
            xr = HB_GET_BE_UINT32( &pSrc[ nLen + 4 ] );
            hb_blowfishDecrypt( s_bf, &xl, &xr );
            HB_PUT_BE_UINT32( &pszData[ nLen ], xl );
            HB_PUT_BE_UINT32( &pszData[ nLen + 4 ], xr );
         }

         /* validate number of padded bytes */
         xr = ( unsigned char ) pszData[ nSize - 1 ];
         nLen -= ( ( xr - 1 ) & ~0x07 ) == 0 ? xr : nLen;
         pszData[ nLen ] = '\0';
         *pLen = ( HB_ULONG ) nLen;
      }
      else
      {
         pszData[ 0 ] = '\0';
         *pLen = 0;
      }
   }
   else
      *pLen = 0;
}

/* reset local key, for temporary using a different */
void leto_cryptReset( HB_BOOL fGlobal )
{
#ifndef LETO_NO_MT
   LETO_BLOW2 * s_bf2 = ( LETO_BLOW2 * ) hb_stackGetTSD( &s_BFData );
#else
   LETO_BLOW2 *        s_bf2 = leto_BFInit();
#endif

   if( fGlobal && s_bf2->fInitGlobal )
   {
      memset( &s_bf2->keyglobal, 0, sizeof( HB_BLOWFISH ) );
      s_bf2->fInitGlobal = HB_FALSE;
   }
   else if( ! fGlobal && s_bf2->fInitLocal )
   {
      memset( &s_bf2->keylocal, 0, sizeof( HB_BLOWFISH ) );
      s_bf2->fInitLocal = HB_FALSE;
   }
}

void leto_random_block( char * sBuf, HB_USHORT uLen, HB_I32 llSeed )
{
#if defined( __HARBOUR30__ )
   HB_SYMBOL_UNUSED( llSeed );

   while( uLen-- )
   {
      sBuf[ uLen ] = 1 + ( rand() % 254 );
   }
#else
   hb_random_seed( llSeed );
   hb_random_block( sBuf, uLen );

   while( uLen-- )
   {
      if( sBuf[ uLen ] == '\0' )
         sBuf[ uLen ] = '\1';
   }
#endif
}

/* generation of the local key */
char * leto_localKey( const char * sPart, HB_USHORT uLen )
{
   if( sPart && sPart[ 0 ] )
   {
      HB_USHORT uKeyLen = ( HB_USHORT ) strlen( LETO_PASSWORD );
      char *    szKey;
      HB_USHORT uModulo;

      if( uLen > uKeyLen )
         uKeyLen = uLen;
      szKey = ( char * ) hb_xgrab( uKeyLen + 1 );
      memcpy( szKey, LETO_PASSWORD, uKeyLen );
      szKey[ uKeyLen ] = '\0';

      uModulo = uKeyLen - uLen + 1;
      if( uModulo < 2 )
         memcpy( szKey, sPart, uLen );
      else
         memcpy( szKey + ( ( unsigned char ) sPart[ 0 ] % uModulo ), sPart, uLen );

      return szKey;
   }
   else
      return NULL;
}


