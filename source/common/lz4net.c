/*
 * LZ4 compression & Blowfish encryption for Harbour/ LetoDBf socket communication
 *
 * Copyright 2015-2016 Rolf 'elch' Beckmann
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

/* usage:
 * PHB_LZ4NET hb_lz4netOpen( int iLevel, int iStrategy )
 *    iStrategy unused; => pointer to struct
 * hb_lz4netEncryptKey( PHB_LZ4NET pStream, const unsigned char * pPassword, int iPasslen )
 *    activate optional blowfish encrytion, pBfKey is stored in struct
 *
 * HB_ULONG lz4_bfEncrypt( const HB_BLOWFISH * pBfKey, const char * pSrc, HB_SIZE nLen, char * pDest, HB_ULONG * pulLen )
 *    compress [plus encrypt] data into pDest, highest bit of leading U32 length value indicates if compressed
 *
 * HB_ULONG hb_lz4netDecrypt( PHB_LZ4NET pStream, char ** pData, HB_ULONG ulLen, HB_ULONG * pulDataLen, HB_BOOL fCompressed )
 *    passed raw data [ without preleading U32 value ] will be returned in same buffer
 *
 * void hb_lz4netClose( PHB_LZ4NET pStream )
 *    free allocated memory in struct
 */

#include "hbapi.h"
#include "hbbfish.h"

/* plus compile & link lz4.c, referenced revision: r131, in subdirectory: lib of*/
/* https://github.com/Cyan4973/lz4 */
#include <lz4.h>

#if 0  /* testing purpose only */
#include <lz4hc.h>
#endif

#define LZ4_COMPRESS_MINLENGTH  384    /* 5 is logical minimum: min. +1 LZ4 overhead, +4 for uncomp. length */
#define LZ4_BUFFER_DEFSIZE      65536  /* min default buffer size, alloc + 1 */
#define LZ4_BF_CBC                     /* activate CBC mode for Blowfish */

#ifndef HB_BF_CIPHERBLOCK
   #define HB_BF_CIPHERBLOCK    8
#endif

typedef struct _HB_LZ4NET
{
   int           iLevel;     /* optimization ! level: -1 (0) = no compress; 1 - 9 , higher = faster */
   int           iErr;       /* error code for last operation */
   HB_ULONG      ulBufLen;   /* size of internal tmp buffer */
   char *        pBuffer;    /* internal tmp buffer */
   HB_BLOWFISH * pBfKey;     /* blowfish en-/de-encrypt key */
   void *        LZ4state;   /* internal used by LZ4 */
   char        * IV;         /* initialization vektor for CBC mode*/
}
HB_LZ4NET, * PHB_LZ4NET;


/* release stream structure */
void hb_lz4netClose( PHB_LZ4NET pStream )
{
   if( pStream )
   {
      if( pStream->ulBufLen && pStream->pBuffer )
         hb_xfree( pStream->pBuffer );
      if( pStream->pBfKey )
         hb_xfree( pStream->pBfKey );
      if( pStream->LZ4state )
         hb_xfree( pStream->LZ4state );
      if( pStream->IV )
      {
         memset( pStream->IV, 0, HB_BF_CIPHERBLOCK );
         hb_xfree( pStream->IV );
      }
      hb_xfree( pStream );
   }
}

/* create new stream structure, caller have to take care about compression level */
PHB_LZ4NET hb_lz4netOpen( int iLevel, int iStrategy )
{
   PHB_LZ4NET pStream = ( PHB_LZ4NET ) hb_xgrabz( sizeof( HB_LZ4NET ) );

   HB_SYMBOL_UNUSED( iStrategy );

   pStream->iErr = 0;
   pStream->iLevel = iLevel;
   pStream->ulBufLen = 0;
   pStream->pBuffer = NULL;
   pStream->pBfKey = NULL;
   if( iLevel > 0 )
#if 0  /* testing purpose only */
      pStream->LZ4state = hb_xgrab( LZ4_sizeofStateHC() );  /* LZ4_HC_STREAMSIZE */
#else
      pStream->LZ4state = hb_xgrab( LZ4_sizeofState() );  /* LZ4_STREAMSIZE */
#endif
   else
      pStream->LZ4state = NULL;

   return pStream;
}

/* create blowfish encryption key */
void hb_lz4netEncryptKey( PHB_LZ4NET pStream, const unsigned char * pPassword, int iPasslen )
{
   if( ! pStream->pBfKey && pPassword && iPasslen )
   {
#ifdef LZ4_BF_CBC
      HB_U32 xl, xr;
      int    i = 0;
#endif
      pStream->pBfKey = ( HB_BLOWFISH * ) hb_xgrab( sizeof( HB_BLOWFISH ) );
      hb_blowfishInit( pStream->pBfKey, pPassword, iPasslen );
#ifdef LZ4_BF_CBC
      /* fill IV (repeated) password, ECB-encrypt with itself */
      pStream->IV = ( char * ) hb_xgrab( HB_BF_CIPHERBLOCK );
      while( i < HB_BF_CIPHERBLOCK - 1 )
      {
         memcpy( pStream->IV + i, pPassword, HB_MIN( iPasslen, HB_BF_CIPHERBLOCK - i ) );
         i += HB_MIN( iPasslen, HB_BF_CIPHERBLOCK - i );
      }

      /* use LE instead HB_GET_BE_UINT32 */ 
      xl = HB_GET_LE_UINT32( &pStream->IV[ 0 ] );
      xr = HB_GET_LE_UINT32( &pStream->IV[ 4 ] );
      hb_blowfishEncrypt( pStream->pBfKey, &xl, &xr );
      HB_PUT_LE_UINT32( &pStream->IV[ 0 ], xl );
      HB_PUT_LE_UINT32( &pStream->IV[ 4 ], xr );
#endif
   }
}

static void lz4_bufSizer( PHB_LZ4NET pStream, HB_ULONG ulLen )
{
   if( ! pStream->ulBufLen )
   {
      pStream->ulBufLen = HB_MAX( ulLen, LZ4_BUFFER_DEFSIZE );
      pStream->pBuffer = ( char * ) hb_xgrab( pStream->ulBufLen );
   }
   else
   {
      pStream->ulBufLen = ulLen;
      pStream->pBuffer = ( char * ) hb_xrealloc( pStream->pBuffer, pStream->ulBufLen );
   }
}

static void lz4_destSizer( char ** pData, HB_ULONG ulLen )
{
   if( *pData )
      *pData = ( char * ) hb_xrealloc( *pData, ulLen + 1 );
   else
      *pData = ( char * ) hb_xgrab( ulLen + 1 );
}

/* capable of encrypting 'in place' without extra buffer
 * caller must ensure possible +8 bytes ready allocated */
static void lz4_bfEncrypt( const HB_BLOWFISH * pBfKey, const char * pSrc, register HB_SIZE nLen, char * pDest, HB_ULONG * pulLen, char ** cIV )
{
   if( pSrc && pDest )
   {
      if( nLen )
      {  /* possible binary data with possible 0 need ANSI X.923 padding */
         const HB_SIZE nSize = ( ( nLen >> 3 ) + 1 ) << 3;
         HB_U32 xl, xr;
#ifndef LZ4_BF_CBC
         HB_SYMBOL_UNUSED( cIV );
#endif

         *pulLen = ( HB_ULONG ) nSize;
         /* copy and padding of 8bytes last block, last byte = padded bytes */
         xr = nSize - nLen;  /* padded bytes */
         if( pSrc != pDest && xr < 8 )  /* source and target NOT the same */
            memcpy( pDest + nSize - 8, pSrc + nLen - ( 8 - xr ), 8 - xr );
         if( xr - 1 > 0 )
            memset( pDest + nLen, '\0', xr - 1 );
         pDest[ nSize - 1 ] = ( char ) xr;

#ifdef LZ4_BF_CBC
         /* move in the IV */
         xl = HB_GET_LE_UINT32( *cIV );
         xr = HB_GET_LE_UINT32( *cIV + 4 );
#endif

         /* except the last block -- changed from HB_GET_BE_UINT32 */
         for( nLen = 0; nLen < nSize - 8; nLen += 8 )
         {
#ifdef LZ4_BF_CBC
            xl = HB_GET_LE_UINT32( &pSrc[ nLen ] ) ^ xl;
            xr = HB_GET_LE_UINT32( &pSrc[ nLen + 4 ] ) ^ xr;
#else
            xl = HB_GET_LE_UINT32( &pSrc[ nLen ] );
            xr = HB_GET_LE_UINT32( &pSrc[ nLen + 4 ] );
#endif
            hb_blowfishEncrypt( pBfKey, &xl, &xr );
            HB_PUT_LE_UINT32( &pDest[ nLen ], xl );
            HB_PUT_LE_UINT32( &pDest[ nLen + 4 ], xr );
         }

         nLen = nSize - 8;
#ifdef LZ4_BF_CBC
         xl = HB_GET_LE_UINT32( &pDest[ nLen ] )  ^ xl;
         xr = HB_GET_LE_UINT32( &pDest[ nLen + 4 ] )  ^ xr;
#else
         xl = HB_GET_LE_UINT32( &pDest[ nLen ] );
         xr = HB_GET_LE_UINT32( &pDest[ nLen + 4 ] );
#endif
         hb_blowfishEncrypt( pBfKey, &xl, &xr );
         HB_PUT_LE_UINT32( &pDest[ nLen ], xl );
         HB_PUT_LE_UINT32( &pDest[ nLen + 4 ], xr );

#ifdef LZ4_BF_CBC
         /* move out the IV */
         HB_PUT_LE_UINT32( *cIV, xl );
         HB_PUT_LE_UINT32( *cIV + 4, xr );
#endif
      }
      else
      {
         *pulLen = 0;
         pDest[ 0 ] = '\0';
      }
   }
   else
      *pulLen = 0;
}

/* can decrypt in place without extra buffer */
static void lz4_bfDecrypt( const HB_BLOWFISH * pBfKey, const char * pSrc, const HB_SIZE nSize, char * pszData, HB_ULONG * pulLen, char ** cIV )
{
   if( pSrc && pszData )
   {
      if( nSize >= 8 && ( nSize & 0x07 ) == 0 )
      {
         register HB_SIZE nLen;
         HB_U32 xl, xr;
#ifdef LZ4_BF_CBC
         HB_U32 xor0 = HB_GET_LE_UINT32( *cIV );
         HB_U32 xor1 = HB_GET_LE_UINT32( *cIV + 4 );
         HB_U32 tin0, tin1;
#else
         HB_SYMBOL_UNUSED( cIV );
#endif

         /* changed from HB_GET_BE_UINT32 */
         for( nLen = 0; nLen < nSize; nLen += 8 )
         {
#ifdef LZ4_BF_CBC
            tin0 = xl = HB_GET_LE_UINT32( &pSrc[ nLen ] );
            tin1 = xr = HB_GET_LE_UINT32( &pSrc[ nLen + 4 ] );
#else
            xl = HB_GET_LE_UINT32( &pSrc[ nLen ] );
            xr = HB_GET_LE_UINT32( &pSrc[ nLen + 4 ] );
#endif
            hb_blowfishDecrypt( pBfKey, &xl, &xr );
#ifdef LZ4_BF_CBC
            HB_PUT_LE_UINT32( &pszData[ nLen ], xl ^ xor0 );
            xor0 = tin0;
            HB_PUT_LE_UINT32( &pszData[ nLen + 4 ], xr ^ xor1 );
            xor1 = tin1;
#else
            HB_PUT_LE_UINT32( &pszData[ nLen ], xl );
            HB_PUT_LE_UINT32( &pszData[ nLen + 4 ], xr );
#endif
         }
#ifdef LZ4_BF_CBC
         HB_PUT_LE_UINT32( *cIV, xor0 );
         HB_PUT_LE_UINT32( *cIV + 4, xor1 );
#endif
         /* validate result with number of padded bytes */
         xr = ( unsigned char ) pszData[ nSize - 1 ];
         nLen -= ( ( xr - 1 ) & ~0x07 ) == 0 ? xr : nLen;
         pszData[ nLen ] = '\0';
         *pulLen = ( HB_ULONG ) nLen;
      }
      else
      {
         *pulLen = 0;
         pszData[ 0 ] = '\0';
      }
   }
   else
      *pulLen = 0;
}

/* hb_lz4netEncryptTest: will a datablock be modified by compression and/or encryption ?
 * if not, and socket flag <MSG_MORE> is available, datablock can be send directly without memcpy() */
HB_BOOL hb_lz4netEncryptTest( const PHB_LZ4NET pStream, const HB_ULONG ulLen )
{
   return ( pStream && ( ( ulLen > LZ4_COMPRESS_MINLENGTH && pStream->iLevel > 0 ) || pStream->pBfKey ) );
}

/* for hb_lz4net[En|De]crypt() the caller is responsible to free 'too' big result blocks after send/ receive,
 * both functions commonly only REalloc an already allocated 'medium sized' buffer
 * no need to zero-ize the buffer, done internally in LZ4_compress_fast_extState() */

/* target pData ever start with 4 bytes HB_U32 [compressed] data length [ highest BIT = [un]compressed flag ]
 * source szData start at offset 0 with length ulLen
 * pulDataLen is already allocated size of target buffer pData */
HB_ULONG hb_lz4netEncrypt( PHB_LZ4NET pStream, char ** pData, HB_ULONG ulLen, HB_ULONG * pulDataLen, const char * szData )
{
   HB_ULONG  ulBufLen;
   HB_SIZE   nDest;
   HB_BOOL   fCompress = ( pStream->iLevel > 0 && ulLen > LZ4_COMPRESS_MINLENGTH );

   if( ! ulLen )
      return 0;
   else if( fCompress )
   {
      nDest = ( HB_SIZE ) LZ4_COMPRESSBOUND( ulLen ); /* value needed for compressing below */
      ulBufLen = nDest + 16;  /* add overhead: encrypt +8, uncomp-length +4, length +4 [, termination +1 ]  */
   }
   else if( ! pStream->pBfKey )  /* use raw uncompressed data, just copy and add length */
   {
      if( ulLen > *pulDataLen )  /* [re]alloc target */
      {
         *pulDataLen = ulLen;
         lz4_destSizer( pData, *pulDataLen );
      }
      if( szData )
      {
         memcpy( *pData + 4, szData, ulLen );
         *( *pData + 4 + ulLen ) = '\0';
      }
      HB_PUT_LE_UINT32( *pData, ulLen );

      return ulLen + 4;
   }
   else  /* only crypt, no compress */
   {
      nDest = 0;
      ulBufLen = ulLen + 8;
   }

   if( ulBufLen > *pulDataLen )  /* [re]alloc target */
   {
      *pulDataLen = ulBufLen;
      lz4_destSizer( pData, *pulDataLen );
   }

   if( fCompress )
   {
#if 0  /* testing purpose only */
      // nDest = LZ4_compress_fast( szData, *pData + 4, ulLen, nDest, pStream->iLevel );
      /* HighCompression algo - much! slower; need different LZ4_sizeofStateHC LZ4state */
      nDest = LZ4_compress_HC_extStateHC( pStream->LZ4state, szData, *pData + 4, ulLen, nDest, pStream->iLevel );
#else  /* newer version with external ( aka Harbour hb_xgrab ) allocated LZ4state */
      nDest = LZ4_compress_fast_extState( pStream->LZ4state, szData, *pData + 4, ulLen, nDest, pStream->iLevel );
#endif
      HB_PUT_LE_UINT32( *pData + 4 + nDest, ulLen );  /* trailing expanded data size */
      ulLen = nDest + 4;
      if( pStream->pBfKey )
         lz4_bfEncrypt( pStream->pBfKey, *pData + 4, ulLen, *pData + 4, &ulLen, &pStream->IV );
      HB_PUT_LE_UINT32( *pData, ulLen | 0x80000000 );  /* highest BIT: flag for compressed content */
      ulLen += 4;
   }
   else if( pStream->pBfKey )
   {
      lz4_bfEncrypt( pStream->pBfKey, szData, ulLen, *pData + 4, &ulLen, &pStream->IV );
      HB_PUT_LE_UINT32( *pData, ulLen );
      ulLen += 4;
   }

   *( *pData + ulLen ) = '\0';
   return ulLen;
}

/* done before by caller: fCompressed = ( ulLen & 0x80000000 ); ulLen &= 0x7FFFFFFF; */
/* pData == raw data, no length preleading
 * pulDataLen is already allocated size of source & target buffer: pData */
HB_ULONG hb_lz4netDecrypt( PHB_LZ4NET pStream, char ** pData, HB_ULONG ulLen, HB_ULONG * pulDataLen, HB_BOOL fCompressed )
{
   if( pStream->pBfKey )
   {
      if( fCompressed )  /* decrypt into tmp buffer */
      {
         if( ulLen > pStream->ulBufLen )
            lz4_bufSizer( pStream, ulLen );
         lz4_bfDecrypt( pStream->pBfKey, *pData, ulLen, pStream->pBuffer, &ulLen, &pStream->IV );
      }
      else  /* decrypt in place */
         lz4_bfDecrypt( pStream->pBfKey, *pData, ulLen, *pData, &ulLen, &pStream->IV );
   }
   else if( fCompressed )  /* need tmp buffer */
   {
      if( ulLen > pStream->ulBufLen )
         lz4_bufSizer( pStream, ulLen );
      memcpy( pStream->pBuffer, *pData, ulLen );
   }

   if( fCompressed )
   {
      HB_SIZE nSize = ulLen - 4;  /* raw compressed size, without trailing U32 */
      int     iCompressed;

      ulLen = HB_GET_LE_UINT32( pStream->pBuffer + nSize );  /* expanded size */
      if( ulLen > *pulDataLen )  /* [re]alloc target */
      {
         *pulDataLen = ulLen;
         lz4_destSizer( pData, *pulDataLen );
      }

#if 0  /* very negligible faster ?, but unsafe against malicious modified data */
      iCompressed = LZ4_decompress_fast( pStream->pBuffer, *pData, ulLen );
      if( ( HB_ULONG ) iCompressed != nSize )
#else  /* the safe version */
      iCompressed = LZ4_decompress_safe( pStream->pBuffer, *pData, nSize, ulLen );
      if( ( HB_ULONG ) iCompressed != ulLen )
#endif
      {
         pStream->iErr = iCompressed;
         ulLen = 0;
      }

      if( pStream->ulBufLen > LZ4_BUFFER_DEFSIZE )  /* downsize blown-up tmp buffer */
         lz4_bufSizer( pStream, LZ4_BUFFER_DEFSIZE );
      *( *pData + ulLen ) = '\0';
   }

   return ulLen;
}

