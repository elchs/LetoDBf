/*
 * Leto db server functions
 *
 * Copyright 2009 Alexander S. Kresin <alex / at / belacy.belgorod.su>
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

#include "srvleto.h"

#define ACC_REALLOC  20

static const char * szOk = "++++";
static const char * szErr2 = "-002";
static const char * szErr3 = "-003";
static const char * szErr4 = "-004";
static const char * szErrAcc = "-ACC";

static PACCSTRU  s_acc = NULL;
static HB_USHORT s_uiAccAlloc = 0;          // Number of allocated account structures
static HB_USHORT s_uiAccCurr = 0;           // Current number of accounts
static HB_BOOL   s_fAccUpdated = 0;
static char *    s_pAccPath = NULL;

static HB_BOOL   s_fLockConnect = HB_FALSE;

#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   static HB_SPINLOCK_T s_accMtx = HB_SPINLOCK_INIT;
   #define HB_GC_LOCKA()    HB_SPINLOCK_ACQUIRE( &s_accMtx )
   #define HB_GC_UNLOCKA()  HB_SPINLOCK_RELEASE( &s_accMtx )
#else
   static HB_CRITICAL_NEW( s_accMtx );
   #define HB_GC_LOCKA()    hb_threadEnterCriticalSection( &s_accMtx )
   #define HB_GC_UNLOCKA()  hb_threadLeaveCriticalSection( &s_accMtx )
#endif

extern int iDebugMode( void );
extern int leto_GetParam( char * szData, ... );
extern void leto_SendAnswer( PUSERSTRU pUStru, const char * szData, HB_ULONG ulLen );
extern HB_BOOL leto_ServerLock( PUSERSTRU pUStru, HB_BOOL bLock, int iSecs );
extern HB_BOOL leto_CheckPass( int iType );
extern void leto_wUsLog( PUSERSTRU pUStru, int n, const char* s, ... );


HB_BOOL leto_ConnectIsLock( void )
{
   return s_fLockConnect;
}

char * leto_memoread( const char * szFilename, HB_ULONG * pulLen )
{
   HB_FHANDLE fhnd;
   char *     pBuffer = NULL;

   fhnd = hb_fsOpen( szFilename, FO_READ | FO_SHARED );
   if( fhnd != FS_ERROR )
   {
      *pulLen = hb_fsSeek( fhnd, 0, FS_END );
      pBuffer = ( char * ) hb_xgrab( *pulLen + 1 );

      hb_fsSeek( fhnd, 0, FS_SET );
      if( hb_fsReadLarge( fhnd, pBuffer, *pulLen ) != *pulLen )
      {
         hb_xfree( pBuffer );
         *pulLen = 0;
         pBuffer = NULL;
      }
      else
      {
         if( *pulLen && pBuffer[ *pulLen - 1 ] == 26 )
            *pulLen -= 1;
         pBuffer[ *pulLen ] = '\0';
      }
      hb_fsClose( fhnd );
   }
   else
      *pulLen = 0;

   return pBuffer;
}

HB_BOOL leto_memowrite( const char * szFilename, const char * pBuffer, HB_ULONG ulLen )
{
   HB_FHANDLE fhnd;
   HB_BOOL    bRetVal = HB_FALSE;

   if( ulLen && ( fhnd = hb_fsCreate( szFilename, FC_NORMAL ) ) != FS_ERROR )
   {
      bRetVal = ( hb_fsWriteLarge( fhnd, pBuffer, ulLen ) == ulLen );
      if( bRetVal )
      {
         char cStrgZ[ 1 ];

         cStrgZ[ 0 ] = 26;
         bRetVal = ( hb_fsWriteLarge( fhnd, cStrgZ, 1 ) == 1 );
      }
      hb_fsClose( fhnd );
   }

   return bRetVal;
}

HB_BOOL leto_fileread( const char * szFilename, char * pBuffer, const HB_ULONG ulStart, HB_ULONG * pulLen )
{
#if defined( __HARBOUR30__ )
   HB_FHANDLE fhnd;
#else
   PHB_FILE   pFile;
#endif
   HB_BOOL    bRes = HB_FALSE;
   HB_ULONG   ulLen = *pulLen;

   pBuffer[ 0 ] = '\0';
   *pulLen = 0;

   if( ! ulLen )
      return HB_TRUE;

#if defined( __HARBOUR30__ )
   fhnd = hb_fsOpen( szFilename, FO_READ | FO_SHARED );
   if( fhnd != FS_ERROR )
   {
      if( ( HB_ULONG ) hb_fsSeekLarge( fhnd, ulStart, FS_SET ) == ulStart )
      {
         *pulLen = hb_fsReadLarge( fhnd, ( void * ) pBuffer, ulLen );
         pBuffer[ *pulLen ] = '\0';
         bRes = HB_TRUE;
      }
      hb_fsClose( fhnd );
   }
#else
   if( ( pFile = hb_fileExtOpen( szFilename, NULL, FO_READ | FO_SHARED, NULL, NULL ) ) != NULL )
   {
      if( ( HB_ULONG ) hb_fileSeek( pFile, ulStart, FS_SET ) == ulStart )
      {
         *pulLen = hb_fileRead( pFile, ( void * ) pBuffer, ulLen, -1 );
         pBuffer[ *pulLen ] = '\0';
         bRes = HB_TRUE;
      }
      hb_fileClose( pFile );
   }
#endif

   return bRes;
}

HB_BOOL leto_filewrite( const char * szFilename, const char * pBuffer, const HB_ULONG ulStart, HB_ULONG ulLen, HB_BOOL bTrunc )
{
   HB_BOOL bRetVal = HB_FALSE;

#if defined( __HARBOUR30__ )
   HB_FHANDLE fhnd;
   HB_USHORT  uiFlags = FO_CREAT | FO_WRITE;

   if( bTrunc )
      uiFlags |= FO_TRUNC;

   if( ( fhnd = hb_fsOpen( szFilename, uiFlags ) ) != FS_ERROR )
   {
      if( ( HB_ULONG ) hb_fsSeekLarge( fhnd, ulStart, FS_SET ) == ulStart )
         bRetVal = ( hb_fsWriteLarge( fhnd, pBuffer, ulLen ) == ulLen );
      hb_fsClose( fhnd );
   }
#else
   PHB_FILE pFile;
   HB_FATTR nMode = FO_READWRITE | FO_DENYNONE | FXO_SHARELOCK;

   if( bTrunc )
      nMode |= FXO_TRUNCATE;
   else
      nMode |= FXO_APPEND;

   if( ( pFile = hb_fileExtOpen( szFilename, NULL, nMode, NULL, NULL ) ) != NULL )
   {
      HB_SIZE nLen = hb_fileWriteAt( pFile, pBuffer, ulLen, ( HB_FOFFSET ) ulStart );

      if( nLen != ( HB_SIZE ) FS_ERROR && nLen == ulLen )
         bRetVal = HB_TRUE;
      hb_fileClose( pFile );
   }
#endif

   return bRetVal;
}

HB_BOOL leto_filesize( const char * szFilename, HB_ULONG * pulLen )
{
   HB_BOOL bRes = HB_FALSE;

#if defined( __HARBOUR30__ )
   HB_FHANDLE fhnd;

   if( szFilename )
      fhnd = hb_fsOpen( szFilename, FO_READ | FO_SHARED );
   else
      fhnd = FS_ERROR;
   if( fhnd != FS_ERROR )
   {
      *pulLen = ( HB_ULONG ) hb_fsSeekLarge( fhnd, 0L, FS_END );
      bRes = HB_TRUE;
      hb_fsClose( fhnd );
   }
#else
   PHB_FILE pFile;

   if( szFilename && ( pFile = hb_fileExtOpen( szFilename, NULL, FO_READ | FO_SHARED, NULL, NULL ) ) != NULL )
   {
      *pulLen = ( HB_ULONG ) hb_fileSeek( pFile, 0L, FS_END );
      bRes = HB_TRUE;
      hb_fileClose( pFile );
   }
   else if( szFilename )  /* perhaps excl. opened ? -- try with directory info */
   {
      PHB_ITEM pDir = hb_fileDirectory( szFilename, "" );

      if( HB_IS_ARRAY( pDir ) && hb_arrayLen( pDir ) > 0 )
      {
         PHB_ITEM pEntry = hb_itemNew( NULL );

         if( hb_arrayGet( pDir, 1, pEntry ) )
         {
            if( hb_arrayLen( pDir ) >= 4 )
            {
               *pulLen = hb_arrayGetNL( pEntry, 2 );
               bRes = HB_TRUE;
            }
         }
         hb_itemRelease( pEntry );
      }
      if( pDir )
         hb_itemRelease( pDir );
   }
#endif
   if( ! bRes )
      *pulLen = 0L;
   return bRes;
}

/* called only once by starting master thread over leto_acc_setPath() */
static void leto_acc_read( const char * szFilename )
{
   HB_ULONG ulLen;
   char *   pBuffer = NULL;

   leto_filesize( szFilename, &ulLen );
   if( ulLen )
   {
      pBuffer = ( char * ) hb_xgrab( ulLen + 1 );
      leto_fileread( szFilename, pBuffer, 0, &ulLen );
      if( ulLen )
         pBuffer[ ulLen ] = '\0';
   }

   if( ulLen )
   {
      const char * ptr, * ptrEnd;
      HB_USHORT    uiUsers = 0;

      leto_cryptReset( HB_TRUE );
      leto_decrypt( pBuffer, ulLen, pBuffer, &ulLen, __RANDOM_STRING__, HB_TRUE );
      leto_cryptReset( HB_TRUE );
      ptr = pBuffer;

      while( *ptr )
      {
         if( *ptr >= '?' )
            uiUsers++;
         if( ( ptr = strchr( ptr, '\n' ) ) != NULL )
         {
            while( *ptr == '\r' || *ptr == '\n' )
               ptr++;
         }
      }

      s_uiAccAlloc = ( HB_USHORT ) ( uiUsers + ACC_REALLOC );
      s_acc = ( ACCSTRU * ) hb_xgrabz( sizeof( ACCSTRU ) * s_uiAccAlloc );

      ptr = pBuffer;
      uiUsers = 0;

      while( *ptr )
      {
         if( *ptr < '?' )
            ptr = strchr( ptr, '\n' );
         else
         {
            ptrEnd = strchr( ptr, ';' );
            if( *ptrEnd && ( ptrEnd - ptr ) )
            {
               char * szUser;

               s_acc[ uiUsers ].uiUslen = ( HB_USHORT ) ( ptrEnd - ptr );
               szUser = ( char * ) hb_xgrab( s_acc[ uiUsers ].uiUslen + 1 );

               memcpy( szUser, ptr, s_acc[ uiUsers ].uiUslen );
               szUser[ s_acc[ uiUsers ].uiUslen ] = '\0';
               s_acc[ uiUsers ].szUser = szUser;
               ptr = ptrEnd + 1;
               ptrEnd = strchr( ptr, ';' );
               if( *ptrEnd )
               {
                  ulLen = ( HB_ULONG ) ( ptrEnd - ptr );
                  if( ulLen > 7 ) // && ulLen <= 48 )
                  {
                     char * szPassword = ( char * ) hb_xgrab( ulLen + 1 );

                     memcpy( szPassword, ptr, ulLen );
                     szPassword[ ulLen ] = '\0';
                     s_acc[ uiUsers ].szPass = szPassword;
                     s_acc[ uiUsers ].uiPasslen = ( HB_USHORT ) ulLen;
                  }
                  else
                  {
                     s_acc[ uiUsers ].szPass = NULL;
                     s_acc[ uiUsers ].uiPasslen = 0;
                  }

                  if( s_acc[ uiUsers ].uiUslen )
                  {
                     ptr = ptrEnd + 1;
                     ptrEnd = strchr( ptr, ';' );
                     if( *ptrEnd )
                     {
                        int i;

                        s_acc[ uiUsers ].szAccess[ 0 ] = s_acc[ uiUsers ].szAccess[ 1 ] = '\0';
                        for( i = 0; i < 8 && *ptr != ';'; i++, ptr++ )
                        {
                           if( *ptr == 'y' || *ptr == 'Y' )
                              s_acc[ uiUsers ].szAccess[ 0 ] |= ( 1 << i );
                        }
                        uiUsers++;
                     }
                  }
               }
            }

            ptr = strchr( ptr, '\n' );
         }
         while( ptr && ( *ptr == '\r' || *ptr == '\n' ) )
            ptr++;
      }
      s_uiAccCurr = uiUsers;
   }

   if( pBuffer )
      hb_xfree( pBuffer );
}

HB_BOOL leto_acc_find( PUSERSTRU pUStru, const char * szPass )
{
   const char * szUser = ( char * ) pUStru->szUsername;
   const char * szRet = NULL;
   HB_USHORT    uiLen = ( HB_USHORT ) strlen( szUser );
   PACCSTRU     pacc;

   if( s_acc && uiLen )
   {
      HB_BOOL fRes = HB_FALSE;
      int     i;

      HB_GC_LOCKA();

      for( i = 0, pacc = s_acc; i < s_uiAccCurr; i++, pacc++ )
      {
         if( uiLen == pacc->uiUslen && ! strncmp( szUser, pacc->szUser, uiLen ) )
         {
            fRes = HB_TRUE;
            if( pacc->uiPasslen )
            {
               char *   pBuf = ( char * ) hb_xgrab( ( pacc->uiPasslen / 2 ) + 1 );
               HB_ULONG ulLen = 0;

               leto_hexchar2byte( pacc->szPass, pacc->uiPasslen, pBuf );
               leto_cryptReset( HB_TRUE );
               leto_decrypt( pBuf, pacc->uiPasslen / 2, pBuf, &ulLen, __RANDOM_STRING__, HB_TRUE );
               leto_cryptReset( HB_TRUE );

               if( ulLen && ! strncmp( szPass, pBuf, ulLen ) )
                  szRet = pacc->szAccess;

               memset( pBuf, 0, pacc->uiPasslen / 2 );  /* bye bye */
               hb_xfree( pBuf );
            }
            else
               szRet = pacc->szAccess;
            break;
         }
      }

      HB_GC_UNLOCKA();

      if( ! fRes && iDebugMode() > 0 )
         leto_writelog( NULL, -1, "INFO: unknown user %s access denied", szUser );
      else if( ! szRet && iDebugMode() > 0 )
         leto_writelog( NULL, -1, "INFO: user %s : wrong password, access denied", szUser );
   }

   if( szRet )
   {
      memcpy( pUStru->szAccess, szRet, 2 );
      return HB_TRUE;
   }

   return HB_FALSE;
}

static HB_BOOL leto_acc_add( const char * szUser, const char * szPass, const char * szAccess )
{
   int       i;
   HB_USHORT uiLen = ( HB_USHORT ) strlen( szUser );
   PACCSTRU  pacc;
   HB_BOOL   fRes = HB_TRUE;

   HB_GC_LOCKA();
   if( uiLen > 0 && s_acc )
   {
      for( i = 0, pacc = s_acc; i < s_uiAccCurr; i++, pacc++ )
      {
         if( uiLen == pacc->uiUslen && ! strncmp( szUser, pacc->szUser, uiLen ) )
         {
            fRes = HB_FALSE;
            break;
         }
      }
   }

   if( fRes && uiLen > 0 )
   {
      char * szUsername = ( char * ) hb_xgrab( uiLen + 1 );

      if( ! s_acc )
      {
         s_uiAccAlloc = ACC_REALLOC;
         s_acc = ( ACCSTRU * ) hb_xgrabz( sizeof( ACCSTRU ) * s_uiAccAlloc );
      }
      if( s_uiAccCurr == s_uiAccAlloc )
      {
         s_acc = ( ACCSTRU * ) hb_xrealloc( s_acc, sizeof( ACCSTRU ) * ( s_uiAccAlloc + ACC_REALLOC ) );
         memset( s_acc + s_uiAccAlloc, 0, sizeof( ACCSTRU ) * ACC_REALLOC );
         s_uiAccAlloc += ACC_REALLOC;
      }
      pacc = s_acc + s_uiAccCurr;
      pacc->uiUslen = uiLen;
      strcpy( szUsername, szUser );
      pacc->szUser = szUsername;

      uiLen = szPass ? ( HB_USHORT ) strlen( szPass ) : 0;
      if( uiLen > 0 )
      {
         char *   pBuf = ( char * ) hb_xgrab( uiLen + 8 );
         char *   szPassword;
         HB_ULONG ulLen;

         leto_cryptReset( HB_TRUE );
         leto_encrypt( szPass, uiLen, pBuf, &ulLen, __RANDOM_STRING__, HB_TRUE );
         leto_cryptReset( HB_TRUE );
         szPassword = ( char * ) hb_xgrab( ( ulLen * 2 ) + 1 );
         leto_byte2hexchar( pBuf, ( int ) ulLen, szPassword );
         pacc->uiPasslen = ( HB_USHORT ) ulLen * 2;
         szPassword[ pacc->uiPasslen ] = '\0';
         pacc->szPass = szPassword;

         hb_xfree( pBuf );
      }
#if 0
      else
      {
         pacc->szPass = NULL;
         pacc->szPass[ uiLen ] = '\0';
      }
#endif

      pacc->szAccess[ 0 ] = pacc->szAccess[ 1 ] = '\0';
      if( szAccess && *szAccess )
      {
         for( i = 0; i < 8 && *szAccess; i++, szAccess++ )
         {
            if( *szAccess == 'Y' || *szAccess == 'y' || *szAccess == 't' || *szAccess == 'T' )
               pacc->szAccess[ 0 ] |= ( 1 << i );
         }
      }

      s_uiAccCurr++;
      s_fAccUpdated = HB_TRUE;
      fRes = HB_TRUE;
   }
   HB_GC_UNLOCKA();

   return fRes;
}

static HB_BOOL leto_acc_accdel( const char * szUser )
{
   HB_USHORT uiLen = ( HB_USHORT ) strlen( szUser );
   PACCSTRU  pacc;
   HB_BOOL   fRes = HB_FALSE;

   HB_GC_LOCKA();
   if( s_acc )
   {
      int i;

      for( i = 0, pacc = s_acc; i < s_uiAccCurr; i++, pacc++ )
      {
         if( uiLen == pacc->uiUslen && ! strncmp( szUser, pacc->szUser, uiLen ) )
         {
            hb_xfree( pacc->szUser );
            if( pacc->szPass )
               hb_xfree( pacc->szPass );
            pacc->uiUslen = 0;
            pacc->uiPasslen = 0;
            s_fAccUpdated = HB_TRUE;
            fRes = HB_TRUE;
            break;
         }
      }

      if( fRes && i < s_uiAccCurr - 1 )
      {
         PACCSTRU paccMove = pacc;

         while( i < s_uiAccCurr - 1 )
         {
            pacc++;
            paccMove->uiUslen = pacc->uiUslen;
            paccMove->uiPasslen = pacc->uiPasslen;
            paccMove->szUser = pacc->szUser;
            paccMove->szPass = pacc->szPass;
            paccMove->szAccess[ 0 ] = pacc->szAccess[ 0 ];
            paccMove = pacc;
            i++;
         }
         paccMove->uiUslen = 0;
         paccMove->uiPasslen = 0;
         paccMove->szUser = NULL;
         paccMove->szPass = NULL;
         paccMove->szAccess[ 0 ] = '\0';
      }

      if( fRes )
         s_uiAccCurr--;

   }
   HB_GC_UNLOCKA();

   return fRes;
}

static HB_BOOL leto_acc_setpass( const char * szUser, const char * szPass )
{
   HB_USHORT uiLen = ( HB_USHORT ) strlen( szUser );
   PACCSTRU  pacc;
   HB_BOOL   fRes = HB_FALSE;

   HB_GC_LOCKA();
   if( s_acc )
   {
      int i;

      for( i = 0, pacc = s_acc; i < s_uiAccCurr; i++, pacc++ )
      {
         if( uiLen == pacc->uiUslen && ! strncmp( szUser, pacc->szUser, uiLen ) )
         {
            if( pacc->szPass )
               hb_xfree( pacc->szPass );
            uiLen = szPass ? ( HB_USHORT ) strlen( szPass ) : 0;
            if( uiLen )
            {
               char *   pBuf = ( char * ) hb_xgrab( uiLen + 8 );
               char *   szPassword;
               HB_ULONG ulLen;

               leto_cryptReset( HB_TRUE );
               leto_encrypt( szPass, uiLen, pBuf, &ulLen, __RANDOM_STRING__, HB_TRUE );
               leto_cryptReset( HB_TRUE );
               szPassword = ( char * ) hb_xgrab( ( ulLen * 2 ) + 1 );
               leto_byte2hexchar( pBuf, ( int ) ulLen, szPassword );
               pacc->uiPasslen = ( HB_USHORT ) ulLen * 2;
               szPassword[ pacc->uiPasslen ] = '\0';
               pacc->szPass = szPassword;
               hb_xfree( pBuf );
               s_fAccUpdated = HB_TRUE;
            }
            else
            {
               pacc->szPass = NULL;
               pacc->uiPasslen = 0;
            }
            fRes = HB_TRUE;
            break;
         }
      }
   }
   HB_GC_UNLOCKA();

   return fRes;
}

static HB_BOOL leto_acc_setacc( const char * szUser, const char * szAccess )
{
   HB_USHORT uiLen = ( HB_USHORT ) strlen( szUser );
   PACCSTRU  pacc;
   HB_BOOL   fRes = HB_FALSE;

   HB_GC_LOCKA();
   if( s_acc )
   {
      int i;

      for( i = 0, pacc = s_acc; i < s_uiAccCurr; i++, pacc++ )
      {
         if( uiLen == pacc->uiUslen && ! strncmp( szUser, pacc->szUser, uiLen ) )
         {
            if( szAccess && *szAccess )
            {
               pacc->szAccess[ 0 ] = '\0';
               for( i = 0; i < 8 && *szAccess; i++, szAccess++ )
               {
                  if( *szAccess == 'y' || *szAccess == 'Y' || *szAccess == 't' || *szAccess == 'T' )
                     pacc->szAccess[ 0 ] |= ( 1 << i );
               }
            }
            s_fAccUpdated = HB_TRUE;
            fRes = HB_TRUE;
            break;
         }
      }
   }
   HB_GC_UNLOCKA();

   return fRes;
}

/* called only once by starting master thread */
void leto_acc_setPath( const char * szPath )
{
   if( szPath )
   {
      HB_USHORT uiLen = ( HB_USHORT ) strlen( szPath );

      if( uiLen > 0 )
      {
         s_pAccPath = ( char * ) hb_xgrab( uiLen + 1 );
         memcpy( s_pAccPath, szPath, uiLen );
      }
      s_pAccPath[ uiLen ] = '\0';
      leto_acc_read( szPath );
   }
}

static HB_BOOL leto_acc_flush( void )
{
   PACCSTRU   pacc;
   char       szBuf[ 128 ], * ptr;
   char *     pData = NULL;
   HB_ULONG   ulLen;
   HB_ULONG   ulLenLen = 0;
   HB_BOOL    fRes = HB_FALSE;

   HB_GC_LOCKA();
   if( s_acc && s_fAccUpdated && s_pAccPath )
   {
      int i, j;

      for( i = 0, pacc = s_acc; i < s_uiAccCurr; i++, pacc++ )
      {
         memcpy( szBuf, pacc->szUser, pacc->uiUslen );
         ptr = szBuf + pacc->uiUslen;
         *ptr++ = ';';
         if( pacc->uiPasslen )
         {
            memcpy( ptr, pacc->szPass, pacc->uiPasslen );
            ptr += pacc->uiPasslen;
         }
         *ptr++ = ';';
         for( j = 0; j < 8; j++ )
            *ptr++ = ( pacc->szAccess[ 0 ] & ( 1 << j ) ) ? 'Y' : 'N';
         *ptr++ = ';';
#if defined( HB_OS_WIN_32 ) || defined( HB_OS_WIN )
         *ptr++ = '\r';
#endif
         *ptr++ = '\n';

         if( ulLenLen == 0 )
         {
            ulLenLen = ptr - szBuf;
            pData = ( char * ) hb_xgrab( ulLenLen );
            memcpy( pData, szBuf, ulLenLen );
         }
         else
         {
            ulLen = ptr - szBuf;
            pData = ( char * ) hb_xrealloc( pData, ulLenLen + ulLen );
            memcpy( pData + ulLenLen, szBuf, ulLen );
            ulLenLen += ulLen;
         }
      }

      if( ulLenLen )
      {
         pData = ( char * ) hb_xrealloc( pData, ulLenLen + 9 );
         leto_cryptReset( HB_TRUE );
         leto_encrypt( ( const char * ) pData, ( HB_ULONG ) ulLenLen, pData, &ulLenLen, __RANDOM_STRING__, HB_TRUE );
         leto_cryptReset( HB_TRUE );

         fRes = leto_filewrite( s_pAccPath, pData, 0, ulLenLen, HB_TRUE );
         hb_xfree( pData );
      }

   }
   HB_GC_UNLOCKA();

   return fRes;
}

/* called only once by master thread EXIT procedure */
void leto_acc_release( void )
{
   leto_acc_flush();

   if( s_acc )
   {
      PACCSTRU pacc;
      int      i;

      for( i = 0, pacc = s_acc; i < s_uiAccCurr; i++, pacc++ )
      {
         hb_xfree( pacc->szUser );
         if( pacc->szPass )
            hb_xfree( pacc->szPass );
      }

      hb_xfree( s_acc );
   }
   if( s_pAccPath )
      hb_xfree( s_pAccPath );
}

void leto_Admin( PUSERSTRU pUStru, char * szData )
{
   const char * pData;
   char         * pp1, * pp2, * pp3;
   HB_ULONG     ulLen;
   int          nParam = leto_GetParam( szData, &pp1, &pp2, &pp3, NULL );

   if( leto_CheckPass( 2 ) || ( pUStru->szAccess[ 0 ] & 1 ) )
   {
      if( ! nParam )
         pData = szErr2;
      else
      {
         if( ! strncmp( szData, "uadd", 4 ) || ! strncmp( szData, "upsw", 4 ) )
         {
            if( ( *( szData + 1 ) == 'a' && nParam < 3 ) || nParam < 2 )
               pData = szErr4;
            else
            {
               ulLen = strlen( pp2 );
               if( ulLen <= 80 )
               {
                  char * szPass = NULL;

                  if( ulLen )
                  {
                     char * szKey = leto_localKey( pUStru->cDopcode, LETO_DOPCODE_LEN );

                     szPass = ( char * ) hb_xgrab( ( ulLen / 2 ) + 1 );
                     leto_hexchar2byte( pp2, ulLen, szPass );
                     leto_decrypt( szPass, ulLen / 2, szPass, &ulLen, szKey, HB_FALSE );
                     if( szKey )
                        hb_xfree( szKey );
                  }
                  if( *( szData + 1 ) == 'a' )
                  {
                     if( leto_acc_add( pp1, szPass, pp3 ) )
                        pData = szOk;
                     else
                        pData = szErr4;
                  }
                  else
                  {
                     if( leto_acc_setpass( pp1, szPass ) )
                        pData = szOk;
                     else
                        pData = szErr4;
                  }
                  if( szPass )
                     hb_xfree( szPass );
               }
               else
                  pData = szErr4;
            }
         }
         else if( ! strncmp( szData, "uacc", 4 ) )
         {
            if( nParam < 3 )
               pData = szErr4;
            else
            {
               if( leto_acc_setacc( pp1, pp2 ) )
                  pData = szOk;
               else
                  pData = szErr4;
            }
         }
         else if( ! strncmp( szData, "udel", 4 ) )
         {
            if( nParam < 2 )
               pData = szErr4;
            else
            {
               if( leto_acc_accdel( pp1 ) )
                  pData = szOk;
               else
                  pData = szErr4;
            }
         }
         else if( ! strncmp( szData, "flush", 5 ) )
         {
            if( leto_acc_flush() )
               pData = szOk;
            else
               pData = szErr4;
         }
         else if( ! strncmp( szData, "lockc", 5 ) )
         {
            if( *pp1 == 'T' || *pp1 == 'F' )
            {
               s_fLockConnect = ( *pp1 == 'T' );
               pData = szOk;
            }
            else if( s_fLockConnect )
              pData = szOk;
            else
              pData = szErr4;
         }
         else if( ! strncmp( szData, "lockl", 5 ) )
         {
            HB_BOOL bLock;

            if( *pp1 != '?' )
            {
               bLock = ( *pp1 == 'T' );
               bLock = leto_ServerLock( pUStru, bLock, atoi( pp2 ) );
            }
            else
               bLock = leto_ServerLock( NULL, HB_FALSE, 0 );
            pData = bLock ? szOk : szErr4;
         }
         else
            pData = szErr3;
      }
   }
   else
      pData = szErrAcc;

   leto_SendAnswer( pUStru, pData, 4 );
}

void leto_ToggleZip( PUSERSTRU pUStru, char * szData )
{
   int      iZipRecord = atoi( szData );
   HB_ULONG ulLen;
   char *   pp1;

   leto_GetParam( szData, &pp1, NULL );

#ifdef USE_LZ4
   if( iZipRecord >= -1 && iZipRecord <= 15 )
#else
   if( iZipRecord >= -1 && iZipRecord <= 9 )
#endif
   {
      /* must send answer with *old* setting !! */
      leto_SendAnswer( pUStru, szOk, 4 );
      if( pUStru->zstream )
      {
#ifdef USE_LZ4
         hb_lz4netClose( ( PHB_LZ4NET ) pUStru->zstream );
#else
         hb_znetClose( pUStru->zstream );
#endif
         pUStru->zstream = NULL;
         pUStru->bZipCrypt = HB_FALSE;
      }

      pUStru->iZipRecord = iZipRecord;
#ifdef USE_LZ4
      if( pUStru->iZipRecord >= 0 && ! pUStru->zstream )
         pUStru->zstream = hb_lz4netOpen( pUStru->iZipRecord, HB_ZLIB_STRATEGY_DEFAULT );
#else
      if( pUStru->iZipRecord > 0 && ! pUStru->zstream )
         pUStru->zstream = hb_znetOpen( pUStru->iZipRecord, HB_ZLIB_STRATEGY_DEFAULT );
#endif

      ulLen = pp1 ? strlen( pp1 ) : 0;
      if( pUStru->zstream && ulLen )
      {
         char * szKey = leto_localKey( pUStru->cDopcode, LETO_DOPCODE_LEN );
         char * szPass = ( char * ) hb_xgrabz( ( ulLen / 2 ) + 1 );
         int    i;

         leto_hexchar2byte( pp1, ( int ) ulLen, szPass );
         leto_decrypt( szPass, ulLen / 2, szPass, &ulLen, szKey, HB_FALSE );
         for( i = 0; i < HB_MIN( LETO_DOPCODE_LEN, ( int ) ulLen ); i++ )
         {
            szPass[ i ] ^= pUStru->cDopcode[ i ];
         }

#ifdef USE_LZ4
         hb_lz4netEncryptKey( ( PHB_LZ4NET ) pUStru->zstream, szPass, ( int ) ulLen );
#else
         hb_znetEncryptKey( pUStru->zstream, szPass, ulLen );
#endif
         memset( szPass, 0, ulLen );
         if( szKey )
         {
            memset( szKey, 0, LETO_DOPCODE_LEN );
            hb_xfree( szKey );
         }
         hb_xfree( szPass );

         pUStru->bZipCrypt = HB_TRUE;
      }

      return;
   }

   leto_SendAnswer( pUStru, szErr2, 4 );
}

