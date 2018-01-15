/*
 * Harbour Leto management functions
 *
 * Copyright 2008 Pavel Tsarenko <tpe2 / at / mail.ru>
 *           2015-16 Rolf 'elch' Beckmann
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

#include "rddleto.h"

#define LETOVAR_LOG      '1'
#define LETOVAR_NUM      '2'
#define LETOVAR_STR      '3'
#define LETOVAR_ARR      '4'
#define LETOVAR_DAT      '5'
#ifndef __XHARBOUR__
   #define LETOVAR_TYPES    ( HB_IT_LOGICAL | HB_IT_NUMERIC | HB_IT_STRING | HB_IT_ARRAY | HB_IT_DATE )
#else
   #define LETOVAR_TYPES    ( HB_IT_LOGICAL | HB_IT_NUMERIC | HB_IT_STRING | HB_IT_DATE )
#endif

extern LETOCONNECTION * letoGetConnPool( HB_UINT uiConnection );
extern LETOCONNECTION * letoGetCurrConn( void );
extern LETOCONNECTION * leto_getConnection( int iParam );
extern HB_USHORT uiGetConnCount( void );

extern void leto_ConnectionClose( LETOCONNECTION * pConnection );
extern void letoClearCurrConn( void );

extern void leto_clientlog( const char * sFile, int n, const char * s, ... );
extern HB_BOOL leto_Ping( LETOCONNECTION * pConnection );


#ifndef LETO_NO_MT
/* for TSD thread local storage last used pItem cache, e.g. for use in filter conditions */
static void leto_InitVarCache( void * cargo )
{
   PHB_ITEM * ppItemTSD = ( PHB_ITEM * ) cargo;

   *ppItemTSD = NULL;
}

static void leto_DeinitVarCache( void * cargo )
{
   PHB_ITEM * ppItemTSD = ( PHB_ITEM * ) cargo;

   if( *ppItemTSD )
   {
      hb_itemRelease( *ppItemTSD );
      *ppItemTSD = NULL;
   }
}

static HB_TSD_NEW( s_TSDitem, sizeof( PHB_ITEM * ), leto_InitVarCache, leto_DeinitVarCache );

static void leto_SetVarCache( PHB_ITEM pVarItem )
{
   PHB_ITEM * ppItemTSD = hb_stackGetTSD( &s_TSDitem );

   if( *ppItemTSD )
      hb_itemRelease( *ppItemTSD );
   *ppItemTSD = pVarItem;
}

static void leto_ClearVarCache( void )
{
   PHB_ITEM * ppItemTSD = hb_stackGetTSD( &s_TSDitem );

   if( ppItemTSD )
   {
      hb_itemRelease( *ppItemTSD );
      *ppItemTSD = NULL;
   }
}

#else  /* ! LETO_NO_MT */

static PHB_ITEM s_TSDitem = NULL;

static void leto_SetVarCache( PHB_ITEM pVarItem )
{
   if( s_TSDitem )
      hb_itemRelease( s_TSDitem );
   s_TSDitem = pVarItem;
}

static void leto_ClearVarCache( void )
{
   if( s_TSDitem )
   {
      hb_itemRelease( s_TSDitem );
      s_TSDitem = NULL;
   }
}

// ToDO:  static void leto_DeinitVarCache( void * cargo )
#endif

static LETOCONNECTION * letoParseFile( const char * szSource, char * szFile )
{
   LETOCONNECTION * pConnection = NULL;
   char szAddr[ 96 ];
   int  iPort = 0;

   HB_TRACE( HB_TR_DEBUG, ( "letoParseFile(%s, %s)", szSource, szFile ) );

   if( leto_getIpFromPath( szSource, szAddr, &iPort, szFile ) )
   {
       if( ( pConnection = leto_ConnectionFind( szAddr, iPort ) ) == NULL )
          pConnection = LetoConnectionNew( szAddr, iPort, NULL, NULL, 0, LETO_USE_THREAD );
   }
   else
      hb_strncpy( szFile, leto_RemoveIpFromPath( szSource ), HB_PATH_MAX - 1 );

   return pConnection;
}

static LETOCONNECTION * letoParseParam( const char * szParam, char * szFile )
{
   LETOCONNECTION * pConnection;

   if( ! szParam || ! *szParam )
   {
      pConnection = letoGetCurrConn();
      szFile[ 0 ] = '\0';
   }
   else if( strlen( szParam ) > 10 && szParam[ 0 ] == '/' && szParam[ 1 ] == '/' )
      pConnection = letoParseFile( szParam, szFile );
   else
   {
      pConnection = letoGetCurrConn();
      hb_strncpy( szFile, szParam, HB_PATH_MAX - 1 );
   }
   leto_BeautifyPath( szFile );

   return pConnection;
}

HB_FUNC( LETO_FERROR )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
   {
      if( HB_ISLOG( 1 ) && hb_parl( 1 ) )
      {
         PHB_DYNS pDo = hb_dynsymFind( "LETO_UDF" );

         if( pDo )
         {
            hb_vmPushDynSym( pDo );
            hb_vmPushNil();
            hb_vmPushString( "FError", 6 );
            hb_vmDo( 1 );
         }
         else
            hb_retni( -1 );
      }
      else
         hb_retni( pCurrentConn->iError );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( LETO_FILE )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
      hb_retl( LetoFileExist( pConnection, szFile ) );
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( LETO_FERASE )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      if( LetoFileErase( pConnection, szFile ) )
         hb_retni( 0 );
      else
         hb_retni( -1 );
   }
   else
      hb_retni( -1 );
}

HB_FUNC( LETO_FRENAME )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && HB_ISCHAR( 2 ) && hb_parclen( 2 ) &&
       ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      if( LetoFileRename( pConnection, szFile, leto_RemoveIpFromPath( hb_parc( 2 ) ) ) )
         hb_retni( 0 );
      else
         hb_retni( -1 );
   }
   else
      hb_retni( -1 );
}

HB_FUNC( LETO_FCOPY )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && HB_ISCHAR( 2 ) && hb_parclen( 2 ) &&
       ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      if( LetoFileCopy( pConnection, szFile, leto_RemoveIpFromPath( hb_parc( 2 ) ) ) )
         hb_retni( 0 );
      else
         hb_retni( -1 );
   }
   else
      hb_retni( -1 );
}

HB_FUNC( LETO_FOPEN )
{
   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) )
   {
      PHB_DYNS pDo = hb_dynsymFind( "LETO_UDF" );

      if( pDo )
      {
         hb_vmPushDynSym( pDo );
         hb_vmPushNil();
         hb_vmPushString( "Leto_FOpen", 10 );
         hb_vmPushString( hb_parc( 1 ), hb_parclen( 1 ) );
         if( HB_ISNUM( 2 ) )
         {
            hb_vmPushInteger( hb_parni( 2 ) );
            hb_vmDo( 3 );
         }
         else
            hb_vmDo( 2 );
         return;
      }
   }

   hb_retni( -1 );
}

HB_FUNC( LETO_FCREATE )
{
   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) )
   {
      PHB_DYNS pDo = hb_dynsymFind( "LETO_UDF" );

      if( pDo )
      {
         hb_vmPushDynSym( pDo );
         hb_vmPushNil();
         hb_vmPushString( "Leto_FCreate", 12 );
         hb_vmPushString( hb_parc( 1 ), hb_parclen( 1 ) );
         if( HB_ISNUM( 2 ) )
         {
            hb_vmPushInteger( hb_parni( 2 ) );
            hb_vmDo( 3 );
         }
         else
            hb_vmDo( 2 );
         return;
      }
   }

   hb_retni( -1 );
}

HB_FUNC( LETO_FCLOSE )
{
   if( HB_ISNUM( 1 ) )
   {
      PHB_DYNS pDo = hb_dynsymFind( "LETO_UDF" );

      if( pDo )
      {
         hb_vmPushDynSym( pDo );
         hb_vmPushNil();
         hb_vmPushString( "Leto_FClose", 11 );
         hb_vmPushNumInt( hb_parnint( 1 ) );
         hb_vmDo( 2 );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

HB_FUNC( LETO_FSEEK )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      PHB_DYNS pDo = hb_dynsymFind( "LETO_UDF" );

      if( pDo )
      {
         hb_vmPushDynSym( pDo );
         hb_vmPushNil();
         hb_vmPushString( "FSeek", 5 );
         hb_vmPushNumInt( hb_parnint( 1 ) );
         hb_vmPushNumInt( hb_parnint( 2 ) );
         if( HB_ISNUM( 3 ) )
         {
            hb_vmPushInteger( hb_parni( 3 ) );
            hb_vmDo( 4 );
         }
         else
            hb_vmDo( 3 );
         return;
      }
   }

   hb_retni( -1 );
}

HB_FUNC( LETO_FEOF )
{
   if( HB_ISNUM( 1 ) )
   {
      PHB_DYNS pDo = hb_dynsymFind( "LETO_UDF" );

      if( pDo )
      {
         hb_vmPushDynSym( pDo );
         hb_vmPushNil();
         hb_vmPushString( "HB_FEOF", 7 );
         hb_vmPushNumInt( hb_parnint( 1 ) );
         hb_vmDo( 2 );
         return;
      }
   }

   hb_retl( HB_TRUE );
}

HB_FUNC( LETO_FREAD )
{
   HB_MAXINT nHandle = hb_parnint( 1 );
   HB_SIZE   nLen = hb_parns( 3 ), nRead = 0;

   if( HB_ISNUM( 1 ) && HB_ISCHAR( 2 ) && HB_ISBYREF( 2 ) && HB_ISNUM( 3 ) )
   {
      PHB_DYNS pDo = hb_dynsymFind( "LETO_UDF" );

      if( pDo && nLen <= hb_parcsiz( 2 ) )
      {
         PHB_ITEM pRefVal = hb_param( 2, HB_IT_STRING );
         PHB_ITEM pResult;
         HB_SIZE  nSize;
         char *   pBuffer;

         hb_vmPushDynSym( pDo );
         hb_vmPushNil();
         hb_vmPushString( "HB_FReadLen", 11 );
         hb_vmPushNumInt( nHandle );
         hb_vmPushNumInt( nLen );
         hb_vmDo( 3 );
         pResult = hb_stackReturnItem();
         if( ( hb_itemType( pResult ) & HB_IT_STRING ) &&
             hb_itemGetWriteCL( pRefVal, &pBuffer, &nSize ) )
         {
            nRead = hb_itemGetCLen( pResult );
            memcpy( pBuffer, hb_itemGetCPtr( pResult ), nRead );
         }
      }
   }

   hb_retns( nRead );
}

HB_FUNC( LETO_FREADSTR )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      PHB_DYNS  pDo = hb_dynsymFind( "LETO_UDF" );
      HB_MAXINT nHandle = hb_parnint( 1 );
      HB_SIZE   nLen = hb_parns( 2 );

      if( pDo )
      {
         hb_vmPushDynSym( pDo );
         hb_vmPushNil();
         hb_vmPushString( "FReadStr", 8 );
         hb_vmPushNumInt( nHandle );
         hb_vmPushNumInt( nLen );
         hb_vmDo( 3 );
         return;
      }
   }

   hb_retc_null();
}

HB_FUNC( LETO_FREADLEN )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      PHB_DYNS  pDo = hb_dynsymFind( "LETO_UDF" );
      HB_MAXINT nHandle = hb_parnint( 1 );
      HB_SIZE   nLen = hb_parns( 2 );

      if( pDo )
      {
         hb_vmPushDynSym( pDo );
         hb_vmPushNil();
         hb_vmPushString( "HB_FReadLen", 11 );
         hb_vmPushNumInt( nHandle );
         hb_vmPushNumInt( nLen );
         hb_vmDo( 3 );
         return;
      }
   }

   hb_retc_null();
}

HB_FUNC( LETO_FWRITE )
{
   if( HB_ISNUM( 1 ) && HB_ISCHAR( 2 ) && hb_parclen( 2 ) )
   {
      PHB_DYNS  pDo = hb_dynsymFind( "LETO_UDF" );
      HB_MAXINT nHandle = hb_parnint( 1 );

      if( pDo )
      {
         hb_vmPushDynSym( pDo );
         hb_vmPushNil();
         hb_vmPushString( "FWrite", 6 );
         hb_vmPushNumInt( nHandle );
         hb_vmPushString( hb_parc( 2 ), hb_parclen( 2 ) );
         if( HB_ISNUM( 3 ) )
         {
            hb_vmPushNumInt( hb_parns( 3 ) );
            hb_vmDo( 4 );
         }
         else
            hb_vmDo( 3 );
         return;
      }
   }

   hb_retni( 0 );
}

static void leto_BufferResize( LETOCONNECTION * pConnection )
{
   if( pConnection->ulBufCryptLen > LETO_SENDRECV_BUFFSIZE )
   {
      hb_xfree( pConnection->pBufCrypt );
      pConnection->pBufCrypt = NULL;
      pConnection->ulBufCryptLen = 0;
   }

   if( pConnection->szBuffer && pConnection->ulBufferLen > LETO_SENDRECV_BUFFSIZE )
   {
      pConnection->ulBufferLen = LETO_SENDRECV_BUFFSIZE;
      pConnection->szBuffer = hb_xrealloc( pConnection->szBuffer, LETO_SENDRECV_BUFFSIZE + 1 );
   }
}

HB_FUNC( LETO_MEMOREAD )
{
   LETOCONNECTION * pConnection;
   char     szFile[ HB_PATH_MAX ];
   HB_ULONG ulMemoLen = 0;

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) )
   {
      pConnection = letoParseParam( hb_parc( 1 ), szFile );
      if( ! pConnection )
         pConnection = letoGetCurrConn();
      if( pConnection )
      {
         const char * ptr;

         if( ( ptr = LetoMemoRead( pConnection, szFile, ( unsigned long * ) &ulMemoLen ) ) != NULL && ulMemoLen )
            hb_retclen( ptr, ulMemoLen );
         else
            hb_retc( "" );
         leto_BufferResize( pConnection );
         return;
      }
   }

   hb_retc( "" );
}

HB_FUNC( LETO_FILEREAD )  /* ( cFile, 0 [ nStart ], 0 == all [ nLen ], @cBuf ) */
{
   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && HB_ISBYREF( 4 ) )
   {
      LETOCONNECTION * pConnection;
      char szFile[ HB_PATH_MAX ];

      pConnection = letoParseParam( hb_parc( 1 ), szFile );
      if( ! pConnection )
         pConnection = letoGetCurrConn();
      if( pConnection )
      {
         const char * ptr;
         HB_ULONG ulStart = HB_ISNUM( 2 ) && hb_parnl( 2 ) > 0 ? hb_parnl( 2 ) : 0;
         HB_ULONG ulLen = HB_ISNUM( 3 ) && hb_parnl( 3 ) > 0 ? hb_parnl( 3 ) : 0;

         if( ( ptr = LetoFileRead( pConnection, szFile, ulStart, ( unsigned long * ) &ulLen ) ) != NULL )
         {
            hb_storclen( ptr, ulLen, 4 );
            hb_retnl( ulLen );
            leto_BufferResize( pConnection );
            return;
         }
         else
         {
            hb_storclen( "", 0, 4 );
            hb_retnl( -1 );
            return;
         }
      }
   }

   hb_retnl( -1 );
}

HB_FUNC( LETO_MEMOWRITE )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && HB_ISCHAR( 2 ) &&
       ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      hb_retl( LetoMemoWrite( pConnection, szFile, hb_parc( 2 ), hb_parclen( 2 ) ) );
      leto_BufferResize( pConnection );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( LETO_FILEWRITE )  /* ( cFile, 0 [ nStart ], cBuf ) */
{
   LETOCONNECTION * pConnection;
   char     szFile[ HB_PATH_MAX ];
   HB_ULONG ulBufLen = hb_parclen( 3 );

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && HB_ISCHAR( 3 ) && ulBufLen &&
       ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      HB_ULONG ulStart = HB_ISNUM( 2 ) && hb_parnl( 2 ) > 0 ? hb_parnl( 2 ) : 0;

      hb_retl( LetoFileWrite( pConnection, szFile, hb_parc( 3 ), ulStart, ulBufLen ) );
      leto_BufferResize( pConnection );
   }
   else
      hb_retl( HB_FALSE );
}

#if ! defined( __HARBOUR30__ )

HB_FUNC( LETO_FCOPYTOSRV )  /* ( cFileLocal, cFileServer, nStepSize ) */
{
   HB_BOOL fSuccess = HB_FALSE;

   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) && hb_parclen( 1 ) && hb_parclen( 2 ) )
   {
      char szFile[ HB_PATH_MAX ];
      LETOCONNECTION * pConnection = letoParseParam( hb_parc( 2 ), szFile );

      if( pConnection )
      {
         HB_BOOL  fError = HB_FALSE;
         PHB_FILE pFile;
         HB_SIZE  nStepSize = HB_ISNUM( 3 ) && hb_parni( 3 ) > 0 ? hb_parni( 3 ) : 0x100000;

         if( ! hb_fileExists( hb_parc( 1 ), NULL ) )
            fError = HB_TRUE;
         if( ! fError && LetoFileExist( pConnection, szFile ) )
            fError = ! LetoFileErase( pConnection, szFile );

         if( ! fError && ( pFile = hb_fileExtOpen( hb_parc( 1 ), NULL, FO_READ | FO_SHARED | FO_PRIVATE, NULL, NULL ) ) != NULL )
         {
            HB_USHORT uStep = 0;
            HB_SIZE   nRead;
            char *    pBuffer = ( char * ) hb_xgrab( nStepSize );  /* 1MB */

            while( 1 )
            {
               nRead = hb_fileRead( pFile, ( void * ) pBuffer, nStepSize, -1 );
               if( nRead )
               {
                  fSuccess = HB_FALSE;
                  if( ! LetoFileWrite( pConnection, szFile, pBuffer, uStep * nStepSize, nRead ) )
                     break;
                  fSuccess = HB_TRUE;
                  uStep++;
               }
               if( nRead < nStepSize )
                  break;
            }
            hb_fileClose( pFile );

            hb_xfree( pBuffer );
            leto_BufferResize( pConnection );
         }
      }
   }

   hb_retl( fSuccess );
}

HB_FUNC( LETO_FCOPYFROMSRV )  /* ( cFileLocal, cFileServer, nStepSize ) */
{
   HB_BOOL fSuccess = HB_FALSE;

   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) && hb_parclen( 1 ) && hb_parclen( 2 ) )
   {
      char szFile[ HB_PATH_MAX ];
      LETOCONNECTION * pConnection = letoParseParam( hb_parc( 2 ), szFile );

      if( pConnection )
      {
         PHB_FILE pFile;
         HB_SIZE  nStepSize = HB_ISNUM( 3 ) && hb_parni( 3 ) > 0 ? hb_parni( 3 ) : 0x100000;
         HB_FATTR nMode = FO_READWRITE | FO_DENYNONE | FO_PRIVATE | FXO_TRUNCATE | FXO_SHARELOCK;

         if( ! LetoFileExist( pConnection, szFile ) )
            hb_fsSetFError( 2 );
         else
         {
            hb_fsSetFError( 0 );
            if( hb_fileExists( hb_parc( 1 ), NULL ) )
               hb_fileDelete( hb_parc( 1 ) );
         }
         if( ! hb_fsError() && ( pFile = hb_fileExtOpen( hb_parc( 1 ), NULL, nMode, NULL, NULL ) ) != NULL )
         {
            HB_USHORT    uStep = 0;
            HB_ULONG     ulLen;
            HB_SIZE      nWrite = 0;
            const char * ptr;

            fSuccess = HB_TRUE;
            while( fSuccess )
            {
               fSuccess = HB_FALSE;
               ulLen = nStepSize;
               if( ( ptr = LetoFileRead( pConnection, szFile, uStep * nStepSize, &ulLen ) ) != NULL )
               {
                  nWrite = hb_fileWrite( pFile, ptr, ulLen, -1 );
                  if( nWrite == ( HB_SIZE ) FS_ERROR || nWrite != ulLen )
                     break;
                  fSuccess = HB_TRUE;
               }
               else
                  hb_fsSetFError( 14 );
               if( nWrite < nStepSize )
                  break;
               uStep++;
            }
            hb_fileClose( pFile );
            leto_BufferResize( pConnection );
         }
      }
   }

   hb_retl( fSuccess );
}

#else  /* dummies for v3.0 */

HB_FUNC( LETO_FCOPYTOSRV )
{
   hb_retl( HB_FALSE );
}

HB_FUNC( LETO_FCOPYFROMSRV )
{
   hb_retl( HB_FALSE );
}

#endif  /* __HARBOUR30__ */


HB_FUNC( LETO_FILESIZE )  /* ( cFile ) */
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      hb_retnl( LetoFileSize( pConnection, szFile ) );
      return;
   }

   hb_retnl( -1 );
}


HB_FUNC( LETO_FILEATTR )  /* ( cFile [, cNewAttr ] ) */
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      hb_retc( LetoFileAttr( pConnection, szFile, HB_ISCHAR( 2 ) ? hb_parc( 2 ) : NULL ) );
      return;
   }

   hb_retc( "" );
}

HB_FUNC( LETO_DIRECTORY )  /* ( cPathSpec, cAttributes ) */
{
   LETOCONNECTION * pConnection;
   char             szFile[ HB_PATH_MAX ];
   const char *     ptr;
   unsigned long    ulLen;

   if( ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      if( ! *szFile )
         strcpy( szFile, "." );
      ptr = LetoDirectory( pConnection, szFile, HB_ISCHAR( 2 ) ? hb_parc( 2 ) : NULL );
      if( ptr != NULL )
      {
         ptr = leto_DecryptText( pConnection, &ulLen, pConnection->szBuffer + 3 );
         if( ulLen )
         {
            hb_itemReturnRelease( hb_itemDeserialize( &ptr, ( HB_SIZE * ) &ulLen ) );
            return;
         }
      }
   }

   hb_itemReturnRelease( hb_itemArrayNew( 0 ) );
}

HB_FUNC( LETO_DIRMAKE )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      if( LetoDirMake( pConnection, szFile ) )
         hb_retni( 0 );
      else
         hb_retni( -1 );
   }
   else
      hb_retni( -1 );
}

/* compatibility to former function name */
HB_FUNC_TRANSLATE( LETO_MAKEDIR, LETO_DIRMAKE )

HB_FUNC( LETO_DIREXIST )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
      hb_retl( LetoDirExist( pConnection, szFile ) );
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( LETO_DIRREMOVE )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      if( LetoDirRemove( pConnection, szFile ) )
         hb_retni( 0 );
      else
         hb_retni( -1 );
   }
   else
      hb_retni( -1 );
}

static int Leto_MgID( LETOCONNECTION * pConnection, HB_BOOL fRefresh )
{
   int iRet = pConnection->iConnectSrv;

   if( iRet <= 0 || fRefresh )
   {
      char szData[ 6 ];

      pConnection->iConnectSrv = iRet = -1;
      eprintf( szData, "%c;08;", LETOCMD_mgmt );
      if( leto_DataSendRecv( pConnection, szData, 5 ) )
      {
         const char * ptr = leto_firstchar( pConnection );

         if( *ptr == '+' && *( ptr + 3 ) == ';' )
         {
            iRet = atoi( ptr + 4 );
            pConnection->iConnectSrv = iRet;
         }
      }
   }

   return iRet;
}

HB_FUNC( LETO_CONNECT )
{
   LETOCONNECTION * pConnection;
   char szAddr[ 96 ];
   char szPath[ 96 ];
   int  iPort = 0;
   const char * szUser = ( HB_ISCHAR( 2 ) && hb_parclen( 2 ) ) ? hb_parc( 2 ) : NULL;
   const char * szPass = ( HB_ISCHAR( 3 ) && hb_parclen( 3 ) ) ? hb_parc( 3 ) : NULL;
   int          iTimeOut = ( HB_ISNUM( 4 ) ) ? hb_parni( 4 ) : -1;
   HB_BOOL      fZombieCheck = ( HB_ISLOG( 6 ) ) ? hb_parl( 6 ) : LETO_USE_THREAD;
   int          iRet = -1;

   HB_TRACE( HB_TR_DEBUG, ( "LETO_CONNECT(%s)", hb_parc( 1 ) ) );

   if( ! HB_ISCHAR( 1 ) || ! hb_parclen( 1 ) )
   {
      pConnection = letoGetCurrConn();
      if( pConnection )
         hb_retni( Leto_MgID( pConnection, HB_TRUE ) );
      else
         hb_retni( iRet );
      return;
   }
   memset( szPath, 0, 96 );
   memcpy( szPath, hb_parc( 1 ), HB_MIN( hb_parclen( 1 ), 95 ) );

   if( szPath[ 0 ] != '/' )  /* add "//.../" */
   {
      memmove( szPath + 2, szPath, 94 );
      szPath[ 0 ] = '/';
      szPath[ 1 ] = '/';
      szPath[ strlen( szPath ) ] = '/';
   }

   if( HB_ISCHAR( 1 ) && leto_getIpFromPath( szPath, szAddr, &iPort, NULL ) &&
       ( ( ( pConnection = leto_ConnectionFind( szAddr, iPort ) ) != NULL ) ||
         ( ( pConnection = LetoConnectionNew( szAddr, iPort, szUser, szPass, iTimeOut, fZombieCheck ) ) != NULL ) ) )
   {
      if( HB_ISNUM( 5 ) )
         pConnection->iBufRefreshTime = HB_MAX( hb_parni( 5 ), -1 );
      hb_rddDefaultDrv( "LETO" );
      iRet = pConnection->iConnection;
   }

   hb_retni( iRet );
}

HB_FUNC( LETO_DISCONNECT )
{
   LETOCONNECTION * pConnection = leto_getConnection( 1 );

   if( pConnection )
   {
      if( ! HB_ISLOG( 2 ) || ! hb_parl( 2 ) )
      {
         leto_ConnectionClose( pConnection );
         letoClearCurrConn();
         hb_idleSleep( 0.005 );
      }
      else
         LetoConnectionClose( pConnection );  /* only socket shutdown */
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* valid "//IP:port/" is needed, else no connection changed */
HB_FUNC( LETO_SETCURRENTCONNECTION )
{
   LETOCONNECTION * pConnection = HB_ISCHAR( 1 ) ? leto_getConnection( 1 ) : NULL;

   if( pConnection )
   {
      char szAddr[ 96 ];
      int  iLen;

      szAddr[ 0 ] = '/';
      szAddr[ 1 ] = '/';
      strcpy( szAddr + 2, pConnection->pAddr );
      iLen = strlen( szAddr );
      szAddr[ iLen++ ] = ':';
      iLen += ultostr( pConnection->iPort, szAddr + iLen );
      szAddr[ iLen++ ] = '/';
      szAddr[ iLen ] = '\0';

      hb_retc( szAddr );
   }
   else
      hb_retc( "" );
}

HB_FUNC( LETO_GETCURRENTCONNECTION )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();

   if( pConnection )
   {
      char szAddr[ 96 ];
      int  iLen;

      szAddr[ 0 ] = '/';
      szAddr[ 1 ] = '/';
      strcpy( szAddr + 2, pConnection->pAddr );
      iLen = strlen( szAddr );
      szAddr[ iLen++ ] = ':';
      iLen += ultostr( pConnection->iPort, szAddr + iLen );
      szAddr[ iLen++ ] = '/';
      szAddr[ iLen ] = '\0';

      hb_retc( szAddr );
   }
   else
      hb_retc( "" );
}

HB_FUNC( LETO_SETLOCKTIMEOUT )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();

   if( pConnection )
   {
      hb_retni( pConnection->iLockTimeOut );
      if( HB_ISNUM( 1 ) && hb_parni( 1 ) >= 0 )
         pConnection->iLockTimeOut = hb_parni( 1 );
   }
   else
      hb_retni( -1 );
}

HB_FUNC( LETO_GETSERVERMODE )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
      hb_retni( pCurrentConn->uiServerMode );
   else
      hb_retni( 0 );
}

HB_FUNC( LETO_GETSERVERVERSION )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
      hb_retc( ! ( HB_ISLOG( 1 ) && hb_parl( 1 ) ) ? LetoGetServerVer( pCurrentConn ) : pCurrentConn->szVerHarbour );
   else
      hb_retc( "" );
}

HB_FUNC( LETO_CPUCORES )
{
   hb_retni( leto_CPUCores() );
}

HB_FUNC( LETO_CPULOAD )
{
   hb_retni( leto_CPULoad() );
}

HB_FUNC( LETO_MGGETINFO )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
   {
      const char * ptr = LetoMgGetInfo( pCurrentConn );

      if( ptr != NULL )
      {
         PHB_ITEM pTmp;
         PHB_ITEM aInfo;

         if( *( ptr - 1 ) == '+' )
         {
            const char * ptr2;
            int          i;

            aInfo = hb_itemArrayNew( 18 );
            for( i = 1; i <= 18; i++ )
            {
               if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )
                  break;

               pTmp = hb_itemPutCL( NULL, ptr, ptr2 - ptr );
               hb_itemArrayPut( aInfo, i, pTmp );
               hb_itemRelease( pTmp );
               ptr = ++ptr2;
            }

            hb_itemReturnRelease( aInfo );
         }
      }
   }
}

HB_FUNC( LETO_MGSYSINFO )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
   {
      const char * ptr = LetoMgSysInfo( pCurrentConn );

      if( ptr != NULL )
      {
         PHB_ITEM pTmp;
         PHB_ITEM aInfo;

         if( *( ptr - 1 ) == '+' )
         {
            const char * ptr2;
            int          i;

            aInfo = hb_itemArrayNew( 9 );
            for( i = 1; i <= 9; i++ )
            {
               if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )
                  break;

               pTmp = hb_itemPutCL( NULL, ptr, ptr2 - ptr );
               hb_itemArrayPut( aInfo, i, pTmp );
               hb_itemRelease( pTmp );
               ptr = ++ptr2;
            }

            hb_itemReturnRelease( aInfo );
         }
      }
   }
}

HB_FUNC( LETO_MGGETUSERS )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
   {
      const char * szTable;
      char         sBuffer[ HB_PATH_MAX ];
      const char * pcptr;
      const char * szList;

      if( HB_ISCHAR( 1 ) )
         szTable = hb_parc( 1 );
      else if( HB_ISNUM( 1 ) )
      {
         eprintf( sBuffer, "%d", hb_parni( 1 ) );
         szTable = sBuffer;
      }
      else
         szTable = NULL;
      if( HB_ISCHAR( 2 ) )
         szList = hb_parc( 2 );
      else
         szList = NULL;

      if( ( pcptr = LetoMgGetUsers( pCurrentConn, szTable, szList ) ) != NULL )
      {
         PHB_ITEM pArray, pArrayItm;

         if( *( pcptr - 1 ) == '+' )
         {
            const char * ptr = pcptr;
            const char * ptr2;
            int          iUsers, i, j;

            if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )
               return;
            iUsers = atoi( ptr );
            ptr = ++ptr2;
            pArray = hb_itemArrayNew( iUsers );
            for( i = 1; i <= iUsers; i++ )
            {
               pArrayItm = hb_arrayGetItemPtr( pArray, i );
               hb_arrayNew( pArrayItm, 9 );
               for( j = 1; j <= 9; j++ )
               {
                  if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )
                     return;
                  hb_itemPutCL( hb_arrayGetItemPtr( pArrayItm, j ), ptr, ptr2 - ptr );
                  ptr = ++ptr2;
               }
            }
            hb_itemReturnRelease( pArray );
         }
      }
   }
}

HB_FUNC( LETO_MGGETTABLES )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
   {
      const char * szUser;
      const char * szList;
      char         sBuffer[ HB_PATH_MAX ];
      const char * pcptr;

      if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) )
         szUser = hb_parc( 1 );
      else if( HB_ISNUM( 1 ) )
      {
         eprintf( sBuffer, "%d", hb_parni( 1 ) );
         szUser = sBuffer;
      }
      else
         szUser = NULL;
      if( HB_ISCHAR( 2 ) && hb_parclen( 2 ) )
         szList = hb_parc( 2 );
      else
         szList = NULL;

      if( ( pcptr = LetoMgGetTables( pCurrentConn, szUser, szList ) ) != NULL )
      {
         PHB_ITEM pArray, pArrayItm;

         if( *( pcptr - 1 ) == '+' )
         {
            const char * ptr = pcptr;
            const char * ptr2;
            int          iTables, i, j;

            if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )
               return;
            iTables = atoi( ptr );
            ptr = ++ptr2;
            pArray = hb_itemArrayNew( iTables );
            for( i = 1; i <= iTables; i++ )
            {
               pArrayItm = hb_arrayGetItemPtr( pArray, i );
               hb_arrayNew( pArrayItm, 7 );
               for( j = 1; j <= 7; j++ )
               {
                  if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )
                     return;
                  if( j != 5 )
                     hb_itemPutCL( hb_arrayGetItemPtr( pArrayItm, j ), ptr, ptr2 - ptr );
                  else
                     hb_itemPutL( hb_arrayGetItemPtr( pArrayItm, j ), ( *ptr == 'T' ) ? HB_TRUE : HB_FALSE );
                  ptr = ++ptr2;
               }
            }
            hb_itemReturnRelease( pArray );
         }
      }
   }
}

HB_FUNC( LETO_MGGETINDEX )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
   {
      const char * szUser;
      const char * szTable;
      const char * szList;
      char         sBuffer[ HB_PATH_MAX ];
      const char * pcptr;

      if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) )
         szUser = hb_parc( 1 );
      else if( HB_ISNUM( 1 ) )
      {
         eprintf( sBuffer, "%d", hb_parni( 1 ) );
         szUser = sBuffer;
      }
      else
         szUser = NULL;

      if( HB_ISCHAR( 2 ) && hb_parclen( 2 ) )
         szTable = hb_parc( 2 );
      else
         szTable = NULL;

      if( HB_ISCHAR( 3 ) && hb_parclen( 3 ) )
         szList = hb_parc( 3 );
      else
         szList = NULL;

      if( ( pcptr = LetoMgGetIndex( pCurrentConn, szUser, szTable, szList ) ) != NULL )
      {
         PHB_ITEM pArray, pArrayItm;

         if( *( pcptr - 1 ) == '+' )
         {
            const char * ptr = pcptr;
            const char * ptr2;
            int          iIndex, i, j;

            if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )
               return;
            iIndex = atoi( ptr );
            ptr = ++ptr2;
            pArray = hb_itemArrayNew( iIndex );
            for( i = 1; i <= iIndex; i++ )
            {
               pArrayItm = hb_arrayGetItemPtr( pArray, i );
               hb_arrayNew( pArrayItm, 4 );
               for( j = 1; j <= 4; j++ )
               {
                  if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )
                     return;
                  hb_itemPutCL( hb_arrayGetItemPtr( pArrayItm, j ), ptr, ptr2 - ptr );
                  ptr = ++ptr2;
               }
            }
            hb_itemReturnRelease( pArray );
         }
      }
   }
}

HB_FUNC( LETO_MGGETLOCKS )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
   {
      const char * szUser;
      const char * szTable;
      const char * szList;
      char         sBuffer[ HB_PATH_MAX ];
      const char * pcptr;

      if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) )
         szUser = hb_parc( 1 );
      else if( HB_ISNUM( 1 ) )
      {
         eprintf( sBuffer, "%d", hb_parni( 1 ) );
         szUser = sBuffer;
      }
      else
         szUser = NULL;
      if( HB_ISCHAR( 2 ) && hb_parclen( 2 ) )
         szTable = hb_parc( 2 );
      else
         szTable = NULL;
      if( HB_ISCHAR( 3 ) && hb_parclen( 3 ) )
         szList = hb_parc( 3 );
      else
         szList = NULL;

      if( ( pcptr = LetoMgGetLocks( pCurrentConn, szUser, szTable, szList ) ) != NULL )
      {
         PHB_ITEM pArray, pArrayItm;

         if( *( pcptr - 1 ) == '+' )
         {
            const char * ptr = pcptr;
            const char * ptr2;
            int          iCount, i, j;

            if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )
               return;
            iCount = atoi( ptr );
            ptr = ++ptr2;
            pArray = hb_itemArrayNew( iCount );
            for( i = 1; i <= iCount; i++ )
            {
               pArrayItm = hb_arrayGetItemPtr( pArray, i );
               hb_arrayNew( pArrayItm, 2 );
               for( j = 1; j <= 2; j++ )
               {
                  if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )
                     return;
                  hb_itemPutCL( hb_arrayGetItemPtr( pArrayItm, j ), ptr, ptr2 - ptr );
                  ptr = ++ptr2;
               }
            }
            hb_itemReturnRelease( pArray );
         }
      }
   }
}

HB_FUNC( LETO_MGLOG )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn && HB_ISNUM( 1 ) )
   {
      char     szData[ 32 ];
      HB_ULONG ulLen;

      ulLen = eprintf( szData, "%c;07;%d;%d;", LETOCMD_mgmt, hb_parni( 1 ), HB_ISNUM( 2 ) ? hb_parni( 2 ) : 0 );
      ulLen = ( HB_ULONG ) leto_DataSendRecv( pCurrentConn, szData, ulLen );
      if( ulLen )
      {
         char * ptr = pCurrentConn->szBuffer;

         if( *ptr == '+' )
         {
            hb_retclen( ++ptr, ulLen - 1 );
            return;
         }
      }
   }

   hb_retc_null();
}

HB_FUNC( LETO_MGID )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();
   int iRet = -1;

   if( pConnection )
      iRet = Leto_MgID( pConnection, HB_ISLOG( 1 ) && ! hb_parl( 1 ) ? HB_FALSE : HB_TRUE );

   hb_retni( iRet );
}

HB_FUNC( LETO_MGKILL )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   int iRet = -2;

   if( pCurrentConn && ( HB_ISCHAR( 1 ) || HB_ISNUM( 1 ) ) )
   {
      const char * szUserId = NULL;
      char         sBuffer[ 21 ];

      if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) > 0 )
         szUserId = hb_parc( 1 );
      else if( HB_ISNUM( 1 ) )
      {
         eprintf( sBuffer, "%d", hb_parni( 1 ) );
         szUserId = sBuffer;
      }
      if( szUserId )
         iRet = LetoMgKillUser( pCurrentConn, szUserId );
   }

   hb_retni( iRet );
}

HB_FUNC( LETO_MGGETTIME )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
   {
      char * ptr = LetoMgGetTime( pCurrentConn );

      if( ptr && *( ptr - 1 ) == '+' )
      {
         PHB_ITEM pTmp;
         PHB_ITEM aInfo;
         char     szData[ 32 ];
         int      i;

         aInfo = hb_itemArrayNew( 3 );
         for( i = 1; i <= 3; i++ )
         {
            if( ! LetoGetCmdItem( &ptr, szData ) )
            {
               hb_itemReturnRelease( aInfo );
               return;
            }
            ptr++;
            if( i == 1 )
            {
               int      iOvf;
               HB_ULONG ulDate = ( HB_ULONG ) hb_strValInt( szData, &iOvf );

               pTmp = hb_itemPutDL( NULL, ulDate );
            }
            else
            {
               pTmp = hb_itemPutND( NULL, hb_strVal( szData, 10 ) );
            }
            hb_itemArrayPut( aInfo, i, pTmp );
            hb_itemRelease( pTmp );
         }

         hb_itemReturnRelease( aInfo );
      }
   }
}

HB_FUNC( LETO_PING )
{
   LETOCONNECTION * pConnection = leto_getConnection( 1 );

   if( ! pConnection )
      pConnection = letoGetCurrConn();
   if( pConnection )
      hb_retl( leto_Ping( pConnection ) );
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( LETO_USERADD )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();
   char             szData[ 96 ];
   char             szPass[ 54 ];
   const char *     szAccess = ( HB_ISCHAR( 3 ) ) ? hb_parc( 3 ) : "";
   HB_ULONG         ulLen;

   if( pConnection )
   {
      if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
      {
         if( ( ulLen = hb_parclen( 2 ) ) > 0 )
         {
            char * szKey = leto_localKey( pConnection->cDopcode, LETO_DOPCODE_LEN );

            if( ulLen > LETO_MAX_KEYLENGTH )
               ulLen = LETO_MAX_KEYLENGTH;

            leto_encrypt( hb_parc( 2 ), ulLen, szData, &ulLen, szKey, HB_FALSE );
            leto_byte2hexchar( szData, ( int ) ulLen, szPass );
            szPass[ ulLen * 2 ] = '\0';
            if( szKey )
               hb_xfree( szKey );
         }
         else
            szPass[ 0 ] = '\0';
         /* removed upper conversion -- as only for adding ? */
         /* hb_strUpper( ( char * ) szAccess, strlen( szAccess ) ); */

         hb_snprintf( szData, 96, "%c;uadd;%s;%s;%s;", LETOCMD_admin, hb_parc( 1 ), szPass, szAccess );
         if( leto_DataSendRecv( pConnection, szData, 0 ) )
         {
            hb_retl( *( leto_firstchar( pConnection ) ) == '+' );
            return;
         }
      }
   }

   hb_retl( HB_FALSE );
}

HB_FUNC( LETO_USERDELETE )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();
   char szData[ 96 ];

   if( pConnection )
   {
      if( HB_ISCHAR( 1 ) )
      {
         hb_snprintf( szData, 96, "%c;udel;%s;", LETOCMD_admin, hb_parc( 1 ) );
         if( leto_DataSendRecv( pConnection, szData, 0 ) )
         {
            hb_retl( *( leto_firstchar( pConnection ) ) == '+' );
            return;
         }
      }
   }

   hb_retl( HB_FALSE );
}

HB_FUNC( LETO_USERPASSWD )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();
   char     szData[ 96 ];
   char     szPass[ 54 ];
   HB_ULONG ulLen;

   if( pConnection )
   {
      if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
      {
         if( ( ulLen = hb_parclen( 2 ) ) > 0 )
         {
            char * szKey = leto_localKey( pConnection->cDopcode, LETO_DOPCODE_LEN );

            if( ulLen > LETO_MAX_KEYLENGTH )
               ulLen = LETO_MAX_KEYLENGTH;
            leto_encrypt( hb_parc( 2 ), ulLen, szData, &ulLen, szKey, HB_FALSE );
            leto_byte2hexchar( szData, ( int ) ulLen, szPass );
            szPass[ ulLen * 2 ] = '\0';
            if( szKey )
               hb_xfree( szKey );
         }
         else
            szPass[ 0 ] = '\0';

         hb_snprintf( szData, 96, "%c;upsw;%s;%s;", LETOCMD_admin, hb_parc( 1 ), szPass );
         if( leto_DataSendRecv( pConnection, szData, 0 ) )
         {
            hb_retl( *( leto_firstchar( pConnection ) ) == '+' );
            return;
         }
      }
   }

   hb_retl( HB_FALSE );
}

HB_FUNC( LETO_USERRIGHTS )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();
   char szData[ 96 ];

   if( pConnection )
   {
      if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
      {
         hb_snprintf( szData, 96, "%c;uacc;%s;%s;", LETOCMD_admin, hb_parc( 1 ), hb_parc( 2 ) );
         if( leto_DataSendRecv( pConnection, szData, 0 ) )
         {
            hb_retl( *( leto_firstchar( pConnection ) ) == '+' );
            return;
         }
      }
   }

   hb_retl( HB_FALSE );
}

HB_FUNC( LETO_USERFLUSH )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();
   char szData[ 24 ];

   if( pConnection )
   {
      hb_snprintf( szData, 24, "%c;flush;", LETOCMD_admin );
      if( leto_DataSendRecv( pConnection, szData, 0 ) )
      {
         hb_retl( *( leto_firstchar( pConnection ) ) == '+' );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

HB_FUNC( LETO_USERGETRIGHTS )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
      hb_retclen( pCurrentConn->szAccess, 3 );
   else
      hb_retc_null();
}

HB_FUNC( LETO_LOCKCONN )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();

   if( pConnection )
   {
      char szData[ 24 ];

      hb_snprintf( szData, 24, "%c;lockc;%c;", LETOCMD_admin,
                   HB_ISLOG( 1 ) ? ( hb_parl( 1 ) ? 'T' : 'F' ) : '?' );
      if( leto_DataSendRecv( pConnection, szData, 0 ) )
      {
         hb_retl( *( leto_firstchar( pConnection ) ) == '+' );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

HB_FUNC( LETO_LOCKLOCK )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();

   if( pConnection )
   {
      HB_USHORT uiSecs = HB_ISNUM( 2 ) ? ( HB_USHORT ) hb_parni( 2 ) : 30;
      char      szData[ 24 ];

      hb_snprintf( szData, 24, "%c;lockl;%c;%d;", LETOCMD_admin,
                   HB_ISLOG( 1 ) ? ( hb_parl( 1 ) ? 'T' : 'F' ) : '?', uiSecs );
      if( leto_DataSendRecv( pConnection, szData, 0 ) )
      {
         hb_retl( *( leto_firstchar( pConnection ) ) == '+' );
         return;
      }
   }

   hb_retl( HB_FALSE );
}


HB_FUNC( LETO_STRTOHEX )
{
   const char * ptri = hb_parc( 1 );
   int          iLen = hb_parclen( 1 );

   if( iLen > 0 )
   {
      char * ptro = ( char * ) hb_xgrabz( iLen * 2 + 1 );

      leto_byte2hexchar( ptri, iLen, ptro );
      hb_retc_buffer( ptro );
   }
   else
      hb_retc_null();
}

HB_FUNC( LETO_HEXTOSTR )
{
   const char * ptri = hb_parc( 1 );
   int          iLen = hb_parclen( 1 );

   if( iLen > 1 && iLen % 2 == 0 )
   {
      char * ptro = ( char * ) hb_xgrabz( iLen / 2 + 1 );

      leto_hexchar2byte( ptri, iLen, ptro );
      hb_retc_buffer( ptro );
   }
   else
      hb_retc_null();
}

HB_FUNC( LETO_ENCRYPT )
{
   const char * psSrc = hb_parc( 1 );
   HB_ULONG     ulLen = hb_parclen( 1 );
   char *       psRet = ( char * ) hb_xgrabz( ulLen + 9 );
   char *       szKey = NULL;
   HB_BOOL      fGlobal = ( HB_ISCHAR( 2 ) && hb_parclen( 2 ) ) ? HB_TRUE : HB_FALSE;

   if( ! fGlobal )
   {
      LETOCONNECTION * pCurrentConn = letoGetCurrConn();

      if( pCurrentConn )
         szKey = leto_localKey( pCurrentConn->cDopcode, LETO_DOPCODE_LEN );
   }
   else
      szKey = hb_strdup(  hb_parc( 2 ) );

   if( fGlobal )
      leto_cryptReset( HB_TRUE );
   if( szKey )
   {
      leto_encrypt( psSrc, ulLen, psRet, &ulLen, szKey, fGlobal );
      hb_retclen_buffer( psRet, ( HB_SIZE ) ulLen );
   }
   else
      hb_retc( "" );

   if( fGlobal )
      leto_cryptReset( HB_TRUE );
   else
      hb_xfree( szKey );
}

HB_FUNC( LETO_DECRYPT )
{
   const char * psSrc = hb_parc( 1 );
   HB_ULONG     ulLen = hb_parclen( 1 );
   char *       psRet = ( char * ) hb_xgrabz( ulLen + 1 );
   char *       szKey = NULL;
   HB_BOOL      fGlobal = ( HB_ISCHAR( 2 ) && hb_parclen( 2 ) ) ? HB_TRUE : HB_FALSE;

   if( ! fGlobal )
   {
      LETOCONNECTION * pCurrentConn = letoGetCurrConn();

      if( pCurrentConn )
         szKey = leto_localKey( pCurrentConn->cDopcode, LETO_DOPCODE_LEN );
   }
   else
      szKey = hb_strdup( hb_parc( 2 ) );

   if( fGlobal )
      leto_cryptReset( HB_TRUE );
   if( szKey )
   {
      leto_decrypt( psSrc, ulLen, psRet, &ulLen, szKey, fGlobal );
      hb_retclen_buffer( psRet, ( HB_SIZE ) ulLen );
   }
   else
      hb_retc( "null" );

   if( fGlobal )
      leto_cryptReset( HB_TRUE );
   else
      hb_xfree( szKey );
}

HB_FUNC( LETO_CRYPTRESET )
{
   leto_cryptReset( HB_TRUE );
}

HB_FUNC( LETO_TOGGLEZIP )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();
   char szData[ 96 ];
   int  iZipRecord;
   int  iKeyLen = HB_ISCHAR( 2 ) ? ( int ) hb_parclen( 2 ) : 0;

   if( pConnection && HB_ISNUM( 1 ) )
   {
      iZipRecord = hb_parni( 1 );
#ifdef USE_LZ4
      if( iZipRecord >= -1 && iZipRecord <= 15 )
#else
      if( iZipRecord >= -1 && iZipRecord <= 9 )
#endif
      {
         char * szPass = ( char * ) hb_xgrab( ( iKeyLen + 9 ) * 2 );

         if( iKeyLen > 0 && iZipRecord >= 0 )
         {
            char *    szKey = leto_localKey( pConnection->cDopcode, LETO_DOPCODE_LEN );
            HB_ULONG  ulLen;

            if( iKeyLen > LETO_MAX_KEYLENGTH )  /* 96 == max pass 37 */
               iKeyLen = LETO_MAX_KEYLENGTH;

            leto_encrypt( hb_parc( 2 ), ( HB_ULONG ) iKeyLen, szData, &ulLen, szKey, HB_FALSE );
            leto_byte2hexchar( szData, ( int ) ulLen, szPass );
            szPass[ ulLen * 2 ] = '\0';
            if( szKey )
               hb_xfree( szKey );
         }
         else
            szPass[ 0 ] = '\0';

         hb_snprintf( szData, 96, "%c;%d;%s;", LETOCMD_zip, iZipRecord, szPass );
         hb_xfree( szPass );
         if( leto_DataSendRecv( pConnection, szData, 0 ) )
         {
            const char * ptr = leto_firstchar( pConnection );

            if( *ptr == '+' )
            {
               if( pConnection->zstream )
               {
#ifdef USE_LZ4
                  hb_lz4netClose( pConnection->zstream );
#else
                  hb_znetClose( pConnection->zstream );
#endif
                  pConnection->zstream = NULL;
                  pConnection->fZipCrypt = HB_FALSE;
               }

               pConnection->iZipRecord = iZipRecord;
#ifdef USE_LZ4
               if( pConnection->iZipRecord >= 0 && ! pConnection->zstream )
                  pConnection->zstream = hb_lz4netOpen( pConnection->iZipRecord, HB_ZLIB_STRATEGY_DEFAULT );
#else
               if( pConnection->iZipRecord > 0 && ! pConnection->zstream )
                  pConnection->zstream = hb_znetOpen( pConnection->iZipRecord, HB_ZLIB_STRATEGY_DEFAULT );
#endif
               if( pConnection->zstream && iKeyLen > 0 )
               {
                  int    i;
                  char * szPass = ( char * ) hb_xgrabz( iKeyLen + 1 );
#ifndef __XHARBOUR__
                  char *   pDst;
                  HB_SIZE nDstLen;
#endif

                  memcpy( szPass, hb_parc( 2 ), iKeyLen );
                  for( i = 0; i < HB_MIN( LETO_DOPCODE_LEN, iKeyLen ); i++ )
                  {
                     szPass[ i ] ^= pConnection->cDopcode[ i ];
                  }
#ifdef USE_LZ4
                  hb_lz4netEncryptKey( pConnection->zstream, szPass, iKeyLen );
#else
                  hb_znetEncryptKey( pConnection->zstream, szPass, iKeyLen );
#endif
#ifndef __XHARBOUR__
                  hb_itemGetWriteCL( hb_param( 2, HB_IT_STRING ), &pDst, &nDstLen );
                  memset(pDst, 0, nDstLen );
#endif
                  pConnection->fZipCrypt = HB_TRUE;
                  memset( szPass, 0, iKeyLen );
                  hb_xfree( szPass );
               }

               if( pConnection->iZipRecord > 0 && ! pConnection->zstream )
                  hb_retni( -5 );
               else
                  hb_retni( pConnection->iZipRecord );
            }
            else
               hb_retni( -4 );

            return;
         }
         hb_retni( -3 );
      }

#if 0  /* test purpose only */
      else if( iZipRecord == 42 )
      {
         char  szPass[ 5 ];

         strcpy( szPass, "elch" );
         hb_snprintf( szData, 24, "%c;%d;%s;", LETOCMD_stop, iZipRecord, szPass );
         leto_DataSendRecv( pConnection, szData, 0 );
      }
#endif

      else
         hb_retni( -2 );
   }
   else if( pConnection )
      hb_retni( pConnection->iZipRecord );
   else
      hb_retni( -2 );
}

static char Leto_VarSet( LETOCONNECTION * pCurrentConn, const char * szGroup, const char * szVar, PHB_ITEM pValue, HB_USHORT uiFlags, char ** pRetValue, HB_UINT * uiRes )
{
   char     cType;
   HB_ULONG ulLen;
   PHB_ITEM pVarItem;
   char     szValue[ HB_PATH_MAX ];

   *szValue = '\0';
   if( HB_IS_LOGICAL( pValue ) )
   {
      cType = LETOVAR_LOG;
      if( hb_itemGetL( pValue ) )
      {
         pVarItem = hb_itemPutL( NULL, HB_TRUE );
         *szValue = '1';
      }
      else
      {
         pVarItem = hb_itemPutL( NULL, HB_FALSE );
         *szValue = '0';
      }
      *( szValue + 1 ) = '\0';
      ulLen = 1;
   }
   else if( HB_IS_NUMERIC( pValue ) )
   {
      cType = LETOVAR_NUM;
      if( HB_IS_INTEGER( pValue ) || HB_IS_LONG( pValue ) )
      {
         pVarItem = hb_itemPutNL( NULL, hb_itemGetNL( pValue ) );
         sprintf( szValue, "%ld", hb_itemGetNL( pValue ) );
      }
      else
      {
         int iDec;

         pVarItem = hb_itemNew( NULL );
         hb_itemCopy( pVarItem, pValue );
         hb_itemGetNDDec( pVarItem, &iDec );
         sprintf( szValue, "%.*f", iDec, hb_itemGetND( pValue ) );
      }
      ulLen = strlen( szValue );
   }
   else if( HB_IS_STRING( pValue ) )
   {
      cType = LETOVAR_STR;
      ulLen = hb_itemGetCLen( pValue );
      pVarItem = hb_itemPutCL( NULL, hb_itemGetCPtr( pValue), ulLen );
   }
   else if( HB_IS_ARRAY( pValue ) )
   {
      HB_SIZE nSize = 0;
      char *  pArr = hb_itemSerialize( pValue, HB_SERIALIZE_NUMSIZE, &nSize );  //  | HB_SERIALIZE_COMPRESS

      cType = LETOVAR_ARR;
      ulLen = ( HB_ULONG ) nSize;
      pVarItem = hb_itemClone( pValue );
      *uiRes = LetoVarSet( pCurrentConn, szGroup, szVar, cType, pArr, ( HB_ULONG ) nSize, uiFlags, NULL );
      if( pArr )
         hb_xfree( pArr );
   }
   else if( HB_IS_DATE( pValue ) )
   {
      cType = LETOVAR_DAT;
      hb_itemGetDS( pValue, szValue );
      ulLen = strlen( szValue );  /* 8 */
      pVarItem = hb_itemPutDS( NULL, hb_itemGetCPtr( pValue) );
   }
   else
   {
      cType = '\0';
      ulLen = 0;
      pVarItem = NULL;
      *uiRes = 0;
   }

   if( cType && cType != LETOVAR_ARR )
      *uiRes = LetoVarSet( pCurrentConn, szGroup, szVar, cType,
                          cType == LETOVAR_STR ? hb_itemGetCPtr( pValue ) : szValue, ulLen,
                          uiFlags, pRetValue && cType < LETOVAR_STR ? pRetValue : NULL );
   if( *uiRes )  /* sucessful set */
      leto_SetVarCache( pVarItem );
   else
   {
      hb_itemRelease( pVarItem );
      cType = '\0';
   }

   return cType;
}

/*
 * LETO_VARSET( cGroupName, cVarName, xValue[, nFlags[, @xRetValue]] ) --> lSuccess
 */
HB_FUNC( LETO_VARSET )  // ToDo hb_parc(1) and 2 need AllTrim
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   unsigned int     uiRes = 0;

   if( pCurrentConn )
   {
      if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && HB_ISCHAR( 2 ) && hb_parclen( 2 ) && hb_param( 3, LETOVAR_TYPES ) )
      {
         HB_USHORT uiFlags = ( ! HB_ISNUM( 4 ) ) ? 0 : ( HB_USHORT ) hb_parni( 4 );
         HB_BOOL   fPrev = HB_ISBYREF( 5 );
         char *    pRetValue = NULL;
         char      cType = '\0';

         if( ! strchr( hb_parc( 1 ), ';' ) && ! strchr( hb_parc( 2 ), ';' ) )  /* illegal char in name */
            cType = Leto_VarSet( pCurrentConn, hb_parc( 1 ), hb_parc( 2 ), hb_param( 3, LETOVAR_TYPES ), uiFlags,
                                 fPrev ? &pRetValue : NULL, &uiRes );
         if( fPrev && uiRes > 3 && pRetValue && cType && cType < LETOVAR_STR )
         {
            char * ptr = pRetValue;

            cType = *ptr;
            ptr += 2;
            if( cType == LETOVAR_LOG )
               hb_storl( *ptr == '1', 5 );
            else if( cType == LETOVAR_NUM )
            {
               char * pDec = strchr( ptr, '.' );

               if( pDec )
               {
                  PHB_ITEM pItem = hb_stackItemFromBase( 5 );

                  hb_itemPutNDLen( hb_itemUnRef( pItem ), atof( ptr ), uiRes - 3, uiRes - 3 - ( pDec - ptr + 1 ) );
               }
               else
                 hb_stornl( atol( ptr ), 5 );
            }
         }
      }
   }

   hb_retl( uiRes );
}

static PHB_ITEM Leto_VarGet( LETOCONNECTION * pCurrentConn, const char * szGroup, const char * szVar )
{
   unsigned long ulLen = 0;
   const char *  pData;
   PHB_ITEM      pValue = NULL;

   if( ( pData = LetoVarGet( pCurrentConn, szGroup, szVar, &ulLen ) ) != NULL )
   {
      switch( *pData )
      {
         case LETOVAR_LOG:
            pValue = hb_itemPutL( pValue, *( pData + 2 ) == '1' );
            break;

         case LETOVAR_NUM:
         {
            char * ptr = strchr( pData + 2, '.' );

            if( ptr )
            {
               int iLen = strlen( pData + 2 );

               pValue = hb_itemPutNDLen( pValue, atof( pData + 2 ), iLen, iLen - ( ptr - pData + 3 ) );
            }
            else
               pValue = hb_itemPutNL( pValue, atol( pData + 2 ) );
            break;
         }

         case LETOVAR_STR:
            pValue = hb_itemPutCL( pValue, pData + 2, ulLen );
            break;

         case LETOVAR_ARR:
         {
            HB_SIZE nSize = ( HB_SIZE ) ulLen;

            pData += 2;
            pValue = hb_itemDeserialize( &pData, &nSize );
            break;
         }

         case LETOVAR_DAT:
            pValue = hb_itemPutDS( pValue, pData + 2 );
            break;
      }
   }

   return pValue;
}

/*
 * LETO_VARGET( cGroupName, cVarName ) --> xValue
 */
HB_FUNC( LETO_VARGET )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   PHB_ITEM pValue = NULL;

   if( pCurrentConn )
   {
      if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && HB_ISCHAR( 2 ) && hb_parclen( 2 )  )
      {
         if( ! strchr( hb_parc( 1 ), ';' ) && ! strchr( hb_parc( 2 ), ';' ) )  /* illegal char in name */
            pValue = Leto_VarGet( pCurrentConn, hb_parc( 1 ), hb_parc( 2 ) );
      }
   }

   if( pValue )
      hb_itemReturnRelease( pValue );
   else
      hb_ret();
}

#ifndef LETO_NO_MT

/* retrieves THREAD LOCAL last leto_Var[Set|Inc|Dec] *value* */
HB_FUNC( LETO_VARGETCACHED )
{
   /* hb_itemReturn( leto_GetVarCached() ); */
   hb_itemReturn( *( PHB_ITEM * ) hb_stackGetTSD( &s_TSDitem ) );   /* do not ! release */
}
#else

HB_FUNC( LETO_VARGETCACHED )
{
   hb_itemReturn( s_TSDitem );   /* do not ! release */
}

#endif

/*
 * LETO_VARINCR( cGroupName, cVarName[, nFlags ) --> nValue
 */
HB_FUNC( LETO_VARINCR )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   HB_LONG          lValue;

   if( pCurrentConn )
   {
      if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && HB_ISCHAR( 2 ) && hb_parclen( 2 ) )
      {
         if( ! strchr( hb_parc( 1 ), ';' ) && ! strchr( hb_parc( 2 ), ';' ) )  /* illegal char in name */
         {
            int iFlag = ( ! HB_ISNUM( 3 ) ) ? 0 : hb_parni( 3 );

            lValue = LetoVarIncr( pCurrentConn, hb_parc( 1 ), hb_parc( 2 ), iFlag );
            if( ! pCurrentConn->iError )
            {
               leto_SetVarCache( hb_itemPutNL( NULL, lValue ) );
               hb_retnl( lValue );
               return;
            }
            else
               pCurrentConn->iError = 0;
         }
      }
   }
   hb_ret();
}

/*
 * LETO_VARDECR( cGroupName, cVarName[, nFlags ) --> nValue
 */
HB_FUNC( LETO_VARDECR )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   HB_LONG          lValue;

   if( pCurrentConn )
   {
      if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && HB_ISCHAR( 2 ) && hb_parclen( 2 ) )
      {
         if( ! strchr( hb_parc( 1 ), ';' ) && ! strchr( hb_parc( 2 ), ';' ) )  /* illegal char in name */
         {
            int iFlag = ( ! HB_ISNUM( 3 ) ) ? 0 : hb_parni( 3 );

            lValue = LetoVarDecr( pCurrentConn, hb_parc( 1 ), hb_parc( 2 ), iFlag );
            if( ! pCurrentConn->iError )
            {
               leto_SetVarCache( hb_itemPutNL( NULL, lValue ) );
               hb_retnl( lValue );
               return;
            }
            else
               pCurrentConn->iError = 0;
         }
      }
   }
   hb_ret();
}

HB_FUNC( LETO_VARDEL )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
   {
      if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) )
      {
         const char * szVar = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : "";

         if( ! strchr( hb_parc( 1 ), ';' ) && ! strchr( szVar, ';' ) )  /* illegal char in name */
         {
            if( LetoVarDel( pCurrentConn, hb_parc( 1 ), szVar ) )
            {
               leto_ClearVarCache();
               hb_retl( HB_TRUE );
            }
            else
               hb_retl( HB_FALSE );
            return;
         }
      }
   }
   hb_retl( HB_FALSE );
}

HB_FUNC( LETO_VARGETLIST )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   const char * ptr;
   const char * pGroup = ( ! HB_ISCHAR( 1 ) ) ? NULL : hb_parc( 1 );
   HB_LONG      lMaxLen = ( HB_ISNUM( 2 ) ) ? hb_parnl( 2 ) : -1;

   if( pCurrentConn )
   {
      if( ( ! pGroup || ! strchr( pGroup, ';' ) ) &&  /* illegal char in name */
          ( ptr = LetoVarGetList( pCurrentConn, pGroup, lMaxLen ) ) != NULL )
      {
         PHB_ITEM     pTmp;
         PHB_ITEM     aInfo, aVar;
         const char * ptr2;
         HB_USHORT    uiItems, ui = 1;
         HB_UCHAR     uLenLen;
         HB_ULONG     ulValLength;

         uiItems = ( HB_USHORT ) atoi( ptr );
         while( *ptr && *ptr++ != ';' )
            ;
         aInfo = hb_itemArrayNew( uiItems );

         while( ui <= uiItems )
         {
            if( pGroup && lMaxLen >= 0 )
            {
               char cType;

               if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )  /* safety */
                  break;
               aVar = hb_arrayGetItemPtr( aInfo, ui );
               hb_arrayNew( aVar, 2 );

               hb_itemPutCL( hb_arrayGetItemPtr( aVar, 1 ), ptr, ptr2 - ptr );  /* var name */
               ptr = ++ptr2;

               cType = *ptr++;
               ptr++;

               if( ( uLenLen = ( ( ( HB_UCHAR ) *ptr ) & 0xFF ) ) < 10 && uLenLen )
               {
                  ulValLength = leto_b2n( ptr + 1, uLenLen );
                  ptr += uLenLen + 1;
               }
               else  /* should never happen */
               {
                  ui++;
                  continue;
               }

               switch( cType )
               {
                  case LETOVAR_LOG:
                     hb_itemPutL( hb_arrayGetItemPtr( aVar, 2 ), ( *ptr == '1' ) );
                     break;

                  case LETOVAR_NUM:  /* new: int or double value possible */
                     ptr2 = ptr;
                     while( ptr2 < ptr + ulValLength )
                     {
                        if( *ptr2 == '.' )
                           break;
                        ptr2++;
                     }
                     if( *ptr2 != '.' )
                        hb_itemPutNL( hb_arrayGetItemPtr( aVar, 2 ), atol( ptr ) );
                     else
                        hb_itemPutND( hb_arrayGetItemPtr( aVar, 2 ), atof( ptr ) );
                     break;

                  case LETOVAR_STR:
                     hb_itemPutCL( hb_arrayGetItemPtr( aVar, 2 ), ptr, ulValLength );
                     break;

                  case LETOVAR_ARR:
                     if( lMaxLen == 0 )
                     {
#ifndef __XHARBOUR__
                        const char * ptrTmp = ptr;
                        HB_SIZE      nSize = ( HB_SIZE ) ulValLength;
                        PHB_ITEM     pArr = hb_itemDeserialize( &ptrTmp, &nSize );

                        hb_itemMove( hb_arrayGetItemPtr( aVar, 2 ), pArr );
                        if( pArr )
                           hb_itemRelease( pArr );
#else
                        hb_arrayNew( hb_arrayGetItemPtr( aVar, 2 ), 0 );
#endif
                     }
                     else
                        hb_itemPutC( hb_arrayGetItemPtr( aVar, 2 ), "{ ... }" );
                     break;

                  case LETOVAR_DAT:
                     hb_itemPutDS( hb_arrayGetItemPtr( aVar, 2 ), ptr );
                     break;
               }
               ptr += ulValLength;
            }
            else
            {
               if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )  /* safety */
                  break;
               pTmp = hb_itemPutCL( NULL, ptr, ptr2 - ptr );
               ptr = ++ptr2;
               hb_itemArrayPut( aInfo, ui, pTmp );
               hb_itemRelease( pTmp );
            }
            ui++;
         }

         hb_itemReturnRelease( aInfo );
         return;
      }
   }
   hb_ret();
}

HB_FUNC( LETO_GETLOCALIP )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
   {
      if( HB_ISLOG( 1 ) && hb_parl( 1 ) )
         hb_retc( pCurrentConn->pAddr );
      else
      {
         PHB_ITEM pIFaces = hb_socketGetIFaces( HB_SOCKET_AF_INET, HB_TRUE );

         if( pIFaces && HB_IS_ARRAY( pIFaces ) )
         {
            const char * pAddrGroupEnd = strrchr( pCurrentConn->pAddr, '.' );
            const char * pAddress;
            HB_SIZE n = 1;

            if( ! pAddrGroupEnd )
            {
               hb_retc( "" );
               return;
            }

            while( n <=  hb_arrayLen( pIFaces ) )
            {
               pAddress = hb_itemGetCPtr( hb_arrayGetItemPtr( hb_arrayGetItemPtr( pIFaces, n ), HB_SOCKET_IFINFO_ADDR ) );
               if( ! strncmp( pCurrentConn->pAddr, pAddress, pAddrGroupEnd - pCurrentConn->pAddr + 1 ) )
               {
                  hb_retc( pAddress );
                  break;
               }
               n++;
            }

            if( n > hb_arrayLen( pIFaces ) )
               hb_retc( "" );
            hb_itemRelease( pIFaces );
         }
      }
   }
   else
      hb_retc( "" );
}

HB_FUNC( LETO_ADDCDPTRANSLATE )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn && hb_parclen( 1 ) && hb_parclen( 2 ) )
   {
      PCDPSTRU pCdps;
      if( pCurrentConn->pCdpTable )
      {
         pCdps = pCurrentConn->pCdpTable;
         while( pCdps->pNext )
            pCdps = pCdps->pNext;
         pCdps = hb_xgrab( sizeof( CDPSTRU ) );
      }
      else
         pCdps = pCurrentConn->pCdpTable = hb_xgrab( sizeof( CDPSTRU ) );
      pCdps->szClientCdp = ( char * ) hb_xgrab( hb_parclen( 1 ) + 1 );
      strcpy( pCdps->szClientCdp, hb_parc( 1 ) );
      pCdps->szServerCdp = ( char * ) hb_xgrab( hb_parclen( 2 ) + 1 );
      strcpy( pCdps->szServerCdp, hb_parc( 2 ) );
      pCdps->pNext = NULL;

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( LETO_UDFEXIST )
{
   LETOCONNECTION * pConnection = NULL;
   char szFuncName[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) )
      pConnection = letoParseParam( hb_parc( 1 ), szFuncName );

   if( pConnection )
   {
      char         szData[ HB_SYMBOL_NAME_LEN + 9 ];
      const char * ptr = ( szFuncName[ 0 ] == '/' ) ? szFuncName + 1 : szFuncName;

      hb_snprintf( szData, HB_SYMBOL_NAME_LEN + 9, "%c;3;;;%s;", LETOCMD_udf_fun, ptr );
      if( ! leto_DataSendRecv( pConnection, szData, 0 ) )
         hb_retl( HB_FALSE );
      else
      {
         ptr = leto_firstchar( pConnection );
         hb_retl( ( *ptr == 'T' ) );
      }
   }
   else
      hb_retl( HB_FALSE );
}

static HB_BOOL leto_IsLetoArea( LETOAREAP pArea )
{
   if( pArea )
   {
      HB_USHORT uiRddID;
      LPRDDNODE pRDDNode = hb_rddFindNode( "LETO", &uiRddID );

      if( pRDDNode && pArea->area.rddID == uiRddID )
         return HB_TRUE;
   }

   return HB_FALSE;
}

void leto_udp( HB_BOOL fInThread, PHB_ITEM pArray )
{
   LETOCONNECTION * pConnection;
   LETOAREAP        pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   char             szFuncName[ HB_PATH_MAX ];
   HB_ULONG         ulMemSize;
   HB_USHORT        uLen;
   HB_BOOL          fLetoArea = leto_IsLetoArea( pArea );

   if( ! pArray )
      pConnection = letoParseParam( hb_parc( 1 ), szFuncName );
   else
   {
      hb_strncpy( szFuncName, hb_arrayGetCPtr( pArray, 1 ), HB_SYMBOL_NAME_LEN + 1 );
      szFuncName[ hb_arrayGetCLen( pArray, 1 ) ] = '\0';
      pConnection = letoGetCurrConn();
   }

   uLen = ( HB_USHORT ) strlen( szFuncName );
   if( strchr( szFuncName, ';' ) != NULL || uLen > HB_PATH_MAX )  /* pDynSym is max 63 */
   {
      hb_retl( HB_FALSE );
      return;
   }  /* correct an often typo for function name without barackets */
   else if( uLen > 2 && szFuncName[ uLen - 2 ] == '(' && szFuncName[ uLen - 1 ] == ')' )
      szFuncName[ uLen - 2 ] = '\0';

   if( fLetoArea && ! pConnection )  /* ToDo: conflict with connection possible given in first param */
      pConnection = letoGetConnPool( pArea->pTable->uiConnection );

#if 0  /* ToDo_ this limit is not forcible needed, but advised */
   if( pConnection && ! pConnection->uiServerMode >= 3 )
   {
      hb_ret();
      return;
   }
#endif

   if( pConnection )
   {
      char *  szData;
      char *  pParam = NULL, * ptr;
      HB_SIZE nSize = 0;
      HB_BOOL fExclusive = hb_setGetExclusive();
#ifdef __XHARBOUR__
      PHB_ITEM * pBase;
#endif

      if( ! pArray )
      {
#ifdef __XHARBOUR__
         pBase = hb_stackGetBase( 0 );
         pArray = hb_arrayFromParams( pBase );
#else
         pArray = hb_arrayFromParams( 0 );
#endif
      }
      if( hb_arrayLen( pArray ) > 1 )  /* first item = function name/ codeblock */
      {
#ifndef __XHARBOUR__
         hb_arrayDel( pArray, 1 );
         hb_arraySize( pArray, hb_arrayLen( pArray ) - 1 );
         pParam = hb_itemSerialize( pArray, HB_SERIALIZE_NUMSIZE, &nSize );  //  | HB_SERIALIZE_COMPRESS
#endif
      }
      hb_itemRelease( pArray );

      ulMemSize = hb_parclen( 1 ) + nSize + 42;
      szData = hb_xgrab( ulMemSize );

      ptr = ( szFuncName[ 0 ] == '/' ) ? szFuncName + 1 : szFuncName;
      if( fLetoArea )  /* valid LETOAREA pArea */
         hb_snprintf( szData, ulMemSize, "%c;%lu;%c;%c;%lu;%s;%c;%" HB_PFS "u;", LETOCMD_udf_dbf, pArea->pTable->hTable,
                      fInThread ? '9' : '1', ( char ) ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 ),
                      pArea->pTable->ulRecNo, ptr, fExclusive ? 'T' : 'F', nSize );
      else
         hb_snprintf( szData, ulMemSize, "%c;%c;%c;%lu;%s;%c;%" HB_PFS "u;", LETOCMD_udf_fun,
                      fInThread ? '9' : '2', ( char ) ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 ),
                      0L, ptr, fExclusive ? 'T' : 'F', nSize );
      ptr = szData + strlen( szData );
      if( pParam )
      {
         memcpy( ptr, pParam, nSize );
         hb_xfree( pParam );
      }

      pConnection->iError = 0;
      if( ! leto_DataSendRecv( pConnection, szData, ( ptr - szData ) + nSize ) )
         hb_ret();
      else
      {
         ptr = pConnection->szBuffer;
         if( fInThread )
            hb_retl( ! strncmp( ptr, "+321", 4 ) );
         else if( ptr[ 0 ] == '+' )  /* NEW: pre-leading '+' before valid [ zero ] result */
         {
#ifdef __XHARBOUR__
            hb_retl( HB_TRUE );
#else
            HB_ULONG     ulLen;
            const char * ptrTmp = leto_DecryptText( pConnection, &ulLen, pConnection->szBuffer + 1 );

            if( ulLen )
               hb_itemReturnRelease( hb_itemDeserialize( &ptrTmp, ( HB_SIZE * ) &ulLen ) );
            else
               hb_ret();
#endif
         }
         else
         {
#if 0   /* throw RTE ? */
            if( ptr[ 0 ] == '-' )
            {
               if( ! strncmp( ptr + 1, "ACC", 3 ) )       /* no data access rights */
                  pConnection->iError = -11;
               else if( ! strncmp( ptr + 1, "002", 3 ) )  /* unknown command */
                  pConnection->iError = -2;
               else if( ! strncmp( ptr + 1, "004", 3 ) )  /* error during exec on server */
                  pConnection->iError = -4;
               else
                  pConnection->iError = -1;
            }
#endif
#ifdef __XHARBOUR__
            hb_retl( HB_FALSE );
#else
            hb_ret();
#endif
         }
      }
      hb_xfree( szData );
   }
   else
      hb_ret();
}

HB_FUNC( LETO_RPC )
{
   if( HB_ISCHAR( 1 ) )
      leto_udp( HB_TRUE, NULL );
   else
      hb_ret();
}

HB_FUNC( LETO_UDF )
{
   if( HB_ISCHAR( 1 ) )
      leto_udp( HB_FALSE, NULL );
   else
      hb_ret();
}

#ifndef __XHARBOUR__

/* with szDst == NULL test only and break with first valid memvar -- else collect also into optional 3-dim pArr */
HB_BOOL Leto_VarExprCreate( LETOCONNECTION * pConnection, const char * szSrc, const HB_SIZE nSrcLen, char ** szDst, PHB_ITEM pArr )
{
   HB_SIZE  nStart = 0, nDst = 0, nDstLen = nSrcLen + 1;
   HB_SIZE  nTmp, nTmpLen;
   HB_BOOL  fField, fValid = HB_FALSE;
   HB_UINT  uiRes;
   PHB_DYNS pDyns;
   PHB_ITEM pRefValue, pSub;
   char     szGroup[ 16 ], szVar[ HB_SYMBOL_NAME_LEN + 1 ];
   char     cTmp;

   if( szDst )
      **szDst = '\0';
   while( nStart < nSrcLen )
   {
      if( szSrc[ nStart ] == ' ' )
      {
         nTmp = nStart + 1;
         while( nTmp < nSrcLen && szSrc[ nTmp ] == ' ' )
         {
            nTmp++;
         }
         if( szDst && nDst && nTmp < nSrcLen && HB_ISNEXTIDCHAR( *( *szDst + nDst - 1 ) ) && HB_ISFIRSTIDCHAR( szSrc[ nTmp ] ) )
            *( *szDst + nDst++ ) = ' ';
         nStart = nTmp;
         continue;
      }
      else if( szSrc[ nStart ] == '"' || szSrc[ nStart ] == '\'' )
      {
         cTmp = szSrc[ nStart ];
         nTmp = nStart + 1;
         while( nTmp < nSrcLen && szSrc[ nTmp++ ] != cTmp )
            ;
         if( szDst )
         {
            nTmpLen = nTmp - nStart;
            memcpy( *szDst + nDst, szSrc + nStart, nTmpLen );
            nDst += nTmpLen;
         }
         nStart = nTmp;
         continue;
      }
      else if( HB_ISFIRSTIDCHAR( szSrc[ nStart ] ) )
      {
         nTmp = nStart + 1;
         while( nTmp < nSrcLen && HB_ISNEXTIDCHAR( szSrc[ nTmp ] ) )
         {
            nTmp++;
         }

         if( szSrc[ nTmp ] == '-' && nTmp < nSrcLen - 1 && szSrc[ nTmp + 1 ] == '>' )
         {
            fField = HB_TRUE;
            nTmp += 2;
            while( nTmp <= nSrcLen && HB_ISNEXTIDCHAR( szSrc[ nTmp ] ) )
            {
               nTmp++;
            }
         }
         else
            fField = HB_FALSE;

         nTmpLen = nTmp - nStart;
         if( szSrc[ nTmp ] != '(' && ! fField && nTmpLen <= HB_SYMBOL_NAME_LEN )
         {
            memcpy( szVar, szSrc + nStart, nTmpLen );
            szVar[ nTmpLen ] = '\0';
            pDyns = hb_dynsymFindName( szVar );  /* converts to upper */
            if( pDyns && hb_dynsymIsMemvar( pDyns ) )
            {
               pRefValue = hb_memvarGetValueBySym( pDyns );
               if( pRefValue && ( hb_itemType( pRefValue ) & LETOVAR_TYPES ) )
               {
                  if( ! szDst || ! pConnection )
                  {
                     fValid = HB_TRUE;
                     break;
                  }

                  sprintf( szGroup, "%s%d", LETO_VPREFIX, Leto_MgID( pConnection, HB_FALSE ) );
                  uiRes = 0;
                  if( Leto_VarSet( pConnection, szGroup, szVar, pRefValue, LETO_VCREAT | LETO_VOWN, NULL, &uiRes ) )
                  {
                     nDstLen += 18 + strlen( szGroup );
                     *szDst = ( char * ) hb_xrealloc( *szDst, nDstLen );
                     nDst += sprintf( *szDst + nDst, "Leto_VarGet(%c%s%c,%c%s%c)",
                                                     '\'', szGroup, '\'', '\'', szVar, '\'' );
                     fValid = HB_TRUE;
                     if( pArr )
                     {
                        pSub = hb_itemArrayNew( 4 );
                        hb_itemCloneTo( hb_arrayGetItemPtr( pSub, 3 ), pRefValue );
                        hb_arraySetSymbol( pSub, 4, pDyns );
                        hb_arraySetC( pSub, 1, szGroup );
                        hb_arraySetC( pSub, 2, szVar );
                        hb_arrayAdd( pArr, pSub );
                        hb_itemRelease( pSub );
                     }
                  }
               }
            }
         }
         if( szDst )
         {
            if( ! fValid )
            {
               memcpy( *szDst + nDst, szSrc + nStart, nTmpLen );
               nDst += nTmpLen;
            }
            else
               fValid = HB_FALSE;
         }
         nStart = nTmp;
      }
      else if( szDst )
         *( *szDst + nDst++ ) = *( szSrc + nStart++ );
      else
         nStart++;
   }

   if( szDst )
      *( *szDst + nDst ) = '\0';
   return fValid;
}

/* parse into pArr if ! NULL  *_or_*  Leto_VarDel() */
static void Leto_VarExprParse( LETOCONNECTION * pConnection, const char * szSrc, PHB_ITEM pArr, HB_BOOL fOnlySynVar )
{
   HB_SIZE  nStart = 0, nLen;
   PHB_ITEM pSub;
   PHB_DYNS pDyns;
   PHB_ITEM pRefValue;
   char *   ptr, * szUpper;
   char     cChar;
   char     szGroup[ HB_SYMBOL_NAME_LEN + 1 ];
   char     szVar[ HB_SYMBOL_NAME_LEN + 1 ];

   szUpper = hb_strupr( hb_strdup( szSrc ) );
   while( ( ptr = strstr( szUpper + nStart, "LETO_VARGET(" ) ) != NULL )
   {
      *szGroup = '\0';
      *szVar = '\0';
      nStart += ( ptr - ( szUpper + nStart ) ) + 12;
      while( HB_TRUE )
      {
         cChar = *( szUpper + nStart );
         if( ! cChar || cChar == '\'' || cChar == '"' )
            break;
         nStart++;
      }
      if( cChar )
         ptr = strchr( szUpper + ++nStart, cChar );
      else
         ptr = NULL;
      if( ptr )
      {
         nLen = ptr - ( szUpper + nStart );
         if( nLen > HB_SYMBOL_NAME_LEN )
            continue;
         memcpy( szGroup, szUpper + nStart, nLen );
         szGroup[ nLen ] = '\0';
         nStart += nLen + 1;

         while( HB_TRUE )
         {
            cChar = *( szUpper + nStart );
            if( ! cChar || cChar == '\'' || cChar == '"' )
               break;
            nStart++;
         }
         if( cChar )
            ptr = strchr( szUpper + ++nStart, cChar );
         else
            ptr = NULL;
         if( ptr )
         {
            nLen = ptr - ( szUpper + nStart );
            if( nLen > HB_SYMBOL_NAME_LEN )
               continue;
            memcpy( szVar, szUpper + nStart, nLen );
            szVar[ nLen ] = '\0';
            nStart += nLen + 1;
            if( ! pArr )
            {
               if( pConnection && LetoVarDel( pConnection, szGroup, szVar ) )
                  leto_ClearVarCache();
            }
            else  /* collect into return array */
            {
               pDyns = hb_dynsymFind( szVar );
               if( pDyns && hb_dynsymIsMemvar( pDyns ) )
                  pRefValue = hb_memvarGetValueBySym( pDyns );
               else
                  pRefValue = NULL;
               if( ! pRefValue ? ! fOnlySynVar : HB_TRUE )
               {
                  if( pRefValue && ( hb_itemType( pRefValue ) & LETOVAR_TYPES ) )
                  {
                     pSub = hb_itemArrayNew( 4 );
                     hb_itemCloneTo( hb_arrayGetItemPtr( pSub, 3 ), pRefValue );
                     hb_arraySetSymbol( pSub, 4, pDyns );
                  }
                  else
                     pSub = hb_itemArrayNew( 2 );
                  hb_arraySetC( pSub, 1, szGroup );
                  hb_arraySetC( pSub, 2, szVar );
                  hb_arrayAdd( pArr, pSub );
                  hb_itemRelease( pSub );
               }
            }
         }
      }
   }
   if( szUpper )
      hb_xfree( szUpper );
}

/* if type is different, return sync is 'TRUE' aka do not sync in that case */
static HB_BOOL Leto_VarExprIsSync( PHB_ITEM pRef, PHB_ITEM pArr )
{
   HB_BOOL fSync = HB_TRUE;
   HB_SIZE nLen;
   HB_TYPE nType;

   if( ! pRef || ! pArr )
      return fSync;
   else if( ( nType = hb_itemType( pRef ) ) != hb_itemType( pArr ) )
   {
      if( ( nType & ( HB_IT_INTEGER | HB_IT_LONG ) ) && ( hb_itemType( pArr ) & ( HB_IT_INTEGER | HB_IT_LONG ) ) )
         nType = HB_IT_LONG;
      else
         return fSync;
   }

   switch( nType )
   {
      case HB_IT_STRING:
         nLen = hb_itemGetCLen( pRef );
         fSync = hb_itemGetCLen( pArr ) == nLen;
         if( fSync && nLen )
            fSync = ! memcmp( hb_itemGetCPtr( pRef ), hb_itemGetCPtr( pArr ), nLen );
         break;

      case HB_IT_INTEGER:
         fSync = hb_itemGetNI( pRef ) == hb_itemGetNI( pArr );
         break;

      case HB_IT_LONG:
         fSync = hb_itemGetNL( pRef ) == hb_itemGetNL( pArr );
         break;

      case HB_IT_DATE:
         fSync = hb_itemGetDL( pRef ) == hb_itemGetDL( pArr );
         break;

      case HB_IT_LOGICAL:
         fSync = hb_itemGetL( pRef ) == hb_itemGetL( pArr );
         break;

      case HB_IT_DOUBLE:
         fSync = hb_itemGetND( pRef ) == hb_itemGetND( pArr );
         break;

      case HB_IT_TIMESTAMP:
         fSync = hb_itemGetTD( pRef ) == hb_itemGetTD( pArr );
         break;

      case HB_IT_ARRAY:
         nLen = hb_arrayLen( pRef );
         fSync = hb_arrayLen( pArr ) == nLen;
         if( fSync )
         {
            while( nLen )
            {
               fSync = Leto_VarExprIsSync( hb_arrayGetItemPtr( pRef, nLen ), hb_arrayGetItemPtr( pArr, nLen ) );
               if( ! fSync )
                  break;
               nLen--;
            }
         }
         break;
   }

   return fSync;
}

/* without incrementing reference counters of pSrc */
static void Leto_VarExprClone( PHB_ITEM pDst, PHB_ITEM pSrc )
{
   switch( hb_itemType( pSrc ) )
   {
      case HB_IT_STRING:
         hb_itemPutCL( pDst, hb_itemGetCPtr( pSrc ), hb_itemGetCLen( pSrc ) );
         break;

      case HB_IT_INTEGER:
         hb_itemPutNI( pDst, hb_itemGetNI( pSrc ) );
         break;

      case HB_IT_LONG:
         hb_itemPutNL( pDst, hb_itemGetNL( pSrc ) );
         break;

      case HB_IT_DATE:
         hb_itemPutDL( pDst,  hb_itemGetDL( pSrc ) );
         break;

      case HB_IT_LOGICAL:
         hb_itemPutL( pDst, hb_itemGetL( pSrc ) );
         break;

      case HB_IT_DOUBLE:
         hb_itemPutND( pDst, hb_itemGetND( pSrc ) );
         break;

      case HB_IT_TIMESTAMP:
         hb_itemPutTD( pDst, hb_itemGetTD( pSrc ) );
         break;

      case HB_IT_ARRAY:
         hb_arrayCloneTo( pDst, pSrc );
         break;
   }
}

HB_ERRCODE Leto_VarExprSync( LETOCONNECTION * pConnection, PHB_ITEM pArr, HB_BOOL fReSync )
{
   HB_ERRCODE errCode = HB_FAILURE;

   if( pConnection && pArr && ( hb_itemType( pArr ) & HB_IT_ARRAY ) )
   {
      HB_SIZE  n = 0;
      HB_UINT  uiRes = 0;
      HB_SIZE  nLen = hb_arrayLen( pArr );
      PHB_ITEM pSub, pRefValue, pValue;
      PHB_DYNS pDyns;

      while( ++n <= nLen )
      {
         pSub = hb_arrayGetItemPtr( pArr, n );  /* { pGroup, pVar, pValue } */
         if( ( hb_itemType( pSub ) & HB_IT_ARRAY ) && hb_arrayLen( pSub ) >= 3 &&
             ( hb_itemType( hb_arrayGetItemPtr( pSub, 1 ) ) & HB_IT_STRING ) &&
             ( hb_itemType( hb_arrayGetItemPtr( pSub, 2 ) ) & HB_IT_STRING ) )
         {
            if( hb_arrayLen( pSub ) >= 4 && ( hb_itemType( hb_arrayGetItemPtr( pSub, 4 ) ) & HB_IT_SYMBOL ) )
               pDyns = hb_arrayGetSymbol( pSub, 4 );
            else
            {
               pDyns = hb_dynsymFindName( hb_arrayGetCPtr( pSub, 2 ) );
               if( pDyns && ! hb_dynsymIsMemvar( pDyns ) )
                  pDyns = NULL;
            }
            if( pDyns )
            {
               pRefValue = hb_memvarGetValueBySym( pDyns );
               if( ! fReSync )
               {
                  pValue = hb_arrayGetItemPtr( pSub, 3 );
                  if( ! Leto_VarExprIsSync( pRefValue, pValue ) )
                  {
                     Leto_VarSet( pConnection, hb_arrayGetCPtr( pSub, 1 ), hb_arrayGetCPtr( pSub, 2 ), pRefValue, 0, NULL, &uiRes );
                     if( uiRes )
                     {
                        errCode = HB_SUCCESS;
                        Leto_VarExprClone( pValue, pRefValue );  /* like hb_itemCloneTo() */
                     }
                  }
               }
               else
               {
                  pValue = Leto_VarGet( pConnection, hb_arrayGetCPtr( pSub, 1 ), hb_arrayGetCPtr( pSub, 2 ) );
                  if( ! Leto_VarExprIsSync( pRefValue, pValue ) )
                  {
                     errCode = HB_SUCCESS;
                     hb_itemCloneTo( pRefValue, pValue );
                     hb_itemCloneTo( hb_arrayGetItemPtr( pSub, 3 ), pValue );
                  }
                  if( pValue )
                     hb_itemRelease( pValue );
               }
            }
         }
      }
   }

   return errCode;
}

/* pArr{ { pGroup, pVar, pValue } } */
HB_ERRCODE Leto_VarExprClear( LETOCONNECTION * pConnection, PHB_ITEM pArr )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   if( pConnection && pArr && ( hb_itemType( pArr ) & HB_IT_ARRAY ) )
   {
      HB_SIZE  nLen = hb_arrayLen( pArr );
      HB_SIZE  n = 0;
      PHB_ITEM pSub;

      while( ++n <= nLen )
      {
         pSub = hb_arrayGetItemPtr( pArr, n );
         if( ( hb_itemType( pSub ) & HB_IT_ARRAY ) && hb_arrayLen( pSub ) >= 2 &&
             ( hb_itemType( hb_arrayGetItemPtr( pSub, 1 ) ) & HB_IT_STRING ) &&
             ( hb_itemType( hb_arrayGetItemPtr( pSub, 2 ) ) & HB_IT_STRING ) )
         {
            if( LetoVarDel( pConnection, hb_arrayGetCPtr( pSub, 1 ), hb_arrayGetCPtr( pSub, 2 ) ) )
               leto_ClearVarCache();
         }
         else
            errCode = HB_FAILURE;
      }
   }

   return errCode;
}

HB_FUNC( LETO_VAREXPRTEST )
{
   const char * szSrc = ( HB_ISCHAR( 1 ) && hb_parclen( 1 ) ) ? hb_parc( 1 ) : NULL;

   if( szSrc )
      hb_retl( Leto_VarExprCreate( NULL, szSrc, hb_parclen( 1 ), NULL, NULL ) );
   else
      hb_retl( HB_FALSE );
}

/* search for PRIVATE/ PUBLIC var in expression; unique replace them with Leto_VarGet();
 * call Leto_VarCreate() with LETO_VCREAT | LETO_VOWN flag for them */
HB_FUNC( LETO_VAREXPRCREATE )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   const char *     szSrc = ( HB_ISCHAR( 1 ) && hb_parclen( 1 ) ) ? hb_parc( 1 ) : NULL;
   char *           szDst = NULL;
   PHB_ITEM         pArr = NULL;

   if( pCurrentConn && szSrc )
   {
      szDst = ( char * ) hb_xgrab( hb_parclen( 1 ) + 1 );
      if( HB_ISBYREF( 2 ) )
         pArr = hb_itemArrayNew( 0 );
      Leto_VarExprCreate( pCurrentConn, szSrc, hb_parclen( 1 ), &szDst, pArr );
      if( pArr )
         hb_itemParamStoreRelease( 2, pArr );
   }

   hb_retc_buffer( szDst );
}

/* search for LETO_VARGET in expression and return 2-dim array about them */
HB_FUNC( LETO_VAREXPRVARS )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   const char *     szSrc = ( HB_ISCHAR( 1 ) && hb_parclen( 1 ) ) ? hb_parc( 1 ) : NULL;
   HB_BOOL          fOnlySynVar = hb_parldef( 2, 0 );
   PHB_ITEM         pArr = hb_itemArrayNew( 0 );

   if( pCurrentConn && szSrc )
      Leto_VarExprParse( pCurrentConn, szSrc, pArr, fOnlySynVar );

   hb_itemReturnRelease( pArr );
}

/* search for LETO_VARGET in expression and call Leto_VarDel() for them */
HB_FUNC( LETO_VAREXPRCLEAR )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   const char *     szSrc = ( HB_ISCHAR( 1 ) && hb_parclen( 1 ) ) ? hb_parc( 1 ) : NULL;
   PHB_ITEM         pArr = hb_param( 1, HB_IT_ARRAY );
   HB_BOOL          fOnlySynVar = hb_parldef( 2, 0 );

   if( pCurrentConn && ( szSrc || pArr ) )
   {
      if( szSrc )
         Leto_VarExprParse( pCurrentConn, szSrc, NULL, fOnlySynVar );
      else if( pArr )
         Leto_VarExprClear( pCurrentConn, pArr );
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* two way sync PRIVATE/ PUBLIC variables with Leto_Var's; array collected by LETO_EXPRVARS */
HB_FUNC( LETO_VAREXPRSYNC )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   PHB_ITEM         pArr = hb_param( 1, HB_IT_ARRAY );
   HB_BOOL          fReSync = hb_parldef( 2, 0 );
   HB_BOOL          fSuccess = HB_FALSE;

   if( pCurrentConn && pArr )
      fSuccess = Leto_VarExprSync( pCurrentConn, pArr, fReSync ) == HB_SUCCESS;

   hb_retl( fSuccess );
}

#else

extern char * LetoSetModName( char * szModule );

HB_FUNC( HB_PROGNAME )
{
   char * sFileDefault = LetoSetModName( NULL );

   hb_retc_buffer( sFileDefault );
}

#endif  /* ! __XHARBOUR__ */

