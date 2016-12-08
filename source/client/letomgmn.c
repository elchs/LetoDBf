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


#ifdef __XHARBOUR__
#include "hbstack.h"
#endif

#if defined( __XHARBOUR__ )
   #define hb_snprintf  snprintf
#endif

extern LETOCONNECTION * letoGetConnPool( HB_UINT uiConnection );
extern LETOCONNECTION * letoGetCurrConn( void );
extern HB_USHORT uiGetConnCount( void );

extern void leto_ConnectionClose( LETOCONNECTION * pConnection );
extern LETOCONNECTION * leto_getConnection( int iParam );

extern const char * leto_DecryptText( LETOCONNECTION * pConnection, HB_ULONG * pulLen );
extern void leto_clientlog( const char * sFile, int n, const char * s, ... );
extern HB_BOOL leto_Ping( LETOCONNECTION * pConnection );


#ifdef __XHARBOUR__
char * hb_itemSerialize( PHB_ITEM pItem, HB_BOOL fNumSize, HB_SIZE * pnSize );
PHB_ITEM hb_itemDeserialize( const char ** pBufferPtr, HB_SIZE * pnSize );

int hb_stor( int iParam )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stor(%d)", iParam ) );

   if( iParam == -1 )
   {
      hb_ret();
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemClear( hb_itemUnRef( pItem ) );
         return 1;
      }
   }

   return 0;
}
#endif

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

static PHB_ITEM leto_GetVarCached( void )
{
   return *( PHB_ITEM * ) hb_stackGetTSD( &s_TSDitem );
}

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

static LETOCONNECTION * letoParseFile( const char * szSource, char * szFile )
{
   LETOCONNECTION * pConnection = NULL;
   char szAddr[ 96 ];
   int  iPort = 0;

   HB_TRACE( HB_TR_DEBUG, ( "letoParseFile(%s, %s)", szSource, szFile ) );

   szAddr[ 0 ] = '\0';

   if( leto_getIpFromPath( szSource, szAddr, &iPort, szFile, HB_TRUE ) &&
       ( ( ( pConnection = leto_ConnectionFind( szAddr, iPort ) ) != NULL ) ||
         ( ( pConnection = LetoConnectionNew( szAddr, iPort, NULL, NULL, 0, HB_FALSE ) ) != NULL ) ) )
   {
      HB_USHORT uiPathLen = ( HB_USHORT ) strlen( szFile );

      leto_getFileFromPath( szSource, szFile + uiPathLen, HB_PATH_MAX - uiPathLen );
   }

   return pConnection;
}

HB_FUNC( LETO_FERROR )
{
   hb_retni( LetoGetError() );
}

LETOCONNECTION * letoParseParam( const char * szParam, char * szFile )
{
   LETOCONNECTION * pConnection;

   if( ! szParam )
   {
      pConnection = letoGetCurrConn();
      szFile[ 0 ] = '\0';
   }
   else if( strlen( szParam ) > 2 && szParam[ 0 ] == '/' && szParam[ 1 ] == '/' )
      pConnection = letoParseFile( szParam, szFile );
   else
   {
      pConnection = letoGetCurrConn();
      hb_strncpy( szFile, szParam, HB_PATH_MAX - 1 );
   }

   return pConnection;
}

HB_FUNC( LETO_FILE )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      pConnection->iError = -10;
      hb_retl( LetoFileExist( pConnection, szFile ) );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( LETO_FERASE )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      pConnection->iError = -10;
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

   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      pConnection->iError = -10;
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

   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      pConnection->iError = -10;
      if( LetoFileCopy( pConnection, szFile, leto_RemoveIpFromPath( hb_parc( 2 ) ) ) )
         hb_retni( 0 );
      else
         hb_retni( -1 );
   }
   else
      hb_retni( -1 );
}

HB_FUNC( LETO_MEMOREAD )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];
   unsigned long ulMemoLen = 0;

   if( HB_ISCHAR( 1 ) )
   {
      pConnection = letoParseParam( hb_parc( 1 ), szFile );
      if( ! pConnection )
         pConnection = letoGetCurrConn();
      if( pConnection )
      {
         const char * ptr;

         pConnection->iError = -10;
         if( ( ptr = LetoMemoRead( pConnection, szFile, &ulMemoLen ) ) != NULL && ulMemoLen )
            hb_retclen( ptr, ulMemoLen );
         else
            hb_retc( "" );
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
         return;
      }
   }

   hb_retc( "" );
}

/* leto_FileRead( cFile, nStart, nLen [ 0 == whole , @cBuf ) */
HB_FUNC( LETO_FILEREAD )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];
   unsigned long ulLen = hb_parnl( 3 );

   if( HB_ISCHAR( 1 ) && HB_ISBYREF( 4 ) )
   {
      pConnection = letoParseParam( hb_parc( 1 ), szFile );
      if( ! pConnection )
         pConnection = letoGetCurrConn();
      if( pConnection )
      {
         const char * ptr;

         pConnection->iError = -10;
         if( ( ptr = LetoFileRead( pConnection, szFile, hb_parnl( 2 ), &ulLen ) ) != NULL )
         {
            hb_storclen( ptr, ulLen, 4 );
            hb_retnl( ulLen );
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

   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      pConnection->iError = -10;
      hb_retl( LetoMemoWrite( pConnection, szFile, hb_parc( 2 ), hb_parclen( 2 ) ) );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( LETO_FILEWRITE )  /* ( cFile, nStart, cBuf ) */
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];
   unsigned long ulBufLen = hb_parclen( 3 );

   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 3 ) && ulBufLen > 0 && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      pConnection->iError = -10;
      hb_retl( LetoFileWrite( pConnection, szFile, hb_parc( 3 ), hb_parnl( 2 ), ulBufLen ) );
   }
   else
      hb_retl( HB_FALSE );
}


HB_FUNC( LETO_FILESIZE )  /* ( cFile ) */
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      pConnection->iError = -10;
      hb_retnl( LetoFileSize( pConnection, szFile ) );
      return;
   }

   hb_retnl( -1 );
}


HB_FUNC( LETO_FILEATTR )  /* ( cFile [, cNewAttr ] ) */
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      pConnection->iError = -10;
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
   HB_ULONG         ulLen;

   if( ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      if( ! *szFile )
         strcpy( szFile, "." );
      pConnection->iError = -10;
      ptr = LetoDirectory( pConnection, szFile, HB_ISCHAR( 2 ) ? hb_parc( 2 ) : NULL );
      if( ptr != NULL )
      {
         ptr = leto_DecryptText( pConnection, &ulLen );
         if( ulLen )
         {
            hb_itemReturnRelease( hb_itemDeserialize( &ptr, ( HB_SIZE * ) &ulLen ) );
            return;
         }
      }
      else
      {
         hb_itemReturnRelease( hb_itemArrayNew( 0 ) );
         return;
      }
   }

   hb_ret();
}

HB_FUNC( LETO_MAKEDIR )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      pConnection->iError = -10;
      if( LetoDirMake( pConnection, szFile ) )
         hb_retni( 0 );
      else
         hb_retni( -1 );
   }
   else
      hb_retni( -1 );
}

HB_FUNC( LETO_DIREXIST )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      pConnection->iError = -10;
      hb_retl( LetoDirExist( pConnection, szFile ) );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( LETO_DIRREMOVE )
{
   LETOCONNECTION * pConnection;
   char szFile[ HB_PATH_MAX ];

   if( HB_ISCHAR( 1 ) && ( pConnection = letoParseParam( hb_parc( 1 ), szFile ) ) != NULL )
   {
      pConnection->iError = -10;
      if( LetoDirRemove( pConnection, szFile ) )
         hb_retni( 0 );
      else
         hb_retni( -1 );
   }
   else
      hb_retni( -1 );
}

HB_FUNC( LETO_CONNECT )
{
   LETOCONNECTION * pConnection;
   char szAddr[ 96 ];
   char szPath[ HB_PATH_MAX ];
   int  iPort = 0;
   const char * szUser = ( HB_ISCHAR( 2 ) && hb_parclen( 2 ) ) ? hb_parc( 2 ) : NULL;
   const char * szPass = ( HB_ISCHAR( 3 ) && hb_parclen( 3 ) ) ? hb_parc( 3 ) : NULL;
   int          iTimeOut = ( HB_ISNUM( 4 ) ) ? hb_parni( 4 ) : -1;
   HB_BOOL      fZombieCheck = ( HB_ISLOG( 6 ) ) ? hb_parl( 6 ) : HB_TRUE;
   int          iRet = -1;

   HB_TRACE( HB_TR_DEBUG, ( "LETO_CONNECT()" ) );

   szAddr[ 0 ] = '\0';
   szPath[ 0 ] = '\0';

   if( HB_ISCHAR( 1 ) && leto_getIpFromPath( hb_parc( 1 ), szAddr, &iPort, szPath, HB_FALSE ) &&
       ( ( ( pConnection = leto_ConnectionFind( szAddr, iPort ) ) != NULL ) ||
         ( ( pConnection = LetoConnectionNew( szAddr, iPort, szUser, szPass, iTimeOut, fZombieCheck ) ) != NULL ) ) )
   {
      if( HB_ISNUM( 5 ) )
         pConnection->iBufRefreshTime = hb_parni( 5 );

      if( strlen( szPath ) > 1 && pConnection->szPath == NULL )
         LetoSetPath( pConnection, szPath );
      iRet = pConnection->iConnection;
   }

   hb_retni( iRet );
}

HB_FUNC( LETO_DISCONNECT )
{
   LETOCONNECTION * pConnection = leto_getConnection( 1 );

   if( pConnection )
      leto_ConnectionClose( pConnection );

   hb_ret();
}

#if 0  /* LetoDBf does not support to change the active connection, should be handled automatical ? */
HB_FUNC( LETO_SETCURRENTCONNECTION )
{
   LETOCONNECTION * pCurrentConn = leto_getConnection( 1 );
   hb_ret();
}
#endif

/* in case of no pCurrentConn --> + 1 -1 == 0 correct invalid answer */
HB_FUNC( LETO_GETCURRENTCONNECTION )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();

   if( pConnection )
      hb_retni( pConnection->iConnection + 1 );
   else
      hb_retni( 0 );
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

HB_FUNC( LETO_PATH )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
   {
      hb_retc( pCurrentConn->szPath ? pCurrentConn->szPath : "" );
      if( HB_ISCHAR( 1 ) )
         LetoSetPath( pCurrentConn, hb_parc( 1 ) );
   }
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
               hb_arrayNew( pArrayItm, 5 );
               for( j = 1; j <= 5; j++ )
               {
                  if( ( ptr2 = LetoFindCmdItem( ptr ) ) == NULL )
                     return;
                  if( j < 5 )
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
            hb_retclen( ++ptr, ulLen );
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
   {
      char szData[ 6 ];

      eprintf( szData, "%c;08;", LETOCMD_mgmt );
      if( leto_DataSendRecv( pConnection, szData, 5 ) )
      {
         const char * ptr = leto_firstchar( pConnection );

         if( *ptr == '+' && *( ptr + 3 ) == ';' )
            iRet = atoi( ptr + 4 );
      }
   }

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
         char * szPass = ( char * ) hb_xgrab( ( iKeyLen + 9 ) * 2 );;

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

         hb_snprintf( szData, 95, "%c;%d;%s;", LETOCMD_zip, iZipRecord, szPass );
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
#ifdef USE_LZ4
                  hb_lz4netEncryptKey( pConnection->zstream, hb_parc( 2 ), iKeyLen );
#else
                  hb_znetEncryptKey( pConnection->zstream, hb_parc( 2 ), iKeyLen );
#endif
                  pConnection->fZipCrypt = HB_TRUE;
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

/*
 * LETO_VARSET( cGroupName, cVarName, xValue[, nFlags[, @xRetValue]] ) --> lSuccess
 */
HB_FUNC( LETO_VARSET )  // ToDo hb_parc(1) and 2 need AllTrim
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   char *       ptr;
   char         szValue[ 32 ];
   unsigned int uiRes = 0;
   HB_USHORT    uiFlags = ( ! HB_ISNUM( 4 ) ) ? 0 : ( HB_USHORT ) hb_parni( 4 );
   HB_BOOL      fPrev = HB_ISBYREF( 5 );
   char *       pRetValue = NULL;
   PHB_ITEM     pVarItem;

   if( pCurrentConn )
   {
      pCurrentConn->iError = -10;
      if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) && ( HB_ISLOG( 3 ) || HB_ISNUM( 3 ) || HB_ISCHAR( 3 ) || HB_ISARRAY( 3 ) ) )
      {
         char     cType;
         HB_ULONG ulLen;

         if( HB_ISLOG( 3 ) )
         {
            cType = '1';
            if( hb_parl( 3 ) )
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
         else if( HB_ISNUM( 3 ) )
         {
            cType = '2';
            if( HB_IS_INTEGER( hb_param( 3, HB_IT_ANY ) ) )
            {
               pVarItem = hb_itemPutNL( NULL, hb_parnl( 3 ) );
               hb_snprintf( szValue, 32, "%ld", hb_parnl( 3 ) );
            }
            else
            {
               pVarItem = hb_itemPutND( NULL, hb_parnd( 3 ) );
               hb_snprintf( szValue, 32, "%f", hb_parnd( 3 ) );
            }
            ulLen = strlen( szValue );
         }
         else if( HB_ISCHAR( 3 ) )
         {
            cType = '3';
            ulLen = hb_parclen( 3 );
            pVarItem = hb_itemPutCL( NULL, hb_parc( 3 ), ulLen );
         }
         else  /* if( HB_ISARRAY( 3 ) ) */
         {
            HB_SIZE nSize = 0;
            char *  pArr = hb_itemSerialize( hb_param( 3, HB_IT_ARRAY ), HB_SERIALIZE_NUMSIZE, &nSize );  //  | HB_SERIALIZE_COMPRESS

            cType = '4';
            ulLen = ( HB_ULONG ) nSize;
            pVarItem = hb_itemClone( hb_param( 3, HB_IT_ARRAY ) );
            uiRes = LetoVarSet( pCurrentConn, hb_parc( 1 ), hb_parc( 2 ), cType, pArr, ( HB_ULONG ) nSize, uiFlags, NULL );
            if( pArr )
               hb_xfree( pArr );
         }

         if( cType < '4' )
            uiRes = LetoVarSet( pCurrentConn, hb_parc( 1 ), hb_parc( 2 ), cType,
                                cType == '3' ? hb_parc( 3 ) : szValue, ulLen,
                                uiFlags, fPrev && cType != '3' ? &pRetValue : NULL );
         if( uiRes )  /* sucessful set */
            leto_SetVarCache( pVarItem );
         else
            hb_itemRelease( pVarItem );

         if( fPrev && uiRes >= 3 && pRetValue )
         {
            ptr = pRetValue;
            cType = *ptr;
            ptr += 2;
            if( cType == '1' )
               hb_storl( *ptr == '1', 5 );
            else if( cType == '2' )
               hb_stornl( atol( ptr ), 5 );
            else if( cType == '3' )
               hb_storclen( ptr, uiRes - 3, 5 );
            else
               hb_stor( 5 );
         }

      }
      else
      {
         hb_retl( HB_FALSE );
         return;
      }
   }
   hb_retl( uiRes );
}

/*
 * LETO_VARGET( cGroupName, cVarName ) --> xValue
 */
HB_FUNC( LETO_VARGET )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
   {
      HB_ULONG     ulLen = 0;
      const char * pData;

      pCurrentConn->iError = -10;
      if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
      {
         if( ( pData = LetoVarGet( pCurrentConn, hb_parc( 1 ), hb_parc( 2 ), &ulLen ) ) != NULL )
         {
            switch( *pData )
            {
               case '1':  /*boolean */
                  hb_retl( *( pData + 2 ) == '1' );
                  break;
               case '2':  /* numeric */
                  if( strchr( pData + 2, '.' ) )
                     hb_retnd( atof( pData + 2 ) );
                  else
                     hb_retnl( atol( pData + 2 ) );
                  break;
               case '3':   /* [ binary ] string */
                  hb_retclen( pData + 2, ulLen );
                  break;
               case '4':   /* array from deserialized string */
               {
                  HB_SIZE  nSize = ( HB_SIZE ) ulLen;
                  PHB_ITEM pArray = NULL;

                  pData += 2;
                  pArray = hb_itemDeserialize( &pData, &nSize );
                  hb_itemReturnRelease( pArray );
                  break;
               }
            }
            return;
         }
      }
   }

   hb_ret();
}

HB_FUNC( LETO_VARGETCACHED )
{
   hb_itemReturn( leto_GetVarCached() );   /* do not ! release */
}

/*
 * LETO_VARINCR( cGroupName, cVarName[, nFlags ) --> nValue
 */
HB_FUNC( LETO_VARINCR )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   HB_LONG          lValue;

   if( pCurrentConn )
   {
      pCurrentConn->iError = -10;
      if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
      {
         int iFlag = ( ! HB_ISNUM( 3 ) ) ? 0 : hb_parni( 3 );

         lValue = LetoVarIncr( pCurrentConn, hb_parc( 1 ), hb_parc( 2 ), iFlag );
         if( ! LetoGetError() )
         {
            leto_SetVarCache( hb_itemPutNL( NULL, lValue ) );
            hb_retnl( lValue );
            return;
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
      pCurrentConn->iError = -10;
      if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
      {
         int iFlag = ( ! HB_ISNUM( 3 ) ) ? 0 : hb_parni( 3 );

         lValue = LetoVarDecr( pCurrentConn, hb_parc( 1 ), hb_parc( 2 ), iFlag );
         if( ! LetoGetError() )
         {
            leto_SetVarCache( hb_itemPutNL( NULL, lValue ) );
            hb_retnl( lValue );
            return;
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
      if( HB_ISCHAR( 1 ) )
      {
         if( LetoVarDel( pCurrentConn, hb_parc( 1 ), HB_ISCHAR( 2 ) ? hb_parc( 2 ) : "" ) )
         {
            leto_ClearVarCache();
            hb_retl( HB_TRUE );
         }
         else
            hb_retl( HB_FALSE );
         return;
      }
   }
   hb_retl( HB_FALSE );
}

HB_FUNC( LETO_VARGETLIST )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();
   const char * ptr;
   const char * pGroup = ( ! HB_ISCHAR( 1 ) ) ? NULL : hb_parc( 1 );
   HB_USHORT    uiMaxLen = ( HB_ISNUM( 2 ) ) ? ( HB_USHORT ) hb_parni( 2 ) : 0;

   if( pCurrentConn )
   {
      pCurrentConn->iError = -10;
      if( ! pGroup || uiMaxLen > 999 )
         uiMaxLen = 0;
      if( ( ptr = LetoVarGetList( pCurrentConn, pGroup, uiMaxLen ) ) != NULL )
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
            if( pGroup && uiMaxLen )
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
               else
               {
                  ulValLength = 0;
                  ptr++;
               }

               switch( cType )
               {
                  case '1':
                     hb_itemPutL( hb_arrayGetItemPtr( aVar, 2 ), ( *ptr == 1 ) );
                     break;

                  case '2':  /* new: int or double value possible */
                  {
                     const char * ptr3 = ptr;
                     HB_BOOL      fDec = HB_FALSE;

                     while( ptr3 < ptr + ulValLength )
                     {
                        if( *ptr3++ == '.' )
                        {
                           fDec = HB_TRUE;
                           break;
                        }
                     }
                     if( ! fDec )
                        hb_itemPutNL( hb_arrayGetItemPtr( aVar, 2 ), atol( ptr ) );
                     else
                        hb_itemPutND( hb_arrayGetItemPtr( aVar, 2 ), atof( ptr ) );
                     break;
                  }

                  case '3':
                     hb_itemPutCL( hb_arrayGetItemPtr( aVar, 2 ), ptr, ulValLength );
                     break;

                  case '4':
                     hb_itemPutC( hb_arrayGetItemPtr( aVar, 2 ), "{ ... }" );
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
      hb_retc( pCurrentConn->pAddr );
   else
      hb_retc( "" );
}

HB_FUNC( LETO_ADDCDPTRANSLATE )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn && HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
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
   }
}


HB_FUNC( LETO_UDFEXIST )
{
   LETOCONNECTION * pConnection = NULL;
   char szFuncName[ HB_SYMBOL_NAME_LEN + 1 ];

   if( HB_ISCHAR( 1 ) )
      pConnection = letoParseParam( hb_parc( 1 ), szFuncName );

   if( pConnection )
   {
      char         szData[ HB_SYMBOL_NAME_LEN + 6 ];
      const char * ptr = ( szFuncName[ 0 ] == '/' ) ? szFuncName + 1 : szFuncName;

      hb_snprintf( szData, HB_SYMBOL_NAME_LEN + 6, "%c;3;;;%s;", LETOCMD_udf_fun, ptr );
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

void leto_udp( HB_BOOL fInThread, PHB_ITEM pArray )
{
   LETOCONNECTION * pConnection;
   LETOAREAP        pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   char             szFuncName[ HB_SYMBOL_NAME_LEN + 1 ];
   HB_ULONG         ulMemSize;
   HB_USHORT        uLen;
   /* HB_BOOL          fLeto = leto_CheckArea( pArea ); ==> if( pArea ) */

   if( ! pArray )
      pConnection = letoParseParam( hb_parc( 1 ), szFuncName );
   else
   {
      strncpy( szFuncName, hb_arrayGetCPtr( pArray, 1 ), HB_SYMBOL_NAME_LEN );
      szFuncName[ hb_arrayGetCLen( pArray, 1 ) ] = '\0';
      pConnection = letoGetCurrConn();
   }

   uLen = ( HB_USHORT ) strlen( szFuncName );
   if( strchr( szFuncName, ';' ) != NULL || uLen > 20 )
   {
      hb_retl( HB_FALSE );
      return;
   }  /* correct an often typo for function name without barackets */
   else if( uLen > 2 && szFuncName[ uLen - 2 ] == '(' && szFuncName[ uLen - 1 ] == ')' )
      szFuncName[ uLen - 2 ] = '\0';

   if( pArea )
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
      if( hb_arrayLen( pArray ) > 1 )  /* first item = funtion name */
      {
         hb_arrayDel( pArray, 1 );
         hb_arraySize( pArray, hb_arrayLen( pArray ) - 1 );
         pParam = hb_itemSerialize( pArray, HB_SERIALIZE_NUMSIZE, &nSize );  //  | HB_SERIALIZE_COMPRESS
      }
      hb_itemRelease( pArray );

      ulMemSize = hb_parclen( 1 ) + nSize + 42;
      szData = hb_xgrab( ulMemSize );

      ptr = ( szFuncName[ 0 ] == '/' ) ? szFuncName + 1 : szFuncName;
      if( pArea )
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
         HB_BOOL fIsRPC = HB_FALSE;

         ptr = pConnection->szBuffer;
         if( ptr[ 0 ] == '-' )
         {
            if( ! strncmp( ptr + 1, "ACC", 3 ) )       /* no data access rights */
               pConnection->iError = -11;
            else if( ! strncmp( ptr + 1, "002", 3 ) )  /* unknown command */
               pConnection->iError = -2;
            else if( ! strncmp( ptr + 1, "004", 3 ) )  /* error during exec on server */
               pConnection->iError = -4;
         }
         else if( ! strncmp( ptr, "+321", 4 ) )        /* positive ack from starting thread */
         {
            fIsRPC = HB_TRUE;
            pConnection->iError = 0;
         }

         if( pConnection->iError )
            hb_ret();
         else if( fIsRPC )
            hb_retl( HB_TRUE );
         else
         {
            HB_ULONG     ulLen;
            const char * ptrTmp = leto_DecryptText( pConnection, &ulLen );

            if( ulLen )
               hb_itemReturnRelease( hb_itemDeserialize( &ptrTmp, ( HB_SIZE * ) &ulLen ) );
            else
               hb_ret();
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

