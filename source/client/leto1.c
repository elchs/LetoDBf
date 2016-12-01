/*
 * Harbour Leto RDD
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
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
#include <ctype.h>


#if defined( HB_OS_UNIX )
   #include <netinet/in.h>
   #include <arpa/inet.h>
#endif

#if defined ( _MSC_VER )
  #define _WINSOCKAPI_
#endif

#define SUPERTABLE   ( &s_letoSuper )

typedef struct
{
   unsigned int uiConnection;
   HB_ULONG     ulAreaID;
   LETOAREAP    pArea;
} FINDAREASTRU;

static HB_USHORT s_uiRddCount = 0;
static HB_USHORT s_uiRddIdLETO = ( HB_USHORT ) -1;
static RDDFUNCS  s_letoSuper;


extern HB_ERRCODE delayedError( void );
extern HB_USHORT uiGetConnCount( void );
extern LETOCONNECTION * letoGetConnPool( HB_UINT uiConnection );
extern LETOCONNECTION * letoGetCurrConn( void );

extern void leto_clientlog( const char * sFile, int n, const char * s, ... );

extern LETOCONNECTION * letoParseParam( const char * szParam, char * szFile );
extern void leto_udp( HB_BOOL fInThread, PHB_ITEM pArray );

#if defined( __XHARBOUR__ ) || defined( __HARBOUR30__ )
   extern char * LetoSetModName( char * szModule );
#endif


static HB_BYTE leto_ItemType( PHB_ITEM pItem )
{
   switch( hb_itemType( pItem ) )
   {
      case HB_IT_STRING:
      case HB_IT_STRING | HB_IT_MEMO:
         return 'C';

      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
         return 'N';

      case HB_IT_DATE:
         return 'D';

      case HB_IT_TIMESTAMP:
         return 'T';

      case HB_IT_LOGICAL:
         return 'L';

      default:
         return 'U';
   }
}

HB_FUNC( LETO_LOOKERROR )
{
   hb_retni( delayedError() );
}

static HB_ERRCODE commonError( LETOAREAP pArea, HB_ERRCODE uiGenCode, HB_ERRCODE uiSubCode,
                            HB_USHORT uiOsCode, const char * szFileName, HB_USHORT uiFlags,
                            const char * szOperation )
{
   HB_ERRCODE errCode = 0;

   HB_SYMBOL_UNUSED( pArea );
   if( ! hb_vmRequestQuery() )  /* avoid more errormessages, we want to quit */
   {
      PHB_ITEM pError = hb_errNew();

      hb_errPutGenCode( pError, uiGenCode );
      hb_errPutSubCode( pError, uiSubCode );
      if( szOperation )
         hb_errPutOperation( pError, szOperation );
      else
         hb_errPutDescription( pError, hb_langDGetErrorDesc( uiGenCode ) );
      if( uiOsCode )
         hb_errPutOsCode( pError, uiOsCode );
      if( szFileName )
         hb_errPutFileName( pError, szFileName );
      if( uiFlags )
         hb_errPutFlags( pError, uiFlags );
      errCode = SUPER_ERROR( ( AREAP ) pArea, pError );
      hb_itemRelease( pError );
   }

   return errCode;
}

static const char * leto_getRcvBuff( void )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   return ( const char * ) pCurrentConn->szBuffer;
}

static long leto_SendRecv( LETOCONNECTION * pConnection, LETOAREAP pArea, const char * sData, HB_ULONG ulLen, int iErr )
{
   long   lRet = leto_DataSendRecv( pConnection, sData, ulLen );
   char * ptr = pConnection->szBuffer;

   if( ! lRet )
      commonError( pArea, EG_DATAWIDTH, 1000, 0, NULL, 0, "CONNECTION ERROR" );
   else if( *ptr == '-'  && iErr )
   {
      char * ptrErr;

      ptrErr = strchr( ptr, ':' );
      if( ptrErr && ptrErr - ptr == 4 )
      {
         HB_ULONG uiGenCode, uiSubCode = 0, uiOsCode = 0, uiFlags = 0;
         char *   szFileName = NULL, * szDescription = NULL;

         /* "%lu-%lu-%lu-%lu" + [\t| ] + [filename[description] */
         uiGenCode = strtoul( ptrErr, &ptrErr, 10 );
         if( ! uiGenCode )
            uiGenCode = EG_DATATYPE;
         if( ptrErr )
            uiSubCode = strtoul( ++ptrErr, &ptrErr, 10 );
         if( ptrErr )
            uiOsCode = strtoul( ++ptrErr, &ptrErr, 10 );
         if( ptrErr )
            uiFlags = strtoul( ++ptrErr, &ptrErr, 10 );
         if( ptrErr )
         {
            if( *ptrErr == '\t' )
               szFileName = ptrErr;
            else if( *ptrErr == '!' )
               szDescription = ptrErr;
         }
         if( ! uiSubCode )
            uiSubCode = iErr;

         commonError( pArea, ( HB_USHORT ) uiGenCode, ( HB_USHORT ) uiSubCode, ( HB_USHORT ) uiOsCode,
                      szFileName, ( HB_USHORT ) uiFlags, szDescription );
      }
      else
         commonError( pArea, EG_DATATYPE, iErr, 0, ptr, 0, NULL );
      return 0;
   }
   return lRet;
}

static _HB_INLINE_ void leto_SetAreaFlags( LETOAREAP pArea )
{
   pArea->area.fBof = pArea->pTable->fBof;
   pArea->area.fEof = pArea->pTable->fEof;
   pArea->area.fFound = pArea->pTable->fFound;
}

static _HB_INLINE_ HB_BOOL leto_CheckArea( LETOAREAP pArea )
{
   return pArea && ( ( AREAP ) pArea )->rddID == s_uiRddIdLETO;
}

static _HB_INLINE_ HB_BOOL leto_CheckAreaConn( AREAP pArea, LETOCONNECTION * pConnection )
{
   /* return leto_CheckArea( ( LETOAREAP ) pArea ) && */
   return pArea && pArea->rddID == s_uiRddIdLETO &&
          ( letoGetConnPool( ( ( LETOAREAP ) pArea )->pTable->uiConnection ) == pConnection );
}

static HB_ERRCODE leto_doClose( AREAP pArea, void * p )
{
   if( leto_CheckAreaConn( pArea, ( LETOCONNECTION * ) p ) )
   {
      ( ( LETOCONNECTION * ) p )->fCloseAll = HB_TRUE;
      SELF_CLOSE( pArea );
   }
   return HB_SUCCESS;
}

static int leto_CloseAll( LETOCONNECTION * pConnection )
{
   hb_rddIterateWorkAreas( leto_doClose, ( void * ) pConnection );

   if( pConnection->fCloseAll )
   {
      pConnection->fCloseAll = HB_FALSE;
      return LetoCloseAll( pConnection );
   }

   return 1;
}

void leto_ConnectionClose( LETOCONNECTION * pConnection )
{
   if( pConnection->pAddr )
   {
      leto_CloseAll( pConnection );
      LetoConnectionClose( pConnection );
   }
}

static void leto_ParseRec( LETOCONNECTION * pConnection, LETOAREAP pArea, const char * szData )
{
   pConnection->iError = 0;
   leto_ParseRecord( pConnection, pArea->pTable, szData, HB_TRUE );
   if( ! pConnection->iError )
      leto_SetAreaFlags( pArea );
   else
      commonError( pArea, EG_DATATYPE, 1022, 0, NULL, 0, NULL );
}

static HB_BOOL leto_CheckError( LETOAREAP pArea )
{
   const char * ptr = leto_getRcvBuff();

   if( *ptr == '-' )
   {
      HB_ULONG uiGenCode, uiSubCode, uiOsCode, uiFlags;
      char *   szFileName = NULL, * szDescription = NULL;

      if( ptr[ 4 ] == ':' )
      {
         sscanf( ptr + 5, "%lu-%lu-%lu-%lu", &uiGenCode, &uiSubCode, &uiOsCode, &uiFlags );
         if( ( szFileName = strchr( ptr + 5, '\t' ) ) != NULL )
            szFileName++;
         else if( ( szDescription = strchr( ptr + 5, '!' ) ) != NULL )
            szDescription++;
      }
      else
      {
         uiGenCode = EG_DATATYPE;
         uiSubCode = 1021;
         uiOsCode = 0;
         uiFlags = 0;
      }

      commonError( pArea, ( HB_USHORT ) uiGenCode, ( HB_USHORT ) uiSubCode, ( HB_USHORT ) uiOsCode,
                   szFileName, ( HB_USHORT ) uiFlags, szDescription );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

static _HB_INLINE_ void leto_PutRec( LETOAREAP pArea, HB_BOOL fCommit )
{
   if( LetoDbPutRecord( pArea->pTable, fCommit ) != 0 )
   {
      if( LetoGetError() == 1000 )
         commonError( pArea, EG_DATATYPE, LetoGetError(), 0, NULL, 0, NULL );
      else
         leto_CheckError( pArea );
   }
}

static void leto_CreateKeyExpr( LETOAREAP pArea, LETOTAGINFO * pTagInfo, PHB_ITEM pKeyExp )
{
   if( ! pTagInfo->pExtra )
      pTagInfo->pExtra = hb_xgrabz( sizeof( LETOTAGEXTRAINFO ) );
   if( *pTagInfo->KeyExpr )
   {
      pTagInfo->uiFieldTag = hb_rddFieldExpIndex( ( AREAP ) pArea, pTagInfo->KeyExpr );
      if( pKeyExp )
         ( ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra )->pKeyItem = hb_itemNew( pKeyExp );
      else if( SELF_COMPILE( ( AREAP ) pArea, pTagInfo->KeyExpr ) != HB_FAILURE )
      {
         ( ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra )->pKeyItem = pArea->area.valResult;
         pArea->area.valResult = NULL;
      }
   }
}

#if 0  /* temporary outcommented */
/* ToDo: nice! idea, but where is the result used ? -- temporary outcommented */
static void ScanIndexFields( LETOAREAP pArea, LETOTAGINFO * pTagInfo )
{
   HB_USHORT          uiCount = pArea->area.uiFieldCount;
   HB_USHORT          uiIndex, uiFound = 0;
   char *             szKey = hb_strdup( pTagInfo->KeyExpr ), * pKey, * ptr;
   char               szName[ 12 ];
   HB_BOOL            fFound = HB_FALSE;
   unsigned int       uiLen;
   LETOTAGEXTRAINFO * pExtra = ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra;

   pExtra->puiFields = ( HB_USHORT * ) hb_xgrab( uiCount * sizeof( HB_USHORT ) );
   hb_strUpper( szKey, strlen( szKey ) );

   for( uiIndex = 1; uiIndex <= uiCount; uiIndex++ )
   {
      szName[ 0 ] = '\0';  /* Harbous does so ! */
      SELF_FIELDNAME( ( AREAP ) pArea, uiIndex, szName );
      uiLen = strlen( szName );
      pKey = szKey;
      while( strlen( pKey ) >= uiLen && ( ptr = strstr( pKey, szName ) ) != NULL )
      {
         if( ( ( ptr == szKey ) || ! HB_ISNEXTIDCHAR( *( ptr - 1 ) ) ) && ! HB_ISNEXTIDCHAR( *( ptr + uiLen ) ) )
         {
            fFound = HB_TRUE;
            break;
         }
         pKey = ptr + uiLen; // strlen( szName );
      }
      if( fFound )
      {
         pExtra->puiFields[ uiFound++ ] = uiIndex;
         fFound = HB_FALSE;
      }
   }

   hb_xfree( szKey );

   if( ! uiFound )
   {
      hb_xfree( pExtra->puiFields );
      pExtra->puiFields = NULL;
   }
   else if( uiFound < uiCount )
      pExtra->puiFields = ( HB_USHORT * ) hb_xrealloc( pExtra->puiFields, uiFound * sizeof( HB_USHORT ) );
   pExtra->uiFCount = uiFound;
}
#endif


/* -- LETO RDD METHODS --  */

static HB_ERRCODE letoBof( LETOAREAP pArea, HB_BOOL * pBof )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoBof(%p, %p)", pArea, pBof ) );

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   *pBof = pArea->area.fBof;

   return HB_SUCCESS;
}

static HB_ERRCODE letoEof( LETOAREAP pArea, HB_BOOL * pEof )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoEof(%p, %p)", pArea, pEof ) );

   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   *pEof = pArea->area.fEof;

   return HB_SUCCESS;
}

static HB_ERRCODE letoFound( LETOAREAP pArea, HB_BOOL * pFound )
{
   /* resolve any pending relations */
   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   *pFound = pArea->area.fFound;

   return HB_SUCCESS;
}

static HB_ERRCODE letoGoBottom( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoGoBottom(%p)", pArea ) );

   pArea->lpdbPendingRel = NULL;
   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( LetoDbGoBottom( pTable ) )
      return HB_FAILURE;
   else
   {
      leto_SetAreaFlags( pArea );
      if( SELF_SKIPFILTER( ( AREAP ) pArea, -1 ) == HB_SUCCESS )
      {
         if( pArea->area.lpdbRelations )
            return SELF_SYNCCHILDREN( ( AREAP ) pArea );
      }
      else
         return HB_FAILURE;

      return HB_SUCCESS;
   }
}

static HB_ERRCODE letoGoTo( LETOAREAP pArea, HB_ULONG ulRecNo )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoGoTo(%p, %lu)", pArea, ulRecNo ) );

   pArea->lpdbPendingRel = NULL;
   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( LetoDbGoTo( pTable, ( unsigned long ) ulRecNo ) )
      return HB_FAILURE;
   else
   {
      leto_SetAreaFlags( pArea );
      if( pArea->area.lpdbRelations )
         return SELF_SYNCCHILDREN( ( AREAP ) pArea );
      else
         return HB_SUCCESS;
   }
}

static HB_ERRCODE letoGoToId( LETOAREAP pArea, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoGoToId(%p, %p)", pArea, pItem ) );

   if( HB_IS_NUMERIC( pItem ) )
      return SELF_GOTO( ( AREAP ) pArea, hb_itemGetNL( pItem ) );
   else
   {
      commonError( pArea, EG_DATATYPE, 1020, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }
}

static HB_ERRCODE letoGoTop( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoGoTop(%p)", pArea ) );

   pArea->lpdbPendingRel = NULL;
   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( LetoDbGoTop( pTable ) )
      return HB_FAILURE;
   else
   {
      leto_SetAreaFlags( pArea );
      if( SELF_SKIPFILTER( ( AREAP ) pArea, 1 ) == HB_SUCCESS )
      {
         if( pArea->area.lpdbRelations )
            return SELF_SYNCCHILDREN( ( AREAP ) pArea );
      }
      else
         return HB_FAILURE;
   }

   return HB_SUCCESS;
}

static HB_USHORT leto_KeyToStr( LETOAREAP pArea, char * szKey, char cType, PHB_ITEM pKey, HB_USHORT uiMaxLen )
{
   HB_USHORT uiKeyLen;

   switch( cType )
   {
      case 'N':
      {
         int iWidth, iDec;

         hb_itemGetNLen( pKey, &iWidth, &iDec );
         uiKeyLen = ( HB_USHORT ) ( iDec ? iWidth + iDec + 1 : iWidth );
         hb_itemStrBuf( szKey, pKey, uiKeyLen, iDec );  /* 256 bytes buffer szKey shell fit */
         break;
      }

      case 'C':
         if( ( uiKeyLen = ( HB_USHORT ) hb_itemGetCLen( pKey ) ) > uiMaxLen )
            uiKeyLen = uiMaxLen;
#if defined ( __XHARBOUR__ )
         memcpy( szKey, hb_itemGetCPtr( pKey ), uiKeyLen );
         if( pArea->area.cdPage != HB_CDP_PAGE() )
            hb_cdpnTranslate( szKey, HB_CDP_PAGE(), pArea->area.cdPage, uiKeyLen );
#else
         if( pArea->area.cdPage != HB_CDP_PAGE() )
         {
            char * pBuff = hb_cdpnDup( hb_itemGetCPtr( pKey ), ( HB_SIZE * ) &uiKeyLen, HB_CDP_PAGE(), pArea->area.cdPage );

            memcpy( szKey, pBuff, uiKeyLen );
            hb_xfree( pBuff );
         }
         else
            memcpy( szKey, hb_itemGetCPtr( pKey ), uiKeyLen );
#endif
         break;

      case 'D':
         hb_itemGetDS( pKey, szKey );
         uiKeyLen = 8;
         break;

      case 'L':
         szKey[ 0 ] = ( hb_itemGetL( pKey ) ) ? 'T' : 'F';
         uiKeyLen = 1;
         break;

      case 'T':
         hb_itemGetTS( pKey, szKey );
         uiKeyLen = ( HB_USHORT ) strlen( szKey ); // 17 ?;
         break;

      default:  /* means error in type */
         uiKeyLen = 0;
   }
   szKey[ uiKeyLen ] = '\0';

   return uiKeyLen;
}

static int leto_ItmCompare( PHB_ITEM pValue, PHB_ITEM pItem, HB_BOOL fExact )
{
   int iRet = 1;

   if( HB_IS_STRING( pValue ) && HB_IS_STRING( pItem ) )
      iRet = hb_itemStrCmp( pValue, pItem, fExact );
   else if( HB_IS_DATE( pValue ) && HB_IS_DATE( pItem ) )
   {
#ifdef __XHARBOUR__
      if( pItem->item.asDate.value == pValue->item.asDate.value &&
          pItem->item.asDate.time == pValue->item.asDate.time )
#else
      if( hb_itemGetTD( pItem ) == hb_itemGetTD( pValue ) )
#endif
         iRet = 0;
#ifdef __XHARBOUR__
      else if( pValue->item.asDate.value < pItem->item.asDate.value )
#else
      else if( hb_itemGetTD( pValue ) < hb_itemGetTD( pItem ) )
#endif
         iRet = -1;
   }
   else if( HB_IS_NUMINT( pValue ) && HB_IS_NUMINT( pItem ) )
   {
      HB_MAXINT l1 = hb_itemGetNInt( pValue );
      HB_MAXINT l2 = hb_itemGetNInt( pItem );

      if( l1 == l2 )
         iRet = 0;
      else if( l1 < l2 )
         iRet = -1;
   }
   else if( HB_IS_NUMBER( pValue ) && HB_IS_NUMBER( pItem ) )
   {
      double d1 = hb_itemGetND( pValue );
      double d2 = hb_itemGetND( pItem );

      if( d1 == d2 )
         iRet = 0;
      else if( d1 < d2 )
         iRet = -1;
   }
   else if( HB_IS_LOGICAL( pValue ) && HB_IS_LOGICAL( pItem ) )
   {
      HB_BOOL f1 = hb_itemGetL( pValue );
      HB_BOOL f2 = hb_itemGetL( pItem );

      if( f1 == f2 )
         iRet = 0;
      else if( ! f1 )
         iRet = -1;
   }

   return iRet;
}

static PHB_ITEM leto_KeyEval( LETOAREAP pArea, LETOTAGINFO * pTagInfo )
{
   PHB_ITEM pItem;

   if( pTagInfo->uiFieldTag )
   {
      pItem = hb_stackReturnItem();
      SELF_GETVALUE( ( AREAP ) pArea, pTagInfo->uiFieldTag, pItem );
   }
   else
      pItem = hb_vmEvalBlockOrMacro( ( ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra )->pKeyItem );
   return pItem;
}

static HB_ERRCODE letoSeek( LETOAREAP pArea, HB_BOOL fSoftSeek, PHB_ITEM pKey, HB_BOOL fFindLast )
{
   LETOTABLE *   pTable = pArea->pTable;
   LETOTAGINFO * pTagInfo = pTable->pTagCurrent;
   char          cType;

   pArea->lpdbPendingRel = NULL;
   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( ! pTagInfo )
   {
      commonError( pArea, EG_NOORDER, 1201, 0, NULL, EF_CANDEFAULT, NULL );
      return HB_FAILURE;
   }

   if( ( cType = leto_ItemType( pKey ) ) != pTagInfo->cKeyType )
   {
      if( LetoDbSeek( pTable, NULL, 0, HB_FALSE, HB_FALSE ) )
         return HB_FAILURE;
   }
   else
   {
      char      szKey[ LETO_MAX_KEY ];
      HB_USHORT uiKeyLen = leto_KeyToStr( pArea, szKey, cType, pKey, pTagInfo->uiKeySize );

      if( LetoDbSeek( pTable, szKey, uiKeyLen, fSoftSeek, fFindLast ) )
         return HB_FAILURE;
   }
   leto_SetAreaFlags( pArea );

   if( pArea->area.dbfi.itmCobExpr && ! pArea->area.dbfi.fOptimized )
   {
      if( SELF_SKIPFILTER( ( AREAP ) pArea, ( fFindLast ? -1 : 1 ) ) == HB_SUCCESS )
      {
         PHB_ITEM pItem = leto_KeyEval( pArea, pTagInfo );

         if( ! fSoftSeek && leto_ItmCompare( pItem, pKey, HB_FALSE ) != 0 )
            SELF_GOTO( ( AREAP ) pArea, 0 );
      }
      else
         return HB_FAILURE;
   }

   if( pArea->area.lpdbRelations )
      return SELF_SYNCCHILDREN( ( AREAP ) pArea );
   return HB_SUCCESS;
}

#define letoSkip  NULL

static HB_ERRCODE letoSkipFilter( LETOAREAP pArea, HB_LONG lUpDown )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoSkipFilter(%p, %ld)", pArea, lUpDown ) );

   if( pArea->area.dbfi.itmCobExpr != NULL && ! pArea->area.dbfi.fOptimized )
      return SUPER_SKIPFILTER( ( AREAP ) pArea, lUpDown );
   return HB_SUCCESS;
}

static HB_ERRCODE letoSkipRaw( LETOAREAP pArea, HB_LONG lToSkip )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoSkipRaw(%p, %ld)", pArea, lToSkip ) );

   pArea->lpdbPendingRel = NULL;
   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( LetoDbSkip( pTable, lToSkip ) == HB_SUCCESS )
      leto_SetAreaFlags( pArea );
   else
   {
      int iErr = 1000;

      if( LetoGetError() != 1000 )
      {
         LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
         const char * ptr = leto_firstchar( pConnection );

         iErr = atoi( ptr );
      }
      commonError( pArea, EG_DATATYPE, iErr, 0, NULL, 0, "SKIP FAILED" );
      return HB_FAILURE;
   }

   if( pArea->area.lpdbRelations )
      return SELF_SYNCCHILDREN( ( AREAP ) pArea );
   return HB_SUCCESS;
}


static HB_ERRCODE letoAddField( LETOAREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoAddField(%p, %p)", pArea, pFieldInfo ) );

   return SUPER_ADDFIELD( ( AREAP ) pArea, pFieldInfo );
}

static void leto_AddRecLock( LETOTABLE * pTable, unsigned long ulRecNo )
{
   unsigned long ulPos = 0;

   /* [re-]allocate locks array for the table */
   if( ! pTable->pLocksPos )
   {
      pTable->ulLocksAlloc = 10;
      pTable->pLocksPos = ( HB_ULONG * ) hb_xgrab( sizeof( HB_ULONG ) * pTable->ulLocksAlloc );
      pTable->ulLocksMax = 0;
   }
   else if( pTable->ulLocksMax == pTable->ulLocksAlloc )
   {
      pTable->pLocksPos = ( HB_ULONG * ) hb_xrealloc( pTable->pLocksPos, sizeof( HB_ULONG ) * ( pTable->ulLocksAlloc + 10 ) );
      pTable->ulLocksAlloc += 10;
   }

   /* search if already registered */
   while( ulPos < pTable->ulLocksMax )
   {
      if( pTable->pLocksPos[ ulPos ] == ulRecNo )
         break;
      ulPos++;
   }

   if( ulPos == pTable->ulLocksMax )
      pTable->pLocksPos[ pTable->ulLocksMax++ ] = ulRecNo;
}

static HB_ERRCODE letoAppend( LETOAREAP pArea, HB_BOOL fUnLockAll )
{
   LETOTABLE *      pTable = pArea->pTable;
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

   HB_TRACE( HB_TR_DEBUG, ( "letoAppend(%p, %d)", pArea, ( int ) fUnLockAll ) );

   if( pTable->fReadonly )
   {
      commonError( pArea, EG_READONLY, EDBF_READONLY, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }
   pArea->lpdbPendingRel = NULL;

   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );
   pArea->area.fBof = pArea->area.fEof = pArea->area.fFound = HB_FALSE;

   if( LetoDbAppend( pTable, fUnLockAll ) )
      return HB_FAILURE;
   if( ! pConnection->fTransActive )
   {
      if( fUnLockAll )
         pTable->ulLocksMax = 0;
      leto_AddRecLock( pTable, pTable->ulRecNo );
   }

   if( pArea->area.lpdbRelations )
      return SELF_SYNCCHILDREN( ( AREAP ) pArea );
   return HB_SUCCESS;
}

#define letoCreateFields  NULL

static HB_ERRCODE letoDeleteRec( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoDeleteRec(%p)", pArea ) );

   if( pTable->fShared && ! pTable->fFLocked && ! pTable->fRecLocked )
   {
      commonError( pArea, EG_UNLOCKED, EDBF_UNLOCKED, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }
   else if( pTable->fReadonly )
   {
      commonError( pArea, EG_READONLY, EDBF_READONLY, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }

   if( ( pTable->uiUpdated & LETO_FLAG_UPD_DELETE ) || ! pTable->fDeleted )
   {
      pTable->fDeleted = HB_TRUE;
      pTable->uiUpdated |= LETO_FLAG_UPD_DELETE;
      pTable->pRecord[ 0 ] = '*';
   }
   return HB_SUCCESS;
}

static HB_ERRCODE letoDeleted( LETOAREAP pArea, HB_BOOL * pDeleted )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoDeleted(%p, %p)", pArea, pDeleted ) );

   *pDeleted = pArea->pTable->fDeleted;
   return HB_SUCCESS;
}

#define letoFieldCount    NULL
#define letoFieldDisplay  NULL

static HB_ERRCODE letoFieldInfo( LETOAREAP pArea, HB_USHORT uiIndex, HB_USHORT uiType, PHB_ITEM pItem )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoFieldInfo(%p, %hu, %hu, %p)", pArea, uiIndex, uiType, pItem ) );

   if( ! uiIndex || uiIndex > pTable->uiFieldExtent )
      return HB_FAILURE;

   if( pTable->fHaveAutoinc && ( uiType == DBS_COUNTER || uiType == DBS_STEP ) )
   {
      LETOFIELD * pField = pTable->pFields + uiIndex - 1;

      if( pField->uiType == HB_FT_AUTOINC || pField->uiType == HB_FT_ROWVER ||
          ( pField->uiFlags & HB_FF_AUTOINC ) )
      {
         LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
         char         szData[ 42 ];
         HB_ULONG     ulLen;
         const char * ptr;

         ulLen = eprintf( szData, "%c;%lu;%d;%d;", LETOCMD_dbi, pTable->hTable,
                          uiType == DBS_COUNTER ? DBI_DBS_COUNTER : DBI_DBS_STEP, uiIndex );
         if( HB_IS_NUMERIC( pItem ) )
            ulLen += eprintf( szData + ulLen, "%d;", hb_itemGetNInt( pItem ) );
         else
         {
            szData[ ulLen++ ] = ';';
            szData[ ulLen ] = '\0';
         }
         ulLen = ( HB_ULONG ) leto_SendRecv( pConnection, pArea, szData, ulLen, 0 );

         if( ulLen )
         {
            ptr = pConnection->szBuffer;
            if( *ptr++ == '+' )
               hb_itemPutNL( pItem, strtoul( ptr, NULL, 10 ) );
            else
               ulLen = 0;
         }

         if( ulLen )
            return HB_SUCCESS;
      }
      hb_itemClear( pItem );
      return HB_FAILURE;
   }
   else
      return SUPER_FIELDINFO( &pArea->area, uiIndex, uiType, pItem );
}

#define letoFieldName     NULL

static HB_ERRCODE letoFlush( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoFlush(%p)", pArea ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( ! letoGetConnPool( pTable->uiConnection )->fTransActive )
   {
      if( LetoDbCommit( pTable ) )
         return HB_FAILURE;
   }

   return HB_SUCCESS;
}

static HB_ERRCODE letoGetRec( LETOAREAP pArea, HB_BYTE ** pBuffer )
{
   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   if( pBuffer != NULL )
      *pBuffer = pArea->pTable->pRecord;
   return HB_SUCCESS;
}

static HB_ERRCODE leto_GetMemoValue( LETOAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem, HB_USHORT uiType )
{
   LETOTABLE *  pTable = pArea->pTable;
   HB_ULONG     ulLen;
   const char * ptr = LetoDbGetMemo( pTable, uiIndex, &ulLen );

   if( ! ptr )
      return HB_FAILURE;
   else if( ! ulLen )
      hb_itemPutC( pItem, "" );
   else
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );

#if defined ( __XHARBOUR__ )
      if( uiType == HB_FT_MEMO && pArea->area.cdPage != HB_CDP_PAGE() )
         hb_cdpnTranslate( ptr, pArea->area.cdPage, HB_CDP_PAGE(), ulLen - 1 );  // ToFix const char *
      hb_itemPutCL( pItem, ptr, ulLen );  //ToElk
#else
      if( uiType == HB_FT_MEMO && pArea->area.cdPage != HB_CDP_PAGE() )
      {
         char *  pBuff;
         HB_SIZE ulItemLen = ulLen;

         pBuff = hb_cdpnDup( ptr, &ulItemLen, pArea->area.cdPage, HB_CDP_PAGE() );
         hb_itemPutCL( pItem, pBuff, ulItemLen );
         hb_xfree( pBuff );
      }
      else
         hb_itemPutCL( pItem, ptr, ulLen );
#endif
      if( pConnection->ulBufCryptLen > LETO_SENDRECV_BUFFSIZE )
      {
         hb_xfree( pConnection->pBufCrypt );
         pConnection->ulBufCryptLen = 0;
      }
      if( pConnection->szBuffer && pConnection->ulBufferLen > LETO_SENDRECV_BUFFSIZE )
      {
         pConnection->szBuffer = hb_xrealloc( pConnection->szBuffer, LETO_SENDRECV_BUFFSIZE + 1 );
         pConnection->ulBufferLen = LETO_SENDRECV_BUFFSIZE;
      }
   }
   return HB_SUCCESS;
}

static HB_ERRCODE letoGetValue( LETOAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   LETOTABLE * pTable = pArea->pTable;
   LPFIELD     pField;

   HB_TRACE( HB_TR_DEBUG, ( "letoGetValue(%p, %hu, %p)", pArea, uiIndex, pItem ) );

   if( ! uiIndex || uiIndex > pArea->area.uiFieldCount )
      return HB_FAILURE;
   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   uiIndex--;
   pField = pArea->area.lpFields + uiIndex;

   switch( pField->uiType )
   {
      case HB_FT_STRING:
         if( pArea->area.cdPage != HB_CDP_PAGE() )
         {
#if defined ( __XHARBOUR__ )
            char * pVal = ( char * ) hb_xgrab( pField->uiLen + 1 );
            memcpy( pVal, pTable->pRecord + pTable->pFieldOffset[ uiIndex ], pField->uiLen );
            pVal[ pField->uiLen ] = '\0';
            hb_cdpnTranslate( pVal, pArea->area.cdPage, HB_CDP_PAGE(), pField->uiLen );
            hb_itemPutCL( pItem, pVal, pField->uiLen );
            hb_xfree( pVal );
#else
            char *  pVal;
            HB_SIZE uiKeyLen = ( HB_SIZE ) pField->uiLen;

            pVal = hb_cdpnDup( ( const char * ) pTable->pRecord + pTable->pFieldOffset[ uiIndex ], &uiKeyLen, pArea->area.cdPage, HB_CDP_PAGE() );
            hb_itemPutCLPtr( pItem, pVal, pField->uiLen );
#endif
         }
         else
            hb_itemPutCL( pItem, ( char * ) pTable->pRecord + pTable->pFieldOffset[ uiIndex ],
                          pField->uiLen );
         break;

      case HB_FT_LONG:
      {
         HB_MAXINT lVal;
#ifdef _MSC_VER
         HB_BOOL fDbl;
         double  dVal;

         /* elch to fix with faster version */
         fDbl = hb_strnToNum( ( const char * ) pTable->pRecord + pTable->pFieldOffset[ uiIndex ],
                              pField->uiLen, &lVal, &dVal );
         if( pField->uiDec )
            hb_itemPutNDLen( pItem, fDbl ? dVal : ( double ) lVal,
                             ( int ) ( pField->uiLen - pField->uiDec - 1 ),
                             ( int ) pField->uiDec );
         else if( fDbl )
            hb_itemPutNDLen( pItem, dVal, ( int ) pField->uiLen, 0 );
         else
            hb_itemPutNIntLen( pItem, lVal, ( int ) pField->uiLen );
#else    /* elch version with replace of hb_strnToNum() -> faster strtold()/ strtol()  */
         char        szTmp[ 21 ];

         memcpy( szTmp, pTable->pRecord + pTable->pFieldOffset[ uiIndex ], pField->uiLen );
         szTmp[ pField->uiLen ] = '\0';
         if( pField->uiDec )
         {
            long double ldVal;

#ifdef __BORLANDC__
            ldVal = _strtold( szTmp, NULL );
#elif defined( __WATCOMC__ )
            ldVal = ( long double ) strtod( szTmp, NULL );
#else
            ldVal = strtold( szTmp, NULL );
#endif
            hb_itemPutNDLen( pItem, ( double ) ldVal,
                             ( int ) ( pField->uiLen - pField->uiDec - 1 ),
                             ( int ) pField->uiDec );
         }
         else
         {
            lVal = strtol( szTmp, NULL, 10 );
            hb_itemPutNIntLen( pItem, lVal, ( int ) pField->uiLen );
         }
#endif
         break;
      }

      case HB_FT_DATE:
         if( pField->uiLen == 3 )
            hb_itemPutDL( pItem, HB_GET_LE_UINT24( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ) );
         else if( pField->uiLen == 4 )
            hb_itemPutDL( pItem, HB_GET_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ) );
         else
         {
            char szBuffer[ 9 ];

            memcpy( szBuffer, pTable->pRecord + pTable->pFieldOffset[ uiIndex ], 8 );
            szBuffer[ 8 ] = '\0';
            hb_itemPutDS( pItem, szBuffer );
         }
         break;

      case HB_FT_LOGICAL:
         hb_itemPutL( pItem, pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] ] == 'T' ||
                      pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] ] == 't' ||
                      pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] ] == 'Y' ||
                      pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] ] == 'y' );
         break;

      case HB_FT_MEMO:
      case HB_FT_BLOB:
      case HB_FT_PICTURE:
      case HB_FT_OLE:
      {
         HB_BOOL fEmpty;

         if( pField->uiLen == 4 )
            fEmpty = HB_GET_LE_UINT32( &pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] ] ) == 0;
         else  /* empty if the rightmost char is a whitespace */
            fEmpty = ( pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] + pField->uiLen - 1 ] == ' ' );

         /* uiUpdated is set with append */
         if( ( pTable->uiUpdated & LETO_FLAG_UPD_APPEND ) || pArea->area.fEof || fEmpty )
            hb_itemPutC( pItem, "" );
         else
         {
            if( leto_GetMemoValue( pArea, uiIndex, pItem, pField->uiType ) == HB_FAILURE )
               return HB_FAILURE;
         }
         hb_itemSetCMemo( pItem );
         break;
      }

      case HB_FT_FLOAT:  /* or treat it alike HB_FT_LONG ? */
      {
         char *  pszVal = ( char * ) pTable->pRecord + pTable->pFieldOffset[ uiIndex ];
         double  dVal = hb_strVal( pszVal, pField->uiLen );
         HB_SIZE nLen = ( HB_SIZE ) pField->uiLen;

         while( --nLen && HB_ISDIGIT( pszVal[ nLen ] ) )
            ;
         if( nLen && ( pszVal[ nLen ] == '+' || pszVal[ nLen ] == '-' ) &&
             ( pszVal[ nLen - 1 ] == 'e' || pszVal[ nLen - 1 ] == 'E' ) )
         {
            HB_USHORT uiLen = ( HB_USHORT ) nLen;
            int       iExp = 0;

            while( ++uiLen < pField->uiLen )
               iExp = iExp * 10 + ( pszVal[ uiLen ] - '0' );
            if( pszVal[ nLen ] == '-' )
               iExp = -iExp;
            dVal = hb_numExpConv( dVal, -iExp );
         }
         hb_itemPutNDLen( pItem, dVal,
                          ( int ) ( pField->uiLen - pField->uiDec - 1 ),
                          ( int ) pField->uiDec );
         break;
      }

      case HB_FT_INTEGER:
      case HB_FT_CURRENCY:
      case HB_FT_AUTOINC:
      case HB_FT_ROWVER:
         if( pField->uiDec )
         {
            int    iLen = 0;
            double dVal = 0;

            switch( pField->uiLen )
            {
               case 1:
                  dVal = ( HB_SCHAR ) pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] ];
                  iLen = 4;
                  break;
               case 2:
                  dVal = HB_GET_LE_INT16( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] );
                  iLen = 6;
                  break;
               case 3:
                  dVal = HB_GET_LE_INT24( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] );
                  iLen = 10;
                  break;
               case 4:
                  dVal = HB_GET_LE_INT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] );
                  iLen = 10;
                  break;
               case 8:
                  dVal = ( double ) HB_GET_LE_INT64( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] );
                  iLen = 20;
                  break;
            }
            hb_itemPutNDLen( pItem, hb_numDecConv( dVal, ( int ) pField->uiDec ),
                             iLen, ( int ) pField->uiDec );
         }
         else
         {
            switch( pField->uiLen )
            {
               case 1:
                  hb_itemPutNILen( pItem, ( HB_SCHAR ) pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] ], 4 );
                  break;
               case 2:
                  hb_itemPutNILen( pItem, ( int ) HB_GET_LE_INT16( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ), 6 );
                  break;
               case 3:
                  hb_itemPutNIntLen( pItem, ( HB_MAXINT ) HB_GET_LE_INT24( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ), 10 );
                  break;
               case 4:
                  hb_itemPutNIntLen( pItem, ( HB_MAXINT ) HB_GET_LE_INT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ), 10 );
                  break;
               case 8:
#ifndef HB_LONG_LONG_OFF
                  hb_itemPutNIntLen( pItem, ( HB_MAXINT ) HB_GET_LE_INT64( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ), 20 );
#else
                  hb_itemPutNLen( pItem, ( double ) HB_GET_LE_INT64( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ), 20, 0 );
#endif
                  break;
            }
         }
         break;

      case HB_FT_DOUBLE:
      case HB_FT_CURDOUBLE:
         hb_itemPutNDLen( pItem, HB_GET_LE_DOUBLE( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ),
                          20 - ( pField->uiDec > 0 ? ( pField->uiDec + 1 ) : 0 ),
                          ( int ) pField->uiDec );
         break;

      case HB_FT_TIME:
         if( pField->uiLen == 4 )
         {
#ifdef __XHARBOUR__
            hb_itemPutDTL( pItem, 0, HB_GET_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ) );
#else
            hb_itemPutTDT( pItem, 0, HB_GET_LE_INT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ) );
#endif
            break;
         }
         /* no break */
      case HB_FT_MODTIME:
      case HB_FT_TIMESTAMP:
#ifdef __XHARBOUR__
         hb_itemPutDTL( pItem,
                        HB_GET_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ),
                        HB_GET_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] + 4 ) );
#else
         hb_itemPutTDT( pItem,
                        HB_GET_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ),
                        HB_GET_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] + 4 ) );
#endif
         break;

      case HB_FT_ANY:
      {
         char * pData = ( char * ) pTable->pRecord + pTable->pFieldOffset[ uiIndex ];

         if( pField->uiLen == 3 )
            hb_itemPutDL( pItem, HB_GET_LE_UINT24( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ) );
         else if( pField->uiLen == 4 )
            hb_itemPutNL( pItem, HB_GET_LE_UINT32( pData ) );
         else
         {
            switch( *pData++ )
            {
               case 'D':
               {
                  char szBuffer[ 9 ];
                  memcpy( szBuffer, pData, 8 );
                  szBuffer[ 8 ] = 0;
                  hb_itemPutDS( pItem, szBuffer );
                  break;
               }
               case 'L':
                  hb_itemPutL( pItem, ( *pData == 'T' ) );
                  break;
               case 'N':
                  /* to do */
                  break;
               case 'C':
               {
                  HB_USHORT uiLen = ( HB_USHORT ) leto_b2n( pData, 2 );

                  hb_itemPutCL( pItem, ( pData + 2 ), uiLen );
                  break;
               }
               case '!':
                  /* memo not buffered */
                  leto_GetMemoValue( pArea, uiIndex, pItem, HB_FT_MEMO );
                  break;
               default:
                  hb_itemPutCL( pItem, "", 0 );
            }
         }
         break;
      }
   }
   return HB_SUCCESS;
}

#define letoGetVarLen  NULL
#define letoGoCold     NULL
#define letoGoCold     NULL
#define letoGoHot      NULL

static HB_ERRCODE letoPutRec( LETOAREAP pArea, HB_BYTE * pBuffer )
{
   LETOTABLE * pTable = pArea->pTable;

   if( pTable->fShared && ! pTable->fFLocked && ! pTable->fRecLocked )
   {
      commonError( pArea, EG_UNLOCKED, EDBF_UNLOCKED, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }
   else if( pTable->fReadonly )
   {
      commonError( pArea, EG_READONLY, EDBF_READONLY, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }
   if( pBuffer != NULL )
   {
      if( pArea->lpdbPendingRel )
      {
         if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
            return HB_FAILURE;
      }
      memcpy( pTable->pRecord, pBuffer, pTable->uiRecordLen );
      pTable->uiUpdated |= LETO_FLAG_UPD_CHANGE;
      pTable->fDeleted = ( pTable->pRecord[ 0 ] == '*' );
      memset( pTable->pFieldUpd, 1, pArea->area.uiFieldCount * sizeof( HB_UCHAR ) );
   }
   return HB_SUCCESS;
}

static HB_ERRCODE leto_PutMemoValue( LETOAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem, HB_USHORT uiType )
{
   const char * ptr = hb_itemGetCPtr( pItem );
   char *       pBuff = NULL;
   unsigned int uiRes;
#if defined ( __XHARBOUR__ )
   HB_ULONG     ulLenMemo = hb_itemGetCLen( pItem );
#else
   HB_SIZE      ulLenMemo = hb_itemGetCLen( pItem );
#endif

   if( uiType == HB_FT_MEMO && pArea->area.cdPage != HB_CDP_PAGE() )
   {
#if defined ( __XHARBOUR__ )
      pBuff = ( char * ) hb_xgrab( ulLenMemo + 1 );
      memcpy( pBuff, ptr, ulLenMemo );
      *pBuff[ ulLebMemo ] = '\0';
      hb_cdpnTranslate( pBuff, HB_CDP_PAGE(), pArea->area.cdPage, ulLenMemo );
#else
      pBuff = hb_cdpnDup( ptr, &ulLenMemo, HB_CDP_PAGE(), pArea->area.cdPage );
#endif
      ptr = pBuff;
   }
   uiRes = LetoDbPutMemo( pArea->pTable, uiIndex, ptr, ulLenMemo );
   if( pBuff )
      hb_xfree( pBuff );
   return uiRes == 0 ? HB_SUCCESS : HB_FAILURE;
}

static HB_ERRCODE letoPutValue( LETOAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   LETOTABLE * pTable = pArea->pTable;
   LPFIELD     pField;
   char        szBuf[ 256 ];
   HB_BOOL     fTypeError = HB_FALSE;

#if ! defined ( __XHARBOUR__ )
   char * pBuff;
#endif

   HB_TRACE( HB_TR_DEBUG, ( "letoPutValue(%p, %hu, %p)", pArea, uiIndex, pItem ) );

   if( ! uiIndex || uiIndex > pArea->area.uiFieldCount )
      return HB_FAILURE;

   if( ! pTable->fRecLocked && ! pTable->fFLocked && pTable->fShared )
   {
      commonError( pArea, EG_UNLOCKED, EDBF_UNLOCKED, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }
   else if( pTable->fReadonly )
   {
      commonError( pArea, EG_READONLY, EDBF_READONLY, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }

   /* decrease uiIndex to zero based */
   pField = pArea->area.lpFields + --uiIndex;

   switch( pField->uiType )
   {
      case HB_FT_STRING:
         if( HB_IS_STRING( pItem ) || HB_IS_MEMO( pItem ) )
         {
#if defined ( __XHARBOUR__ )
            HB_USHORT uiSize = ( HB_USHORT ) hb_itemGetCLen( pItem );

            if( uiSize > pField->uiLen )
               uiSize = pField->uiLen;
            memcpy( pTable->pRecord + pTable->pFieldOffset[ uiIndex ], hb_itemGetCPtr( pItem ), uiSize );
            if( pArea->area.cdPage != HB_CDP_PAGE() )
               hb_cdpnTranslate( ( char * ) pTable->pRecord + pTable->pFieldOffset[ uiIndex ], HB_CDP_PAGE(), pArea->area.cdPage, uiSize );
            memset( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] + uiSize, ' ', pField->uiLen - uiSize );
#else
            HB_SIZE ulLen1 = hb_itemGetCLen( pItem );

            if( pArea->area.cdPage != HB_CDP_PAGE() )
            {
               pBuff = hb_cdpnDup( hb_itemGetCPtr( pItem ), &ulLen1, HB_CDP_PAGE(), pArea->area.cdPage );
               if( ulLen1 > ( HB_SIZE ) pField->uiLen )
                  ulLen1 = pField->uiLen;
               memcpy( pTable->pRecord + pTable->pFieldOffset[ uiIndex ], pBuff, ulLen1 );
               hb_xfree( pBuff );
            }
            else
            {
               if( ulLen1 > ( HB_SIZE ) pField->uiLen )
                  ulLen1 = pField->uiLen;
               memcpy( pTable->pRecord + pTable->pFieldOffset[ uiIndex ], hb_itemGetCPtr( pItem ), ulLen1 );
            }
            memset( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] + ulLen1, ' ', pField->uiLen - ulLen1 );
#endif
         }
         else
            fTypeError = HB_TRUE;
         break;

      case HB_FT_LONG:
      case HB_FT_FLOAT:
         if( HB_IS_NUMBER( pItem ) )
         {
            if( hb_itemStrBuf( szBuf, pItem, pField->uiLen, pField->uiDec ) )
            {
               memcpy( pTable->pRecord + pTable->pFieldOffset[ uiIndex ],
                       szBuf, pField->uiLen );
            }
            else
            {
               memset( pTable->pRecord + pTable->pFieldOffset[ uiIndex ], '*', pField->uiLen );
               commonError( pArea, EG_DATAWIDTH, EDBF_DATAWIDTH, 0, NULL, 0, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
               return HB_FAILURE;
            }
         }
         else
            fTypeError = HB_TRUE;
         break;

      case HB_FT_DATE:
         if( HB_IS_DATE( pItem ) )
         {
            if( pField->uiLen == 3 )
            {
               HB_PUT_LE_UINT24( pTable->pRecord + pTable->pFieldOffset[ uiIndex ],
                                 hb_itemGetDL( pItem ) );
            }
            else if( pField->uiLen == 4 )
            {
               HB_PUT_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ],
                                 hb_itemGetDL( pItem ) );
            }
            else
            {
               hb_itemGetDS( pItem, szBuf );
               memcpy( pTable->pRecord + pTable->pFieldOffset[ uiIndex ], szBuf, 8 );
            }
         }
         else
            fTypeError = HB_TRUE;
         break;

      case HB_FT_LOGICAL:
         if( HB_IS_LOGICAL( pItem ) )
         {
            pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] ] = hb_itemGetL( pItem ) ? 'T' : 'F';
         }
         else
            fTypeError = HB_TRUE;
         break;

      case HB_FT_MEMO:
      case HB_FT_BLOB:
      case HB_FT_PICTURE:
      case HB_FT_OLE:
         if( HB_IS_MEMO( pItem ) || HB_IS_STRING( pItem ) )
         {
            HB_BOOL fEmpty;

            if( pField->uiLen == 4 )
               fEmpty = ( HB_GET_LE_UINT32( &pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] ] ) == 0 );
            else
               fEmpty = ( pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] + pField->uiLen - 1 ] == ' ' );

            if( fEmpty && hb_itemGetCLen( pItem ) == 0 )
               return HB_SUCCESS;
            else
               return leto_PutMemoValue( pArea, uiIndex, pItem, pField->uiType );
         }
         else
            fTypeError = HB_TRUE;
         break;

      case HB_FT_INTEGER:
      case HB_FT_AUTOINC:
         if( HB_IS_NUMBER( pItem ) )
         {
#ifndef HB_LONG_LONG_OFF
            HB_LONGLONG lVal;
#else
            HB_LONG lVal;
#endif
            double  dVal;
            int     iSize;

            if( pField->uiDec )
            {
               dVal = hb_numDecConv( hb_itemGetND( pItem ), -( int ) pField->uiDec );
#ifndef HB_LONG_LONG_OFF
               lVal = ( HB_LONGLONG ) dVal;
#else
               lVal = ( HB_LONG ) dVal;
#endif
               if( ! HB_DBL_LIM_INT64( dVal ) )
                  iSize = 99;
               else
#ifndef HB_LONG_LONG_OFF
                  iSize = HB_LIM_INT8( lVal ) ? 1 :
                          ( HB_LIM_INT16( lVal ) ? 2 :
                            ( HB_LIM_INT24( lVal ) ? 3 :
                              ( HB_LIM_INT32( lVal ) ? 4 : 8 ) ) );
#else
                  iSize = HB_DBL_LIM_INT8( dVal ) ? 1 :
                          ( HB_DBL_LIM_INT16( dVal ) ? 2 :
                            ( HB_DBL_LIM_INT24( dVal ) ? 3 :
                              ( HB_DBL_LIM_INT32( dVal ) ? 4 : 8 ) ) );
#endif
            }
            else if( HB_IS_DOUBLE( pItem ) )
            {
               dVal = hb_itemGetND( pItem );
#ifndef HB_LONG_LONG_OFF
               lVal = ( HB_LONGLONG ) dVal;
#else
               lVal = ( HB_LONG ) dVal;
#endif
               if( ! HB_DBL_LIM_INT64( dVal ) )
                  iSize = 99;
               else
#ifndef HB_LONG_LONG_OFF
                  iSize = HB_LIM_INT8( lVal ) ? 1 :
                          ( HB_LIM_INT16( lVal ) ? 2 :
                            ( HB_LIM_INT24( lVal ) ? 3 :
                              ( HB_LIM_INT32( lVal ) ? 4 : 8 ) ) );
#else
                  iSize = HB_DBL_LIM_INT8( dVal ) ? 1 :
                          ( HB_DBL_LIM_INT16( dVal ) ? 2 :
                            ( HB_DBL_LIM_INT24( dVal ) ? 3 :
                              ( HB_DBL_LIM_INT32( dVal ) ? 4 : 8 ) ) );
#endif
            }
            else
            {
#ifndef HB_LONG_LONG_OFF
               lVal = ( HB_LONGLONG ) hb_itemGetNInt( pItem );
#else
               lVal = ( HB_LONG ) hb_itemGetNInt( pItem );
#endif
#ifdef HB_LONG_LONG_OFF
               dVal = ( double ) lVal;
#endif
               iSize = HB_LIM_INT8( lVal ) ? 1 :
                       ( HB_LIM_INT16( lVal ) ? 2 :
                         ( HB_LIM_INT24( lVal ) ? 3 :
                           ( HB_LIM_INT32( lVal ) ? 4 : 8 ) ) );
            }

            if( iSize > pField->uiLen )
            {
               memset( pTable->pRecord + pTable->pFieldOffset[ uiIndex ],
                       '*', pField->uiLen );
               commonError( pArea, EG_DATAWIDTH, EDBF_DATAWIDTH, 0, NULL, 0, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
               return HB_FAILURE;
            }
            else
            {
               switch( pField->uiLen )
               {
                  case 1:
                     pTable->pRecord[ pTable->pFieldOffset[ uiIndex ] ] = ( HB_UCHAR ) lVal;
                     break;
                  case 2:
                     HB_PUT_LE_UINT16( pTable->pRecord + pTable->pFieldOffset[ uiIndex ], ( HB_U16 ) lVal );
                     break;
                  case 3:
                     HB_PUT_LE_UINT24( pTable->pRecord + pTable->pFieldOffset[ uiIndex ], ( HB_U32 ) lVal );
                     break;
                  case 4:
                     HB_PUT_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ], ( HB_U32 ) lVal );
                     break;
                  case 8:
#ifndef HB_LONG_LONG_OFF
                     HB_PUT_LE_UINT64( pTable->pRecord + pTable->pFieldOffset[ uiIndex ], ( HB_U64 ) lVal );
#else
                     HB_PUT_LE_UINT64( pTable->pRecord + pTable->pFieldOffset[ uiIndex ], dVal );
#endif
                     break;
                  default:
                     fTypeError = HB_TRUE;
                     break;
               }
            }
         }
         else
            fTypeError = HB_TRUE;
         break;

      case HB_FT_DOUBLE:
      case HB_FT_CURDOUBLE:
         if( HB_IS_NUMBER( pItem ) )
         {
            HB_PUT_LE_DOUBLE( pTable->pRecord + pTable->pFieldOffset[ uiIndex ],
                              hb_itemGetND( pItem ) );
         }
         else
            fTypeError = HB_TRUE;
         break;

      case HB_FT_TIME:
      {
         HB_BYTE * ptr = pTable->pRecord + pTable->pFieldOffset[ uiIndex ];

         if( pField->uiLen == 4 )
         {
#ifdef __XHARBOUR__
            if( HB_IS_DATE( pItem ) )
               HB_PUT_LE_UINT32( ptr, hb_itemGetT( pItem ) );
#else
            if( HB_IS_DATETIME( pItem ) )
            {
               long lDate, lTime;

               hb_itemGetTDT( pItem, &lDate, &lTime );
               HB_PUT_LE_UINT32( ptr, lTime );
            }
            else if( HB_IS_STRING( pItem ) )
            {
               HB_LONG lJulian, lMillisec;

               hb_timeStampStrGetDT( hb_itemGetCPtr( pItem ), &lJulian, &lMillisec );
               HB_PUT_LE_UINT32( ptr, lMillisec );
            }
#endif
            else
               fTypeError = HB_TRUE;
            break;
         }
         /* no break */
      }
      case HB_FT_TIMESTAMP:
      case HB_FT_MODTIME:
      {
         HB_BYTE * ptr = pTable->pRecord + pTable->pFieldOffset[ uiIndex ];

#ifdef __XHARBOUR__
         if( HB_IS_DATE( pItem ) )
         {
            HB_PUT_LE_UINT32( ptr, hb_itemGetDL( pItem ) );
            HB_PUT_LE_UINT32( ptr + 4, hb_itemGetT( pItem ) );
         }
#else
         if( HB_IS_DATETIME( pItem ) )
         {
            long lDate, lTime;

            hb_itemGetTDT( pItem, &lDate, &lTime );
            HB_PUT_LE_UINT32( ptr, lDate );
            HB_PUT_LE_UINT32( ptr + 4, lTime );
         }
         else if( HB_IS_STRING( pItem ) )
         {
            HB_LONG lJulian, lMillisec;

            hb_timeStampStrGetDT( hb_itemGetCPtr( pItem ), &lJulian, &lMillisec );
            HB_PUT_LE_UINT32( ptr, lJulian );
            HB_PUT_LE_UINT32( ptr + 4, lMillisec );
         }
#endif
         else
            fTypeError = HB_TRUE;
         break;
      }

      case HB_FT_ANY:
      {
         char * pData = ( char * ) pTable->pRecord + pTable->pFieldOffset[ uiIndex ];

         if( pField->uiLen == 3 )
         {
            if( HB_IS_DATE( pItem ) )
               HB_PUT_LE_UINT24( pData, hb_itemGetDL( pItem ) );
            else
               fTypeError = HB_TRUE;
         }
         else if( pField->uiLen == 4 )
         {
            if( HB_IS_NUMBER( pItem ) )
               HB_PUT_LE_UINT32( pData, hb_itemGetNL( pItem ) );
            else
               fTypeError = HB_TRUE;
         }
         else if( HB_IS_DATE( pItem ) )
         {
            hb_itemGetDS( pItem, szBuf );
            *pData++ = 'D';
            memcpy( pData, szBuf, 8 );
         }
         else if( HB_IS_LOGICAL( pItem ) )
         {
            *pData++ = 'L';
            *pData = hb_itemGetL( pItem ) ? 'T' : 'F';
         }
         else if( HB_IS_NUMBER( pItem ) )
         {
            *pData = 'N';
         }
         else if( HB_IS_STRING( pItem ) )
         {
            HB_ULONG ulLen = hb_itemGetCLen( pItem );

            if( ulLen <= ( HB_ULONG ) pField->uiLen - 3 )
            {
               *pData++ = 'C';
               *pData++ = ( HB_UCHAR ) ulLen & 0xFF;
               *pData++ = ( HB_UCHAR ) ( ulLen >> 8 ) & 0xFF;
               if( pArea->area.cdPage != HB_CDP_PAGE() )
               {
#if defined ( __XHARBOUR__ )
                  memcpy( pData, hb_itemGetCPtr( pItem ), ulLen );
                  hb_cdpnTranslate( pData, HB_CDP_PAGE(), pArea->area.cdPage, ulLen );
#else
                  HB_SIZE ulLen1 = ulLen;
                  pBuff = hb_cdpnDup( hb_itemGetCPtr( pItem ), &ulLen1, HB_CDP_PAGE(), pArea->area.cdPage );
                  memcpy( pData, pBuff, ulLen1 );
                  hb_xfree( pBuff );
#endif
               }
               else
                  memcpy( pData, hb_itemGetCPtr( pItem ), ulLen );
            }
            else
            {
               /* memo not buffered */
               *pData = '!';
               return leto_PutMemoValue( pArea, uiIndex, pItem, HB_FT_MEMO );
            }
         }
         break;
      }
   }

   if( fTypeError )
   {
      commonError( pArea, EG_DATATYPE, 1020, 0, NULL, 0, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
      return HB_FAILURE;
   }

   pTable->uiUpdated |= LETO_FLAG_UPD_CHANGE;
   *( pTable->pFieldUpd + uiIndex ) = 1;

   return HB_SUCCESS;
}

static HB_ERRCODE letoRecall( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoRecall(%p)", pArea ) );

   if( pTable->fShared && ! pTable->fFLocked && ! pTable->fRecLocked )
   {
      commonError( pArea, EG_UNLOCKED, EDBF_UNLOCKED, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }
   else if( pTable->fReadonly )
   {
      commonError( pArea, EG_READONLY, EDBF_READONLY, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }

   if( ( pTable->uiUpdated & LETO_FLAG_UPD_DELETE ) || pTable->fDeleted )
   {
      pTable->fDeleted = HB_FALSE;
      pTable->uiUpdated |= LETO_FLAG_UPD_DELETE;
      pTable->pRecord[ 0 ] = ' ';
   }
   return HB_SUCCESS;
}

static HB_ERRCODE letoRecCount( LETOAREAP pArea, HB_ULONG * pRecCount )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoRecCount(%p, %p)", pArea, pRecCount ) );

   if( LetoDbRecCount( pArea->pTable, ( unsigned long * ) pRecCount ) )
      return HB_FAILURE;
   else
      return HB_SUCCESS;
}

/* dbRecordInfo() */
static HB_ERRCODE letoRecInfo( LETOAREAP pArea, PHB_ITEM pRecID, HB_USHORT uiInfoType, PHB_ITEM pInfo )
{
   LETOTABLE * pTable = pArea->pTable;
   HB_ULONG    ulRecNo = hb_itemGetNL( pRecID ), ulPrevRec = 0;
   HB_ERRCODE  errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "letoRecInfo(%p, %p, %hu, %p)", pArea, pRecID, uiInfoType, pInfo ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   if( ulRecNo == 0 )
      ulRecNo = pTable->ulRecNo;
   else if( ulRecNo != pTable->ulRecNo )
   {
      switch( uiInfoType )
      {
         case DBRI_DELETED:
         case DBRI_RAWRECORD:
         case DBRI_RAWMEMOS:
         case DBRI_RAWDATA:
            ulPrevRec = pTable->ulRecNo;
            errCode = SELF_GOTO( ( AREAP ) pArea, ulRecNo );
            if( errCode != HB_SUCCESS )
               return errCode;
            break;
      }
   }

   switch( uiInfoType )
   {

      case DBRI_DELETED:
         hb_itemPutL( pInfo, pTable->fDeleted );
         break;
      case DBRI_LOCKED:
      {
         unsigned int uiRes;

         if( LetoDbIsRecLocked( pTable, ulRecNo, &uiRes ) )
         {
            if( LetoGetError() == 1021 )
               commonError( pArea, EG_DATATYPE, 1021, 0, leto_getRcvBuff(), 0, NULL );
            return HB_FAILURE;
         }
         else
            hb_itemPutL( pInfo, uiRes );
         break;
      }
      case DBRI_RECSIZE:
         hb_itemPutNL( pInfo, pTable->uiRecordLen );
         break;

      case DBRI_RECNO:
         if( ulRecNo == 0 )
            errCode = SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
         hb_itemPutNL( pInfo, ulRecNo );
         break;

      case DBRI_UPDATED:
         hb_itemPutL( pInfo, ulRecNo == pTable->ulRecNo && pTable->uiUpdated );
         break;

#if 0
      case DBRI_ENCRYPTED:
         if( pArea->fValidBuffer ) // && ! hb_dbfReadRecord( pArea ) )
            hb_itemPutL( pInfo, pArea->fEncrypted );
         break;
#endif

      case DBRI_RAWRECORD:
         hb_itemPutCL( pInfo, ( char * ) pTable->pRecord, pTable->uiRecordLen );
         break;

      case DBRI_RAWMEMOS:
      case DBRI_RAWDATA:
      {
         HB_USHORT uiFields;
         HB_BYTE * pResult;
         HB_ULONG  ulLength, ulLen;

         ulLength = uiInfoType == DBRI_RAWDATA ? pTable->uiRecordLen : 0;
         pResult = ( HB_BYTE * ) hb_xgrab( ulLength + 1 );
         if( ulLength )
            memcpy( pResult, pTable->pRecord, ulLength );

         if( pTable->uiMemoVersion )
         {
            for( uiFields = 0; uiFields < pArea->area.uiFieldCount; uiFields++ )
            {
               if( pArea->area.lpFields[ uiFields ].uiType == HB_FT_MEMO ||
                   pArea->area.lpFields[ uiFields ].uiType == HB_FT_PICTURE ||
                   pArea->area.lpFields[ uiFields ].uiType == HB_FT_BLOB ||
                   pArea->area.lpFields[ uiFields ].uiType == HB_FT_OLE )
               {
                  errCode = SELF_GETVALUE( ( AREAP ) pArea, uiFields + 1, pInfo );
                  if( errCode != HB_SUCCESS )
                     break;
                  ulLen = hb_itemGetCLen( pInfo );
                  if( ulLen > 0 )
                  {
                     pResult = ( HB_BYTE * ) hb_xrealloc( pResult, ulLength + ulLen + 1 );
                     memcpy( pResult + ulLength, hb_itemGetCPtr( pInfo ), ulLen );
                     ulLength += ulLen;
                  }
               }
            }
         }
         hb_itemPutCLPtr( pInfo, ( char * ) pResult, ulLength );
         break;
      }

      default:
         errCode = SUPER_RECINFO( ( AREAP ) pArea, pRecID, uiInfoType, pInfo );
   }

   if( ulPrevRec != 0 )
   {
      if( SELF_GOTO( ( AREAP ) pArea, ulPrevRec ) != HB_SUCCESS &&
          errCode == HB_SUCCESS )
         errCode = HB_FAILURE;
   }

   return errCode;
}

static HB_ERRCODE letoRecNo( LETOAREAP pArea, HB_ULONG * ulRecNo )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoRecNo(%p, %p)", pArea, ulRecNo ) );

   if( pTable->uiUpdated & LETO_FLAG_UPD_APPEND )
      leto_PutRec( pArea, HB_FALSE );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   *ulRecNo = pTable->ulRecNo;

   return HB_SUCCESS;
}

static HB_ERRCODE letoRecId( LETOAREAP pArea, PHB_ITEM pRecNo )
{
   HB_ERRCODE errCode;
   HB_ULONG   ulRecNo;

   HB_TRACE( HB_TR_DEBUG, ( "letoRecId(%p, %p)", pArea, pRecNo ) );

   errCode = SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
   hb_itemPutNL( pRecNo, ulRecNo );

   return errCode;
}

#define letoSetFieldExtent  NULL
#define letoAlias           NULL

static void leto_FreeTag( LETOTAGINFO * pTagInfo )
{
   if( pTagInfo )
   {
      if( pTagInfo->pExtra )
      {
         LETOTAGEXTRAINFO * pExtra = ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra;

         if( pExtra->pKeyItem != NULL )
            hb_vmDestroyBlockOrMacro( pExtra->pKeyItem );
         if( pExtra->pTopScope )
            hb_itemRelease( pExtra->pTopScope );
         if( pExtra->pBottomScope )
            hb_itemRelease( pExtra->pBottomScope );
         if( pExtra->puiFields )
            hb_xfree( pExtra->puiFields );
         hb_xfree( pTagInfo->pExtra );
      }
      LetoDbFreeTag( pTagInfo );
   }
}

static HB_ERRCODE letoClose( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoClose(%p)", pArea ) );

   if( pTable && pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   pArea->lpdbPendingRel = NULL;
   if( pTable )
      SUPER_CLOSE( ( AREAP ) pArea );

   if( pTable && pTable->pTagInfo )
   {
      LETOTAGINFO * pTagInfo = pTable->pTagInfo;

      while( pTagInfo )
      {
         LETOTAGINFO * pTagNext = pTagInfo->pNext;

         leto_FreeTag( pTagInfo );
         pTagInfo = pTagNext;
      }

      pTable->pTagInfo = NULL;
   }

   if( pTable )
   {
      if( LetoDbCloseTable( pTable ) )
         commonError( pArea, EG_SYNTAX, LetoGetError(), 0, NULL, 0, NULL );
      pArea->pTable = NULL;
   }

   if( pArea->szDataFileName )
   {
      hb_xfree( pArea->szDataFileName );
      pArea->szDataFileName = NULL;
   }

   return HB_SUCCESS;
}

static LETOCONNECTION * leto_OpenConnection( LETOAREAP pArea, LPDBOPENINFO pOpenInfo, char * szFile, HB_BOOL fCreate )
{
   LETOCONNECTION * pConnection;
   char *       ptr;
   int          iPort = 0;
   unsigned int uiLen;

   HB_TRACE( HB_TR_DEBUG, ( "()" ) );

   if( pOpenInfo->ulConnection > 0 && pOpenInfo->ulConnection <= ( HB_ULONG ) uiGetConnCount() )
   {
      pConnection = letoGetConnPool( ( HB_USHORT ) pOpenInfo->ulConnection - 1 );

      if( pConnection && pConnection->pAddr )
      {
         const char * szPath = leto_RemoveIpFromPath( pOpenInfo->abName );

         if( ( ptr = strrchr( szPath, '/' ) ) != NULL ||
             ( ptr = strrchr( szPath, '\\' ) ) != NULL )
         {
            uiLen = HB_MIN( ptr - szPath, HB_PATH_MAX - 1 );
            memcpy( szFile, szPath, uiLen );
            szFile[ uiLen ] = '\0';
         }
         else if( pConnection->szPath )
            hb_strncpy( szFile, pConnection->szPath, HB_PATH_MAX );
         else
            hb_strncpy( szFile, ( fCreate ? hb_setGetDefault() : hb_setGetPath() ), HB_PATH_MAX );

         uiLen = strlen( szFile );
         if( ! uiLen )
            uiLen = 1;
         if( szFile[ uiLen - 1 ] != '/' && szFile[ uiLen - 1 ] != '\\' )
         {
            szFile[ uiLen - 1 ] = HB_OS_PATH_DELIM_CHR;
            szFile[ uiLen ] = '\0';
         }
      }
      else
         pConnection = NULL;
   }
   else
   {
      char szAddr[ 96 ];

      szAddr[ 0 ] = '\0';
      if( ! leto_getIpFromPath( pOpenInfo->abName, szAddr, &iPort, szFile, HB_TRUE ) &&
          ! leto_getIpFromPath( ( fCreate ? hb_setGetDefault() : hb_setGetPath() ), szAddr, &iPort, szFile, HB_FALSE ) )
      {
         LETOCONNECTION * pCurrentConn = letoGetCurrConn();

         if( pCurrentConn )
         {
            ptr = strrchr( pOpenInfo->abName, '/' );
            if( ptr == NULL )
               ptr = strrchr( pOpenInfo->abName, '\\' );
            if( ptr )
            {
               char * ptrdouble;

               uiLen = HB_MIN( ptr - pOpenInfo->abName + 1, HB_PATH_MAX - 1);
               memcpy( szFile, pOpenInfo->abName, uiLen );
               szFile[ uiLen ] = '\0';
               /* beautify doubled path separator */
               while( ( ptrdouble = strstr( szFile, "//" ) ) != NULL ||
                      ( ptrdouble = strstr( szFile, "\\\\" ) ) != NULL )
               {
                  memmove( ptrdouble, ptrdouble + 1, strlen( ptrdouble + 1 ) + 1 );
               }
            }
            else if( pCurrentConn->szPath )
            {
               ptr = strchr( pCurrentConn->szPath, ',' );
               if( ptr == NULL )
                  ptr = strchr( pCurrentConn->szPath, ';' );
               if( ptr == NULL )
                  ptr = pCurrentConn->szPath + strlen( pCurrentConn->szPath );
               uiLen = HB_MIN( ptr - pCurrentConn->szPath, HB_PATH_MAX - 1 );
               memcpy( szFile, pCurrentConn->szPath, uiLen );
               if( szFile[ uiLen ] != '/' && szFile[ uiLen ] != '\\' )
               {
                  szFile[ uiLen ] = HB_OS_PATH_DELIM_CHR;
                  szFile[ uiLen + 1 ] = '\0';
               }
            }
            else
            {
               szFile[ 0 ] = HB_OS_PATH_DELIM_CHR;
               szFile[ 1 ] = '\0';
            }
            return pCurrentConn;
         }
         else
         {
            commonError( pArea, EG_OPEN, ( fCreate ? 1 : 101 ), 0, pOpenInfo->abName, 0, NULL );
            return NULL;
         }
      }

      if( ( pConnection = leto_ConnectionFind( szAddr, iPort ) ) == NULL &&
          ( pConnection = LetoConnectionNew( szAddr, iPort, NULL, NULL, 0, HB_FALSE ) ) == NULL )
      {
         commonError( pArea, EG_OPEN, ( fCreate ? 1 : 102 ), 0, pOpenInfo->abName, 0, NULL );
         return NULL;
      }
   }

   return pConnection;
}

static HB_ERRCODE letoCreate( LETOAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   LETOTABLE *      pTable;
   LETOCONNECTION * pConnection;
   LPFIELD          pField;
   HB_USHORT        uiCount = 0, uiPathLen;
   char *           szData, * ptr;
   char             szFile[ HB_PATH_MAX ];
   char             szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   char             cType;
   const char *     szFieldName;
   HB_ERRCODE       errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "letoCreate(%p, %p)", pArea, pCreateInfo ) );

   szFile[ 0 ] = '\0';
   if( ( pConnection = leto_OpenConnection( pArea, pCreateInfo, szFile, HB_TRUE ) ) == NULL )
      return HB_FAILURE;
   uiPathLen = ( HB_USHORT ) strlen( szFile );
   /* add filename to path from above */
   leto_getFileFromPath( pCreateInfo->abName, szFile + uiPathLen, HB_PATH_MAX - uiPathLen );
   ptr = pArea->szDataFileName = hb_strdup( pCreateInfo->abName );
   if( strlen( ptr ) >= 2 && *( ptr + 1 ) == ':' )  /* C:\... */
      ptr += 2;
   while( *ptr == '/' || *ptr == '\\' )
   {
      ptr++;
   }
   if( ptr > pArea->szDataFileName )
      memmove( pArea->szDataFileName, ptr, strlen( ptr ) + 1 );
   while( ( ptr = strstr( szFile, "//" ) ) != NULL ||
          ( ptr = strstr( szFile, "\\\\" ) ) != NULL )
   {
      memmove( ptr, ptr + 1, strlen( ptr + 1 ) + 1 );
   }

   /* ( 10 + ';' + 7 [ C:attribute ] + ';' + 5 + ';' + 3 + ';' ) */
   szData = ( char * ) hb_xgrab( ( ( unsigned int ) pArea->area.uiFieldCount * 29 ) + 10 );
   ptr = szData;
   pField = pArea->area.lpFields;
   while( uiCount++ < pArea->area.uiFieldCount && errCode == HB_SUCCESS )
   {
      szFieldName = hb_dynsymName( ( PHB_DYNS ) pField->sym );
      if( ! szFieldName || szFieldName[ 0 ] == '\0' )
         errCode = HB_FAILURE;

      switch( pField->uiType )
      {
         case HB_FT_STRING:
            cType = 'C';
            if( pField->uiDec )
            {
               pField->uiLen += pField->uiDec * 256;
               pField->uiDec = 0;
            }
            break;
         case HB_FT_LONG:
            cType = 'N';
            break;
         case HB_FT_LOGICAL:
            cType = 'L';
            break;
         case HB_FT_DATE:
            cType = 'D';
            break;
         case HB_FT_MEMO:
            cType = 'M';
            break;
         case HB_FT_BLOB:
            cType = 'W';
            break;
         case HB_FT_PICTURE:
            cType = 'P';
            break;
         case HB_FT_OLE:
            cType = 'G';
            break;
         case HB_FT_FLOAT:
            cType = 'F';
            break;
         case HB_FT_INTEGER:
            if( pField->uiLen == 2 )
               cType = '2';
            else
               cType = 'I';
            break;
         case HB_FT_DOUBLE:
            cType = '8';
            break;
         case HB_FT_TIMESTAMP:
            cType = '@';
            break;
         case HB_FT_MODTIME:
            cType = '=';
            break;
         case HB_FT_TIME:
            cType = 'T';
            break;
         case HB_FT_ANY:
            cType = 'V';
            break;
         case HB_FT_ROWVER:
            cType = '^';
            break;
         case HB_FT_AUTOINC:
            cType = '+';
            break;
         case HB_FT_CURRENCY:
            cType = 'Y';
            break;
         case HB_FT_CURDOUBLE:
            cType = 'Z';
            break;
         case HB_FT_VARLENGTH:
            cType = 'Q';
            break;
         default:
            // commonError( pArea, EG_DATATYPE, 1000, 0, NULL, 0, NULL );
            hb_xfree( szData );
            return HB_FAILURE;
      }

      if( pField->uiFlags )
      {
         char szFlags[ 8 ];
         int  i = 0;

         szFlags[ i++ ] = ':';
#if defined( __HARBOUR__ ) && ! defined( __HARBOUR30__ )
         if( pField->uiFlags & HB_FF_AUTOINC )
            szFlags[ i++ ] = '+';
#endif
         if( pField->uiFlags & HB_FF_BINARY )
            szFlags[ i++ ] = 'B';
         if( pField->uiFlags & HB_FF_ENCRYPTED )
            szFlags[ i++ ] = 'E';
         if( pField->uiFlags & HB_FF_NULLABLE )
            szFlags[ i++ ] = 'N';
         if( pField->uiFlags & HB_FF_UNICODE )
            szFlags[ i++ ] = 'U';
         if( pField->uiFlags & HB_FF_COMPRESSED )
            szFlags[ i++ ] = 'Z';
         szFlags[ i ] = '\0';

         ptr += eprintf( ptr, "%s;%c%s;%d;%d;", szFieldName, cType, ( *szFlags ? szFlags : "" ),
                         pField->uiLen, pField->uiDec );
      }
      else
         ptr += eprintf( ptr, "%s;%c;%d;%d;", szFieldName, cType, pField->uiLen, pField->uiDec );

      if( pField->uiDec > 99 )  /* avoid ptr buffer overflow */
         errCode = HB_FAILURE;

      pField++;
   }

   /* search for free Area, as info for server */
   if( pCreateInfo->uiArea == 0 )
   {
      hb_rddSelectWorkAreaNumber( 0 );
      pCreateInfo->uiArea = ( HB_USHORT ) hb_rddGetCurrentWorkAreaNumber();
   }

   if( ! pCreateInfo->atomAlias || ! *pCreateInfo->atomAlias )  /* create a missing Alias */
   {
      char *    ptrBeg, * ptrEnd;
      HB_USHORT uLen;

      ptrEnd = strrchr( szFile, '.' );
      if( ptrEnd == NULL )
         ptrEnd = szFile + strlen( szFile );
      ptrBeg = strchr( szFile + uiPathLen, ':' );
      if( ptrBeg )  /* mem:... */
         ptrBeg++;
      else
         ptrBeg = szFile + uiPathLen;
      uLen = ( HB_USHORT ) ( ptrEnd - ptrBeg );
      if( uLen > HB_RDD_MAX_ALIAS_LEN )
         uLen = HB_RDD_MAX_ALIAS_LEN;
      strncpy( szAlias, ptrBeg, uLen );
      szAlias[ uLen ] = '\0';
      hb_strUpper( szAlias, uLen );
      pCreateInfo->atomAlias = ( const char * ) szAlias;
   }

   if( errCode == HB_SUCCESS )
      pTable = LetoDbCreateTable( pConnection, szFile, pCreateInfo->atomAlias, szData,
                                  pCreateInfo->uiArea, pCreateInfo->cdpId );
   else
      pTable = NULL;
   hb_xfree( szData );

   if( ! pTable )
   {
      if( LetoGetError() == 1023 )  /* NetErr() */
         commonError( pArea, EG_OPEN, 1023, 32, pArea->szDataFileName, EF_CANDEFAULT, NULL );
      else if( LetoGetError() == 1 || LetoGetError() == 1000 )
         commonError( pArea, EG_DATATYPE, LetoGetError(), 0, NULL, 0, NULL );
      else
         leto_CheckError( pArea );
      return HB_FAILURE;
   }

   pArea->pTable = pTable;
   if( pCreateInfo->cdpId )
   {
      pArea->area.cdPage = hb_cdpFind( pCreateInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = HB_CDP_PAGE();
   }
   else
      pArea->area.cdPage = HB_CDP_PAGE();

   return SUPER_CREATE( ( AREAP ) pArea, pCreateInfo );
}

/* ToDo is a valid pArea with e.g. pArea->szDataFileName ensured ? */
static HB_ERRCODE letoInfo( LETOAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   LETOTABLE *      pTable = pArea->pTable;
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

   HB_TRACE( HB_TR_DEBUG, ( "letoInfo(%p, %hu, %p)", pArea, uiIndex, pItem ) );

   switch( uiIndex )
   {
      case DBI_ISDBF:
      case DBI_CANPUTREC:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case DBI_GETHEADERSIZE:
         hb_itemPutNL( pItem, 32 + pArea->area.uiFieldCount * 32 + 2 );
         break;

      case DBI_GETRECSIZE:
         hb_itemPutNL( pItem, pTable->uiRecordLen );
         break;

      case DBI_FCOUNT:
         hb_itemPutNL( pItem, pTable->uiFieldExtent );
         break;

      case DBI_LOCKCOUNT:
      case DBI_GETLOCKARRAY:
      {
         char   szData[ 32 ];
         char * ptr;

         eprintf( szData, "%c;%lu;%d;;", LETOCMD_dbi, pTable->hTable, uiIndex );
         if( ! leto_SendRecv( pConnection, pArea, szData, 0, 1021 ) )
            return HB_FAILURE;

         ptr = leto_firstchar( pConnection );
         if( *( ptr - 1 ) == '+' )
         {
            HB_ULONG ulCount;

            ulCount = strtoul( ptr, &ptr, 10 );
            if( uiIndex == DBI_LOCKCOUNT )
            {
               hb_itemPutNL( pItem, ulCount );
               break;
            }

            hb_arrayNew( pItem, ulCount );
            while( ptr && ulCount )
            {
               hb_arraySetNL( pItem, ulCount, strtoul( ++ptr, &ptr, 10 ) );
               ulCount--;
            }
         }
         break;
      }

      case DBI_TABLEEXT:
      {
         PHB_FNAME pFilePath = hb_fsFNameSplit( pArea->szDataFileName );

         if( ! pFilePath->szExtension || ! *pFilePath->szExtension )
            hb_itemPutC( pItem, ".dbf" );
         else
            hb_itemPutC( pItem, pFilePath->szExtension );
         hb_xfree( pFilePath );
         break;
      }

      case DBI_FULLPATH:
         hb_itemPutCL( pItem, pArea->szDataFileName, strlen( pArea->szDataFileName ) );
         break;

      case DBI_ISFLOCK:
         hb_itemPutL( pItem, pTable->fFLocked );
         break;

      case DBI_ISREADONLY:
         hb_itemPutL( pItem, pTable->fReadonly );
         break;

      case DBI_SHARED:
         hb_itemPutL( pItem, pTable->fShared );
         break;

      case DBI_MEMOEXT:
         hb_itemPutC( pItem, pTable->szMemoExt );
         break;

      case DBI_MEMOTYPE:
         hb_itemPutNL( pItem, pTable->uiMemoType );
         break;

      case DBI_MEMOVERSION:
         hb_itemPutNL( pItem, pTable->uiMemoVersion );
         break;

      case DBI_MEMOBLOCKSIZE:
         hb_itemPutNI( pItem, pTable->uiMemoBlocksize );
         break;

      case DBI_LOCKSCHEME:
         hb_itemPutNI( pItem, pTable->uiLockScheme );
         break;

      case DBI_ISENCRYPTED:
      {
         char   szData[ 32 ];
         char * ptr;

         eprintf( szData, "%c;%lu;%d;;", LETOCMD_dbi, pTable->hTable, uiIndex );
         if( ! leto_SendRecv( pConnection, pArea, szData, 0, 1021 ) )
            return HB_FAILURE;

         ptr = leto_firstchar( pConnection );
         if( *( ptr - 1 ) == '+' )
            pTable->fEncrypted = HB_TRUE;
         else
            pTable->fEncrypted = HB_FALSE;

         hb_itemPutL( pItem, pTable->fEncrypted );
         break;
      }

      case DBI_PASSWORD:
      case DBI_ENCRYPT:  /* hb_dbfTableEncrypt( pArea, pPasswd, HB_TRUE ) */
      case DBI_DECRYPT:  /* hb_dbfTableEncrypt( pArea, pPasswd, HB_FALSE ) */
         if( pConnection && HB_IS_STRING( pItem ) && hb_itemGetCLen( pItem ) > 0 )
         {
            char     szData[ 96 ];
            char *   szPass;
            char *   szBuf;
            char *   szKey = leto_localKey( pConnection->cDopcode, LETO_DOPCODE_LEN );
            char *   ptr;
            HB_ULONG ulLen = ( HB_ULONG ) hb_itemGetCLen( pItem );

            szBuf = ( char * ) hb_xgrab( ulLen + 9 );
            szPass = ( char * ) hb_xgrab( ( ulLen + 9 ) * 2 );
            leto_encrypt( hb_itemGetCPtr( pItem ), ulLen, szBuf, &ulLen, szKey, HB_FALSE );
            if( szKey )
               hb_xfree( szKey );
            leto_byte2hexchar( szBuf, ( int ) ulLen, szPass );
            hb_xfree( szBuf );
            szPass[ ulLen * 2 ] = '\0';

            eprintf( szData, "%c;%lu;%d;%s;", LETOCMD_dbi, pTable->hTable, uiIndex, szPass );
            hb_xfree( szPass );
            if( ! leto_SendRecv( pConnection, pArea, szData, 0, 1021 ) )
               return HB_FAILURE;

            ptr = leto_firstchar( pConnection );
            if( *( ptr - 1 ) == '+' )
            {
               if( uiIndex != DBI_PASSWORD )
                  pTable->fEncrypted = ( uiIndex == DBI_ENCRYPT );
            }
            else
            {
               memset( szPass, ' ', hb_itemGetCLen( pItem ) );
               szPass[ hb_itemGetCLen( pItem ) ] = '\0';
               hb_itemPutC( pItem, szPass );
               return HB_FAILURE;
            }
         }
         break;

      case DBI_DB_VERSION:        /* HOST driver Version */
         hb_itemPutC( pItem, LetoGetServerVer( letoGetConnPool( pTable->uiConnection ) ) );
         break;

      case DBI_RDD_VERSION:       /* RDD version (current RDD) */
         hb_itemPutC( pItem, LETO_VERSION_STRING );
         break;

      case DBI_TRIGGER:
      {
         char         szData[ 512 ];
         HB_ULONG     ulLen;
         const char * ptr, * ptr2;

         if( HB_IS_LOGICAL( pItem ) )
         {
            ulLen = eprintf( szData, "%c;%lu;%d;.%c.;", LETOCMD_dbi, pTable->hTable, uiIndex,
                             ( hb_itemGetL( pItem ) ? 'T' : 'F' ) );
         }
         else if( HB_IS_STRING( pItem ) )
         {
            ulLen = eprintf( szData, "%c;%lu;%d;%s;", LETOCMD_dbi, pTable->hTable, uiIndex,
                             hb_itemGetCPtr( pItem ) );
         }
         else
            ulLen = eprintf( szData, "%c;%lu;%d;", LETOCMD_dbi, pTable->hTable, uiIndex );

         if( ! leto_SendRecv( pConnection, pArea, szData, ulLen, 1021 ) )
            return HB_FAILURE;

         ptr = leto_firstchar( pConnection );
         if( ( ptr2 = LetoFindCmdItem( ptr ) ) != NULL )
            hb_itemPutCL( pItem, ptr, ptr2 - ptr );
         break;
      }

      case DBI_LASTUPDATE:  /* stored as long Julian */
         hb_itemPutDL( pItem, pTable->lLastUpdate );
         break;

      case DBI_BUFREFRESHTIME:
      {
         int iBufRefreshTime = pTable->iBufRefreshTime;

         if( HB_IS_NUMERIC( pItem ) )
            pTable->iBufRefreshTime = hb_itemGetNI( pItem );
         hb_itemPutNI( pItem, iBufRefreshTime );
         break;
      }

      case DBI_CLEARBUFFER:
         pTable->ptrBuf = NULL;
         break;

      default:
         return SUPER_INFO( ( AREAP ) pArea, uiIndex, pItem );
   }

   return HB_SUCCESS;
}

static HB_ERRCODE letoNewArea( LETOAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoNewArea(%p)", pArea ) );

   return SUPER_NEW( ( AREAP ) pArea );
}

static HB_ERRCODE letoOpen( LETOAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   LETOCONNECTION * pConnection;
   HB_USHORT        uiFields, uiCount, uiPathLen;
   DBFIELDINFO      dbFieldInfo;
   LETOTABLE *      pTable;
   LETOFIELD *      pField;
   char             szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   char             szFile[ HB_PATH_MAX ];
   char *           ptrdouble;

   HB_TRACE( HB_TR_DEBUG, ( "letoOpen(%p)", pArea ) );

   if( ! pOpenInfo->atomAlias || ! *pOpenInfo->atomAlias )  /* create a missing Alias */
   {
      char *    ptr;
      HB_USHORT uLen;

      leto_getFileFromPath( pOpenInfo->abName, szFile, HB_PATH_MAX );
      ptr = strrchr( szFile, '.' );
      if( ptr )
         *ptr = '\0';
      ptr = strchr( szFile, ':' );
      if( ptr )  /* mem:... */
         ptr++;
      else
         ptr = szFile;
      uLen = ( HB_USHORT ) strlen( ptr );
      if( uLen > HB_RDD_MAX_ALIAS_LEN )
         uLen = HB_RDD_MAX_ALIAS_LEN;
      strncpy( szAlias, ptr, uLen );
      szAlias[ uLen ] = '\0';
      hb_strUpper( szAlias, uLen );
      pOpenInfo->atomAlias = ( const char * ) szAlias;
   }

   szFile[ 0 ] = '\0';
   if( ( pConnection = leto_OpenConnection( pArea, pOpenInfo, szFile, HB_FALSE ) ) == NULL )
      return HB_FAILURE;
   uiPathLen = ( HB_USHORT ) strlen( szFile );
   leto_getFileFromPath( pOpenInfo->abName, szFile + uiPathLen, HB_PATH_MAX - uiPathLen );
   ptrdouble = pArea->szDataFileName = hb_strdup( pOpenInfo->abName );
   if( strlen( ptrdouble ) >= 2 && *( ptrdouble + 1 ) == ':' )  /* C:\... */
      ptrdouble += 2;
   while( *ptrdouble == '/' || *ptrdouble == '\\' )
   {
      ptrdouble++;
   }
   if( ptrdouble > pArea->szDataFileName )
      memmove( pArea->szDataFileName, ptrdouble, strlen( ptrdouble ) + 1 );
   while( ( ptrdouble = strstr( szFile, "//" ) ) != NULL ||
          ( ptrdouble = strstr( szFile, "\\\\" ) ) != NULL )
   {
      memmove( ptrdouble, ptrdouble + 1, strlen( ptrdouble + 1 ) + 1 );
   }

   /* close already used WA before */
   if( pOpenInfo->uiArea && pArea->pTable )  // ToDo verify as untested
      SELF_CLOSE( ( AREAP ) pArea );

   /* sets pTable->pTagCurrent */
   pTable = LetoDbOpenTable( pConnection, szFile, pOpenInfo->atomAlias,
                             pOpenInfo->fShared, pOpenInfo->fReadonly,
                             ( pOpenInfo->cdpId ) ? pOpenInfo->cdpId : "", pOpenInfo->uiArea );

   if( ! pTable )
   {
      if( LetoGetError() != 103 )  /* no runtime error, only NetErr() ? */
         commonError( pArea, EG_DATATYPE, 1021, 0, pArea->szDataFileName, 0, NULL );  /* EDBF_DATAWIDTH */
      else
         commonError( pArea, EG_OPEN, 103, 32, pArea->szDataFileName, EF_CANDEFAULT, NULL );
      return HB_FAILURE;
   }
   pArea->pTable = pTable;

   if( pOpenInfo->cdpId )
   {
      pArea->area.cdPage = hb_cdpFind( pOpenInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = HB_CDP_PAGE();
   }
   else
      pArea->area.cdPage = HB_CDP_PAGE();

   SELF_SETFIELDEXTENT( ( AREAP ) pArea, pTable->uiFieldExtent );
   uiFields = pTable->uiFieldExtent;
   for( uiCount = 0; uiCount < uiFields; uiCount++ )
   {
      /* memset( &dbFieldInfo, 0, sizeof( dbFieldInfo ) ); */
      pField = pTable->pFields + uiCount;
      dbFieldInfo.atomName = pField->szName;
      dbFieldInfo.uiType = pField->uiType;
      dbFieldInfo.uiTypeExtended = 0;
      dbFieldInfo.uiLen = pField->uiLen;
      dbFieldInfo.uiDec = pField->uiDec;
      dbFieldInfo.uiFlags = pField->uiFlags;
      if( SELF_ADDFIELD( ( AREAP ) pArea, &dbFieldInfo ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   if( SUPER_OPEN( ( AREAP ) pArea, pOpenInfo ) != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      return HB_FAILURE;
   }

   if( pTable->pTagInfo )
   {
      LETOTAGINFO * pTagInfo = pTable->pTagInfo;

      while( pTagInfo )
      {
         leto_CreateKeyExpr( pArea, pTagInfo, NULL );
#if 0  /* temporary outcommented */
         ScanIndexFields( pArea, pTagInfo );
#endif
         pTagInfo = pTagInfo->pNext;
      }
#if 0  /* done by Harbour, send info about */
      if( ! pTable->uiDriver && hb_setGetAutOpen() ) /* don't do that for NTX */
      {
         HB_USHORT uiAutOrder = ( HB_USHORT ) hb_setGetAutOrder();

         if( uiAutOrder )
         {
            pTagInfo = pTable->pTagInfo;
            while( pTagInfo )
            {
               if( pTagInfo->uiTag == uiAutOrder )
               {
                  pTable->pTagCurrent = pTagInfo;
                  break;
               }
               pTagInfo = pTagInfo->pNext;
            }
         }
      }
#endif
   }
   pArea->area.fBof = pTable->fBof;
   pArea->area.fEof = pTable->fEof;
   pArea->area.fFound = pTable->fFound;
   return HB_SUCCESS;
}

#define letoRelease  NULL

static HB_ERRCODE letoStructSize( LETOAREAP pArea, HB_USHORT * StructSize )
{
   HB_SYMBOL_UNUSED( pArea );

   *StructSize = sizeof( LETOAREA );

   return HB_SUCCESS;
}

static HB_ERRCODE letoSysName( LETOAREAP pArea, HB_BYTE * pBuffer )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoSysName(%p, %p)", pArea, pBuffer ) );

   HB_SYMBOL_UNUSED( pArea );
   hb_strncpy( ( char * ) pBuffer, "LETO", HB_RDD_MAX_DRIVERNAME_LEN );
   return HB_SUCCESS;
}

/* static HB_ERRCODE letoEval( LETOAREAP pArea, LPDBEVALINFO pEvalInfo ) */
#define letoEval  NULL  /* dbEval contain CBs */

static HB_ERRCODE letoPack( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoPack(%p)", pArea ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( letoGetConnPool( pTable->uiConnection )->fTransActive )
   {
      commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }
   if( pTable->fReadonly )
   {
      commonError( pArea, EG_READONLY, EDBF_READONLY, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }
   if( pTable->fShared )
   {
      commonError( pArea, EG_SHARED, EDBF_SHARED, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }

   if( LetoDbPack( pTable ) != HB_SUCCESS )
      return HB_FAILURE;

   return SELF_GOTOP( ( AREAP ) pArea );
}

#define letoPackRec  NULL

/* used by LETOCMD_sort + LETOCMD_trans; */
static char * leto_PutTransInfo( LETOAREAP pArea, LETOAREAP pAreaDst, LPDBTRANSINFO pTransInfo, char * pData )
{
   LETOTABLE * pTable = pArea->pTable;
   char *      ptr = pData;
   HB_USHORT   uiIndex;

   ptr += eprintf( pData, "%lu;%lu;%c;%lu;%s;%s;%lu;%lu;%c;%c;%c;%c;%c;%c;%c;%d;%d;",
                   pTable->hTable,
                   pTable->ulRecNo,
                   ( char ) ( ( hb_setGetDeleted() ) ? 'T' : 'F' ),
                   pAreaDst->pTable->hTable,
                   hb_itemGetCPtr( pTransInfo->dbsci.lpstrFor ),
                   hb_itemGetCPtr( pTransInfo->dbsci.lpstrWhile ),
                   hb_itemGetNL( pTransInfo->dbsci.lNext ),
                   hb_itemGetNL( pTransInfo->dbsci.itmRecID ),
                   hb_itemGetL( pTransInfo->dbsci.fRest ) ? 'T' : 'F',
                   pTransInfo->dbsci.fIgnoreFilter ? 'T' : 'F',
                   pTransInfo->dbsci.fIncludeDeleted ? 'T' : 'F',
                   pTransInfo->dbsci.fLast ? 'T' : 'F',
                   pTransInfo->dbsci.fIgnoreDuplicates ? 'T' : 'F',
                   pTransInfo->dbsci.fBackward ? 'T' : 'F',
                   pTransInfo->dbsci.fOptimized ? 'T' : 'F',
                   pTransInfo->uiFlags, pTransInfo->uiItemCount );

   for( uiIndex = 0; uiIndex < pTransInfo->uiItemCount; uiIndex++ )
   {
      ptr += eprintf( ptr, "%d,%d;",
                      pTransInfo->lpTransItems[ uiIndex ].uiSource,
                      pTransInfo->lpTransItems[ uiIndex ].uiDest );
   }

   return ptr;
}

static HB_ERRCODE letoSort( LETOAREAP pArea, LPDBSORTINFO pSortInfo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   LPDBTRANSINFO pTransInfo = &pSortInfo->dbtri;
   LETOAREAP     pAreaDst = ( LETOAREAP ) pTransInfo->lpaDest;
   HB_ERRCODE    errCode;
   char *        pData, * ptr;
   HB_USHORT     uiIndex;
   HB_ULONG      ulLen;

   if( ! leto_CheckArea( pAreaDst ) ||
       ( pArea->pTable->uiConnection != pAreaDst->pTable->uiConnection ) ||
       ( pTransInfo->dbsci.itmCobFor && ! pTransInfo->dbsci.lpstrFor ) ||
       ( pTransInfo->dbsci.itmCobWhile && ! pTransInfo->dbsci.lpstrWhile ) )
   {
      return SUPER_SORT( ( AREAP ) pArea, pSortInfo );
   }

   ulLen = 92 + LETO_MAX_TAGNAME +
           hb_itemGetCLen( pTransInfo->dbsci.lpstrFor ) +
           hb_itemGetCLen( pTransInfo->dbsci.lpstrWhile ) +
           pTransInfo->uiItemCount * 16 + pSortInfo->uiItemCount * 16;

   pData = hb_xgrab( ulLen );

   pData[ 0 ] = LETOCMD_sort;
   pData[ 1 ] = ';';

   ptr = leto_PutTransInfo( pArea, pAreaDst, pTransInfo, pData + 2 );

   ptr += eprintf( ptr, "%d;", pSortInfo->uiItemCount );

   for( uiIndex = 0; uiIndex < pSortInfo->uiItemCount; uiIndex++ )
   {
      ptr += eprintf( ptr, "%d,%d;",
                      pSortInfo->lpdbsItem[ uiIndex ].uiField,
                      pSortInfo->lpdbsItem[ uiIndex ].uiFlags );
   }

   errCode = ( leto_SendRecv( pConnection, pArea, pData, ptr - pData, 1021 ) ? HB_SUCCESS : HB_FAILURE );
   hb_xfree( pData );

   return errCode;
}

static HB_ERRCODE letoTrans( LETOAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   LETOAREAP  pAreaDst = ( LETOAREAP ) pTransInfo->lpaDest;
   HB_ERRCODE errCode;
   char *     pData, * ptr;
   HB_ULONG   ulLen;

   if( ! leto_CheckArea( pAreaDst ) ||
       ( pArea->pTable->uiConnection != pAreaDst->pTable->uiConnection ) ||
       ( pTransInfo->dbsci.itmCobFor && ! pTransInfo->dbsci.lpstrFor ) ||
       ( pTransInfo->dbsci.itmCobWhile && ! pTransInfo->dbsci.lpstrWhile ) )
   {
      return SUPER_TRANS( ( AREAP ) pArea, pTransInfo );
   }

   if( pAreaDst->pTable->uiUpdated )
      leto_PutRec( pAreaDst, HB_FALSE );

   ulLen = 82 + LETO_MAX_TAGNAME +
           hb_itemGetCLen( pTransInfo->dbsci.lpstrFor ) +
           hb_itemGetCLen( pTransInfo->dbsci.lpstrWhile ) +
           pTransInfo->uiItemCount * 16;

   pData = hb_xgrab( ulLen );
   pData[ 0 ] = LETOCMD_trans;
   pData[ 1 ] = ';';
   ptr = leto_PutTransInfo( pArea, pAreaDst, pTransInfo, pData + 2 );

   errCode = ( leto_SendRecv( pConnection, pArea, pData, ptr - pData, 1021 ) ? HB_SUCCESS : HB_FAILURE );
   if( errCode == HB_SUCCESS )  // v 2.16
   {
      ptr = leto_firstchar( pConnection );
      if( ! memcmp( ptr, "+++;", 4 ) )
         leto_ParseRec( pConnection, pAreaDst, ptr + 4 );
   }
   hb_xfree( pData );

   return errCode;
}

#define letoTransRec  NULL

static HB_ERRCODE letoZap( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoZap(%p)", pArea ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( letoGetConnPool( pTable->uiConnection )->fTransActive )
   {
      commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }
   if( pTable->fReadonly )
   {
      commonError( pArea, EG_READONLY, EDBF_READONLY, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }
   if( pTable->fShared )
   {
      commonError( pArea, EG_SHARED, EDBF_SHARED, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }

   if( LetoDbZap( pTable ) )
      return HB_FAILURE;

   return SELF_GOTOP( ( AREAP ) pArea );
}

static HB_ERRCODE letoChildEnd( LETOAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "letoChildEnd(%p, %p)", pArea, pRelInfo ) );

   if( pArea->lpdbPendingRel == pRelInfo )
      errCode = SELF_FORCEREL( ( AREAP ) pArea );
   else
      errCode = HB_SUCCESS;
   SUPER_CHILDEND( ( AREAP ) pArea, pRelInfo );
   return errCode;
}

static HB_ERRCODE letoChildStart( LETOAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoChildStart(%p, %p)", pArea, pRelInfo ) );

   if( SELF_CHILDSYNC( ( AREAP ) pArea, pRelInfo ) != HB_SUCCESS )
      return HB_FAILURE;
   return SUPER_CHILDSTART( ( AREAP ) pArea, pRelInfo );
}

static HB_ERRCODE letoChildSync( LETOAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoChildSync(%p, %p)", pArea, pRelInfo ) );

   pArea->lpdbPendingRel = pRelInfo;

   if( pArea->area.lpdbRelations )
      return SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return HB_SUCCESS;
}

static HB_ERRCODE letoForceRel( LETOAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoForceRel(%p)", pArea ) );

   if( pArea->lpdbPendingRel )
   {
      LPDBRELINFO lpdbPendingRel;

      lpdbPendingRel = pArea->lpdbPendingRel;
      pArea->lpdbPendingRel = NULL;

      return SELF_RELEVAL( ( AREAP ) pArea, lpdbPendingRel );
   }
   return HB_SUCCESS;
}

#define letoSyncChildren  NULL
#define letoRelArea       NULL
#define letoRelEval       NULL
#define letoRelText       NULL

static HB_ERRCODE letoClearRel( LETOAREAP pArea )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "letoClearRel(%p)", pArea ) );

   if( ( ( AREAP ) pArea )->lpdbRelations )
   {
      errCode = SUPER_CLEARREL( ( AREAP ) pArea );
      if( errCode == HB_SUCCESS && letoGetConnPool( pArea->pTable->uiConnection )->uiServerMode >= 3 )
      {
         LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
         char     szData[ 17 ];
         HB_ULONG ulLen;

         ulLen = eprintf( szData, "%c;%lu;02;", LETOCMD_rela, pArea->pTable->hTable );
         if( ! leto_SendRecv( pConnection, pArea, szData, ulLen, 1026 ) )
            errCode = HB_FAILURE;
      }
   }
   return errCode;
}

static HB_ERRCODE leto_doCyclicCheck( AREAP pArea, void * p )
{
   LETOCONNECTION * pConnection = ( LETOCONNECTION * ) p;

   if( pArea->lpdbRelations )
   {
      LPDBRELINFO lpDbRel = pArea->lpdbRelations;

      while( lpDbRel )
      {
         if( lpDbRel->lpaChild->uiArea == pConnection->iConnectRes )
         {
            pConnection->iError = lpDbRel->lpaChild->uiArea;
            break;
         }
         lpDbRel = lpDbRel->lpdbriNext;
      }
   }
   return pConnection->iError ? HB_FAILURE : HB_SUCCESS;
}

/* can fail at server because of CB with a user function */
static HB_ERRCODE letoSetRel( LETOAREAP pArea, LPDBRELINFO pRelInf )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "letoSetRel(%p, %p)", pArea, pRelInf ) );

   pConnection->iError = 0;
   pConnection->iConnectRes = ( ( AREAP ) pArea )->uiArea;
   hb_rddIterateWorkAreas( leto_doCyclicCheck, ( void * ) pConnection );
   if( pConnection->iError )  /* affected cyclic WA number */
   {
      commonError( pArea, EG_SYNTAX, 1020, 0, NULL, 0, "cyclic relations detected" );
      errCode = HB_FAILURE;
      if( pRelInf->abKey )
         hb_itemRelease( pRelInf->abKey );
      if( pRelInf->itmCobExpr )
         hb_itemRelease( pRelInf->itmCobExpr );
   }
   else
      errCode = SUPER_SETREL( ( AREAP ) pArea, pRelInf );

   if( errCode == HB_SUCCESS )
   {
      if( pConnection->uiServerMode >= 3 )  /* only needed in this mode */
      {
         LPDBRELINFO lpDbRel = pRelInf;
         HB_USHORT   uiCount = 0;
         HB_ULONG    ulLen = 512, ulTmp;
         char *      szRelations = ( char * ) hb_xgrab( ulLen );
         char *      ptr = szRelations;

         /* parent area is transmitted as server internal ulAreaID,
          * not as client lpDbRel->lpaParent->uiArea */
         while( lpDbRel )
         {
            if( uiCount++ == 0 )
               ptr += eprintf( ptr, "%c;%lu;01;", LETOCMD_rela, pArea->pTable->hTable );
            ulTmp = ptr - szRelations;
            if( ulLen - ulTmp < 256 )
            {
               ulLen += 255;
               szRelations = ( char * ) hb_xrealloc( szRelations, ulLen );
               ptr = szRelations + ulTmp;
            }
            if( ! HB_IS_STRING( lpDbRel->abKey ) || ! hb_itemGetCLen( lpDbRel->abKey ) ||
                ! HB_IS_BLOCK( lpDbRel->itmCobExpr ) )
            {
               commonError( pArea, EG_SYNTAX, 1020, 0, NULL, 0,
                            HB_IS_BLOCK( lpDbRel->itmCobExpr ) ? "empty relation string" : "empty relation block" );
               errCode = HB_FAILURE;
               break;
            }
            ptr += eprintf( ptr, "%lu;%s;", ( HB_ULONG ) lpDbRel->lpaChild->uiArea,
                                             hb_itemGetCPtr( lpDbRel->abKey ) );
            lpDbRel = lpDbRel->lpdbriNext;
         }
         if( uiCount && errCode == HB_SUCCESS )
         {
            if( leto_SendRecv( pConnection, pArea, szRelations, ptr - szRelations, 0 ) )
            {
               if( leto_CheckError( pArea ) )
                  errCode = HB_FAILURE;
            }
            else
               errCode = HB_FAILURE;
         }
         else
            errCode = HB_FAILURE;
         hb_xfree( szRelations );
      }
      if( errCode == HB_SUCCESS && pArea->area.lpdbRelations )
         return SELF_SYNCCHILDREN( ( AREAP ) pArea );
   }

   if( errCode == HB_FAILURE )
      letoClearRel( pArea );

   return errCode;
}

/* ToDo make such in letocl.c for LetoDB C API */
static HB_ERRCODE letoOrderListAdd( LETOAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   LETOTABLE *      pTable = pArea->pTable;
   char             szData[ HB_PATH_MAX + 16 ], * ptr1;
   const char *     ptr;
   const char *     szBagName;
   LETOTAGINFO *    pTagInfo;
   int              iRcvLen;

   HB_TRACE( HB_TR_DEBUG, ( "letoOrderListAdd(%p, %p)", pArea, pOrderInfo ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   szBagName = leto_RemoveIpFromPath( hb_itemGetCPtr( pOrderInfo->atomBagName ) );

   hb_strncpy( szData, szBagName, HB_PATH_MAX - 1 );
   ptr1 = szData + strlen( szData ) - 1;  /* last char */
   for( ;; )
   {
      if( *ptr1 == '.' )
         *ptr1 = '\0';
      else if( *ptr1 == '/' || *ptr1 == '\\' )
      {
         ptr1++;
         break;
      }
      if( ptr1 == szData )
         break;
      --ptr1;
   }
   ptr = ptr1;

   pTagInfo = pTable->pTagInfo;
   while( pTagInfo )
   {
      if( ( ! pConnection->fLowerCase ) ? ! strcmp( ptr, pTagInfo->BagName ) :    /* FIXED BAGname is case sensitive */
                                          ! leto_stricmp( ptr, pTagInfo->BagName ) )
      {
         if( pTable->pTagCurrent )
            return HB_SUCCESS;
         else
         {
            pTable->pTagCurrent = pTagInfo;
            return letoGoTop( pArea );
         }
      }
      pTagInfo = pTagInfo->pNext;
   }

   iRcvLen = eprintf( szData, "%c;%lu;%s;", LETOCMD_open_i, pTable->hTable, szBagName );
   if( ( iRcvLen = leto_SendRecv( pConnection, pArea, szData, iRcvLen, 0 ) ) == 0 || leto_CheckError( pArea ) )
      return HB_FAILURE;

   /* sets pTable->pTagCurrent */
   ptr = leto_ParseTagInfo( pTable, leto_firstchar( pConnection ) );

   pTagInfo = pTable->pTagInfo;
   while( pTagInfo )
   {
      if( ! pTagInfo->pExtra || ! ( ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra )->pKeyItem )
         leto_CreateKeyExpr( pArea, pTagInfo, NULL );
#if 0  /* temporary outcommented */
      if( ( ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra )->uiFCount == 0 )
         ScanIndexFields( pArea, pTagInfo );
#endif
      pTagInfo = pTagInfo->pNext;
   }

   if( ( iRcvLen - 4 ) > ( ptr - leto_getRcvBuff() ) )
      leto_ParseRec( pConnection, pArea, ptr );

   leto_SetAreaFlags( pArea );
   if( pArea->area.lpdbRelations )
      return SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return HB_SUCCESS;
}

static HB_ERRCODE letoOrderListClear( LETOAREAP pArea )  /* OrdListClear() */
{
   LETOTABLE *   pTable = pArea->pTable;
   LETOTAGINFO * pTagInfo = pTable->pTagInfo, * pTagPrev = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "letoOrderListClear(%p)", pArea ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( pTagInfo )
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
      LETOTAGINFO * pTag1;
      char          szData[ 32 ];

      eprintf( szData, "%c;%lu;04;", LETOCMD_ord, pTable->hTable );
      if( ! leto_SendRecv( pConnection, pArea, szData, 0, 1021 ) )
         return HB_FAILURE;

      pTable->uiOrders = 0;
      do
      {
         /* no leto_ProdCheck() for NTX */
         if( ! pTagInfo->fProduction || pTable->uiDriver || ! hb_setGetAutOpen() )
         {
#ifdef LETO_CLIENTLOG
            leto_clientlog( NULL, 0, "letoOrderListClear( driver %d prod %d %s (%s) )", pTable->uiDriver,
                            pTagInfo->fProduction, pTagInfo->BagName, pArea->szDataFileName );
#endif
            pTag1 = pTagInfo;
            if( pTagInfo == pTable->pTagInfo )
               pTagInfo = pTable->pTagInfo = pTagInfo->pNext;
            else
               pTagInfo = pTagPrev->pNext = pTagInfo->pNext;

            if( pTable->pTagCurrent == pTag1 )
               pTable->pTagCurrent = NULL;
            leto_FreeTag( pTag1 );
         }
         else
         {
            pTagPrev = pTagInfo;
            pTagInfo = pTagInfo->pNext;
            pTable->uiOrders++;
         }
      }
      while( pTagInfo );

   }

   return HB_SUCCESS;
}

/* ToDo make such in letocl.c for LetoDB C API */
static HB_ERRCODE letoOrderListDelete( LETOAREAP pArea, LPDBORDERINFO pOrderInfo )  /* OrdBagClear */
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   LETOTABLE *   pTable = pArea->pTable;
   char          szData[ HB_PATH_MAX + 21 ];
   char          szBagName[ HB_PATH_MAX ];
   LETOTAGINFO * pTagInfo = pTable->pTagInfo, * pTag1, * pTagPrev;


   HB_TRACE( HB_TR_DEBUG, ( "letoOrderListDelete(%p, %p)", pArea, pOrderInfo ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( ! pTagInfo )
      return HB_FAILURE;
   else
   {
      HB_USHORT uiLen;
      char *    ptr;

      hb_strncpy( szBagName, leto_RemoveIpFromPath( hb_itemGetCPtr( pOrderInfo->atomBagName ) ),
                  HB_PATH_MAX - 1 );
      if( strchr( szBagName, ';' ) != NULL )
         return HB_FAILURE;
      if( ! *szBagName )
         strcpy( szBagName, pTagInfo->BagName );

      if( ( ptr = strchr( szBagName, '.' ) ) != NULL )
      {
         *( ptr + 1 ) = '\0';
         uiLen = ( HB_USHORT ) strlen( szBagName );
      }
      else
      {
         uiLen = ( HB_USHORT ) strlen( szBagName );
         szBagName[ uiLen++ ] = '.';
         szBagName[ uiLen ] = '\0';
      }

      while( uiLen > 1 && pTagInfo )
      {
         if( ( ! pConnection->fLowerCase ) ? ! strncmp( pTagInfo->BagName, szBagName, uiLen ) :
                                             ! hb_strnicmp( pTagInfo->BagName, szBagName, uiLen ) )
            break;
         pTagInfo = pTagInfo->pNext;
      }

      if( uiLen < 2 || ! pTagInfo )
         return HB_FAILURE;

      if( ! pTagInfo->fProduction || pTable->uiDriver || ! hb_setGetAutOpen() )
      {
         /* note: pOrderInfo->atomBagName is TAGname */
         eprintf( szData, "%c;%lu;10;%s;", LETOCMD_ord, pTable->hTable, szBagName );
         if( ! leto_SendRecv( pConnection, pArea, szData, 0, 1021 ) )
            return HB_FAILURE;

         if( *pConnection->szBuffer != '+' )
            return HB_FAILURE;

         pTagInfo = pTable->pTagInfo;
         pTagPrev = pTagInfo;
         while( pTagInfo )
         {
            if( ( ! pConnection->fLowerCase ) ? ! strncmp( pTagInfo->BagName, szBagName, uiLen ) :
                                                ! hb_strnicmp( pTagInfo->BagName, szBagName, uiLen ) )
            {
               pTag1 = pTagInfo;
               if( pTagInfo == pTable->pTagInfo )
                  pTagInfo = pTable->pTagInfo = pTagInfo->pNext;
               else
                  pTagInfo = pTagPrev->pNext = pTagInfo->pNext;

               pTable->uiOrders--;
               if( pTable->pTagCurrent == pTag1 )
                  pTable->pTagCurrent = NULL;
               leto_FreeTag( pTag1 );
            }
            else
            {
               pTagPrev = pTagInfo;
               pTagInfo = pTagInfo->pNext;
            }
         }
      }
   }

   return HB_SUCCESS;
}

static HB_ERRCODE letoOrderListFocus( LETOAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoOrderListFocus(%p, %p)", pArea, pOrderInfo ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( HB_IS_STRING( pOrderInfo->itmOrder ) )
   {
      char szTag[ LETO_MAX_TAGNAME + 1 ];

      hb_strncpyUpper( szTag, hb_itemGetCPtr( pOrderInfo->itmOrder ), hb_itemGetCLen( pOrderInfo->itmOrder ) );
      pOrderInfo->itmOrder = hb_itemPutC( pOrderInfo->itmOrder, szTag );
   }
   if( ! pTable->pTagCurrent || ! pTable->pTagCurrent->TagName )
      pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, "" );
   else
      pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult,
                                           pTable->pTagCurrent->TagName );

   if( pOrderInfo->itmOrder )
   {
      if( LetoDbOrderFocus( pTable,
                     HB_IS_STRING( pOrderInfo->itmOrder ) ? hb_itemGetCPtr( pOrderInfo->itmOrder ) : NULL,
                     HB_IS_NUMERIC( pOrderInfo->itmOrder ) ? ( HB_USHORT ) hb_itemGetNI( pOrderInfo->itmOrder ) : 0 ) == 0 )
         return HB_SUCCESS;
   }

   return HB_FAILURE;
}

static HB_ERRCODE letoOrderListRebuild( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoOrderListRebuild(%p)", pArea ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( LetoDbReindex( pTable ) )
      return HB_FAILURE;

   return SELF_GOTOP( ( AREAP ) pArea );
}

#define letoOrderCondition  NULL

static HB_ERRCODE letoOrderCreate( LETOAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   LETOTABLE *   pTable = pArea->pTable;
   const char *  szKey, * szFor, * szBagName;
   char          szTag[ LETO_MAX_TAGNAME + 1 ];
   LETOTAGINFO * pTagInfo;
   unsigned int  uiFlags;
   int           iRes;
   LPDBORDERCONDINFO lpdbOrdCondInfo = pArea->area.lpdbOrdCondInfo;

   HB_TRACE( HB_TR_DEBUG, ( "letoOrderCreate(%p, %p)", pArea, pOrderInfo ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   /* we need to transfer to server and use the compiled string expression
   * a possible CB in pOrderInfo->itmCobExpr doesn't matter */
   if( ! hb_itemGetCLen( pOrderInfo->abExpr ) )
      return HB_FAILURE;
   szKey = hb_itemGetCPtr( pOrderInfo->abExpr );

   if( pOrderInfo->abBagName )
      szBagName = leto_RemoveIpFromPath( pOrderInfo->abBagName );
   else
      szBagName = NULL;

   if( pOrderInfo->atomBagName && *pOrderInfo->atomBagName )
      hb_strncpy( szTag, pOrderInfo->atomBagName, LETO_MAX_TAGNAME + 1 );
   else if( szBagName && ! pTable->uiDriver )  /* for NTX create a TAG from BAG */
   {
      PHB_FNAME    pFilePath = hb_fsFNameSplit( szBagName );
      const char * ptr = pFilePath->szName;
      char *       ptr2;

      ptr2 = strrchr( ptr, '/' );
      if( ptr2 )
      {
         ptr = ptr2 + 1;
         ptr2 = strrchr( ptr, '\\' );
         if( ptr2 )
            ptr = ptr2 + 1;
      }
      hb_strncpy( szTag, ptr, LETO_MAX_TAGNAME + 1 );
      hb_xfree( pFilePath );
      szTag[ LETO_MAX_TAGNAME ] = '\0';
   }
   else
      szTag[ 0 ] = '\0';

   szFor = ( lpdbOrdCondInfo && lpdbOrdCondInfo->abFor ) ? lpdbOrdCondInfo->abFor : "";

   uiFlags = pOrderInfo->fUnique ? LETO_INDEX_UNIQ : 0;
   if( lpdbOrdCondInfo )
      uiFlags |= ( lpdbOrdCondInfo->fAll ? LETO_INDEX_ALL : 0 ) |
                 ( lpdbOrdCondInfo->fRest ? LETO_INDEX_REST : 0 ) |
                 ( lpdbOrdCondInfo->fDescending ? LETO_INDEX_DESC : 0 ) |
                 ( lpdbOrdCondInfo->fCustom ? LETO_INDEX_CUST : 0 ) |
                 ( lpdbOrdCondInfo->fAdditive ? LETO_INDEX_ADD : 0 ) |
                 ( lpdbOrdCondInfo->fTemporary ? LETO_INDEX_TEMP : 0 ) |
                 ( lpdbOrdCondInfo->fUseFilter ? LETO_INDEX_FILT : 0 );

   /* close all index order, re-register the orders reported by server after creating the new order */
   /* letoOrderListClear( pArea ); */
   if( pTable->pTagInfo )
   {
      LETOTAGINFO * pTag, * pTagNext;

      pTag = pTable->pTagInfo;
      do
      {
         pTagNext = pTag->pNext;
         leto_FreeTag( pTag );
         pTag = pTagNext;
      }
      while( pTag );

      pTable->uiOrders = 0;
      pTable->pTagInfo = NULL;
      pTable->pTagCurrent = NULL;
   }

   /* sets pTable->pTagCurrent */
   iRes = LetoDbOrderCreate( pTable, szBagName, szTag,
                             szKey, uiFlags, szFor,
                             lpdbOrdCondInfo ? lpdbOrdCondInfo->abWhile : NULL,
                             lpdbOrdCondInfo ? lpdbOrdCondInfo->lNextCount : 0 );
   if( iRes )
   {
      if( LetoGetError() == 1000 )
         commonError( pArea, EG_DATATYPE, LetoGetError(), 0, NULL, 0, NULL );
      else
         leto_CheckError( pArea );
      return HB_FAILURE;
   }

   pTagInfo = pTable->pTagInfo;
   while( pTagInfo )
   {
      if( ! pTagInfo->pExtra || ! ( ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra )->pKeyItem )
         leto_CreateKeyExpr( pArea, pTagInfo, NULL );
#if 0  /* temporary outcommented */
      if( ( ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra )->uiFCount == 0 )
         ScanIndexFields( pArea, pTagInfo );
#endif
      pTagInfo = pTagInfo->pNext;
   }

   return HB_SUCCESS;  /* Harbour should do for us: return SELF_GOTOP( ( AREAP ) pArea ); */
}

static HB_ERRCODE letoOrderDestroy( LETOAREAP pArea, LPDBORDERINFO pOrderInfo )  /* orddestroy() */
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   LETOTABLE *   pTable = pArea->pTable;
   char          szData[ HB_PATH_MAX + 21 ];
   const char *  szTagName = NULL;
   LETOTAGINFO * pTagInfo = pTable->pTagInfo, * pTagPrev = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "letoOrderDestroy(%p, %p)", pArea, pOrderInfo ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( ! pTagInfo || ! pTable )
      return HB_FAILURE;
   if( ! pOrderInfo->itmOrder && pTable->pTagCurrent )
      szTagName = pTable->pTagCurrent->TagName;
   else if( HB_IS_NUMERIC( pOrderInfo->itmOrder ) )
   {
      HB_USHORT ui = ( HB_USHORT ) hb_itemGetNI( pOrderInfo->itmOrder );

      while( pTagInfo && ui > 1 )
         pTagInfo = pTagInfo->pNext;
      if( pTagInfo && ui == 1 )
         szTagName = pTagInfo->TagName;
   }
   else if( HB_IS_STRING( pOrderInfo->itmOrder ) )
      szTagName = hb_itemGetCPtr( pOrderInfo->itmOrder );

   if( ! szTagName )
      return HB_FAILURE;
   else
   {
      eprintf( szData, "%c;%lu;13;%s;", LETOCMD_ord, pTable->hTable, szTagName );
      if( ! leto_SendRecv( pConnection, pArea, szData, 0, 0 ) || leto_CheckError( pArea ) )
         return HB_FAILURE;

      pTagInfo = pTable->pTagInfo;
      while( pTagInfo )
      {
         if( ( ! pConnection->fLowerCase ) ? ! strcmp( szTagName, pTagInfo->TagName ) :    /* FIXED BAGname is case sensitive */
                                             ! leto_stricmp( szTagName, pTagInfo->TagName ) )
         {
            if( pTagInfo == pTable->pTagInfo )
               pTable->pTagInfo = pTagInfo->pNext;
            else
               pTagPrev->pNext = pTagInfo->pNext;
            pTable->uiOrders--;
            if( pTable->pTagCurrent == pTagInfo )
               pTable->pTagCurrent = NULL;
            leto_FreeTag( pTagInfo );
            break;
         }

         pTagPrev = pTagInfo;
         pTagInfo = pTagInfo->pNext;
      }
   }

   return HB_SUCCESS;
}

static HB_BOOL leto_SkipEval( LETOAREAP pArea, LETOTAGINFO * pTagInfo, HB_BOOL fForward, PHB_ITEM pEval )
{
   LETOTABLE * pTable = pArea->pTable;
   PHB_ITEM    pKeyVal, pKeyRec;
   HB_BOOL     fRet;

   if( ! pTagInfo || hb_itemType( pEval ) != HB_IT_BLOCK )
   {
      if( SELF_SKIP( ( AREAP ) pArea, fForward ? 1 : -1 ) == HB_FAILURE )
         return HB_FALSE;
      return fForward ? ! pArea->area.fEof : ! pArea->area.fBof;
   }

   for( ;; )
   {
      SELF_SKIP( ( AREAP ) pArea, fForward ? 1 : -1 );
      if( fForward ? pArea->area.fEof : pArea->area.fBof )
         break;
      pKeyVal = leto_KeyEval( pArea, pTagInfo );
      pKeyRec = hb_itemPutNInt( NULL, pTable->ulRecNo );
      fRet = hb_itemGetL( hb_vmEvalBlockV( pEval, 2, pKeyVal, pKeyRec ) );
      hb_itemRelease( pKeyRec );
      if( fRet )
         break;
   }

   return fForward ? ! pArea->area.fEof : ! pArea->area.fBof;
}

static HB_ERRCODE letoOrderInfo( LETOAREAP pArea, HB_USHORT uiIndex, LPDBORDERINFO pOrderInfo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   LETOTABLE *      pTable = pArea->pTable;
   LETOTAGINFO *    pTagInfo = NULL;
   char             szData[ 256 ]; /* was much too short DBOI_SKIPWILD */
   HB_USHORT        uiTag = 0;

   HB_TRACE( HB_TR_DEBUG, ( "letoOrderInfo(%p, %hu, %p)", pArea, uiIndex, pOrderInfo ) );

   if( pTable && pOrderInfo->itmOrder )
   {
      if( pTable->pTagInfo )  /* index enabled */
      {
         if( HB_IS_STRING( pOrderInfo->itmOrder ) )
         {
            char szTagName[ LETO_MAX_TAGNAME + 1 ];

            memset( szTagName, '\0', LETO_MAX_TAGNAME + 1 );
            strncpy( szTagName, hb_itemGetCPtr( pOrderInfo->itmOrder ), LETO_MAX_TAGNAME );
            if( *szTagName )
            {
               pTagInfo = pTable->pTagInfo;
               do
               {
                  if( ! leto_stricmp( szTagName, pTagInfo->TagName ) )
                  {
                     uiTag = pTagInfo->uiTag;
                     break;
                  }
                  pTagInfo = pTagInfo->pNext;
               }
               while( pTagInfo );
            }
         }
         else if( HB_IS_NUMERIC( pOrderInfo->itmOrder ) )
         {
            HB_USHORT uiOrder = ( HB_USHORT ) hb_itemGetNI( pOrderInfo->itmOrder );

            if( uiOrder && uiOrder <= pTable->uiOrders )
            {
               pTagInfo = pTable->pTagInfo;
               while( --uiOrder )
                  pTagInfo = pTagInfo->pNext;
               uiOrder++;
            }
            else
               uiOrder = 0;
            uiTag = uiOrder;
         }
      }
      else
        return HB_FAILURE;
   }
   else if( pTable )
   {
      pTagInfo = pTable->pTagCurrent;
      if( pTagInfo )
         uiTag = pTagInfo->uiTag;
   }
   else
     return HB_FAILURE;

   switch( uiIndex )
   {
      case DBOI_CONDITION:
         if( pTagInfo )
            hb_itemPutC( pOrderInfo->itmResult, pTagInfo->ForExpr );
         else
            hb_itemPutC( pOrderInfo->itmResult, "" );
         break;

      case DBOI_EXPRESSION:
         if( pTagInfo )
            hb_itemPutC( pOrderInfo->itmResult, pTagInfo->KeyExpr );
         else
            hb_itemPutC( pOrderInfo->itmResult, "" );
         break;

      case DBOI_KEYTYPE:
         if( pTagInfo )
         {
            char s[ 2 ];

            s[ 0 ] = pTagInfo->cKeyType;
            s[ 1 ] = '\0';
            hb_itemPutC( pOrderInfo->itmResult, s );
         }
         else
            hb_itemPutC( pOrderInfo->itmResult, "U" );
         break;

      case DBOI_KEYSIZE:
         hb_itemPutNL( pOrderInfo->itmResult, pTagInfo ? pTagInfo->uiKeySize : 0 );
         break;

      case DBOI_KEYVAL:
         hb_itemClear( pOrderInfo->itmResult );

         if( pArea->lpdbPendingRel )
            SELF_FORCEREL( ( AREAP ) pArea );

         if( ! pArea->area.fEof && pTagInfo && ( ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra )->pKeyItem )
         {
            PHB_ITEM pItem = leto_KeyEval( pArea, pTagInfo );

            hb_itemCopy( pOrderInfo->itmResult, pItem );
         }
         break;

      case DBOI_KEYCOUNT:
      case DBOI_KEYCOUNTRAW:
      {
         HB_ULONG ul = 0;

#if 0  /* nonsense, won't get to here without active index */
         if( ! pTagInfo )
         {
            LetoDbRecCount( pTable, ( unsigned long * ) &ul );
            hb_itemPutNL( pOrderInfo->itmResult, ul );
            break;
         }
#endif
         if( pTagInfo && pTagInfo->ulKeyCount && uiIndex != DBOI_KEYCOUNTRAW )
            ul = pTagInfo->ulKeyCount;
         else if( pTagInfo )
         {
            if( uiIndex == DBOI_KEYCOUNTRAW )
               ul = eprintf( szData, "%c;%lu;12;%s;", LETOCMD_ord, pTable->hTable, pTagInfo->TagName );
            else
               ul = eprintf( szData, "%c;%lu;01;%s;", LETOCMD_ord, pTable->hTable, pTagInfo->TagName );
            if( ! leto_SendRecv( pConnection, pArea, szData, ul, 1021 ) )
               return HB_FAILURE;

            ul = strtoul( leto_firstchar( pConnection ), NULL, 10 );
            pTagInfo->ulKeyCount = ul;
         }
         hb_itemPutNL( pOrderInfo->itmResult, ul );

         break;
      }

      case DBOI_SCOPETOP:
      case DBOI_SCOPEBOTTOM:
      case DBOI_SCOPESET:
      case DBOI_SCOPETOPCLEAR:
      case DBOI_SCOPEBOTTOMCLEAR:
      case DBOI_SCOPECLEAR:
         if( pTagInfo )  // TOFIX: pTagInfo->fUsrAscend - inverted TOP and BOTTOM!!!
         {
            HB_BOOL fClearTop = ( ( uiIndex == DBOI_SCOPETOPCLEAR ) || ( uiIndex == DBOI_SCOPECLEAR ) );
            HB_BOOL fClearBottom = ( ( uiIndex == DBOI_SCOPEBOTTOMCLEAR ) || ( uiIndex == DBOI_SCOPECLEAR ) );
            HB_BOOL fSetTop = ( fClearTop || ( uiIndex == DBOI_SCOPETOP ) || ( uiIndex == DBOI_SCOPESET ) );
            HB_BOOL fSetBottom = ( fClearBottom || ( uiIndex == DBOI_SCOPEBOTTOM ) || ( uiIndex == DBOI_SCOPESET ) );

            if( ! ( ( LETOTAGEXTRAINFO * ) ( pTagInfo->pExtra ) )->pTopScope )
               ( ( LETOTAGEXTRAINFO * ) ( pTagInfo->pExtra ) )->pTopScope = hb_itemNew( NULL );
            if( ! ( ( LETOTAGEXTRAINFO * ) ( pTagInfo->pExtra ) )->pBottomScope )
               ( ( LETOTAGEXTRAINFO * ) ( pTagInfo->pExtra ) )->pBottomScope = hb_itemNew( NULL );

            if( fSetTop && ( uiIndex != DBOI_SCOPECLEAR ) )
               hb_itemCopy( pOrderInfo->itmResult, ( ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra )->pTopScope );
            else if( fSetBottom && ( uiIndex != DBOI_SCOPECLEAR ) )
               hb_itemCopy( pOrderInfo->itmResult, ( ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra )->pBottomScope );
            else if( pOrderInfo->itmResult )
               hb_itemClear( pOrderInfo->itmResult );

            if( pOrderInfo->itmNewVal || fClearTop || fClearBottom )
            {
               char      szKey[ LETO_MAX_KEY ];
               HB_USHORT uiKeyLen = 0;
               HB_ULONG  ulLen;
               char *    pData;
               HB_BOOL   fUpdateTop = HB_FALSE;
               HB_BOOL   fUpdateBottom = HB_FALSE;

               if( fClearTop || fClearBottom )
               {
                  uiKeyLen = 0;
                  szKey[ 0 ] = '\0';
               }
               else
               {
                  HB_BYTE cType = leto_ItemType( pOrderInfo->itmNewVal );

                  if( pTagInfo->cKeyType == cType )
                     uiKeyLen = leto_KeyToStr( pArea, szKey, cType, pOrderInfo->itmNewVal, pTagInfo->uiKeySize );
                  else
                     fSetTop = fSetBottom = fUpdateTop = fUpdateBottom = HB_FALSE;
               }

               if( fSetTop )
               {
                  if( fClearTop )
                     fUpdateTop = ( pTagInfo->pTopScopeAsString != NULL );
                  else if( ! pTagInfo->pTopScopeAsString )
                     fUpdateTop = HB_TRUE;
                  else
                     fUpdateTop = ( strcmp( szKey, pTagInfo->pTopScopeAsString ) != 0 );
               }
               if( fSetBottom )
               {
                  if( fClearBottom )
                     fUpdateBottom = ( pTagInfo->pBottomScopeAsString != NULL );
                  else if( ! pTagInfo->pBottomScopeAsString )
                     fUpdateBottom = HB_TRUE;
                  else
                     fUpdateBottom = ( strcmp( szKey, pTagInfo->pBottomScopeAsString ) != 0 );
               }

               if( fUpdateTop || fUpdateBottom )
               {
                  pTagInfo->ulKeyNo = pTagInfo->ulKeyCount = 0;

                  pData = szData + 4;
                  ulLen = eprintf( pData, "%c;%lu;%d;%s;", LETOCMD_scop, pTable->hTable, uiIndex, pTagInfo->TagName );
                  leto_AddKeyToBuf( pData, szKey, uiKeyLen, &ulLen );
                  if( ! leto_SendRecv( pConnection, pArea, pData, ( HB_ULONG ) ulLen, 1021 ) )
                     return HB_FAILURE;

                  if( fSetTop )
                  {
                     if( fClearTop )
                     {
                        if( fUpdateTop )
                        {
                           hb_xfree( pTagInfo->pTopScopeAsString );
                           pTagInfo->pTopScopeAsString = NULL;
                           pTagInfo->uiTopScopeAsString = 0;
                           hb_itemClear( ( ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra )->pTopScope );
                        }
                     }
                     else if( fUpdateTop )
                     {
                        if( ! pTagInfo->pTopScopeAsString )
                        {
                           pTagInfo->pTopScopeAsString = ( char * ) hb_xgrab( uiKeyLen + 1 );
                           pTagInfo->uiTopScopeAsString = uiKeyLen + 1;
                        }
                        else if( uiKeyLen >= pTagInfo->uiTopScopeAsString )
                        {
                           pTagInfo->pTopScopeAsString = ( char * ) hb_xrealloc( pTagInfo->pTopScopeAsString, uiKeyLen + 1 );
                           pTagInfo->uiTopScopeAsString = uiKeyLen + 1;
                        }
                        memcpy( pTagInfo->pTopScopeAsString, szKey, uiKeyLen + 1 );
                        hb_itemCopy( ( ( LETOTAGEXTRAINFO * ) pTagInfo->pExtra )->pTopScope, pOrderInfo->itmNewVal );
                     }
                  }
                  if( fSetBottom )
                  {
                     if( fClearBottom )
                     {
                        if( fUpdateBottom )
                        {
                           hb_xfree( pTagInfo->pBottomScopeAsString );
                           pTagInfo->pBottomScopeAsString = NULL;
                           pTagInfo->uiBottomScopeAsString = 0;
                           hb_itemClear( ( ( LETOTAGEXTRAINFO * ) ( pTagInfo->pExtra ) )->pBottomScope );
                        }
                     }
                     else if( fUpdateBottom )
                     {
                        if( ! pTagInfo->pBottomScopeAsString )
                        {
                           pTagInfo->pBottomScopeAsString = ( char * ) hb_xgrab( uiKeyLen + 1 );
                           pTagInfo->uiBottomScopeAsString = uiKeyLen + 1;
                        }
                        else if( uiKeyLen >= pTagInfo->uiBottomScopeAsString )
                        {
                           pTagInfo->pBottomScopeAsString = ( char * ) hb_xrealloc( pTagInfo->pBottomScopeAsString, uiKeyLen + 1 );
                           pTagInfo->uiBottomScopeAsString = uiKeyLen + 1;
                        }
                        memcpy( pTagInfo->pBottomScopeAsString, szKey, uiKeyLen + 1 );
                        hb_itemCopy( ( ( LETOTAGEXTRAINFO * ) ( pTagInfo->pExtra ) )->pBottomScope, pOrderInfo->itmNewVal );
                     }
                  }
               }
               pTable->ptrBuf = NULL;
            }
         }
         else if( pOrderInfo->itmResult )
            hb_itemClear( pOrderInfo->itmResult );

         break;

      case DBOI_POSITION:
      case DBOI_KEYNORAW:
      {
         HB_ULONG ul;

         if( pOrderInfo->itmNewVal && HB_IS_NUMERIC( pOrderInfo->itmNewVal ) )   /* ordkeygoto */
         {
            HB_ULONG ulLen;

            if( ! pTagInfo || ! HB_IS_NUMBER( pOrderInfo->itmNewVal ) )
            {
               hb_itemPutL( pOrderInfo->itmResult, HB_FALSE );
               break;
            }

            pArea->lpdbPendingRel = NULL;
            if( pTable->uiUpdated )
               leto_PutRec( pArea, HB_FALSE );

            ulLen = eprintf( szData, "%c;%lu;0%c;%s;%lu;", LETOCMD_ord, pTable->hTable,
                             ( uiIndex == DBOI_POSITION ? '5' : '9' ),
                             pTagInfo->TagName, hb_itemGetNL( pOrderInfo->itmNewVal ) );
            if( ! leto_SendRecv( pConnection, pArea, szData, ulLen, 1021 ) )
               return HB_FAILURE;
            hb_itemPutL( pOrderInfo->itmResult, HB_TRUE );

            leto_ParseRec( pConnection, pArea, leto_firstchar( pConnection ) );
            pTable->ptrBuf = NULL;

            if( pArea->area.lpdbRelations )
               return SELF_SYNCCHILDREN( ( AREAP ) pArea );
            break;
         }
         else     /* ordkeyno */
         {
            if( ! pTagInfo )
            {
               hb_itemPutNL( pOrderInfo->itmResult, HB_FALSE );
               break;
            }
            if( uiIndex == DBOI_POSITION && pTagInfo->ulKeyNo )
               ul = pTagInfo->ulKeyNo;
            else
            {
               HB_ULONG ulLen;

               ulLen = eprintf( szData, "%c;%lu;0%c;%s;%lu;", LETOCMD_ord, pTable->hTable,
                                ( uiIndex == DBOI_POSITION ? '2' : '8' ),
                                pTagInfo->TagName, pTable->ulRecNo );
               if( ! leto_SendRecv( pConnection, pArea, szData, ulLen, 1021 ) )
                  return HB_FAILURE;

               ul = strtoul( leto_firstchar( pConnection ), NULL, 10 );
            }
            hb_itemPutNL( pOrderInfo->itmResult, ul );
         }
         break;
      }

      case DBOI_SKIPUNIQUE:
      {
         int      lRet;
         HB_ULONG ulLen;

         pArea->lpdbPendingRel = NULL;
         if( pTable->uiUpdated )
            leto_PutRec( pArea, HB_FALSE );

         ulLen = eprintf( szData, "%c;%lu;06;%s;%lu;%c;%lu;", LETOCMD_ord, pTable->hTable,
                          ( pTagInfo ) ? pTagInfo->TagName : "",
                          pTable->ulRecNo, ( char ) ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 ),
                          pOrderInfo->itmNewVal && HB_IS_NUMERIC( pOrderInfo->itmNewVal ) ?
                          hb_itemGetNL( pOrderInfo->itmNewVal ) : 1 );
         lRet = leto_SendRecv( pConnection, pArea, szData, ulLen, 1021 );
         if( ! lRet )
            return HB_FAILURE;

         pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, lRet );
         leto_ParseRec( pConnection, pArea, leto_firstchar( pConnection ) );
         pTable->ptrBuf = NULL;

         if( pArea->area.lpdbRelations )
            return SELF_SYNCCHILDREN( ( AREAP ) pArea );
         break;
      }

      case DBOI_SKIPEVAL:
      case DBOI_SKIPEVALBACK:
         pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult,
                                              leto_SkipEval( pArea, pTagInfo, uiIndex == DBOI_SKIPEVAL, pOrderInfo->itmNewVal ) );
         break;

      case DBOI_SKIPWILD:
      case DBOI_SKIPWILDBACK:
      case DBOI_SKIPREGEX:
      case DBOI_SKIPREGEXBACK:
         pArea->lpdbPendingRel = NULL;
         if( pTable->uiUpdated )
            leto_PutRec( pArea, HB_FALSE );

         if( pTagInfo && pOrderInfo->itmNewVal && HB_IS_STRING( pOrderInfo->itmNewVal ) )
         {
            HB_ULONG     ulLen;

            ulLen = eprintf( szData, "%c;%lu;07;%s;%lu;%c;%d;%lu;%s;", LETOCMD_ord, pTable->hTable,
                             pTagInfo->TagName,
                             pTable->ulRecNo, ( char ) ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 ), uiIndex,
                             ( HB_ULONG ) hb_itemGetCLen( pOrderInfo->itmNewVal ),  // ToDo limit length
                             hb_itemGetCPtr( pOrderInfo->itmNewVal ) );

            if( ! leto_SendRecv( pConnection, pArea, szData, ulLen, 1021 ) )
               return HB_FAILURE;

            leto_ParseRec( pConnection, pArea, leto_firstchar( pConnection ) );
            pTable->ptrBuf = NULL;

            if( pArea->area.lpdbRelations )
               SELF_SYNCCHILDREN( ( AREAP ) pArea );
         }
         else
         {
            SELF_SKIP( ( AREAP ) pArea, ( ( uiIndex == DBOI_SKIPWILD ) ||
                                          ( uiIndex == DBOI_SKIPREGEX ) ) ? 1 : -1 );
         }
         pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, ! pArea->area.fEof && ! pArea->area.fBof );
         break;

      case DBOI_BAGEXT:
         if( pOrderInfo->itmResult )
            hb_itemClear( pOrderInfo->itmResult );
         else
            pOrderInfo->itmResult = hb_itemNew( NULL );
         hb_itemPutC( pOrderInfo->itmResult, pTable->szOrderExt );
         break;

      case DBOI_NUMBER:
         hb_itemPutNI( pOrderInfo->itmResult, pTagInfo ? uiTag : 0 );
         break;

      case DBOI_ISDESC:
      case DBOI_CUSTOM:
      case DBOI_UNIQUE:
      {
         HB_BOOL fResult;

         if( ! pTagInfo )
            fResult = HB_FALSE;
         else if( pOrderInfo->itmNewVal && HB_IS_LOGICAL( pOrderInfo->itmNewVal ) )
         {
            const char * ptr;

            eprintf( szData, "%c;%lu;%d;%s;%c;", LETOCMD_dboi, pTable->hTable, uiIndex, pTagInfo->TagName,
                     hb_itemGetL( pOrderInfo->itmNewVal ) ? 'T' : 'F' );

            if( ! leto_SendRecv( pConnection, pArea, szData, 0, 1021 ) )
               return HB_FAILURE;

            ptr = leto_firstchar( pConnection );
            fResult = ( *ptr == 'T' );
            if( uiIndex == DBOI_ISDESC )
               pTagInfo->fUsrAscend = ! fResult;
            else if( uiIndex == DBOI_CUSTOM )
               pTagInfo->fCustom = fResult;
            else
               pTagInfo->fUniqueKey = fResult;
         }
         else
         {
            if( uiIndex == DBOI_ISDESC )
               fResult = ! pTagInfo->fUsrAscend;
            else if( uiIndex == DBOI_CUSTOM )
               fResult = pTagInfo->fCustom;
            else
               fResult = pTagInfo->fUniqueKey;
         }
         pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, fResult );
         break;
      }

      case DBOI_NAME:
         pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, pTagInfo ? pTagInfo->TagName : "" );
         break;

      case DBOI_FULLPATH:
      case DBOI_BAGNAME:
         pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, pTagInfo ? pTagInfo->BagName : "" );
         break;

      case DBOI_BAGCOUNT:
      {
         HB_USHORT uiCount = 0;

         if( pTable->uiOrders )
         {
            char **       pBagNames = hb_xgrab( sizeof( char * ) * pTable->uiOrders );
            LETOTAGINFO * pTag = pTable->pTagInfo;
            HB_BOOL       fFound = HB_FALSE;
            HB_USHORT     ui;

            while( pTag )
            {
               if( pTag->BagName )
               {
                  for( ui = 0; ui < uiCount; ui++ )
                  {
                     if( ( pBagNames[ ui ] == pTag->BagName ) || ! strcmp( pBagNames[ ui ], pTag->BagName ) )
                     {
                        fFound = HB_TRUE;
                        break;
                     }
                  }
                  if( ! fFound && uiCount < pTable->uiOrders )
                     pBagNames[ uiCount++ ] = pTag->BagName;
               }
               pTag = pTag->pNext;
            }
            hb_xfree( pBagNames );
         }
         hb_itemPutNI( pOrderInfo->itmResult, uiCount );
         break;
      }

      case DBOI_BAGNUMBER:
      {
         const char *  pBagName, * pLast = NULL;
         HB_USHORT     uiNumber = 0;
         LETOTAGINFO * pTag = pTable->pTagInfo;

         if( hb_itemGetCLen( pOrderInfo->atomBagName ) > 0 )
            pBagName = hb_itemGetCPtr( pOrderInfo->atomBagName );
         else if( pTagInfo )
            pBagName = pTagInfo->BagName;
         else
            pBagName = NULL;
         if( pBagName )
         {
            while( pTag )
            {
               if( pTag->BagName )
               {
                  if( ! pLast || ! strcmp( pLast, pTag->BagName ) )
                  {
                     pLast = pTag->BagName;
                     ++uiNumber;
                  }
                  if( ! strcmp( pBagName, pTag->BagName ) )
                     break;
               }
               pTag = pTag->pNext;
            }
         }
         hb_itemPutNI( pOrderInfo->itmResult, pTag ? uiNumber : 0 );
         break;
      }

      case DBOI_BAGORDER:
      {
         const char *  pBagName;
         HB_USHORT     uiNumber = 0;
         LETOTAGINFO * pTag = pTable->pTagInfo;

         if( hb_itemGetCLen( pOrderInfo->atomBagName ) > 0 )
            pBagName = hb_itemGetCPtr( pOrderInfo->atomBagName );
         else if( pTagInfo )
            pBagName = pTagInfo->BagName;
         else
            pBagName = NULL;
         if( pBagName )
         {
            while( pTag )
            {
               ++uiNumber;
               if( pTag->BagName && ! strcmp( pBagName, pTag->BagName ) )
                  break;
               pTag = pTag->pNext;
            }
         }
         hb_itemPutNI( pOrderInfo->itmResult, pTag ? uiNumber : 0 );
         break;
      }

      case DBOI_KEYADD:
      case DBOI_KEYDELETE:
      {
         char *    ptr;
         PHB_ITEM  pItem = pOrderInfo->itmNewVal ? pOrderInfo->itmNewVal : leto_KeyEval( pArea, pTagInfo );
         char      szData1[ LETO_MAX_KEY + LETO_MAX_TAGNAME + 56 ];
         char      cType;
         char *    pData = szData1 + 4;
         HB_USHORT uiKeyLen;
         HB_ULONG  ulLen;

         ulLen = eprintf( pData, "%c;%lu;%d;%s;%lu;", LETOCMD_dboi, pTable->hTable, uiIndex,
                  ( pTagInfo ) ? pTagInfo->TagName : "", pTable->ulRecNo );

         if( pItem && ( cType = leto_ItemType( pItem ) ) == pTagInfo->cKeyType )
         {
            char szKey[ LETO_MAX_KEY ];

            uiKeyLen = leto_KeyToStr( pArea, szKey, cType, pItem, pTagInfo->uiKeySize );
            leto_AddKeyToBuf( pData, szKey, uiKeyLen, &ulLen );
            hb_xfree( szKey );
         }
         else
            *( pData + ulLen ) = '\0';

         if( ! leto_SendRecv( pConnection, pArea, pData, ulLen, 1021 ) )
            return HB_FAILURE;
         ptr = leto_firstchar( pConnection );
         pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, *ptr == 'T' );
         break;
      }

      /* ToDo: limiting count to specific Bag */
      case DBOI_ORDERCOUNT:
      {
         PHB_ITEM pItem = ( pOrderInfo->itmNewVal ? pOrderInfo->itmNewVal : NULL );

         if( ! pTable->uiOrders || ! pItem )
            hb_itemPutNI( pOrderInfo->itmResult, pTable->uiOrders );
         else
         {
            const char * pBagName = hb_itemGetCPtr( pOrderInfo->atomBagName );
            int       iBagOrders = 0;
            HB_USHORT uiLen = ( HB_USHORT ) hb_itemGetCLen( pOrderInfo->atomBagName );
            char *    ptr2;

            /* pBagName = pTagInfo->BagName; */

            if( uiLen && ( ptr2 = strchr( pBagName, '.' ) ) != NULL )
               uiLen = ( HB_USHORT ) ( ptr2 - pBagName );

            pTagInfo = pTable->pTagInfo;
            while( uiLen && pTagInfo )
            {
               if( pTagInfo->BagName && ( ! pConnection->fLowerCase ? ! strncmp( pBagName, pTagInfo->BagName, uiLen ) :
                                                                      ! hb_strnicmp( pBagName, pTagInfo->BagName, uiLen ) ) )
                  iBagOrders++;
               pTagInfo = pTagInfo->pNext;
            }

            hb_itemPutNI( pOrderInfo->itmResult, iBagOrders );
         }
         break;
      }
   }

   return HB_SUCCESS;
}

static HB_ERRCODE letoClearFilter( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoClearFilter(%p)", pArea ) );

   if( pTable->hTable && pArea->area.dbfi.fFilter )
   {
      if( pArea->area.dbfi.itmCobExpr )
      {
         hb_vmDestroyBlockOrMacro( pArea->area.dbfi.itmCobExpr );
         pArea->area.dbfi.itmCobExpr = NULL;
      }

      if( pArea->area.dbfi.fOptimized )  /* else no filter at server */
      {
         if( LetoDbClearFilter( pTable ) )
            return HB_FAILURE;
      }
      else
         leto_ClearBuffers( pTable );
   }

#ifdef __BM
   if( pArea->area.dbfi.lpvCargo )
      pArea->area.dbfi.lpvCargo = NULL;
#endif
   return SUPER_CLEARFILTER( ( AREAP ) pArea );
}

#define letoClearLocate  NULL
#define letoClearScope   NULL
#define letoCountScope   NULL
#define letoFilterText   NULL
#define letoScopeInfo    NULL

static HB_ERRCODE letoSetFilter( LETOAREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoSetFilter(%p, %p)", pArea, pFilterInfo ) );

   if( pArea->lpdbPendingRel )  /* SELF_FORCEREL added */
      SELF_FORCEREL( ( AREAP ) pArea );

   if( hb_setGetOptimize() )
   {
      if( SELF_CLEARFILTER( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;

      /* test available filter text for validity at server */
      if( pFilterInfo->abFilterText && hb_itemGetCLen( pFilterInfo->abFilterText ) )
      {
         unsigned int uiRes = LetoDbSetFilter( pTable, hb_itemGetCPtr( pFilterInfo->abFilterText ), hb_setGetForceOpt() );

         if( ! uiRes )  /* server can evaluate expression -> optimized */
         {
            if( pFilterInfo->itmCobExpr )  /* remove a given codeblock */
               pFilterInfo->itmCobExpr = NULL;
            pFilterInfo->fOptimized = HB_TRUE;
         }
         else if( LetoGetError() == 1000 )  /* connection error */
            return HB_FAILURE;
         else
         {
            pFilterInfo->fOptimized = HB_FALSE;
#if 0  /* throw ? error of server - or ignore it ? */
            if( hb_setGetForceOpt() )
            {
               const char * ptr = leto_firstchar( pConnection );

               if( strlen( ptr ) >= 3 && if( ptr[ 3 ] == ':' ) )
                  leto_CheckError( pArea );
            }
#endif
         }
      }

      if( pFilterInfo->itmCobExpr )
      {
         if( ! HB_IS_LOGICAL( hb_vmEvalBlockOrMacro( pFilterInfo->itmCobExpr ) ) )
            return HB_FAILURE;
         ( ( AREAP ) pArea )->dbfi.itmCobExpr = hb_itemNew( pFilterInfo->itmCobExpr );
      }
      else if( ! pFilterInfo->fOptimized )
         return HB_FAILURE;

      if( pFilterInfo->abFilterText )
         ( ( AREAP ) pArea )->dbfi.abFilterText = hb_itemNew( pFilterInfo->abFilterText );
      ( ( AREAP ) pArea )->dbfi.fOptimized = pFilterInfo->fOptimized;
      ( ( AREAP ) pArea )->dbfi.fFilter = HB_TRUE;

      return HB_SUCCESS;
   }

   return SUPER_SETFILTER( ( AREAP ) pArea, pFilterInfo );
}

#define letoSetLocate  NULL
#define letoSetScope   NULL
#define letoSkipScope  NULL
#define letoLocate     NULL
#define letoCompile    NULL
#define letoError      NULL
#define letoEvalBlock  NULL

static HB_ERRCODE letoRawLock( LETOAREAP pArea, HB_USHORT uiAction, HB_ULONG ulRecNo )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoRawLock(%p, %hu, %lu)", pArea, uiAction, ulRecNo ) );

   if( pTable->fReadonly )
      return HB_SUCCESS;  /* Harbour conform, better would be HB_FAILURE */

   if( pTable->uiUpdated )
      leto_PutRec( pArea, HB_FALSE );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   switch( uiAction )
   {
      case REC_LOCK:
         if( LetoDbRecLock( pTable, ulRecNo ) )
         {
            if( LetoGetError() == 1021 )
               commonError( pArea, EG_DATATYPE, 1021, 0, leto_getRcvBuff(), 0, NULL );
            return HB_FAILURE;
         }
         break;

      case REC_UNLOCK:
         if( LetoDbRecUnLock( pTable, ulRecNo ) )
         {
            if( LetoGetError() == 1021 )
               commonError( pArea, EG_DATATYPE, 1021, 0, leto_getRcvBuff(), 0, NULL );
            else if( LetoGetError() == 1031 )
               commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, NULL );
            return HB_FAILURE;
         }
         break;

      case FILE_LOCK:
      case HEADER_LOCK:
         if( LetoDbFileLock( pTable ) )
         {
            if( LetoGetError() == 1021 )
               commonError( pArea, EG_DATATYPE, 1021, 0, leto_getRcvBuff(), 0, NULL );
            return HB_FAILURE;
         }
         break;

      case FILE_UNLOCK:
      case HEADER_UNLOCK:
         if( LetoDbFileUnLock( pTable ) )
         {
            if( LetoGetError() == 1021 )
               commonError( pArea, EG_DATATYPE, 1021, 0, leto_getRcvBuff(), 0, NULL );
            else if( LetoGetError() == 1031 )
               commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, NULL );
            return HB_FAILURE;
         }
         break;
   }
   return HB_SUCCESS;
}

static HB_ERRCODE letoLock( LETOAREAP pArea, LPDBLOCKINFO pLockInfo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   LETOTABLE * pTable = pArea->pTable;
   HB_ULONG    ulRecNo = ( HB_ULONG ) hb_itemGetNL( pLockInfo->itmRecID );
   HB_USHORT   uiAction;

   HB_TRACE( HB_TR_DEBUG, ( "letoLock(%p, %p)", pArea, pLockInfo ) );

   if( ! ulRecNo && pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   switch( pLockInfo->uiMethod )
   {
      case DBLM_EXCLUSIVE:
         ulRecNo = pTable->ulRecNo;
         uiAction = REC_LOCK;
         if( ! pConnection->fTransActive && ( pTable->ulLocksMax || pTable->fFLocked ) )  /* release also file-lock */
            SELF_RAWLOCK( ( AREAP ) pArea, FILE_UNLOCK, 0 );
         break;

      case DBLM_MULTIPLE:
         if( pLockInfo->itmRecID == NULL )
            ulRecNo = pTable->ulRecNo;
         uiAction = REC_LOCK;
         break;

      case DBLM_FILE:
         uiAction = FILE_LOCK;
         if( ! pConnection->fTransActive && pTable->ulLocksMax )  /* release all file locks */
            SELF_RAWLOCK( ( AREAP ) pArea, FILE_UNLOCK, 0 );
         break;

      default:
         /* This should probably throw a real error... */
         pLockInfo->fResult = HB_FALSE;
         return HB_FAILURE;
   }

   pLockInfo->fResult = SELF_RAWLOCK( ( AREAP ) pArea, uiAction, ulRecNo ) == HB_SUCCESS ? HB_TRUE : HB_FALSE;

   if( pLockInfo->fResult && ( pLockInfo->uiMethod == DBLM_EXCLUSIVE || pLockInfo->uiMethod == DBLM_MULTIPLE ) )
      leto_AddRecLock( pTable, ulRecNo );

   return HB_SUCCESS;
}

static HB_ERRCODE letoUnLock( LETOAREAP pArea, PHB_ITEM pRecNo )
{
   LETOTABLE *   pTable = pArea->pTable;
   unsigned long ulRecNo = hb_itemGetNL( pRecNo );
   HB_ERRCODE    errCode;

   HB_TRACE( HB_TR_DEBUG, ( "letoUnLock(%p, %p)", pArea, pRecNo ) );

   errCode = SELF_RAWLOCK( ( AREAP ) pArea, ( ulRecNo ) ? REC_UNLOCK : FILE_UNLOCK, ulRecNo );
   if( errCode == HB_SUCCESS && ulRecNo && pTable->pLocksPos )
   {
      unsigned long ulPos;

      for( ulPos = 0; ulPos < pTable->ulLocksMax; ulPos++ )
      {
         if( pTable->pLocksPos[ ulPos ] == ulRecNo )
         {
            pTable->ulLocksMax--;
            if( ulPos < pTable->ulLocksMax )
            {
               unsigned long * pList = pTable->pLocksPos + ulPos;

               memmove( pList, pList + 1, ( pTable->ulLocksMax - ulPos ) * sizeof( HB_ULONG ) );
            }
            break;
         }
      }
   }

   return errCode;
}

#define letoCloseMemFile   NULL
#define letoCreateMemFile  NULL
#define letoGetValueFile   NULL
#define letoOpenMemFile    NULL
#define letoPutValueFile   NULL
#define letoReadDBHeader   NULL
#define letoWriteDBHeader  NULL

static HB_ERRCODE letoDrop( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, HB_ULONG ulConnect )
{
   LETOCONNECTION * pConnection;
   char szTFileName[ HB_PATH_MAX ];
   char szIFileName[ HB_PATH_MAX ];
   char szData[ ( 2 * HB_PATH_MAX ) + 16 ];
   const char * szTableFile, * szIndexFile;

   HB_SYMBOL_UNUSED( pRDD );
   HB_SYMBOL_UNUSED( ulConnect );

   szTableFile = hb_itemGetCPtr( pItemTable );
   szIndexFile = hb_itemGetCPtr( pItemIndex );

   pConnection = letoParseParam( szIndexFile, szIFileName );
   if( pConnection == NULL )
      pConnection = letoParseParam( szTableFile, szTFileName );
   else
      letoParseParam( szTableFile, szTFileName );

   hb_rddSetNetErr( HB_FALSE );
   if( pConnection != NULL )
   {
      HB_ULONG ulLen = eprintf( szData, "%c;%s;%s;", LETOCMD_drop, szTFileName, szIFileName );

      if( leto_DataSendRecv( pConnection, szData, ulLen ) )
      {
         const char * ptr = leto_firstchar( pConnection );

         if( *ptr == 'F' && *( ptr + 2 ) == '1' )
            hb_rddSetNetErr( HB_TRUE );
         if( *ptr == 'T' )
            return HB_SUCCESS;
      }
   }

   return HB_FAILURE;
}

static HB_ERRCODE letoExists( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, HB_ULONG ulConnect )
{
   LETOCONNECTION * pConnection;
   char szTFileName[ HB_PATH_MAX ];
   char szIFileName[ HB_PATH_MAX ];
   char szData[ ( 2 * HB_PATH_MAX ) + 16 ];
   const char * szTableFile, * szIndexFile;

   HB_SYMBOL_UNUSED( pRDD );
   HB_SYMBOL_UNUSED( ulConnect );

   szTableFile = hb_itemGetCPtr( pItemTable );
   szIndexFile = hb_itemGetCPtr( pItemIndex );

   pConnection = letoParseParam( szIndexFile, szIFileName );
   if( pConnection == NULL )
      pConnection = letoParseParam( szTableFile, szTFileName );
   else
      letoParseParam( szTableFile, szTFileName );

   if( pConnection != NULL )
   {
      HB_ULONG ulLen = eprintf( szData, "%c;%s;%s;", LETOCMD_exists, szTFileName, szIFileName );

      if( leto_DataSendRecv( pConnection, szData, ulLen ) )
      {
         const char * ptr = leto_firstchar( pConnection );

         if( *ptr == 'T' )
            return HB_SUCCESS;
      }
   }

   return HB_FAILURE;
}

static HB_ERRCODE letoRename( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, PHB_ITEM pItemNew, HB_ULONG ulConnect )
{
   LETOCONNECTION * pConnection;
   char szTFileName[ HB_PATH_MAX ];
   char szIFileName[ HB_PATH_MAX ];
   char szFileNameNew[ HB_PATH_MAX ];
   char szData[ ( 3 * HB_PATH_MAX ) + 16 ];
   const char * szTableFile, * szIndexFile, * szNewFile;

   HB_SYMBOL_UNUSED( pRDD );
   HB_SYMBOL_UNUSED( ulConnect );

   szTableFile = hb_itemGetCPtr( pItemTable );
   szIndexFile = hb_itemGetCPtr( pItemIndex );
   szNewFile = hb_itemGetCPtr( pItemNew );

   pConnection = letoParseParam( szIndexFile, szIFileName );
   if( pConnection == NULL )
      pConnection = letoParseParam( szTableFile, szTFileName );
   else
      letoParseParam( szTableFile, szTFileName );
   letoParseParam( szNewFile, szFileNameNew );

   if( pConnection != NULL )
   {
      HB_ULONG ulLen = eprintf( szData, "%c;%s;%s;%s;", LETOCMD_rename, szTFileName, szIFileName, szFileNameNew );

      if( leto_DataSendRecv( pConnection, szData, ulLen ) )
      {
         const char * ptr = leto_firstchar( pConnection );

         if( *ptr == 'T' )
            return HB_SUCCESS;
      }
   }

   return HB_FAILURE;
}

static HB_ERRCODE letoInit( LPRDDNODE pRDD )
{
   HB_SYMBOL_UNUSED( pRDD );

   LetoInit();

#if defined( __XHARBOUR__ ) || defined( __HARBOUR30__ )
   {
      char szFile[ HB_PATH_MAX ];
      char ** pArgv = hb_cmdargARGV();

      if( pArgv )
          leto_getFileFromPath( *pArgv, szFile, HB_PATH_MAX );
      else
          szFile[ 0 ] = '\0';
      LetoSetModName( szFile );
   }
#endif

   return HB_SUCCESS;
}

static HB_ERRCODE letoExit( LPRDDNODE pRDD )
{
   HB_USHORT i;

   HB_SYMBOL_UNUSED( pRDD );

   if( letoGetConnPool( 0 ) )
   {
      for( i = 0; i < uiGetConnCount(); i++ )
      {
         leto_ConnectionClose( letoGetConnPool( i ) );
      }
   }

   LetoExit( 0 );
   if( s_uiRddCount )
   {
      if( ! --s_uiRddCount )
         LetoExit( 1 );
   }

   /* free pRDD->lpvCargo if necessary */

   return HB_SUCCESS;
}

static HB_USHORT leto_MemoType( HB_ULONG ulConnect )
{
   LETOCONNECTION * pConnection = ( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() ) ?
                                  letoGetConnPool( ( HB_USHORT ) ulConnect - 1 ) : letoGetCurrConn();
   HB_USHORT        uiMemoType = 0;

   if( pConnection && pConnection->uiMemoType )
      uiMemoType = pConnection->uiMemoType;
   else if( pConnection && pConnection->szDriver )
   {
#ifdef __BM
      if( ! strcmp( pConnection->szDriver, "BMDBFNTX" ) )
#else
      if( ! strcmp( pConnection->szDriver, "DBFNTX" ) )
#endif
         uiMemoType = DB_MEMO_DBT;
#ifdef __BM
      else if( ! strcmp( pConnection->szDriver, "BMDBFCDX" ) )
#else
      else if( ! strcmp( pConnection->szDriver, "DBFCDX" ) )
#endif
         uiMemoType = DB_MEMO_FPT;
      else if( ! strcmp( pConnection->szDriver, "SIXCDX" ) )
         uiMemoType = DB_MEMO_SMT;
   }
   else if( pConnection && pConnection->uiDriver == 1 )
      uiMemoType = DB_MEMO_DBT;
   else
      uiMemoType = DB_MEMO_FPT;

   return uiMemoType;
}

static HB_USHORT leto_MemoBlocksize( HB_ULONG ulConnect )
{
   LETOCONNECTION * pConnection = ( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() ) ?
                                  letoGetConnPool( ( HB_USHORT ) ulConnect - 1 ) : letoGetCurrConn();
   HB_USHORT        uiMemoBlocksize;

   if( pConnection && pConnection->uiMemoBlocksize )
      uiMemoBlocksize = pConnection->uiMemoBlocksize;
   else if( pConnection )
   {
      switch( leto_MemoType( ulConnect ) )
      {
         case DB_MEMO_DBT:
            uiMemoBlocksize = 512;
            break;

         case DB_MEMO_FPT:
            uiMemoBlocksize = 64;
            break;

         case DB_MEMO_SMT:
            uiMemoBlocksize = 32;
            break;

         default:
            uiMemoBlocksize = 0;
            break;
      }
   }
   else
      uiMemoBlocksize = 64; /* DB_MEMO_FPT */

   return uiMemoBlocksize;
}


static int leto_LockScheme( LETOCONNECTION * pConnection )
{
   int iLockScheme;

   if( pConnection->uiLockSchemeExtend )  // extended or normal ?
   {
      if( pConnection->uiDriver == LETO_NTX )
#if defined( __HARBOUR30__ )  /* does not know DB_DBFLOCK_CLIPPER2 */
         iLockScheme = DB_DBFLOCK_HB32;
#else
         iLockScheme = DB_DBFLOCK_CLIPPER2;
#endif
      else if( pConnection->uiLockSchemeExtend != DB_DBFLOCK_COMIX )
         iLockScheme = DB_DBFLOCK_HB32;
      else
         iLockScheme = DB_DBFLOCK_COMIX;
   }
   else
   {
      if( pConnection->uiDriver == LETO_NTX )
         iLockScheme = DB_DBFLOCK_CLIPPER;
      else
         iLockScheme = DB_DBFLOCK_VFP;
   }
   return iLockScheme;
}


static HB_ERRCODE letoRddInfo( LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoRddInfo(%p, %hu, %lu, %p)", pRDD, uiIndex, ulConnect, pItem ) );

   switch( uiIndex )
   {
      case RDDI_REMOTE:
         hb_itemPutL( pItem, HB_TRUE );
         break;
      case RDDI_CONNECTION:
      {
         HB_USHORT uiConnection;

#if 0  /* !! SCARY !! -- possible take over another connection ... */
         if( HB_IS_NUMBER( pItem ) )
         {
            HB_USHORT uiNewConn = hb_itemGetNI( pItem );

            if( uiNewConn && uiNewConn <= uiGetConnCount() )
               pCurrentConn = letoGetConnPool( uiNewConn - 1 );
         }
#endif
         if( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() )
            uiConnection = ( HB_USHORT ) ulConnect;
         else
         {
            LETOCONNECTION * pConnection = letoGetCurrConn();

            if( pConnection )
               uiConnection = ( HB_USHORT ) pConnection->iConnection + 1;
            else
               uiConnection = 0;
         }

         hb_itemPutNI( pItem, uiConnection );
         break;
      }
      case RDDI_ISDBF:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case RDDI_CANPUTREC:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case RDDI_MEMOTYPE:
         hb_itemPutNI( pItem, leto_MemoType( ulConnect ) );
         break;

      case RDDI_MEMOBLOCKSIZE:
         hb_itemPutNI( pItem, leto_MemoBlocksize( ulConnect ) );
         break;

      case RDDI_LOCKSCHEME:
      {
         LETOCONNECTION * pConnection = ( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() ) ?
                                        letoGetConnPool( ( HB_USHORT ) ulConnect - 1 ) : letoGetCurrConn();

         if( pConnection )
            hb_itemPutNI( pItem, leto_LockScheme( pConnection ) );
         else
            hb_itemPutNI( pItem, 0 );
         break;
      }

      /* boolean ask/ set from server */
      case RDDI_OPTIMIZE:
      case RDDI_FORCEOPT:
      case RDDI_AUTOOPEN:
      case RDDI_MULTITAG:
      {
         LETOCONNECTION * pConnection;
         int iRes = 1;

         if( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() )
            pConnection = letoGetConnPool( ( HB_USHORT ) ulConnect - 1 );
         else
            pConnection = letoGetCurrConn();

         if( pConnection )
         {
            const char * szNewSet = ( HB_IS_LOGICAL( pItem ) ? ( hb_itemGetL( pItem ) ? "T" : "F" ) : NULL );

            if( LetoRddInfo( pConnection, uiIndex, szNewSet ) == HB_SUCCESS )
            {
               if( *( pConnection->szBuffer ) == '+' )
               {
                  if( szNewSet )
                  {
                     hb_itemPutL( pItem, *szNewSet == 'T' );
                     switch( uiIndex )
                     {
                        case RDDI_OPTIMIZE:
                           hb_setSetItem( HB_SET_OPTIMIZE, pItem );
                           break;

                        case RDDI_FORCEOPT:
                           hb_setSetItem( HB_SET_FORCEOPT, pItem );
                           break;

                        case RDDI_AUTOOPEN:
                           hb_setSetItem( HB_SET_AUTOPEN, pItem );
                           break;
                     }
                  }
                  hb_itemPutL( pItem, *( pConnection->szBuffer + 1 ) == 'T' );
                  iRes = 0;
               }
            }
         }
         if( iRes )
            hb_itemPutL( pItem, HB_FALSE );
         break;
      }

      /* string values ask/ set from server */
      case RDDI_TABLEEXT:
      case RDDI_MEMOEXT:
      case RDDI_ORDBAGEXT:      /* multi-TAG default */
      case RDDI_ORDEREXT:       /* single-TAG default */
      case RDDI_ORDSTRUCTEXT:   /* single-TAG default */
      case RDDI_PASSWORD:
      {
         LETOCONNECTION * pConnection;
         int iRes = 1;

         if( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() )
            pConnection = letoGetConnPool( ( HB_USHORT ) ulConnect - 1 );
         else
            pConnection = letoGetCurrConn();

         if( pConnection )
         {
            if( LetoRddInfo( pConnection, uiIndex, HB_IS_STRING( pItem ) ? hb_itemGetC( pItem ) : NULL ) == HB_SUCCESS )
            {
               const char * ptr = leto_firstchar( pConnection );

               if( *( ptr - 1 ) == '+' )
               {
                  char * ptr2 = strchr( ptr, ';' );

                  if( ptr2 )
                  {
                     *ptr2 = '\0';
                     hb_itemPutC( pItem, ptr );
                     iRes = 0;
                  }
               }
            }
         }
         if( iRes )
            hb_itemPutC( pItem, "" );
         break;
      }

      /* numerics ask/ set from server */
      case RDDI_AUTOORDER:
      case RDDI_DEBUGLEVEL:
#if 0   /* ToDo && ! defined( __HARBOUR30__ ) */
      case RDDI_INDEXPAGESIZE:
      case RDDI_SETHEADER:
#endif
      {
         LETOCONNECTION * pConnection;
         int iRes = 1;

         if( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() )
            pConnection = letoGetConnPool( ( HB_USHORT ) ulConnect - 1 );
         else
            pConnection = letoGetCurrConn();

         if( pConnection )
         {
            char *  szNum = NULL;
            HB_BOOL fSet = HB_FALSE;

            if( HB_IS_NUMERIC( pItem ) )
            {
               fSet = HB_TRUE;
               szNum = ( char * ) hb_xgrab( 21 );
               eprintf( szNum, "%d", hb_itemGetNI( pItem ) );
            }
            if( LetoRddInfo( pConnection, uiIndex, szNum ) == HB_SUCCESS )
            {
               const char * ptr = leto_firstchar( pConnection );

               if( *( ptr - 1 ) == '+' )
               {
                  if( uiIndex == RDDI_AUTOORDER )
                     hb_itemPutNL( pItem, strtol( ptr, NULL, 10 ) );
                  else
                     hb_itemPutNL( pItem, strtoul( ptr, NULL, 10 ) );
                  iRes = 0;
                  if( fSet && uiIndex == RDDI_AUTOORDER )
                  {
                     if( hb_itemGetNI( pItem ) < 0 )
                     {
                        PHB_ITEM pItem2 = hb_itemNew( NULL );

                        hb_itemPutL( pItem2, HB_FALSE );
                        hb_setSetItem( HB_SET_AUTOPEN, pItem );
                        hb_itemRelease( pItem2 );

                        hb_itemPutNL( pItem, 0 );
                     }
                     hb_setSetItem( HB_SET_AUTORDER, pItem );
                  }
               }
            }
            if( szNum )
               hb_xfree( szNum );
         }
         if( iRes )
            hb_itemPutNL( pItem, -1 );
         break;
      }

      case RDDI_REFRESHCOUNT:
      {
         LETOCONNECTION * pConnection;

         if( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() )
            pConnection = letoGetConnPool( ( HB_USHORT ) ulConnect - 1 );
         else
            pConnection = letoGetCurrConn();

         if( pConnection )
         {
            HB_BOOL fSet = HB_IS_LOGICAL( pItem );
            HB_BOOL fRefresh = hb_itemGetL( pItem );

            hb_itemPutL( pItem, pConnection->fRefreshCount );
            if( fSet )
               pConnection->fRefreshCount = fRefresh;
         }
         else
            hb_itemPutL( pItem, HB_TRUE );
         break;
      }

      case RDDI_BUFKEYNO:
      case RDDI_BUFKEYCOUNT:
      {
         LETOCONNECTION * pConnection;

         if( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() )
            pConnection = letoGetConnPool( ( HB_USHORT ) ulConnect - 1 );
         else
            pConnection = letoGetCurrConn();

         if( pConnection )
         {
            if( HB_IS_LOGICAL( pItem ) )
            {
               HB_BOOL fValue = hb_itemGetL( pItem );

               LetoSet( pConnection, ( uiIndex == RDDI_BUFKEYNO ? 3 : 4 ), fValue ? "T" : "F" );  /* LETOCMD_set */
               if( uiIndex == RDDI_BUFKEYNO )
                  pConnection->fBufKeyNo = fValue;
               else
                  pConnection->fBufKeyCount = fValue;
            }
            else if( uiIndex == RDDI_BUFKEYNO )
               hb_itemPutL( pItem, pConnection->fBufKeyNo );
            else
               hb_itemPutL( pItem, pConnection->fBufKeyCount );
         }
         else
            hb_itemPutL( pItem, HB_FALSE );
         break;
      }

      case RDDI_VERSION:
         hb_itemPutC( pItem, LETO_VERSION_STRING );
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );
   }

   return HB_SUCCESS;
}

#define letoWhoCares  NULL

static const RDDFUNCS s_letoTable = { ( DBENTRYP_BP ) letoBof,
                                      ( DBENTRYP_BP ) letoEof,
                                      ( DBENTRYP_BP ) letoFound,
                                      ( DBENTRYP_V ) letoGoBottom,
                                      ( DBENTRYP_UL ) letoGoTo,
                                      ( DBENTRYP_I ) letoGoToId,
                                      ( DBENTRYP_V ) letoGoTop,
                                      ( DBENTRYP_BIB ) letoSeek,
                                      ( DBENTRYP_L ) letoSkip,
                                      ( DBENTRYP_L ) letoSkipFilter,
                                      ( DBENTRYP_L ) letoSkipRaw,
                                      ( DBENTRYP_VF ) letoAddField,
                                      ( DBENTRYP_B ) letoAppend,
                                      ( DBENTRYP_I ) letoCreateFields,
                                      ( DBENTRYP_V ) letoDeleteRec,
                                      ( DBENTRYP_BP ) letoDeleted,
                                      ( DBENTRYP_SP ) letoFieldCount,
                                      ( DBENTRYP_VF ) letoFieldDisplay,
                                      ( DBENTRYP_SSI ) letoFieldInfo,
                                      ( DBENTRYP_SCP ) letoFieldName,
                                      ( DBENTRYP_V ) letoFlush,
                                      ( DBENTRYP_PP ) letoGetRec,
                                      ( DBENTRYP_SI ) letoGetValue,
                                      ( DBENTRYP_SVL ) letoGetVarLen,
                                      ( DBENTRYP_V ) letoGoCold,
                                      ( DBENTRYP_V ) letoGoHot,
                                      ( DBENTRYP_P ) letoPutRec,
                                      ( DBENTRYP_SI ) letoPutValue,
                                      ( DBENTRYP_V ) letoRecall,
                                      ( DBENTRYP_ULP ) letoRecCount,
                                      ( DBENTRYP_ISI ) letoRecInfo,
                                      ( DBENTRYP_ULP ) letoRecNo,
                                      ( DBENTRYP_I ) letoRecId,
                                      ( DBENTRYP_S ) letoSetFieldExtent,
                                      ( DBENTRYP_CP ) letoAlias,
                                      ( DBENTRYP_V ) letoClose,
                                      ( DBENTRYP_VO ) letoCreate,
                                      ( DBENTRYP_SI ) letoInfo,
                                      ( DBENTRYP_V ) letoNewArea,
                                      ( DBENTRYP_VO ) letoOpen,
                                      ( DBENTRYP_V ) letoRelease,
                                      ( DBENTRYP_SP ) letoStructSize,
                                      ( DBENTRYP_CP ) letoSysName,
                                      ( DBENTRYP_VEI ) letoEval,
                                      ( DBENTRYP_V ) letoPack,
                                      ( DBENTRYP_LSP ) letoPackRec,
                                      ( DBENTRYP_VS ) letoSort,
                                      ( DBENTRYP_VT ) letoTrans,
                                      ( DBENTRYP_VT ) letoTransRec,
                                      ( DBENTRYP_V ) letoZap,
                                      ( DBENTRYP_VR ) letoChildEnd,
                                      ( DBENTRYP_VR ) letoChildStart,
                                      ( DBENTRYP_VR ) letoChildSync,
                                      ( DBENTRYP_V ) letoSyncChildren,
                                      ( DBENTRYP_V ) letoClearRel,
                                      ( DBENTRYP_V ) letoForceRel,
                                      ( DBENTRYP_SSP ) letoRelArea,
                                      ( DBENTRYP_VR ) letoRelEval,
                                      ( DBENTRYP_SI ) letoRelText,
                                      ( DBENTRYP_VR ) letoSetRel,
                                      ( DBENTRYP_VOI ) letoOrderListAdd,
                                      ( DBENTRYP_V ) letoOrderListClear,
                                      ( DBENTRYP_VOI ) letoOrderListDelete,
                                      ( DBENTRYP_VOI ) letoOrderListFocus,
                                      ( DBENTRYP_V ) letoOrderListRebuild,
                                      ( DBENTRYP_VOO ) letoOrderCondition,
                                      ( DBENTRYP_VOC ) letoOrderCreate,
                                      ( DBENTRYP_VOI ) letoOrderDestroy,
                                      ( DBENTRYP_SVOI ) letoOrderInfo,
                                      ( DBENTRYP_V ) letoClearFilter,
                                      ( DBENTRYP_V ) letoClearLocate,
                                      ( DBENTRYP_V ) letoClearScope,
                                      ( DBENTRYP_VPLP ) letoCountScope,
                                      ( DBENTRYP_I ) letoFilterText,
                                      ( DBENTRYP_SI ) letoScopeInfo,
                                      ( DBENTRYP_VFI ) letoSetFilter,
                                      ( DBENTRYP_VLO ) letoSetLocate,
                                      ( DBENTRYP_VOS ) letoSetScope,
                                      ( DBENTRYP_VPL ) letoSkipScope,
                                      ( DBENTRYP_B ) letoLocate,
                                      ( DBENTRYP_CC ) letoCompile,
                                      ( DBENTRYP_I ) letoError,
                                      ( DBENTRYP_I ) letoEvalBlock,
                                      ( DBENTRYP_VSP ) letoRawLock,
                                      ( DBENTRYP_VL ) letoLock,
                                      ( DBENTRYP_I ) letoUnLock,
                                      ( DBENTRYP_V ) letoCloseMemFile,
                                      ( DBENTRYP_VO ) letoCreateMemFile,
                                      ( DBENTRYP_SCCS ) letoGetValueFile,
                                      ( DBENTRYP_VO ) letoOpenMemFile,
                                      ( DBENTRYP_SCCS ) letoPutValueFile,
                                      ( DBENTRYP_V ) letoReadDBHeader,
                                      ( DBENTRYP_V ) letoWriteDBHeader,
                                      ( DBENTRYP_R ) letoInit,
                                      ( DBENTRYP_R ) letoExit,
                                      ( DBENTRYP_RVVL ) letoDrop,
                                      ( DBENTRYP_RVVL ) letoExists,
#if ! defined( __XHARBOUR__ )
                                      ( DBENTRYP_RVVVL ) letoRename,
#endif
                                      ( DBENTRYP_RSLV ) letoRddInfo,
                                      ( DBENTRYP_SVP ) letoWhoCares };

static void leto_RegisterRDD( HB_USHORT * pusRddId )
{
   HB_USHORT * uiCount = ( HB_USHORT * ) hb_parptr( 1 );
   RDDFUNCS *  pTable = ( RDDFUNCS * ) hb_parptr( 2 );
   HB_USHORT   uiRddId = ( HB_USHORT ) hb_parni( 4 );

   if( pTable )
   {
      HB_ERRCODE errCode;

      if( uiCount )
         *uiCount = RDDFUNCSCOUNT;
      errCode = hb_rddInherit( pTable, &s_letoTable, &s_letoSuper, NULL );
      if( errCode == HB_SUCCESS )
      {
         /*
          * we successfully register our RDD so now we can initialize it
          * You may think that this place is RDD init statement, Druzus
          */
         *pusRddId = uiRddId;
         ++s_uiRddCount;
      }
      hb_retni( errCode );
   }
   else
      hb_retni( HB_FAILURE );
}

HB_FUNC_STATIC( LETO_GETFUNCTABLE )
{
   HB_TRACE( HB_TR_DEBUG, ( "LETO_GETFUNCTABLE()" ) );

   leto_RegisterRDD( &s_uiRddIdLETO );
}

HB_FUNC( LETO_MILLISEC )
{
   hb_retnll( hb_dateMilliSeconds() );
}

HB_FUNC( LETORDD )
{
}

static HB_ERRCODE leto_UpdArea( AREAP pArea, void * p )
{
   if( leto_CheckAreaConn( pArea, ( LETOCONNECTION * ) p ) &&
       ( ( LETOAREAP ) pArea )->pTable->uiUpdated )
   {
      leto_PutRec( ( LETOAREAP ) pArea, HB_FALSE );
   }
   return HB_SUCCESS;
}

/* used by transactions, remove a remaining F-lock at server */
static HB_ERRCODE leto_UnLockRec( AREAP pArea, void * p )
{
   if( leto_CheckAreaConn( pArea, ( LETOCONNECTION * ) p ) &&
       ( ( LETOAREAP ) pArea )->pTable->uiUpdated )
   {
      LETOAREAP pLetoArea = ( LETOAREAP ) pArea;

      pLetoArea->pTable->fRecLocked = HB_FALSE;
      if( pLetoArea->pTable->ulLocksMax || pLetoArea->pTable->fFLocked )
         SELF_RAWLOCK( pArea, FILE_UNLOCK, 0 );
      pLetoArea->pTable->fFLocked = HB_FALSE;
      pLetoArea->pTable->ulLocksMax = 0;
   }
   return HB_SUCCESS;
}

static HB_ERRCODE leto_ClearUpd( AREAP pArea, void * p )
{
   if( leto_CheckAreaConn( pArea, ( LETOCONNECTION * ) p ) &&
       ( ( LETOAREAP ) pArea )->pTable->uiUpdated )
   {
      LETOAREAP pLetoArea = ( LETOAREAP ) pArea;

      leto_SetUpdated( pLetoArea->pTable, LETO_FLAG_UPD_NONE );
      letoGoTo( pLetoArea, pLetoArea->pTable->ulRecNo );
   }
   return HB_SUCCESS;
}

static HB_ERRCODE leto_FindArea( AREAP pArea, void * p )
{
   if( leto_CheckArea( ( LETOAREAP ) pArea ) )
   {
      LETOAREAP      pLetoArea = ( LETOAREAP ) pArea;
      FINDAREASTRU * psArea = ( FINDAREASTRU * ) p;

      if( pLetoArea->pTable->uiConnection == psArea->uiConnection &&
          pLetoArea->pTable->hTable == psArea->ulAreaID )
      {
         psArea->pArea = pLetoArea;
         return HB_FAILURE;  /* to stop iterating */
      }
   }
   return HB_SUCCESS;
}

static HB_BOOL leto_CheckTrans( LETOAREAP pArea, HB_BOOL fActive )
{
   if( ! leto_CheckArea( pArea ) )
      return HB_FALSE;
   else
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );

      return fActive ? pConnection->fTransActive : ! pConnection->fTransActive;
   }
}

HB_FUNC( LETO_BEGINTRANSACTION )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( ! leto_CheckTrans( pArea, HB_FALSE ) )
      commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, NULL );
   else
   {
      LETOTABLE *      pTable = pArea->pTable;
      LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

      hb_rddIterateWorkAreas( leto_UpdArea, ( void * ) pConnection );
      pConnection->ulTransBlockLen = HB_ISNUM( 1 ) ? hb_parnl( 1 ) : 0;
      pConnection->fTransActive = HB_TRUE;
   }
}

HB_FUNC( LETO_ROLLBACK )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( ! leto_CheckTrans( pArea, HB_TRUE ) )
      commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, NULL );
   else
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
      char szData[ 8 ];

      pConnection->fTransActive = HB_FALSE;
      pConnection->ulTransDataLen = pConnection->ulRecsInTrans = 0;
      pConnection->ulRecsInList = 0;
      if( pConnection->szTransBuffer )
      {
         hb_xfree( pConnection->szTransBuffer );
         pConnection->szTransBuffer = NULL;
      }
      if( pConnection->pTransList )
      {
         hb_xfree( pConnection->pTransList );
         pConnection->pTransList = NULL;
      }

      szData[ 0 ] = LETOCMD_ta;
      szData[ 1 ] = ';';
      HB_PUT_LE_UINT32( szData + 2, 0 );
      szData[ 6 ] = ( char ) 0x41;
      szData[ 7 ] = '\0';
      if( leto_SendRecv( pConnection, pArea, szData, 7, 1021 ) )
      {
         if( ! ( HB_ISLOG( 1 ) && ! hb_parl( 1 ) ) )  /* not unlock if explicitely given */
            hb_rddIterateWorkAreas( leto_UnLockRec, ( void * ) pConnection );
         hb_rddIterateWorkAreas( leto_ClearUpd, ( void * ) pConnection );
      }
   }
}

HB_FUNC( LETO_COMMITTRANSACTION )
{
   LETOAREAP        pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   LETOTABLE *      pTable = pArea->pTable;
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

   if( ! leto_CheckTrans( pArea, HB_TRUE ) )
   {
      commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, NULL );
      return;
   }

   hb_rddIterateWorkAreas( leto_UpdArea, ( void * ) pConnection );
   pConnection->fTransActive = HB_FALSE;

   if( pConnection->szTransBuffer && ( pConnection->ulTransDataLen > ( HB_ULONG ) pConnection->uiTBufOffset ) )
   {
      HB_BOOL fUnlockAll = ( HB_ISLOG( 1 ) ) ? hb_parl( 1 ) : HB_TRUE;  /* ToDo: should be ever lUnlockAll */

      HB_PUT_LE_UINT32( pConnection->szTransBuffer + 2, pConnection->ulRecsInTrans );
      pConnection->szTransBuffer[ pConnection->uiTBufOffset - 1 ] = ( HB_BYTE ) ( ( fUnlockAll ) ? 0x41 : 0x40 );

      if( ! leto_SendRecv( pConnection, pArea, ( char * ) pConnection->szTransBuffer, pConnection->ulTransDataLen, 1021 ) )
      {
         if( fUnlockAll )
            hb_rddIterateWorkAreas( leto_UnLockRec, ( void * ) pConnection );
         hb_retl( HB_FALSE );
         return;
      }
      else
      {
         char * pData = leto_firstchar( pConnection );

         if( fUnlockAll )
            hb_rddIterateWorkAreas( leto_UnLockRec, ( void * ) pConnection );
         if( *( pData + 3 ) == ';' )
         {
            int iCount = atoi( pData + 4 );

            pData = strchr( pData + 4, ';' );
            if( pData && iCount )
            {
               HB_ULONG     ulAreaID, ulRecNo, ulLastArea = 0;
               FINDAREASTRU sArea = { 0, 0L, NULL };
               LETOAREAP    pLastArea = NULL;
               char *       pNext;
               int          i;

               for( i = 0; i < iCount; i++ )
               {
                  ulAreaID = strtoul( ++pData, &pNext, 10 );
                  ulRecNo = strtoul( pNext + 1, NULL, 10 );
                  if( ulAreaID && ulRecNo )
                  {
                     if( ulLastArea != ulAreaID || ! pLastArea )
                     {
                        sArea.uiConnection = pTable->uiConnection;
                        sArea.ulAreaID = ulAreaID;
                        sArea.pArea = NULL;
                        hb_rddIterateWorkAreas( leto_FindArea, ( void * ) &sArea );
                        pLastArea = sArea.pArea;
                        ulLastArea = ulAreaID;
                     }
                     if( sArea.pArea && sArea.pArea->pTable->ulRecNo == 0 )
                        sArea.pArea->pTable->ulRecNo = ulRecNo;
                  }
                  pData = strchr( pData, ';' );
                  if( pData == NULL )
                     break;
               }
            }
         }
      }

      if( pConnection->szTransBuffer )
      {
         hb_xfree( pConnection->szTransBuffer );
         pConnection->szTransBuffer = NULL;
      }
      if( pConnection->pTransList )
      {
         hb_xfree( pConnection->pTransList );
         pConnection->pTransList = NULL;
      }
   }

   pConnection->ulRecsInList = 0;
   pConnection->ulTransDataLen = pConnection->ulRecsInTrans = 0;
   hb_retl( HB_TRUE );
}

HB_FUNC( LETO_INTRANSACTION )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();

   hb_retl( leto_CheckTrans( pArea, HB_TRUE ) );
}

static char * leto_AddScopeExp( LETOAREAP pArea, char * pData, int iIndex )
{
   PHB_ITEM  pScope = hb_param( iIndex, HB_IT_ANY );
   HB_USHORT uiKeyLen;
   char      szKey[ LETO_MAX_EXP + 1 ];

   if( pScope )
      uiKeyLen = leto_KeyToStr( pArea, szKey, leto_ItemType( pScope ), pScope, LETO_MAX_EXP );
   else
   {
      uiKeyLen = 0;
      szKey[ 0 ] = '\0';
   }
   *pData++ = ( HB_UCHAR ) ( uiKeyLen ) & 0xFF;
   memcpy( pData, szKey, uiKeyLen );
   pData += uiKeyLen;

   pScope = hb_param( iIndex + 1, HB_IT_ANY );
   if( pScope )
      uiKeyLen = leto_KeyToStr( pArea, szKey, leto_ItemType( pScope ), pScope, LETO_MAX_EXP );
   else
   {
      uiKeyLen = 0;
      szKey[ 0 ] = '\0';
   }
   *pData++ = ( HB_UCHAR ) ( uiKeyLen ) & 0xFF;
   memcpy( pData, szKey, uiKeyLen );
   pData += uiKeyLen;

   *pData = '\0';
   return pData;
}

/* leto_DbEval( cBlock, cFor, cWhile, nNext, nRec, lRest ) */
HB_FUNC( LETO_DBEVAL )
{
   PHB_ITEM * pParams = hb_itemArrayNew( 7 );

   hb_arraySetC( pParams, 1, "LETO_DBEVAL" );

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) > 0 )
   {
      char * szBlock = hb_strdup( hb_parc( 1 ) );

      if( leto_CbTrim( szBlock ) )
         hb_arraySetC( pParams, 2, szBlock );  // eval block each record
      hb_xfree( szBlock );
   }
   else
      hb_arraySetC( pParams, 2, "leto_ResultRow('*')" );
   if( HB_ISCHAR( 2 ) && hb_parclen( 2 ) > 0 )
   {
      char * szBlock = hb_strdup( hb_parc( 2 ) );

      if( leto_CbTrim( szBlock ) )
         hb_arraySetC( pParams, 3, szBlock );  /* FOR */
      hb_xfree( szBlock );
   }
   if( HB_ISCHAR( 3 ) && hb_parclen( 3 ) > 0 )
   {
      char * szBlock = hb_strdup( hb_parc( 3 ) );

      if( leto_CbTrim( szBlock ) )
         hb_arraySetC( pParams, 4, szBlock );  /* WHILE */
      hb_xfree( szBlock );
   }

   hb_arraySetNL( pParams, 5, HB_ISNUM( 4 ) ? ( HB_ULONG ) hb_parnl( 4 ) : 0 );
   hb_arraySetNL( pParams, 6, HB_ISNUM( 5 ) ? ( HB_ULONG ) hb_parnl( 5 ) : 0 );
   hb_arraySetL( pParams, 7, HB_ISLOG( 6 ) ? ( HB_ULONG ) hb_parl( 6 ) : 0 );

   leto_udp( HB_FALSE, pParams );
}

HB_FUNC( LETO_GROUPBY )
{
   LETOAREAP    pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   char         szData[ LETO_MAX_EXP * 2 + LETO_MAX_EXP + LETO_MAX_TAGNAME + 47 ], * pData;
   const char * szGroup = ( HB_ISCHAR( 1 ) ? hb_parc( 1 ) : NULL );
   const char * szField, * szFilter;
   HB_ULONG     ulLen;

   if( leto_CheckArea( pArea ) && szGroup && HB_ISCHAR( 2 ) )
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
      LETOTABLE *      pTable = pArea->pTable;

      if( pTable->uiUpdated )
         leto_PutRec( pArea, HB_FALSE );

      szField = hb_parc( 2 );
      if( strchr( szField, ',' ) == NULL && ! hb_rddFieldIndex( ( AREAP ) pArea, szField ) )
      {
         hb_reta( 0 );
         return;
      }
      szFilter = HB_ISCHAR( 3 ) ? hb_parc( 3 ) : "";

      ulLen = eprintf( szData, "%c;%lu;%s;%s;%s;%s;%c;", LETOCMD_group, pTable->hTable,
                       ( pTable->pTagCurrent ) ? pTable->pTagCurrent->TagName : "",
                       szGroup, szField, szFilter,
                       ( char ) ( ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 ) ) );
      pData = leto_AddScopeExp( pArea, szData + ulLen, 4 );
      ulLen = pData - szData;

      if( ! leto_SendRecv( pConnection, pArea, szData, ulLen, 1020 ) )
         hb_reta( 0 );
      else
      {
         HB_ULONG ulRow, ulIndex;
         int      uiCount, uiIndex, uiGroupLen;
         PHB_ITEM pItem = hb_itemNew( NULL );
         PHB_ITEM pArray = hb_itemNew( NULL );
         PHB_ITEM pSubArray;
         char *   ptr;

         char      cGroupType;
         int       iLen, iWidth, iDec;
         HB_BOOL   fDbl;
         HB_MAXINT lValue;
         double    dValue;

         pData = leto_firstchar( pConnection );

         sscanf( pData, "%lu;", &ulRow );
         pData = strchr( pData, ';' ) + 1;
         sscanf( pData, "%d;", &uiCount );
         pData = strchr( pData, ';' ) + 1;
         sscanf( pData, "%c;", &cGroupType );
         pData = strchr( pData, ';' ) + 1;

         hb_arrayNew( pArray, ulRow );
         for( ulIndex = 1; ulIndex <= ulRow; ulIndex++ )
         {
            hb_arrayNew( hb_arrayGetItemPtr( pArray, ulIndex ), uiCount + 1 );
         }

         ulIndex = 1;
         while( ulIndex <= ulRow )
         {
            pSubArray = hb_arrayGetItemPtr( pArray, ulIndex );
            switch( cGroupType )
            {
               case 'C':
                  uiGroupLen = HB_GET_LE_UINT16( pData );
                  pData += 2;
                  hb_arraySetCL( pSubArray, 1, pData, uiGroupLen );
                  pData += uiGroupLen;
                  break;

               case 'N':
                  ptr = strchr( pData, ',' );
                  iLen = ptr - pData;
                  fDbl = hb_valStrnToNum( pData, iLen, &lValue, &dValue, &iDec, &iWidth );
                  if( fDbl )
                     hb_arraySetND( pSubArray, 1, dValue );
                  else
                     hb_arraySetNInt( pSubArray, 1, lValue );
                  pData = ptr;
                  break;

               case 'D':
                  hb_arraySetDS( pSubArray, 1, pData );
                  pData += 8;
                  break;

               case 'L':
                  hb_arraySetL( pSubArray, 1, ( *pData == 'T' ) );
                  pData++;
                  break;
            }
            if( *pData != ',' )
               break;

            uiIndex = 1;
            while( uiIndex <= uiCount )
            {
               pData++;
               ptr = strchr( pData, uiIndex == uiCount ? ';' : ',' );
               if( ! ptr )
                  break;

               iLen = ptr - pData;
               fDbl = hb_valStrnToNum( pData, iLen, &lValue, &dValue, &iDec, &iWidth );
               if( fDbl )
                  hb_itemPutNDLen( hb_arrayGetItemPtr( pSubArray, uiIndex + 1 ), dValue, iWidth, iDec );
               else
                  hb_arraySetNInt( pSubArray, uiIndex + 1, lValue );

               pData = ptr;
               uiIndex++;
            }

            if( *pData != ';' )
               break;
            pData++;
            ulIndex++;
         }

         hb_itemRelease( pItem );
         hb_itemReturnForward( pArray );

      }
   }
}

HB_FUNC( LETO_SUM )
{
   LETOAREAP    pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   char         szData[ LETO_MAX_EXP * 2 + LETO_MAX_EXP + LETO_MAX_TAGNAME + 35 ], * pData;
   const char * szField, * szFilter;
   HB_ULONG     ulLen;

   if( leto_CheckArea( pArea ) && HB_ISCHAR( 1 ) )
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
      LETOTABLE *      pTable = pArea->pTable;

      if( pTable->uiUpdated )
         leto_PutRec( pArea, HB_FALSE );

      szField = hb_parc( 1 );
      szFilter = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : "";

      ulLen = eprintf( szData, "%c;%lu;%s;%s;%s;%c;", LETOCMD_sum, pTable->hTable,
                       ( pTable->pTagCurrent ) ? pTable->pTagCurrent->TagName : "",
                       szField, szFilter,
                       ( char ) ( ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 ) ) );
      pData = leto_AddScopeExp( pArea, szData + ulLen, 3 );
      ulLen = pData - szData;

      if( ! leto_SendRecv( pConnection, pArea, szData, ulLen, 1020 ) )
         hb_retni( 0 );
      else
      {
         int       iWidth, iDec, iLen;
         HB_BOOL   fDbl;
         HB_MAXINT lValue;
         double    dValue;
         char *    ptr;

         pData = leto_firstchar( pConnection );

         if( strchr( pData, ',' ) == NULL )
         {
            ptr = strchr( pData, ';' );
            iLen = ( ptr ? ptr - pData : ( int ) strlen( pData ) );
            fDbl = hb_valStrnToNum( pData, iLen, &lValue, &dValue, &iDec, &iWidth );
            if( ! fDbl )
               hb_retnintlen( lValue, iWidth );
            else
               hb_retnlen( dValue, iWidth, iDec );
         }
         else
         {
            PHB_ITEM pArray = hb_itemNew( NULL );
            PHB_ITEM pItem = hb_itemNew( NULL );

            hb_arrayNew( pArray, 0 );

            while( 1 )
            {
               ptr = strchr( pData, ',' );
               if( ! ptr )
                  ptr = strchr( pData, ';' );
               iLen = ( ptr ? ptr - pData : ( int ) strlen( pData ) );
               fDbl = hb_valStrnToNum( pData, iLen, &lValue, &dValue, &iDec, &iWidth );

               hb_arrayAddForward( pArray, ( fDbl ? hb_itemPutNDLen( pItem, dValue, iWidth, iDec ) : hb_itemPutNInt( pItem, lValue ) ) );

               if( ! ptr || ( *ptr == ';' ) )
                  break;
               pData = ptr + 1;
            }
            hb_itemRelease( pItem );
            hb_itemReturnForward( pArray );
         }
      }
   }
   else
      hb_retni( 0 );
}

HB_FUNC( LETO_COMMIT )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( leto_CheckArea( pArea ) )
   {
      LETOTABLE * pTable = pArea->pTable;

      if( letoGetConnPool( pTable->uiConnection )->fTransActive )
         commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, NULL );
      else if( pTable->uiUpdated )
      {
         leto_PutRec( pArea, HB_TRUE );
         pTable->fFLocked = pTable->fRecLocked = HB_FALSE;
         pTable->ulLocksMax = 0;
      }
      else
      {
         if( LetoDbCommit( pTable ) )
            commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, NULL );
         if( letoUnLock( pArea, NULL ) == HB_FAILURE )
            commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, NULL );
      }
   }
}

HB_FUNC( LETO_PARSEREC )
{
   LETOAREAP   pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   LETOTABLE * pTable = pArea->pTable;

   if( leto_CheckArea( pArea ) && HB_ISCHAR( 1 ) && hb_parclen( 1 )  )
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

      leto_ParseRec( pConnection, pArea, hb_parc( 1 ) );
      pTable->ptrBuf = NULL;
   }
   hb_ret();
}

#if 0  /* this concept is put on dry ice because of strong design problems */
HB_FUNC( LETO_PARSERECORDS )
{
   LETOAREAP   pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   LETOTABLE * pTable = pArea->pTable;

   if( leto_CheckArea( pArea ) && HB_ISCHAR( 1 ) )
   {
      const char * pBuffer = hb_parc( 1 );
      HB_ULONG     ulDataLen;

      if( ( ulDataLen = hb_parclen( 1 ) ) > 0 )
      {
         pTable->BufDirection = 1;

         //ulDataLen = leto_b2n( pBuffer + 1, uLenLen );
         //ptr = pBuffer + 2 + uLenLen;
         leto_ParseRec( pArea, pBuffer );

         leto_setSkipBuf( pTable, pBuffer, ulDataLen, 0 );
         pTable->ptrBuf = pTable->Buffer.pBuffer;
      }
   }
   hb_ret();
}
#endif

HB_FUNC( LETO_CONNECT_ERR )
{
   HB_BOOL fAsText = hb_parldef( 1, HB_FALSE );
   int     iResult = LetoGetConnectRes();

   if( ! fAsText )
      hb_retni( iResult );
   else
   {
      const char * szResult;
      switch( iResult )
      {
         case LETO_ERR_CONNECT:
            szResult = "connection error";
            break;
         case LETO_ERR_LOGIN:
            szResult = "login failed";
            break;
         case LETO_ERR_ACCESS:
            szResult = "access error";
            break;
         case LETO_ERR_RECV:
            szResult = "receive error";
            break;
         case LETO_ERR_SEND:
            szResult = "send error";
            break;
         case LETO_ERR_MANY_CONN:
            szResult = "too many connection";
            break;
         case LETO_ERR_SOCKET:
            szResult = "socket error";
            break;
         case LETO_ERR_PROTO:
            szResult = "protocol error";
            break;
         case LETO_ERR_LOCKED:
            szResult = "server locked";
            break;
         default:
            szResult = "unknown error";  /* ;-) */
            break;
      }
      hb_retc( szResult );
   }
}

HB_FUNC( LETO_ISFLTOPTIM )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( leto_CheckArea( pArea ) )
#ifndef __BM
      hb_retl( pArea->area.dbfi.fOptimized );
#else
      hb_retl( pArea->area.dbfi.fOptimized || pArea->area.dbfi.lpvCargo );
#endif
   else
      hb_retl( HB_FALSE );
}

/* deprecated */
HB_FUNC( LETO_SETFASTAPPEND )
{
   hb_retl( HB_FALSE );
}

LETOCONNECTION * leto_getConnection( int iParam )
{
   LETOCONNECTION * pConnection = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "leto_getConnection()" ) );

   if( HB_ISCHAR( iParam ) )
   {
      char * szAddr = ( char * ) hb_xgrabz( 96 );
      int    iPort = 0;

      if( leto_getIpFromPath( hb_parc( iParam ), szAddr, &iPort, NULL, HB_FALSE ) )
         pConnection = leto_ConnectionFind( szAddr, iPort );
      hb_xfree( szAddr );
      if( ! pConnection )
         pConnection = letoGetCurrConn();
   }
   else if( HB_ISNUM( iParam ) )
   {
      HB_USHORT uiConn = ( HB_USHORT ) hb_parni( iParam );

      if( uiConn > 0 && uiConn <= uiGetConnCount() )
         pConnection = letoGetConnPool( uiConn - 1 );
      else
         pConnection = letoGetCurrConn();
   }
   else
      pConnection = letoGetCurrConn();

   return pConnection;
}

HB_FUNC( LETO_CONNECTINFO )
{
   LETOCONNECTION * pConnection = NULL;
   int iParam = HB_ISNUM( 1 ) ? 1 : 0;

   if( iParam > 0 )
      pConnection = leto_getConnection( iParam );
   if( ! pConnection )
      pConnection = letoGetCurrConn();
   if( pConnection )
   {
      PHB_ITEM pInfoArr = hb_itemArrayNew( 6 );

      hb_arraySetNI( pInfoArr, 1, pConnection->uiDriver );
      hb_arraySetNI( pInfoArr, 2, leto_LockScheme( pConnection ) );
      hb_arraySetNI( pInfoArr, 3, pConnection->uiMemoType );
      hb_arraySetNI( pInfoArr, 4, pConnection->uiMemoBlocksize );
      hb_arraySetNI( pInfoArr, 5, pConnection->iServerPort );
      hb_arraySetC( pInfoArr, 6, pConnection->szDriver );
      hb_itemReturnRelease( pInfoArr );
   }
}

HB_FUNC( LETO_DBDRIVER )
{
   LETOCONNECTION * pConnection = NULL;
   const char *     szDriver = HB_ISCHAR( 1 ) ? hb_parc( 1 ) : NULL;
   const char *     szMemoType = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : NULL;
   int              iMemoBlocksize = HB_ISNUM( 3 ) ? hb_parni( 3 ) : 0;
   int              iParam = HB_ISNUM( 4 ) ? 4 : 0;

   if( iParam > 0 )
      pConnection = leto_getConnection( iParam );
   if( ! pConnection )
      pConnection = letoGetCurrConn();

   if( pConnection )
   {
      PHB_ITEM pInfo = hb_itemArrayNew( 3 );
      HB_BOOL  fDriverChanged = HB_FALSE;

      if( szDriver )
      {
         int iDriver;

         if( ! strcmp( szDriver, "DBFNTX" ) || ! strcmp( szDriver, "BMDBFNTX" ) )
            iDriver = 1;
         else if( ! strcmp( szDriver, "SIXCDX" ) )
            iDriver = 0;
         else if( ! strcmp( szDriver, "DBFCDX" ) || ! strcmp( szDriver, "BMDBFCDX" ) )
            iDriver = 0;
         else if( ! strcmp( szDriver, "DBFNSX" ) || ! strcmp( szDriver, "BMDBFNSX" ) )
            iDriver = 0;
         else
            iDriver = -1;

         if( iDriver >= 0 )
         {
            if( strcmp( pConnection->szDriver, szDriver ) )
            {
               fDriverChanged = HB_TRUE;
               strcpy( pConnection->szDriver, szDriver );
               pConnection->uiDriver = ( HB_USHORT ) iDriver;  /* internal use, indicate NTX == 1 versus somthing else */
            }
         }
      }

      if( szMemoType || fDriverChanged )
      {
         int iMemoType;

         if( ! szMemoType )
         {
            if( pConnection->uiDriver )
            {
               iMemoType = DB_MEMO_DBT;
               memcpy( pConnection->szMemoExt, ".dbt\0", 5 );
            }
            else
            {
               iMemoType = DB_MEMO_FPT;
               memcpy( pConnection->szMemoExt, ".fpt\0", 5 );
            }
         }
         else if( ! strcmp( szMemoType, "DBT" ) )
         {
            iMemoType = DB_MEMO_DBT;
            memcpy( pConnection->szMemoExt, ".dbt\0", 5 );
         }
         else if( ! strcmp( szMemoType, "FPT" ) )
         {
            iMemoType = DB_MEMO_FPT;
            memcpy( pConnection->szMemoExt, ".fpt\0", 5 );
         }
         else if( ! strcmp( szMemoType, "SMT" ) )
         {
            iMemoType = DB_MEMO_SMT;
            memcpy( pConnection->szMemoExt, ".smt\0", 5 );
         }
         else
            iMemoType = -1;

         if( iMemoType >= 0 )
         {
            pConnection->uiMemoType = ( HB_USHORT ) iMemoType;

            if( ! iMemoBlocksize || fDriverChanged )
            {
               if( ! ( iMemoBlocksize > 0 && iMemoBlocksize <= 0xFFFF && iMemoBlocksize % 32 == 0 ) )
               {
                  if( iMemoType == DB_MEMO_DBT )
                     iMemoBlocksize = 512;
                  else if( iMemoType == DB_MEMO_FPT )
                     iMemoBlocksize = 64;
                  else if( iMemoType == DB_MEMO_SMT )
                     iMemoBlocksize = 32;
               }
               pConnection->uiMemoBlocksize = ( HB_USHORT ) iMemoBlocksize;
            }
            else if( iMemoBlocksize > 0 && iMemoBlocksize <= 0xFFFF && iMemoBlocksize % 32 == 0 )
               pConnection->uiMemoBlocksize = ( HB_USHORT ) iMemoBlocksize;
         }
      }

      hb_arraySetC( pInfo, 1, pConnection->szDriver );
      if( pConnection->uiMemoType == DB_MEMO_DBT )
         hb_arraySetC( pInfo, 2, "DBT" );
      else if( pConnection->uiMemoType == DB_MEMO_FPT )
         hb_arraySetC( pInfo, 2, "FPT" );
      else if( pConnection->uiMemoType == DB_MEMO_SMT )
         hb_arraySetC( pInfo, 2, "SMT" );
      else
         hb_arraySetC( pInfo, 2, "???" );
      hb_arraySetNI( pInfo, 3, pConnection->uiMemoBlocksize );

      hb_itemReturnRelease( pInfo );
      return;
   }

   hb_ret();
}

HB_FUNC( LETO_MEMOISEMPTY )
{
   HB_BOOL fEmpty = HB_TRUE;  /* return HB_TRUE for all param errors and also not memofields */
   int     iArea = 0;

   if( HB_ISCHAR( 1 ) )
   {
      const char * szAlias = hb_parc( 1 );

      if( hb_rddVerifyAliasName( szAlias ) == HB_SUCCESS )
         hb_rddGetAliasNumber( szAlias, &iArea );
   }
   else if( HB_ISNUM( 1 ) )
      iArea = hb_parni( 1 );

   if( iArea > 0 )
   {
      LETOAREAP pArea = ( LETOAREAP ) hb_rddGetWorkAreaPointer( iArea );

      if( pArea )
      {
         LETOTABLE * pTable;
         LPFIELD     pField;
         HB_USHORT   uiFieldPos;

         if( HB_ISCHAR( 2 ) )
            uiFieldPos = hb_rddFieldIndex( ( LPAREA ) pArea, hb_parc( 2 ) );
         else if( HB_ISNUM( 2 ) )
            uiFieldPos = ( HB_USHORT ) hb_parni( 2 );
         else
            uiFieldPos = 0;

         pTable = pArea->pTable;
         if( pTable && uiFieldPos && uiFieldPos <= pTable->uiFieldExtent )
         {
            uiFieldPos--;
            pField = pArea->area.lpFields + uiFieldPos;
            switch( pField->uiType )
            {
               case HB_FT_MEMO:
               case HB_FT_BLOB:
               case HB_FT_PICTURE:
               case HB_FT_OLE:
                  if( pField->uiLen == 4 )
                     fEmpty = HB_GET_LE_UINT32( &pTable->pRecord[ pTable->pFieldOffset[ uiFieldPos ] ] ) == 0;
                  else  /* empty if the rightmost char is a whitespace */
                     fEmpty = ( pTable->pRecord[ pTable->pFieldOffset[ uiFieldPos ] + pField->uiLen - 1 ] == ' ' );
                  break;
               default:
                  fEmpty = HB_TRUE;
            }
         }
      }
   }

   hb_retl( fEmpty );
}

HB_FUNC( LETO_CLOSEALL )
{
   LETOCONNECTION * pConnection = leto_getConnection( 1 );

   if( pConnection )
      hb_retl( leto_CloseAll( pConnection ) );
   else
      hb_retl( HB_FALSE );
}

/* To FIX: ?really?, only lock at client side in pLocksPos array ?? */
HB_FUNC( LETO_RECLOCKLIST )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( leto_CheckArea( pArea ) && HB_ISARRAY( 1 ) )
   {
      LETOTABLE * pTable = pArea->pTable;
      PHB_ITEM    pArray = hb_param( 1, HB_IT_ARRAY );
      HB_SIZE     nLen = hb_arrayLen( pArray ), nIndex;

      if( ! pTable->pLocksPos )
      {
         /* Allocate locks array for the table, if it isn't allocated yet */
         pTable->ulLocksAlloc = nLen;
         pTable->pLocksPos = ( HB_ULONG * ) hb_xgrab( sizeof( HB_ULONG ) * pTable->ulLocksAlloc );
         pTable->ulLocksMax = 0;
      }
      else if( pTable->ulLocksMax + nLen > pTable->ulLocksAlloc )
      {
         pTable->ulLocksAlloc = pTable->ulLocksMax + nLen;
         pTable->pLocksPos = ( HB_ULONG * ) hb_xrealloc( pTable->pLocksPos, sizeof( HB_ULONG ) * ( pTable->ulLocksAlloc ) );
      }

      for( nIndex = 1; nIndex <= nLen; nIndex++ )
      {
         leto_AddRecLock( pTable, hb_arrayGetNL( pArray, nIndex ) );
      }
   }
   hb_ret();
}

HB_FUNC( LETO_ERRORCODE )
{
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( pCurrentConn )
      hb_retni( pCurrentConn->iErrorCode );
   else
      hb_retni( 0 );
}

HB_FUNC( LETO_SETSKIPBUFFER )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   LETOCONNECTION * pCurrentConn = letoGetCurrConn();

   if( leto_CheckArea( pArea ) )
   {
      if( HB_ISNUM( 1 ) )
      {
         HB_USHORT uiNum = ( HB_USHORT ) hb_parni( 1 );

         if( uiNum < 1 )
            uiNum = 1;
         if( pArea->pTable->uiRecordLen * uiNum > LETO_MAX_RECV_BLOCK )
            uiNum = ( HB_USHORT ) ( LETO_MAX_RECV_BLOCK / pArea->pTable->uiRecordLen );
         if( pCurrentConn )
         {
            char szTmp[ 42 ];

            eprintf( szTmp, "%lu;%d", pArea->pTable->hTable, uiNum );
            LetoSet( pCurrentConn, 2, szTmp );  /* LETOCMD_set */
            hb_retni( uiNum );
         }
         else
            hb_retni( 0 );
      }
      else
         hb_retni( pArea->pTable->Buffer.uiShoots );
   }
   else
      hb_retni( 0 );
}

/* deprecated */
HB_FUNC( LETO_SETSEEKBUFFER )
{
   hb_retni( 0 );
}

#ifdef __BM
HB_FUNC( LETO_SETBM )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();

   pArea->area.dbfi.lpvCargo = ( void * ) 1;
   leto_ClearBuffers( pArea->pTable );
}
#endif

/* for debug / optimizing purpose */
HB_FUNC( LETO_SIZE )
{
   int iValue = hb_parni( 1 );

   switch( iValue )
   {
      case 1:
         hb_retni( sizeof( LETOBUFFER ) );  // 40
         break;
      case 2:
         hb_retni( sizeof( LETOFIELD ) );  // 20
         break;
      case 3:
         hb_retni( sizeof( LETOTAGINFO ) ); // 160 -20
         break;
      case 4:
         hb_retni( sizeof( LETOTABLE ) ); // 344 - 44
         break;
      case 5:
         hb_retni( sizeof( LETOCONNECTION ) ); // 352 -44
         break;
   }
}

#if 0
HB_FUNC( LETO_ALIAS )
{
   if( hb_parclen( 1 ) )
   {
      char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

      memcpy( szAlias, hb_parc( 1 ), HB_MIN( HB_RDD_MAX_ALIAS_LEN, hb_parclen( 1 ) ) );
      szAlias[ HB_MIN( HB_RDD_MAX_ALIAS_LEN, hb_parclen( 1 ) ) ] = '\0';
      hb_retc( szAlias );
      return;
   }

   hb_retc_null();
}
#endif

HB_FUNC( LETO_HASH )
{
   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) )
   {
      const char * szKey = hb_parc( 1 );
      HB_U32 uRet = leto_hash( szKey, hb_parclen( 1 ) );

      if( HB_ISLOG( 2 ) && hb_parl( 2 ) )
         hb_retc( szKey );
      else
         hb_retni( uRet );
   }
   else
      hb_retni( 0 );
}

static void hb_letoRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "LETO", RDT_FULL ) > 1 )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
   }
}

HB_INIT_SYMBOLS_BEGIN( leto1__InitSymbols )
{
   "LETORDD", { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( LETORDD ) }, NULL
},
{ "LETO_GETFUNCTABLE", { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( LETO_GETFUNCTABLE ) }, NULL },
HB_INIT_SYMBOLS_END( leto1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_leto_rdd_init_ )
hb_vmAtInit( hb_letoRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_leto_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup leto1__InitSymbols
   #pragma startup _hb_leto_rdd_init_
#elif defined( HB_MSC_STARTUP )  // support for old Harbour version
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto_leto1__InitSymbols = leto1__InitSymbols;
   static HB_$INITSYM hb_vm_auto_leto_rdd_init = _hb_leto_rdd_init_;
   #pragma data_seg()
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( leto1__InitSymbols ) \
   HB_DATASEG_FUNC( _hb_leto_rdd_init_ )
   #include "hbiniseg.h"
#endif

