/*
 * Harbour Leto RDD
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 *           2015-17 Rolf 'elch' Beckmann
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
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "commonError(%p, %d, %d, %d, '%s', %d)", pArea, uiGenCode, uiSubCode, uiOsCode, szFileName, uiFlags ) );

   if( hb_vmRequestQuery() == 0 )  /* avoid more errormessages, we want to quit */
   {
      LETOCONNECTION * pCurrentConn = letoGetCurrConn();
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
      if( pCurrentConn )
         pCurrentConn->iError = 0;
      hb_itemRelease( pError );
   }

   return errCode;
}

static HB_LONG leto_SendRecv( LETOCONNECTION * pConnection, LETOAREAP pArea, const char * sData, HB_ULONG ulLen, int iErr )
{
   HB_LONG lRet = leto_DataSendRecv( pConnection, sData, ulLen );
   char *  ptr = pConnection->szBuffer;

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
      lRet = 0;
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
   if( pConnection && pConnection->pAddr )
   {
      leto_CloseAll( pConnection );
      LetoConnectionClose( pConnection );
   }
}

static HB_BOOL leto_CheckError( LETOAREAP pArea, LETOCONNECTION * pConnection )
{
   const char * ptr = pConnection->szBuffer;

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

static _HB_INLINE_ void leto_PutRec( LETOAREAP pArea )
{
   if( LetoDbPutRecord( pArea->pTable ) != 0 )
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );

      if( pConnection->iError )
         commonError( pArea, pConnection->iError == 1000 ? EG_SYNTAX : EG_DATATYPE, pConnection->iError, 0, NULL, 0, NULL );
      else
         leto_CheckError( pArea, pConnection );
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
         pKey = ptr + uiLen;
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
   HB_TRACE( HB_TR_DEBUG, ( "letoFound(%p, %p)", pArea, pFound ) );

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
      leto_PutRec( pArea );

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
      leto_PutRec( pArea );

   if( LetoDbGoTo( pTable, ( HB_ULONG ) ulRecNo ) )
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
      leto_PutRec( pArea );

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
         uiKeyLen = ( HB_USHORT ) strlen( szKey );  /* 23 + '\0' */
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

   HB_TRACE( HB_TR_DEBUG, ( "letoSeek(%p, %d, %p, %d)", pArea, ( int ) fSoftSeek, pKey, ( int ) fFindLast ) );

   pArea->lpdbPendingRel = NULL;
   if( pTable->uiUpdated )
      leto_PutRec( pArea );

   if( ! pTagInfo )
   {
      commonError( pArea, EG_NOORDER, 1201, 0, NULL, EF_CANDEFAULT, NULL );
      return HB_FAILURE;
   }

   /* ToDo timestamp behaviour for '0' and "" */
   cType = leto_ItemType( pKey );
   if( cType != pTagInfo->cKeyType )
      return SELF_GOTO( ( AREAP ) pArea, 0 );
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
      leto_PutRec( pArea );

   if( LetoDbSkip( pTable, lToSkip ) == HB_SUCCESS )
      leto_SetAreaFlags( pArea );
   else
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
      int iErr;

      if( pConnection->iError != 1000 )
      {
         const char * ptr = leto_firstchar( pConnection );

         iErr = atoi( ptr );
      }
      else
         iErr = pConnection->iError;
      if( iErr )
         commonError( pArea, iErr == 1000 ? EG_SYNTAX : EG_DATATYPE, iErr, 0, NULL, 0, "SKIP FAILED" );
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

static void leto_AddRecLock( LETOTABLE * pTable, HB_ULONG ulRecNo )
{
   HB_ULONG ulPos = 0;

   /* [re-]allocate locks array for the table */
   if( ! pTable->pLocksPos )
   {
      pTable->ulLocksAlloc = 64;
      pTable->pLocksPos = ( HB_ULONG * ) hb_xgrab( sizeof( HB_ULONG ) * pTable->ulLocksAlloc );
      pTable->ulLocksMax = 0;
   }
   else if( pTable->ulLocksMax == pTable->ulLocksAlloc )
   {
      pTable->pLocksPos = ( HB_ULONG * ) hb_xrealloc( pTable->pLocksPos, sizeof( HB_ULONG ) * ( pTable->ulLocksAlloc + 64 ) );
      pTable->ulLocksAlloc += 64;
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

static void leto_DelRecLock( LETOTABLE * pTable, HB_ULONG ulRecNo )
{
   HB_ULONG ulPos;

   for( ulPos = 0; ulPos < pTable->ulLocksMax; ulPos++ )
   {
      if( pTable->pLocksPos[ ulPos ] == ulRecNo )
      {
         pTable->ulLocksMax--;
         if( ulPos < pTable->ulLocksMax )
         {
            HB_ULONG * pList = pTable->pLocksPos + ulPos;

            memmove( pList, pList + 1, ( pTable->ulLocksMax - ulPos ) * sizeof( HB_ULONG ) );
         }
         break;
      }
   }
}

static HB_ERRCODE letoAppend( LETOAREAP pArea, HB_BOOL fUnLockAll )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoAppend(%p, %d)", pArea, ( int ) fUnLockAll ) );

   if( pTable->fReadonly )
   {
      commonError( pArea, EG_READONLY, EDBF_READONLY, 0, NULL, 0, NULL );
      return HB_FAILURE;
   }
   pArea->lpdbPendingRel = NULL;

   if( pTable->uiUpdated )
      leto_PutRec( pArea );
   pArea->area.fBof = pArea->area.fEof = pArea->area.fFound = HB_FALSE;

   if( LetoDbAppend( pTable, fUnLockAll ) )
      return HB_FAILURE;

   if( pTable->ulRecNo )  /* ! pConnection->fTransActive */
   {
      if( fUnLockAll )
         pTable->ulLocksMax = 0;
      if( pTable->fRecLocked )
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

#ifndef __HARBOUR30__
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
#endif
      return SUPER_FIELDINFO( &pArea->area, uiIndex, uiType, pItem );
}

#define letoFieldName     NULL

static HB_ERRCODE letoFlush( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoFlush(%p)", pArea ) );

   if( pTable->uiUpdated )
   {
      pTable->uiUpdated |= LETO_FLAG_UPD_FLUSH;
      leto_PutRec( pArea );
      return HB_SUCCESS;
   }

   if( ! letoGetConnPool( pTable->uiConnection )->fTransActive )
   {
      if( LetoDbCommit( pTable ) )
         return HB_FAILURE;
   }

   return HB_SUCCESS;
}

static HB_ERRCODE letoGetRec( LETOAREAP pArea, HB_BYTE ** pBuffer )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoGetRec(%p, %p)", pArea, pBuffer ) );

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

   /* automatic refresh data if record is accessible to other; 0 == infinite cache */
   if( pTable->fAutoRefresh && pTable->fShared && ! pTable->fFLocked && ! pTable->fRecLocked && ! pTable->fReadonly )
   {
      if( pTable->iBufRefreshTime && LETO_CENTISEC() - pTable->llCentiSec >= pTable->iBufRefreshTime )
         LetoDbSkip( pTable, 0 );
   }

   pField = pArea->area.lpFields + --uiIndex;
   switch( pField->uiType )
   {
      case HB_FT_STRING:
         if( ! pField->uiFlags && pArea->area.cdPage != HB_CDP_PAGE() )
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
         HB_BOOL fDbl;
         double  dVal;

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
         break;
      }

      case HB_FT_DATE:
         if( pField->uiLen == 3 )
            hb_itemPutDL( pItem, HB_GET_LE_UINT24( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ) );
         else if( pField->uiLen == 4 )
            hb_itemPutDL( pItem, HB_GET_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ) );
         else
            hb_itemPutDS( pItem, ( char * ) pTable->pRecord + pTable->pFieldOffset[ uiIndex ] );
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

      case HB_FT_FLOAT:
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

   HB_TRACE( HB_TR_DEBUG, ( "letoPutRec(%p, %p)", pArea, pBuffer ) );

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
      if( pTable->fHaveAutoinc && ! pArea->fTransRec )
      {
         HB_USHORT uiField = 0;

         while( uiField < pArea->area.uiFieldCount )
         {
            if( ! ( ( ( LETOFIELD * ) ( pTable->pFields + uiField ) )->uiFlags & HB_FF_AUTOINC ) )
               pTable->pFieldUpd[ uiField ] = 1;
            uiField++;
         }
      }
      else
         memset( pTable->pFieldUpd, 1, pArea->area.uiFieldCount * sizeof( HB_UCHAR ) );
   }
   return HB_SUCCESS;
}

static HB_ERRCODE leto_PutMemoValue( LETOAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem, HB_USHORT uiFlags )
{
   const char * ptr = hb_itemGetCPtr( pItem );
   char *       pBuff = NULL;
   HB_ERRCODE   errCode;
#if defined ( __XHARBOUR__ )
   HB_ULONG     ulLenMemo = hb_itemGetCLen( pItem );
#else
   HB_SIZE      ulLenMemo = hb_itemGetCLen( pItem );
#endif

   if( ! uiFlags && pArea->area.cdPage != HB_CDP_PAGE() )  /* only possible for HB_FT_MEMO without field flags */
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
   errCode = LetoDbPutMemo( pArea->pTable, uiIndex, ptr, ulLenMemo );
   if( pBuff )
      hb_xfree( pBuff );
   return errCode;
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

            if( ! pField->uiFlags && pArea->area.cdPage != HB_CDP_PAGE() )
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
               return leto_PutMemoValue( pArea, uiIndex, pItem, pField->uiType == HB_FT_MEMO ? pField->uiFlags : HB_FF_BINARY );
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
               HB_LONG lDate, lTime;

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
            HB_LONG lDate, lTime;

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
               return leto_PutMemoValue( pArea, uiIndex, pItem, 0 );
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

   if( LetoDbRecCount( pArea->pTable, pRecCount ) )
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
            LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

            if( pConnection->iError )
               commonError( pArea, pConnection->iError == 1038 ? EG_DATATYPE : EG_SYNTAX, pConnection->iError, 0,
                            pConnection->iError == 1038 ? pConnection->szBuffer : NULL, 0, NULL );
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
      leto_PutRec( pArea );

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
   HB_ULONG   ulRecNo;
   HB_ERRCODE errCode = SELF_RECNO( ( AREAP ) pArea, &ulRecNo );

   HB_TRACE( HB_TR_DEBUG, ( "letoRecId(%p, %p)", pArea, pRecNo ) );

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
      leto_PutRec( pArea );

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
      {
         LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

         commonError( pArea, EG_SYNTAX, pConnection->iError, 0, NULL, 0, NULL );
      }
      pArea->pTable = NULL;
   }

   if( pArea->szDataFileName )
   {
      hb_xfree( pArea->szDataFileName );
      pArea->szDataFileName = NULL;
   }

   return HB_SUCCESS;
}

static LETOCONNECTION * leto_OpenConn( LETOCONNECTION * pConnection, const char * szParam, char * szFile )
{
   HB_USHORT        uiPathLen;
   HB_BOOL          fTwister = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "leto_OpenConn(%p, %s, szFile)", pConnection, szParam ? szParam : "(null)" ) );

   szFile[ 0 ] = '\0';

   if( ! pConnection )
   {
      if( ! szParam || ! *szParam )
         pConnection = letoGetCurrConn();
      else if( strlen( szParam ) >= 5 && szParam[ 0 ] == '/' && szParam[ 1 ] == '/' )
      {
         char szAddr[ 96 ];
         int  iPort = 0;

         if( leto_getIpFromPath( szParam, szAddr, &iPort, szFile ) )
         {
            pConnection = leto_ConnectionFind( szAddr, iPort );
            if( ! pConnection )
               pConnection = LetoConnectionNew( szAddr, iPort, NULL, NULL, 0, HB_FALSE );
         }
         else
            pConnection = letoGetCurrConn();
         szParam = leto_RemoveIpFromPath( szParam );
      }
      else
      {
         pConnection = letoGetCurrConn();
         fTwister = HB_TRUE;
      }
   }
   else
      fTwister = HB_TRUE;

   if( fTwister )
      strcpy( szFile, leto_RemoveIpFromPath( szParam ) );

   uiPathLen = ( HB_USHORT ) strlen( szFile );
   if( uiPathLen )
      leto_BeautifyPath( szFile );  /* possible double action */
   else
   {
      strcpy( szFile, leto_RemoveIpFromPath( szParam ) );
      leto_BeautifyPath( szFile );
   }

   return pConnection;
}

static LETOCONNECTION * leto_OpenConnection( LETOAREAP pArea, LPDBOPENINFO pOpenInfo, char * szFile, HB_BOOL fCreate )
{
   LETOCONNECTION * pConnection;
   int              iPort = 0;

   HB_TRACE( HB_TR_DEBUG, ( "leto_OpenConnection(%p, %p, szFile, %d)", pArea, pOpenInfo, ( int ) fCreate ) );

   szFile[ 0 ] = '\0';

   if( pOpenInfo->ulConnection > 0 && pOpenInfo->ulConnection <= ( HB_ULONG ) uiGetConnCount() )
      pConnection = letoGetConnPool( ( HB_USHORT ) pOpenInfo->ulConnection - 1 );
   else
   {
      char szAddr[ 96 ];

      if( ! leto_getIpFromPath( pOpenInfo->abName, szAddr, &iPort, NULL ) )
         pConnection = letoGetCurrConn();
      else
      {
         pConnection = leto_ConnectionFind( szAddr, iPort );
         if( ! pConnection )
            pConnection = LetoConnectionNew( szAddr, iPort, NULL, NULL, 0, HB_FALSE );
      }
   }

   if( pOpenInfo->abName )
   {
      strcpy( szFile, leto_RemoveIpFromPath( pOpenInfo->abName ) );
      leto_BeautifyPath( szFile );
   }

   if( ! pConnection )
   {
      commonError( pArea, EG_OPEN, ( fCreate ? 1 : 101 ), 0, *szFile ? szFile : "? FILE ?", 0, "CONNECTION ERROR" );
      return NULL;
   }

   return pConnection;
}

static void letoCreateAlias( const char * szFile, char * szAlias )
{
   const char * ptrBeg, * ptrEnd;
   HB_USHORT    uLen;

   ptrEnd = strrchr( szFile, '.' );
   if( ptrEnd == NULL )
       ptrEnd = szFile + strlen( szFile );

   ptrBeg = strchr( szFile, ':' );
   if( ptrBeg )  /* mem:... */
      ptrBeg++;
   else
   {
      ptrBeg = strrchr( szFile, '/' );
      if( ! ptrBeg )
         ptrBeg = strrchr( szFile, '\\' );
      if( ! ptrBeg )
         ptrBeg = szFile;
      else
         ptrBeg++;
   }
   uLen = ( HB_USHORT ) ( ptrEnd - ptrBeg );
   if( uLen > HB_RDD_MAX_ALIAS_LEN )
      uLen = HB_RDD_MAX_ALIAS_LEN;
   strncpy( szAlias, ptrBeg, uLen );
   szAlias[ uLen ] = '\0';
   hb_strUpper( szAlias, uLen );
}

static HB_ERRCODE letoCreate( LETOAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   LETOCONNECTION * pConnection;
   LETOTABLE *      pTable;
   LPFIELD          pField;
   HB_USHORT        uiCount = 0;
   HB_ULONG         ulFieldDups;
   char *           szData, * ptr, * szFieldDup;
   char             szFile[ HB_PATH_MAX ];
   char             cType;
   const char *     szFieldName;
   HB_ERRCODE       errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "letoCreate(%p, %p)", pArea, pCreateInfo ) );

   if( ( pConnection = leto_OpenConnection( pArea, pCreateInfo, szFile, HB_TRUE ) ) == NULL )
   {
      hb_rddSetNetErr( HB_TRUE );
      return HB_FAILURE;
   }
   pArea->szDataFileName = hb_strdup( szFile );

   /* ( 10 + ';' + 7 [ C:attribute ] + ';' + 5 + ';' + 3 + ';' ) */
   szData = ( char * ) hb_xgrab( ( ( unsigned int ) pArea->area.uiFieldCount * 29 ) + 10 );
   szFieldDup = ( char * ) hb_xgrab( ( ( unsigned int ) pArea->area.uiFieldCount * 12 ) + 2 );
   szFieldDup[ 0 ] = ';';
   szFieldDup[ 1 ] = '\0';
   ulFieldDups = 1;
   ptr = szData;
   pField = pArea->area.lpFields;
   while( uiCount++ < pArea->area.uiFieldCount && errCode == HB_SUCCESS )
   {
      szFieldName = hb_dynsymName( ( PHB_DYNS ) pField->sym );
      if( ! szFieldName )
         errCode = HB_FAILURE;
      else
      {
         int          iFieldNameLen = strlen( szFieldName );
         const char * ptrDouble = strstr( szFieldDup, szFieldName );

         if( ! iFieldNameLen || iFieldNameLen > 10 )
            errCode = HB_FAILURE;
         else if( ptrDouble != NULL && *( ptrDouble - 1 ) == ';' && *( ptrDouble + iFieldNameLen ) == ';' )
            errCode = HB_FAILURE;
         else
         {
            memcpy( szFieldDup + ulFieldDups, szFieldName, iFieldNameLen );
            ulFieldDups += iFieldNameLen;
            szFieldDup[ ulFieldDups++ ] = ';';
            szFieldDup[ ulFieldDups ] = '\0';
         }
      }

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
            cType = '?';
            errCode = HB_FAILURE;
            break;
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

   if( errCode == HB_SUCCESS )
   {
      if( pCreateInfo->uiArea == 0 )  /* search for free Area, as info for server */
      {
         hb_rddSelectWorkAreaNumber( 0 );
         pCreateInfo->uiArea = ( HB_USHORT ) hb_rddGetCurrentWorkAreaNumber();
      }
      if( ( ! pCreateInfo->atomAlias || ! *pCreateInfo->atomAlias ) && *szFile )  /* create a missing Alias */
      {
         char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

         letoCreateAlias( szFile, szAlias );
         pCreateInfo->atomAlias = szAlias;
      }

      pTable = LetoDbCreateTable( pConnection, szFile, pCreateInfo->atomAlias, szData,
                                  pCreateInfo->uiArea, pCreateInfo->cdpId );
   }
   else
      pTable = NULL;

   hb_xfree( szFieldDup );
   hb_xfree( szData );

   if( ! pTable )
   {
      if( pConnection->iError == EDBF_SHARED )  /* NetErr() */
         commonError( pArea, EG_OPEN, EDBF_SHARED, 32, pArea->szDataFileName, EF_CANDEFAULT, NULL );
      else if( pConnection->iError == 1 || pConnection->iError == 1000 )
         commonError( pArea, EG_SYNTAX, pConnection->iError, 0, NULL, 0, NULL );
      else if( ! pConnection->iError )
         commonError( pArea, EG_CREATE, EDBF_CORRUPT, 0, szFile, 0, NULL );
      else
         hb_rddSetNetErr( HB_TRUE );
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

/* lpdbTransInfo->lpaDest is a LetoDBf WA; ->lpaSrc possible not */
static void leto_dbfTransCheckCounters( LPDBTRANSINFO lpdbTransInfo )
{
   HB_BOOL     fCopyCtr = HB_TRUE;
   HB_USHORT   uiCount, uiDest;
   LETOAREAP   pArea = ( LETOAREAP ) lpdbTransInfo->lpaDest;
   LETOTABLE * pTable = pArea->pTable;

   if( pTable->ulRecCount > 0 || ( pTable->fShared && ! pTable->fFLocked ) )
      fCopyCtr = HB_FALSE;
   else if( pTable->fHaveAutoinc )
   {
      PHB_ITEM pItem = NULL;

      for( uiCount = 0; uiCount < lpdbTransInfo->uiItemCount; ++uiCount )
      {
         HB_USHORT   uiField = lpdbTransInfo->lpTransItems[ uiCount ].uiDest;
         LETOFIELD * pField = pArea->pTable->pFields + uiField - 1;

         if( ( pField->uiFlags & HB_FF_AUTOINC ) )
         {
            if( pItem == NULL )
               pItem = hb_itemNew( NULL );
            if( SELF_FIELDINFO( lpdbTransInfo->lpaSource,
                                lpdbTransInfo->lpTransItems[ uiCount ].uiSource,
                                DBS_COUNTER, pItem ) != HB_SUCCESS )
            {
               fCopyCtr = HB_FALSE;
               break;
            }
         }
      }
      if( pItem != NULL )
         hb_itemRelease( pItem );
   }

   if( fCopyCtr )
   {
      if( pTable->fHaveAutoinc )
         lpdbTransInfo->uiFlags |= DBTF_CPYCTR;
   }
   else
   {
      for( uiCount = uiDest = 0; uiCount < lpdbTransInfo->uiItemCount; ++uiCount )
      {
         HB_USHORT   uiField = lpdbTransInfo->lpTransItems[ uiCount ].uiDest;
         LETOFIELD * pField = pArea->pTable->pFields + uiField - 1;

         /* remove ?ALL? autoinc fields and HB_FT_MODTIME field from list */
         if( ! ( pField->uiFlags & HB_FF_AUTOINC ) && pField->uiType != HB_FT_MODTIME )
         {
            if( uiDest != uiCount )
            {
               lpdbTransInfo->lpTransItems[ uiDest ].uiSource =
               lpdbTransInfo->lpTransItems[ uiCount ].uiSource;
               lpdbTransInfo->lpTransItems[ uiDest ].uiDest =
               lpdbTransInfo->lpTransItems[ uiCount ].uiDest;
            }
            ++uiDest;
         }
      }
      if( uiDest < uiCount )
      {
         lpdbTransInfo->uiItemCount = uiDest;
         lpdbTransInfo->uiFlags &= ~( DBTF_MATCH | DBTF_PUTREC );
      }
   }
}

static HB_ERRCODE letoInfo( LETOAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   LETOTABLE *      pTable = pArea->pTable;
   LETOCONNECTION * pConnection;

   HB_TRACE( HB_TR_DEBUG, ( "letoInfo(%p, %hu, %p)", pArea, uiIndex, pItem ) );

   switch( uiIndex )
   {
      case DBI_ISDBF:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case DBI_CANPUTREC:
         hb_itemPutL( pItem, HB_FALSE );
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

         pConnection = letoGetConnPool( pTable->uiConnection );
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
         hb_itemPutC( pItem, pArea->szDataFileName );
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

         pConnection = letoGetConnPool( pTable->uiConnection );
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
         pConnection = letoGetConnPool( pTable->uiConnection );
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
         pConnection = letoGetConnPool( pTable->uiConnection );
         hb_itemPutC( pItem, LetoGetServerVer( pConnection ) );
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
         else if( HB_IS_STRING( pItem ) && hb_itemGetCLen( pItem ) <= HB_SYMBOL_NAME_LEN )
         {
            ulLen = eprintf( szData, "%c;%lu;%d;%s;", LETOCMD_dbi, pTable->hTable, uiIndex,
                             hb_itemGetCPtr( pItem ) );
         }
         else
            ulLen = eprintf( szData, "%c;%lu;%d;", LETOCMD_dbi, pTable->hTable, uiIndex );

         pConnection = letoGetConnPool( pTable->uiConnection );
         if( ! leto_SendRecv( pConnection, pArea, szData, ulLen, 1021 ) )
            return HB_FAILURE;

         ptr = leto_firstchar( pConnection );
         if( ( ptr2 = LetoFindCmdItem( ptr ) ) != NULL )
            hb_itemPutCL( pItem, ptr, ptr2 - ptr );
         break;
      }

      case DBI_TRANSREC:
      {
         char    szData[ 32 ];
         HB_BOOL fTransRec = pArea->fTransRec;

         if( HB_IS_LOGICAL( pItem ) )
            pArea->fTransRec = hb_itemGetL( pItem );
         else if( HB_IS_POINTER( pItem ) )
         {
            LPDBTRANSINFO pTransInfo = hb_dbTransInfoGet( pItem );

            if( pTransInfo )
            {
               pArea->fTransRec = HB_TRUE;
               leto_dbfTransCheckCounters( pTransInfo );
            }
         }

         eprintf( szData, "%c;%lu;%d;.%c.;", LETOCMD_dbi, pTable->hTable, uiIndex, ( pArea->fTransRec ? 'T' : 'F' ) );
         pConnection = letoGetConnPool( pTable->uiConnection );
         if( ! leto_SendRecv( pConnection, pArea, szData, 0, 0 ) || *pConnection->szBuffer != '+' )
         {
            pArea->fTransRec = fTransRec;  /* reset to old state */
            return HB_FAILURE;
         }

         hb_itemPutL( pItem, fTransRec );  /* ugly overwrite GCalloc pTransInfo with last state */
         break;
      }

      case DBI_LASTUPDATE:  /* stored as HB_LONG Julian */
         hb_itemPutDL( pItem, pTable->lLastUpdate );
         break;

      case DBI_BUFREFRESHTIME:
      {
         int iBufRefreshTime = pTable->iBufRefreshTime;

         if( HB_IS_NUMERIC( pItem ) )
         {
            pConnection = letoGetConnPool( pTable->uiConnection );
            if( hb_itemGetNI( pItem ) < -1 )
               pTable->iBufRefreshTime = pConnection->iBufRefreshTime;
            else
               pTable->iBufRefreshTime = hb_itemGetNI( pItem );
            pTable->llCentiSec = 0;
         }

         hb_itemPutNI( pItem, iBufRefreshTime );
         break;
      }

      case DBI_AUTOREFRESH:
         if( HB_IS_LOGICAL( pItem ) )
            pTable->fAutoRefresh = hb_itemGetL( pItem );
         else
            hb_itemPutL( pItem, pTable->fAutoRefresh );
         break;

      case DBI_CLEARBUFFER:
         pTable->ptrBuf = NULL;
         pTable->llCentiSec = 0;
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
   HB_USHORT        uiFields, uiCount;
   DBFIELDINFO      dbFieldInfo;
   LETOTABLE *      pTable;
   LETOFIELD *      pField;
   HB_ERRCODE       errCode;
   char             szFile[ HB_PATH_MAX ];

   HB_TRACE( HB_TR_DEBUG, ( "letoOpen(%p, %p)", pArea, pOpenInfo ) );

   if( ( pConnection = leto_OpenConnection( pArea, pOpenInfo, szFile, HB_FALSE ) ) == NULL )
   {
      hb_rddSetNetErr( HB_TRUE );
      return HB_FAILURE;
   }
   pArea->szDataFileName = hb_strdup( szFile );
   if( ( ! pOpenInfo->atomAlias || ! *pOpenInfo->atomAlias ) && *szFile )  /* create a missing Alias */
   {
      char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

      letoCreateAlias( szFile, szAlias );
      pOpenInfo->atomAlias = szAlias;
   }

   /* sets pTable->pTagCurrent */
   pTable = LetoDbOpenTable( pConnection, szFile, pOpenInfo->atomAlias,
                             pOpenInfo->fShared, pOpenInfo->fReadonly,
                             pOpenInfo->cdpId ? pOpenInfo->cdpId : "", pOpenInfo->uiArea );
   if( ! pTable )
   {
      if( pConnection->iError != 103 )  /* no runtime error, only NetErr() ? */
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

   errCode = SELF_SETFIELDEXTENT( ( AREAP ) pArea, pTable->uiFieldExtent );
   if( errCode == HB_SUCCESS )
      uiFields = pTable->uiFieldExtent;
   else
      uiFields = 0;
   for( uiCount = 0; errCode == HB_SUCCESS && uiCount < uiFields; uiCount++ )
   {
      /* memset( &dbFieldInfo, 0, sizeof( dbFieldInfo ) ); */
      pField = pTable->pFields + uiCount;
      dbFieldInfo.atomName = pField->szName;
      dbFieldInfo.uiType = pField->uiType;
      dbFieldInfo.uiTypeExtended = 0;
      dbFieldInfo.uiLen = pField->uiLen;
      dbFieldInfo.uiDec = pField->uiDec;
      dbFieldInfo.uiFlags = pField->uiFlags;
      errCode = SELF_ADDFIELD( ( AREAP ) pArea, &dbFieldInfo );
   }

   if( errCode != HB_SUCCESS || SUPER_OPEN( ( AREAP ) pArea, pOpenInfo ) != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      hb_rddSetNetErr( HB_TRUE );
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
   }
   pArea->area.fBof = pTable->fBof;
   pArea->area.fEof = pTable->fEof;
   pArea->area.fFound = pTable->fFound;

   return HB_SUCCESS;
}

#define letoRelease  NULL

static HB_ERRCODE letoStructSize( LETOAREAP pArea, HB_USHORT * StructSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoStructSize(%p, %p)", pArea, StructSize ) );

   HB_SYMBOL_UNUSED( pArea );
   *StructSize = sizeof( LETOAREA );

   return HB_SUCCESS;
}

static HB_ERRCODE letoSysName( LETOAREAP pArea, HB_BYTE * pBuffer )
{
   HB_TRACE( HB_TR_DEBUG, ( "letoSysName(%p, %p)", pArea, pBuffer ) );

   HB_SYMBOL_UNUSED( pArea );
   strcpy( ( char * ) pBuffer, "LETO" );
   return HB_SUCCESS;
}

/* static HB_ERRCODE letoEval( LETOAREAP pArea, LPDBEVALINFO pEvalInfo ) */
#define letoEval  NULL  /* dbEval contain CBs */

static HB_ERRCODE letoPack( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoPack(%p)", pArea ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea );

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

static PHB_ITEM leto_mkCodeBlock( const char * szExp, HB_ULONG ulLen )
{
   PHB_ITEM pBlock = NULL;

   if( ulLen > 0 )
   {
      PHB_ITEM pFreshBlock;

      if( szExp[ 0 ] == '{' && szExp[ ulLen - 1 ] == '}' )
         hb_vmPushString( szExp, ulLen );
      else
      {
         char * szMacro = ( char * ) hb_xgrab( ulLen + 5 );

         szMacro[ 0 ] = '{';
         szMacro[ 1 ] = '|';
         szMacro[ 2 ] = '|';
         memcpy( szMacro + 3, szExp, ulLen );
         szMacro[ 3 + ulLen ] = '}';
         szMacro[ 4 + ulLen ] = '\0';
         hb_vmPushString( szMacro, ulLen + 5 );
         hb_xfree( szMacro );
      }

      pFreshBlock = hb_stackItemFromTop( -1 );
      if( pFreshBlock )
      {
         hb_macroGetValue( pFreshBlock, 0, 64 );  /* 64 = HB_MACRO_GEN_REFER */
         pBlock = hb_itemNew( hb_stackItemFromTop( -1 ) );
         hb_stackPop();
      }
   }

   return pBlock;
}

static HB_ERRCODE letoSort( LETOAREAP pArea, LPDBSORTINFO pSortInfo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   LPDBTRANSINFO pTransInfo = &pSortInfo->dbtri;
   LETOAREAP     pAreaDst = ( LETOAREAP ) pTransInfo->lpaDest;
   HB_BOOL       fLetoAreaDst = leto_CheckArea( pAreaDst );
   HB_ERRCODE    errCode = HB_FAILURE;
   char *        pData, * ptr;
   HB_USHORT     uiIndex;
   HB_ULONG      ulLen;

   HB_TRACE( HB_TR_DEBUG, ( "letoSort(%p, %p)", pArea, pSortInfo ) );

   if( pArea->pTable->uiUpdated )
      leto_PutRec( pArea );
   if( fLetoAreaDst && pAreaDst->pTable->uiUpdated )
      leto_PutRec( pAreaDst );

   if( ! fLetoAreaDst ||
       ( pArea->pTable->uiConnection != pAreaDst->pTable->uiConnection ) ||
       ( pTransInfo->dbsci.itmCobFor && ! pTransInfo->dbsci.lpstrFor ) ||
       ( pTransInfo->dbsci.itmCobWhile && ! pTransInfo->dbsci.lpstrWhile ) )
   {
      errCode = SUPER_SORT( ( AREAP ) pArea, pSortInfo );
      if( fLetoAreaDst && pAreaDst->pTable->uiUpdated )
         leto_PutRec( pAreaDst );
      return errCode;
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

   if( ! leto_SendRecv( pConnection, pArea, pData, ptr - pData, 0 ) )
      ptr = NULL;
   else
      ptr = leto_firstchar( pConnection );

   if( ptr && *( ptr - 1 ) == '+' )
      errCode = HB_SUCCESS;
   else if( ptr )
   {
      if( ( ! pTransInfo->dbsci.itmCobFor && pTransInfo->dbsci.lpstrFor ) ||
          ( ! pTransInfo->dbsci.itmCobWhile && pTransInfo->dbsci.lpstrWhile ) )
      {
         HB_BOOL fValid = HB_TRUE;

         if( pTransInfo->dbsci.lpstrFor )
         {
            pTransInfo->dbsci.itmCobFor = leto_mkCodeBlock( hb_itemGetCPtr( pTransInfo->dbsci.lpstrFor ),
                                                            hb_itemGetCLen( pTransInfo->dbsci.lpstrFor ) );
            if( ! pTransInfo->dbsci.itmCobFor ||
                ! HB_IS_LOGICAL( hb_vmEvalBlockOrMacro( pTransInfo->dbsci.itmCobFor ) ) )
               fValid = HB_FALSE;
         }
         if( fValid && pTransInfo->dbsci.lpstrWhile )
         {
            pTransInfo->dbsci.itmCobWhile = leto_mkCodeBlock( hb_itemGetCPtr( pTransInfo->dbsci.lpstrWhile ),
                                                              hb_itemGetCLen( pTransInfo->dbsci.lpstrWhile ) );
            if( ! pTransInfo->dbsci.itmCobWhile ||
                ! HB_IS_LOGICAL( hb_vmEvalBlockOrMacro( pTransInfo->dbsci.itmCobWhile ) ) )
               fValid = HB_FALSE;
         }

         if( fValid )
         {
            errCode = SUPER_SORT( ( AREAP ) pArea, pSortInfo );
            if( fLetoAreaDst && pAreaDst->pTable->uiUpdated )
               leto_PutRec( pAreaDst );
         }
         else
            commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, pTransInfo->dbsci.lpstrFor ?
                                                             hb_itemGetCPtr( pTransInfo->dbsci.lpstrFor ) :
                                                             hb_itemGetCPtr( pTransInfo->dbsci.lpstrWhile ) );

         if( pTransInfo->dbsci.itmCobFor )
         {
            hb_vmDestroyBlockOrMacro( pTransInfo->dbsci.itmCobFor );
            pTransInfo->dbsci.itmCobFor = NULL;
         }
         if( pTransInfo->dbsci.itmCobWhile )
         {
            hb_vmDestroyBlockOrMacro( pTransInfo->dbsci.itmCobWhile );
            pTransInfo->dbsci.itmCobWhile = NULL;
         }
      }
   }
   hb_xfree( pData );

   return errCode;
}

static HB_ERRCODE letoTrans( LETOAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   LETOAREAP  pAreaDst = ( LETOAREAP ) pTransInfo->lpaDest;
   HB_BOOL    fLetoAreaDst = leto_CheckArea( pAreaDst );
   HB_ERRCODE errCode = HB_FAILURE;
   char *     pData, * ptr;
   HB_ULONG   ulLen;

   HB_TRACE( HB_TR_DEBUG, ( "letoTrans(%p, %p)", pArea, pTransInfo ) );

   if( pArea->pTable->uiUpdated )
      leto_PutRec( pArea );
   if( fLetoAreaDst && pAreaDst->pTable->uiUpdated )
      leto_PutRec( pAreaDst );

   if( ! fLetoAreaDst ||
       ( pArea->pTable->uiConnection != pAreaDst->pTable->uiConnection ) ||
       ( pTransInfo->dbsci.itmCobFor && ! pTransInfo->dbsci.lpstrFor ) ||
       ( pTransInfo->dbsci.itmCobWhile && ! pTransInfo->dbsci.lpstrWhile ) )
   {
      errCode = SUPER_TRANS( ( AREAP ) pArea, pTransInfo );
      if( fLetoAreaDst && pAreaDst->pTable->uiUpdated )
         leto_PutRec( pAreaDst );
      return errCode;
   }

   ulLen = 82 + LETO_MAX_TAGNAME +
           hb_itemGetCLen( pTransInfo->dbsci.lpstrFor ) +
           hb_itemGetCLen( pTransInfo->dbsci.lpstrWhile ) +
           pTransInfo->uiItemCount * 16;

   pData = hb_xgrab( ulLen );
   pData[ 0 ] = LETOCMD_trans;
   pData[ 1 ] = ';';
   ptr = leto_PutTransInfo( pArea, pAreaDst, pTransInfo, pData + 2 );

   if( ! leto_SendRecv( pConnection, pArea, pData, ptr - pData, 0 ) )
      ptr = NULL;
   else
      ptr = leto_firstchar( pConnection );
   if( ptr && *( ptr - 1 ) == '+' )
   {
      errCode = HB_SUCCESS;
      if( ! memcmp( ptr, "+++;", 4 ) )
      {
         LETOTABLE * pTable = pArea->pTable;

         leto_ParseRecord( pConnection, pAreaDst->pTable, ptr + 4 );
         leto_SetAreaFlags( pAreaDst );
         if( pTable->fAutoRefresh )
            pTable->llCentiSec = LETO_CENTISEC();
      }
      pArea->pTable->ptrBuf = NULL;
   }
   else if( ptr )  /* try if expression is vaild at client */
   {
      if( ( ! pTransInfo->dbsci.itmCobFor && pTransInfo->dbsci.lpstrFor ) ||
          ( ! pTransInfo->dbsci.itmCobWhile && pTransInfo->dbsci.lpstrWhile ) )
      {
         HB_BOOL fValid = HB_TRUE;

         if( pTransInfo->dbsci.lpstrFor )
         {
            pTransInfo->dbsci.itmCobFor = leto_mkCodeBlock( hb_itemGetCPtr( pTransInfo->dbsci.lpstrFor ),
                                                            hb_itemGetCLen( pTransInfo->dbsci.lpstrFor ) );
            if( ! pTransInfo->dbsci.itmCobFor ||
                ! HB_IS_LOGICAL( hb_vmEvalBlockOrMacro( pTransInfo->dbsci.itmCobFor ) ) )
               fValid = HB_FALSE;
         }
         if( fValid && pTransInfo->dbsci.lpstrWhile )
         {
            pTransInfo->dbsci.itmCobWhile = leto_mkCodeBlock( hb_itemGetCPtr( pTransInfo->dbsci.lpstrWhile ),
                                                              hb_itemGetCLen( pTransInfo->dbsci.lpstrWhile ) );
            if( ! pTransInfo->dbsci.itmCobWhile ||
                ! HB_IS_LOGICAL( hb_vmEvalBlockOrMacro( pTransInfo->dbsci.itmCobWhile ) ) )
               fValid = HB_FALSE;
         }

         if( fValid )
         {
            errCode = SUPER_TRANS( ( AREAP ) pArea, pTransInfo );
            if( fLetoAreaDst && pAreaDst->pTable->uiUpdated )
               leto_PutRec( pAreaDst );
         }
         else
            commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, pTransInfo->dbsci.lpstrFor ?
                                                             hb_itemGetCPtr( pTransInfo->dbsci.lpstrFor ) :
                                                             hb_itemGetCPtr( pTransInfo->dbsci.lpstrWhile ) );

         if( pTransInfo->dbsci.itmCobFor )
         {
            hb_vmDestroyBlockOrMacro( pTransInfo->dbsci.itmCobFor );
            pTransInfo->dbsci.itmCobFor = NULL;
         }
         if( pTransInfo->dbsci.itmCobWhile )
         {
            hb_vmDestroyBlockOrMacro( pTransInfo->dbsci.itmCobWhile );
            pTransInfo->dbsci.itmCobWhile = NULL;
         }
      }
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
      leto_PutRec( pArea );

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

   HB_TRACE( HB_TR_DEBUG, ( "letoClearRel(%p) [%p]", pArea, ( ( AREAP ) pArea )->lpdbRelations ) );

   if( ( ( AREAP ) pArea )->lpdbRelations )
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );

      errCode = SUPER_CLEARREL( ( AREAP ) pArea );
      if( errCode == HB_SUCCESS && pConnection->uiServerMode >= 3 )
      {
         char     szData[ 17 ];
         HB_ULONG ulLen;

         ulLen = eprintf( szData, "%c;%lu;02;", LETOCMD_rela, pArea->pTable->hTable );
         if( ! leto_SendRecv( pConnection, pArea, szData, ulLen, 1026 ) )
            errCode = HB_FAILURE;
      }
   }
   return errCode;
}

static HB_BOOL leto_TestRelCyclic( PHB_ITEM pParentChain, HB_USHORT uiChildArea )
{
   AREAP   pArea = ( AREAP ) hb_rddGetWorkAreaPointer( uiChildArea );
   HB_BOOL bCyclic = HB_FALSE;

   if( pArea && pArea->lpdbRelations )
   {
      LPDBRELINFO lpDbRel = pArea->lpdbRelations;
      HB_SIZE     nRel, nLen = hb_arrayLen( pParentChain );

      hb_arraySize( pParentChain, nLen + 1 );
      while( lpDbRel && ! bCyclic )
      {
         nRel = 1;
         while( nRel <= nLen )
         {
            if( ( int ) lpDbRel->lpaChild->uiArea == hb_arrayGetNI( pParentChain, nRel ) )
            {
               bCyclic = HB_TRUE;
               break;
            }
            nRel++;
         }

         if( ! bCyclic )
         {
            hb_arraySetNI( pParentChain, nLen + 1, lpDbRel->lpaParent->uiArea );
            bCyclic = leto_TestRelCyclic( pParentChain, lpDbRel->lpaChild->uiArea );
         }

         lpDbRel = lpDbRel->lpdbriNext;
      }
      hb_arraySize( pParentChain, nLen );
   }

   return bCyclic;
}

/* can fail at server because of CB with a user function/ local variable */
static HB_ERRCODE letoSetRel( LETOAREAP pArea, LPDBRELINFO pRelInf )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   HB_ERRCODE errCode;
   HB_BOOL    bCyclic;

   HB_TRACE( HB_TR_DEBUG, ( "letoSetRel(%p, %p)", pArea, pRelInf ) );

   if( pRelInf->lpaParent->uiArea == pRelInf->lpaChild->uiArea )
      bCyclic = HB_TRUE;
   else
   {
      PHB_ITEM pParentChain = hb_itemArrayNew( 1 );

      hb_arraySetNI( pParentChain, 1, pRelInf->lpaParent->uiArea );
      bCyclic = leto_TestRelCyclic( pParentChain, pRelInf->lpaChild->uiArea );
      hb_itemRelease( pParentChain );
   }

   if( bCyclic )
   {
      if( pRelInf->abKey )
         hb_itemRelease( pRelInf->abKey );
      if( pRelInf->itmCobExpr )
         hb_itemRelease( pRelInf->itmCobExpr );
      commonError( pArea, EG_SYNTAX, 1020, 0, NULL, 0, "Cyclic relation detected" );

      return HB_FAILURE;
   }

   errCode = SUPER_SETREL( ( AREAP ) pArea, pRelInf );
   if( errCode == HB_SUCCESS )
   {
      if( pConnection->uiServerMode >= 3 )  /* only possible in No_Save_WA = 1 */
      {
         char *   szData = NULL;
         HB_ULONG ulLen = 0;

         if( ! HB_IS_STRING( pRelInf->abKey ) || ! hb_itemGetCLen( pRelInf->abKey ) ||
             ! HB_IS_BLOCK( pRelInf->itmCobExpr ) )
         {
            commonError( pArea, EG_SYNTAX, 1020, 0, NULL, 0,
                         HB_IS_BLOCK( pRelInf->itmCobExpr ) ? "empty relation string" : "empty relation codeblock" );
            errCode = HB_FAILURE;
         }
         else
         {
            ulLen = hb_itemGetCLen( pRelInf->abKey ) + 32;
            szData = ( char * ) hb_xgrab( ulLen );

            /* parent area transmitted as server internal ulAreaID, in this mode same as pRelInf->lpaParent->uiArea */
            ulLen = eprintf( szData, "%c;%lu;01;%u;%s;", LETOCMD_rela, pArea->pTable->hTable,
                                                         pRelInf->lpaChild->uiArea, hb_itemGetCPtr( pRelInf->abKey ) );
         }

         if( errCode == HB_SUCCESS )
         {
            if( leto_SendRecv( pConnection, pArea, szData, ulLen, 0 ) )
            {
               if( leto_CheckError( pArea, pConnection ) )
                  errCode = HB_FAILURE;
            }
            else
               errCode = HB_FAILURE;
         }

         if( szData )
            hb_xfree( szData );
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
   char             szIFileName[ HB_PATH_MAX ];
   const char *     ptr;
   const char *     szBagName;
   LETOTAGINFO *    pTagInfo;
   int              iRcvLen;

   HB_TRACE( HB_TR_DEBUG, ( "letoOrderListAdd(%p, %p)", pArea, pOrderInfo ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea );

   leto_OpenConn( pConnection, leto_RemoveIpFromPath( hb_itemGetCPtr( pOrderInfo->atomBagName ) ), szIFileName );
   szBagName = szIFileName;

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
   if( ( iRcvLen = leto_SendRecv( pConnection, pArea, szData, iRcvLen, 0 ) ) == 0 || leto_CheckError( pArea, pConnection ) )
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

   if( ( iRcvLen - 4 ) > ( ptr - pConnection->szBuffer ) )
   {
      leto_ParseRecord( pConnection, pArea->pTable, ptr );
      leto_SetAreaFlags( pArea );
      if( pTable->fAutoRefresh )
         pTable->llCentiSec = LETO_CENTISEC();
   }
   pTable->ptrBuf = NULL;

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
      leto_PutRec( pArea );

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
         if( ! pTagInfo->fProduction || ! LetoProdSupport() || ! hb_setGetAutOpen() )
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
      leto_PutRec( pArea );

   if( ! pTagInfo )
      return HB_FAILURE;
   else
   {
      HB_USHORT uiLen;
      char *    ptr;

      hb_strncpy( szBagName, leto_RemoveIpFromPath( hb_itemGetCPtr( pOrderInfo->atomBagName ) ), HB_PATH_MAX - 1 );
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

      if( ! pTagInfo->fProduction || ! LetoProdSupport() || ! hb_setGetAutOpen() )
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
      leto_PutRec( pArea );

   if( ! pTable->pTagCurrent || ! pTable->pTagCurrent->TagName )
      pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, "" );
   else
      pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult,
                                           pTable->pTagCurrent->TagName );

   if( pOrderInfo->itmOrder )
   {
      if( HB_IS_STRING( pOrderInfo->itmOrder ) )
      {
         char szTag[ LETO_MAX_TAGNAME + 1 ];

         hb_strncpyUpper( szTag, hb_itemGetCPtr( pOrderInfo->itmOrder ), hb_itemGetCLen( pOrderInfo->itmOrder ) );
         pOrderInfo->itmOrder = hb_itemPutC( pOrderInfo->itmOrder, szTag );
      }

      if( HB_IS_STRING( pOrderInfo->itmOrder ) || HB_IS_NUMERIC( pOrderInfo->itmOrder ) )
         return LetoDbOrderFocus( pTable,
                                  HB_IS_STRING( pOrderInfo->itmOrder ) ? hb_itemGetCPtr( pOrderInfo->itmOrder ) : NULL,
                                  HB_IS_NUMERIC( pOrderInfo->itmOrder ) ? ( HB_USHORT ) hb_itemGetNI( pOrderInfo->itmOrder ) : 0 );
      return HB_SUCCESS;
   }

   return HB_FAILURE;
}

static HB_ERRCODE letoOrderListRebuild( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoOrderListRebuild(%p)", pArea ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea );

   if( LetoDbReindex( pTable ) )
      return HB_FAILURE;

   return SELF_GOTOP( ( AREAP ) pArea );
}

#define letoOrderCondition  NULL

static HB_ERRCODE letoOrderCreate( LETOAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   LETOTABLE *   pTable = pArea->pTable;
   const char *  szKey, * szFor, * szBagName;
   char          szTag[ LETO_MAX_TAGNAME + 1 ];
   char          szIFileName[ HB_PATH_MAX ];
   LETOTAGINFO * pTagInfo;
   unsigned int  uiFlags;
   LPDBORDERCONDINFO lpdbOrdCondInfo = pArea->area.lpdbOrdCondInfo;

   HB_TRACE( HB_TR_DEBUG, ( "letoOrderCreate(%p, %p)", pArea, pOrderInfo ) );

   if( pTable->uiUpdated )
      leto_PutRec( pArea );

   /* we need to transfer to server and use the compiled string expression
    * a possible CB in pOrderInfo->itmCobExpr doesn't matter */
   if( ! hb_itemGetCLen( pOrderInfo->abExpr ) )
   {
      hb_rddSetNetErr( HB_TRUE );
      return HB_FAILURE;
   }
   szKey = hb_itemGetCPtr( pOrderInfo->abExpr );

   if( pOrderInfo->abBagName )
   {
      leto_OpenConn( pConnection, leto_RemoveIpFromPath( pOrderInfo->abBagName ), szIFileName );
      szBagName = szIFileName;
   }
   else
      szBagName = NULL;

   if( pOrderInfo->atomBagName && *pOrderInfo->atomBagName )
      hb_strncpy( szTag, pOrderInfo->atomBagName, LETO_MAX_TAGNAME );
   else  /* else TAG is created by server from szBagName */
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
                 ( lpdbOrdCondInfo->fExclusive ? LETO_INDEX_EXCL : 0 ) |
                 ( lpdbOrdCondInfo->fUseFilter ? LETO_INDEX_FILT : 0 );

   /* close all index order, re-register the orders reported by server after creating the new order */
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
   if( LetoDbOrderCreate( pTable, szBagName, szTag,
                          szKey, uiFlags, szFor,
                          lpdbOrdCondInfo ? lpdbOrdCondInfo->abWhile : NULL,
                          lpdbOrdCondInfo ? lpdbOrdCondInfo->lNextCount : 0 ) )
   {
      if( pConnection->iError )
         commonError( pArea, EG_SYNTAX, pConnection->iError, 0, NULL, 0, NULL );
      else
         leto_CheckError( pArea, pConnection );
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
      leto_PutRec( pArea );

   if( ! pTagInfo || ! pTable )
      return HB_SUCCESS;

   if( ! pOrderInfo->itmOrder )
      szTagName = NULL;
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

   if( szTagName )
   {
      eprintf( szData, "%c;%lu;13;%s;", LETOCMD_ord, pTable->hTable, szTagName );
      if( ! leto_SendRecv( pConnection, pArea, szData, 0, 0 ) || leto_CheckError( pArea, pConnection ) )
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

      if( ! pTable->pTagCurrent )
         LetoDbOrderFocus( pTable, NULL, 0 );
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
   char             szData[ LETO_MAX_TAGNAME + LETO_MAX_KEY + 54 ];
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

         if( ! pTagInfo )
         {
            LetoDbRecCount( pTable, ( HB_ULONG * ) &ul );
            hb_itemPutNL( pOrderInfo->itmResult, ul );
            break;
         }

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
               leto_PutRec( pArea );

            ulLen = eprintf( szData, "%c;%lu;0%c;%s;%lu;", LETOCMD_ord, pTable->hTable,
                             ( uiIndex == DBOI_POSITION ? '5' : '9' ),
                             pTagInfo->TagName, hb_itemGetNL( pOrderInfo->itmNewVal ) );
            if( ! leto_SendRecv( pConnection, pArea, szData, ulLen, 1021 ) )
               return HB_FAILURE;
            hb_itemPutL( pOrderInfo->itmResult, HB_TRUE );

            leto_ParseRecord( pConnection, pArea->pTable, leto_firstchar( pConnection ) );
            leto_SetAreaFlags( pArea );
            pTable->ptrBuf = NULL;
            if( pTable->fAutoRefresh )
               pTable->llCentiSec = LETO_CENTISEC();

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
         HB_LONG  lRet;
         HB_ULONG ulLen;

         pArea->lpdbPendingRel = NULL;
         if( pTable->uiUpdated )
            leto_PutRec( pArea );

         ulLen = eprintf( szData, "%c;%lu;06;%s;%lu;%c;%lu;", LETOCMD_ord, pTable->hTable,
                          ( pTagInfo ) ? pTagInfo->TagName : "",
                          pTable->ulRecNo, ( char ) ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 ),
                          pOrderInfo->itmNewVal && HB_IS_NUMERIC( pOrderInfo->itmNewVal ) ?
                          hb_itemGetNL( pOrderInfo->itmNewVal ) : 1 );
         lRet = leto_SendRecv( pConnection, pArea, szData, ulLen, 1021 );
         if( ! lRet )
            return HB_FAILURE;

         pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, lRet );
         leto_ParseRecord( pConnection, pArea->pTable, leto_firstchar( pConnection ) );
         leto_SetAreaFlags( pArea );
         pTable->ptrBuf = NULL;
         if( pTable->fAutoRefresh )
            pTable->llCentiSec = LETO_CENTISEC();

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
            leto_PutRec( pArea );

         if( pTagInfo && pOrderInfo->itmNewVal && HB_IS_STRING( pOrderInfo->itmNewVal ) )
         {
            HB_ULONG ulLen = hb_itemGetCLen( pOrderInfo->itmNewVal );
            char *   szData2 = ( char * ) hb_xgrab( ulLen + LETO_MAX_TAGNAME + 64 );

            ulLen = eprintf( szData2, "%c;%lu;07;%s;%lu;%c;%d;%lu;%s;", LETOCMD_ord, pTable->hTable,
                             pTagInfo->TagName,
                             pTable->ulRecNo, ( char ) ( ( hb_setGetDeleted() ) ? 0x41 : 0x40 ), uiIndex,
                             ulLen, hb_itemGetCPtr( pOrderInfo->itmNewVal ) );

            if( ! leto_SendRecv( pConnection, pArea, szData2, ulLen, 1021 ) )
            {
               hb_xfree( szData2 );
               return HB_FAILURE;
            }
            else
               hb_xfree( szData2 );

            leto_ParseRecord( pConnection, pArea->pTable, leto_firstchar( pConnection ) );
            leto_SetAreaFlags( pArea );
            pTable->ptrBuf = NULL;
            if( pTable->fAutoRefresh )
               pTable->llCentiSec = LETO_CENTISEC();

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
         char      cType;
         char *    pData = szData + 4;
         HB_USHORT uiKeyLen;
         HB_ULONG  ulLen;

         ulLen = eprintf( pData, "%c;%lu;%d;%s;%lu;", LETOCMD_dboi, pTable->hTable, uiIndex,
                  ( pTagInfo ) ? pTagInfo->TagName : "", pTable->ulRecNo );

         if( pItem && ( cType = leto_ItemType( pItem ) ) == pTagInfo->cKeyType )
         {
            char szKey[ LETO_MAX_KEY + 1 ];

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

   pTable->ptrBuf = NULL;
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
   }

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

   pTable->ptrBuf = NULL;
   if( hb_setGetOptimize() )
   {
      if( SELF_CLEARFILTER( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;

      /* test available filter text for validity at server */
      if( pFilterInfo->abFilterText && hb_itemGetCLen( pFilterInfo->abFilterText ) )
      {
         if( ! LetoDbSetFilter( pTable, hb_itemGetCPtr( pFilterInfo->abFilterText ), hb_setGetForceOpt() ) )
         {
            /* server can evaluate expression -> optimized */
            if( pFilterInfo->itmCobExpr )  /* remove a given codeblock */
               pFilterInfo->itmCobExpr = NULL;
            pFilterInfo->fOptimized = HB_TRUE;
         }
         else
         {
            LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

            if( pConnection->iError )
            {
               commonError( pArea, EG_SYNTAX, pConnection->iError, 0, NULL, 0, NULL );
               return HB_FAILURE;
            }

            pFilterInfo->fOptimized = HB_FALSE;
#if 0  /* throw ? error of server - or ignore it ? */
            if( hb_setGetForceOpt() )
            {
               const char * ptr = leto_firstchar( pConnection );

               if( strlen( ptr ) >= 3 && if( ptr[ 3 ] == ':' ) )
                  leto_CheckError( pArea, pConnection );
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
   LETOCONNECTION * pConnection;
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoRawLock(%p, %hu, %lu)", pArea, uiAction, ulRecNo ) );

   if( pTable->fReadonly )
      return HB_SUCCESS;  /* Harbour conform, better would be HB_FAILURE */

   if( pTable->uiUpdated )
      leto_PutRec( pArea );

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
            pConnection = letoGetConnPool( pTable->uiConnection );
            if( pConnection->iError )
               commonError( pArea, pConnection->iError == 1038 ? EG_DATATYPE : EG_SYNTAX, pConnection->iError, 0,
                            pConnection->iError == 1038 ? pConnection->szBuffer : NULL, 0, NULL );
            return HB_FAILURE;
         }
         break;

      case REC_UNLOCK:
         if( LetoDbRecUnLock( pTable, ulRecNo ) )
         {
            pConnection = letoGetConnPool( pTable->uiConnection );
            if( pConnection->iError )
               commonError( pArea, pConnection->iError == 1038 ? EG_DATATYPE : EG_SYNTAX, pConnection->iError, 0,
                            pConnection->iError == 1038 ? pConnection->szBuffer : NULL, 0, NULL );
            return HB_FAILURE;
         }
         break;

      case FILE_LOCK:
      case HEADER_LOCK:
         if( LetoDbFileLock( pTable ) )
         {
            pConnection = letoGetConnPool( pTable->uiConnection );
            if( pConnection->iError )
               commonError( pArea, pConnection->iError == 1038 ? EG_DATATYPE : EG_SYNTAX, pConnection->iError, 0,
                            pConnection->iError == 1038 ? pConnection->szBuffer : NULL, 0, NULL );
            return HB_FAILURE;
         }
         break;

      case FILE_UNLOCK:
      case HEADER_UNLOCK:
         if( LetoDbFileUnLock( pTable ) )
         {
            pConnection = letoGetConnPool( pTable->uiConnection );
            if( pConnection->iError )
               commonError( pArea, pConnection->iError == 1038 ? EG_DATATYPE : EG_SYNTAX, pConnection->iError, 0,
                            pConnection->iError == 1038 ? pConnection->szBuffer : NULL, 0, NULL );
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
         if( ! pConnection->fTransActive && pTable->ulLocksMax )  /* release all record locks */
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
   LETOTABLE * pTable = pArea->pTable;
   HB_ULONG    ulRecNo = hb_itemGetNL( pRecNo );
   HB_ERRCODE  errCode;

   HB_TRACE( HB_TR_DEBUG, ( "letoUnLock(%p, %p)", pArea, pRecNo ) );

   errCode = SELF_RAWLOCK( ( AREAP ) pArea, ( ulRecNo ) ? REC_UNLOCK : FILE_UNLOCK, ulRecNo );
   if( errCode == HB_SUCCESS && ulRecNo && pTable->pLocksPos )
      leto_DelRecLock( pTable, ulRecNo );

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

   HB_TRACE( HB_TR_DEBUG, ( "letoDrop(%p, %p, %lu)", pRDD, pItemTable, ulConnect ) );

   HB_SYMBOL_UNUSED( pRDD );
   HB_SYMBOL_UNUSED( ulConnect );

   szTableFile = hb_itemGetCPtr( pItemTable );
   szIndexFile = hb_itemGetCPtr( pItemIndex );

   pConnection = leto_OpenConn( NULL, szTableFile, szTFileName );
   pConnection = leto_OpenConn( pConnection, szIndexFile, szIFileName );

   hb_rddSetNetErr( HB_FALSE );
   if( pConnection != NULL )
   {
      HB_ULONG ulLen = eprintf( szData, "%c;%s;%s;%s;", LETOCMD_drop, szTFileName, szIFileName, pConnection->szMemoExt );

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

   HB_TRACE( HB_TR_DEBUG, ( "letoExists(%p, %p, %p, %lu)", pRDD, pItemTable, pItemIndex, ulConnect ) );

   HB_SYMBOL_UNUSED( pRDD );
   HB_SYMBOL_UNUSED( ulConnect );

   szTableFile = hb_itemGetCPtr( pItemTable );
   szIndexFile = hb_itemGetCPtr( pItemIndex );

   pConnection = leto_OpenConn( NULL, szTableFile, szTFileName );
   pConnection = leto_OpenConn( pConnection, szIndexFile, szIFileName );
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

   HB_TRACE( HB_TR_DEBUG, ( "letoRename(%p, %p, %p, %p, %lu)", pRDD, pItemTable, pItemIndex, pItemNew, ulConnect ) );

   HB_SYMBOL_UNUSED( pRDD );
   HB_SYMBOL_UNUSED( ulConnect );

   szTableFile = hb_itemGetCPtr( pItemTable );
   szIndexFile = hb_itemGetCPtr( pItemIndex );
   szNewFile = hb_itemGetCPtr( pItemNew );

   pConnection = leto_OpenConn( NULL, szTableFile, szTFileName );
   pConnection = leto_OpenConn( pConnection, szIndexFile, szIFileName );
   leto_OpenConn( pConnection, szNewFile, szFileNameNew );

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
   HB_TRACE( HB_TR_DEBUG, ( "letoInit(%p)", pRDD ) );

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

   HB_TRACE( HB_TR_DEBUG, ( "letoExit(%p)", pRDD ) );

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
   HB_USHORT        uiMemoType;

   if( pConnection && pConnection->uiMemoType )
      uiMemoType = pConnection->uiMemoType;
   else if( pConnection && *( pConnection->szDriver ) )
   {
      if( strstr( pConnection->szDriver, "NTX" ) != NULL )
         uiMemoType = DB_MEMO_DBT;
      else if( strstr( pConnection->szDriver, "CDX" ) != NULL )
         uiMemoType = DB_MEMO_FPT;
      else
         uiMemoType = DB_MEMO_SMT;
   }
   else if( pConnection && pConnection->uiDriver == LETO_NTX )
      uiMemoType = DB_MEMO_DBT;
   else
      uiMemoType = DB_MEMO_FPT;

   return uiMemoType;
}

static const char * leto_MemoExt( HB_ULONG ulConnect )
{
   LETOCONNECTION * pConnection = ( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() ) ?
                                  letoGetConnPool( ( HB_USHORT ) ulConnect - 1 ) : letoGetCurrConn();

   if( pConnection && *pConnection->szMemoExt )
      return pConnection->szMemoExt;
   else if( pConnection )
   {
      switch( leto_MemoType( ulConnect ) )
      {
         case DB_MEMO_DBT:
            return ".dbt";

         case DB_MEMO_FPT:
            return ".fpt";

         case DB_MEMO_SMT:
            return ".smt";
      }
   }

   return NULL;
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
         LETOCONNECTION * pConnection = ( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() ) ?
                                        letoGetConnPool( ( HB_USHORT ) ulConnect - 1 ) : letoGetCurrConn();
         char szAddr[ 96 ];

         if( pConnection )
         {
            int  iLen;

            szAddr[ 0 ] = '/';
            szAddr[ 1 ] = '/';
            strcpy( szAddr + 2, pConnection->pAddr );
            iLen = strlen( szAddr );
            szAddr[ iLen++ ] = ':';
            iLen += ultostr( pConnection->iPort, szAddr + iLen );
            szAddr[ iLen++ ] = '/';
            szAddr[ iLen ] = '\0';
         }
         else
            szAddr[ 0 ] = '\0';

         if( HB_IS_STRING( pItem ) )
         {
            char szNewAddr[ 96 ];
            int  iNewPort = 0;

            if( leto_getIpFromPath( hb_itemGetCPtr( pItem ), szNewAddr, &iNewPort, NULL ) )
               leto_ConnectionFind( szNewAddr, iNewPort );
         }

         hb_itemPutC( pItem, szAddr );
         break;
      }

      case RDDI_ISDBF:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case RDDI_CANPUTREC:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case RDDI_ORDBAGEXT:      /* multi-TAG default */
      case RDDI_ORDEREXT:       /* single-TAG default */
      case RDDI_ORDSTRUCTEXT:   /* single-TAG default */
      {
         LETOCONNECTION * pConnection = ( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() ) ?
                                        letoGetConnPool( ( HB_USHORT ) ulConnect - 1 ) : letoGetCurrConn();

         if( pConnection && *( pConnection->szDriver ) )
         {
            if( strstr( pConnection->szDriver, "NTX" ) != NULL )
               hb_itemPutC( pItem, ".ntx" );
            else if( strstr( pConnection->szDriver, "NSX" ) != NULL )
               hb_itemPutC( pItem, ".nsx" );
            else if( strstr( pConnection->szDriver, "FPT" ) == NULL )
               hb_itemPutC( pItem, ".cdx" );
            else
               hb_itemPutC( pItem, "" );
         }
         else
            hb_itemPutC( pItem, "" );
         break;
      }

      case RDDI_MEMOEXT:  /* by intention not setable ! */
         hb_itemPutC( pItem, leto_MemoExt( ulConnect ) );
         break;

      case RDDI_MEMOTYPE:
         if( HB_IS_NUMERIC( pItem ) )
         {
            HB_USHORT uiMemoType = ( HB_USHORT ) hb_itemGetNI( pItem );

            if( uiMemoType == DB_MEMO_DBT || uiMemoType == DB_MEMO_FPT || uiMemoType == DB_MEMO_SMT )
            {
               LETOCONNECTION * pConnection = ( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() ) ?
                                              letoGetConnPool( ( HB_USHORT ) ulConnect - 1 ) : letoGetCurrConn();

               if( pConnection && pConnection->uiMemoType != uiMemoType )
               {
                  HB_USHORT uiOldMemoType = pConnection->uiMemoType;

                  pConnection->uiMemoType = uiMemoType;
                  if( uiMemoType == DB_MEMO_DBT )
                  {
                     strcpy( pConnection->szMemoExt, ".dbt" );
                     pConnection->uiMemoBlocksize = 512;
                  }
                  else if( uiMemoType == DB_MEMO_FPT )
                  {
                     strcpy( pConnection->szMemoExt, ".fpt" );
                     pConnection->uiMemoBlocksize = 64;
                  }
                  else
                  {
                     strcpy( pConnection->szMemoExt, ".smt" );
                     pConnection->uiMemoBlocksize = 32;
                  }

                  hb_itemPutNI( pItem, uiOldMemoType );
               }
               else if( pConnection )
                  hb_itemPutNI( pItem, pConnection->uiMemoType );
               else
                  hb_itemPutNI( pItem, 0 );
            }
            else
               hb_itemPutNI( pItem, leto_MemoType( ulConnect ) );
         }
         else
            hb_itemPutNI( pItem, leto_MemoType( ulConnect ) );
         break;

      case RDDI_MEMOBLOCKSIZE:
         if( HB_IS_NUMERIC( pItem ) && hb_itemGetNI( pItem ) > 0 )
         {
            LETOCONNECTION * pConnection = ( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() ) ?
                                           letoGetConnPool( ( HB_USHORT ) ulConnect - 1 ) : letoGetCurrConn();

            if( pConnection )
            {
               HB_USHORT uiOldSize = pConnection->uiMemoBlocksize;

               pConnection->uiMemoBlocksize = ( HB_USHORT ) hb_itemGetNI( pItem );
               if( uiOldSize )
                  hb_itemPutNI( pItem, uiOldSize );
               else
                  hb_itemPutNI( pItem, leto_MemoBlocksize( ulConnect ) );
            }
            else
               hb_itemPutNI( pItem, leto_MemoBlocksize( ulConnect ) );
         }
         else
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

      case RDDI_TABLEEXT:
         hb_itemPutC( pItem, ".dbf" );
         break;

      /* boolean ask/ set from server */
      case RDDI_OPTIMIZE:
      case RDDI_FORCEOPT:
      case RDDI_AUTOOPEN:
      case RDDI_STRUCTORD:
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
               if( *pConnection->szBuffer == '+' )
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

      /* numerics ask/ set from server */
      case RDDI_AUTOORDER:
      case RDDI_DEBUGLEVEL:
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
               if( hb_itemGetNI( pItem ) < 0 )
                  hb_itemPutNI( pItem, 0 );
               eprintf( szNum, "%d", hb_itemGetNI( pItem ) );
            }
            if( LetoRddInfo( pConnection, uiIndex, szNum ) == HB_SUCCESS )
            {
               const char * ptr = leto_firstchar( pConnection );

               if( *( ptr - 1 ) == '+' )
               {
                  hb_itemPutNL( pItem, strtoul( ptr, NULL, 10 ) );
                  iRes = 0;
                  if( fSet && uiIndex == RDDI_AUTOORDER )
                  {
                     if( uiIndex == RDDI_AUTOORDER )
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

            hb_itemPutL( pItem, pConnection->fRefreshCount );
            if( fSet )
               pConnection->fRefreshCount = hb_itemGetL( pItem );
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

      case RDDI_TRIGGER:
      {
         LETOCONNECTION * pConnection;

         if( ulConnect > 0 && ulConnect <= ( HB_ULONG ) uiGetConnCount() )
            pConnection = letoGetConnPool( ( HB_USHORT ) ulConnect - 1 );
         else
            pConnection = letoGetCurrConn();

         if( pConnection )
         {
            if( LetoRddInfo( pConnection, uiIndex, NULL ) == HB_SUCCESS )
            {
               const char * ptr = leto_firstchar( pConnection );

               if( *( ptr - 1 ) == '+' )
                  hb_itemPutC( pItem, ptr );
               else
                  hb_itemPutC( pItem, "" );
            }
         }
         else
            hb_itemPutC( pItem, "?" );
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

/* used by transactions begin and commit */
static HB_ERRCODE leto_UpdArea( AREAP pArea, void * p )
{
   if( leto_CheckAreaConn( pArea, ( LETOCONNECTION * ) p ) &&
       ( ( LETOAREAP ) pArea )->pTable->uiUpdated )
   {
      leto_PutRec( ( LETOAREAP ) pArea );
   }
   return HB_SUCCESS;
}

/* used by transactions, forcible remove all locks at server */
static HB_ERRCODE leto_UnLockRec( AREAP pArea, void * p )
{
   if( leto_CheckAreaConn( pArea, ( LETOCONNECTION * ) p ) )
   {
      ( ( LETOAREAP ) pArea )->pTable->ptrBuf = NULL;
      SELF_RAWLOCK( pArea, FILE_UNLOCK, 0 );
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
#if 0  /* alternative */
      pConnection->ulTransBlockLen = HB_ISNUM( 1 ) ? hb_parnl( 1 ) : 0;
#else
      if( ( HB_ISLOG( 1 ) && hb_parl( 1 ) ) )  /* unlock if explicitely given */
         hb_rddIterateWorkAreas( leto_UnLockRec, ( void * ) pConnection );
#endif
      pConnection->fTransActive = HB_TRUE;
   }
}

static void leto_ClearTransBuffers( LETOCONNECTION * pConnection )
{
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
   if( pConnection->pTransAppend )
   {
      hb_xfree( pConnection->pTransAppend );
      pConnection->pTransAppend = NULL;
   }

   if( pConnection->szBuffer && pConnection->ulBufferLen > LETO_SENDRECV_BUFFSIZE )
   {
      pConnection->ulBufferLen = LETO_SENDRECV_BUFFSIZE;
      pConnection->szBuffer = hb_xrealloc( pConnection->szBuffer, LETO_SENDRECV_BUFFSIZE + 1 );
   }

   pConnection->pRecsNotList.ulRecNo = pConnection->pRecsNotList.hTable = 0;
   pConnection->ulRecsInList = pConnection->uiTransAppend = 0;
   pConnection->ulTransDataLen = pConnection->ulRecsInTrans = pConnection->uiTransAppLen = 0;
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
      leto_ClearTransBuffers( pConnection );

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

/* if NOT fUnlockAll, client will not know about append RLocks */
HB_FUNC( LETO_COMMITTRANSACTION )
{
   LETOAREAP        pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   LETOTABLE *      pTable = pArea->pTable;
   LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
   HB_USHORT        ui = 0;

   HB_TRACE( HB_TR_DEBUG, ( "LETO_COMMITTRANSACTION(%d)", ( int ) ( HB_ISLOG( 1 ) ? hb_parl( 1 ) : HB_TRUE ) ) );

   if( ! leto_CheckTrans( pArea, HB_TRUE ) )
   {
      commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, NULL );
      return;
   }

   /* check all WA for not updated data, before deactivate transaction mode */
   hb_rddIterateWorkAreas( leto_UpdArea, ( void * ) pConnection );
   pConnection->fTransActive = HB_FALSE;

   while( ui < pConnection->uiTransAppend )
   {
      /* this will indicate a shared WA with no [F|R]lock, so we try a Flock() */
      if( ! pConnection->pTransAppend[ ui ].ulRecNo )
      {
         if( LetoDbFileLock( pConnection->pTransAppend[ ui ].pTable ) )
         {
            leto_ClearTransBuffers( pConnection );
            commonError( pArea, EG_UNLOCKED, EDBF_APPENDLOCK, 0, NULL, 0, NULL );
            return;
         }
      }
      ui++;
   }

   if( pConnection->szTransBuffer && ( pConnection->ulTransDataLen > ( HB_ULONG ) pConnection->uiTBufOffset ) )
   {
      HB_BOOL fUnlockAll = ( HB_ISLOG( 1 ) ) ? hb_parl( 1 ) : HB_TRUE;

      HB_PUT_LE_UINT32( pConnection->szTransBuffer + 2, pConnection->ulRecsInTrans );
      pConnection->szTransBuffer[ pConnection->uiTBufOffset - 1 ] = ( HB_BYTE ) ( ( fUnlockAll ) ? 0x41 : 0x40 );

      if( ! leto_SendRecv( pConnection, pArea, ( char * ) pConnection->szTransBuffer, pConnection->ulTransDataLen, 1021 ) )
      {
         if( fUnlockAll )
            hb_rddIterateWorkAreas( leto_UnLockRec, ( void * ) pConnection );
         leto_ClearTransBuffers( pConnection );
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

      leto_ClearTransBuffers( pConnection );
   }

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
   PHB_ITEM pParams = hb_itemArrayNew( 7 );

   hb_arraySetC( pParams, 1, "LETO_DBEVAL" );

   if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) > 0 )
   {
      char * szBlock = hb_strdup( hb_parc( 1 ) );

      if( leto_CbTrim( szBlock ) )
         hb_arraySetC( pParams, 2, szBlock );
      hb_xfree( szBlock );
   }

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

   hb_arraySetNL( pParams, 5, HB_ISNUM( 4 ) ? ( HB_LONG ) hb_parnl( 4 ) : -1 );
   hb_arraySetNL( pParams, 6, HB_ISNUM( 5 ) ? ( HB_ULONG ) hb_parnl( 5 ) : 0 );
   hb_arraySetL( pParams, 7, HB_ISLOG( 6 ) ? ( HB_ULONG ) hb_parl( 6 ) : 0 );

   if( hb_arrayGetType( pParams, 2 ) == HB_IT_STRING )
      leto_udp( HB_FALSE, pParams );
   else
   {
      hb_itemRelease( pParams );
      hb_ret();
   }
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
         leto_PutRec( pArea );

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
         leto_PutRec( pArea );

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

/* replaces __DBTRANS() by alternative use of string expressions instead of codeblocks,
 * Leto_dbTrans( cnDstArea, aFields, cbFor, cbWhile, nNext, nRecord, lRest ) */
HB_FUNC( LETO_DBTRANS )
{
   AREAP   pSrcArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   AREAP   pDstArea = NULL;
   HB_UINT uiDstArea = 0;

   if( HB_ISCHAR( 1 ) )      /* ALIAS = LetoDBf extension */
      hb_rddGetAliasNumber( hb_parc( 1 ), ( int * ) &uiDstArea );
   else if( HB_ISNUM( 1 ) )  /* WA number */
      uiDstArea = hb_parni( 1 );
   if( uiDstArea )
      pDstArea = ( AREAP ) hb_rddGetWorkAreaPointer( uiDstArea );

   if( pDstArea && pSrcArea )
   {
      PHB_ITEM    pFields = hb_param( 2, HB_IT_ARRAY );
      DBTRANSINFO dbTransInfo;
      HB_ERRCODE  errCode;

      memset( &dbTransInfo, 0, sizeof( DBTRANSINFO ) );
      /* set DBTF_MATCH in case of fields (must be not all) are at same pos & type */
      errCode = hb_dbTransStruct( pSrcArea, pDstArea, &dbTransInfo, NULL, pFields );
      if( errCode == HB_SUCCESS )
      {
#if ! defined( __HARBOUR30__ )
         PHB_ITEM pTransItm;
#endif
         dbTransInfo.dbsci.itmCobFor   = hb_param( 3, HB_IT_BLOCK );
         dbTransInfo.dbsci.lpstrFor    = hb_param( 3, HB_IT_STRING );
         dbTransInfo.dbsci.itmCobWhile = hb_param( 4, HB_IT_BLOCK );
         dbTransInfo.dbsci.lpstrWhile  = hb_param( 4, HB_IT_STRING );
         dbTransInfo.dbsci.lNext       = hb_param( 5, HB_IT_NUMERIC );
         dbTransInfo.dbsci.itmRecID    = HB_ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY );
         dbTransInfo.dbsci.fRest       = hb_param( 7, HB_IT_LOGICAL );

         /* different to Harbour: fIgnoreFilter = HB_FALSE -- but nowhere used in HB */
         dbTransInfo.dbsci.fIgnoreFilter     = HB_TRUE;
         dbTransInfo.dbsci.fIncludeDeleted   = HB_TRUE;
         dbTransInfo.dbsci.fLast             = HB_FALSE;
         dbTransInfo.dbsci.fIgnoreDuplicates = HB_FALSE;
         dbTransInfo.dbsci.fBackward         = HB_FALSE;
         dbTransInfo.dbsci.fOptimized        = HB_FALSE;

#if ! defined( __HARBOUR30__ )
         pTransItm = hb_dbTransInfoPut( NULL, &dbTransInfo );
         /* call hb_dbfTransCheckCounters() to add DBTF_CPYCTR or remove DBTF_MATCH & DBTF_PUTREC */
         /* content of pTransItem will be afterwards boolean with state of previous fTransRec */
         errCode = SELF_INFO( dbTransInfo.lpaDest, DBI_TRANSREC, pTransItm );
#endif
         if( errCode == HB_SUCCESS )
         {
            if( ! dbTransInfo.uiItemCount )
               errCode = HB_FAILURE;
            else
               errCode = SELF_TRANS( dbTransInfo.lpaSource, &dbTransInfo );
#if ! defined( __HARBOUR30__ )
            /* reset state of fTransRec */
            SELF_INFO( dbTransInfo.lpaDest, DBI_TRANSREC, pTransItm );
            if( errCode == HB_SUCCESS && ( dbTransInfo.uiFlags & DBTF_CPYCTR ) )
               hb_dbTransCounters( &dbTransInfo );
#endif
         }
#if ! defined( __HARBOUR30__ )
         if( pTransItm )
            hb_itemRelease( pTransItm );
#endif
      }

      if( dbTransInfo.lpTransItems )
         hb_xfree( dbTransInfo.lpTransItems );

      hb_retl( errCode == HB_SUCCESS );
   }
   else
      commonError( ( LETOAREAP ) pSrcArea, EG_ARG, EDBCMD_NOTABLE, 0, NULL, 0, "LETO_DBTRANS" );
}

HB_FUNC( LETO_COMMIT )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( leto_CheckArea( pArea ) )
      hb_retni( letoFlush( pArea ) );
}

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
         case LETO_ERR_RESTORE:
            szResult = "restoring WA failed";
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

   if( pArea && leto_CheckArea( pArea ) )
      hb_retl( pArea->area.dbfi.fOptimized );
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

   HB_TRACE( HB_TR_DEBUG, ( "leto_getConnection(%d)", iParam ) );

   if( HB_ISCHAR( iParam ) )
   {
      char szAddr[ 96 ];
      int  iPort = 0;

      if( leto_getIpFromPath( hb_parc( iParam ), szAddr, &iPort, NULL ) )
         pConnection = leto_ConnectionFind( szAddr, iPort );
   }
   else
      pConnection = letoGetCurrConn();

   return pConnection;
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
         else if( ! strcmp( szDriver, "SIXCDX" ) || ! strcmp( szDriver, "DBFFPT" ) )
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
            if( ! strcmp( szDriver, "DBFNTX" ) || ! strcmp( szDriver, "BMDBFNTX" ) )
            {
               iMemoType = DB_MEMO_DBT;
               memcpy( pConnection->szMemoExt, ".dbt\0", 5 );
            }
            else if( ! strcmp( szDriver, "DBFCDX" ) || ! strcmp( szDriver, "BMDBFCDX" ) ||
                     ! strcmp( szDriver, "DBFFPT" ) )
            {
               iMemoType = DB_MEMO_FPT;
               memcpy( pConnection->szMemoExt, ".fpt\0", 5 );
            }
            else
            {
               iMemoType = DB_MEMO_SMT;
               memcpy( pConnection->szMemoExt, ".smt\0", 5 );
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
   HB_BOOL   fEmpty = HB_TRUE;  /* return HB_TRUE for all param errors and also not memofields */
   int       iArea = 0;
   LETOAREAP pArea;

   if( HB_ISCHAR( 2 ) )
   {
      const char * szAlias = hb_parc( 2 );

      if( hb_rddVerifyAliasName( szAlias ) == HB_SUCCESS )
         hb_rddGetAliasNumber( szAlias, &iArea );
   }
   else if( HB_ISNUM( 2 ) )
      iArea = hb_parni( 2 );

   if( iArea > 0 )
      pArea = ( LETOAREAP ) hb_rddGetWorkAreaPointer( iArea );
   else
      pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      LETOTABLE * pTable = pArea->pTable;
      HB_USHORT   uiFieldPos;

      if( HB_ISCHAR( 1 ) )
         uiFieldPos = hb_rddFieldIndex( ( LPAREA ) pArea, hb_parc( 1 ) );
      else if( HB_ISNUM( 1 ) )
         uiFieldPos = ( HB_USHORT ) hb_parni( 1 );
      else
         uiFieldPos = 0;

      if( pTable && uiFieldPos && uiFieldPos <= pTable->uiFieldExtent )
      {
         LPFIELD pField = pArea->area.lpFields + --uiFieldPos;

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

   if( pArea && leto_CheckArea( pArea ) )
   {
      LETOTABLE * pTable = pArea->pTable;

      if( pTable )
      {
         if( HB_ISNUM( 1 ) )
         {
            LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
            HB_USHORT uiNum = ( HB_USHORT ) hb_parni( 1 );

            if( uiNum < 1 )
               uiNum = 1;
            if( pTable->uiRecordLen * uiNum > LETO_MAX_RECV_BLOCK )
               uiNum = ( HB_USHORT ) ( LETO_MAX_RECV_BLOCK / pTable->uiRecordLen );
            if( pConnection )
            {
               char szTmp[ 42 ];

               eprintf( szTmp, "%lu;%d", pTable->hTable, uiNum );
               LetoSet( pConnection, 2, szTmp );  /* LETOCMD_set */
               hb_retni( uiNum );
            }
            else
               hb_retni( 0 );
         }
         else
            hb_retni( pTable->Buffer.ulShoots );
      }
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

   if( pArea )
   {
      pArea->area.dbfi.fFilter = HB_TRUE;
      pArea->area.dbfi.fOptimized = HB_TRUE;
      pArea->pTable->ptrBuf = NULL;
   }
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

HB_FUNC( LETO_SET )
{
   HB_set_enum setId = ( HB_set_enum ) hb_parnidef( 1, HB_SET_INVALID_ );
   PHB_ITEM    pItem, pNewItem = NULL;
   HB_BOOL     fSet = HB_FALSE;
   HB_BOOL     fNewSet = HB_FALSE;
   int         iNewSet = 0;

   if( ! setId )
      return;
   else
   {
      if( setId != HB_SET_SOFTSEEK && setId != HB_SET_DELETED &&
          setId != HB_SET_AUTOPEN && setId != HB_SET_AUTORDER )
      {
         PHB_DYNS pDo = hb_dynsymFindName( "SET" );
         PHB_ITEM pSet = hb_param( 2, HB_IT_ANY );
         PHB_ITEM pExtra = hb_param( 3, HB_IT_ANY );

         hb_vmPushDynSym( pDo );
         hb_vmPushNil();
         hb_vmPushNumInt( setId );
         if( pSet )
         {
            hb_vmPush( pSet );
            if( pExtra )
            {
               hb_vmPush( pExtra );
               hb_vmDo( 3 );
            }
            else
               hb_vmDo( 2 );
         }
         else
            hb_vmDo( 1 );

         return;
      }

      if( HB_ISLOG( 2 ) )
      {
         fNewSet = hb_parl( 2 );
         fSet = HB_TRUE;
      }
      else if( HB_ISCHAR( 2 ) && hb_parclen( 2 ) )
      {
         if( ! leto_stricmp( hb_parc( 2 ), "ON" ) )
         {
            fNewSet = HB_TRUE;
            fSet = HB_TRUE;
         }
         else if( ! leto_stricmp( hb_parc( 2 ), "OFF" ) )
         {
            fNewSet = HB_FALSE;
            fSet = HB_TRUE;
         }
      }
      else if( HB_ISNUM( 2 ) )
      {
         iNewSet = hb_parni( 2 );
         if( iNewSet >= 0 )
            fSet = HB_TRUE;
      }
   }

   /* up here we come only for below 4 settings */
   pItem = hb_itemNew( NULL );
   if( fSet )
      pNewItem = hb_itemNew( NULL );

   switch( setId )
   {
      case HB_SET_SOFTSEEK:
         hb_itemPutL( pItem, hb_setGetSoftSeek() );
         if( fSet && fNewSet == hb_itemGetL( pItem ) )
            fSet = HB_FALSE;
         if( fSet )
            hb_itemPutL( pNewItem, fNewSet );
         break;

      case HB_SET_DELETED:
         hb_itemPutL( pItem, hb_setGetDeleted() );
         if( fSet && fNewSet == hb_itemGetL( pItem ) )
            fSet = HB_FALSE;
         if( fSet )
            hb_itemPutL( pNewItem, fNewSet );
         break;

      case HB_SET_AUTOPEN:
         hb_itemPutL( pItem, hb_setGetAutOpen() );
         if( fSet && fNewSet == hb_itemGetL( pItem ) )
            fSet = HB_FALSE;
         if( fSet )
            hb_itemPutL( pNewItem, fNewSet );
         break;

      case HB_SET_AUTORDER:
         hb_itemPutNI( pItem, hb_setGetAutOrder() );
         if( fSet && iNewSet == hb_itemGetNI( pItem ) )
            fSet = HB_FALSE;
         if( fSet )
            hb_itemPutNI( pNewItem, iNewSet );
         break;

      default:
         fSet = HB_FALSE;
         break;
   }

   if( fSet )
   {
      LETOCONNECTION * pConnection = NULL;

      /* apply local */
      hb_setSetItem( ( HB_set_enum ) hb_parni( 1 ), pNewItem );

      if( setId == HB_SET_DELETED )
      {
         LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();

         if( pArea && leto_CheckArea( pArea ) )
         {
            LETOTABLE * pTable = pArea->pTable;

            if( pTable )  /* reset skipbuffer */
            {
               pTable->ptrBuf = NULL;
               pTable->llCentiSec = 0;
               pConnection = letoGetConnPool( pTable->uiConnection );
            }
         }
      }

      /* apply at server if connected */
      if( ! pConnection )
         pConnection = letoGetCurrConn();
      if( pConnection )
      {
         if( HB_IS_NUMERIC( pNewItem ) )
         {
            char * szSet = hb_itemStr( pNewItem, NULL, NULL );

            if( szSet )
            {
               LetoSet( pConnection, 1000 + setId, szSet );
               hb_xfree( szSet );
            }
         }
         else  /* boolean */
            LetoSet( pConnection, 1000 + setId, hb_itemGetL( pNewItem ) ? "T" : "F" );
      }
   }

   if( pNewItem )
      hb_itemRelease( pNewItem );
   hb_itemReturnRelease( pItem );
}

static HB_ERRCODE leto_PreReopen( AREAP pArea, void * p )
{
   LETOCONNECTION * pConnection = ( LETOCONNECTION * ) p;

   if( leto_CheckAreaConn( pArea, pConnection ) )
   {
      LETOAREAP   pLetoArea = ( LETOAREAP ) pArea;
      LETOTABLE * pTable = pLetoArea->pTable;

      pTable->llCentiSec = 1;
   }
   else if( leto_CheckArea( ( LETOAREAP ) pArea ) )
   {
      LETOAREAP   pLetoArea = ( LETOAREAP ) pArea;
      LETOTABLE * pTable = pLetoArea->pTable;

      pTable->llCentiSec = 0;
   }

   return HB_SUCCESS;
}

static HB_ERRCODE leto_doReopen( AREAP pArea, void * p )
{
   LETOCONNECTION * pConnection = ( LETOCONNECTION * ) p;
   HB_ERRCODE errCode = HB_SUCCESS;

   if( leto_CheckArea( ( LETOAREAP ) pArea ) )
   {
      LETOAREAP   pLetoArea = ( LETOAREAP ) pArea;
      LETOTABLE * pTable = pLetoArea->pTable;
      char        szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
      HB_ULONG    ulRecNo = pTable->ulRecNo;

      if( pTable->llCentiSec && SELF_ALIAS( pArea, szAlias ) == HB_SUCCESS && *szAlias )
      {
         LETOTAGINFO * pTagInfo = pTable->pTagInfo;
         PHB_ITEM      pOrderList = hb_itemArrayNew( 0 ), pOrder;
         char          szCdp[ 32 ];
         char          szFile[ HB_PATH_MAX ];
         PHB_ITEM      pFilterText = NULL, pFilterBlock = NULL, pItem = NULL;
         HB_USHORT     uiArea = pLetoArea->area.uiArea;
         HB_BOOL       fAutOpen = hb_setGetAutOpen();
         char          szTagCurrent[ LETO_MAX_TAGNAME + 1 ];
         int           iOrder = 0;
         HB_BOOL       fFLocked = pTable->fFLocked;
         HB_ULONG      ulLocksMax = pTable->ulLocksMax;
         HB_ULONG *    pLocksPos = NULL;

         strcpy( szFile, pLetoArea->szDataFileName );
         if( pLetoArea->area.cdPage )
            hb_strncpy( szCdp, pLetoArea->area.cdPage->id, 31 );
         else
            szCdp[ 0 ] = '\0';
         if( pTable->pTagCurrent )
            strcpy( szTagCurrent, pTable->pTagCurrent->TagName );
         else
            szTagCurrent[ 0 ] = '\0';
         while( pTagInfo )
         {
            iOrder++;
            hb_arraySize( pOrderList, iOrder );
            pOrder = hb_itemArrayNew( 4 );
            hb_arraySetC( pOrder, 1, pTagInfo->BagName );
            hb_arraySetC( pOrder, 2, pTagInfo->TagName );
            if( pTagInfo->pTopScopeAsString )
               hb_arraySetC( pOrder, 3, pTagInfo->pTopScopeAsString );
            if( pTagInfo->pBottomScopeAsString )
               hb_arraySetC( pOrder, 4, pTagInfo->pBottomScopeAsString );
            hb_arraySet( pOrderList, iOrder, pOrder );
            hb_itemRelease( pOrder );
            pTagInfo = pTagInfo->pNext;
         }

         if( iOrder && fAutOpen )  /* set autoopen off */
         {
            pItem = hb_itemPutL( pItem, HB_FALSE );
            hb_setSetItem( ( HB_set_enum ) HB_SET_AUTOPEN, pItem );
            LetoSet( pConnection, 1000 + HB_SET_AUTOPEN, "F" );
         }

         if( pArea->dbfi.abFilterText )
         {
            pFilterText = hb_itemClone( pArea->dbfi.abFilterText );
            hb_itemRelease( pArea->dbfi.abFilterText );
            pArea->dbfi.abFilterText = NULL;
         }
         if( pArea->dbfi.itmCobExpr )
         {
            pFilterBlock = hb_itemClone( pArea->dbfi.itmCobExpr );
            /* manually remove filter CB */
            hb_vmDestroyBlockOrMacro( pArea->dbfi.itmCobExpr );
            pArea->dbfi.itmCobExpr = NULL;
         }
         pArea->dbfi.fFilter = HB_FALSE;

         if( pArea->lpdbRelations )
         {
            PHB_ITEM    pRelationList = pConnection->whoCares;
            PHB_ITEM    pRelation;
            LPDBRELINFO pRelInf;

            while( pArea->lpdbRelations )
            {
               pRelInf = pArea->lpdbRelations;
               pArea->lpdbRelations = pRelInf->lpdbriNext;

               pRelation = hb_itemArrayNew( 5 );
               if( HB_IS_BLOCK( pRelInf->itmCobExpr ) )
                  hb_arraySet( pRelation, 1, hb_itemClone( pRelInf->itmCobExpr ) );
               if( HB_IS_STRING( pRelInf->abKey ) )
                  hb_arraySetC( pRelation, 2, hb_itemGetCPtr( pRelInf->abKey ) );
               hb_arraySetNI( pRelation, 3, pRelInf->lpaChild->uiArea );
               hb_arraySetNI( pRelation, 4, uiArea );
               hb_arraySetNL( pRelation, 5, ulRecNo );
               hb_arrayAdd( pRelationList, pRelation );
               hb_itemRelease( pRelation );

               if( pRelInf->itmCobExpr )
                  hb_itemRelease( pRelInf->itmCobExpr );
               if( pRelInf->abKey )
                  hb_itemRelease( pRelInf->abKey );
               hb_xfree( pRelInf );
            }
         }

         if( ! fFLocked && ulLocksMax )
         {
            pLocksPos = ( HB_ULONG * ) hb_xgrab( sizeof( HB_ULONG ) * ulLocksMax );
            memcpy( pLocksPos, pTable->pLocksPos, sizeof( HB_ULONG ) * ulLocksMax );
            pTable->ulLocksMax = 0;
         }

         strcpy( pConnection->szDriver, pTable->szDriver );
         errCode = hb_rddOpenTable( szFile, NULL, uiArea, szAlias, pTable->fShared, pTable->fReadonly,
                                    *szCdp ? szCdp : NULL, 0, NULL, NULL );

         if( errCode == HB_SUCCESS )
         {
            pArea = ( AREAP ) hb_rddGetWorkAreaPointer( uiArea );
            if( iOrder )
            {
               int i = 1;
               DBORDERINFO pOrderInfo;

               memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
               while( i <= iOrder )
               {
                  pOrderInfo.atomBagName = hb_itemPutC( NULL, hb_arrayGetCPtr( hb_arrayGetItemPtr( pOrderList, i ), 1 ) );
                  pOrderInfo.itmOrder = hb_itemPutC( NULL, hb_arrayGetCPtr( hb_arrayGetItemPtr( pOrderList, i ), 2 ) );
                  pOrderInfo.itmResult = hb_itemNew( NULL );
                  errCode = SELF_ORDLSTADD( pArea, &pOrderInfo );
                  hb_itemRelease( pOrderInfo.atomBagName );
                  hb_itemRelease( pOrderInfo.itmOrder );
                  hb_itemRelease( pOrderInfo.itmResult );

                  if( hb_arrayGetCPtr( hb_arrayGetItemPtr( pOrderList, i ), 3 ) )
                  {
                     memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
                     pOrderInfo.itmResult = hb_itemNew( NULL );
                     pOrderInfo.itmNewVal = hb_arrayGetItemPtr( hb_arrayGetItemPtr( pOrderList, i ), 3 );
                     SELF_ORDINFO( pArea, DBOI_SCOPETOP, &pOrderInfo );
                     hb_itemRelease( pOrderInfo.itmResult );
                  }
                  if( hb_arrayGetCPtr( hb_arrayGetItemPtr( pOrderList, i ), 4 ) )
                  {
                     memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
                     pOrderInfo.itmResult = hb_itemNew( NULL );
                     pOrderInfo.itmNewVal = hb_arrayGetItemPtr( hb_arrayGetItemPtr( pOrderList, i ), 4 );
                     SELF_ORDINFO( pArea, DBOI_SCOPEBOTTOM, &pOrderInfo );
                     hb_itemRelease( pOrderInfo.itmResult );
                  }

                  i++;
               }
               if( *szTagCurrent && errCode == HB_SUCCESS )
               {
                  memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
                  pOrderInfo.itmOrder = hb_itemPutC( NULL, szTagCurrent );
                  pOrderInfo.itmResult = hb_itemPutC( NULL, NULL );
                  SELF_ORDLSTFOCUS( pArea, &pOrderInfo );
                  hb_itemRelease( pOrderInfo.itmOrder );
                  hb_itemRelease( pOrderInfo.itmResult );
               }

               if( fAutOpen )  /* restore autoopen */
               {
                  pItem = hb_itemPutL( pItem, fAutOpen );
                  hb_setSetItem( ( HB_set_enum ) HB_SET_AUTOPEN, pItem );
                  LetoSet( pConnection, 1000 + HB_SET_AUTOPEN, fAutOpen ? "T" : "F" );
               }
            }

            pLetoArea = ( LETOAREAP ) pArea;
            pTable = pLetoArea->pTable;

            errCode = SELF_GOTO( pArea, ulRecNo );
            if( pTable->ulRecNo != ulRecNo )
               errCode = HB_FAILURE;

            if( errCode == HB_SUCCESS )
            {
               if( fFLocked )
                  errCode = SELF_RAWLOCK( pArea, FILE_LOCK, ulRecNo );
               else if( ulLocksMax )
               {
                  HB_ULONG ul = 0;

                  while( ul < ulLocksMax && errCode == HB_SUCCESS )
                  {
                     errCode = SELF_RAWLOCK( pArea, REC_LOCK, pLocksPos[ ul++ ] );
                  }
               }
            }

            if( pFilterBlock || pFilterText )
            {
               DBFILTERINFO pFilterInfo;

               pFilterInfo.itmCobExpr = pFilterBlock;
               if( pFilterText )
                  pFilterInfo.abFilterText = pFilterText;
               else
                  pFilterInfo.abFilterText = hb_itemPutC( NULL, NULL );
               pFilterInfo.fFilter = HB_TRUE;
               pFilterInfo.lpvCargo = NULL;
               pFilterInfo.fOptimized = HB_FALSE;
               SELF_SETFILTER( pArea, &pFilterInfo );
               if( ! pFilterText )
                  hb_itemRelease( pFilterInfo.abFilterText );
               else
                  hb_itemRelease( pFilterText );
            }

            pTable->llCentiSec = 0;  /* reset the marker */
         }
         else  /* HB_FAILURE to open table */
         {
            if( pFilterBlock )
               hb_itemRelease( pFilterBlock );
            if( pFilterText )
               hb_itemRelease( pFilterText );
         }

         if( pOrderList )
            hb_itemRelease( pOrderList );
         if( pLocksPos )
            hb_xfree( pLocksPos );
         if( pItem )
            hb_itemRelease( pItem );
      }
   }

   return errCode;
}

HB_FUNC( LETO_RECONNECT )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();

   if( pConnection )
   {
      char         szAddr[ 96 ];
      int          iPort = pConnection->iPort;
      const char * szUser = ( HB_ISCHAR( 2 ) && hb_parclen( 2 ) ) ? hb_parc( 2 ) : NULL;
      const char * szPass = ( HB_ISCHAR( 3 ) && hb_parclen( 3 ) ) ? hb_parc( 3 ) : NULL;
      int          iTimeOut = hb_parnidef( 4, pConnection->iTimeOut );
      int          iBufRefreshTime = hb_parnidef( 5, pConnection->iBufRefreshTime );
      HB_BOOL      fZombieCheck = hb_parldef( 6, HB_TRUE );
      double       dIdle = HB_ISNUM( 7 ) ? hb_parnd( 7 ) : 0.0;
      LETOAREAP    pLetoArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
      HB_USHORT    uiActiveWA = pLetoArea ? pLetoArea->area.uiArea : 0;
      HB_USHORT    uiMemoType = pConnection->uiMemoType;
      HB_USHORT    uiMemoBlocksize = pConnection->uiMemoBlocksize;
      HB_USHORT    uiLockSchemeExtend = pConnection->uiLockSchemeExtend;
      char         szMemoExt[ HB_MAX_FILE_EXT + 1 ];
      char         szDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];

      /* given new address as first param */
      if( ! ( HB_ISCHAR( 1 ) && hb_parclen( 1 ) && leto_getIpFromPath( hb_parc( 1 ), szAddr, &iPort, NULL ) ) )
         strcpy( szAddr, pConnection->pAddr );
      /* re-connect to same server may need a second to reset locks */
      if( ! leto_stricmp( pConnection->pAddr, szAddr ) && ! HB_ISNUM( 7 ) )
         dIdle = 1;

      strcpy( szDriver, pConnection->szDriver );
      strcpy( szMemoExt, pConnection->szMemoExt );

      /* prepare a marker for WA used by closing connection */
      hb_rddIterateWorkAreas( leto_PreReopen, ( void * ) pConnection );

      LetoConnectionClose( pConnection );  /* only socket shutdown, free pConnection resources */
      if( dIdle > 0 )
         hb_idleSleep( dIdle );
      pConnection = LetoConnectionNew( szAddr, iPort, szUser, szPass, iTimeOut, fZombieCheck );

      if( pConnection )
      {
         HB_ERRCODE errCode;

         pConnection->iBufRefreshTime = iBufRefreshTime;
         pConnection->uiLockSchemeExtend = uiLockSchemeExtend;
         pConnection->uiMemoType = uiMemoType;
         pConnection->uiMemoBlocksize = uiMemoBlocksize;

         strcpy( pConnection->szDriver, szDriver );
         strcpy( pConnection->szMemoExt, szMemoExt );
         pConnection->whoCares = hb_itemArrayNew( 0 );  /* for collecting relations */
         errCode = hb_rddIterateWorkAreas( leto_doReopen, ( void * ) pConnection );
         if( errCode == HB_SUCCESS )
         {
            if( hb_arrayLen( pConnection->whoCares ) )
            {
               PHB_ITEM   pRelation;
               DBRELINFO  dbRelations;
               AREAP      pParentArea;
               int        iArea, i = 1, iLen = hb_arrayLen( pConnection->whoCares );

               while( i <= iLen && errCode == HB_SUCCESS )
               {
                  pRelation = hb_arrayGetItemPtr( pConnection->whoCares, i );

                  if( HB_IS_BLOCK( hb_arrayGetItemPtr( pRelation, 1 ) ) )
                     dbRelations.itmCobExpr = hb_itemClone( hb_arrayGetItemPtr( pRelation, 1 ) );
                  else
                     dbRelations.itmCobExpr = hb_itemNew( NULL );
                  if( HB_IS_STRING( hb_arrayGetItemPtr( pRelation, 2 ) ) )
                     dbRelations.abKey = hb_itemNew( hb_arrayGetItemPtr( pRelation, 2 ) );
                  else
                     dbRelations.abKey = hb_itemNew( NULL );

                  iArea = hb_itemGetNI( hb_arrayGetItemPtr( pRelation, 3 ) );
                  dbRelations.lpaChild = ( AREAP ) hb_rddGetWorkAreaPointer( iArea );
                  iArea = hb_itemGetNI( hb_arrayGetItemPtr( pRelation, 4 ) );
                  pParentArea = ( AREAP ) hb_rddGetWorkAreaPointer( iArea );
                  dbRelations.lpaParent = pParentArea;
                  dbRelations.isScoped = HB_FALSE;
                  dbRelations.isOptimized = HB_FALSE;
                  dbRelations.lpdbriNext = NULL;
                  errCode = SELF_SETREL( pParentArea, &dbRelations );
                  if( errCode == HB_SUCCESS && SELF_SYNCCHILDREN( pParentArea ) != HB_SUCCESS )
                     errCode = HB_FAILURE;

                  i++;
               }
            }

            hb_rddSelectWorkAreaNumber( uiActiveWA );
            strcpy( pConnection->szDriver, szDriver );
         }

         if( errCode != HB_SUCCESS )
            pConnection->iConnectRes = LETO_ERR_RESTORE;

         hb_itemRelease( pConnection->whoCares );
      }
   }

   if( pConnection && ! pConnection->iConnectRes )
      hb_retni( pConnection->iConnection );
   else
      hb_retni( -1 );
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

