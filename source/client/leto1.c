/*
 * Harbour Leto RDD
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 *           2015-18 Rolf 'elch' Beckmann
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

#define SUPERTABLE   ( &s_letoSuper )

typedef struct
{
   unsigned int uiConnection;
   HB_ULONG     ulAreaID;
   LETOAREAP    pArea;
} FINDAREASTRU;

typedef struct
{
   int       iArea;
   AREAP     pArea;
   char      szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   HB_ULONG  ulRecNo;
   PHB_ITEM  pFor;
   HB_BOOL   fEof;
} JOINAREASTRU;

static HB_USHORT s_uiRddCount = 0;
static HB_USHORT s_uiRddIdLETO = ( HB_USHORT ) -1;
static RDDFUNCS  s_letoSuper;

#ifndef LETO_NO_THREAD
   HB_ERRCODE delayedError( void );
#endif

#if defined( __HARBOUR30__ )
   char * LetoSetModName( char * szModule );
#endif


static HB_BYTE leto_ItemType( PHB_ITEM pItem )
{
   switch( hb_itemType( pItem ) )
   {
      case HB_IT_STRING:
      case HB_IT_MEMO:
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
#ifndef LETO_NO_THREAD
   hb_retni( delayedError() );
#else
   hb_retni( 0 );
#endif
}

static HB_ERRCODE commonError( LETOAREAP pArea, HB_ERRCODE uiGenCode, HB_ERRCODE uiSubCode,
                            HB_USHORT uiOsCode, const char * szFileName, HB_USHORT uiFlags,
                            const char * szOperation )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "commonError(%p, %d, %d, %d, '%s', %d, %s)", pArea, uiGenCode, uiSubCode, uiOsCode, szFileName, uiFlags, szOperation ) );

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
   return pArea && pArea->rddID == s_uiRddIdLETO && ( ( LETOAREAP ) pArea )->pTable &&
          ( ( LETOAREAP ) pArea )->pTable->uiConnection == pConnection->iConnection;
}

static HB_ERRCODE leto_ClearBuffer( AREAP pArea, void * p )
{
   if( leto_CheckAreaConn( pArea, ( LETOCONNECTION * ) p ) )
   {
      ( ( LETOAREAP ) pArea )->pTable->ptrBuf = NULL;
      SELF_SKIP( pArea, 0 );
   }

   return HB_SUCCESS;
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
      HB_ULONG     uiGenCode, uiSubCode, uiOsCode, uiFlags;
      const char * szFileName = NULL, * szDescription = NULL;

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
   hb_rddSetNetErr( HB_FALSE );
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

   if( LetoDbGoTo( pTable, ulRecNo ) )
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

#ifdef __XHARBOUR__

static char * hb_timeStampStrRawPut( char * szDateTime, long lJulian, long lMilliSec )
{
   int    iYear, iMonth, iDay, iHour, iMinutes;
   double dSeconds;

   hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
   hb_dateStrPut( szDateTime, iYear, iMonth, iDay );
   hb_timeDecode( lMilliSec, &iHour, &iMinutes, &dSeconds );
   /* hb_dateStrPut rely on 8 + 1 bytes long, now adding 9 + \0 --> len 18 */
   sprintf( szDateTime + 8, "%02d%02d%02d%03d", iHour, iMinutes, ( int ) dSeconds,
            ( int ) ( ( dSeconds - ( int ) dSeconds ) * 1000 ) );

   return szDateTime;
}

#define hb_itemGetTS( pKey, szKey )   hb_timeStampStrRawPut( szKey, hb_itemGetDL( pKey ), hb_itemGetT( pKey ) )
#define hb_itemGetTD( pItem )         ( ( double ) hb_itemGetDL( pItem ) + ( ( double ) hb_itemGetT( pItem ) / 86400000 ) )

#endif

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
      if( hb_itemGetTD( pItem ) == hb_itemGetTD( pValue ) )
         iRet = 0;
      else if( hb_itemGetTD( pValue ) < hb_itemGetTD( pItem ) )
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

static HB_ERRCODE leto_IsRelationed( AREAP pArea, void * p )
{
   if( leto_CheckAreaConn( pArea, ( LETOCONNECTION * ) p ) )
   {
      LPDBRELINFO lpDbRel = pArea->lpdbRelations;

      while( lpDbRel )
      {
         if( ( int ) lpDbRel->lpaChild->uiArea == hb_itemGetNI( ( ( LETOCONNECTION * ) p )->whoCares ) )
         {
            hb_itemPutNI( ( ( LETOCONNECTION * ) p )->whoCares, pArea->uiArea );
            return HB_FAILURE;
         }
         lpDbRel = lpDbRel->lpdbriNext;
      }
   }

   return HB_SUCCESS;
}

static HB_ERRCODE letoAppend( LETOAREAP pArea, HB_BOOL fUnLockAll )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
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

   if( pConnection->fTransActive )
   {
      if( fUnLockAll )  /* never release other locks in transaction */
         fUnLockAll = HB_FALSE;
   }

   if( LetoDbAppend( pTable, fUnLockAll ) )
      return HB_FAILURE;

   if( pTable->ulRecNo )  /* ! pConnection->fTransActive */
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

   if( ! pTable )
      return HB_FAILURE;
   else if( pTable->uiUpdated )
   {
      /* pTable->uiUpdated |= LETO_FLAG_UPD_FLUSH; */
      leto_PutRec( pArea );
      if( ! letoGetConnPool( pTable->uiConnection )->fTransActive )
      {
         if( LetoDbCommit( pTable ) )
            return HB_FAILURE;
      }
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
   const char * ptr = LetoDbGetMemo( pTable, uiIndex + 1, ( unsigned long * ) &ulLen );

   if( ! ptr )
      return HB_FAILURE;
   else if( ! ulLen )
      hb_itemPutC( pItem, "" );
   else
   {
#if defined ( __XHARBOUR__ )
      char * pBuff = NULL;

      if( uiType == HB_FT_MEMO && pArea->area.cdPage != HB_CDP_PAGE() )
      {
         pBuff = ( char * ) hb_xgrab( ulLen + 1 );
         memcpy( pBuff, ptr, ulLen );
         pBuff[ ulLen ] = '\0';

         hb_cdpnTranslate( pBuff, pArea->area.cdPage, HB_CDP_PAGE(), ulLen );
         ptr = pBuff;
      }
      hb_itemPutCL( pItem, ptr, ulLen );
	  if( pBuff )
	     hb_xfree( pBuff );
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
            hb_itemPutTDT( pItem, 0, HB_GET_LE_INT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ) );
         else
            hb_itemPutTDT( pItem,
                           HB_GET_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ),
                           HB_GET_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] + 4 ) );
         break;

      case HB_FT_MODTIME:
      case HB_FT_TIMESTAMP:
         hb_itemPutTDT( pItem,
                        HB_GET_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] ),
                        HB_GET_LE_UINT32( pTable->pRecord + pTable->pFieldOffset[ uiIndex ] + 4 ) );
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
      pBuff[ ulLenMemo ] = '\0';
      hb_cdpnTranslate( pBuff, HB_CDP_PAGE(), pArea->area.cdPage, ulLenMemo );
#else
      pBuff = hb_cdpnDup( ptr, &ulLenMemo, HB_CDP_PAGE(), pArea->area.cdPage );
#endif
      ptr = pBuff;
   }
   errCode = LetoDbPutMemo( pArea->pTable, uiIndex + 1, ptr, ulLenMemo );
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
               char * pBuff = hb_cdpnDup( hb_itemGetCPtr( pItem ), &ulLen1, HB_CDP_PAGE(), pArea->area.cdPage );

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
            if( HB_IS_DATE( pItem ) || HB_IS_DATETIME( pItem ) )
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
               hb_timeStampStrGetDT( hb_itemGetCPtr( pItem ), &lJulian, &lMillisec );
               HB_PUT_LE_UINT32( ptr, lMillisec );
            }
#endif
            else
               fTypeError = HB_TRUE;
         }
         else
         {
#ifdef __XHARBOUR__
            if( HB_IS_DATE( pItem ) || HB_IS_DATETIME( pItem ) )
            {
               HB_PUT_LE_UINT32( ptr, hb_itemGetDL( pItem ) );
               if( HB_IS_DATETIME( pItem ) )
                  HB_PUT_LE_UINT32( ptr + 4, hb_itemGetT( pItem ) );
               else
                  HB_PUT_LE_UINT32( ptr + 4, 0 );
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
         }
         break;
      }

      case HB_FT_TIMESTAMP:
      case HB_FT_MODTIME:
      {
         HB_BYTE * ptr = pTable->pRecord + pTable->pFieldOffset[ uiIndex ];

#ifdef __XHARBOUR__
         if( HB_IS_DATE( pItem ) || HB_IS_DATETIME( pItem ) )
         {
            HB_PUT_LE_UINT32( ptr, hb_itemGetDL( pItem ) );
            if( HB_IS_DATETIME( pItem ) )
               HB_PUT_LE_UINT32( ptr + 4, hb_itemGetT( pItem ) );
            else
               HB_PUT_LE_UINT32( ptr + 4, 0 );
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
                  char *  pBuff = hb_cdpnDup( hb_itemGetCPtr( pItem ), &ulLen1, HB_CDP_PAGE(), pArea->area.cdPage );

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

static HB_ERRCODE letoRecCount( LETOAREAP pArea, unsigned long * pRecCount )
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

static HB_ERRCODE leto_doFreeTable( AREAP pArea, void * p )
{
   if( leto_CheckAreaConn( pArea, ( LETOCONNECTION * ) p ) )
   {
      LETOTABLE * pTable = ( ( LETOAREAP ) pArea )->pTable;

      if( pTable )
      {
         LETOTAGINFO * pTagInfo = pTable->pTagInfo;

         if( pTable->pFieldOffset )
            hb_xfree( pTable->pFieldOffset );
         if( pTable->pFieldIsBinary )
            hb_xfree( pTable->pFieldIsBinary );
         if( pTable->pFieldUpd )
            hb_xfree( pTable->pFieldUpd );
         if( pTable->pRecord )
            hb_xfree( pTable->pRecord );
         if( pTable->pFields )
            hb_xfree( pTable->pFields );
         if( pTable->szTags )
            hb_xfree( pTable->szTags );
         if( pTable->Buffer.pBuffer )
            hb_xfree( pTable->Buffer.pBuffer );
         if( pTable->pLocksPos )
            hb_xfree( pTable->pLocksPos );

         while( pTagInfo )
         {
            LETOTAGINFO * pTagNext = pTagInfo->pNext;

            LetoDbFreeTag( pTagInfo );
            pTagInfo = pTagNext;
         }
         hb_xfree( pTable );
         ( ( LETOAREAP ) pArea )->pTable = NULL;
      }
   }

   return HB_SUCCESS;
}

static HB_ERRCODE letoClose( LETOAREAP pArea )
{
   LETOTABLE * pTable = pArea->pTable;

   HB_TRACE( HB_TR_DEBUG, ( "letoClose(%p)", pArea ) );

   if( pTable && pTable->uiUpdated )
      leto_PutRec( pArea );

   pArea->lpdbPendingRel = NULL;
   if( pTable )   /* shortcut: else no need to handle filter/ Relation in hb_waClose() */
      SUPER_CLOSE( ( AREAP ) pArea );

   if( pTable && pTable->pTagInfo )
   {
      LETOTAGINFO * pTagNext, * pTagInfo = pTable->pTagInfo;

      while( pTagInfo )
      {
         pTagNext = pTagInfo->pNext;
         LetoDbFreeTag( pTagInfo );
         pTagInfo = pTagNext;
      }

      pTable->pTagInfo = NULL;
   }

   if( pTable )
   {
      if( LetoDbCloseTable( pTable ) )
      {
         LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

         hb_rddIterateWorkAreas( leto_doFreeTable, ( void * ) pConnection );
         commonError( pArea, EG_SYNTAX, pConnection->iError, 0, NULL, 0, NULL );
      }
      pArea->pTable = NULL;
   }

   if( pArea->szDataFileName )
   {
      hb_xfree( pArea->szDataFileName );
      pArea->szDataFileName = NULL;
   }
   pArea->fTemporary = HB_FALSE;

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
               pConnection = LetoConnectionNew( szAddr, iPort, NULL, NULL, 0, LETO_USE_THREAD );
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
      leto_BeautifyPath( szFile, 0 );  /* possible double action */
   else
   {
      strcpy( szFile, leto_RemoveIpFromPath( szParam ) );
      leto_BeautifyPath( szFile, 0 );
   }

   return pConnection;
}

static LETOCONNECTION * leto_OpenConnection( LETOAREAP pArea, LPDBOPENINFO pOpenInfo, char * szFile, HB_BOOL fCreate )
{
   LETOCONNECTION * pConnection;
   int              iPort = 0;

   HB_TRACE( HB_TR_DEBUG, ( "leto_OpenConnection(%p, %p, %s, %d)", pArea, pOpenInfo ? pOpenInfo->atomAlias : "",
                            pOpenInfo ? pOpenInfo->abName : "?", ( int ) fCreate ) );

   szFile[ 0 ] = '\0';

   if( pOpenInfo->ulConnection > 0 && pOpenInfo->ulConnection <= ( HB_ULONG ) letoGetConnCount() )
      pConnection = letoGetConnPool( ( unsigned int ) pOpenInfo->ulConnection - 1 );
   else
   {
      char szAddr[ 96 ];

      if( ! leto_getIpFromPath( pOpenInfo->abName, szAddr, &iPort, NULL ) )
         pConnection = letoGetCurrConn();
      else
      {
         pConnection = leto_ConnectionFind( szAddr, iPort );
         if( ! pConnection )
            pConnection = LetoConnectionNew( szAddr, iPort, NULL, NULL, 0, LETO_USE_THREAD );
      }
   }

   if( pOpenInfo->abName )
   {
      strcpy( szFile, leto_RemoveIpFromPath( pOpenInfo->abName ) );
      leto_BeautifyPath( szFile, 0 );
   }

   if( ! pConnection )
   {
      commonError( pArea, EG_OPEN, ( fCreate ? 1 : 101 ), 0, *szFile ? szFile : "? FILE ?", 0, "CONNECTION ERROR" );
      return NULL;
   }

   return pConnection;
}

static void letoCreateAliasWA( char * szAlias )
{
   int iArea, iActiveWA = hb_rddGetCurrentWorkAreaNumber();

   hb_rddSelectFirstAvailable();
   iArea = hb_rddGetCurrentWorkAreaNumber();
   hb_rddSelectWorkAreaNumber( iActiveWA );

   eprintf( szAlias, "TmpWA%d", iArea );
}

static void letoCreateAlias( const char * szFile, char * szAlias )
{
   const char * ptrBeg, * ptrEnd, *ptrWin;

   ptrEnd = strrchr( szFile, '.' );
   if( ! ptrEnd || ( ptrEnd > szFile && *( ptrEnd - 1 ) == '.' ) )  /* .. */
       ptrEnd = szFile + strlen( szFile );

   ptrBeg = strchr( szFile, ':' );
   if( ptrBeg )  /* mem:... */
   {
      const char * ptrTmp;

      ptrBeg++;
      ptrTmp = strrchr( szFile, '/' );
      ptrWin = strrchr( szFile, '\\' );
      if( ptrWin )
      {
         if( ( ! ptrTmp || ptrWin > ptrTmp ) )
            ptrTmp = ptrWin;
      }
      if( ptrTmp )
      {
         if( ++ptrTmp > ptrBeg )
            ptrBeg = ptrTmp;
      }
   }
   else
   {
      ptrBeg = strrchr( szFile, '/' );
      ptrWin = strrchr( szFile, '\\' );
      if( ptrWin )
      {
         if( ! ptrBeg || ptrWin > ptrBeg )
            ptrBeg = ptrWin;
      }
      if( ! ptrBeg )
         ptrBeg = szFile;
      else
         ptrBeg++;
   }

   hb_strncpyUpper( szAlias, ptrBeg, HB_MIN( ptrEnd - ptrBeg, HB_RDD_MAX_ALIAS_LEN ) );
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
   HB_BOOL          fTemporary = HB_FALSE;

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

      if( ! pField->uiLen || pField->uiDec > HB_MIN( 99, pField->uiLen ) )  /* avoid ptr buffer overflow */
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
      if( ! pCreateInfo->atomAlias || ! *pCreateInfo->atomAlias )  /* create a missing Alias */
      {
         char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

         /* explicitely given blank alias convert to: "TmpWA" + first free WA number */
         if( pCreateInfo->atomAlias || ! *szFile )
            letoCreateAliasWA( szAlias );
         else
            letoCreateAlias( szFile, szAlias );
         pCreateInfo->atomAlias = szAlias;
      }

      if( pCreateInfo->uiArea == ( HB_USHORT ) hb_rddGetCurrentWorkAreaNumber() )
      {
         pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
         fTemporary = pArea->fTemporary;
      }
      else if( pCreateInfo->uiArea )
      {
         hb_rddSelectWorkAreaNumber( pCreateInfo->uiArea );
         pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
         fTemporary = pArea->fTemporary;
      }

      do
      {
         hb_rddSetNetErr( HB_FALSE );
         pTable = LetoDbCreateTable( pConnection, szFile, pCreateInfo->atomAlias, szData,
                                     pCreateInfo->uiArea, pCreateInfo->cdpId, fTemporary );
         if( pTable )
            break;

         if( ! pConnection->iError )
            pConnection->iError = EG_CREATE;
         else if( pConnection->iError == 1 || pConnection->iError == 1000 )
            pConnection->iError = EG_SYNTAX;
         else if( pConnection->iError == EDBF_SHARED )
            break;  /* with hb_rddSetNetErr() set soon */
      }
      while( commonError( pArea, EG_CREATE, pConnection->iError, 0, szFile, EF_CANRETRY | EF_CANDEFAULT, NULL ) == E_RETRY );
   }
   else
      pTable = NULL;

   hb_xfree( szFieldDup );
   hb_xfree( szData );
   if( ! pTable )
   {
      SELF_CLOSE( ( AREAP ) pArea );
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

   if( SUPER_CREATE( ( AREAP ) pArea, pCreateInfo ) != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      return HB_FAILURE;
   }

   leto_SetAreaFlags( pArea );
   return HB_SUCCESS;
}

#if ! defined( __HARBOUR30__ )
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
      PHB_ITEM pItem = hb_itemNew( NULL );

      for( uiCount = 0; uiCount < lpdbTransInfo->uiItemCount; ++uiCount )
      {
         HB_USHORT   uiField = lpdbTransInfo->lpTransItems[ uiCount ].uiDest;
         LETOFIELD * pField = pArea->pTable->pFields + uiField - 1;

         if( ( pField->uiFlags & HB_FF_AUTOINC ) )
         {
            if( SELF_FIELDINFO( lpdbTransInfo->lpaSource,
                                lpdbTransInfo->lpTransItems[ uiCount ].uiSource,
                                DBS_COUNTER, pItem ) != HB_SUCCESS )
            {
               fCopyCtr = HB_FALSE;
               break;
            }
         }
      }
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
#endif

static HB_ERRCODE letoInfo( LETOAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   LETOTABLE *      pTable = pArea->pTable;
   LETOCONNECTION * pConnection;

   HB_TRACE( HB_TR_DEBUG, ( "letoInfo(%p, %hu, %p)", pArea, uiIndex, pItem ) );

   if( ! pTable && uiIndex != DBI_TABLEEXT )  /* called by DbTableExt() possible without WA */
   {
      HB_ERRCODE errCode = HB_SUCCESS;

      /* ugly hack to fill pItem for uninitalized pTable */
      if( uiIndex == DBI_LASTUPDATE )
         hb_itemPutDL( pItem, 0 );
      else if( uiIndex == DBI_ISTEMPORARY && pItem && HB_IS_LOGICAL( pItem ) )
      {
         if( ! pArea )
         {
            hb_itemPutL( pItem, HB_FALSE );
            errCode = HB_FAILURE;
         }
         else if( hb_itemGetL( pItem ) != pArea->fTemporary )
         {
            pArea->fTemporary = hb_itemGetL( pItem );
            hb_itemPutL( pItem, pArea->fTemporary ? HB_FALSE : HB_TRUE );
         }
         else
            hb_itemPutL( pItem, pArea->fTemporary );
      }
      else if( pItem )
      {
         hb_itemPutNI( pItem, 0 );
         errCode = HB_FAILURE;
      }
      return errCode;
   }

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
            HB_ULONG ulCount, ulPos = 1;

            ulCount = strtoul( ptr, &ptr, 10 );
            if( uiIndex == DBI_LOCKCOUNT )
            {
               hb_itemPutNL( pItem, ulCount );
               break;
            }

            hb_arrayNew( pItem, ulCount );
            while( ptr && ulCount-- )
            {
               hb_arraySetNL( pItem, ulPos++, strtoul( ++ptr, &ptr, 10 ) );
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

      case DBI_ISTEMPORARY:
         /* not set-able for already open table */
         hb_itemPutL( pItem, pArea->fTemporary );
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

      case DBI_LOCKTEST:
         pConnection = letoGetConnPool( pTable->uiConnection );
         if( ! pConnection )
            return HB_FAILURE;
         else
         {
            char szData[ 64 ];

            eprintf( szData, "%c;%lu;%d;%lu;", LETOCMD_dbi, pTable->hTable, uiIndex, ( HB_ULONG ) hb_itemGetNL( pItem ) );
            if( ! leto_SendRecv( pConnection, pArea, szData, 0, 0 ) )
               return HB_FAILURE;
            else if( *pConnection->szBuffer != '+' )
               return HB_FAILURE;
            hb_itemPutNL( pItem, strtoul( leto_firstchar( pConnection ), NULL, 10 ) );
         }
         break;

      case DBI_LOCKSCHEME:
         hb_itemPutNI( pItem, pTable->uiLockScheme );
         break;

      case DBI_ISENCRYPTED:
      {
         char   szData[ 32 ];

         pConnection = letoGetConnPool( pTable->uiConnection );
         eprintf( szData, "%c;%lu;%d;;", LETOCMD_dbi, pTable->hTable, uiIndex );
         if( ! leto_SendRecv( pConnection, pArea, szData, 0, 0 ) )
            return HB_FAILURE;

         if( *pConnection->szBuffer == '+' )
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

#if ! defined( __HARBOUR30__ )
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
#endif

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
         SELF_SKIP( ( AREAP ) pArea, 0 );
         pTable->llCentiSec = 0;
         break;

      case DBI_CHILDPARENT:  /* have this WA a LETO parent [ return first found ] */
         pConnection = letoGetConnPool( pTable->uiConnection );
         pConnection->whoCares = hb_itemPutNI( NULL, ( ( AREAP ) pArea )->uiArea );
         if( hb_rddIterateWorkAreas( leto_IsRelationed, ( void * ) pConnection ) != HB_SUCCESS )
         {
            hb_itemPutNI( pItem, hb_itemGetNI( pConnection->whoCares ) );
         }
         else
            hb_itemPutNI( pItem, 0 );
         hb_itemRelease( pConnection->whoCares );
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
   char             szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

   HB_TRACE( HB_TR_DEBUG, ( "letoOpen(%p, %p)", pArea, pOpenInfo ) );

   if( ( pConnection = leto_OpenConnection( pArea, pOpenInfo, szFile, HB_FALSE ) ) == NULL )
   {
      hb_rddSetNetErr( HB_TRUE );
      return HB_FAILURE;
   }
   pArea->szDataFileName = hb_strdup( szFile );
   if( ( ! pOpenInfo->atomAlias || ! *pOpenInfo->atomAlias ) && *szFile )  /* create a missing Alias */
   {
      letoCreateAlias( szFile, szAlias );
      pOpenInfo->atomAlias = szAlias;
   }

   do
   {
      hb_rddSetNetErr( HB_FALSE );
      /* sets pTable->pTagCurrent */
      pTable = LetoDbOpenTable( pConnection, szFile, pOpenInfo->atomAlias,
                                pOpenInfo->fShared, pOpenInfo->fReadonly,
                                pOpenInfo->cdpId ? pOpenInfo->cdpId : "", pOpenInfo->uiArea );
      if( pTable )
         break;

      if( ! pConnection->iError )
         pConnection->iError = 1021;
      else if( pConnection->iError == EDBF_SHARED )  /* no RTE, only NetErr() */
         break;
   }
   while( commonError( pArea, EG_OPEN, pConnection->iError, 0, szFile, EF_CANRETRY | EF_CANDEFAULT, NULL ) == E_RETRY );

   if( ! pTable )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      hb_rddSetNetErr( HB_TRUE );
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
      if( pField->uiType == HB_FT_STRING && pField->uiDec )
      {
         pField->uiLen += pField->uiDec * 256;
         pField->uiDec = 0;
      }
      if( ! pField->uiLen || pField->uiDec > HB_MIN( 99, pField->uiLen ) )
         errCode = HB_FAILURE;
      else
         errCode = SELF_ADDFIELD( ( AREAP ) pArea, &dbFieldInfo );
   }

   if( errCode != HB_SUCCESS || SUPER_OPEN( ( AREAP ) pArea, pOpenInfo ) != HB_SUCCESS )
   {
      commonError( pArea, EG_OPEN, EDBF_CORRUPT, 0, szFile, EF_CANDEFAULT, NULL );
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

   leto_SetAreaFlags( pArea );
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

static HB_ERRCODE letoEval( LETOAREAP pArea, LPDBEVALINFO pEvalInfo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   DBLOCKINFO       dbLockInfo;
   PHB_ITEM pProces = hb_itemPutNS( NULL, 0 );
   PHB_ITEM pEvalut = hb_itemPutNS( NULL, 0 );
   PHB_ITEM pRLocks = NULL;
   PHB_ITEM pSaveValResult = NULL;
   AREAP    pRawArea = ( AREAP ) pArea;
   HB_ULONG ulNextRecNo = 0;
   HB_LONG  lNext = -1;
   HB_BOOL  fValid = ( hb_itemType( pEvalInfo->itmBlock ) & HB_IT_BLOCK ) ? HB_TRUE : HB_FALSE;
   HB_BOOL  fProved = HB_TRUE;

   HB_TRACE( HB_TR_DEBUG, ( "letoEval(%p, %p)", pArea, pEvalInfo ) );

   if( pArea->pTable && pArea->pTable->uiUpdated )
      leto_PutRec( pArea );
   memset( &dbLockInfo, 0, sizeof( DBLOCKINFO ) );

   if( fValid )
   {
      HB_BOOL fGoTop;

      if( pRawArea->valResult )  /* used by local filter block */
      {
         pSaveValResult = hb_itemClone( pRawArea->valResult );
         hb_vmDestroyBlockOrMacro( pRawArea->valResult );
         pRawArea->valResult = NULL;
      }
      else
         pSaveValResult = hb_itemNew( NULL );

      if( pEvalInfo->dbsci.lNext && hb_itemGetNL( pEvalInfo->dbsci.lNext ) >= 0 )
         lNext = hb_itemGetNL( pEvalInfo->dbsci.lNext );

      fGoTop = ( ! pEvalInfo->dbsci.fRest || ! hb_itemGetL( pEvalInfo->dbsci.fRest ) );
      if( pConnection->fDbEvalCompat && ( pEvalInfo->dbsci.itmCobWhile || lNext >= 0 ) )
         fGoTop = HB_FALSE;

      if( pEvalInfo->dbsci.itmRecID )
         fValid = ( SELF_GOTOID( pRawArea, pEvalInfo->dbsci.itmRecID ) == HB_SUCCESS );
      else if( fGoTop )
      {
         if( ! pEvalInfo->dbsci.fBackward )
            fValid = ( SELF_GOTOP( pRawArea ) == HB_SUCCESS );
         else
            fValid = ( SELF_GOBOTTOM( pRawArea ) == HB_SUCCESS );
      }

      if( lNext >= 0 )
      {
         if( ! lNext )
            fValid = HB_FALSE;
      }
      else
         lNext = 0;  /* with true fValid -> infinite, default */
   }

   if( fValid && ( pConnection->uSrvLock & 0x01 ) )  /* RDDI_AUTOLOCK */
   {
      HB_ULONG ulNewRecNo, ulLockRecNo;
      HB_LONG  lNewNext = lNext;
      HB_BOOL  fEof;

      pRLocks = hb_itemArrayNew( 0 );
      dbLockInfo.uiMethod = DBLM_MULTIPLE;
      SELF_RECNO( pRawArea, &ulNewRecNo );

      SELF_EOF( pRawArea, &fEof );
      if( ! fEof && pEvalInfo->dbsci.fBackward )
         SELF_BOF( pRawArea, &fEof );
      if( ! fEof )
         hb_itemPutNS( pEvalut, 1 );

      while( ! fEof && ( ! pEvalInfo->dbsci.itmCobWhile || hb_itemGetL( hb_vmEvalBlockV( pEvalInfo->dbsci.itmCobWhile, 2, pProces, pEvalut ) ) ) )
      {
         if( pEvalInfo->dbsci.itmCobFor )  /* FOR */
            fValid = hb_itemGetL( hb_vmEvalBlockV( pEvalInfo->dbsci.itmCobFor, 2, pProces, pEvalut  ) );
         hb_itemPutNS( pEvalut, hb_itemGetNS( pEvalut ) + 1 );
         if( fValid )
         {
            hb_itemPutNS( pProces, hb_itemGetNS( pProces ) + 1 );

            SELF_RECNO( pRawArea, &ulLockRecNo );
            dbLockInfo.itmRecID = hb_itemPutNL( dbLockInfo.itmRecID, ulLockRecNo );
            SELF_LOCK( pRawArea, &dbLockInfo );

            if( ! dbLockInfo.fResult )
            {
               fProved = HB_FALSE;
               break;
            }

            hb_arrayAdd( pRLocks, dbLockInfo.itmRecID );

            if( ( pEvalInfo->dbsci.itmRecID && hb_itemGetNL( pEvalInfo->dbsci.itmRecID ) ) ||
                ( lNext && --lNext < 1 ) )
               break;
         }

         SELF_SKIP( pRawArea, ! pEvalInfo->dbsci.fBackward ? 1 : -1 );
         if( ! pEvalInfo->dbsci.fBackward )
            SELF_EOF( pRawArea, &fEof );
         else
            SELF_BOF( pRawArea, &fEof );
      }

      SELF_RECNO( pRawArea, &ulNextRecNo );
      fValid = HB_TRUE;
      lNext = lNewNext;
      SELF_GOTO( pRawArea, ulNewRecNo );
      hb_itemRelease( dbLockInfo.itmRecID );
   }

   if( fValid && fProved )
   {
      PHB_ITEM pResult, pLast;
      HB_SIZE  nLen = 0;
      HB_BOOL  fAsArr, fEof;

      hb_itemPutNS( pProces, 0 );

      fAsArr = ( hb_itemType( pSaveValResult ) & HB_IT_ARRAY );
      if( fAsArr )
         nLen = hb_arrayLen( pSaveValResult );

      if( pRLocks )
      {
         if( hb_arrayLen( pRLocks ) )
         {
            SELF_GOTO( pRawArea, hb_arrayGetNL( pRLocks, 1 ) );
            fEof = HB_FALSE;
         }
         else
            fEof = HB_TRUE;
      }
      else
      {
         SELF_EOF( pRawArea, &fEof );
         if( ! fEof && pEvalInfo->dbsci.fBackward )
            SELF_BOF( pRawArea, &fEof );
      }
      if( ! fEof )
         hb_itemPutNS( pEvalut, 1 );

      while( ! fEof && ( pRLocks || ! pEvalInfo->dbsci.itmCobWhile || hb_itemGetL( hb_vmEvalBlockV( pEvalInfo->dbsci.itmCobWhile, 2, pProces, pEvalut ) ) ) )
      {
         if( ! pRLocks && pEvalInfo->dbsci.itmCobFor )  /* FOR */
            fValid = hb_itemGetL( hb_vmEvalBlockV( pEvalInfo->dbsci.itmCobFor, 2, pProces, pEvalut  ) );
         hb_itemPutNS( pEvalut, hb_itemGetNS( pEvalut ) + 1 );

         if( fValid )
         {
            hb_itemPutNS( pProces, hb_itemGetNS( pProces ) + 1 );
            if( fAsArr )
            {
               if( nLen )
                  pLast = hb_arrayGetItemPtr( pSaveValResult, nLen );
               else  /* first call */
                  pLast = hb_itemNew( NULL );
            }
            else
               pLast = pSaveValResult;

            pResult = hb_vmEvalBlockV( pEvalInfo->itmBlock, 2, pProces, pLast );

            if( fAsArr )
            {
               if( ++nLen == 1 )
                  hb_itemRelease( pLast );
               hb_arrayAdd( pSaveValResult, pResult );
            }
            else
               hb_itemCopy( pSaveValResult, pResult );

            if( ! pRLocks &&
                ( ( pEvalInfo->dbsci.itmRecID && hb_itemGetNL( pEvalInfo->dbsci.itmRecID ) ) || ( lNext && --lNext < 1 ) ) )
               break;
         }

         if( pRLocks )
         {
            if( ! pArea->pTable->uiUpdated )  /* data changes without RDD methods */
               pArea->pTable->uiUpdated |= LETO_FLAG_UPD_ALL;
            leto_PutRec( pArea );
            if( ( HB_SIZE ) hb_itemGetNL( pProces ) < hb_arrayLen( pRLocks ) )
               SELF_GOTO( pRawArea, hb_arrayGetNL( pRLocks, hb_itemGetNL( pProces ) + 1 ) );
            else
            {
               SELF_GOTO( pRawArea, ulNextRecNo );
               break;
            }
         }
         else
         {
            if( SELF_SKIP( pRawArea, ! pEvalInfo->dbsci.fBackward ? 1 : -1 ) != HB_SUCCESS )
               break;
            if( ! pEvalInfo->dbsci.fBackward )
               SELF_EOF( pRawArea, &fEof );
            else
               SELF_BOF( pRawArea, &fEof );
         }
      }

      leto_SetAreaFlags( pArea );
   }

   if( pRawArea )
   {
      if( pRLocks && hb_arrayLen( pRLocks ) )  /* RDDI_AUTOLOCK */
         SELF_UNLOCK( pRawArea, NULL );

      if( pRawArea->valResult )
      {
         hb_vmDestroyBlockOrMacro( pRawArea->valResult );
         pRawArea->valResult = NULL;
      }
      if( pSaveValResult )
      {
         pRawArea->valResult = hb_itemClone( pSaveValResult );
         hb_itemRelease( pSaveValResult );
      }
   }
   hb_itemRelease( pProces );
   hb_itemRelease( pEvalut );
   hb_itemRelease( pRLocks );

   return fValid ? HB_SUCCESS : HB_FAILURE;
}

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
static char * leto_PutTransInfo( LETOAREAP pArea, LETOAREAP pAreaDst, LPDBTRANSINFO pTransInfo, char * pData, PHB_ITEM * pArr )
{
   LETOTABLE *  pTable = pArea->pTable;
   char *       ptr = pData, * szForOpt = NULL, * szWhileOpt = NULL;
   const char * szFor = hb_itemGetCPtr( pTransInfo->dbsci.lpstrFor );
   const char * szWhile = hb_itemGetCPtr( pTransInfo->dbsci.lpstrWhile );
   HB_USHORT    uiIndex;

   if( pArr )
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
      HB_SIZE nLen = szFor ? strlen( szFor ) : 0;

      if( szFor && Leto_VarExprCreate( NULL, szFor, nLen, NULL, NULL ) )
      {
         *pArr = hb_itemArrayNew( 0 );
         szForOpt = ( char * ) hb_xgrab( nLen + 1 );
         Leto_VarExprCreate( pConnection, szFor, nLen, &szForOpt, *pArr );
         szFor = szForOpt;
      }
      nLen = szWhile ? strlen( szWhile ) : 0;
      if( szWhile && Leto_VarExprCreate( NULL, szWhile, nLen, NULL, NULL ) )
      {
         if( ! *pArr )
            *pArr = hb_itemArrayNew( 0 );
         szWhileOpt = ( char * ) hb_xgrab( nLen + 1 );
         Leto_VarExprCreate( pConnection, szWhile, nLen, &szWhileOpt, *pArr );
         szWhile = szWhileOpt;
      }
   }

   ptr += eprintf( pData, "%lu;%lu;%c;%lu;%s;%s;%lu;%lu;%c;%c;%c;%c;%c;%c;%c;%d;%d;",
                   pTable->hTable,
                   pTable->ulRecNo,
                   ( char ) ( ( hb_setGetDeleted() ) ? 'T' : 'F' ),
                   pAreaDst->pTable->hTable,
                   szFor,
                   szWhile,
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

   if( szForOpt )
      hb_xfree( szForOpt );
   if( szWhileOpt )
      hb_xfree( szWhileOpt );

   return ptr;
}

static PHB_ITEM leto_mkCodeBlock( const char * szExp, HB_ULONG ulLen )
{
   PHB_ITEM pBlock = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "leto_mkCodeBlock(%.*s, %lu)", ( int ) ulLen, szExp, ulLen ) );

   if( ulLen > 0 )
   {
      if( strlen( szExp ) < ulLen )
         ulLen = ( HB_ULONG ) strlen( szExp );
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

      hb_macroGetValue( hb_stackItemFromTop( -1 ), 0, 64 );  /* 64 = HB_SM_RT_MACRO */
      if( hb_vmRequestQuery() == 0 )
      {
         if( HB_IS_BLOCK( hb_stackItemFromTop( -1 ) ) )
            pBlock = hb_itemNew( hb_stackItemFromTop( -1 ) );
         hb_stackPop();
      }
   }

   return pBlock;
}

static HB_BOOL leto_ChkTransInfo( LPDBTRANSINFO pTransInfo, PHB_ITEM * pFor, PHB_ITEM * pWhile )
{
   HB_BOOL fValid = HB_TRUE;

   if( ! pTransInfo->dbsci.itmCobFor && pTransInfo->dbsci.lpstrFor )
   {
      pTransInfo->dbsci.itmCobFor = leto_mkCodeBlock( hb_itemGetCPtr( pTransInfo->dbsci.lpstrFor ),
                                                      hb_itemGetCLen( pTransInfo->dbsci.lpstrFor ) );
      if( ! pTransInfo->dbsci.itmCobFor )
         fValid = HB_FALSE;
      else
         *pFor = pTransInfo->dbsci.itmCobFor;
   }
   if( fValid && pTransInfo->dbsci.itmCobFor &&
       ! HB_IS_LOGICAL( hb_vmEvalBlockOrMacro( pTransInfo->dbsci.itmCobFor ) ) )
      fValid = HB_FALSE;

   if( fValid && ! pTransInfo->dbsci.itmCobWhile && pTransInfo->dbsci.lpstrWhile )
   {
      pTransInfo->dbsci.itmCobWhile = leto_mkCodeBlock( hb_itemGetCPtr( pTransInfo->dbsci.lpstrWhile ),
                                                        hb_itemGetCLen( pTransInfo->dbsci.lpstrWhile ) );
      if( ! pTransInfo->dbsci.itmCobWhile )
         fValid = HB_FALSE;
      else
         *pWhile = pTransInfo->dbsci.itmCobWhile;
   }
   if( fValid && pTransInfo->dbsci.itmCobWhile &&
       ! HB_IS_LOGICAL( hb_vmEvalBlockOrMacro( pTransInfo->dbsci.itmCobWhile ) ) )
      fValid = HB_FALSE;

   return fValid;
}

static void leto_CleanTransInfo( LPDBTRANSINFO pTransInfo, PHB_ITEM pFor, PHB_ITEM pWhile )
{
   if( pFor )
   {
      hb_vmDestroyBlockOrMacro( pTransInfo->dbsci.itmCobFor );
      pTransInfo->dbsci.itmCobFor = NULL;
   }
   if( pWhile )
   {
      hb_vmDestroyBlockOrMacro( pTransInfo->dbsci.itmCobWhile );
      pTransInfo->dbsci.itmCobWhile = NULL;
   }
}

static HB_ERRCODE letoSort( LETOAREAP pArea, LPDBSORTINFO pSortInfo )
{
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   LPDBTRANSINFO pTransInfo = &pSortInfo->dbtri;
   LETOAREAP     pAreaDst = ( LETOAREAP ) pTransInfo->lpaDest;
   HB_BOOL       fLetoAreaDst = leto_CheckArea( pAreaDst );
   HB_ERRCODE    errCode = HB_FAILURE;
   HB_ULONG      ulRecNo, ulLen;
   char *        pData, * ptr;
   HB_USHORT     uiIndex;
   PHB_ITEM      pArr = NULL;
#if ! defined( __XHARBOUR__ )
   HB_BOOL       fMemvarAllowed = hb_setGetForceOpt();
#else
   HB_BOOL       fMemvarAllowed = HB_FALSE;
#endif

   HB_TRACE( HB_TR_DEBUG, ( "letoSort(%p, %p)", pArea, pSortInfo ) );

   if( pArea->pTable->uiUpdated )
      leto_PutRec( pArea );
   if( fLetoAreaDst && pAreaDst->pTable->uiUpdated )
      leto_PutRec( pAreaDst );
   SELF_RECNO( ( AREAP ) pArea, &ulRecNo );

   if( ! pArea->area.dbfi.fFilter || pArea->area.dbfi.fOptimized )
      pTransInfo->dbsci.fOptimized = ! ( ( pTransInfo->dbsci.itmCobFor && ! pTransInfo->dbsci.lpstrFor ) ||
                                         ( pTransInfo->dbsci.itmCobWhile && ! pTransInfo->dbsci.lpstrWhile ) );
   if( pTransInfo->dbsci.fOptimized )
   {
      if( pTransInfo->dbsci.lpstrFor )
         pTransInfo->dbsci.fOptimized = Leto_VarExprTest( hb_itemGetCPtr( pTransInfo->dbsci.lpstrFor ), fMemvarAllowed );
      if( pTransInfo->dbsci.fOptimized && pTransInfo->dbsci.lpstrWhile )
         pTransInfo->dbsci.fOptimized = Leto_VarExprTest( hb_itemGetCPtr( pTransInfo->dbsci.lpstrWhile ), fMemvarAllowed );
   }
   if( ! fLetoAreaDst || ! pTransInfo->dbsci.fOptimized ||
       ( pArea->pTable->uiConnection != pAreaDst->pTable->uiConnection ) )
   {
      PHB_ITEM pFor = NULL, pWhile = NULL;
      HB_BOOL  fValid = leto_ChkTransInfo( pTransInfo, &pFor, &pWhile );

      if( fValid )
      {
         errCode = SUPER_SORT( ( AREAP ) pArea, pSortInfo );
         if( fLetoAreaDst && pAreaDst->pTable->uiUpdated )
            leto_PutRec( pAreaDst );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
      }
      leto_CleanTransInfo( pTransInfo, pFor, pWhile );

      return errCode;
   }

   ulLen = 92 + LETO_MAX_TAGNAME +
           hb_itemGetCLen( pTransInfo->dbsci.lpstrFor ) +
           hb_itemGetCLen( pTransInfo->dbsci.lpstrWhile ) +
           pTransInfo->uiItemCount * 16 + pSortInfo->uiItemCount * 16;

   pData = ( char * ) hb_xgrab( ulLen );

   pData[ 0 ] = LETOCMD_sort;
   pData[ 1 ] = ';';
   ptr = leto_PutTransInfo( pArea, pAreaDst, pTransInfo, pData + 2, fMemvarAllowed ? &pArr : NULL );

   ptr += eprintf( ptr, "%d;", pSortInfo->uiItemCount );

   for( uiIndex = 0; uiIndex < pSortInfo->uiItemCount; uiIndex++ )
   {
      ptr += eprintf( ptr, "%d,%d;",
                      pSortInfo->lpdbsItem[ uiIndex ].uiField,
                      pSortInfo->lpdbsItem[ uiIndex ].uiFlags );
   }

   if( pTransInfo->dbsci.fOptimized )
   {
      if( ! leto_SendRecv( pConnection, pArea, pData, ptr - pData, 0 ) )
         ptr = NULL;
      else
         ptr = leto_firstchar( pConnection );

      if( ptr && *( ptr - 1 ) == '+' )
         errCode = HB_SUCCESS;
      SELF_GOTO( ( AREAP ) pArea, ulRecNo );
   }
   if( pArr )
   {
      Leto_VarExprClear( pConnection, pArr );
      hb_itemRelease( pArr );
   }

   if( errCode == HB_FAILURE ) /* try expression is vaild at client */
   {
      if( pTransInfo->dbsci.lpstrFor || pTransInfo->dbsci.lpstrWhile ) /* CB */
      {
         PHB_ITEM pFor = NULL, pWhile = NULL;
         HB_BOOL  fValid = leto_ChkTransInfo( pTransInfo, &pFor, &pWhile );

         if( fValid )
         {
            errCode = SUPER_SORT( ( AREAP ) pArea, pSortInfo );
            if( fLetoAreaDst && pAreaDst->pTable->uiUpdated )
               leto_PutRec( pAreaDst );
            SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         }
         else
            commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, pTransInfo->dbsci.lpstrFor ?
                                                             hb_itemGetCPtr( pTransInfo->dbsci.lpstrFor ) :
                                                             hb_itemGetCPtr( pTransInfo->dbsci.lpstrWhile ) );

         leto_CleanTransInfo( pTransInfo, pFor, pWhile );
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
   HB_ULONG   ulRecNo;
   HB_ULONG   ulLen;
   PHB_ITEM   pArr = NULL;
#if ! defined( __XHARBOUR__ )
   HB_BOOL    fMemvarAllowed = hb_setGetForceOpt();
#else
   HB_BOOL    fMemvarAllowed = HB_FALSE;
#endif

   HB_TRACE( HB_TR_DEBUG, ( "letoTrans(%p, %p)", pArea, pTransInfo ) );

   if( pArea->pTable->uiUpdated )
      leto_PutRec( pArea );
   if( fLetoAreaDst && pAreaDst->pTable->uiUpdated )
      leto_PutRec( pAreaDst );
   SELF_RECNO( ( AREAP ) pArea, &ulRecNo );

   if( ( ( ! pArea->area.dbfi.fFilter || pArea->area.dbfi.fOptimized ) &&
         ( fLetoAreaDst && ( ! pAreaDst->area.dbfi.fFilter || pAreaDst->area.dbfi.fOptimized ) ) ) )
      pTransInfo->dbsci.fOptimized = ! ( ( pTransInfo->dbsci.itmCobFor && ! pTransInfo->dbsci.lpstrFor ) ||
                                         ( pTransInfo->dbsci.itmCobWhile && ! pTransInfo->dbsci.lpstrWhile ) );
   if( pTransInfo->dbsci.fOptimized )
   {
      if( pTransInfo->dbsci.lpstrFor )
         pTransInfo->dbsci.fOptimized = Leto_VarExprTest( hb_itemGetCPtr( pTransInfo->dbsci.lpstrFor ), fMemvarAllowed );
      if( pTransInfo->dbsci.fOptimized && pTransInfo->dbsci.lpstrWhile )
         pTransInfo->dbsci.fOptimized = Leto_VarExprTest( hb_itemGetCPtr( pTransInfo->dbsci.lpstrWhile ), fMemvarAllowed );
   }
   if( ! fLetoAreaDst || ! pTransInfo->dbsci.fOptimized ||
       ( pArea->pTable->uiConnection != pAreaDst->pTable->uiConnection ) )
   {
      PHB_ITEM pFor = NULL, pWhile = NULL;
      HB_BOOL  fValid = leto_ChkTransInfo( pTransInfo, &pFor, &pWhile );

      if( fValid )
      {
         errCode = SUPER_TRANS( ( AREAP ) pArea, pTransInfo );
         if( fLetoAreaDst && pAreaDst->pTable->uiUpdated )
            leto_PutRec( pAreaDst );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
      }
      leto_CleanTransInfo( pTransInfo, pFor, pWhile );

      return errCode;
   }

   ulLen = 82 + LETO_MAX_TAGNAME +
           hb_itemGetCLen( pTransInfo->dbsci.lpstrFor ) +
           hb_itemGetCLen( pTransInfo->dbsci.lpstrWhile ) +
           pTransInfo->uiItemCount * 16;

   pData = ( char * ) hb_xgrab( ulLen );
   pData[ 0 ] = LETOCMD_trans;
   pData[ 1 ] = ';';
   ptr = leto_PutTransInfo( pArea, pAreaDst, pTransInfo, pData + 2, fMemvarAllowed ? &pArr : NULL );

   if( pTransInfo->dbsci.fOptimized )
   {
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
      }
      SELF_GOTO( ( AREAP ) pArea, ulRecNo );
      pArea->pTable->ptrBuf = NULL;
   }
   if( pArr )
   {
      Leto_VarExprClear( pConnection, pArr );
      hb_itemRelease( pArr );
   }

   if( errCode == HB_FAILURE ) /* try expression is vaild at client */
   {
      if( pTransInfo->dbsci.lpstrFor || pTransInfo->dbsci.lpstrWhile ) /* CB */
      {
         PHB_ITEM pFor = NULL, pWhile = NULL;
         HB_BOOL  fValid = leto_ChkTransInfo( pTransInfo, &pFor, &pWhile );

         if( fValid )
         {
            errCode = SUPER_TRANS( ( AREAP ) pArea, pTransInfo );
            if( fLetoAreaDst && pAreaDst->pTable->uiUpdated )
               leto_PutRec( pAreaDst );
            SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         }
         else
            commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, pTransInfo->dbsci.lpstrFor ?
                                                             hb_itemGetCPtr( pTransInfo->dbsci.lpstrFor ) :
                                                             hb_itemGetCPtr( pTransInfo->dbsci.lpstrWhile ) );

         leto_CleanTransInfo( pTransInfo, pFor, pWhile );
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
      LETOTABLE * pTable = pArea->pTable;

      errCode = SUPER_CLEARREL( ( AREAP ) pArea );
      if( errCode == HB_SUCCESS && pTable )
      {
         LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

         if( pConnection->uiServerMode >= 3 )
         {
            char     szData[ 17 ];
            HB_ULONG ulLen;

            ulLen = eprintf( szData, "%c;%lu;02;", LETOCMD_rela, pArea->pTable->hTable );
            if( ! leto_SendRecv( pConnection, pArea, szData, ulLen, 1026 ) )
               errCode = HB_FAILURE;
         }
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

/* can fail at server because of CB with a user function/ local variable, ignored if ! FORCEOPT */
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
      hb_itemRelease( pRelInf->abKey );
      hb_itemRelease( pRelInf->itmCobExpr );
      commonError( pArea, EG_SYNTAX, 1020, 0, NULL, 0, "Cyclic relation detected" );

      return HB_FAILURE;
   }

   errCode = SUPER_SETREL( ( AREAP ) pArea, pRelInf );
   if( errCode == HB_SUCCESS )
   {
      if( pConnection->uiServerMode >= 3 &&  /* only possible in No_Save_WA = 1 */
          leto_CheckArea( ( LETOAREAP ) hb_rddGetWorkAreaPointer( pRelInf->lpaChild->uiArea ) ) )
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
               pRelInf->isOptimized = ! leto_CheckError( pArea, pConnection );
            else
               errCode = HB_FAILURE;
         }

         if( errCode == HB_SUCCESS && pRelInf->isOptimized )
         {
            LPDBRELINFO lpdbRelations = ( ( AREAP ) pArea )->lpdbRelations;

            while( lpdbRelations->lpdbriNext )  /* last one is ours */
            {
               lpdbRelations = lpdbRelations->lpdbriNext;
            }
            lpdbRelations->isOptimized = HB_TRUE;
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
   if( ! *szIFileName )
      return HB_SUCCESS;
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

   if( pArea->area.lpdbRelations )
      return SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return HB_SUCCESS;
}

static HB_BOOL letoProdSupport( void )
{
   LPRDDNODE pRDDNode;
   HB_USHORT uiRddID;
   HB_BOOL   fSupportStruct = HB_FALSE;

   pRDDNode = hb_rddFindNode( "LETO", &uiRddID );
   if( pRDDNode )
   {
      PHB_ITEM pItem = hb_itemPutC( NULL, NULL );

      if( SELF_RDDINFO( pRDDNode, RDDI_STRUCTORD, 0, pItem ) == HB_SUCCESS )
         fSupportStruct = hb_itemGetL( pItem );
      hb_itemRelease( pItem );
   }

   return fSupportStruct;
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
         if( ! pTagInfo->fProduction || ! letoProdSupport() || ! hb_setGetAutOpen() )
         {
            pTag1 = pTagInfo;
            if( pTagInfo == pTable->pTagInfo )
               pTagInfo = pTable->pTagInfo = pTagInfo->pNext;
            else
               pTagInfo = pTagPrev->pNext = pTagInfo->pNext;

            if( pTable->pTagCurrent == pTag1 )
               pTable->pTagCurrent = NULL;
            LetoDbFreeTag( pTag1 );
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

      if( ! pTagInfo->fProduction || ! letoProdSupport() || ! hb_setGetAutOpen() )
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
               LetoDbFreeTag( pTag1 );
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
      pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, pTable->pTagCurrent->TagName );

   if( pOrderInfo->itmOrder )
   {
      if( HB_IS_STRING( pOrderInfo->itmOrder ) )
      {
         char szTag[ LETO_MAX_TAGNAME + 1 ];

         hb_strncpyUpper( szTag, hb_itemGetCPtr( pOrderInfo->itmOrder ),
                                 HB_MIN( hb_itemGetCLen( pOrderInfo->itmOrder ), LETO_MAX_TAGNAME ) );
         return LetoDbOrderFocus( pTable, szTag, 0 );
      }
      else if( HB_IS_NUMERIC( pOrderInfo->itmOrder ) )
         return LetoDbOrderFocus( pTable, NULL, ( HB_USHORT ) hb_itemGetNI( pOrderInfo->itmOrder ) );
   }

   return HB_SUCCESS;
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
                 ( lpdbOrdCondInfo->fUseFilter ? LETO_INDEX_FILT : 0 ) |
                 ( lpdbOrdCondInfo->fUseCurrent ? LETO_INDEX_USEI : 0 );

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
   leto_SetAreaFlags( pArea );  /* leto_ParseRecord() done in LetoDbOrderCreate() */

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

   return HB_SUCCESS;
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
            LetoDbFreeTag( pTagInfo );
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
   LETOCONNECTION * pConnection;
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
            hb_strncpy( szTagName, hb_itemGetCPtr( pOrderInfo->itmOrder ), LETO_MAX_TAGNAME );
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
   else if( uiIndex == DBOI_BAGEXT )
   {
      pConnection = letoGetCurrConn();
      if( pConnection && *( pConnection->szDriver ) )
      {
         if( strstr( pConnection->szDriver, "NTX" ) != NULL )
            hb_itemPutC( pOrderInfo->itmResult, ".ntx" );
         else if( strstr( pConnection->szDriver, "NSX" ) != NULL )
            hb_itemPutC( pOrderInfo->itmResult, ".nsx" );
         else if( strstr( pConnection->szDriver, "FPT" ) == NULL )
            hb_itemPutC( pOrderInfo->itmResult, ".cdx" );
         else
            hb_itemPutC( pOrderInfo->itmResult, "" );
      }
      else
         hb_itemPutC( pOrderInfo->itmResult, "" );
      return HB_SUCCESS;
   }
   else
     return HB_FAILURE;

   pConnection = letoGetConnPool( pTable->uiConnection );
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
            LetoDbRecCount( pTable, ( unsigned long * ) &ul );
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
                  leto_AddKeyToBuf( pData, szKey, uiKeyLen, ( unsigned long * ) &ulLen );
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
         pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, pTable->szOrderExt );
         break;

      case DBOI_NUMBER:
         hb_itemPutNI( pOrderInfo->itmResult, pTagInfo ? uiTag : 0 );
         break;

      case DBOI_ISMULTITAG:
      case DBOI_MULTIKEY:
      case DBOI_ISCOND:
      {
         HB_BOOL fResult;

         if( ! pTagInfo )
            fResult = HB_FALSE;
         else
         {
            const char * ptr;

            eprintf( szData, "%c;%lu;%d;%s;", LETOCMD_dboi, pTable->hTable, uiIndex, pTagInfo->TagName );
            if( ! leto_SendRecv( pConnection, pArea, szData, 0, 1021 ) )
               return HB_FAILURE;

            ptr = leto_firstchar( pConnection );
            fResult = ( *ptr == 'T' );
         }
         pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, fResult );
         break;
      }

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
            char **       pBagNames = ( char ** ) hb_xgrab( sizeof( char * ) * pTable->uiOrders );
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
            leto_AddKeyToBuf( pData, szKey, uiKeyLen, ( unsigned long * ) &ulLen );
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
            int          iBagOrders = 0;
            HB_USHORT    uiLen = ( HB_USHORT ) hb_itemGetCLen( pOrderInfo->atomBagName );
            const char * ptr2;

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

   if( pArea->area.dbfi.itmCobExpr )
   {
      hb_vmDestroyBlockOrMacro( pArea->area.dbfi.itmCobExpr );
      pArea->area.dbfi.itmCobExpr = NULL;
   }

   if( pTable )
   {
      pTable->ptrBuf = NULL;
      if( pArea->area.dbfi.fFilter && pArea->area.dbfi.fOptimized )  /* else no filter at server */
      {
         if( LetoDbClearFilter( pTable ) )
            return HB_FAILURE;
#if ! defined(  __XHARBOUR__ )
         if( pTable->pFilterVar )
         {
            LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

            Leto_VarExprClear( pConnection, pTable->pFilterVar );
            hb_itemRelease( pTable->pFilterVar );
            pTable->pFilterVar = NULL;
         }
#endif
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
         PHB_ITEM pFilter = NULL;
         HB_BOOL  fCanOptimize = HB_TRUE;
#if ! defined( __XHARBOUR__ )
         HB_BOOL  fMemvarAllowed = hb_setGetForceOpt();
#else
         HB_BOOL  fMemvarAllowed = HB_FALSE;
#endif

         if( ! Leto_VarExprTest( hb_itemGetCPtr( pFilterInfo->abFilterText ), fMemvarAllowed ) )
            fCanOptimize = HB_FALSE;

#if ! defined( __XHARBOUR__ )
         if( fCanOptimize && hb_setGetForceOpt() )
         {
            LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );
            HB_SIZE nLen = hb_itemGetCLen( pFilterInfo->abFilterText );

            /* first test for memvar in expression */
            if( Leto_VarExprCreate( NULL, hb_itemGetCPtr( pFilterInfo->abFilterText ), nLen, NULL, NULL ) )
            {
               PHB_ITEM pArr = hb_itemArrayNew( 0 );
               char *   szFilter = ( char * ) hb_xgrab( nLen + 1 );

               pFilter = hb_itemPutCL( pFilter, hb_itemGetCPtr( pFilterInfo->abFilterText ), nLen );
               Leto_VarExprCreate( pConnection, hb_itemGetCPtr( pFilterInfo->abFilterText ), nLen, &szFilter, pArr );
               pFilterInfo->abFilterText = hb_itemPutC( pFilterInfo->abFilterText, szFilter );
               pTable->pFilterVar = pArr;
               hb_xfree( szFilter );
            }
         }
#endif

         if( fCanOptimize && ! fMemvarAllowed )
         {
            PHB_ITEM pBlock = leto_mkCodeBlock( hb_itemGetCPtr( pFilterInfo->abFilterText ),
                                                hb_itemGetCLen( pFilterInfo->abFilterText ) );

            if( ! pBlock || ! HB_IS_LOGICAL( hb_vmEvalBlockOrMacro( pBlock ) ) )
               fCanOptimize = HB_FALSE;  /* don't try at server */
            hb_itemRelease( pBlock );
         }

         if( fCanOptimize && ! LetoDbSetFilter( pTable, hb_itemGetCPtr( pFilterInfo->abFilterText ), hb_setGetForceOpt() ) )
         {
            /* server can evaluate expression -> optimized */
            if( pFilterInfo->itmCobExpr )  /* remove a given codeblock */
               pFilterInfo->itmCobExpr = NULL;
            pFilterInfo->fOptimized = HB_TRUE;
         }
         else
         {
            LETOCONNECTION * pConnection = letoGetConnPool( pTable->uiConnection );

            if( pFilter )  /* reset to unmodified expression */
            {
               pFilterInfo->abFilterText = hb_itemPutC( pFilterInfo->abFilterText, hb_itemGetCPtr( pFilter ) );
               hb_itemRelease( pFilter );
            }
            if( pTable->pFilterVar )
            {
               hb_itemRelease( pTable->pFilterVar );
               pTable->pFilterVar = NULL;
            }
            pFilterInfo->fOptimized = HB_FALSE;
            if( pConnection->iError )
            {
               commonError( pArea, EG_SYNTAX, pConnection->iError, 0, NULL, 0, hb_itemGetCPtr( pFilterInfo->abFilterText ) );
               return HB_FAILURE;
            }
         }
      }

      if( ! pFilterInfo->fOptimized )
      {
         /* prefere at client-side a given CB, else try to create */
         if( ! pFilterInfo->itmCobExpr && hb_itemGetCLen( pFilterInfo->abFilterText ) )
            pFilterInfo->itmCobExpr = leto_mkCodeBlock( hb_itemGetCPtr( pFilterInfo->abFilterText ), hb_itemGetCLen( pFilterInfo->abFilterText ) );
         if( ! pFilterInfo->itmCobExpr || ! HB_IS_LOGICAL( hb_vmEvalBlockOrMacro( pFilterInfo->itmCobExpr ) ) )
         {
            commonError( pArea, EG_SYNTAX, 1026, 0, NULL, 0,
                         pFilterInfo->abFilterText ? hb_itemGetCPtr( pFilterInfo->abFilterText ) : NULL );
            return HB_FAILURE;
         }

         ( ( AREAP ) pArea )->dbfi.itmCobExpr = hb_itemNew( pFilterInfo->itmCobExpr );
      }

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
   {
      HB_BOOL fInstant = HB_FALSE;

      if( ! ( pTable->uiUpdated & LETO_FLAG_UPD_APPEND ) && ( uiAction == REC_UNLOCK || uiAction == FILE_UNLOCK ) )
      {
         pConnection = letoGetConnPool( pTable->uiConnection );
         if( ! pConnection->fTransActive )
         {
            fInstant = HB_TRUE;
            pTable->uiUpdated |= LETO_FLAG_UPD_FLUSH;
            if( uiAction == FILE_UNLOCK || ( ulRecNo && ulRecNo == pTable->ulRecNo ) )
               pTable->uiUpdated |= LETO_FLAG_UPD_UNLOCK;
         }
      }

      leto_PutRec( pArea );

      if( fInstant )
      {
         if( uiAction == REC_UNLOCK )
         {
            if( ulRecNo == pTable->ulRecNo )
               pTable->fRecLocked = HB_FALSE;
            return HB_SUCCESS;
         }
         else if( uiAction == FILE_UNLOCK )
         {
            pTable->fFLocked = pTable->fRecLocked = HB_FALSE;
            pTable->ulLocksMax = 0;
            return HB_SUCCESS;
         }
      }
   }

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
         pConnection = letoGetConnPool( pTable->uiConnection );
         if( ! pConnection->fTransForce && LetoDbRecUnLock( pTable, ulRecNo ) )
         {
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
         pConnection = letoGetConnPool( pTable->uiConnection );
         if( ! pConnection->fTransForce &&
            ( pTable->fFLocked || pTable->fRecLocked || pTable->ulLocksMax ) && LetoDbFileUnLock( pTable ) )
         {
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
   LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
   LETOTABLE * pTable = pArea->pTable;
   HB_ULONG    ulRecNo = hb_itemGetNL( pRecNo );
   HB_ERRCODE  errCode;

   HB_TRACE( HB_TR_DEBUG, ( "letoUnLock(%p, %p)", pArea, pRecNo ) );

   if( pConnection->fTransForce )
      return HB_SUCCESS;  /* keep the lock */

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
      HB_ERRCODE errCode = LetoDbDrop( pConnection, szTFileName, szIFileName );

      if( pConnection->iError == EDBF_SHARED )
         hb_rddSetNetErr( HB_TRUE );
      return errCode;
   }

   return HB_FAILURE;
}

static HB_ERRCODE letoExists( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, HB_ULONG ulConnect )
{
   LETOCONNECTION * pConnection;
   char szTFileName[ HB_PATH_MAX ];
   char szIFileName[ HB_PATH_MAX ];
   const char * szTableFile, * szIndexFile;

   HB_TRACE( HB_TR_DEBUG, ( "letoExists(%p, %p, %p, %lu)", pRDD, pItemTable, pItemIndex, ulConnect ) );

   HB_SYMBOL_UNUSED( pRDD );
   HB_SYMBOL_UNUSED( ulConnect );

   szTableFile = hb_itemGetCPtr( pItemTable );
   szIndexFile = hb_itemGetCPtr( pItemIndex );

   pConnection = leto_OpenConn( NULL, szTableFile, szTFileName );
   pConnection = leto_OpenConn( pConnection, szIndexFile, szIFileName );
   if( pConnection != NULL )
      return LetoDbExists( pConnection, szTFileName, szIFileName );

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

      if( leto_SendRecv( pConnection, NULL, szData, ulLen, 0 ) )
      {
         if( *( leto_firstchar( pConnection ) ) == 'T' )
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

#if defined( __HARBOUR30__ )
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
   HB_TRACE( HB_TR_DEBUG, ( "letoExit(%p)", pRDD ) );

   HB_SYMBOL_UNUSED( pRDD );

   if( letoGetConnPool( 0 ) )
   {
      unsigned int i;

      for( i = 0; i < letoGetConnCount(); i++ )
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

static HB_USHORT leto_MemoType( LETOCONNECTION * pConnection )
{
   HB_USHORT uiMemoType;

   if( pConnection )
   {
      if( pConnection->uiMemoType )
         uiMemoType = pConnection->uiMemoType;
      else if( *pConnection->szDriver )
      {
         if( strstr( pConnection->szDriver, "NTX" ) != NULL )
            uiMemoType = DB_MEMO_DBT;
         else if( strstr( pConnection->szDriver, "CDX" ) != NULL )
            uiMemoType = DB_MEMO_FPT;
         else
            uiMemoType = DB_MEMO_SMT;
      }
      else if( pConnection->uiDriver == LETO_NTX )
         uiMemoType = DB_MEMO_DBT;
      else
         uiMemoType = DB_MEMO_FPT;
   }
   else
      uiMemoType = DB_MEMO_FPT;

   return uiMemoType;
}

static HB_USHORT leto_MemoBlocksize( LETOCONNECTION * pConnection )
{
   HB_USHORT uiMemoBlocksize;

   if( pConnection && pConnection->uiMemoBlocksize )
      uiMemoBlocksize = pConnection->uiMemoBlocksize;
   else if( pConnection )
   {
      switch( leto_MemoType( pConnection ) )
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

   if( pConnection->uiLockSchemeExtend )  /* extended or normal ? */
   {
      if( pConnection->uiDriver == LETO_NTX )
#if defined( __HARBOUR30__ )  /* does not know DB_DBFLOCK_CLIPPER2 */
         iLockScheme = DB_DBFLOCK_HB32;
#else
         iLockScheme = DB_DBFLOCK_CLIPPER2;
#endif
      else if( pConnection->uiLockSchemeExtend == DB_DBFLOCK_HB64 )
         iLockScheme = DB_DBFLOCK_HB64;
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

static HB_ERRCODE letoRddInfo( LPRDDNODE pRDD, HB_USHORT uiIndex, unsigned int uiConnect, PHB_ITEM pItem )
{
   LETOCONNECTION * pConnection = ( uiConnect > 0 && uiConnect <= letoGetConnCount() ) ?
                                    letoGetConnPool( uiConnect - 1 ) : letoGetCurrConn();
   HB_TRACE( HB_TR_DEBUG, ( "letoRddInfo(%p, %hu, %d, %p)", pRDD, uiIndex, uiConnect, pItem ) );

   switch( uiIndex )
   {
      case RDDI_REMOTE:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case RDDI_CONNECTION:
      {
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

      case RDDI_MEMOEXT:  /* by intention not setable ! */
      {
         char szExt[ HB_MAX_FILE_EXT + 1 ] = { 0 };

         if( pConnection && *pConnection->szMemoExt )
            hb_strncpy( szExt, pConnection->szMemoExt, HB_MAX_FILE_EXT );
         else if( pConnection )
         {
            switch( leto_MemoType( pConnection ) )
            {
               case DB_MEMO_DBT:
                  strcpy( szExt, ".dbt" );
                  break;

               case DB_MEMO_FPT:
                  strcpy( szExt, ".fpt" );
                  break;

               case DB_MEMO_SMT:
                  strcpy( szExt, ".smt" );
                  break;
            }
         }

         hb_itemPutC( pItem, szExt );
         break;
      }

      case RDDI_MEMOTYPE:
      {
         HB_USHORT uiType = leto_MemoType( pConnection );

         if( HB_IS_NUMERIC( pItem ) && pConnection )
         {
            HB_USHORT uiMemoType = ( HB_USHORT ) hb_itemGetNI( pItem );

            if( pConnection->uiMemoType != uiMemoType )
            {
               switch( uiMemoType )
               {
                  case DB_MEMO_DBT:
                     pConnection->uiMemoType = uiMemoType;
                     strcpy( pConnection->szMemoExt, ".dbt" );
                     pConnection->uiMemoBlocksize = 512;
                     break;

                  case DB_MEMO_FPT:
                     pConnection->uiMemoType = uiMemoType;
                     strcpy( pConnection->szMemoExt, ".fpt" );
                     pConnection->uiMemoBlocksize = 64;
                     break;

                  case DB_MEMO_SMT:
                     pConnection->uiMemoType = uiMemoType;
                     strcpy( pConnection->szMemoExt, ".smt" );
                     pConnection->uiMemoBlocksize = 32;
                     break;
               }
            }
         }

         hb_itemPutNI( pItem, uiType );
         break;
      }

      case RDDI_MEMOBLOCKSIZE:
      {
         HB_USHORT uiSize = leto_MemoBlocksize( pConnection );

         if( HB_IS_NUMERIC( pItem ) && hb_itemGetNI( pItem ) > 0 && pConnection )
         {
            /* validated by client */
            pConnection->uiMemoBlocksize = ( HB_USHORT ) hb_itemGetNI( pItem );
         }

         hb_itemPutNI( pItem, uiSize );
         break;
      }

      case RDDI_LOCKSCHEME:
         if( pConnection )
            hb_itemPutNI( pItem, leto_LockScheme( pConnection ) );
         else
            hb_itemPutNI( pItem, 0 );
         break;

      case RDDI_TABLEEXT:
         hb_itemPutC( pItem, ".dbf" );
         break;

      /* boolean ask/ set from server */
      case RDDI_MULTITAG:
      case RDDI_OPTIMIZE:
      case RDDI_FORCEOPT:
      case RDDI_AUTOOPEN:
      case RDDI_AUTOLOCK:
      case RDDI_STRUCTORD:
      case RDDI_DBEVALCOMPAT:
      {
         int iRes = 1;

         if( pConnection )
         {
            const char * szNewSet = ( HB_IS_LOGICAL( pItem ) ? ( hb_itemGetL( pItem ) ? "T" : "F" ) : NULL );

            if( ! szNewSet && uiIndex == RDDI_AUTOLOCK )
            {
               hb_itemPutL( pItem, ( pConnection->uSrvLock & 0x01 ) ? HB_TRUE : HB_FALSE );
               iRes = 0;
            }
            else if( LetoRddInfo( pConnection, uiIndex, szNewSet ) == HB_SUCCESS )
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

                        case RDDI_AUTOLOCK:
                           if( hb_itemGetL( pItem ) )
                              pConnection->uSrvLock |= 0x01;
                           else
                              pConnection->uSrvLock &= ~( 0x01 );
                           break;

                        case RDDI_DBEVALCOMPAT:
                           pConnection->fDbEvalCompat = hb_itemGetL( pItem );
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
      case RDDI_LOCKRETRY:  /* not used in Harbour core, used as RDDI_LOCKTIMEOUT */
      case RDDI_LOCKTIMEOUT:
      {
         int iRes = 1;

         if( pConnection )
         {
            char *  szNum = NULL;

            if( uiIndex == RDDI_LOCKRETRY )
               uiIndex = RDDI_LOCKTIMEOUT;
            if( uiIndex == RDDI_LOCKTIMEOUT && pConnection->uiServerMode < 3 )
            {
               hb_itemPutNI( pItem, 0 );
               break;
            }

            if( HB_IS_NUMERIC( pItem ) )
            {
               szNum = ( char * ) hb_xgrab( 21 );
               if( ( hb_itemGetNI( pItem ) < 0 ) )
                  hb_itemPutNI( pItem, 0 );
               eprintf( szNum, "%d", hb_itemGetNI( pItem ) );
               if( uiIndex == RDDI_AUTOORDER )
                  hb_setSetItem( HB_SET_AUTORDER, pItem );
               else if( uiIndex == RDDI_LOCKTIMEOUT )
                  pConnection->iLockTimeOut = hb_itemGetNI( pItem );
            }
            if( LetoRddInfo( pConnection, uiIndex, szNum ) == HB_SUCCESS )
            {
               const char * ptr = leto_firstchar( pConnection );

               if( *( ptr - 1 ) == '+' )
               {
                  hb_itemPutNL( pItem, strtoul( ptr, NULL, 10 ) );
                  iRes = 0;
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

      case RDDI_BUFKEYNO:
      case RDDI_BUFKEYCOUNT:
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

      case RDDI_TRIGGER:
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

      case RDDI_VERSION:
         hb_itemPutC( pItem, LETO_VERSION_STRING );
         break;

      case RDDI_CLEARBUFFER:
         if( pConnection )
            hb_rddIterateWorkAreas( leto_ClearBuffer, ( void * ) pConnection );
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, uiConnect, pItem );
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
                                      ( DBENTRYP_RVVVL ) letoRename,
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

static HB_ERRCODE leto_FindField( AREAP pArea, void * p )
{
   if( leto_CheckArea( ( LETOAREAP ) pArea ) )
   {
      LETOAREAP      pLetoArea = ( LETOAREAP ) pArea;
      FINDAREASTRU * psArea = ( FINDAREASTRU * ) p;

      if( pLetoArea->pTable->uiConnection == psArea->uiConnection )
      {
         LETOCONNECTION * pConnection = letoGetConnPool( psArea->uiConnection );

         if( hb_rddFieldIndex( pArea, hb_itemGetCPtr( pConnection->whoCares ) ) )
         {
            psArea->ulAreaID = hb_rddGetCurrentWorkAreaNumber();
            psArea->pArea = pLetoArea;
            return HB_FAILURE;  /* to stop iterating */
         }
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

   HB_TRACE( HB_TR_DEBUG, ( "leto_ClearTransBuffers(%d)", ( int ) hb_parl( 1 ) ) );

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
      {
         hb_rddIterateWorkAreas( leto_UnLockRec, ( void * ) pConnection );
         pConnection->fTransForce = HB_TRUE;
      }
#endif
      pConnection->fTransActive = HB_TRUE;
   }
}

static void leto_ClearTransBuffers( LETOCONNECTION * pConnection )
{
   HB_TRACE( HB_TR_DEBUG, ( "leto_ClearTransBuffers(%p)", pConnection ) );

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
      pConnection->szBuffer = ( char * ) hb_xrealloc( pConnection->szBuffer, LETO_SENDRECV_BUFFSIZE + 1 );
   }

   pConnection->pRecsNotList.ulRecNo = pConnection->pRecsNotList.hTable = 0;
   pConnection->ulRecsInList = pConnection->uiTransAppend = 0;
   pConnection->ulTransDataLen = pConnection->ulRecsInTrans = pConnection->uiTransAppLen = 0;
}

HB_FUNC( LETO_ROLLBACK )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();

   HB_TRACE( HB_TR_DEBUG, ( "LETO_ROLLBACK()" ) );

   if( ! leto_CheckTrans( pArea, HB_TRUE ) )
      commonError( pArea, EG_SYNTAX, 1031, 0, NULL, 0, NULL );
   else
   {
      LETOCONNECTION * pConnection = letoGetConnPool( pArea->pTable->uiConnection );
      char szData[ 8 ];

      pConnection->fTransActive = HB_FALSE;
      pConnection->fTransForce = HB_FALSE;
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
   pConnection->fTransForce = HB_FALSE;

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

static int leto_FieldInValue( const char * ptr, char * szFieldname )
{
   int iLen = 0;

   szFieldname[ 0 ] = '\0';
   while( *ptr == ' ' || *ptr == '(' )
      ptr++;
   if( HB_ISFIRSTIDCHAR( *ptr ) )
   {
      const char * ptr2 = strstr( ptr, "->" );

      if( ptr2 && ptr2 - ptr <= HB_RDD_MAX_ALIAS_LEN )
      {
         while( ptr + ++iLen < ptr2 )
         {
            if( ! HB_ISNEXTIDCHAR( *( ptr + iLen ) ) )
            {
               iLen = 0;
               break;
            }
         }
         if( iLen )
         {
            ptr = ptr2 + 2;
            iLen = 0;
         }
      }

      szFieldname[ 0 ] = HB_TOUPPER( *ptr );
      while( ++iLen < 10 )
      {
         if( *( ptr + iLen ) == '.' && HB_ISFIRSTIDCHAR( *ptr + iLen + 1 ) )  /* SQL style ALIAS, max 9 char */
         {
            ptr = ptr + iLen + 1;
            szFieldname[ 0 ] = HB_TOUPPER( *ptr );
            iLen = 0;
            continue;
         }
         else if( ! HB_ISNEXTIDCHAR( *( ptr + iLen ) ) )
         {
            szFieldname[ iLen ] = '\0';
            iLen--;
            break;
         }

         szFieldname[ iLen ] = HB_TOUPPER( *( ptr + iLen ) );
      }
      if( iLen >= 10 )
      {
         iLen = 0;
         szFieldname[ 0 ] = '\0';
      }
      else if( iLen )
         iLen++;
   }

   return iLen;
}

static HB_BOOL leto_IsUDFAllowed( LETOCONNECTION * pConnection, const char * szCommand )
{
   HB_BOOL fUDFAllowed = HB_FALSE;

   if( pConnection && pConnection->fUDFAllowed )
      fUDFAllowed = pConnection->fUDFAllowed;
   else if( pConnection )
   {
      char     szData[ HB_SYMBOL_NAME_LEN + 1 ];
      HB_ULONG ulLen;

      ulLen = eprintf( szData, "%c;3;;;%s;", LETOCMD_udf_fun, szCommand ? szCommand : "LETO_DBTOTAL" );
      if( leto_SendRecv( pConnection, NULL, szData, ulLen, 0 ) && *( leto_firstchar( pConnection ) ) == 'T' )
         fUDFAllowed = HB_TRUE;
      pConnection->fUDFAllowed = fUDFAllowed;
   }

   return fUDFAllowed;
}

static HB_BOOL leto_LockValidate( LETOAREAP pArea, char * szBlock, HB_SIZE nLen )
{
   HB_SIZE nPos = hb_strAtI( "LOCK(", 5, szBlock, nLen );
   HB_SIZE nSearchLen, nReplLen, nCount = 0;
   HB_BOOL fFileLock = HB_FALSE;
   HB_BOOL fTestLock = HB_TRUE, fTestUnlock = HB_TRUE;

   HB_TRACE( HB_TR_DEBUG, ( "leto_LockValidate(%p, %s, %ld)", pArea, szBlock, nLen ) );

   while( nPos )
   {
      if( fTestLock )
      {
         fTestLock = HB_FALSE;
         if( hb_strAtI( "Leto_RecLock(", 13, szBlock, nLen ) )
            nCount++;
         if( hb_strAtI( "Leto_TableLock(", 15, szBlock, nLen ) )
            nCount += 999;
      }

      nSearchLen = 7;
      nPos = hb_strAtI( "DBRLOCK(", nSearchLen + 1, szBlock, nLen );
      if( ! nPos )
      {
         nSearchLen = 5;
         nPos = hb_strAtI( "RLOCK(", nSearchLen + 1, szBlock, nLen );
         if( ! nPos )
         {
            nPos = hb_strAtI( "FLOCK(", nSearchLen + 1, szBlock, nLen );
            if( nPos )
               fFileLock = HB_TRUE;
         }
      }
      if( nPos )
      {
         if( fFileLock )
         {
            nCount += 999;
            nReplLen = 14;  /* "Leto_TableLock" */
         }
         else
         {
            nCount++;
            nReplLen = 12;  /* "Leto_RecLock" */
         }
      }
      else
      {
         if( fTestUnlock )
         {
            fTestUnlock = HB_FALSE;
            if( hb_strAtI( "Leto_RecUnlock(", 15, szBlock, nLen ) && nCount )
               nCount--;
            if( hb_strAtI( "Leto_TableUnlock(", 17, szBlock, nLen ) )
               nCount = 0;
         }

         nSearchLen = 9;
         nPos = hb_strAtI( "DBRUNLOCK(", nSearchLen + 1, szBlock, nLen );
         if( nPos )
         {
            if( nCount )
               nCount--;
            nReplLen = 14;  /* "Leto_RecUnlock" */
         }
         else
         {
            nSearchLen = 8;
            nPos = hb_strAtI( "DBUNLOCK(", nSearchLen + 1, szBlock, nLen );
            if( nPos )
            {
               nCount = 0;
               nReplLen = 16;  /* "Leto_TableUnlock" */
            }
            else
               nReplLen = 0;  /* and done! */
         }
      }

      if( nPos && nReplLen )
      {
         memmove( szBlock + nPos + nSearchLen + ( nReplLen - nSearchLen ) - 1, szBlock + nPos + nSearchLen - 1, nLen - ( nPos + nSearchLen ) + 1 );
         nLen += ( nReplLen - nSearchLen );
         if( nReplLen == 12 )
            memcpy( szBlock + nPos - 1, "Leto_RecLock", nReplLen );
         else if( nReplLen == 14 )
         {
            if( fFileLock )
            {
               memcpy( szBlock + nPos - 1, "Leto_TableLock", nReplLen );
               fFileLock = HB_FALSE;
            }
            else
               memcpy( szBlock + nPos - 1, "Leto_RecUnlock", nReplLen );
         }
         else  /* 16 */
            memcpy( szBlock + nPos - 1, "Leto_TableUnlock", nReplLen );
      }
   }
   if( nCount )
      commonError( pArea, EG_ARG, EDBCMD_BADPARAMETER, 0, NULL, 0, "Unresolved Locking in Expression" );

   return ! nCount ? HB_TRUE : HB_FALSE;
}

static HB_BOOL leto_RelOptimized( LETOAREAP pArea )
{
   LPDBRELINFO lpDbRel = ( ( AREAP ) pArea )->lpdbRelations;
   HB_BOOL     fOptimized = HB_TRUE;

   if( ! ( ! pArea->area.dbfi.fFilter || pArea->area.dbfi.fOptimized ) )
      fOptimized = HB_FALSE;
   while( lpDbRel && fOptimized )
   {
      if( ! lpDbRel->isOptimized )
         fOptimized = HB_FALSE;
      else
         fOptimized = leto_RelOptimized( ( LETOAREAP ) lpDbRel->lpaChild );
      lpDbRel = lpDbRel->lpdbriNext;
   }

   return fOptimized;
}

HB_FUNC( LETO_DBEVALTEST )
{
   LETOCONNECTION * pConnection = NULL;
   LETOAREAP        pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL          fOptimized = HB_FALSE;

   if( pArea && leto_CheckArea( pArea ) && pArea->pTable )
      pConnection = letoGetConnPool( pArea->pTable->uiConnection );

   if( pConnection )
   {
      LetoDbEval( pArea->pTable, NULL, hb_parc( 1 ), hb_parc( 2 ),
                  -1, -1, 0, HB_FALSE, HB_FALSE, HB_FALSE, HB_TRUE, NULL, NULL );
      if( strncmp( pConnection->szBuffer, "-004", 4 ) )
         fOptimized = HB_TRUE;
   }

   hb_retl( fOptimized );
}

/* leto_DbEval( cbBlock[, cbFor, cbWhile, nNext, nRec, lRest, lResultArr, lNeedLock, lBackward, lStay ] ) */
HB_FUNC( LETO_DBEVAL )
{
   LETOCONNECTION * pConnection = NULL;
   const char *     szFor = ! HB_ISARRAY( 2 ) ? hb_parc( 2 ) : hb_arrayGetCPtr( hb_stackItemFromBase( 2 ), 2 );
   const char *     szWhile = ! HB_ISARRAY( 3 ) ? hb_parc( 3 ) : hb_arrayGetCPtr( hb_stackItemFromBase( 3 ), 2 );
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL   fOptimized = HB_FALSE;
   HB_LONG   lNext = hb_parnldef( 4, -1 );
   HB_ULONG  ulRecNo = hb_parnldef( 5, 0 );  /* -1, if given checked later */
   HB_BOOL   fRest = hb_parldef( 6, hb_param( 3, HB_IT_BLOCK | HB_IT_STRING ) || lNext >= 0 ? HB_TRUE : HB_FALSE );
   HB_BOOL   fResultAsArr = hb_parldef( 7, HB_ISBYREF( 7 ) ? HB_TRUE : HB_FALSE );
   HB_BOOL   fNeedLock = hb_parldef( 8, HB_FALSE );
   HB_BOOL   fBackward = hb_parldef( 9, HB_FALSE );
   HB_BOOL   fStay = hb_parldef( 10, HB_TRUE );
   HB_BOOL   fValid = pArea ? HB_TRUE : HB_FALSE;
   PHB_ITEM  pRefresh = NULL;
   HB_ULONG  ulLastRecNo = 0;
   char *    szBlock = NULL;
   char *    szForOpt = NULL, * szWhileOpt = NULL;
   PHB_ITEM  pForVar = NULL, pWhileVar = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "LETO_DBEVAL(%s, %s, %s, %ld, %ld, %d, %d, %d)",  hb_parc( 1 ), szFor, szWhile,
                            lNext, HB_ISNUM( 5 ) ? ( HB_LONG ) ulRecNo : -1, ( int ) fRest, ( int ) fResultAsArr, ( int ) fNeedLock ) );

   if( szFor && ! *szFor )
      szFor = NULL;
   if( szWhile && ! *szWhile )
      szWhile = NULL;
   if( fValid && leto_CheckArea( pArea ) && pArea->pTable )
      pConnection = letoGetConnPool( pArea->pTable->uiConnection );

   if( fValid && hb_parclen( 1 ) && ( szFor || ! HB_ISBLOCK( 2 ) ) && ( szWhile || ! HB_ISBLOCK( 3 ) ) &&
       leto_CheckArea( pArea ) )
   {
      /* none or optimized filter, none or all optimzed relations */
      if( ! ( ! pArea->area.dbfi.fFilter || pArea->area.dbfi.fOptimized ) )
         fOptimized = HB_FALSE;
      else if( ( ( AREAP ) pArea )->lpdbRelations )
         fOptimized = leto_RelOptimized( pArea );
      else
         fOptimized = HB_TRUE;

      if( fOptimized && ( szFor || szWhile ) )
      {
#if ! defined( __XHARBOUR__ )
         HB_BOOL fMemvarAllowed = hb_setGetForceOpt();
#else
         HB_BOOL fMemvarAllowed = HB_FALSE;
#endif

         if( szFor && ! Leto_VarExprTest( szFor, fMemvarAllowed ) )
            fOptimized = HB_FALSE;
         if( fOptimized && szWhile && ! Leto_VarExprTest( szWhile, fMemvarAllowed ) )
            fOptimized = HB_FALSE;
      }

      if( fOptimized && ( szFor || szWhile ) )
      {
#if ! defined( __XHARBOUR__ )
         if( hb_setGetForceOpt() )
         {
            /* test for memvar in FOR/ WHILE expressions */
            if( szFor && Leto_VarExprCreate( NULL, szFor, strlen( szFor ), NULL, NULL ) )
            {
               pForVar = hb_itemArrayNew( 0 );
               szForOpt = ( char * ) hb_xgrab( strlen( szFor ) + 1 );
               Leto_VarExprCreate( pConnection, szFor, strlen( szFor ), &szForOpt, pForVar );
            }
            if( szWhile && Leto_VarExprCreate( NULL, szWhile, strlen( szWhile ), NULL, NULL ) )
            {
               pWhileVar = hb_itemArrayNew( 0 );
               szWhileOpt = ( char * ) hb_xgrab( strlen( szWhile ) + 1 );
               Leto_VarExprCreate( pConnection, szWhile, strlen( szWhile ), &szWhileOpt, pWhileVar );
            }
         }
#endif
         /* pre-test without block for FOR and WHILE */
         LetoDbEval( pArea->pTable, NULL, szForOpt ? szForOpt : szFor, szWhileOpt ? szWhileOpt : szWhile, lNext, -1, -1,
                     fResultAsArr, fNeedLock, fBackward, fStay, NULL, NULL );
         if( ! strncmp( pConnection->szBuffer, "-004", 4 ) )  /* error in for or while expression */
            fOptimized = HB_FALSE;
         else
         {
            if( szForOpt )
               szFor = szForOpt;
            if( szWhileOpt )
               szWhile = szWhileOpt;
         }
      }
   }
   if( fValid && ! ( hb_parclen( 1 ) || HB_ISBLOCK( 1 ) ) )
      fValid = HB_FALSE;

   if( fValid )  /* WA & first param */
   {
      SELF_RECNO( ( AREAP ) pArea, &ulLastRecNo );

      if( hb_parclen( 1 ) )
      {
         HB_SIZE      nBlockSize = HB_MAX( HB_PATH_MAX, hb_parclen( 1 ) + 8 + ( ( hb_parclen( 1 ) / 8 ) * 10 ) );
         char         szSearch[ 16 ] = { 'F', 'I', 'E', 'L', 'D', 'P', 'U', 'T', '(', '\'', 0 };
         const char * ptr, * ptr2;

         szBlock = ( char * ) hb_xgrabz( nBlockSize );
         strcpy( szBlock, hb_parc( 1 ) );
         /* translate Harbour lock functions to LetoDBf variant & verify unlock */
         fValid = leto_LockValidate( pArea, szBlock, hb_parclen( 1 ) );

         if( fValid && fOptimized && fNeedLock )  /* collect ALIAS of WA to refresh */
         {
            char    szMasterAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
            HB_SIZE n, nMasterLen = 0, nAliasLen;

            ptr = strstr( szBlock, "->" );
            if( ptr )
            {
               SELF_ALIAS( ( AREAP ) pArea, szMasterAlias );
               nMasterLen = strlen( szMasterAlias );
            }
            while( ptr )
            {
               nAliasLen = 0;
               while( ptr - nAliasLen - 1 >= szBlock )
               {
                  if( ! HB_ISNEXTIDCHAR( *( ptr - nAliasLen - 1 ) ) )
                     break;
                  nAliasLen++;
               }

               if( nAliasLen && ( nMasterLen != nAliasLen || hb_strnicmp( ptr - nAliasLen, szMasterAlias, nAliasLen ) ) )
               {
                  if( ! pRefresh )
                     pRefresh = hb_itemArrayNew( 0 );

                  for( n = 1; n <= hb_arrayLen( pRefresh ); n++ )
                  {
                     if( hb_arrayGetCLen( pRefresh, n ) == nAliasLen &&
                         ! hb_strnicmp( ptr - nAliasLen, hb_arrayGetCPtr( pRefresh, n ), nAliasLen ) )
                        break;
                  }
                  if( n > hb_arrayLen( pRefresh ) )
                  {
                     hb_arraySize( pRefresh, n );
                     hb_arraySetCL( pRefresh, n, ptr - nAliasLen, nAliasLen );
                  }
               }

               ptr = strstr( ptr + 2, "->" );
            }
         }

         ptr = strstr( szBlock, "=>" );
         if( ptr )  /* check for empty '' ( or PP repeated ) hashkey to substitute */
         {
            char    szNewKey[ 16 ] = { 0 };
            char    szLastKey[ HB_SYMBOL_NAME_LEN + 1 ] = { 0 };
            char *  pptr;
            HB_SIZE nKey = 0;
            int     iKeyLen;

            while( ptr )
            {
               iKeyLen = 0;
               if( *( ptr - 1 ) == ' ' )
                  ptr2 = ptr - 2;
               else
                  ptr2 = ptr - 1;

               if( *ptr2 == '\'' )  /* look up for the other '' */
               {
                  ptr2--;
                  while( ptr2 > szBlock && *ptr2 != '\'' && HB_ISNEXTIDCHAR( *ptr2 ) )
                  {
                     iKeyLen++;
                     ptr2--;
                  }
               }

               if( *ptr2 == '\'' )
               {
                  nKey++;
                  pptr = szBlock + ( ptr2 - szBlock ) + 1;

                  if( iKeyLen && iKeyLen <= HB_SYMBOL_NAME_LEN )  /* a given key-name */
                  {
                     /* note: PP repeat last keyname, if at least two are given, but less as key-values .. */
                     if( nKey > 1 && ( int ) strlen( szLastKey ) == iKeyLen && ! strncmp( szLastKey, pptr, iKeyLen ) )
                     {
                        memmove( pptr, pptr + iKeyLen, strlen( pptr + iKeyLen ) + 1 );
                        ptr -= iKeyLen;
                        iKeyLen = 0;
                     }
                     else
                     {
                        memcpy( szLastKey, pptr, iKeyLen );
                        szLastKey[ iKeyLen ] = '\0';
                     }
                  }

                  if( ! iKeyLen )  /* key-name to create */
                  {
                     iKeyLen = leto_FieldInValue( ptr + 2, szNewKey );
                     if( ! iKeyLen )
                        iKeyLen = ultostr( nKey, szNewKey );

                     memmove( pptr + iKeyLen, pptr, strlen( pptr ) + 1 );
                     memcpy( pptr, szNewKey, iKeyLen );
                     ptr += iKeyLen;
                  }
               }

               ptr = strstr( ptr + 2, "=>" );
            }

            if( ! leto_CbTrim( szBlock ) )
               fValid = HB_FALSE;
         }
         else if( strstr( szBlock, "{ * }" ) )  /* "{|n| { * } }" add all fields as hashtable */
         {
            HB_USHORT    ui, uiCount;
            char         szField[ 16 ];
            HB_SIZE      nHPos = 24;

            SELF_FIELDCOUNT( ( AREAP ) pArea, &uiCount );
            if( uiCount )
               memcpy( szBlock, "{|n| {'recno'=>RecNo(), ", 24 );
            for( ui = 1; ui <= uiCount; ui++ )
            {
               SELF_FIELDNAME( ( AREAP ) pArea, ui, szField );
               if( nHPos > nBlockSize - 32 ) /* 28 worst case */
               {
                  nBlockSize += HB_PATH_MAX;
                  szBlock = ( char * ) hb_xrealloc( szBlock, nBlockSize );
               }
               nHPos += eprintf( szBlock + nHPos, "%c%c%s%c => %s", ui == 1 ? ' ' : ',','"', szField, '"', szField );
            }
            strcpy( szBlock + nHPos, " } }" );
         }
         else if( strstr( szBlock, szSearch ) )  /* "FIELDPUT('" replace field-name with field-num */
         {
            char      szField[ 16 ];
            int       iKeyLen, iKeyDiff;
            char *    pptr;
            HB_USHORT uiField;
            HB_ULONG  ulLen;

            ptr = strstr( szBlock, szSearch );
            while( ptr )
            {
               iKeyLen = 0;
               if( *( ptr + 9 ) == '\'' )
               {
                  while( *( ptr + 10 + iKeyLen ) != '\'' )
                  {
                     if( ! *( ptr + 10 + iKeyLen ) )
                     {
                        iKeyLen = 0;
                        break;
                     }
                     iKeyLen++;
                  }
                  if( iKeyLen && iKeyLen < 16 )
                  {
                     memcpy( szField, ptr + 10, iKeyLen );
                     szField[ iKeyLen ] = '\0';
                     uiField = hb_rddFieldIndex( ( AREAP ) pArea, szField );

                     if( uiField )
                     {
                        pptr = szBlock + ( ptr - szBlock ) + 9;
                        ulLen = ultostr( uiField, szField );
                        iKeyDiff = iKeyLen + 2 - ( int ) ulLen;
                        if( iKeyDiff != 0 )  /* < 0 --> move right */
                           memmove( pptr + iKeyLen + 2 - iKeyDiff, pptr + iKeyLen + 2, strlen( pptr + iKeyLen + 2 ) + 1 );
                        memcpy( pptr, szField, ulLen );
                        ptr -= iKeyDiff;
                     }
                  }
               }

               ptr = strstr( ptr + 2, szSearch );
            }
         }
      }
   }

   if( fValid && fOptimized )
   {
      PHB_ITEM pParams = NULL;
      HB_LONG  lRecNo;
      int      iRest;

      if( pArea->pTable && pArea->pTable->uiUpdated )
         leto_PutRec( pArea );

      if( HB_ISNUM( 5 ) )
         lRecNo = ( HB_LONG ) ulRecNo;
      else
         lRecNo = -1;
      if( HB_ISLOG( 6 ) )
         iRest = fRest ? 1 : 0;
      else
         iRest = -1;

      do
      {
         if( LetoDbEval( pArea->pTable, szBlock, szFor, szWhile, lNext, lRecNo, iRest,
                         fResultAsArr, fNeedLock, fBackward, fStay, &pParams, hb_parc( 11 ) ) )
         {
            hb_itemRelease( pParams );
            if( ! strncmp( pConnection->szBuffer + 1, "004", 3 ) )  /* error in expression ? */
            {
               leto_CheckError( pArea, pConnection );
               break;
            }
            else if( fNeedLock && ! strncmp( pConnection->szBuffer + 1, "003", 3 ) )  /* not all locked */
            {
               if( ! fValid && fNeedLock )
                  hb_rddSetNetErr( HB_TRUE );
               if( commonError( pArea, EG_UNLOCKED, EDBF_UNLOCKED, 0, NULL, EF_CANDEFAULT | EF_CANRETRY, NULL ) != E_RETRY )
                  break;
            }
            else
            {
               hb_retl( HB_FALSE );
               break;
            }
         }
         else
         {
            if( HB_ISBYREF( 7 ) )
               hb_itemCopy( hb_itemUnRef( hb_stackItemFromBase( 7 ) ), pParams );
            hb_itemReturnRelease( pParams );
            leto_SetAreaFlags( pArea );
            break;
         }
      }
      while( HB_TRUE );

      if( pRefresh )  /* fNeedLock & sub WAs to refresh */
      {
         LETOAREAP pSubArea = NULL;
         int       iSubArea = 0;
         HB_SIZE   n;

         for( n = 1; n <= hb_arrayLen( pRefresh ); n++ )
         {
            hb_rddGetAliasNumber( hb_arrayGetCPtr( pRefresh, n ), &iSubArea );
            if( iSubArea )
               pSubArea = ( LETOAREAP ) hb_rddGetWorkAreaPointer( ( HB_AREANO ) iSubArea );
            if( pSubArea )
               SELF_INFO( ( AREAP ) pSubArea, DBI_CLEARBUFFER, NULL );
         }
      }
   }
   else if( fValid )  /* but not optimized */
   {
      DBEVALINFO  pEvalInfo;
      LETOTABLE * pTable = pArea->pTable;
      AREAP       pRawArea = ( AREAP ) pArea;
      PHB_ITEM    pBlock = NULL, pFor = NULL, pWhile = NULL;
      PHB_ITEM    pNext = NULL, pRec = NULL, pRest = NULL;
      HB_BYTE     uOldSrvMode = pConnection ? pConnection->uSrvLock : 0;

      memset( &pEvalInfo, 0, sizeof( pEvalInfo ) );
      if( szBlock )
         pEvalInfo.itmBlock = pBlock = leto_mkCodeBlock( szBlock, strlen( szBlock ) );
      else
         pEvalInfo.itmBlock = hb_param( 1, HB_IT_BLOCK );
      if( HB_ISBLOCK( 2 ) )
         pEvalInfo.dbsci.itmCobFor = hb_param( 2, HB_IT_BLOCK );
      else if( HB_ISARRAY( 2 ) && ( hb_itemType( hb_arrayGetItemPtr( hb_stackItemFromBase( 2 ), 1 ) ) & HB_IT_BLOCK ) )
         pEvalInfo.dbsci.itmCobFor = hb_arrayGetItemPtr( hb_stackItemFromBase( 2 ), 1 );
      else if( szFor )
         pEvalInfo.dbsci.itmCobFor = pFor = leto_mkCodeBlock( szFor, strlen( szFor ) );
      if( HB_ISBLOCK( 3 ) )
         pEvalInfo.dbsci.itmCobWhile = hb_param( 3, HB_IT_BLOCK );
      else if( HB_ISARRAY( 3 ) && ( hb_itemType( hb_arrayGetItemPtr( hb_stackItemFromBase( 3 ), 1 ) ) & HB_IT_BLOCK ) )
         pEvalInfo.dbsci.itmCobWhile = hb_arrayGetItemPtr( hb_stackItemFromBase( 3 ), 1 );
      else if( szWhile )
         pEvalInfo.dbsci.itmCobWhile = pWhile = leto_mkCodeBlock( szWhile, strlen( szWhile ) );

      if( HB_ISNUM( 4 ) )
         pNext = hb_itemPutNL( pNext, lNext );
      pEvalInfo.dbsci.lNext = pNext;
      if( HB_ISNUM( 5 ) )
         pRec = hb_itemPutNL( pRec, ulRecNo );
      pEvalInfo.dbsci.itmRecID = pRec;
      if( HB_ISLOG( 6 ) )
         pRest = hb_itemPutL( pRest, fRest );
      pEvalInfo.dbsci.fRest = pRest;
      pEvalInfo.dbsci.fOptimized = HB_FALSE;
      pEvalInfo.dbsci.fBackward = fBackward;

      if( pRawArea->valResult )
         hb_vmDestroyBlockOrMacro( pRawArea->valResult );
      if( fResultAsArr && leto_CheckArea( pArea ) )
         pRawArea->valResult = hb_itemArrayNew( 0 );
      else
         pRawArea->valResult = hb_itemNew( NULL );

      if( fNeedLock && leto_CheckArea( pArea ) )
      {
         if( pTable->fReadonly )
            fValid = HB_FALSE;
         else if( ! ( pTable->fFLocked || ! pTable->fShared ) )
         {
            if( ! ( uOldSrvMode & 0x01 ) )  /* RDDI_AUTOLOCK */
               fValid = HB_FALSE;
         }
         else
            fNeedLock = HB_FALSE;
      }
      if( ! fNeedLock && ( uOldSrvMode & 0x01 ) )
         pConnection->uSrvLock &= ~( 0x01 );  /* deactivate as hint for RDD method */

      while( fValid )
      {
         fValid = SELF_DBEVAL( pRawArea, &pEvalInfo ) == HB_SUCCESS;
         if( ! fValid && fNeedLock )
            hb_rddSetNetErr( HB_TRUE );
         if( ! fValid && fNeedLock && commonError( pArea, EG_UNLOCKED, EDBF_UNLOCKED, 0, NULL, EF_CANDEFAULT | EF_CANRETRY, NULL ) == E_RETRY )
            fValid = HB_TRUE;
         else
            break;
      }

      if( pConnection )
         pConnection->uSrvLock = uOldSrvMode;

      if( ! fValid )
         hb_retl( HB_FALSE );
      else if( leto_CheckArea( pArea ) )
      {
         if( ( hb_itemType( pRawArea->valResult ) & HB_IT_NIL ) )
            hb_retl( HB_TRUE );
         else
         {
            if( HB_ISBYREF( 7 ) )
               hb_itemCopy( hb_itemUnRef( hb_stackItemFromBase( 7 ) ), pRawArea->valResult );
            hb_itemReturn( pRawArea->valResult );
         }
      }
      else
         hb_ret();
      if( pRawArea->valResult )
      {
         hb_itemRelease( pRawArea->valResult );
         pRawArea->valResult = NULL;
      }

      /* AFTER! ->valResult is processed */
      if( ! fStay || ! fValid )
         SELF_GOTO( pRawArea, ulLastRecNo );

      hb_itemRelease( pBlock );
      hb_itemRelease( pFor );
      hb_itemRelease( pWhile );
      hb_itemRelease( pNext );
      hb_itemRelease( pRec );
      hb_itemRelease( pRest );
   }
   else
      commonError( ( LETOAREAP ) pArea, EG_ARG, EDBCMD_BADPARAMETER, 0, NULL, 0, "LETO_DBEVAL" );

   if( pForVar )
   {
      Leto_VarExprClear( pConnection, pForVar );
      hb_itemRelease( pForVar );
      hb_xfree( szForOpt );
   }
   if( pWhileVar )
   {
      Leto_VarExprClear( pConnection, pWhileVar );
      hb_itemRelease( pWhileVar );
      hb_xfree( szWhileOpt );
   }
   if( szBlock )
      hb_xfree( szBlock );
   hb_itemRelease( pRefresh );
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

static PHB_ITEM leto_paramtype( PHB_ITEM pItem, int iSub, long lMask )
{
   if( pItem )
   {
      if( ( hb_itemType( pItem ) & HB_IT_ARRAY ) && iSub >= 1 )
         pItem = hb_arrayGetItemPtr( pItem, iSub );

      if( ( hb_itemType( pItem ) & ( HB_TYPE ) lMask ) || ( HB_TYPE ) lMask == HB_IT_ANY )
         return pItem;
   }

   return NULL;
}

static HB_ERRCODE leto_DbArrange( PHB_ITEM pDestArea, PHB_ITEM pStruct, PHB_ITEM pFor, PHB_ITEM pWhile,
                                  PHB_ITEM pNext, PHB_ITEM pRecord, PHB_ITEM pRest, PHB_ITEM pFields )
{
   HB_ERRCODE errCode = HB_FAILURE;
   AREAP      pSrcArea, pDstArea;
   int        iDstArea;

   HB_TRACE( HB_TR_DEBUG, ( "leto_DbArrange(%p, ...)", pDestArea ) );

   pSrcArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   iDstArea = hb_itemGetNI( pDestArea );
   if( ! iDstArea && ( hb_itemType( pDestArea ) & HB_IT_STRING ) )
   {
      hb_rddGetAliasNumber( hb_parc( 1 ), &iDstArea );
      hb_itemPutNI( pDestArea, iDstArea );
   }
   pDstArea = ( AREAP ) hb_rddGetWorkAreaPointer( ( HB_AREANO ) iDstArea );

   if( pSrcArea && pDstArea && pSrcArea != pDstArea )
   {
      DBSORTINFO dbSortInfo;
      LPDBTRANSINFO pTransInfo;

      memset( &dbSortInfo, 0, sizeof( dbSortInfo ) );
      pTransInfo = &dbSortInfo.dbtri;
      errCode = hb_dbTransStruct( pSrcArea, pDstArea, pTransInfo, NULL, pStruct );
      if( errCode == HB_SUCCESS )
      {
         PHB_ITEM pTransItm;

         pTransInfo->dbsci.itmCobFor   = leto_paramtype( pFor, 1, HB_IT_BLOCK );
         pTransInfo->dbsci.lpstrFor    = leto_paramtype( pFor, 2, HB_IT_STRING );
         pTransInfo->dbsci.itmCobWhile = leto_paramtype( pWhile, 1, HB_IT_BLOCK );
         pTransInfo->dbsci.lpstrWhile  = leto_paramtype( pWhile, 2, HB_IT_STRING );
         pTransInfo->dbsci.lNext       = pNext;
         pTransInfo->dbsci.itmRecID    = pRecord;
         pTransInfo->dbsci.fRest       = pRest;
         pTransInfo->dbsci.fOptimized  = ! ( pTransInfo->dbsci.itmCobFor ||
                                           pTransInfo->dbsci.itmCobWhile );

         /* do not transfer record deleted flag to destination area */
         dbSortInfo.dbtri.uiFlags |= DBTF_RECALL;

         dbSortInfo.uiItemCount = pFields ? ( HB_USHORT ) hb_arrayLen( pFields ) : 0;
         if( dbSortInfo.uiItemCount > 0 )
         {
            HB_USHORT uiCount, uiDest;
            char * szFieldLine;
            HB_SIZE nSize = 0;

            dbSortInfo.lpdbsItem = ( LPDBSORTITEM ) hb_xgrab( dbSortInfo.uiItemCount * sizeof( DBSORTITEM ) );
            for( uiCount = 1; uiCount <= dbSortInfo.uiItemCount; ++uiCount )
            {
               HB_SIZE nLine = hb_arrayGetCLen( pFields, uiCount );
               if( nLine > nSize )
                  nSize = nLine;
            }
            szFieldLine = ( char * ) hb_xgrab( nSize + 1 );
            for( uiDest = 0, uiCount = 1; uiCount <= dbSortInfo.uiItemCount; ++uiCount )
            {
               char * szPos;
               dbSortInfo.lpdbsItem[ uiDest ].uiFlags = 0;
               hb_strncpyUpper( szFieldLine, hb_arrayGetCPtr( pFields, uiCount ),
                                hb_arrayGetCLen( pFields, uiCount ) );
               szPos = strchr( szFieldLine, '/' );
               if( szPos )
               {
                  *szPos++ = 0;
                  if( strchr( szPos, 'D' ) > strchr( szPos, 'A' ) )
                     dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_DESCEND;
                  else
                     dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_ASCEND;
                  if( strchr( szPos, 'C' ) != NULL )
                     dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_CASE;
               }
               else
                  dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_ASCEND;

               dbSortInfo.lpdbsItem[ uiDest ].uiField = hb_rddFieldExpIndex( pSrcArea, szFieldLine );
               /* Field found */
               if( dbSortInfo.lpdbsItem[ uiDest ].uiField != 0 )
                  ++uiDest;
            }
            dbSortInfo.uiItemCount = uiDest;
            hb_xfree( szFieldLine );
         }

#if ! defined( __HARBOUR30__ )
         /* see remark in LETO_DBTRANS() what happens here */
         pTransItm = hb_dbTransInfoPut( NULL, pTransInfo );
         errCode = SELF_INFO( pTransInfo->lpaDest, DBI_TRANSREC, pTransItm );
#else
         pTransItm = NULL;
#endif
         if( errCode == HB_SUCCESS )
         {
            errCode = pTransInfo->uiItemCount == 0 ? HB_FAILURE :
                      ( dbSortInfo.uiItemCount == 0 ?
                        SELF_TRANS( pSrcArea, pTransInfo ) :
                        SELF_SORT( pSrcArea, &dbSortInfo ) );
#if ! defined( __HARBOUR30__ )
            SELF_INFO( dbSortInfo.dbtri.lpaDest, DBI_TRANSREC, pTransItm );
            if( errCode == HB_SUCCESS && ( pTransInfo->uiFlags & DBTF_CPYCTR ) )
               errCode = hb_dbTransCounters( pTransInfo );
#endif
         }
         hb_itemRelease( pTransItm );
      }

      if( dbSortInfo.lpdbsItem )
         hb_xfree( dbSortInfo.lpdbsItem );
      if( dbSortInfo.dbtri.lpTransItems )
         hb_xfree( dbSortInfo.dbtri.lpTransItems );
   }

   return errCode;
}

static LETOCONNECTION * leto_CheckForIP( const char * szSource, char * szFile )
{
   LETOCONNECTION * pConnection = NULL;
   char szAddr[ 96 ];
   int  iPort = 0;

   HB_TRACE( HB_TR_DEBUG, ( "leto_CheckForIP(%s, %s)", szSource, szFile ) );

   if( leto_getIpFromPath( szSource, szAddr, &iPort, szFile ) )
   {
       if( ( pConnection = leto_ConnectionFind( szAddr, iPort ) ) == NULL )
          pConnection = LetoConnectionNew( szAddr, iPort, NULL, NULL, 0, LETO_USE_THREAD );
   }
   else
      hb_strncpy( szFile, leto_RemoveIpFromPath( szSource ), HB_PATH_MAX - 1 );

   return pConnection;
}

static PHB_ITEM leto_param( int iParam, int iSub, long lMask )
{
   if( iParam >= 1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( ( hb_itemType( pItem ) & HB_IT_ARRAY ) && iSub >= 1 )
         pItem = hb_arrayGetItemPtr( pItem, iSub );

      if( ( hb_itemType( pItem ) & ( HB_TYPE ) lMask ) || ( HB_TYPE ) lMask == HB_IT_ANY )
         return pItem;
   }

   return NULL;
}

/* __dbSort( cToFileName, aFields, cbFor, cbWhile, nNext, nRecord, lRest, cRDD, nConnection, cCodePage */
HB_FUNC( LETO_DBSORT )
{
   HB_ERRCODE errCode = HB_FAILURE;
   LETOAREAP  pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   PHB_ITEM   pFields = hb_param( 2, HB_IT_ARRAY );

   HB_TRACE( HB_TR_DEBUG, ( "LETO_DBSORT(%s, ...)", hb_parc( 1 ) ) );

   if( pArea && hb_parclen( 1 ) && pFields && hb_arrayLen( pFields ) )
   {
      PHB_ITEM pStruct = hb_itemArrayNew( 0 );
      int      iSrcArea = hb_rddGetCurrentWorkAreaNumber();

      hb_tblStructure( ( AREAP ) pArea, pStruct, DBS_ALEN );
      if( hb_arrayLen( pStruct ) )
      {
         char szFile[ HB_PATH_MAX ] = { 0 };

         leto_CheckForIP( hb_parc( 1 ), szFile );
         /* blank! ALIAS "" to create temporary ALIAS not derived from filename */
         errCode = hb_rddCreateTable( szFile, hb_parc( 8 ), 0, "", HB_TRUE,
                                      hb_parc( 10 ), hb_parnl( 9 ), pStruct, NULL );
         if( errCode == HB_SUCCESS )
         {
            PHB_ITEM pDstArea = hb_itemPutNI( NULL, hb_rddGetCurrentWorkAreaNumber() );
            PHB_ITEM pFor    = hb_param( 3, HB_IT_BLOCK | HB_IT_STRING | HB_IT_ARRAY );
            PHB_ITEM pWhile  = hb_param( 4, HB_IT_BLOCK | HB_IT_STRING | HB_IT_ARRAY );
            PHB_ITEM pNext   = hb_param( 5, HB_IT_NUMERIC );
            PHB_ITEM pRecord = hb_param( 6, HB_IT_NUMERIC );
            PHB_ITEM pRest   = hb_param( 7, HB_IT_LOGICAL );

            hb_rddSelectWorkAreaNumber( iSrcArea );
            errCode = leto_DbArrange( pDstArea, pStruct, pFor, pWhile, pNext, pRecord, pRest, pFields );
            if( hb_rddSelectWorkAreaNumber( hb_itemGetNI( pDstArea ) ) == HB_SUCCESS )
               hb_rddReleaseCurrentArea();
            hb_itemRelease( pDstArea );
         }
      }
      hb_itemRelease( pStruct );

      hb_rddSelectWorkAreaNumber( iSrcArea );
   }
   else
      commonError( ( LETOAREAP ) pArea, EG_ARG, EDBCMD_BADPARAMETER, 0, NULL, 0, "LETO_DBSORT" );

   hb_retl( errCode == HB_SUCCESS );
}

/* __dbArrange( cnToArea, aStruct, cbFor, cbWhile, nNext, nRecord, lRest, aFields ) */
HB_FUNC( LETO_DBARRANGE )
{
   HB_ERRCODE errCode;
   PHB_ITEM   pDstArea, pStruct, pFor, pWhile, pNext, pRecord, pRest, pFields;

   HB_TRACE( HB_TR_DEBUG, ( "LETO_DBARRANGE(%s[%d], ...)", hb_parc( 1 ), hb_parni( 1 ) ) );

   pDstArea = hb_param( 1, HB_IT_NUMERIC | HB_IT_STRING );
   pStruct  = hb_param( 2, HB_IT_ARRAY );
   pFor     = hb_param( 3, HB_IT_BLOCK | HB_IT_STRING | HB_IT_ARRAY );
   pWhile   = hb_param( 4, HB_IT_BLOCK | HB_IT_STRING | HB_IT_ARRAY );
   pNext    = hb_param( 5, HB_IT_NUMERIC );
   pRecord  = hb_param( 6, HB_IT_NUMERIC );
   pRest    = hb_param( 7, HB_IT_LOGICAL );
   pFields  = hb_param( 8, HB_IT_ARRAY );
   errCode  = leto_DbArrange( pDstArea, pStruct, pFor, pWhile, pNext, pRecord, pRest, pFields );

   hb_retl( errCode == HB_SUCCESS );
}

/* __dbTrans( cnDstArea, aFields, cbFor, cbWhile, nNext, nRecord, lRest ) */
HB_FUNC( LETO_DBTRANS )
{
   AREAP   pSrcArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   AREAP   pDstArea = NULL;
   HB_UINT uiDstArea = 0;

   HB_TRACE( HB_TR_DEBUG, ( "LETO_DBTRANS(%s[%d], ...)", hb_parc( 1 ), hb_parni( 1 ) ) );

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
         PHB_ITEM pTransItm;

         dbTransInfo.dbsci.itmCobFor   = leto_param( 3, 1, HB_IT_BLOCK );
         dbTransInfo.dbsci.lpstrFor    = leto_param( 3, 2, HB_IT_STRING );
         dbTransInfo.dbsci.itmCobWhile = leto_param( 4, 1, HB_IT_BLOCK );
         dbTransInfo.dbsci.lpstrWhile  = leto_param( 4, 2, HB_IT_STRING );
         dbTransInfo.dbsci.lNext       = hb_param( 5, HB_IT_NUMERIC );
         dbTransInfo.dbsci.itmRecID    = hb_param( 6, HB_IT_NUMERIC );
         dbTransInfo.dbsci.fRest       = hb_param( 7, HB_IT_LOGICAL );
         dbTransInfo.dbsci.fOptimized  = ! ( dbTransInfo.dbsci.itmCobFor ||
                                           dbTransInfo.dbsci.itmCobWhile );

#if ! defined( __HARBOUR30__ )
         pTransItm = hb_dbTransInfoPut( NULL, &dbTransInfo );
         /* call hb_dbfTransCheckCounters() to add DBTF_CPYCTR or remove DBTF_MATCH & DBTF_PUTREC */
         /* content of pTransItem will be afterwards boolean with state of previous fTransRec */
         errCode = SELF_INFO( dbTransInfo.lpaDest, DBI_TRANSREC, pTransItm );
#else
         pTransItm = NULL;
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
         hb_itemRelease( pTransItm );
      }

      if( dbTransInfo.lpTransItems )
         hb_xfree( dbTransInfo.lpTransItems );

      hb_retl( errCode == HB_SUCCESS );
   }
   else
      commonError( ( LETOAREAP ) pSrcArea, EG_ARG, EDBCMD_NOTABLE, 0, NULL, 0, "LETO_DBARRANGE" );
}

/* hb_rddTransRecords( pArea, szFileName, szDriver, ulConnection, pFields, fExport,
 *                     pCobFor, pStrFor, pCobWhile, pStrWhile, pNext, pRecID, pRest, szCpId, pDelim ) */

/* __dbApp( cFilename, aFields, cbFor, cbWhile, nNext, nRecord, lRest, cRDD, nConnection, cCDP, xDelimiter ) */
HB_FUNC( LETO_DBAPP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   HB_TRACE( HB_TR_DEBUG, ( "LETO_DBAPP(%s, ...)", hb_parc( 1 ) ) );

   if( pArea && hb_parclen( 1 ) )
   {
      char szFile[ HB_PATH_MAX ] = { 0 };

      leto_CheckForIP( hb_parc( 1 ), szFile );
      /* note: import from local file done via e.g. cRDD = DBFNTX */
      hb_retl( hb_rddTransRecords( pArea, szFile, hb_parc( 8 ),
               hb_parnl( 9 ),
               hb_param( 2, HB_IT_ARRAY ),
               HB_FALSE,
               leto_param( 3, 1, HB_IT_BLOCK ),
               leto_param( 3, 2, HB_IT_STRING ),
               leto_param( 4, 1, HB_IT_BLOCK ),
               leto_param( 4, 2, HB_IT_STRING ),
               hb_param( 5, HB_IT_NUMERIC ),
               hb_param( 6, HB_IT_NUMERIC ),
               hb_param( 7, HB_IT_LOGICAL ),
               hb_parc( 10 ),
               hb_param( 11, HB_IT_ANY ) ) == HB_SUCCESS );
   }
   else
      commonError( ( LETOAREAP ) pArea, EG_ARG, EDBCMD_BADPARAMETER, 0, NULL, 0, "LETO_DBAPP" );
}

/* __dbCopy( cFilename, aFields, cbFor, cbWhile, nNext, nRecID, lRest, cRDD, nConnection, cCDP, xDelim ) */
HB_FUNC( LETO_DBCOPY )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   HB_TRACE( HB_TR_DEBUG, ( "LETO_DBCOPY(%s, ...)", hb_parc( 1 ) ) );

   if( pArea && hb_parclen( 1 ) )
   {
      char szFile[ HB_PATH_MAX ] = { 0 };

      leto_CheckForIP( hb_parc( 1 ), szFile );
      /* note: export to local file done via e.g. cRDD = DBFNTX */
      hb_retl( hb_rddTransRecords( pArea, szFile, hb_parc( 8 ),
               hb_parnl( 9 ),
               hb_param( 2, HB_IT_ARRAY ),
               HB_TRUE,
               leto_param( 3, 1, HB_IT_BLOCK ),
               leto_param( 3, 2, HB_IT_STRING ),
               leto_param( 4, 1, HB_IT_BLOCK ),
               leto_param( 4, 2, HB_IT_STRING ),
               hb_param( 5, HB_IT_NUMERIC ),
               hb_param( 6, HB_IT_NUMERIC ),
               hb_param( 7, HB_IT_LOGICAL ),
               hb_parc( 10 ),
               hb_param( 11, HB_IT_ANY ) ) == HB_SUCCESS );
   }
   else
      commonError( ( LETOAREAP ) pArea, EG_ARG, EDBCMD_BADPARAMETER, 0, NULL, 0, "LETO_DBCOPY" );
}

/* pSArea == NULL -> search in all tables of pMArea connection */
static PHB_ITEM leto_FieldBlockVerify( JOINAREASTRU pMArea, JOINAREASTRU pSArea, PHB_ITEM pFields )
{
   FINDAREASTRU sXArea = { 0, 0L, NULL };
   char *       szBlock = NULL;
   char         szField[ HB_SYMBOL_NAME_LEN + 1 ] = { 0 };
   char         szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ] = { 0 };
   const char * pSearch, * pAlias;
   HB_USHORT    uiTstField = 0;
   HB_BOOL      fBlock, fMasterField;
   PHB_ITEM     pSub, pItem, pBlock = NULL;
   PHB_ITEM     pFieldList = hb_itemArrayNew( 0 );  /* processed list */
   HB_USHORT    ui, uii;

   for( ui = 1; ui <= ( HB_USHORT ) hb_arrayLen( pFields ); ui++ )  /* validate fields & blocks */
   {
      pSearch = hb_arrayGetCPtr( pFields, ui );
      if( ! pSearch || ! *pSearch )
         continue;

      fBlock = ( pSearch[ 0 ] == '{' && pSearch[ strlen( pSearch ) - 1 ] == '}' );
      if( ! fBlock )
      {
         pAlias = strstr( pSearch, "->" );  /* blocks opt. aliased as !field!name */
         if( pAlias && ( *( pAlias + 2 ) == '{' && pSearch[ strlen( pSearch ) - 1 ] == '}' ) )
            fBlock = HB_TRUE;
      }
      else
         pAlias = NULL;

      fMasterField = HB_FALSE;
      if( fBlock )
      {
         if( pAlias )
         {
            hb_strncpy( szField, pSearch, HB_MIN( pAlias - pSearch, HB_SYMBOL_NAME_LEN ) );
            szBlock = hb_strdup( pAlias + 2 );
            leto_CbTrim( szBlock );
         }
         else
         {
            strcpy( szField, "MACRO_0" );
            szBlock = hb_strdup( pSearch );
         }
         pBlock = leto_mkCodeBlock( szBlock, strlen( szBlock ) );
      }
      else  /* field */
      {
         if( pAlias )  /* -> */
         {
            strcpy( szField, pAlias + 2 );
            hb_strncpy( szAlias, pSearch, HB_MIN( pAlias - pSearch, HB_RDD_MAX_ALIAS_LEN ) );
         }
         else if( ( pAlias = strstr( pSearch, "." ) ) != NULL )  /* SQL alias version */
         {
            strcpy( szField, pAlias + 1 );
            hb_strncpy( szAlias, pSearch, HB_MIN( pAlias - pSearch, HB_RDD_MAX_ALIAS_LEN ) );
         }
         else
         {
            strcpy( szAlias, pMArea.szAlias );
            strcpy( szField, pSearch );
         }

         if( ! leto_stricmp( szAlias, pMArea.szAlias ) )
         {
            fMasterField = HB_TRUE;
            uiTstField = hb_rddFieldIndex( pMArea.pArea, szField );
         }
         else if( ! pSArea.szAlias || ! pSArea.szAlias[ 0 ] )  /* all WA */
         {
            LETOCONNECTION * pConnection;

            sXArea.uiConnection = ( ( LETOAREAP ) pMArea.pArea )->pTable->uiConnection;
            pConnection = letoGetConnPool( sXArea.uiConnection );
            hb_itemPutC( pConnection->whoCares, szField );
            sXArea.ulAreaID = 0;
            sXArea.pArea = NULL;

            if( hb_rddIterateWorkAreas( leto_FindField, ( void * ) &sXArea ) != HB_SUCCESS )
               uiTstField = hb_rddFieldIndex( ( AREAP ) sXArea.pArea, szField );
            hb_itemRelease( pConnection->whoCares );
         }
         else if( ! leto_stricmp( szAlias, pSArea.szAlias ) )
            uiTstField = hb_rddFieldIndex( pSArea.pArea, szField );
         else
            uiTstField = 0;

         if( uiTstField )  /* autoinc fields get no CB, as auto-filled */
         {
            AREAP pZArea = ( AREAP ) ( fMasterField ? pMArea.pArea : pSArea.pArea );

            if( sXArea.pArea )
               pZArea = ( AREAP ) sXArea.pArea;
            pItem = hb_itemNew( NULL );

#ifdef DBS_FLAG
            SELF_FIELDINFO( pZArea, uiTstField, DBS_FLAG, pItem );
            if( ! ( hb_itemGetNL( pItem ) & HB_FF_AUTOINC ) )
#else
            SELF_FIELDINFO( pZArea, uiTstField, DBS_TYPE, pItem );
            if( strchr( hb_itemGetCPtr( pItem ), '+' ) == NULL )
#endif
            {
               SELF_ALIAS( pZArea, szAlias );
               szBlock = ( char * ) hb_xgrab( strlen( szAlias ) + strlen( szField ) + 4 );
               eprintf( szBlock, "%s->%s", szAlias, szField );
            }
            hb_itemRelease( pItem );
         }
      }

      if( pBlock || uiTstField )  /* add valid fields & CBs to process list */
      {
         uii = 1;
         while( uii <= ( HB_USHORT ) hb_arrayLen( pFieldList ) )
         {
            /* try to fix duplicate field-names by adding counter */
            if( ! leto_stricmp( szField, hb_arrayGetCPtr( hb_arrayGetItemPtr( pFieldList, uii ), 1 ) ) )
            {
               char szIndex[ 11 ];
               int  iLen = HB_MIN( strlen( szField ), 10 );
               int  iPos = 1, iDigitLen;
               const char * ptr;

               while( iLen - iPos > 0 && HB_ISDIGIT( szField[ iLen - iPos ] ) )
               {
                  iPos++;
               }
               ptr = szField + iLen - iPos + 1;
               iDigitLen = eprintf( szIndex, "%ld", atol( ptr ) + 1 );
               if( iDigitLen > 9 )
                  break;
               if( ptr - szField  + iDigitLen > 9 )
                  ptr = szField + 10 - iDigitLen;
               if( ptr - szField < 1 )
                  break;
               strcpy( szField + ( ptr - szField ), szIndex );

               uii = 1;
               continue;
            }
            uii++;
         }

         if( uii > ( HB_USHORT ) hb_arrayLen( pFieldList ) )  /* duplicate name solved! */
         {
            pSub = hb_itemArrayNew( 7 );
            hb_arraySetCL( pSub, 1, szField, strlen( szField ) );

            if( ! pBlock )  /* real field, query for struc */
            {
               AREAP pZArea = ( AREAP ) ( fMasterField ? pMArea.pArea : pSArea.pArea );

               if( sXArea.pArea )
                  pZArea = ( AREAP ) sXArea.pArea;
               pItem = hb_itemNew( NULL );
               SELF_FIELDINFO( pZArea, uiTstField, DBS_TYPE, pItem );
               hb_arraySetC( pSub, 2, hb_itemGetCPtr( pItem ) );
               SELF_FIELDINFO( pZArea, uiTstField, DBS_LEN, pItem );
               hb_arraySetNI( pSub, 3, hb_itemGetNI( pItem ) );
               SELF_FIELDINFO( pZArea, uiTstField, DBS_DEC, pItem );
               hb_arraySetNI( pSub, 4, hb_itemGetNI( pItem ) );
               hb_arraySetC( pSub, 5, szBlock ? szBlock : "" );
               if( szBlock )
               {
                  pBlock = leto_mkCodeBlock( szBlock, strlen( szBlock ) );
                  hb_arraySet( pSub, 6, pBlock );
               }
               hb_itemRelease( pItem );
            }
            else
            {
               pItem = hb_vmEvalBlock( pBlock );
               hb_arraySetC( pSub, 2, ( hb_itemType( pItem ) & HB_IT_TIMESTAMP ) ? "@" : hb_itemTypeStr( pItem ) );
               hb_arraySetNI( pSub, 3, ( hb_itemType( pItem ) & HB_IT_NUMERIC ) ? 16 : hb_itemGetCLen( pItem ) );
               hb_arraySetNI( pSub, 4, ( hb_itemType( pItem ) & HB_IT_DOUBLE ) ? 3: 0 );
               hb_arraySetC( pSub, 5, szBlock );
               hb_arraySet( pSub, 6, pBlock );
               hb_arraySet( pSub, 7, pItem );
               hb_itemRelease( pItem );
            }
            hb_arrayAdd( pFieldList, pSub );
            hb_itemRelease( pSub );
         }

         hb_itemRelease( pBlock );
         pBlock = NULL;
         szField[ 0 ] = '\0';
      }
      if( szBlock )
         hb_xfree( szBlock );
      szBlock = NULL;
   }

   hb_itemRelease( pFields );
   pFields = pFieldList;

   return pFields;
}

/* __dbJoin( cnAlias, cFile, aFields|CBs, cbFor, cRDD, nConnection, cCodePage[, lTemp ] ) */
HB_FUNC( LETO_DBJOIN )
{
   JOINAREASTRU pMArea, pSArea;
   char         szFile[ HB_PATH_MAX ] = { 0 };
   PHB_ITEM     pFlds = hb_param( 3, HB_IT_ARRAY );
   PHB_ITEM     pFieldList = hb_itemArrayNew( 0 );  /* processed list */
   PHB_ITEM     pFor = NULL;
   char *       szFor = NULL;
   const char * szDriver = hb_parclen( 5 ) ? hb_parc( 5 ) : "LETO";
   const char * szCdp = hb_parclen( 7 ) ? hb_parc( 7 ) : NULL;
   HB_BOOL      fOptimized = ( ! szDriver || ! leto_stricmp( szDriver, "LETO" ) ) ? HB_TRUE : HB_FALSE ;
   HB_USHORT    ui;

   HB_TRACE( HB_TR_DEBUG, ( "LETO_DBJOIN(%s, %s, ...)", hb_parc( 1 ), hb_parc( 2 ) ) );

   memset( &pMArea, 0, sizeof( JOINAREASTRU ) );
   memset( &pSArea, 0, sizeof( JOINAREASTRU ) );
   pMArea.pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( hb_parclen( 1 ) )
      hb_strncpy( pSArea.szAlias, hb_parc( 1 ), HB_RDD_MAX_ALIAS_LEN );
   if( hb_parclen( 2 ) )
      leto_CheckForIP( hb_parc( 2 ), szFile );

   if( pMArea.pArea )  /* collect WA infos */
   {
      pMArea.iArea = hb_rddGetCurrentWorkAreaNumber();
      SELF_ALIAS( pMArea.pArea, pMArea.szAlias );
      SELF_RECNO( pMArea.pArea, &pMArea.ulRecNo );

      if( pSArea.szAlias && pSArea.szAlias[ 0 ] )
      {
         hb_rddGetAliasNumber( pSArea.szAlias, &pSArea.iArea );
         if( pSArea.iArea )
         {
            LPDBRELINFO lpDbRel = pMArea.pArea->lpdbRelations;

            while( lpDbRel )
            {
               if( ( int ) lpDbRel->lpaChild->uiArea == pMArea.iArea )
               {
                  pSArea.iArea = 0;
                  break;
               }
               lpDbRel = lpDbRel->lpdbriNext;
            }
            if( pSArea.iArea )
            {
               pSArea.pArea = ( AREAP ) hb_rddGetWorkAreaPointer( ( HB_AREANO ) pSArea.iArea );
               SELF_RECNO( pSArea.pArea, &pSArea.ulRecNo );
            }
         }
      }
   }

   if( pMArea.iArea && pSArea.iArea )  /* check pFor and initial fields collects */
   {
      if( ! leto_CheckArea( ( LETOAREAP ) pMArea.pArea ) || ! leto_CheckArea( ( LETOAREAP ) pSArea.pArea ) )
         fOptimized = HB_FALSE;
      else if( ( pMArea.pArea->dbfi.fFilter && ! pMArea.pArea->dbfi.fOptimized ) ||
               ( pSArea.pArea->dbfi.fFilter && ! pSArea.pArea->dbfi.fOptimized ) )
         fOptimized = HB_FALSE;

      if( ! fOptimized && leto_param( 4, 1, HB_IT_EVALITEM | HB_IT_BLOCK ) )
         pFor = hb_itemClone( leto_param( 4, 1, HB_IT_EVALITEM | HB_IT_BLOCK ) );
      else if( leto_param( 4, 2, HB_IT_STRING ) )
      {
         szFor = hb_strdup( hb_itemGetCPtr( leto_param( 4, 2, HB_IT_STRING ) ) );
         leto_CbTrim( szFor );
         pFor = leto_mkCodeBlock( szFor, strlen( szFor ) );
      }
      else if( leto_param( 4, 1, HB_IT_EVALITEM | HB_IT_BLOCK ) )
      {
         pFor = hb_itemClone( hb_param( 4, HB_IT_EVALITEM | HB_IT_BLOCK ) );
         fOptimized = HB_FALSE;
      }
      else
      {
         szFor = hb_strdup( "{||.T.}" );
         pFor = leto_mkCodeBlock( szFor, strlen( szFor ) );
      }

      if( ! pFor || ! ( hb_itemType( hb_vmEvalBlock( pFor ) ) & HB_IT_LOGICAL ) )
      {
         hb_itemRelease( pFor );
         pFor = NULL;
      }
      else
      {
         HB_USHORT uiFields = pFlds ? ( HB_USHORT ) hb_arrayLen( pFlds ) : 0;

         if( ! uiFields )  /* all fields of master */
         {
            char szField[ HB_SYMBOL_NAME_LEN + 1 ] = { 0 };

            SELF_FIELDCOUNT( pMArea.pArea, &uiFields );
            hb_arraySize( pFieldList, uiFields );
            for( ui = 1; ui <= uiFields; ui++ )
            {
               SELF_FIELDNAME( pMArea.pArea, ui, szField );
               hb_arraySetC( pFieldList, ui, szField );
            }
         }
         else
         {
            hb_arraySize( pFieldList, uiFields );
            for( ui = 1; ui <= uiFields; ui++ )
            {
               hb_arraySetC( pFieldList, ui, hb_arrayGetCPtr( pFlds, ui ) );
            }
         }
      }
   }

   pFieldList = leto_FieldBlockVerify( pMArea, pSArea, pFieldList );  /* verify! */

   if( fOptimized && hb_arrayLen( pFieldList ) )  /* extended check for fOptimze */
   {
      LETOCONNECTION * pConnection = letoGetConnPool( ( ( LETOAREAP ) pMArea.pArea )->pTable->uiConnection );

      if( pConnection != letoGetCurrConn() )
         fOptimized = HB_FALSE;
      else
         fOptimized = leto_IsUDFAllowed( pConnection, NULL );  /* "LETO_DBJOIN" */
   }

   if( hb_arrayLen( pFieldList ) )  /* main action */
   {
      const HB_USHORT nLen = ( HB_USHORT ) hb_arrayLen( pFieldList );
      JOINAREASTRU    pDArea;
      HB_ERRCODE      errcode;
      DBOPENINFO      pInfo;

      /* note: use more complex DBOPENINFO to precisely 'turn the screws, instead of
       * errcode = hb_rddCreateTable( szFile, szDriver, ... )
       */
      memset( &pDArea, 0, sizeof( JOINAREASTRU ) );
      memset( &pInfo, 0, sizeof( DBOPENINFO ) );
      hb_rddSelectWorkAreaNumber( 0 );
      if( *szFile )
         szDriver = hb_rddFindDrv( szDriver, szFile );
      if( ! szDriver || ! hb_rddInsertAreaNode( szDriver ) )
      {
         hb_rddSelectWorkAreaNumber( pMArea.iArea );
         errcode = HB_FAILURE;
      }
      else  /* fill new blank WA */
      {
         pDArea.pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

         pInfo.uiArea = pDArea.pArea->uiArea;
         pInfo.abName = *szFile ? szFile : NULL;
         /* pInfo.atomAlias = ""; */
         pInfo.cdpId = szCdp ? szCdp : hb_setGetDBCODEPAGE();
         pInfo.ulConnection = 0;
         if( ! *szFile || hb_parldef( 8, HB_FALSE ) )
         {
            PHB_ITEM pItem = hb_itemPutL( NULL, HB_TRUE );

            SELF_INFO( pDArea.pArea, DBI_ISTEMPORARY, pItem );
            hb_itemRelease( pItem );
         }
         errcode = SELF_CREATEFIELDS( pDArea.pArea, pFieldList );
         if( errcode == HB_SUCCESS )
            errcode = SELF_CREATE( pDArea.pArea, &pInfo );  //letocreate
         else
         {
            hb_rddReleaseCurrentArea();
            hb_rddSelectWorkAreaNumber( pMArea.iArea );
         }
         if( errcode == HB_SUCCESS )
         {
            pDArea.pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
            pDArea.iArea = hb_rddGetCurrentWorkAreaNumber();
            SELF_ALIAS( pDArea.pArea, pDArea.szAlias );
         }
      }

      if( fOptimized && errcode == HB_SUCCESS )  /* target created, let the server join */
      {
         LETOCONNECTION * pConnection = letoGetConnPool( ( ( LETOAREAP ) pMArea.pArea )->pTable->uiConnection );
         PHB_ITEM pParam = hb_itemArrayNew( 4 );
         PHB_ITEM pBlocks = hb_itemArrayNew( nLen );
         HB_BOOL  fSuccess;

         hb_arraySetC( pParam, 1, pSArea.szAlias );
         hb_arraySetC( pParam, 2, pMArea.szAlias );
         for( ui = 1; ui <= nLen; ui++ )
         {
            hb_arraySetC( pBlocks, ui, hb_arrayGetCPtr( hb_arrayGetItemPtr( pFieldList, ui ), 5 ) );
         }
         hb_arraySet( pParam, 3, pBlocks );
         hb_arraySetC( pParam, 4, szFor );

         fSuccess = LetoUdf( pConnection, ( ( LETOAREAP ) pDArea.pArea )->pTable, HB_FALSE, "LETO_DBJOIN", &pParam );

         SELF_GOTOP( pDArea.pArea );
         hb_itemRelease( pBlocks );
         hb_itemRelease( pFieldList );

         if( fSuccess )
            hb_itemReturnRelease( pParam );
         else
         {
            hb_itemRelease( pParam );
            hb_retl( fSuccess );
         }
      }
      else if( errcode == HB_SUCCESS )  /* must collect locally at client ... */
      {
         PHB_ITEM * pBlocks = ( PHB_ITEM * ) hb_xgrabz( sizeof( PHB_ITEM ) * nLen );
         //HB_SIZE    nProcessed = 0;

         for( ui = 0; ui < nLen; ui++ )
         {
            if( HB_IS_BLOCK( hb_arrayGetItemPtr( hb_arrayGetItemPtr( pFieldList, ui + 1 ), 6 ) ) )
               pBlocks[ ui ] = hb_arrayGetItemPtr( hb_arrayGetItemPtr( pFieldList, ui + 1 ), 6 );
         }

         hb_rddSelectWorkAreaNumber( pMArea.iArea );
         SELF_GOTOP( pMArea.pArea );

         SELF_EOF( pMArea.pArea, &pMArea.fEof );
         while( ! pMArea.fEof && errcode == HB_SUCCESS )
         {
            SELF_GOTOP( pSArea.pArea );

            SELF_EOF( pSArea.pArea, &pSArea.fEof );
            while( ! pSArea.fEof && errcode == HB_SUCCESS )
            {
               if( hb_itemGetL( hb_vmEvalBlock( pFor ) ) )
               {
                  errcode = SELF_APPEND( pDArea.pArea, HB_TRUE );
                  if( errcode == HB_SUCCESS )
                  {
                     for( ui = 0; ui < nLen; ui++ )
                     {
                        if( pBlocks[ ui ] )  /* blocks are verified */
                           SELF_PUTVALUE( pDArea.pArea, ui + 1, hb_vmEvalBlock( pBlocks[ ui ] ) );
                     }
                     //nProcessed++;
                  }
               }

               SELF_SKIP( pSArea.pArea, 1 );
               SELF_EOF( pSArea.pArea, &pSArea.fEof );
            }

            SELF_SKIP( pMArea.pArea, 1 );
            SELF_EOF( pMArea.pArea, &pMArea.fEof );
         }

         SELF_GOTO( pSArea.pArea, pSArea.ulRecNo );

         if( pDArea.iArea )
         {
            hb_rddSelectWorkAreaNumber( pDArea.iArea );
            SELF_GOTOP( pDArea.pArea );
         }

         hb_xfree( pBlocks );
         hb_itemRelease( pFieldList );
         hb_retl( errcode == HB_SUCCESS );
      }
   }

   if( pMArea.iArea && pMArea.ulRecNo )
      SELF_GOTO( pMArea.pArea, pMArea.ulRecNo );

   if( szFor )
      hb_xfree( szFor );
   hb_itemRelease( pFor );
}

/* __dbTotal( cFile, xKey, aFields, xFor, xWhile, nNext, nRec, lRest, cRDD, nConnection, cCodePage ) */
HB_FUNC( LETO_DBTOTAL )
{
   LETOCONNECTION * pConnection = NULL;
   LETOTABLE * pTable = NULL;
   char        szFile[ HB_PATH_MAX ] = { 0 };
   PHB_ITEM    pRDD = hb_param( 9, HB_IT_STRING );
   AREAP       pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL     fOptimized = pArea ? HB_TRUE : HB_FALSE;

   if( ! leto_CheckArea( ( LETOAREAP ) pArea ) ||
       ! leto_param( 2, 2, HB_IT_STRING ) ||
       ! leto_param( 4, 2, HB_IT_STRING ) ||
       ! leto_param( 5, 2, HB_IT_STRING ) ||
       ( hb_itemGetCLen( pRDD ) && leto_stricmp( hb_itemGetCPtr( pRDD ), "LETO" ) ) )
      fOptimized = HB_FALSE;
   else
      leto_CheckForIP( hb_parc( 1 ), szFile );

   if( fOptimized )  /* extended check for fOptimze */
   {
      pTable = ( ( LETOAREAP ) pArea )->pTable;
      pConnection = letoGetConnPool( pTable->uiConnection );
      if( pConnection != letoGetCurrConn() )
         fOptimized = HB_FALSE;
      else
         fOptimized = leto_IsUDFAllowed( pConnection, NULL );  /* "LETO_DBTOTAL" */
   }

   if( fOptimized )
   {
      PHB_ITEM pParam = hb_itemArrayNew( HB_MAX( 1, hb_pcount() ) );
      HB_ULONG ulRecNo = 0;
      HB_BOOL  fSuccess;
      int      i, iLen = hb_pcount();

      SELF_RECNO( pArea, &ulRecNo );
      hb_arraySetC( pParam, 1, szFile );
      for( i = 2; i <= iLen; i++ )
      {
         if( i == 2 || i == 4 || i == 5 )
            hb_vmPush( leto_param( i, 2, HB_IT_ANY ) );
         else
            hb_arraySet( pParam, i, hb_param( i, HB_IT_ANY ) );
      }

      fSuccess = LetoUdf( pConnection, pTable, HB_FALSE, "LETO_DBTOTAL", &pParam );
      if( fSuccess )
         hb_itemReturnRelease( pParam );
      else
      {
         hb_itemRelease( pParam );
         hb_retl( fSuccess );
      }

      SELF_GOTOP( pArea );
      SELF_GOTO( pArea, ulRecNo );
   }
   else  /* call local __dbTotal() */
   {
      PHB_DYNS pDo = hb_dynsymFind( "__DBTOTAL" );
      int      i, iLen = HB_MAX( 1, hb_pcount() );

      if( pDo )
      {
         hb_vmPushDynSym( pDo );
         hb_vmPushNil();
         for( i = 1; i <= iLen; i++ )
         {
            if( i == 1 )
               hb_vmPushString( szFile, strlen( szFile ) );
            else if( i == 2 || i == 4 || i == 5 )
               hb_vmPush( leto_param( i, 1, HB_IT_ANY ) );
            else
               hb_vmPush( hb_param( i, HB_IT_ANY ) );
         }
         hb_vmDo( ( HB_USHORT ) iLen );
      }
   }
}

/* __dbUpdate( cnAlias, cbKey, lRandom, aAssign, aFields ) */
HB_FUNC( LETO_DBUPDATE )  /* candidate for RDDI_AUTOLOCK ;-) */
{
   LETOCONNECTION * pConnection = NULL;
   LETOTABLE * pTable = NULL;
   JOINAREASTRU pMArea, pSArea;
   PHB_ITEM pKey    = leto_param( 2, 1, HB_IT_EVALITEM );
   HB_BOOL  fRandom = hb_parldef( 3, HB_FALSE );
   PHB_ITEM pExpres = hb_param( 4, HB_IT_ARRAY );
   PHB_ITEM pFields = hb_param( 5, HB_IT_ARRAY );
   PHB_ITEM pBlocks = NULL, pKeyBlock = NULL;
   HB_BOOL  fOptimized = HB_TRUE, fValid = hb_arrayLen( pFields ) ? HB_TRUE : HB_FALSE;

   memset( &pMArea, 0, sizeof( JOINAREASTRU ) );
   memset( &pSArea, 0, sizeof( JOINAREASTRU ) );

   if( ( HB_ISNUM( 1 ) || hb_parclen( 1 ) ) && ( pKey || leto_param( 2, 2, HB_IT_STRING ) ) && pExpres &&
       pFields && hb_arrayLen( pFields ) )
   {
      pMArea.pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      pMArea.iArea = hb_rddGetCurrentWorkAreaNumber();
      if( pMArea.iArea && HB_ISNUM( 1 ) && hb_parni( 1 ) > 0 )
      {
         hb_rddSelectWorkAreaNumber( hb_parni( 1 ) );
         hb_rddGetAliasNumber( pSArea.szAlias, &pMArea.iArea );
         hb_rddSelectWorkAreaNumber( pMArea.iArea );
      }
      else
         hb_strncpy( pSArea.szAlias, hb_parc( 1 ), HB_RDD_MAX_ALIAS_LEN );
   }
   else
      fValid = HB_FALSE;

   if( pMArea.pArea )  /* collect WA infos */
   {
      SELF_ALIAS( pMArea.pArea, pMArea.szAlias );
      SELF_RECNO( pMArea.pArea, &pMArea.ulRecNo );

      if( pSArea.szAlias && pSArea.szAlias[ 0 ] )
      {
         hb_rddGetAliasNumber( pSArea.szAlias, &pSArea.iArea );
         if( pSArea.iArea )
         {
            LPDBRELINFO lpDbRel = pMArea.pArea->lpdbRelations;

            while( lpDbRel )
            {
               if( ( int ) lpDbRel->lpaChild->uiArea == pMArea.iArea )
               {
                  pSArea.iArea = 0;
                  break;
               }
               lpDbRel = lpDbRel->lpdbriNext;
            }
            if( pSArea.iArea )
            {
               pSArea.pArea = ( AREAP ) hb_rddGetWorkAreaPointer( ( HB_AREANO ) pSArea.iArea );
               SELF_RECNO( pSArea.pArea, &pSArea.ulRecNo );
            }
         }
      }
   }

   if( pMArea.iArea && pSArea.iArea )
   {
      HB_SIZE  n, nLen = hb_arrayLen( pFields );
      int      iPos, iLen;
      HB_SIZE  nBlockSize = 4;
      char *   szBlock = ( char * ) hb_xgrab( nBlockSize );

      if( ! leto_CheckArea( ( LETOAREAP ) pMArea.pArea ) || ! leto_CheckArea( ( LETOAREAP ) pSArea.pArea ) )
         fOptimized = HB_FALSE;
      else if( ! leto_param( 2, 2, HB_IT_STRING ) )
         fOptimized = HB_FALSE;
      else if( ( ( LETOAREAP ) pMArea.pArea )->pTable->uiConnection !=
               ( ( LETOAREAP ) pSArea.pArea )->pTable->uiConnection )
         fOptimized = HB_FALSE;
      else if( ( pMArea.pArea->dbfi.fFilter && ! pMArea.pArea->dbfi.fOptimized ) ||
               ( pSArea.pArea->dbfi.fFilter && ! pSArea.pArea->dbfi.fOptimized ) )
         fOptimized = HB_FALSE;

      pTable = ( ( LETOAREAP ) pMArea.pArea )->pTable;
      if( fRandom && ! pTable->pTagCurrent )
         fValid = HB_FALSE;
      else if( ! nLen || hb_arrayLen( pExpres ) < nLen )
         fValid = HB_FALSE;
      else if( ! ( ! pTable->fShared || pTable->fFLocked ) )  /* need lock */
         fValid = HB_FALSE;
      else
      {
         for( n = 1; n <= nLen; n++ )
         {
            if( ! hb_rddFieldIndex( pMArea.pArea, hb_arrayGetCPtr( pFields, n ) ) )
            {
               fValid = HB_FALSE;
               break;
            }
         }
      }

      if( fValid )
      {
         HB_SIZE nNeedSize;

         strcpy( szBlock, "{||" );
         iPos = 3;
         for( n = 1; n <= nLen; n++ )
         {

            nNeedSize = strlen( hb_arrayGetCPtr( pExpres, n ) ) + 5 +
                        strlen( pMArea.szAlias ) + 10 + 16;
            nBlockSize += nNeedSize;
            szBlock = ( char * ) hb_xrealloc( szBlock, nBlockSize );
            iLen = eprintf( szBlock + iPos, "%s->%s:=(%d)->(%s), ", pMArea.szAlias, hb_arrayGetCPtr( pFields, n ),
                                                                    pSArea.iArea, hb_arrayGetCPtr( pExpres, n ) );
            iPos += iLen;
         }
         strcpy( szBlock + iPos++, "}" );
         pBlocks = leto_mkCodeBlock( szBlock, iPos );

         if( ! pKey && leto_param( 2, 2, HB_IT_STRING ) )
         {
            nNeedSize = hb_itemGetCLen( leto_param( 2, 2, HB_IT_STRING ) ) + 6;
            if( nNeedSize > nBlockSize )
               szBlock = ( char * ) hb_xrealloc( szBlock, nBlockSize );
            iLen = eprintf( szBlock, "{||%s}", pSArea.iArea, hb_itemGetCPtr( leto_param( 2, 2, HB_IT_STRING ) ) );

            pKeyBlock = leto_mkCodeBlock( szBlock, iLen );
            pKey = pKeyBlock;
         }
      }

      hb_xfree( szBlock );
   }

   if( fValid && fOptimized )  /* extended check for fOptimze */
   {
      pConnection = letoGetConnPool( pTable->uiConnection );
      if( pConnection != letoGetCurrConn() )
         fOptimized = HB_FALSE;
      else
         fOptimized = leto_IsUDFAllowed( pConnection, NULL );  /* "LETO_DBUPDATE" */
   }

   if( fValid && fOptimized )
   {
      PHB_ITEM pParam = hb_itemArrayNew( HB_MAX( 1, hb_pcount() ) );
      HB_BOOL  fSuccess;
      int      i, iLen = hb_pcount();

      for( i = 1; i <= iLen; i++ )
      {
         if( i == 2 )
            hb_arraySet( pParam, i, leto_param( 2, 2, HB_IT_STRING ) );
         else
            hb_arraySet( pParam, i, hb_param( i, HB_IT_ANY ) );
      }

      fSuccess = LetoUdf( pConnection, pTable, HB_FALSE, "LETO_DBUPDATE", &pParam );

      if( fSuccess )
         hb_itemReturnRelease( pParam );
      else
      {
         hb_itemRelease( pParam );
         hb_retl( fSuccess );
      }

      SELF_GOTOP( pMArea.pArea );
      SELF_GOTOP( pSArea.pArea );
      SELF_GOTO( pMArea.pArea, pMArea.ulRecNo );
      SELF_GOTO( pSArea.pArea, pSArea.ulRecNo );
   }
   else if( fValid )
   {
      DBORDERINFO pOrderInfo;
      HB_BOOL     fEof, fFirst = HB_TRUE;
      PHB_ITEM    pKeyValue;
      PHB_ITEM    pSkipValue;
      int         iResult = 1;

      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      SELF_GOTOP( pMArea.pArea );
      SELF_GOTOP( pSArea.pArea );

      SELF_EOF( pSArea.pArea, &fEof );
      while( ! fEof && fValid )
      {
         hb_rddSelectWorkAreaNumber( pSArea.iArea );
         pKeyValue = hb_vmEvalBlock( pKey );  /* in slave */
         hb_rddSelectWorkAreaNumber( pMArea.iArea );
         if( fRandom && fFirst )  /* check type */
         {
            fFirst = HB_FALSE;
            pOrderInfo.itmResult = hb_itemNew( NULL );
            SELF_ORDINFO( pMArea.pArea, DBOI_KEYVAL, &pOrderInfo );
            fValid = hb_itemCompare( pOrderInfo.itmResult, pKeyValue, HB_TRUE, &iResult );
            hb_itemRelease( pOrderInfo.itmResult );
            if( ! fValid )
               continue;  /* -> break of different type */
         }
         if( fRandom )
         {
            if( SELF_SEEK( pMArea.pArea, HB_FALSE, pKeyValue, HB_FALSE ) == HB_SUCCESS )  /* no softseek */
               hb_vmEvalBlock( pBlocks );
         }
         else
         {
            while( ! fEof )
            {
               pSkipValue = hb_vmEvalBlock( pKey );  /* in master */
               fValid = hb_itemCompare( pSkipValue, pKeyValue, HB_TRUE, &iResult );
               if( ! fValid )  /* different type */
                  break;
               if( iResult < 0 )
               {
                  SELF_SKIP( pMArea.pArea, 1 );
                  SELF_EOF( pSArea.pArea, &fEof );
               }
               else
                  break;
            }

            if( ! fEof && fValid && iResult == 0 )
               hb_vmEvalBlock( pBlocks );
         }

         SELF_SKIP( pSArea.pArea, 1 );
         SELF_EOF( pSArea.pArea, &fEof );
      }

      hb_retl( fValid );
   }
   else
      hb_retl( HB_FALSE );

   hb_vmDestroyBlockOrMacro( pKeyBlock );
   hb_vmDestroyBlockOrMacro( pBlocks );
}


/* calls HB_DBCREATETEMP( [<cAlias>], <aStruct>, [<cRDD>], [<cCdp>], [<nConnection>] ) */
static void hb_DbCreateTemp( const char * szAlias, PHB_ITEM pStruct, const char * szRDD, const char * szCdp, HB_ULONG ulConnection )
{
   PHB_DYNS pDo = hb_dynsymFind( "HB_DBCREATETEMP" );

   if( pDo )
   {
      hb_vmPushDynSym( pDo );
      hb_vmPushNil();
      hb_vmPushString( szAlias, szAlias ? strlen( szAlias ) : 0 );
      hb_vmPush( pStruct );
      hb_vmPushString( szRDD, szRDD ? strlen( szRDD ) : 0 );
      if( ! szCdp )
         hb_vmPushNil();
      else
         hb_vmPushString( szCdp, strlen( szCdp ) );
      hb_vmPushLong( ulConnection );
      hb_vmDo( 5 );
   }
   else
      hb_retl( HB_FALSE );
}

/* leto_DbCreateTemp( cFile, aStruct [, cDriver, lKeepOpen, cAlias, xDelim, cCdp, nConnection ] ) */
HB_FUNC( LETO_DBCREATETEMP )
{
   PHB_ITEM pStruct = hb_param( 2, HB_IT_ARRAY );

   if( ! pStruct || hb_arrayLen( pStruct ) < 1 )  /* at very least aStruct */
   {
      commonError( NULL, EG_ARG, EDBCMD_BADPARAMETER, 0, NULL, 0, "LETO_DBCREATETEMP" );
      hb_retl( HB_FALSE );
   }
   else
   {
      HB_USHORT    uiPrevArea = ( HB_AREANO ) hb_rddGetCurrentWorkAreaNumber();
      const char * szDriver = hb_parc( 3 );
      int          iLastArea = hb_rddGetCurrentWorkAreaNumber();
      AREAP        pArea = NULL;

      if( szDriver && leto_stricmp( szDriver, "LETO" ) )  /* execute HB_DBCREATETEMP() for non LETO */
      {
         const char * szAlias = hb_parclen( 5 ) ? hb_parc( 5 ) : NULL;

         if( ! szAlias )  /* throw the RTE here, before HB do */
         {
            commonError( NULL, EG_ARG, EDBCMD_BADPARAMETER, 0, NULL, 0, "LETO_DBCREATETEMP" );
            hb_retl( HB_FALSE );
         }
         else
            hb_DbCreateTemp( szAlias, pStruct, hb_parc( 3 ), hb_parc( 7 ), ( HB_ULONG ) hb_parnl( 8 ) );

         return;
      }
      else if( ! szDriver )
         szDriver = "LETO";

      hb_rddSelectWorkAreaNumber( 0 );
      if( ! hb_rddInsertAreaNode( szDriver ) )  /* new WorkArea */
      {
         hb_rddSelectWorkAreaNumber( iLastArea );
         commonError( NULL, EG_NOTABLE, EDBCMD_NOTABLE, 0, NULL, 0, "LETO_DBCREATETEMP" );
         hb_retl( HB_FALSE );
      }
      else
         pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

      if( pArea )
      {
         DBOPENINFO pInfo;
         PHB_ITEM   pItem = hb_itemPutL( NULL, HB_TRUE );
         HB_ERRCODE errCode = SELF_INFO( pArea, DBI_ISTEMPORARY, pItem );

         hb_itemRelease( pItem );
         if( errCode == HB_SUCCESS )
         {
            memset( &pInfo, 0, sizeof( DBOPENINFO ) );
            pInfo.uiArea       = pArea->uiArea;
            pInfo.abName       = HB_ISCHAR( 1 ) ? hb_parc( 1 ) : "";
            pInfo.atomAlias    = HB_ISCHAR( 5 ) ? hb_parc( 5 ) : "";
            pInfo.cdpId        = HB_ISCHAR( 7 ) ? hb_parc( 7 ) : hb_setGetDBCODEPAGE();
            pInfo.ulConnection = ( HB_ULONG ) hb_parnl( 8 );

            errCode = SELF_CREATEFIELDS( pArea, pStruct );
            if( errCode == HB_SUCCESS )
               errCode = SELF_CREATE( pArea, &pInfo );
         }

         if( errCode != HB_SUCCESS )
         {
            hb_rddReleaseCurrentArea();
            hb_rddSelectWorkAreaNumber( uiPrevArea );
            hb_retl( HB_FALSE );
         }
         else
            hb_retl( HB_TRUE );
      }
      else
         hb_retl( HB_TRUE );
   }
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
         case 0:
            szResult = "connection ok";
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
      hb_retl( pArea->area.dbfi.fFilter && pArea->area.dbfi.fOptimized );
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( LETO_ISERROPTIM )
{
   LETOCONNECTION * pConnection = letoGetCurrConn();

   if( pConnection )  /* query not mutex secured */
      hb_retl( pConnection->hSocketErr != HB_NO_SOCKET );
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

      if( HB_ISNUM( 2 ) )
      {
         switch( hb_parni( 2 ) )
         {
            case DB_MEMO_DBT:
               szMemoType = "DBT";
               break;
            case DB_MEMO_FPT:
               szMemoType = "FPT";
               break;
            case DB_MEMO_SMT:
               szMemoType = "SMT";
               break;
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

HB_FUNC( LETO_RECLOCK )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_ULONG  ulRecNo = ( HB_ULONG ) hb_parnldef( 1, 0 );
   HB_BOOL   fRet = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "LETO_RECLOCK(%p, %lu, %f)", pArea, ulRecNo, hb_parnd( 2 ) ) );

   if( pArea && ! ulRecNo )
      SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
   if( pArea && ulRecNo )
   {
      LETOCONNECTION * pConnection = NULL;
      int        iOldTimeout = 0;
      DBLOCKINFO dbLockInfo;

      if( pArea->pTable && HB_ISNUM( 2 ) && hb_parnd( 2 ) >= 0 )
      {
         pConnection = letoGetConnPool( pArea->pTable->uiConnection );
         if( pConnection )
         {
            iOldTimeout = pConnection->iLockTimeOut;
            pConnection->iLockTimeOut = ( int ) ( hb_parnd( 2 ) * 1000 );
         }
      }

      memset( &dbLockInfo, 0, sizeof( DBLOCKINFO ) );
      dbLockInfo.itmRecID = hb_itemPutNL( NULL, ulRecNo );
      dbLockInfo.uiMethod = DBLM_MULTIPLE;
      SELF_LOCK( ( AREAP ) pArea, &dbLockInfo );
      if( dbLockInfo.fResult )
         fRet = HB_TRUE;
      hb_itemRelease( dbLockInfo.itmRecID );

      if( pConnection )
         pConnection->iLockTimeOut = iOldTimeout;
   }
   hb_retl( fRet );
}

HB_FUNC( LETO_RECUNLOCK )  /* mimic server-side with DbRunlock() */
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_ULONG  ulRecNo = ( HB_ULONG ) hb_parnldef( 1, 0 );
   HB_BOOL   fRet = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "LETO_RECUNLOCK(%p, %lu)", pArea, ulRecNo ) );

   if( pArea && ! ulRecNo )
      SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
   if( pArea && ulRecNo )
   {
      PHB_ITEM pRecNo = hb_itemPutNL( NULL, ulRecNo );
      PHB_ITEM pItem = hb_itemNew( NULL );

      SELF_RECINFO( ( AREAP ) pArea, pRecNo, DBRI_LOCKED, pItem );
      fRet = hb_itemGetL( pItem );
      SELF_UNLOCK( ( AREAP ) pArea, pRecNo );

      hb_itemRelease( pItem );
      hb_itemRelease( pRecNo );
   }
   hb_retl( fRet );
}

HB_FUNC( LETO_TABLELOCK )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL   fRet = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "LETO_TABLELOCK(%p, %f)", pArea, hb_parnd( 1 ) ) );

   if( pArea && pArea->pTable )
   {
      LETOCONNECTION * pConnection = NULL;
      int        iOldTimeout = 0;
      DBLOCKINFO dbLockInfo;

      if( pArea->pTable && HB_ISNUM( 1 ) && hb_parnd( 1 ) >= 0 )
      {
         pConnection = letoGetConnPool( pArea->pTable->uiConnection );
         if( pConnection )
         {
            iOldTimeout = pConnection->iLockTimeOut;
            pConnection->iLockTimeOut = ( int ) ( hb_parnd( 1 ) * 1000 );
         }
      }

      memset( &dbLockInfo, 0, sizeof( DBLOCKINFO ) );
      dbLockInfo.uiMethod = DBLM_FILE;
      SELF_LOCK( ( AREAP ) pArea, &dbLockInfo );
      if( dbLockInfo.fResult )
         fRet = HB_TRUE;

      if( pConnection )
         pConnection->iLockTimeOut = iOldTimeout;
   }
   hb_retl( fRet );
}

HB_FUNC( LETO_TABLEUNLOCK )
{
   LETOAREAP pArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL   fRet = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "LETO_TABLEUNLOCK(%p)", pArea ) );

   if( pArea )
   {
      PHB_ITEM pItem = hb_itemNew( NULL );

      SELF_INFO( ( AREAP ) pArea, DBI_ISFLOCK, pItem );
      fRet = hb_itemGetL( pItem );
      SELF_UNLOCK( ( AREAP ) pArea, NULL );

      hb_itemRelease( pItem );
   }
   hb_retl( fRet );
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
         PHB_DYNS pDo = hb_dynsymFind( "SET" );
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
      else if( hb_parclen( 2 ) )
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
      pTable->uiUpdated = LETO_FLAG_UPD_NONE;
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
               hb_itemRelease( pRelInf->itmCobExpr );
               hb_itemRelease( pRelInf->abKey );
               hb_xfree( pRelInf );
            }
            pArea->lpdbRelations = NULL;
         }

         if( ! fFLocked && ulLocksMax )
         {
            pLocksPos = ( HB_ULONG * ) hb_xgrab( sizeof( HB_ULONG ) * ulLocksMax );
            memcpy( pLocksPos, pTable->pLocksPos, sizeof( HB_ULONG ) * ulLocksMax );
         }

         strcpy( pConnection->szDriver, pTable->szDriver );
         errCode = hb_rddOpenTable( szFile, "LETO", uiArea, szAlias, pTable->fShared, pTable->fReadonly,
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

            errCode = SELF_GOTO( pArea, ulRecNo );
            if( errCode == HB_SUCCESS )
            {
               if( fFLocked )
               {
                  DBLOCKINFO dbLockInfo;

                  memset( &dbLockInfo, 0, sizeof( DBLOCKINFO ) );
                  dbLockInfo.uiMethod = DBLM_FILE;
                  SELF_LOCK( pArea, &dbLockInfo );
                  if( ! dbLockInfo.fResult )
                     errCode = HB_FAILURE;
               }
               else if( ulLocksMax )
               {
                  HB_ULONG   ul = 0;
                  DBLOCKINFO dbLockInfo;

                  while( ul < ulLocksMax )
                  {
                     memset( &dbLockInfo, 0, sizeof( DBLOCKINFO ) );
                     dbLockInfo.itmRecID = hb_itemPutNL( NULL, pLocksPos[ ul++ ] );
                     dbLockInfo.uiMethod = DBLM_MULTIPLE;
                     dbLockInfo.fResult = HB_FALSE;
                     SELF_LOCK( pArea, &dbLockInfo );
                     hb_itemRelease( dbLockInfo.itmRecID );
                     if( ! dbLockInfo.fResult )
                        errCode = HB_FAILURE;
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
         }
         else  /* HB_FAILURE to open table */
         {
            hb_itemRelease( pFilterBlock );
            hb_itemRelease( pFilterText );
         }

         hb_itemRelease( pOrderList );
         hb_itemRelease( pItem );
         if( pLocksPos )
            hb_xfree( pLocksPos );
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
      const char * szUser = hb_parclen( 2 ) ? hb_parc( 2 ) : NULL;
      const char * szPass = hb_parclen( 3 ) ? hb_parc( 3 ) : NULL;
      int          iTimeOut = hb_parnidef( 4, pConnection->iTimeOut );
      HB_BOOL      fZombieCheck = hb_parldef( 6, ( pConnection->hSocketErr != HB_NO_SOCKET ) );

      if( hb_parni( 4 ) )
         pConnection->iTimeOut = hb_parni( 4 );
      if( hb_parni( 5 ) )
         pConnection->iBufRefreshTime = hb_parni( 5 );
      /* given new address as first param */
      if( ! ( hb_parclen( 1 ) && leto_getIpFromPath( hb_parc( 1 ), szAddr, &iPort, NULL ) ) )
         strcpy( szAddr, pConnection->pAddr );

      /* prepare a marker for WAs' used by active connection */
      hb_rddIterateWorkAreas( leto_PreReopen, ( void * ) pConnection );

      if( pConnection->hSocket != HB_NO_SOCKET )  /* active socket */
      {
         double dIdle = HB_ISNUM( 7 ) ? hb_parnd( 7 ) : 0.0;

         if( ! leto_stricmp( pConnection->pAddr, szAddr ) && ! HB_ISNUM( 7 ) )
            dIdle = HB_MAX( 1, dIdle );
         if( pConnection->fMustResync || pConnection->iError )  /* extra delay */
            hb_idleSleep( HB_MIN( 1.0, dIdle * 2 ) );
         LetoConnectionClose( pConnection );  /* only socket shutdown, free pConnection resources */
         if( dIdle > 0 )
            hb_idleSleep( dIdle );
      }
      LetoConnectionOpen( pConnection, szAddr, iPort, szUser, szPass, iTimeOut, fZombieCheck );

      if( pConnection->iConnectRes )  /* error, no new connection -- restore free-ed address */
      {
         if( ! pConnection->pAddr )
            pConnection->pAddr = ( char * ) hb_xgrab( strlen( szAddr ) + 1 );
         else
            pConnection->pAddr = ( char * ) hb_xrealloc( pConnection->pAddr, strlen( szAddr ) + 1 );
         strcpy( pConnection->pAddr, szAddr );
      }
      else
      {
         LETOAREAP  pLetoArea = ( LETOAREAP ) hb_rddGetCurrentWorkAreaPointer();
         HB_USHORT  uiActiveWA = pLetoArea ? pLetoArea->area.uiArea : 0;
         char       szDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];
         HB_ERRCODE errCode;
         int        iLockTimeOut = pConnection->iLockTimeOut;

         strcpy( szDriver, pConnection->szDriver );
         pConnection->whoCares = hb_itemArrayNew( 0 );  /* for collecting relations */
         if( HB_ISNUM( 8 ) )  /* experimental: temporary increase iLockTimeOut for next line */
            pConnection->iLockTimeOut = HB_MAX( 0, hb_parni( 8 ) );
         errCode = hb_rddIterateWorkAreas( leto_doReopen, ( void * ) pConnection );
         pConnection->iLockTimeOut = iLockTimeOut;
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

