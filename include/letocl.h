/*
 * Header file for Leto RDD
 *
 * Copyright 2013 Alexander S. Kresin <alex / at / kresin.ru>
 * www - http://www.kresin.ru
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

#include "hbdefs.h"
#include "hbsocket.h"
#include "hbznet.h"  /* for PHB_ZNETSTREAM pointer */
#include "hbzlib.ch"
#include "hbthread.h"
#include "hbapirdd.h"

#define HB_MAX_FILE_EXT    10

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

#define LETO_INDEX_UNIQ    1
#define LETO_INDEX_ALL     2
#define LETO_INDEX_REST    4
#define LETO_INDEX_DESC    8
#define LETO_INDEX_CUST    16
#define LETO_INDEX_ADD     32
#define LETO_INDEX_TEMP    64
#define LETO_INDEX_FILT    128
#define LETO_INDEX_USEI    256

#define leto_firstchar( pConnection )  pConnection->szBuffer + 1

#ifndef LETO_DOPCODE_LEN
   #define LETO_DOPCODE_LEN         7
#endif

HB_EXTERN_BEGIN

typedef struct _CDPSTRU
{
   char *            szClientCdp;
   char *            szServerCdp;
   struct _CDPSTRU * pNext;
} CDPSTRU, * PCDPSTRU;

typedef struct _LETOBUFFER_
{
   unsigned char *   pBuffer;         /* Buffer for records */
   unsigned long     ulBufLen;        /* allocated buffer length */
   unsigned long     ulBufDataLen;    /* data length in buffer */
   HB_I64            llDeciSec;       /* buffer time in 1/100 seconds */
   unsigned long     ulShoots;        /* using statistic */
} LETOBUFFER;                         /* 40 */

typedef struct _LETOFIELD
{
   char              szName[ 12 ];
   HB_USHORT         uiType;
   HB_USHORT         uiLen;
   HB_USHORT         uiDec;
   HB_USHORT         uiFlags;
} LETOFIELD;                           /* 20 */

typedef struct _LETOTAGINFO
{
   char *            BagName;
   char *            TagName;
   char *            KeyExpr;
   char *            ForExpr;
   HB_BOOL           fUniqueKey;
   HB_BOOL           fProduction;     /* CDX auto opened order[s] */
   unsigned char     cKeyType;
   HB_USHORT         uiKeySize;
   HB_USHORT         uiTag;
   HB_USHORT         uiFieldTag;      /* Field number for simple (one field) key expersion */
   char *            pTopScopeAsString;
   char *            pBottomScopeAsString;
   HB_USHORT         uiTopScopeAsString;
   HB_USHORT         uiBottomScopeAsString;
   HB_BOOL           fUsrAscend;      /* user settable ascending/descending order flag */
   HB_BOOL           fCustom;         /* user settable custom order flag */
   unsigned long     ulKeyNo;         /* value for ordKeyNo(), if buffering */
   unsigned long     ulKeyCount;      /* value for ordKeyCount(), if buffering */
   void *            pExtra;
   struct _LETOTAGINFO * pNext;
} LETOTAGINFO;                        /* 160 */


typedef struct _LETOTABLE
{
   unsigned long     hTable;            /* workarea */
   unsigned int      uiDriver;          /* 0 for CDX, 1 for NTX */
   char              szDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];
   unsigned int      uiConnection;
   HB_USHORT         uiFieldExtent;     /* number of fields per record */
   LETOFIELD *       pFields;           /* pointer to the fields */
   HB_USHORT         uiUpdated;         /* table update state: appended/ changed/ deleted records */
   unsigned char *   pFieldUpd;         /* pointer to updated fields array */
   unsigned int *    pFieldOffset;      /* pointer to field offset array */
   unsigned char *   pFieldIsBinary;    /* pointer to array: field is a binary type */
   HB_USHORT         uiOrders;
   char *            szTags;
   char              szOrderExt[ HB_MAX_FILE_EXT + 1 ];
   char              szMemoExt[ HB_MAX_FILE_EXT + 1 ];
   unsigned char     uiMemoType;        /* MEMO type used in DBF memo fields */
   unsigned int      uiMemoVersion;     /* MEMO file version */
   unsigned int      uiMemoBlocksize;   /* MEMO BLOCKSIZE ( > 32; <= 64K, % 32 == 0 ) */
   HB_BOOL           fHaveMemo;         /* HB_TRUE if table with memo fields */
   HB_BOOL           fHaveBinary;       /* HB_TRUE if table with binary fields */
   HB_BOOL           fHaveAutoinc;      /* HB_TRUE if table with auto incrementing fields */
   HB_BOOL           fShared;           /* shared file */
   HB_BOOL           fReadonly;         /* read only file */
   HB_BOOL           fEncrypted;        /* encrypted file */
   HB_BOOL           fBof;              /* HB_TRUE if "bof" */
   HB_BOOL           fEof;              /* HB_TRUE if "eof" */
   HB_BOOL           fFound;            /* HB_TRUE if "found" */
   HB_BOOL           fDeleted;          /* deleted record */
   HB_BOOL           fFLocked;          /* TRUE if file is locked */
   HB_BOOL           fRecLocked;        /* HB_TRUE if record is locked */
   unsigned int      uiRecordLen;       /* Size of record including delete flag*/
   unsigned long     ulRecNo;
   unsigned long     ulRecCount;        /* Count of records */
   unsigned char *   pRecord;           /* Buffer of record data */
   LETOBUFFER        Buffer;            /* skip buffer */
   unsigned char *   ptrBuf;            /* pointer into Buffer */
   unsigned int      uiRecInBuf;
   signed char       BufDirection;
   unsigned long     lLastUpdate;       /* from dbf header: last update */
   int               iBufRefreshTime;   /* cache refresh time in 1/100 sec */
   LETOTAGINFO *     pTagInfo;
   LETOTAGINFO *     pTagCurrent;       /* current order */
   unsigned long *   pLocksPos;         /* List of records locked */
   unsigned long     ulLocksMax;        /* Number of records locked */
   unsigned long     ulLocksAlloc;      /* Number of records locked (allocated) */
   HB_USHORT         uiLockScheme;      /* elch new */
} LETOTABLE;                            /* 344 */

typedef struct
{
   HB_ULONG          hTable;            /* workarea */
   HB_ULONG          ulRecNo;
} TRANSACTLIST;

typedef struct _LETOCONNECTION_
{
   unsigned int      iConnection;       /* ID of connection */
   int               iConnectRes;       /* error state of connection -- just after connect */
   int               iError;            /* formerly static s_iError */
   HB_SOCKET         hSocket;
   HB_SOCKET         hSocketErr;
   PHB_ZNETSTREAM    zstream;
   char *            pAddr;
   int               iPort;             /* port at client side */
   int               iServerPort;       /* port at server side */
   int               iTimeOut;
   int               iLockTimeOut;      /* used for rlock and flock, -1 infinite, 0 = none, ms */
   char *            szPath;
   char              szVersion[ 24 ];
   unsigned int      uiMajorVer;
   unsigned int      uiMinorVer;
   char *            szVerHarbour;      /* Harbour version of LetoDB server build */
   char              szAccess[ 8 ];
   char              cDopcode[ LETO_DOPCODE_LEN ];   /* bytes > 0 for mixed into LETO_PASSWORD */
   HB_BOOL           fCrypt;
   HB_BOOL           fCloseAll;
   PCDPSTRU          pCdpTable;
   HB_BOOL           fTransActive;
   HB_BYTE *         szTransBuffer;
   HB_ULONG          ulTransBuffLen;
   HB_ULONG          ulTransDataLen;
   HB_ULONG          ulRecsInTrans;
   HB_ULONG          ulTransBlockLen;
   TRANSACTLIST *    pTransList;
   HB_ULONG          ulTransListLen;
   HB_ULONG          ulRecsInList;
   HB_BOOL           fRefreshCount;
   HB_BOOL           fBufKeyNo;
   HB_BOOL           fBufKeyCount;
   char *            szBuffer;             /* socket communication buffer */
   HB_ULONG          ulBufferLen;          /* len of buffer */
   char *            pBufCrypt;
   int               iZipRecord;
   HB_BOOL           fZipCrypt;
   HB_ULONG          ulBufCryptLen;
   int               iBufRefreshTime;      /* BufRefreshTime in 0.01 sec, afterwards buffer refresh */
   HB_USHORT         uiDriver;             /* default driver 0 = NTX, 1 = CDX */
   char              szDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];       /* DBF driver NAME */
   HB_USHORT         uiLockSchemeExtend;   /* use default or extended lockscheme !! only info, set by server */
   HB_USHORT         uiMemoType;           /* default memo type: dbt, fpt, smt */
   HB_USHORT         uiMemoVersion;
   HB_USHORT         uiMemoBlocksize;                    /* default memo blocksize: 512, 64, 32 or multiple of 32 */
   char              szMemoExt[ HB_MAX_FILE_EXT + 1 ];   /* default memo file extension */
   HB_USHORT         uiProto;
   HB_USHORT         uiTBufOffset;
   HB_THREAD_HANDLE  hThread;
   HB_THREAD_ID      hThreadID;
   int               iDebugMode;
   HB_USHORT         uiServerMode;         /* server file mode: 1,3 == shareTables=0, >= 3 NoSaveWa=1 */
   HB_BOOL           fLowerCase;           /* server config option Lower_Path */
   int               iErrorCode;
   HB_FHANDLE        hSockPipe[ 2 ];
   HB_BYTE           uSrvLock;             /* 0 or type of server 'lock for e.g. replication' mode */
   HB_BOOL           fMustResync;          /* future idea, e.g. after missing answer for a request */
} LETOCONNECTION;                          /* 352 */

HB_EXTERN_END

void LetoInit( void );
void LetoExit( unsigned int uiFull );
void LetoSetCdp( const char * szCdp );
void LetoSet( LETOCONNECTION * pConnection, int iCommand, const char * szCommand );
int LetoGetConnectRes( void );
int LetoGetCmdItem( char ** pptr, char * szDest );
const char * LetoFindCmdItem( const char * ptr );
LETOCONNECTION * LetoConnectionNew( const char * szAddr, int iPort, const char * szUser, const char * szPass, int iTimeOut, HB_BOOL fZombieCheck );
void LetoConnectionClose( LETOCONNECTION * pConnection );
int LetoCloseAll( LETOCONNECTION * pConnection );
const char * LetoGetServerVer( LETOCONNECTION * pConnection );
void LetoSetPath( LETOCONNECTION * pConnection, const char * szPath );
HB_BOOL LetoSetFastAppend( int uiFApp );

void leto_clientlog( const char * sFile, int n, const char * s, ... );
void LetoDbFreeTag( LETOTAGINFO * pTagInfo );
HB_ERRCODE LetoRddInfo( LETOCONNECTION * pConnection, HB_USHORT uiIndex, const char * szNewSet );
HB_ERRCODE LetoDbCloseTable( LETOTABLE * pTable );
LETOTABLE * LetoDbCreateTable( LETOCONNECTION * pConnection, const char * szFile, const char * szAlias, const char * szFields, unsigned int uiArea, const char * szCdpage );
LETOTABLE * LetoDbOpenTable( LETOCONNECTION * pConnection, const char * szFile, const char * szAlias, int iShared, int iReadOnly, const char * szCdp, unsigned int uiArea );
const char * LetoDbGetMemo( LETOTABLE * pTable, unsigned int uiIndex, unsigned long * ulLen );
HB_ERRCODE LetoDbRecCount( LETOTABLE * pTable, unsigned long * ulCount );
HB_ERRCODE LetoDbGoTo( LETOTABLE * pTable, unsigned long ulRecNo );
HB_ERRCODE LetoDbGoTop( LETOTABLE * pTable );
HB_ERRCODE LetoDbGoBottom( LETOTABLE * pTable );
HB_ERRCODE LetoDbSkip( LETOTABLE * pTable, long lToSkip );
HB_ERRCODE LetoDbPutRecord( LETOTABLE * pTable, HB_BOOL fCommit );
HB_ERRCODE LetoDbPutMemo( LETOTABLE * pTable, unsigned int uiIndex, const char * szValue, unsigned long ulLenMemo );
HB_ERRCODE LetoDbAppend( LETOTABLE * pTable, unsigned int fUnLockAll );
HB_ERRCODE LetoDbOrderCreate( LETOTABLE * pTable, const char * szBagName, const char * szTag, const char * szKey, unsigned int uiFlags, const char * szFor, const char * szWhile, unsigned long ulNext );
HB_ERRCODE LetoDbOrderFocus( LETOTABLE * pTable, const char * szTagName, unsigned int uiOrder );
HB_ERRCODE LetoDbSeek( LETOTABLE * pTable, const char * szKey, HB_USHORT uiKeyLen, HB_BOOL fSoftSeek, HB_BOOL fFindLast );
HB_ERRCODE LetoDbClearFilter( LETOTABLE * pTable );
HB_ERRCODE LetoDbSetFilter( LETOTABLE * pTable, const char * szFilter, HB_BOOL fForceOpt );
HB_ERRCODE LetoDbCommit( LETOTABLE * pTable );
HB_ERRCODE LetoDbIsRecLocked( LETOTABLE * pTable, unsigned long ulRecNo, unsigned int * uiRes );
HB_ERRCODE LetoDbRecLock( LETOTABLE * pTable, unsigned long ulRecNo );
HB_ERRCODE LetoDbRecUnLock( LETOTABLE * pTable, unsigned long ulRecNo );
HB_ERRCODE LetoDbFileLock( LETOTABLE * pTable );
HB_ERRCODE LetoDbFileUnLock( LETOTABLE * pTable );
HB_ERRCODE LetoDbPack( LETOTABLE * pTable );
HB_ERRCODE LetoDbZap( LETOTABLE * pTable );
HB_ERRCODE LetoDbReindex( LETOTABLE * pTable );

/* additional pure C access API */
unsigned int LetoDbBof( LETOTABLE * pTable );
unsigned int LetoDbEof( LETOTABLE * pTable );
unsigned int LetoDbGetField( LETOTABLE * pTable, HB_USHORT uiIndex, char * szRet, HB_USHORT * uiLen );
unsigned int LetoDbPutField( LETOTABLE * pTable, HB_USHORT uiIndex, char * szValue, HB_USHORT uiLen );
unsigned int LetoDbRecNo( LETOTABLE * pTable, unsigned long * ulRecNo );
unsigned int LetoDbFieldCount( LETOTABLE * pTable, unsigned int * uiCount );
unsigned int LetoDbFieldName( LETOTABLE * pTable, HB_USHORT uiIndex, char * szName );
unsigned int LetoDbFieldType( LETOTABLE * pTable, HB_USHORT uiIndex, unsigned int * uiType );
unsigned int LetoDbFieldLen( LETOTABLE * pTable, HB_USHORT uiIndex, unsigned int * uiLen );
unsigned int LetoDbFieldDec( LETOTABLE * pTable, HB_USHORT uiIndex, unsigned int * uiDec );
void LetoFreeStr( char * szStr );

long leto_DataSendRecv( LETOCONNECTION * pConnection, const char * sData, unsigned long ulLen );
LETOCONNECTION * letoGetConnPool( HB_UINT uiConnection );
LETOCONNECTION * leto_ConnectionFind( const char * szAddr, int iPort );
/* int LetoCheckServerVer( LETOCONNECTION * pConnection, HB_USHORT uiVer ); */
const char * leto_RemoveIpFromPath( const char * szPath );
HB_BOOL leto_getIpFromPath( const char * sSource, char * szAddr, int * piPort, char * szPath, HB_BOOL fFile );
void leto_getFileFromPath( const char * sSource, char * szFile, HB_USHORT uLenMax );

const char * leto_DecryptText( LETOCONNECTION * pConnection, HB_ULONG * pulLen );

const char * LetoMgGetInfo( LETOCONNECTION * pConnection );
const char * LetoMgSysInfo( LETOCONNECTION * pConnection );
const char * LetoMgGetUsers( LETOCONNECTION * pConnection, const char * szTable, const char * szList );
const char * LetoMgGetTables( LETOCONNECTION * pConnection, const char * szUser, const char * szList );
const char * LetoMgGetIndex( LETOCONNECTION * pConnection, const char * szUser, const char * szTable, const char * szList );
const char * LetoMgGetLocks( LETOCONNECTION * pConnection, const char * szUser, const char * szTable, const char * szList );
int LetoMgKillUser( LETOCONNECTION * pConnection, const char * szUserId );
char * LetoMgGetTime( LETOCONNECTION * pConnection );

int LetoGetError( void );
int LetoVarSet( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar, char cType, const char * szValue, unsigned int uilLength, unsigned int uiFlags, char ** pRetValue );
const char * LetoVarGet( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar, unsigned long * pulLen );
long LetoVarIncr( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar, unsigned int uiFlags );
long LetoVarDecr( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar, unsigned int uiFlags );
int LetoVarDel( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar );
const char * LetoVarGetList( LETOCONNECTION * pConnection, const char * szGroup, unsigned int uiMaxLen );

HB_BOOL LetoFileExist( LETOCONNECTION * pConnection, const char * szFile );
HB_BYTE LetoFileErase( LETOCONNECTION * pConnection, const char * szFile );
HB_BYTE LetoFileRename( LETOCONNECTION * pConnection, const char * szFile, const char * szFileNew );
HB_BYTE LetoFileCopy( LETOCONNECTION * pConnection, const char * szFile, const char * szFileNew );
const char * LetoMemoRead( LETOCONNECTION * pConnection, const char * szFile, unsigned long * ulMemoLen );
HB_BOOL LetoMemoWrite( LETOCONNECTION * pConnection, const char * szFile, const char * szValue, unsigned long ulLen );
const char * LetoFileRead( LETOCONNECTION * pConnection, const char * szFile, unsigned long ulStart, unsigned long * ulLen );
HB_BOOL LetoFileWrite( LETOCONNECTION * pConnection, const char * szFile, const char * szValue, unsigned long ulStart, unsigned long ulLen );
long LetoFileSize( LETOCONNECTION * pConnection, const char * szFile );
const char * LetoFileAttr( LETOCONNECTION * pConnection, const char * szFile, const char * szAttr );
const char * LetoDirectory( LETOCONNECTION * pConnection, const char * szDir, const char * szAttr );
HB_BYTE LetoDirMake( LETOCONNECTION * pConnection, const char * szFile );
HB_BYTE LetoDirExist( LETOCONNECTION * pConnection, const char * szFile );
HB_BYTE LetoDirRemove( LETOCONNECTION * pConnection, const char * szFile );

void leto_ParseRecord( LETOCONNECTION * pConnection, LETOTABLE * pTable, const char * szData, HB_BOOL fCrypt );
void leto_SetUpdated( LETOTABLE * pTable, HB_USHORT uiUpdated );
const char * leto_ParseTagInfo( LETOTABLE * pTable, const char * pBuffer );
void leto_AddKeyToBuf( char * szData, const char * szKey, unsigned int uiKeyLen, unsigned long * pulLen );
void leto_ClearBuffers( LETOTABLE * pTable );

