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
#ifndef __XHARBOUR__
   #include "hbznet.h"  /* for PHB_ZNETSTREAM pointer */
   #include "hbzlib.ch"
   #include "hbthread.h"
#endif
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

#define LETO_INDEX_UNIQ   0x0001
#define LETO_INDEX_ALL    0x0002
#define LETO_INDEX_REST   0x0004
#define LETO_INDEX_DESC   0x0008
#define LETO_INDEX_CUST   0x0010
#define LETO_INDEX_ADD    0x0020
#define LETO_INDEX_TEMP   0x0040
#define LETO_INDEX_FILT   0x0080
#define LETO_INDEX_USEI   0x0100
#define LETO_INDEX_EXCL   0x0200

#define leto_firstchar( pConnection )  pConnection->szBuffer + 1
#define LETO_CENTISEC()                ( leto_MilliSec() / 10 )
#define LETO_DEFAULT_TIMEOUT           120000  /* two minutes */
#define LETO_INITIAL_TIMEOUT             6000

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
   unsigned long     ulShoots;        /* using statistic */
} LETOBUFFER;                         /* 32 */

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
} LETOTAGINFO;                        /* 112 */


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
   char              szOrderExt[ HB_MAX_FILE_EXT + 1 ];  /* default BAG extension for RDD */
   char              szMemoExt[ HB_MAX_FILE_EXT + 1 ];   /* real used BAG extension */
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
   HB_BOOL           fMemIO;            /* 'mem:' in filename */
   HB_BOOL           fAutoRefresh;      /* if true fetch autorefresh data from server if hotbuffer elapsed */
   LETOTAGINFO *     pTagInfo;
   LETOTAGINFO *     pTagCurrent;       /* current order */
   HB_ULONG *        pLocksPos;         /* List of records locked */
   unsigned long     ulLocksMax;        /* Number of records locked */
   unsigned long     ulLocksAlloc;      /* Number of records locked (allocated) */
   HB_USHORT         uiLockScheme;      /* elch new */
   HB_I64            llCentiSec;        /* timepoint last access record[-buffer] data in 1/ 100 s */
   PHB_ITEM          pFilterVar;        /* PHB_ITEM array with LETO_VAR in filter expression to sync */
} LETOTABLE;                            /* 344 */

typedef struct
{
   HB_ULONG          hTable;
   HB_ULONG          ulRecNo;
} TRANSACTLIST;

typedef struct
{
   LETOTABLE *       pTable;
   HB_ULONG          ulRecNo;
} TRANSACTWA;

typedef struct _LETOCONNECTION_
{
   unsigned int      iConnection;          /* ID of connection */
   int               iConnectRes;          /* error state of connection -- just after connect */
   int               iConnectSrv;          /* ID of connection at server */
   int               iError;               /* formerly static s_iError */
   HB_SOCKET         hSocket;
   HB_SOCKET         hSocketErr;
   PHB_ZNETSTREAM    zstream;
   char *            pAddr;                /* server IP address */
   int               iPort;                /* port at client side */
   int               iServerPort;          /* port at server side */
   int               iTimeOut;
   int               iLockTimeOut;         /* used for rlock and flock, -1 infinite, 0 = none, ms */
   char              szVersion[ 24 ];
   unsigned int      uiMajorVer;
   unsigned int      uiMinorVer;
   char *            szVerHarbour;         /* Harbour version of LetoDB server build */
   char              szAccess[ 8 ];
   char              cDopcode[ LETO_DOPCODE_LEN + 1 ];   /* bytes > 0 for mixed into LETO_PASSWORD */
   HB_BOOL           fCloseAll;
   PCDPSTRU          pCdpTable;
   HB_BOOL           fTransActive;
   HB_BOOL           fTransForce;          /* to ignore un!locks during transaction */
   HB_BYTE *         szTransBuffer;        /* buffered transaction data */
   HB_ULONG          ulTransBuffLen;       /* allocated len */
   HB_ULONG          ulTransDataLen;       /* buffered data len */
   HB_ULONG          ulRecsInTrans;        /* count of blocks of Put[memo|record] */
   HB_ULONG          ulTransBlockLen;      /* step size for [re]alloc */
   TRANSACTLIST *    pTransList;           /* ulRecNo / hTable pairs */
   HB_ULONG          ulTransListLen;       /* allocated pairs */
   HB_ULONG          ulRecsInList;         /* count of filled pairs */
   TRANSACTLIST      pRecsNotList;         /* searched pair not to be in pairs */
   TRANSACTWA *      pTransAppend;         /* WA used for transaction append */
   HB_USHORT         uiTransAppend;        /* count of WA for append */
   HB_USHORT         uiTransAppLen;        /* allocated for WA for append */
   HB_ULONG          ulTransLockErr;       /* temporary eperimental */
   HB_BOOL           fRefreshCount;
   HB_BOOL           fBufKeyNo;
   HB_BOOL           fBufKeyCount;
   char *            szBuffer;             /* socket communication send/ receive buffer */
   HB_ULONG          ulBufferLen;          /* len of socket send/ receive buffer, +1 for term  */
   char *            pBufCrypt;
   HB_ULONG          ulBufCryptLen;
   int               iZipRecord;
   HB_BOOL           fZipCrypt;
   HB_BOOL           fDbEvalCompat;        /* enable scope to REST if WHILE/NEXT given */
   int               iBufRefreshTime;      /* in 0.01 sec, afterwards SKIP buffer refresh */
   HB_USHORT         uiDriver;             /* default driver 0 = NTX, 1 = CDX */
   char              szDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];       /* DBF driver NAME */
   HB_USHORT         uiLockSchemeExtend;   /* use default or extended lockscheme !! only info, set by server */
   HB_USHORT         uiMemoType;           /* default memo type: dbt, fpt, smt */
   HB_USHORT         uiMemoBlocksize;                    /* default memo blocksize: 512, 64, 32 or multiple of 32 */
   char              szMemoExt[ HB_MAX_FILE_EXT + 1 ];   /* default memo file extension */
   HB_USHORT         uiProto;
   HB_USHORT         uiTBufOffset;
   HB_THREAD_HANDLE  hThread;
   HB_THREAD_ID      hThreadID;
   int               iDebugMode;
   HB_USHORT         uiServerMode;         /* server file mode: 1,3 == shareTables=0, >= 3 NoSaveWa=1 */
   HB_BOOL           fLowerCase;           /* server config option Lower_Path */
   HB_BOOL           fUDFAllowed;          /* server config option Allow_UDF, set after first test */
   int               iErrorCode;
   HB_FHANDLE        hSockPipe[ 2 ];
   HB_BYTE           uSrvLock;             /* 0 or type of server 'lock for e.g. replication' mode */
   HB_BOOL           fMustResync;          /* future idea, e.g. after missing answer for a request */
   PHB_ITEM          whoCares;             /* temporary tasks, e.g. collect WA relations for Leto_ReConnect() */
} LETOCONNECTION;                          /* 408 */


extern HB_EXPORT void LetoInit( void );
extern HB_EXPORT void LetoExit( unsigned int uiFull );
extern HB_EXPORT HB_ERRCODE LetoSet( LETOCONNECTION * pConnection, int iCommand, const char * szCommand );
extern HB_EXPORT int LetoGetConnectRes( void );
extern HB_EXPORT const char * LetoGetCmdItem( const char * ptr, char * szDest );
extern HB_EXPORT const char * LetoFindCmdItem( const char * ptr );
extern HB_EXPORT void LetoConnectionOpen( LETOCONNECTION * pConnection, const char * szAddr, int iPort, const char * szUser, const char * szPass, int iTimeOut, HB_BOOL fZombieCheck );
extern HB_EXPORT LETOCONNECTION * LetoConnectionNew( const char * szAddr, int iPort, const char * szUser, const char * szPass, int iTimeOut, HB_BOOL fZombieCheck );
extern HB_EXPORT void LetoConnectionClose( LETOCONNECTION * pConnection );
extern HB_EXPORT int LetoCloseAll( LETOCONNECTION * pConnection );
extern HB_EXPORT const char * LetoGetServerVer( LETOCONNECTION * pConnection );
extern HB_EXPORT HB_BOOL LetoPing( LETOCONNECTION * pConnection );
extern HB_EXPORT int LetoToggleZip( LETOCONNECTION * pConnection, int iZipRecord, const char * szPassword );
extern HB_EXPORT HB_BOOL LetoUdf( LETOCONNECTION * pConnection, LETOTABLE * pTable, HB_BOOL fInThread, const char * szFuncName, PHB_ITEM * pItem );

extern HB_EXPORT void LetoDbFreeTag( LETOTAGINFO * pTagInfo );
extern HB_EXPORT HB_ERRCODE LetoRddInfo( LETOCONNECTION * pConnection, HB_USHORT uiIndex, const char * szNewSet );
extern HB_EXPORT HB_ERRCODE LetoDbCloseTable( LETOTABLE * pTable );
extern HB_EXPORT HB_ERRCODE LetoDbDrop( LETOCONNECTION * pConnection, const char * szTFileName, const char * szIFileName );
extern HB_EXPORT HB_ERRCODE LetoDbExists( LETOCONNECTION * pConnection, const char * szTFileName, const char * szIFileName );
extern HB_EXPORT LETOTABLE * LetoDbCreateTable( LETOCONNECTION * pConnection, const char * szFile, const char * szAlias, const char * szFields, unsigned int uiArea, const char * szCdpage, HB_BOOL fTemporary );
extern HB_EXPORT LETOTABLE * LetoDbOpenTable( LETOCONNECTION * pConnection, const char * szFile, const char * szAlias, int iShared, int iReadOnly, const char * szCdp, unsigned int uiArea );
extern HB_EXPORT const char * LetoDbGetMemo( LETOTABLE * pTable, unsigned int uiIndex, unsigned long * ulLen );
extern HB_EXPORT HB_ERRCODE LetoDbRecCount( LETOTABLE * pTable, unsigned long * ulCount );
extern HB_EXPORT HB_ERRCODE LetoDbGoTo( LETOTABLE * pTable, unsigned long ulRecNo );
extern HB_EXPORT HB_ERRCODE LetoDbGoTop( LETOTABLE * pTable );
extern HB_EXPORT HB_ERRCODE LetoDbGoBottom( LETOTABLE * pTable );
extern HB_EXPORT HB_ERRCODE LetoDbSkip( LETOTABLE * pTable, long lToSkip );
extern HB_EXPORT HB_ERRCODE LetoDbPutRecord( LETOTABLE * pTable );
extern HB_EXPORT HB_ERRCODE LetoDbPutMemo( LETOTABLE * pTable, unsigned int uiIndex, const char * szValue, unsigned long ulLenMemo );
extern HB_EXPORT HB_ERRCODE LetoDbAppend( LETOTABLE * pTable, unsigned int fUnLockAll );
extern HB_EXPORT HB_ERRCODE LetoDbEval( LETOTABLE * pTable, const char * szBlock, const char * szFor, const char * szWhile, long lNext, long lRecNo, int iRest, HB_BOOL fResultSet, HB_BOOL fNeedLock, HB_BOOL fBackward, HB_BOOL fStay, PHB_ITEM * pParams, const char * szJoins );
extern HB_EXPORT HB_ERRCODE LetoDbOrderCreate( LETOTABLE * pTable, const char * szBagName, const char * szTag, const char * szKey, unsigned int uiFlags, const char * szFor, const char * szWhile, unsigned long ulNext );
extern HB_EXPORT HB_ERRCODE LetoDbOrderFocus( LETOTABLE * pTable, const char * szTagName, unsigned int uiOrder );
extern HB_EXPORT HB_ERRCODE LetoDbSeek( LETOTABLE * pTable, const char * szKey, HB_USHORT uiKeyLen, HB_BOOL fSoftSeek, HB_BOOL fFindLast );
extern HB_EXPORT HB_ERRCODE LetoDbClearFilter( LETOTABLE * pTable );
extern HB_EXPORT HB_ERRCODE LetoDbSetFilter( LETOTABLE * pTable, const char * szFilter, HB_BOOL fForceOpt );
extern HB_EXPORT HB_ERRCODE LetoDbCommit( LETOTABLE * pTable );
extern HB_EXPORT HB_ERRCODE LetoDbIsRecLocked( LETOTABLE * pTable, unsigned long ulRecNo, unsigned int * uiRes );
extern HB_EXPORT HB_ERRCODE LetoDbRecLock( LETOTABLE * pTable, unsigned long ulRecNo );
extern HB_EXPORT HB_ERRCODE LetoDbRecUnLock( LETOTABLE * pTable, unsigned long ulRecNo );
extern HB_EXPORT HB_ERRCODE LetoDbFileLock( LETOTABLE * pTable );
extern HB_EXPORT HB_ERRCODE LetoDbFileUnLock( LETOTABLE * pTable );
extern HB_EXPORT HB_ERRCODE LetoDbPack( LETOTABLE * pTable );
extern HB_EXPORT HB_ERRCODE LetoDbZap( LETOTABLE * pTable );
extern HB_EXPORT HB_ERRCODE LetoDbReindex( LETOTABLE * pTable );

/* additional pure C access API */
extern HB_EXPORT unsigned int LetoDbBof( LETOTABLE * pTable );
extern HB_EXPORT unsigned int LetoDbEof( LETOTABLE * pTable );
extern HB_EXPORT unsigned int LetoDbGetField( LETOTABLE * pTable, HB_USHORT uiIndex, char ** szRet, unsigned long * ulLen );
extern HB_EXPORT unsigned int LetoDbPutField( LETOTABLE * pTable, HB_USHORT uiIndex, const char * szValue, unsigned long ulLen );
extern HB_EXPORT unsigned int LetoDbRecNo( LETOTABLE * pTable, unsigned long * ulRecNo );
extern HB_EXPORT unsigned int LetoDbFieldCount( LETOTABLE * pTable, unsigned int * uiCount );
extern HB_EXPORT unsigned int LetoDbFieldName( LETOTABLE * pTable, HB_USHORT uiIndex, char * szName );
extern HB_EXPORT unsigned int LetoDbFieldType( LETOTABLE * pTable, HB_USHORT uiIndex, unsigned int * uiType );
extern HB_EXPORT unsigned int LetoDbFieldLen( LETOTABLE * pTable, HB_USHORT uiIndex, unsigned int * uiLen );
extern HB_EXPORT unsigned int LetoDbFieldDec( LETOTABLE * pTable, HB_USHORT uiIndex, unsigned int * uiDec );
extern HB_EXPORT void LetoFreeStr( char * szStr );
extern HB_EXPORT void LetoSetAddress( int argc, char * argv[], char * szAddr, int * iPort );

long leto_DataSendRecv( LETOCONNECTION * pConnection, const char * sData, unsigned long ulLen );
unsigned long leto_SendRecv2( LETOCONNECTION * pConnection, const char * szData, unsigned long ulLen, int iErr );
LETOCONNECTION * letoGetConnPool( unsigned int uiConnection );
LETOCONNECTION * letoGetCurrConn( void );
unsigned int letoGetConnCount( void );
void letoClearCurrConn( void );
LETOCONNECTION * leto_ConnectionFind( const char * szAddr, int iPort );
/* int leto_CheckServerVer( LETOCONNECTION * pConnection, HB_USHORT uiVer ); */
const char * leto_RemoveIpFromPath( const char * szPath );
void leto_BeautifyPath( char * szPath, const char cReplace );
HB_BOOL leto_getIpFromPath( const char * sSource, char * szAddr, int * piPort, char * szPath );
void leto_getFileFromPath( const char * sSource, char * szFile, HB_USHORT uLenMax );

const char * leto_DecryptText( LETOCONNECTION * pConnection, unsigned long * pulLen, char * ptr );
HB_ULONG leto_CryptText( LETOCONNECTION * pConnection, const char * pData, HB_ULONG ulLen, HB_ULONG ulPrelead );

extern HB_EXPORT const char * LetoMgGetInfo( LETOCONNECTION * pConnection );
extern HB_EXPORT const char * LetoMgSysInfo( LETOCONNECTION * pConnection );
extern HB_EXPORT const char * LetoMgGetUsers( LETOCONNECTION * pConnection, const char * szTable, const char * szList );
extern HB_EXPORT const char * LetoMgGetTables( LETOCONNECTION * pConnection, const char * szUser, const char * szList );
extern HB_EXPORT const char * LetoMgGetIndex( LETOCONNECTION * pConnection, const char * szUser, const char * szTable, const char * szList );
extern HB_EXPORT const char * LetoMgGetLocks( LETOCONNECTION * pConnection, const char * szUser, const char * szTable, const char * szList );
extern HB_EXPORT int LetoMgKillUser( LETOCONNECTION * pConnection, const char * szUserId );
extern HB_EXPORT const char * LetoMgGetTime( LETOCONNECTION * pConnection );

extern HB_EXPORT int LetoGetError( void );
extern HB_EXPORT int LetoVarSet( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar, char cType, const char * szValue, unsigned int uilLength, unsigned int uiFlags, char ** pRetValue );
extern HB_EXPORT const char * LetoVarGet( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar, unsigned long * pulLen );
extern HB_EXPORT long LetoVarIncr( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar, unsigned int uiFlags, const char * szIncrement );
extern HB_EXPORT long LetoVarDecr( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar, unsigned int uiFlags, const char * szDecrement );
extern HB_EXPORT int LetoVarDel( LETOCONNECTION * pConnection, const char * szGroup, const char * szVar );
extern HB_EXPORT const char * LetoVarGetList( LETOCONNECTION * pConnection, const char * szGroup, HB_LONG lMaxLen );

extern HB_EXPORT HB_BOOL LetoFileExist( LETOCONNECTION * pConnection, const char * szFile );
extern HB_EXPORT HB_BYTE LetoFileErase( LETOCONNECTION * pConnection, const char * szFile );
extern HB_EXPORT HB_BYTE LetoFileRename( LETOCONNECTION * pConnection, const char * szFile, const char * szFileNew );
extern HB_EXPORT HB_BYTE LetoFileCopy( LETOCONNECTION * pConnection, const char * szFile, const char * szFileNew );
extern HB_EXPORT const char * LetoMemoRead( LETOCONNECTION * pConnection, const char * szFile, unsigned long * ulMemoLen );
extern HB_EXPORT HB_BOOL LetoMemoWrite( LETOCONNECTION * pConnection, const char * szFile, const char * szValue, unsigned long ulLen );
extern HB_EXPORT const char * LetoFileRead( LETOCONNECTION * pConnection, const char * szFile, unsigned long ulStart, unsigned long * ulLen );
extern HB_EXPORT HB_BOOL LetoFileWrite( LETOCONNECTION * pConnection, const char * szFile, const char * szValue, unsigned long ulStart, unsigned long ulLen );
extern HB_EXPORT long LetoFileSize( LETOCONNECTION * pConnection, const char * szFile );
extern HB_EXPORT const char * LetoFileAttr( LETOCONNECTION * pConnection, const char * szFile, const char * szAttr );
extern HB_EXPORT const char * LetoDirectory( LETOCONNECTION * pConnection, const char * szDir, const char * szAttr );
extern HB_EXPORT HB_BYTE LetoDirMake( LETOCONNECTION * pConnection, const char * szFile );
extern HB_EXPORT HB_BYTE LetoDirExist( LETOCONNECTION * pConnection, const char * szFile );
extern HB_EXPORT HB_BYTE LetoDirRemove( LETOCONNECTION * pConnection, const char * szFile );

void leto_ParseRecord( LETOCONNECTION * pConnection, LETOTABLE * pTable, const char * szData );
void leto_SetUpdated( LETOTABLE * pTable, HB_USHORT uiUpdated );
const char * leto_ParseTagInfo( LETOTABLE * pTable, const char * pBuffer );
void leto_AddKeyToBuf( char * szData, const char * szKey, unsigned int uiKeyLen, unsigned long * pulLen );

#ifdef LETO_CLIENTLOG
   extern HB_EXPORT void leto_clientlog( const char * sFile, int n, const char * s, ... );
#endif

#if ! defined( __LETO_C_API__ )
   HB_BOOL Leto_VarExprTest( const char * szSrc, HB_BOOL fMemvarAllowed );
#endif
#if ! defined( __XHARBOUR__ ) && ! defined( __LETO_C_API__ )
   HB_BOOL Leto_VarExprCreate( LETOCONNECTION * pConnection, const char * szSrc, const HB_SIZE nSrcLen, char ** szDst, PHB_ITEM pArr );
   HB_ERRCODE Leto_VarExprSync( LETOCONNECTION * pConnection, PHB_ITEM pArr, HB_BOOL fReSync );
   HB_ERRCODE Leto_VarExprClear( LETOCONNECTION * pConnection, PHB_ITEM pArr );
#else
   #define Leto_VarExprSync( connection, arr, resync )  /* do { } while( 0 ) */
#endif

#if defined( __HARBOUR30__ ) || defined( __LETO_C_API__ )
   extern HB_EXPORT char * LetoSetModName( char * szModule );
#endif

#if defined( __LETO_C_API__ )
   extern HB_EXPORT void LetoSetSetSoftseek( HB_BOOL fSet );
   extern HB_EXPORT void LetoSetSetDeleted( HB_BOOL fSet );
   extern HB_EXPORT void LetoSetSetExclusive( HB_BOOL fSet );
   extern HB_EXPORT void LetoSetSetAutOpen( HB_BOOL fSet );
   extern HB_EXPORT void LetoSetSetAutOrder( HB_UCHAR uSet );
   extern HB_EXPORT void LetoSetSetDateFormat( const char * pSet );
   extern HB_EXPORT void LetoSetSetEpoch( HB_SIZE nSet );
   extern HB_EXPORT void LetoSetSetDefault( const char * pSet );
   extern HB_EXPORT void LetoSetSetPath( const char * pSet );
#endif

HB_EXTERN_END
