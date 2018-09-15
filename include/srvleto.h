/*
 * Header file for Leto DB Server
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
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
#include "hbapicls.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbdate.h"
#include "hbdefs.h"
#include "hbrdddbf.h"
#include "hbset.h"
#include "hbsetup.h"
#include "hbvm.h"
#include "hbxvm.h"
#include "hbthread.h"
#include "hbatomic.h"
#include "hbsocket.h"
#ifndef USE_LZ4
   #include "hbzlib.h"
#endif
#include "hbznet.h"
#include "math.h"

#include "cmdleto.h"
#include "funcleto.h"

#include "dbinfo.ch"
#include "hbmemory.ch"
#ifdef __HARBOUR30__
   #define HB_SERIALIZE_NUMSIZE   HB_TRUE
#else
   #include "hbserial.ch"
#endif
#include "hbzlib.ch"

#include "rddleto.ch"

#if defined( HB_OS_UNIX )
   #if defined( HB_OS_LINUX ) && ! defined( __USE_GNU )
      #define __USE_GNU
   #endif
   #include <unistd.h>
   #include <sys/socket.h>   /* only with above __USE_GNU: MSG_MORE flag */
   #if defined( HB_OS_BSD ) || ( defined( _POSIX_C_SOURCE ) && _POSIX_C_SOURCE >= 200112L )
      #define HB_HAS_POLL
      #include <poll.h>
      #ifndef POLLRDNORM
         #define POLLRDNORM  0x0040
      #endif
   #endif
   #include <errno.h>
   /* splice == leto_dbfCopy() for extended Linux advantages */
   #if defined( HB_OS_LINUX ) && defined( USE_SPLICE )
      #include <splice.h>
   #endif
#endif

typedef struct _LETO_LIST_ITEM
{
   struct _LETO_LIST_ITEM * pNext;
} LETO_LIST_ITEM, * PLETO_LIST_ITEM;

typedef struct _LETO_LOCK_ITEM
{
   struct _LETO_LOCK_ITEM * pNext;
   HB_ULONG          ulRecNo;
} LETO_LOCK_ITEM, * PLETO_LOCK_ITEM;

typedef struct
{
#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   HB_SPINLOCK_T     pMutex;
#else
   HB_CRITICAL_T     pMutex;
#endif
   HB_ULONG          ulSize;
   PLETO_LIST_ITEM   pItem;
   PLETO_LIST_ITEM   pLastItem;
} LETO_LIST, * PLETO_LIST;

typedef struct _VAR_LINK
{
   HB_USHORT         uiGroup;
   HB_USHORT         uiVar;
} VAR_LINK;

typedef struct _DATABASE
{
   char *            szPath;
   HB_USHORT         uiDriver;
   struct _DATABASE * pNext;
} DATABASE;

typedef struct
{
   char *            szTagName;                /* TagName used internal by Harbour */
   char *            szBagName;                /* filename plus extension */
   char *            szFullPath;               /* complete non relative path to index */
   char *            szOrdKey;                 /* key expression of index */
   HB_USHORT         uiAreas;
   HB_BOOL           bCompound;                /* multiple TAGs in one index, e.g. CDX */
   HB_BOOL           bClear;                   /* temporary help var for OrdListClear() */
   HB_BOOL           bProduction;              /* CDX autoopened production order */
   HB_BOOL           bShared;                  /* temporary private index order */
   char              cKeyType;                 /* type of expression */
} INDEXSTRU, * PINDEXSTRU;                     /* 48 */

typedef struct _LETOTAG
{
   PINDEXSTRU        pIStru;
   char              szTagName[ LETO_MAX_TAGNAME + 1 ];
   PHB_ITEM          pTopScope;
   PHB_ITEM          pBottomScope;
   struct _LETOTAG * pNext;
} LETOTAG;

typedef struct
{
   char *            szCdp;                    /* CP or NULL for default */
   HB_BYTE *         szTable;                  /* may include leading [back]slash, name of table */
   HB_ULONG          ulRecCount;               /* Count of records in table */
   HB_U32            uiCrc;                    /* hash value for szTable speed search */
#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   HB_SPINLOCK_T     pMutex;
#else
   HB_CRITICAL_T     pMutex;
#endif
   HB_BOOL           bLocked;                  /* table filelock [ not reclock ] */
   unsigned char     uMemoType;                /* MEMO type DBT 1/ FPT 2/ SMT 3 */
   HB_USHORT         uiAreas;                  /* Number of references */
} GLOBESTRU, * PGLOBESTRU;                     /* 48 */

typedef struct
{
   HB_ULONG          ulAreaID;                 /* Global area number (virtual) */
   char *            szDriver;                 /* human readable driver used for this table, HB_RDD_MAX_DRIVERNAME_LEN */
   HB_BOOL           bShared;                  /* TRUE = non-exclusive opened */
   HB_BOOL           bReadonly;                /* TRUE = not write- lock- able */
   HB_BOOL           bMemIO;                   /* is this a HbMemIO table = special handling */
   HB_BOOL           bTemporary;               /* is this a named/ unnamed temporary table */
   HB_BYTE *         szTable;                  /* may include leading [back]slash, name of table */
   char              szLetoAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];  /* server internal alias: 7 + 1 or No_save_WA: 63 + 1 */
   HB_U32            uiCrc;                    /* hash value for szTable speed search */
   HB_USHORT         uiAreas;                  /* Number of references, 1 for s_bNoSaveWA */
   HB_USHORT         uiRecordLen;
   HB_USHORT         uiFields;                 /* number of fields per record */
   HB_USHORT         uiIndexCount;             /* index count opend by [all] users */
   LETO_LIST         LocksList;                /* List of records locked */
   LETO_LIST         IndexList;                /* Index List */
   HB_ULONG          ulFlags;                  /* Lock flags, some prehistoric relict ;-) */
   PGLOBESTRU        pGlobe;                   /* 1 to 1 relation into GLOBESTRU, for s_bNoSaveWA: n to 1 */
} TABLESTRU, * PTABLESTRU;                     /* 200 */

typedef struct
{
   HB_ULONG          ulAreaID;                 /* Global area number (virtual) */
   HB_ULONG          ulSelectID;               /* Client local area number (real) */
   PTABLESTRU        pTStru;                   /* reference to a TABLESTRU struct */
   HB_BOOL           bLocked;                  /* TABLE lock active */
   LETO_LIST         LocksList;                /* List of locked records */
   HB_USHORT         uiSkipBuf;                /* size of hotbuffer, can be different to glabal setting */
   LETOTAG *         pTag;                     /* List of index tags */
   LETOTAG *         pTagCurrent;              /* Last used index tag */
   PHB_ITEM          itmFltExpr;               /* CB with filter condition */
   HB_BOOL           itmFltOptimized;          /* optimized filter if not contain [user]function */
   char              szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];        /* !client! alias -- 63 + 1 */
   HB_U32            uiCrc;                    /* hash value for szAlias name speed search */
   HB_BOOL           bNotDetached;             /* Detached */
   HB_ULONG          ulUdf;                    /* pUStru->iUserStru ID if table was new opened/ created in UDP mode */
   HB_BOOL           bUseSkipBuffer;           /* for temporary disable uiSkipBuf */
#ifdef __BM
   void *            pBM;
#endif
} AREASTRU, * PAREASTRU;                       /* 200 */

typedef struct
{
   int               iUserStru;
   HB_CRITICAL_T     pMutex;
   HB_SOCKET         hSocket;                 /* socket for odinary communication */
   HB_SOCKET         hSocketErr;              /* second socket for delayed errors */
   PHB_ZNETSTREAM    zstream;                 /* structure used for compressed traffic */
   HB_BYTE *         pBuffer;                 /* recv buffer */
   HB_ULONG          ulBufferLen;             /* allocated size of pBuffer */
   HB_ULONG          ulDataLen;               /* raw lenght of current request */
   HB_BYTE *         pSendBuffer;             /* used for ( zip, etc ) data transfer */
   HB_ULONG          ulSndBufLen;
   char *            szVersion;
   unsigned int      uiMajorVer;
   unsigned int      uiMinorVer;
   HB_BYTE *         szAddr;                  /* ip address, empty for headless UDF */
   HB_BYTE *         szNetname;
   HB_BYTE           szExename[ 24 ];
   HB_BYTE *         szUsername;
   PHB_CODEPAGE      cdpage;                  /* codepage */
   HB_USHORT         uiDriver;                /* contains a '1' for NTX index mode */
   char              szDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];   /* last used = active RDD */
   char *            szDateFormat;
   unsigned int      uiEpoch;
   HB_BOOL           bDeleted;                /* value to spare unnecessary leto_setSetDeleted() calls */
   char              szAccess[ 2 ];
   char              cDopcode[ LETO_DOPCODE_LEN + 1 ];   /* random bytes > 0 flexible mixed into LETO_PASSWORD */
   HB_BOOL           bLastAct;                /* temporary value, internal used as replace for void return value */
   HB_I64            llLastAct;               /* seconds ago of last activity */
   HB_U64            ullCPULoad;              /* milliseconds sum of server CPU load for requests except network time */
   LETO_LIST         AreasList;
   HB_USHORT         uiAreasCount;
   VAR_LINK *        pVarLink;
   HB_USHORT         uiVarsOwnCurr;
   HB_THREAD_HANDLE  hThread;
   HB_THREAD_ID      hThreadID;
   HB_ULONG          ulCurAreaID;             /* activ selected global area number (virtual) */
   PAREASTRU         pCurAStru;               /* activ AREASTRU */
   HB_ULONG          ulBytesSend;             /* bytes send with sendAnswer() */
   char              szLastRequest[ 64 ];     /* first max. 63 bytes of the last request */
   HB_BOOL           bNoAnswer;               /* all fine, so no ACK sent -- used temporary */
   int               iHbError;                /* result of hb_errGetOsCode( pError ) or '1' in case of VM error */
   char *            szHbError;               /* ":genCode-subCode-osCode-flags['\t'filename]" */
   HB_BYTE *         pBufCrypt;
   int               iZipRecord;              /* zlib compress level default -1 = off, 0 - 9 */
   HB_ULONG          ulBufCryptLen;
   HB_BOOL           bZipCrypt;               /* encryption ON for LZ4 network traffic */
   HB_BOOL           bDbEvalCompat;           /* enable scope to REST if WHILE/NEXT given */
   HB_BOOL           bBufKeyNo;
   HB_BOOL           bBufKeyCount;
   int               iPort;
   int               iLockTimeOut;            /* used for RDDI_AUTOLOCK; value >= 0; 0 = none ms */
   HB_FHANDLE        hSockPipe[ 2 ];          /* the magic pipe */
   HB_BOOL           bCloseConnection;        /* connection will be closed, e.g. after wrong login */
   HB_BOOL           bBeQuiet;                /* disable network answer for request; used by UDF functions */
   HB_BOOL           bRpcMode;                /* indicate an UDF running in an independent thread, own WA-set */
   HB_BOOL           bSetExclusive;           /* used by UDF: representing client set of _set_exclusive */
   HB_ULONG          ulUdfAreaID;             /* active workarea-id when the UDF started */
   HB_BYTE           uSrvLock;                /* 0 or type of server 'lock for e.g. replication' mode */
   HB_BOOL           bGCCollect;              /* true if this thread is designated to do GC */
   HB_MAXINT *       pOpenHandles;            /* list of files opened by Leto_Fopen()/ Leto_FCreate() */
   HB_ULONG          ulOpenHandles;           /* number of open file handles */
} USERSTRU, * PUSERSTRU;                      /* 536 */

typedef struct
{
   AREAP             pArea;
   PAREASTRU         pAStru;
   HB_ULONG          ulSelectID;              /* Client local area number (real) */
   HB_ULONG          ulRecNo;
   HB_BOOL           bAppend;
   HB_BOOL           bLockable;
   HB_USHORT         uiFlag;
   HB_USHORT         uiItems;
   HB_USHORT *       puiIndex;
   PHB_ITEM *        pItems;
} TRANSACTSTRU;

typedef struct
{
   HB_ULONG *        pulAreaID;
   HB_ULONG          ulNextID;
   int               iCurIndex;
   int               iAllocIndex;
} AVAILAREAID;

typedef struct
{
   HB_USHORT         uiUslen;
   HB_USHORT         uiPasslen;
   char *            szUser;                  /* max LETO_MAX_USERNAME + 1 */
   char *            szPass;                  /* max LETO_MAX_KEYLENGTH + 1 */
   char              szAccess[ 2 ];
} ACCSTRU, * PACCSTRU;


#ifdef __HARBOUR30__
   extern HB_SIZE hb_fsPipeWrite( HB_FHANDLE hPipeHandle, const void * buffer, HB_SIZE nSize, HB_MAXINT nTimeOut );
#endif

