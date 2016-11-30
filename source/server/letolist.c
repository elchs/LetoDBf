/*
 * Harbour Leto singly-linked lists functions
 *
 * Copyright 2012 Pavel Tsarenko <tpe2 / at / mail.ru>
 *           2016 Rolf 'elch' Beckmann
 *                mods and optimize, e.g. pLastItem, spinlocks
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

#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   #define HB_GC_LOCKL()    HB_SPINLOCK_ACQUIRE( &pList->pMutex )
   #define HB_GC_UNLOCKL()  HB_SPINLOCK_RELEASE( &pList->pMutex )
#else
   #define HB_GC_LOCKL()    hb_threadEnterCriticalSection( &pList->pMutex )
   #define HB_GC_UNLOCKL()  hb_threadLeaveCriticalSection( &pList->pMutex )
#endif


void letoListInit( PLETO_LIST pList, HB_ULONG ulSize )
{
   pList->ulSize = ulSize;
   pList->pItem = NULL;
   pList->pLastItem = NULL;
#if defined( HB_SPINLOCK_INIT ) && ! defined( HB_HELGRIND_FRIENDLY )
   pList->pMutex = HB_SPINLOCK_INIT;
#endif
}

/* for external locking/ unlocking */
void letoListLock( PLETO_LIST pList )
{
   HB_GC_LOCKL();
}

void letoListUnlock( PLETO_LIST pList )
{
   HB_GC_UNLOCKL();
}

/* without mutex locking, use if needed letoListLock() */
void letoClearList( PLETO_LIST pList )
{
   PLETO_LIST_ITEM pItem, pLast;

   pItem = pList->pItem;
   while( pItem )
   {
      pLast = pItem;
      pItem = pItem->pNext;
      hb_xfree( pLast );
   }
   pList->pItem = NULL;
   pList->pLastItem = NULL;
}

void letoListFree( PLETO_LIST pList )
{
   letoClearList( pList );
}

HB_BOOL letoListEmptyTS( PLETO_LIST pList )
{
   HB_BOOL fEmpty;

   HB_GC_LOCKL();
   fEmpty = pList->pItem ? HB_FALSE : HB_TRUE;
   HB_GC_UNLOCKL();

   return fEmpty;
}

void * letoAddToList( PLETO_LIST pList )
{
   PLETO_LIST_ITEM pNewItem = ( PLETO_LIST_ITEM ) hb_xgrabz( pList->ulSize + sizeof( PLETO_LIST_ITEM ) );

   if( pList->pLastItem )
      pList->pLastItem = pList->pLastItem->pNext = pNewItem;
   else
      pList->pLastItem = pList->pItem = pNewItem;

   return pNewItem + 1;
}

void * letoGetListItem( PLETO_LIST pList, HB_ULONG uiNum )
{
   PLETO_LIST_ITEM pItem = pList->pItem;

   if( pItem )
   {
      if( ! uiNum )
         return pItem + 1;
      else
      {
         for( ;; )
         {
            pItem = pItem->pNext;
            if( ! pItem )
               return NULL;
            if( ! --uiNum )
               return pItem + 1;
         }
      }
   }

   return NULL;
}

void letoDelFromList( PLETO_LIST pList, HB_ULONG ulNum )
{
   PLETO_LIST_ITEM pItem = pList->pItem;
   PLETO_LIST_ITEM pLast = NULL;

   while( pItem )
   {
      if( ! ulNum )
      {
         if( pLast )
            pLast->pNext = pItem->pNext;
         else
            pList->pItem = ( PLETO_LIST_ITEM ) pItem->pNext;

         if( ! pItem->pNext )  /* a xfree at tail of list */
            pList->pLastItem = pLast;

         hb_xfree( pItem );
         break;
      }
      else
      {
         ulNum--;
         pLast = pItem;
         pItem = pItem->pNext;
      }
   }
}

void letoDelItemList( PLETO_LIST pList, PLETO_LIST_ITEM pDelItem )
{
   PLETO_LIST_ITEM pItem = pList->pItem;
   PLETO_LIST_ITEM pLast = NULL;

   while( pItem )
   {
      if( ( pItem + 1 ) == pDelItem )
      {
         if( pLast )
            pLast->pNext = pItem->pNext;
         else
            pList->pItem = ( PLETO_LIST_ITEM ) pItem->pNext;

         if( ! pItem->pNext )  /* a xfree at tail of list */
            pList->pLastItem = pLast;

         hb_xfree( pItem );
         break;
      }
      else
      {
         pLast = pItem;
         pItem = pItem->pNext;
      }
   }
}

HB_BOOL letoIsRecInList( PLETO_LIST pList, HB_ULONG ulRecNo )
{
   PLETO_LOCK_ITEM pItem;

   if( pList->pLastItem && ( ( PLETO_LOCK_ITEM ) pList->pLastItem )->ulRecNo <= ulRecNo )
      pItem = ( PLETO_LOCK_ITEM ) pList->pLastItem;
   else
      pItem = ( PLETO_LOCK_ITEM ) pList->pItem;

   while( pItem )
   {
      if( pItem->ulRecNo == ulRecNo )
      {
         if( ( PLETO_LIST_ITEM ) pItem != pList->pItem )  /* not the first */
            pList->pLastItem = ( PLETO_LIST_ITEM ) pItem;
         return HB_TRUE;
      }
      else if( pItem->ulRecNo > ulRecNo )
         return HB_FALSE;
      else
         pItem = pItem->pNext;
   }

   return HB_FALSE;
}

/* with mutex lock */
HB_BOOL letoIsRecInListTS( PLETO_LIST pList, HB_ULONG ulRecNo )
{
   HB_BOOL fFound;

   HB_GC_LOCKL();
   fFound = letoIsRecInList( pList, ulRecNo );
   HB_GC_UNLOCKL();

   return fFound;
}

/* without mutex lock -- alike search logic, but with immediate adding */
HB_BOOL letoAddRecToList( PLETO_LIST pList, HB_ULONG ulRecNo, HB_BOOL fAdd )
{
   HB_BOOL fFound = HB_FALSE;

   if( pList->pItem )
   {
      PLETO_LOCK_ITEM pLast = NULL;
      PLETO_LOCK_ITEM pItem;

      /* lower !! than ulRecNo to ensure pLast */
      if( pList->pLastItem && ( ( PLETO_LOCK_ITEM ) pList->pLastItem )->ulRecNo < ulRecNo )
         pItem = ( PLETO_LOCK_ITEM ) pList->pLastItem;
      else
         pItem = ( PLETO_LOCK_ITEM ) pList->pItem;

      while( pItem )
      {
         if( pItem->ulRecNo == ulRecNo )
         {
            fFound = HB_TRUE;
            break;
         }
         else if( pItem->ulRecNo > ulRecNo )
            break;
         else
         {
            pLast = pItem;
            pItem = pItem->pNext;
         }
      }
      if( ! fFound && fAdd )
      {
         PLETO_LOCK_ITEM pNew = ( PLETO_LOCK_ITEM ) hb_xgrab( sizeof( LETO_LOCK_ITEM ) );

         pNew->ulRecNo = ulRecNo;
         if( pLast )  /* not the first */
         {
            pNew->pNext = pLast->pNext;
            pLast->pNext = pNew;
            pList->pLastItem = ( PLETO_LIST_ITEM ) pLast->pNext;
         }
         else
         {
            pNew->pNext = ( PLETO_LOCK_ITEM ) pList->pItem;
            pList->pItem = ( PLETO_LIST_ITEM ) pNew;
         }
      }
   }
   else if( fAdd )
   {
      PLETO_LOCK_ITEM pNew = ( PLETO_LOCK_ITEM ) hb_xgrab( sizeof( LETO_LOCK_ITEM ) );

      pNew->ulRecNo = ulRecNo;
      pNew->pNext = NULL;
      pList->pItem = ( PLETO_LIST_ITEM ) pNew;
   }

   return fFound;
}

/* see param fAdd, is like letoIsRecInList(), but with immediate MT safe add if HB_TRUE */
HB_BOOL letoAddRecToListTS( PLETO_LIST pList, HB_ULONG ulRecNo, HB_BOOL fAdd )
{
   HB_BOOL fFound;

   HB_GC_LOCKL();
   fFound = letoAddRecToList( pList, ulRecNo, fAdd );
   HB_GC_UNLOCKL();

   return fFound;
}

HB_BOOL letoDelRecFromList( PLETO_LIST pList, HB_ULONG ulRecNo )
{
   PLETO_LOCK_ITEM pItem, pLast = NULL;
   HB_BOOL         fRet = HB_FALSE;

   /* lower !! than ulRecNo to ensure pLast */
   if( pList->pLastItem && ( ( PLETO_LOCK_ITEM ) pList->pLastItem )->ulRecNo < ulRecNo )
      pItem = ( PLETO_LOCK_ITEM ) pList->pLastItem;
   else
      pItem = ( PLETO_LOCK_ITEM ) pList->pItem;

   while( pItem )
   {
      if( pItem->ulRecNo == ulRecNo )
      {
         if( pLast )
            pLast->pNext = pItem->pNext;
         else
            pList->pItem = ( PLETO_LIST_ITEM ) pItem->pNext;
         if( ( PLETO_LIST_ITEM ) pItem == pList->pLastItem )
            pList->pLastItem = NULL;
         hb_xfree( pItem );
         fRet = HB_TRUE;
         break;
      }
      else if( pItem->ulRecNo > ulRecNo )
         break;
      else
      {
         pLast = pItem;
         pItem = pItem->pNext;
      }
   }

   return fRet;
}

/* with mutex lock */
HB_BOOL letoDelRecFromListTS( PLETO_LIST pList, HB_ULONG ulRecNo )
{
   HB_BOOL fDeleted;

   HB_GC_LOCKL();
   fDeleted = letoDelRecFromList( pList, ulRecNo );
   HB_GC_UNLOCKL();

   return fDeleted;
}

