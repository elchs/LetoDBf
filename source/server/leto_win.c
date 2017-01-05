/*
 * Leto db server (Windows) functions
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


#ifdef __WIN_SERVICE__

#include "srvleto.h"

#define _SERVICE_NAME          "LetoDBf_Service"
#define _SERVICE_DISPLAY_NAME  "LetoDBf Service"

static SERVICE_STATUS_HANDLE s_hServiceHandle = 0;
static char     s_ServiceEntryFunc[ HB_SYMBOL_NAME_LEN + 1 ];
static TCHAR    s_ServiceName[] = _SERVICE_NAME;
static TCHAR    s_ServiceDisplayName[] = _SERVICE_DISPLAY_NAME;
static HB_ULONG s_ulSvcError = 0;

extern void leto_SrvShutDown( unsigned int uiWait );

void leto_SetServiceStatus( DWORD State )
{
   SERVICE_STATUS SrvStatus;

   SrvStatus.dwServiceType = SERVICE_WIN32_OWN_PROCESS;
   SrvStatus.dwCurrentState = State;
   SrvStatus.dwControlsAccepted = SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN;
   SrvStatus.dwWin32ExitCode = 0;
   SrvStatus.dwServiceSpecificExitCode = 0;
   SrvStatus.dwCheckPoint = 0;
   SrvStatus.dwWaitHint = 0;

   if( s_hServiceHandle )
      SetServiceStatus( s_hServiceHandle, &SrvStatus );
}

DWORD WINAPI leto_GetServiceStatus( void )
{
   DWORD State = 0;
   SC_HANDLE schSCM;

   schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_CONNECT );

   if( schSCM )
   {
      SC_HANDLE schSrv;

      schSrv = OpenService( schSCM, s_ServiceName, SERVICE_QUERY_STATUS);
      if( schSrv )
      {
         SERVICE_STATUS ss;

         if( QueryServiceStatus( schSrv, &ss ) )
            State = ss.dwCurrentState;
         CloseServiceHandle( schSrv );
      }
      else
         s_ulSvcError = ( HB_ULONG ) GetLastError();

      CloseServiceHandle( schSCM );
   }
   else
      s_ulSvcError = ( HB_ULONG ) GetLastError();

   return State;
}

void WINAPI leto_ServiceControlHandler( DWORD dwCtrlCode )
{
   DWORD State = SERVICE_RUNNING;
   double dExtraWait = 5.0;

   switch( dwCtrlCode )
   {
      case SERVICE_CONTROL_STOP:
         State = SERVICE_STOPPED;
         leto_SrvShutDown( 1 );
         while( dExtraWait > 0 && leto_GetServiceStatus() != SERVICE_STOPPED )
         {
            hb_idleSleep( 1 );
            dExtraWait -= 1;
         }
         break;

      case SERVICE_CONTROL_SHUTDOWN:
         State = SERVICE_STOPPED;
         leto_SrvShutDown( 1 );
         while( dExtraWait > 0 && leto_GetServiceStatus() != SERVICE_STOPPED )
         {
            hb_idleSleep( 1 );
            dExtraWait -= 1;
         }
         break;
   }

   leto_SetServiceStatus( State );
}

void WINAPI leto_ServiceMainFunction( DWORD dwArgc, LPTSTR * lpszArgv )
{
   HB_SYMBOL_UNUSED( dwArgc );
   HB_SYMBOL_UNUSED( lpszArgv );

   s_hServiceHandle = RegisterServiceCtrlHandler( s_ServiceName, ( LPHANDLER_FUNCTION ) leto_ServiceControlHandler );

   if( s_hServiceHandle != ( SERVICE_STATUS_HANDLE ) 0 )
   {
      PHB_DYNS pDynSym;

      leto_SetServiceStatus( SERVICE_RUNNING );

      hb_vmThreadInit( NULL );

      pDynSym = hb_dynsymFindName( s_ServiceEntryFunc );

      if( pDynSym )
      {
         if( hb_vmRequestReenter() )
         {
            hb_vmPushSymbol( hb_dynsymSymbol( pDynSym ) );
            hb_vmPushNil();
            hb_vmProc( 0 );

            hb_vmRequestRestore();
         }
      }

      leto_SetServiceStatus( SERVICE_STOPPED );

      hb_vmThreadQuit();
   }
}

HB_FUNC( LETO_SERVICESTART )
{
   HB_BOOL bRetVal = HB_FALSE;
   SERVICE_TABLE_ENTRY lpServiceTable[ 2 ] = { { s_ServiceName, ( LPSERVICE_MAIN_FUNCTION ) leto_ServiceMainFunction },
                                               { NULL,        NULL                                                 } };

   hb_strncpy( s_ServiceEntryFunc, hb_parc( 1 ), HB_SYMBOL_NAME_LEN );

   if( StartServiceCtrlDispatcher( lpServiceTable ) )
      bRetVal = HB_TRUE;
   else
      s_ulSvcError = ( HB_ULONG ) GetLastError();

   hb_retl( bRetVal );
}


HB_FUNC( LETO_SERVICEINSTALL )
{
   HB_BOOL   bRetVal = HB_FALSE;
   TCHAR     szPath[ MAX_PATH ];
   SC_HANDLE schSrv;

   if( GetModuleFileName( NULL, szPath, MAX_PATH ) )
   {
      SC_HANDLE schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );

      if( schSCM )
      {
         schSrv = CreateService( schSCM,
                                 s_ServiceName,
                                 s_ServiceDisplayName,
                                 SERVICE_ALL_ACCESS,
                                 SERVICE_WIN32_OWN_PROCESS,
                                 SERVICE_AUTO_START,        //SERVICE_DEMAND_START,
                                 SERVICE_ERROR_NORMAL,
                                 szPath,
                                 NULL, NULL, NULL, NULL, NULL );

         if( schSrv )
         {
            CloseServiceHandle( schSrv );
            bRetVal = HB_TRUE;
         }
         else
         {
            s_ulSvcError = ( HB_ULONG ) GetLastError();
         }

         CloseServiceHandle( schSCM );
      }
      else
         s_ulSvcError = ( HB_ULONG ) GetLastError();
   }

   hb_retl( bRetVal );
}

HB_FUNC( LETO_GETSERVICESTATE )
{
   hb_retni( leto_GetServiceStatus() );
}

HB_FUNC( LETO_SERVICEDELETE )
{
   HB_BOOL bRetVal = HB_FALSE;

   SC_HANDLE schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );

   if( schSCM )
   {
      SC_HANDLE schService;
      SERVICE_STATUS_PROCESS ssp;
      DWORD dwStartTime, dwTimeout, dwWaitTime;
      DWORD dwBytesNeeded;

      // check and stop
      schService = OpenService( schSCM, s_ServiceName, SERVICE_STOP | SERVICE_QUERY_STATUS );
      if( schService )
      {
         dwStartTime = GetTickCount();
         // Make sure the service is not already stopped.
         if( QueryServiceStatusEx( schService, SC_STATUS_PROCESS_INFO, ( LPBYTE ) &ssp,
                                   sizeof( SERVICE_STATUS_PROCESS ), &dwBytesNeeded ) )
         {
            // If a stop is pending, wait for it.
            dwTimeout = 30000; // 30-second time-out
            while( ssp.dwCurrentState == SERVICE_STOP_PENDING )
            {
               dwWaitTime = ssp.dwWaitHint / 10;
               if( dwWaitTime < 1000 )
                  dwWaitTime = 1000;
               else if( dwWaitTime > 10000 )
                  dwWaitTime = 10000;

               Sleep( dwWaitTime );

               if( ! QueryServiceStatusEx( schService, SC_STATUS_PROCESS_INFO, ( LPBYTE ) &ssp,
                                           sizeof( SERVICE_STATUS_PROCESS ), &dwBytesNeeded ) )
               {
                  break;
               }

               if( ssp.dwCurrentState == SERVICE_STOPPED )
                  break;

               if( GetTickCount() - dwStartTime > dwTimeout )
                  break;
            }
         }

         // Send a stop code to the service.
         if( ControlService( schService, SERVICE_CONTROL_STOP, ( LPSERVICE_STATUS ) &ssp ) )
         {
            // Wait for the service to stop.
            dwTimeout = 10000; // 10-second time-out
            while( ssp.dwCurrentState != SERVICE_STOPPED )
            {
               dwWaitTime = ssp.dwWaitHint / 10;
               if( dwWaitTime < 1000 )
                  dwWaitTime = 1000;
               else if( dwWaitTime > 10000 )
                  dwWaitTime = 10000;

               Sleep( dwWaitTime );

               if( ! QueryServiceStatusEx( schService, SC_STATUS_PROCESS_INFO, ( LPBYTE ) &ssp,
                                           sizeof( SERVICE_STATUS_PROCESS ), &dwBytesNeeded ) )
               {
                  break;
               }
               if( ssp.dwCurrentState == SERVICE_STOPPED )
                  break;

               if( GetTickCount() - dwStartTime > dwTimeout )
                  break;
            }
         }

         CloseServiceHandle( schService );
      }

      // delete
      schService = OpenService( schSCM, s_ServiceName, DELETE );
      if( schService )
      {
         bRetVal = ( HB_BOOL ) DeleteService( schService );
         CloseServiceHandle( schService );
         if( ! bRetVal )
         {
            s_ulSvcError = ( HB_ULONG ) GetLastError();
         }
      }
      else
         s_ulSvcError = ( HB_ULONG ) GetLastError();

      CloseServiceHandle( schSCM );
   }

   hb_retl( bRetVal );
}

HB_FUNC( LETOWIN_GETLASTERROR )
{
   hb_retnl( s_ulSvcError );
}

#endif

