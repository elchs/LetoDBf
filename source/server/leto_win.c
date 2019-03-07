/*
 * Leto db server (Windows) functions
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 *           2018 Rolf 'elch' Beckmann
 *                ( formatting, fixes, service description, ... )
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
#define _SERVICE_NAME_LEN      256

static SERVICE_STATUS_HANDLE s_hServiceHandle = 0;
static char     s_ServiceEntryFunc[ HB_SYMBOL_NAME_LEN + 1 ];
static TCHAR    s_ServiceName[ _SERVICE_NAME_LEN ] = _SERVICE_NAME;
static TCHAR    s_ServiceDisplayName[ _SERVICE_NAME_LEN ] = _SERVICE_NAME;
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

static DWORD WINAPI leto_GetServiceStatus( void )
{
   DWORD     dwState = 0;
   SC_HANDLE schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_CONNECT );

   if( schSCM )
   {
      SC_HANDLE schSrv = OpenService( schSCM, s_ServiceName, SERVICE_QUERY_STATUS );

      if( schSrv )
      {
         SERVICE_STATUS ss;

         if( QueryServiceStatus( schSrv, &ss ) )
            dwState = ss.dwCurrentState;
         CloseServiceHandle( schSrv );
      }
      else
         s_ulSvcError = ( HB_ULONG ) GetLastError();

      CloseServiceHandle( schSCM );
   }
   else
      s_ulSvcError = ( HB_ULONG ) GetLastError();

   return dwState;
}

/* wait max 10 second to shutdown */
void WINAPI leto_ServiceControlHandler( DWORD dwCtrlCode )
{
   DWORD dwState = SERVICE_RUNNING;
   DWORD dwStartTime = GetTickCount();

   switch( dwCtrlCode )
   {
      case SERVICE_CONTROL_STOP:
         dwState = SERVICE_STOPPED;
         leto_SrvShutDown( 1 );
         while( leto_GetServiceStatus() != SERVICE_STOPPED )
         {
            Sleep( 500 );
            if( GetTickCount() - dwStartTime > 10000 )
               break;
         }
         break;

      case SERVICE_CONTROL_SHUTDOWN:
         dwState = SERVICE_STOPPED;
         leto_SrvShutDown( 1 );
         while( leto_GetServiceStatus() != SERVICE_STOPPED )
         {
            Sleep( 500 );
            if( GetTickCount() - dwStartTime > 10000 )
               break;
         }
         break;
   }

   leto_SetServiceStatus( dwState );
}

static void WINAPI leto_ServiceMainFunction( DWORD dwArgc, LPTSTR * lpszArgv )
{
   HB_SYMBOL_UNUSED( dwArgc );
   HB_SYMBOL_UNUSED( lpszArgv );

   s_hServiceHandle = RegisterServiceCtrlHandler( s_ServiceName, ( LPHANDLER_FUNCTION ) leto_ServiceControlHandler );

   if( s_hServiceHandle != ( SERVICE_STATUS_HANDLE ) 0 )
   {
      PHB_DYNS pDynSym;

      hb_vmThreadInit( NULL );
      pDynSym = hb_dynsymFindName( s_ServiceEntryFunc );

      if( pDynSym )
      {
         leto_SetServiceStatus( SERVICE_RUNNING );

         if( hb_vmRequestReenter() )
         {
            hb_vmPushDynSym( pDynSym );
            hb_vmPushNil();
            hb_vmProc( 0 );

            hb_vmRequestRestore();
         }
      }

      hb_vmThreadQuit();
      leto_SetServiceStatus( SERVICE_STOPPED );
   }
}

HB_FUNC( LETO_SERVICESTART )
{
   HB_BOOL bRetVal = HB_FALSE;

   if( hb_parclen( 2 ) && strcmp( hb_parc( 2 ), _SERVICE_NAME ) )
      hb_strncpy( s_ServiceName, hb_parc( 2 ), _SERVICE_NAME_LEN - 1 );

   if( hb_parclen( 1 ) )
   {
      SERVICE_TABLE_ENTRY lpServiceTable[ 2 ] = { { s_ServiceName, ( LPSERVICE_MAIN_FUNCTION ) leto_ServiceMainFunction },
                                                  { NULL, NULL } };

      hb_strncpy( s_ServiceEntryFunc, hb_parc( 1 ), HB_SYMBOL_NAME_LEN );
      if( StartServiceCtrlDispatcher( lpServiceTable ) )
         bRetVal = HB_TRUE;
      else
         s_ulSvcError = ( HB_ULONG ) GetLastError();
   }

   hb_retl( bRetVal );
}

/* need a string for service description as first param - optional service name & config filename */
HB_FUNC( LETO_SERVICEINSTALL )
{
   HB_BOOL bRetVal = HB_FALSE;
   TCHAR   szPath[ MAX_PATH  + 1 ] = { 0 };

   if( hb_parclen( 1 ) && GetModuleFileName( NULL, szPath, MAX_PATH ) )
   {
      SC_HANDLE    schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );
      unsigned int uiLen = strlen( szPath );

      /* adapt if non-default service name */
      if( hb_parclen( 2 ) && strcmp( hb_parc( 2 ), _SERVICE_NAME ) )
      {
         hb_strncpy( s_ServiceName, hb_parc( 2 ), _SERVICE_NAME_LEN - 1 );
         hb_strncpy( s_ServiceDisplayName, hb_parc( 2 ), _SERVICE_NAME_LEN - 1 );
      }

      /* add ' config letodb.ini' to path and display name */
      if( hb_parclen( 3 ) && uiLen < MAX_PATH - hb_parclen( 3 ) - 8 )
      {
         strcpy( szPath + uiLen, " config " );
         uiLen += 8;
         strcpy( szPath + uiLen, hb_parc( 3 ) );

         uiLen = strlen( s_ServiceDisplayName );
         if( uiLen < _SERVICE_NAME_LEN - hb_parclen( 3 ) - 1 )
         {
            s_ServiceDisplayName[ uiLen ] = ' ';
            strcpy( s_ServiceDisplayName + uiLen + 1, hb_parc( 3 ) );
         }
      }

      if( schSCM )
      {
         SC_HANDLE schSrv = CreateService( schSCM,
                                           s_ServiceName,
                                           s_ServiceDisplayName,
                                           SERVICE_ALL_ACCESS,
                                           SERVICE_WIN32_OWN_PROCESS,
                                           SERVICE_AUTO_START,        /* SERVICE_DEMAND_START */
                                           SERVICE_ERROR_NORMAL,
                                           szPath,
                                           NULL, NULL, NULL, NULL, NULL );

         if( schSrv )
         {
            SERVICE_DESCRIPTION sd;

            sd.lpDescription = hb_strdup( hb_parc( 1 ) );
            ChangeServiceConfig2( schSrv, SERVICE_CONFIG_DESCRIPTION, &sd );
            CloseServiceHandle( schSrv );
            bRetVal = HB_TRUE;
         }
         else
            s_ulSvcError = ( HB_ULONG ) GetLastError();

         CloseServiceHandle( schSCM );
      }
      else
         s_ulSvcError = ( HB_ULONG ) GetLastError();
   }

   hb_retl( bRetVal );
}

/* not used */
HB_FUNC( LETO_GETSERVICESTATE )
{
   if( hb_parclen( 1 ) && strcmp( hb_parc( 1 ), _SERVICE_NAME ) )
      hb_strncpy( s_ServiceName, hb_parc( 1 ), _SERVICE_NAME_LEN - 1 );

   hb_retni( leto_GetServiceStatus() );
}

HB_FUNC( LETO_SERVICEDELETE )
{
   HB_BOOL   bRetVal = HB_FALSE;
   SC_HANDLE schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );

   if( hb_parclen( 1 ) && strcmp( hb_parc( 1 ), _SERVICE_NAME ) )
      hb_strncpy( s_ServiceName, hb_parc( 1 ), _SERVICE_NAME_LEN - 1 );

   if( schSCM )
   {
      SC_HANDLE schService = OpenService( schSCM, s_ServiceName, SERVICE_STOP | SERVICE_QUERY_STATUS );

      /* stop the service if running */
      if( schService )
      {
         SERVICE_STATUS_PROCESS ssp;
         DWORD dwStartTime, dwWaitTime, dwBytesNeeded;

         /* Make sure the service is stopped */
         if( QueryServiceStatusEx( schService, SC_STATUS_PROCESS_INFO, ( LPBYTE ) &ssp,
                                   sizeof( SERVICE_STATUS_PROCESS ), &dwBytesNeeded ) )
         {
            dwStartTime = GetTickCount();
            /* If a stop is pending, wait for it */
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
                  break;

               if( ssp.dwCurrentState == SERVICE_STOPPED )
                  break;

               /* wait max 10 seconds */
               if( GetTickCount() - dwStartTime > 10000 )
                  break;
            }
         }

         /* Send a stop code to the service */
         if( ControlService( schService, SERVICE_CONTROL_STOP, ( LPSERVICE_STATUS ) &ssp ) )
         {
            dwStartTime = GetTickCount();
            /* Wait for the service to stop. */
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
                  break;

               if( ssp.dwCurrentState == SERVICE_STOPPED )
                  break;

               /*  wait max 10 seconds */
               if( GetTickCount() - dwStartTime > 10000 )
                  break;
            }
         }

         CloseServiceHandle( schService );
      }

      /* delete service */
      schService = OpenService( schSCM, s_ServiceName, DELETE );
      if( schService )
      {
         bRetVal = ( HB_BOOL ) DeleteService( schService );
         CloseServiceHandle( schService );
         if( ! bRetVal )
            s_ulSvcError = ( HB_ULONG ) GetLastError();
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

