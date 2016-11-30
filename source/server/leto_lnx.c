/*
 * Leto db server (linux) functions
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

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "srvleto.h"


HB_FUNC( LETO_DAEMON )
{
   //int	iPID, stdioFD, i, numFiles, iDupOut, iDupErr;
   int        iPID, i, numFiles;
   HB_FHANDLE hNull;

   iPID = fork();

   switch( iPID )
   {
      case 0:             /* we are the child process */
         break;

      case -1:            /* error - bail out (fork failing is very bad) */
         fprintf( stderr, "Error: initial fork failed: %s\n", strerror( errno ) );
         hb_retl( HB_FALSE );
         return;

      default:            /* we are the parent, so exit */
         hb_vmRequestQuit();
         break;
   }

   if( setsid() < 0 )
   {
      hb_retl( HB_FALSE );
      return;
   }

   numFiles = sysconf( _SC_OPEN_MAX );  /* how many file descriptors? */
   for( i = numFiles - 1; i >= 0; --i ) /* close all open files except lock */
   {
      close( i );
   }

   hNull = open( "/dev/null", O_RDWR );
   dup2( hNull, 0 );               /* fd 0 = stdin */
   dup2( hNull, 1 );               /* fd 1 = stdout */
   dup2( hNull, 2 );               /* fd 1 = stderr */
   if( hNull != ( HB_FHANDLE ) -1 )
      hb_fsClose( hNull );

   ( void ) setuid( getuid() );
   ( void ) setgid( getgid() );
   umask( 0x02 );                             /* set this to whatever is appropriate for you */

   setpgrp();
   hb_retl( HB_TRUE );
   return;
}

HB_FUNC( LETO_UMASK )
{
   if( HB_ISNUM( 1 ) )
   {
      mode_t mask = ( mode_t ) hb_parni( 1 );
      umask( mask );
   }
}

