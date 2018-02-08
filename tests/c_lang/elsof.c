/*
 * optimized lsof to search max. 2 Pid for opened filename
 * (c) 2018 Rolf 'elch' Beckmann
 */

#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>

void main( const int argc, const char * argv[] )
{
   char            szProc[ 32 ], szLinkFd[ 64 ], szLink[ 256 ];
   DIR *           procfd, * proc = opendir( "/proc" );
   struct dirent * ent, * entfd;
   int             iFound = 0;
   register int    iLenLink;
   register char * pptr;
   register char * ptr;

   if( proc == NULL || argc < 2 )
      exit( EXIT_FAILURE );

   while( ent = readdir( proc ) )
   {
      pptr = ent->d_name;
      if( ! isdigit( *pptr ) )
         continue;
      else
      {
         pptr = ent->d_name;
         ptr = szProc;
         *ptr++ = '/';
         *ptr++ = 'p';
         *ptr++ = 'r';
         *ptr++ = 'o';
         *ptr++ = 'c';
         *ptr++ = '/';
         while( *pptr != '\0' )
            *ptr++ = *pptr++;
         *ptr++ = '/';
         *ptr++ = 'f';
         *ptr++ = 'd';
         *ptr = '\0';

         if( procfd = opendir( szProc ) )
         {
            while( entfd = readdir( procfd ) )
            {
               if( *entfd->d_name == '.' )
                  continue;
               ptr = szLinkFd;
               pptr = szProc;
               while( *pptr != '\0' )
                  *ptr++ = *pptr++;
               *ptr++ = '/';
               pptr = entfd->d_name;
               while( *pptr != '\0' )
                  *ptr++ = *pptr++;
               *ptr = '\0';
               iLenLink = readlink( szLinkFd, szLink, 255 );
               if( *szLink != '/' )
                  continue;
               szLink[ iLenLink ] = '\0';
               if( strstr( szLink, argv[ 1 ] ) )
               {
                  printf( "%s:%s\n", ent->d_name, szLink );
                  if( ++iFound > 1 )
                     break;
               }
            }

            closedir( procfd );
            if( iFound > 1 )
               break;
         }
         else  /* missing root access ? */
         {
            closedir( proc );
            exit( EXIT_FAILURE );
         }
      }
   }

   closedir( proc );
   exit( EXIT_SUCCESS );
}

