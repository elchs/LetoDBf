/*
 * Header file for Leto file translate functions
 *
 * based on idea/ suggests from 2017 Mauricio Ventura Faria
 * 'formatting'                 2017 Rolf 'elch' Beckmann
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

/*
   Include this header for <automatic> translations of locally working functions.
   Out-comment at top groups not wanted, to keep them working at client
 */


#ifndef LETOFILE_CH_
#define LETOFILE_CH_

/* non-modifying queries for size and exist */
   #define __LETO_TRANSLATE_FILE
/* extended set of copy, delete, rename */
   #define __LETO_TRANSLATE_FILE_MORE
/* low-level Fxxx() function set */
//   #define __LETO_TRANSLATE_FILE_LOW
/* memo functions to read, write */
   #define __LETO_TRANSLATE_MEMO
/* directory function set to query, test, make, remove */
   #define __LETO_TRANSLATE_DIR


#if defined( __LETO_TRANSLATE_FILE )
   /* DbExists() / hb_DbExists() uses LetoDBf RDD methods, need no translation */
   #xtranslate FILE( <x> )                   => LETO_FILE( <x> )
   #xtranslate FILESIZE( [<x,...>] )         => LETO_FILESIZE( <x> )
#endif

#if defined( __LETO_TRANSLATE_FILE_MORE ) || defined( __LETO_TRANSLATE_FILE_LOW )
   /* a.) ask client side, here possible set as result of a Leto_File*() function */
   /* #xtranslate FERROR( [<x,...>] )        => LETO_FERROR( <x> ) */

   /* b.) query the server */
   #xtranslate FERROR( [<x,...>] )           => leto_Udf( "FError", <x> )
#endif

#if defined( __LETO_TRANSLATE_FILE_MORE )
   #xtranslate COPY FILE <(src)> TO <(dst)>  => LETO_FCOPY( <(src)>, <(dst)> )
   #xtranslate __COPYFILE( [<x,...>] )       => LETO_FCOPY( <x> )
   #xtranslate DELETE FILE <(f)>             => LETO_FERASE( <f> )
   #xtranslate FERASE( [<x,...>] )           => LETO_FERASE( <x> )
   #xtranslate FRENAME( [<x,...>] )          => LETO_FRENAME( <x> )
#endif

#if defined( __LETO_TRANSLATE_FILE_LOW )
   /* all based on UDF execute at server -- not for xHb */
   #xtranslate FOPEN( [<x,...>] )            => leto_Udf( "Leto_FOpen", <x> )
   #xtranslate FCREATE( [<x,...>] )          => leto_Udf( "Leto_FCreate", <x> )
   #xtranslate FCLOSE(  [<x,...>] )          => leto_Udf( "Leto_FClose", <x> )
   #xtranslate FSEEK( [<x,...>] )            => leto_Udf( "FSeek", <x> )
   #xtranslate FREAD( [<x,...>] )            => leto_Fread( <x> )
   #xtranslate FREADSTR( [<x,...>] )         => leto_Udf( "FReadStr", <x> )
   #xtranslate FWRITE( [<x,...>] )           => leto_Udf( "FWrite", <x> )
#endif

#if defined( __LETO_TRANSLATE_MEMO )
   #xtranslate MEMOREAD( [<x,...>] )         => LETO_MEMOREAD( <x> )
   #xtranslate MEMOWRITE( [<x,...>] )        => LETO_MEMOWRITE( <x> )
#endif

#if defined( __LETO_TRANSLATE_DIR )
   // #xtranslate ADIR( cSpec, aName, aSize, aDate, aTime, aAttr )
   #xtranslate ADIR( <cSpec> )               => LEN( LETO_DIRECTORY( <cSpec> ) )
   #xtranslate ADIR( <cSpec>, <aArr> )       => AEVAL( LETO_DIRECTORY( <cSpec> ),;
                                                { | aDir, nI | <aArr> \[ nI \] := aDir\[ 1 \] } )
   #xtranslate DIRECTORY( [<x,...>] )        => LETO_DIRECTORY( <x> )
   #xtranslate ISDIRECTORY( [<x,...>] )      => LETO_DIREXIST( <x> )
   #xtranslate ISDIR( [<x,...>] )            => LETO_DIREXIST( <x> )
   #xtranslate DIRMAKE( [<x,...>] )          => LETO_DIRMAKE( <x> )
   #xtranslate MAKEDIR( [<x,...>] )          => LETO_DIRMAKE( <x> )
   #xtranslate DIRREMOVE( [<x,...>] )        => LETO_DIRREMOVE( <x> )
#endif


#endif /* LETOFILE_CH_ */
