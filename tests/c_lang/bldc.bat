@echo off
REM ## batch for Borland BCC

REM ## adapt paths to C-compiler and Harbour bin directory
rem SET PATH=C:\BCC55\BIN;F:\hbbcc\bin
rem SET HB_PATH=F:\hbbcc

set HB_DIR=%HB_PATH%
set HB_INC=%HB_DIR%\include
set HB_LIB=%HB_DIR%\lib\win\bcc

REM ## no adapt below needed

set LETO_DIR=..\..
set LETO_INC=%LETO_DIR%\include
set LETO_LIB=%LETO_DIR%\lib

set HARBOUR_LIBS=gtwin.lib hbvm.lib hbrtl.lib hbcommon.lib hbrdd.lib hbmacro.lib rddntx.lib rddcdx.lib rddnsx.lib rddfpt.lib hbsix.lib hbhsx.lib hbzlib.lib
set BORLAND_LIBS=ws2_32.lib PSDK\iphlpapi.lib

REM ## static build
bcc32 -O2 -d -I%HB_INC%;%LETO_INC% -L%LETO_LIB%;%HB_LIB% %1.c leto.lib %HARBOUR_LIBS% %BORLAND_LIBS%

REM ## dynmic build
rem bcc32 -O2 -d -I%HB_INC%;%LETO_INC% -L%LETO_LIB%;%HB_LIB% %1.c leto.lib harbour-32-bcc.lib hbzlib.lib %BORLAND_LIBS%

del %1.obj
del %1.tds
