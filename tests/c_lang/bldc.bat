@echo off
REM need to adapt <HRB_PATH> env-var or set it directly, example: D:\harbour\bin
set HRB_DIR=%HB_PATH%

set LETO_DIR=..\..\
set HARBOUR_LIBS=gtwin.lib hbdebug.lib hbvmmt.lib hbrtl.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbcpage.lib hbpcre.lib hbzlib.lib rddnsx.lib hbmemio.lib hbextern.lib hbhsx.lib cw32mt.lib PSDK\iphlpapi.lib

bcc32 -O2 -d -I%HRB_DIR%\include;%LETO_DIR%\include -L%LETO_DIR%\lib;%HB_PATH%\lib\win\bcc %1.c ws2_32.lib leto.lib %HARBOUR_LIBS%

del %1.obj
del %1.tds
