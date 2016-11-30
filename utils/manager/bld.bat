@echo off

set HRB_DIR=%HB_PATH%
set HWGUI_INSTALL=c:\myapps\hwgui
REM set HRB_DIR=C:\CVS-Developers\xharbour
REM set HWGUI_INSTALL=C:\CVS-Developers\xharbour\contrib\hwgui

%HRB_DIR%\bin\harbour manage.prg -n -i%HRB_DIR%\include;%HWGUI_INSTALL%\include;..\..\include

bcc32  -c -O2 -tW -M -I%HRB_DIR%\include;%HWGUI_INSTALL%\include manage.c
echo 1 24 "%HWGUI_INSTALL%\samples\image\WindowsXP.Manifest" > hwgui_xp.rc
brc32 -r hwgui_xp -fohwgui_xp

echo c0w32.obj + > b32.bc
echo manage.obj, + >> b32.bc
echo manage.exe, + >> b32.bc
echo manage.map, + >> b32.bc

echo %HWGUI_INSTALL%\lib\hwgui.lib + >> b32.bc
echo %HWGUI_INSTALL%\lib\procmisc.lib + >> b32.bc
echo %HWGUI_INSTALL%\lib\hbxml.lib + >> b32.bc

if exist %HRB_DIR%\lib\rtl%HB_MT%.lib echo %HRB_DIR%\lib\rtl%HB_MT%.lib + >> b32.bc
if exist %HRB_DIR%\lib\hbrtl%HB_MT%.lib echo %HRB_DIR%\lib\hbrtl%HB_MT%.lib + >> b32.bc
if exist %HRB_DIR%\lib\vm%HB_MT%.lib echo %HRB_DIR%\lib\vm%HB_MT%.lib + >> b32.bc
if exist %HRB_DIR%\lib\hbvm.lib echo %HRB_DIR%\lib\hbvm.lib + >> b32.bc
if exist %HRB_DIR%\lib\gtgui.lib echo %HRB_DIR%\lib\gtgui.lib + >> b32.bc
if not exist %HRB_DIR%\lib\gtgui.lib echo %HRB_DIR%\lib\gtwin.lib + >> b32.bc
if exist %HRB_DIR%\lib\lang.lib echo %HRB_DIR%\lib\lang.lib + >> b32.bc
if exist %HRB_DIR%\lib\hblang.lib echo %HRB_DIR%\lib\hblang.lib + >> b32.bc
if exist %HRB_DIR%\lib\codepage.lib echo %HRB_DIR%\lib\codepage.lib + >> b32.bc
if exist %HRB_DIR%\lib\hbcpage.lib echo %HRB_DIR%\lib\hbcpage.lib + >> b32.bc
if exist %HRB_DIR%\lib\macro%HB_MT%.lib echo %HRB_DIR%\lib\macro%HB_MT% + >> b32.bc
if exist %HRB_DIR%\lib\hbmacro.lib echo %HRB_DIR%\lib\hbmacro.lib + >> b32.bc
if exist %HRB_DIR%\lib\common.lib echo %HRB_DIR%\lib\common + >> b32.bc
if exist %HRB_DIR%\lib\hbcommon.lib echo %HRB_DIR%\lib\hbcommon.lib + >> b32.bc
if exist %HRB_DIR%\lib\debug.lib echo %HRB_DIR%\lib\debug.lib + >> b32.bc
if exist %HRB_DIR%\lib\hbdebug.lib echo %HRB_DIR%\lib\hbdebug.lib + >> b32.bc
if exist %HRB_DIR%\lib\pp.lib echo %HRB_DIR%\lib\pp.lib + >> b32.bc
if exist %HRB_DIR%\lib\hbpp.lib echo %HRB_DIR%\lib\hbpp.lib + >> b32.bc
if exist %HRB_DIR%\lib\pcrepos.lib echo %HRB_DIR%\lib\pcrepos.lib + >> b32.bc
if exist %HRB_DIR%\lib\dbfcdx.lib echo %HRB_DIR%\lib\dbfcdx.lib + >> b32.bc
if exist %HRB_DIR%\lib\dbfntx.lib echo %HRB_DIR%\lib\dbfntx.lib + >> b32.bc
if exist %HRB_DIR%\lib\dbffpt.lib echo %HRB_DIR%\lib\dbffpt.lib + >> b32.bc
if exist %HRB_DIR%\lib\rddntx.lib echo %HRB_DIR%\lib\rddntx.lib + >> b32.bc
if exist %HRB_DIR%\lib\rddfpt.lib echo %HRB_DIR%\lib\rddfpt.lib + >> b32.bc
if exist %HRB_DIR%\lib\hbsix.lib echo %HRB_DIR%\lib\hbsix.lib + >> b32.bc

echo %HRB_DIR%\lib\rddleto.lib + >> b32.bc
if exist %HRB_DIR%\lib\rdd%HB_MT%.lib echo %HRB_DIR%\lib\rdd%HB_MT% + >> b32.bc
if exist %HRB_DIR%\lib\hbrdd.lib echo %HRB_DIR%\lib\hbrdd.lib + >> b32.bc

echo cw32.lib + >> b32.bc
echo import32.lib, >> b32.bc
echo hwgui_xp.res >> b32.bc
ilink32 -Gn -Tpe -aa @b32.bc

del *.tds
del manage.c
del manage.map
del manage.obj
del b32.bc
