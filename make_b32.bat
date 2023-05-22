@echo off
REM
REM check file "Readme.txt", chapter: 2.0 ff -- Harbour user use hbmk2 by Szakats
REM

rem Adapt search paths for C-Compiler and Harbour:
SET PATH=C:\BCC58\BIN;C:\xharbour\bin
rem the root path to harbour:
SET HB_PATH=C:\xharbour

SET HARBOUR_EXE=
if exist %HB_PATH%\bin\harbour.exe SET HARBOUR_EXE=harbour
if exist %HB_PATH%\bin\xHB.exe SET HARBOUR_EXE=xHb

if "%HARBOUR_EXE%." == "." (
  @echo ! Please verify the paths herein for Harbour!
  @echo .
  goto EXIT
)

SET arg1=%1
if "%arg1%." == "." SET arg1=rdd
)
if "%arg1%" == "clean" goto CLEAN

if not exist lib md lib
if not exist obj md obj
if not exist obj\bcc md obj\bcc
if not exist obj\api md obj\api

if "%arg1%" == "rdd" goto BUILD
if "%arg1%" == "api" goto BUILD
if "%arg1%" == "all" goto BUILD
  @echo .
  @echo rdd    for RDD lib (default)
  @echo api    for C-API lib
  @echo all    for RDD and C-API
  @echo Clean  for clean-up
  @echo .
goto EXIT


:BUILD
make -l OBJ_DIR=obj\bcc API_DIR=obj\api -fmakefile.bc %arg1% %2 %3 > make_bcc.log 2> make_bcc.err
if errorlevel 1 goto BUILD_ERR
if "%arg1%" == "rdd" (
   @echo ... copying RDD and headers into place for %HARBOUR_EXE%
   copy lib\rddleto.lib     %HB_PATH%\lib\rddleto.lib
   copy include\leto_std.ch %HB_PATH%\include
   copy include\rddleto.ch  %HB_PATH%\include
   copy include\letofile.ch %HB_PATH%\include
)
goto BUILD_OK

:BUILD_OK
   @echo Hooray! ...;-)
   goto EXIT

:BUILD_ERR
   type make_b32.log
   goto EXIT

:CLEAN
   del lib\*.lib     2>NUL
   del lib\*.bak     2>NUL
   del obj\bcc\*.obj 2>NUL
   del obj\api\*.obj 2>NUL
   del obj\bcc\*.c   2>NUL
   del make_bcc.log  2>NUL

   goto EXIT

:EXIT

