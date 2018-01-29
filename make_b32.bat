rem SET PATH=C:\BCC55\BIN;F:\hbcc\bin
rem SET HB_PATH=F:\hbcc

@echo off
if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if not exist lib md lib
if not exist obj md obj
if not exist obj\b32 md obj\b32
if not exist obj\api md obj\api

:BUILD
make -l EXE_OBJ_DIR=obj\b32\bin OBJ_DIR=obj\b32 API_DIR=obj\api -fmakefile.bc %1 %2 %3 > make_b32.log
if errorlevel 1 goto BUILD_ERR
copy lib\rddleto.lib %HB_PATH%\lib\rddleto.lib
goto BUILD_OK

:BUILD_OK
   goto EXIT

:BUILD_ERR
   notepad make_b32.log
   goto EXIT

:CLEAN
   del lib\*.lib
   del lib\*.bak
   del obj\b32\*.obj
   del obj\api\*.obj
   del obj\b32\*.c
   del make_b32.log

   goto EXIT

:EXIT

