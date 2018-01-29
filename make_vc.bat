REM SET PATH=F:\MsVc8\VC\bin;F:\MsVc8\SDK\v2.0\bin;f:\HbVc8\bin
REM SET PATH=F:\MsVc9\VC\bin;F:\MsVc9\SDK\v3.5\bin;f:\HbVc9\bin

REM SET HB_PATH=F:\HbVc8
REM SET HB_PATH=F:\HbVc9

@echo off
if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if not exist lib md lib
if not exist obj md obj
if not exist obj\vc md obj\vc
if not exist obj\api md obj\api

REM SET VSINSTALLDIR=F:\MsVc8
REM SET VCINSTALLDIR=F:\MsVc8\VC
REM SET PATH=F:\MsVc8\VC\BIN;F:\MsVc8\Common7\IDE;F:\MsVc8\Common7\Tools;F:\MsVc8\SDK\v2.0\bin;%PATH%
REM SET INCLUDE=F:\MsVc8\VC\include;F:\MsVc8\SDK\v2.0\include
REM SET LIB=F:\MsVc8\VC\lib;F:\MsVc8\SDK\v2.0\lib
REM SET LIBPATH=F:\MsVc8\VC\lib;F:\MsVc8\SDK\v2.0\lib

REM SET VSINSTALLDIR=F:\MsVc9
REM SET VCINSTALLDIR=F:\MsVc9\VC
REM SET PATH=F:\MsVc9\VC\BIN;F:\MsVc9\Common7\IDE;F:\MsVc9\Common7\Tools;F:\MsVc9\SDK\v3.5\bin;%PATH%
REM SET INCLUDE=F:\MsVc9\VC\include;F:\MsVc9\SDK\v3.5\include
REM SET LIB=F:\MsVc9\VC\lib;F:\MsVc9\SDK\v3.5\lib
REM SET LIBPATH=F:\MsVc9\VC\lib;F:\MsVc9\SDK\v3.5\lib

:BUILD
nmake /I /Fmakefile.vc %1 %2 %3 > make_vc.log
if errorlevel 1 goto BUILD_ERR
rem copy lib\rddleto.lib %HB_PATH%\lib\rddleto.lib
goto BUILD_OK

:BUILD_OK
   goto EXIT

:BUILD_ERR
   notepad make_vc.log
   goto EXIT

:CLEAN
   del lib\*.lib
   del obj\vc\*.obj
   del obj\vc\*.c
   del obj\api\*.obj
   del make_vc.log

   goto EXIT

:EXIT

