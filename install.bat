@echo off

rem From ant.bat in apache-ant:
rem  %~dp0 is expanded pathname of the current script under NT
set PLTDIR=%~dp0
cd %~dp0

rem  look for MrEd.exe
if exist "%PLTDIR%\MrEd.exe" goto MrFound
if exist "%PLTDIR%\MzScheme.exe" goto MzFound
echo Could not find %PLTDIR%\MzScheme.exe or %PLTDIR%\MrEd.exe, abort.
pause
goto done
:MrFound
set MZMR=MrEd.exe
goto binaryFound
:MzFound
set MZMR=MzScheme.exe
goto binaryFound
:binaryFound

rem  look for install
if exist "%PLTDIR%\install" goto installFound
echo %PLTDIR%\install not found, abort.
pause
goto done
:installFound

echo Running %PLTDIR%\%MZMR% %PLT_INSTALL_ARGS%
"%PLTDIR%\%MZMR%" -qC "%PLTDIR%\install" -i

:done
