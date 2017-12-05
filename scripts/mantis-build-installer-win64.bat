rem DEPENDENCIES:
rem   1. Node.js ('npm' binary in PATH)
rem   2. 7zip    ('7z'  binary in PATH)
rem   3. git
rem   4. Windows SDK
rem   5. python  ('python' binary in PATH) FIXME: Is it needed with windows-build-tools?
rem   6. Install windows build tools "npm install -g windows-build-tools"
rem
rem   installer dev mode:  set SKIP_TO_FRONTEND/SKIP_TO_INSTALLER

set LIBRESSL_VERSION=2.5.3
set CURL_VERSION=7.54.0
set DAEDALUS_VERSION=local-dev-build-mantis

set CURL_URL=https://bintray.com/artifact/download/vszakats/generic/curl-%CURL_VERSION%-win64-mingw.7z
set CURL_BIN=curl-%CURL_VERSION%-win64-mingw\bin
set NSISVER=3.02.1
set NSIS_URL=https://downloads.sourceforge.net/project/nsis/NSIS%%203/%NSISVER%/nsis-%NSISVER%-setup.exe
set NSIS_PATCH_URL=https://downloads.sourceforge.net/project/nsis/NSIS%%203/%NSISVER%/nsis-%NSISVER%-strlen_8192.zip
set LIBRESSL_URL=https://ftp.openbsd.org/pub/OpenBSD/LibreSSL/libressl-%LIBRESSL_VERSION%-windows.zip

rem Location of the mantis client with the embedded JVM is configurable
set MANTIS_LOCATION=%1
@if [%MANTIS_LOCATION%]==[]   (@echo ERROR: MANTIS_LOCATION [argument #1] was not provided, terminating installation
    exit /b 1);

@echo Building Daedalus version:  %DAEDALUS_VERSION%
@echo ..with LibreSSL version:    %LIBRESSL_VERSION%
@echo .

@if not [%SKIP_TO_INSTALLER%]==[] (@echo WARNING: SKIP_TO_INSTALLER set, skipping to frontend packaging
    pushd installers & goto :build_installer)
@if not [%SKIP_TO_FRONTEND%]==[]   (@echo WARNING: SKIP_TO_FRONTEND set, skipping directly to installer rebuild
    pushd installers & goto :build_frontend)

@echo Obtaining curl
powershell -Command "try { Import-Module BitsTransfer; Start-BitsTransfer -Source '%CURL_URL%' -Destination 'curl.7z'; } catch { exit 1; }"
@if %errorlevel% neq 0 (@echo FAILED: couldn't obtain curl from %CURL_URL% using BITS
	popd & exit /b 1)
del /f curl.exe curl-ca-bundle.crt libcurl.dll
7z e curl.7z %CURL_BIN%\curl.exe %CURL_BIN%\curl-ca-bundle.crt %CURL_BIN%\libcurl.dll
@if %errorlevel% neq 0 (@echo FAILED: couldn't extract curl from downloaded archive
	popd & exit /b 1)

@echo Obtaining NSIS %NSISVER% with 8k-string patch
del /f nsis-setup.exe nsis-strlen_8192.zip
curl -o nsis-setup.exe       --location %NSIS_URL%
@if %errorlevel% neq 0 (@echo FAILED: curl -o nsis-setup.exe       --location %NSIS_URL%
    exit /b 1)

curl -o nsis-strlen_8192.zip --location %NSIS_PATCH_URL%
@if %errorlevel% neq 0 (@echo FAILED: curl -o nsis-strlen_8192.zip --location %NSIS_PATCH_URL%
    exit /b 1)

nsis-setup.exe /S /SD
@if %errorlevel% neq 0 (@echo FAILED: nsis-setup.exe /S /SD
    exit /b 1)

rem Install NSIS if it's not already installed
@if not exist "c:\Program Files (x86)\NSIS" (
  7z    x nsis-strlen_8192.zip -o"c:\Program Files (x86)\NSIS" -aoa -r
  @if %errorlevel% neq 0 (@echo FAILED: 7z    x nsis-strlen_8192.zip -o"c:\Program Files (x86)\NSIS" -aoa -r
      exit /b 1)
)

@echo Installing NPM
call npm install
@if %errorlevel% neq 0 (@echo FAILED: npm install
    exit /b 1)

rem Move mantis client location to installers (FIXME: should be replaced with downloading and uncompressing it)
@echo Moving Mantis from %MANTIS_LOCATION%
rmdir /s/q installers\mantis 2>nul
xcopy   %MANTIS_LOCATION% installers\mantis\ /s

:build_frontend
@echo Packaging frontend
call npm run package -- --icon installers/icons/64x64
@if %errorlevel% neq 0 (@echo FAILED: Failed to package the frontend
	exit /b 1)

pushd installers
    del /f LibreSSL.zip 2>nul
    @echo Obtaining LibreSSL %LIBRESSL_VERSION%
    ..\curl %LIBRESSL_URL% -o LibreSSL.zip
    @if %errorlevel% neq 0 (@echo FAILED: LibreSSL couldn't be obtained
	popd & exit /b 1)
    7z x LibreSSL.zip
    @if %errorlevel% neq 0 (@echo FAILED: LibreSSL couldn't be extracted from downloaded archive
	popd & exit /b 1)
    del LibreSSL.zip
    rmdir /s/q libressl
    move libressl-%LIBRESSL_VERSION%-windows libressl

    @echo Installing stack
    ..\curl --location http://www.stackage.org/stack/windows-x86_64 -o stack.zip
    @if %errorlevel% neq 0 (@echo FAILED: stack couldn't be obtained
	popd & exit /b 1)
    del /f stack.exe 2>nul
    7z x stack.zip stack.exe
    @if %errorlevel% neq 0 (@echo FAILED: couldn't extract stack from the distribution package
	exit /b 1)
    del stack.zip

    @echo Building the installer
    stack setup --no-reinstall > nul
    @if %errorlevel% neq 0 (@echo FAILED: stack setup --no-reinstall
	exit /b 1)

:build_installer
    call ..\scripts\appveyor-retry call stack --no-terminal build -j 2 --exec mantis-make-installer
    @if %errorlevel% equ 0 goto :built

    @echo FATAL: persistent failure while building installer with:  call stack --no-terminal build -j 2 --exec mantis-make-installer
    exit /b 1
:built
@echo SUCCESS: call stack --no-terminal build -j 2 --exec mantis-make-installer
popd

@dir /b/s installers\daedalus*
