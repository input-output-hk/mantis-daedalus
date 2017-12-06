module MantisWindowsInstaller where

import qualified Data.List          as L
import           Data.Maybe         (fromJust, fromMaybe)
import           Data.Monoid        ((<>))
import           Development.NSIS
import           System.Environment (lookupEnv)
import           Turtle             (ExitCode (..), echo, proc, procs)

import           WindowsInstaller

mantisLauncherScript :: [String]
mantisLauncherScript =
  [ "@echo off"
  , "SET DAEDALUS_DIR=%~dp0"
  , "start /D \"%DAEDALUS_DIR%mantis\" mantis.exe" --Start the Mantis client
  , "start /D \"%DAEDALUS_DIR%\" Daedalus.exe " --Start the Daedalus wallet (FIXME: temporarily disabled as the Mantis client can't properly connect with it yet)
  ]
  where httpsArgs = "-J-Dmantis.network.rpc.mode=https" <>
                    "-J-Dmantis.network.rpc.certificate-keystore-path=\"%DAEDALUS_DIR%certificate-keystore\\mantisKeystore.p12\"" <>
                    "-J-Dmantis.network.rpc.certificate-keystore-type=\"PKCS12\"" <>
                    "-J-Dmantis.network.rpc.certificate-password-file=\"%DAEDALUS_DIR%certificate-keystore\\keystore-password.txt\""

mantisWriteInstallerNSIS :: String -> IO ()
mantisWriteInstallerNSIS fullVersion = do
  tempDir <- fmap fromJust $ lookupEnv "TEMP"
  writeFile "daedalus.nsi" $ nsis $ do
    _ <- constantStr "Version" (str fullVersion)
    name "Daedalus ($Version)"                  -- The name of the installer
    outFile "daedalus-win64-$Version-installer.exe"           -- Where to produce the installer
    unsafeInjectGlobal $ "!define MUI_ICON \"icons\\64x64.ico\""
    unsafeInjectGlobal $ "!define MUI_HEADERIMAGE"
    unsafeInjectGlobal $ "!define MUI_HEADERIMAGE_BITMAP \"icons\\installBanner.bmp\""
    unsafeInjectGlobal $ "!define MUI_HEADERIMAGE_RIGHT"
    unsafeInjectGlobal $ "VIProductVersion " <> (L.intercalate "." $ parseVersion fullVersion)
    unsafeInjectGlobal $ "VIAddVersionKey \"ProductVersion\" " <> fullVersion
    unsafeInjectGlobal "Unicode true"
    requestExecutionLevel Highest
    unsafeInjectGlobal "!addplugindir \"nsis_plugins\\liteFirewall\\bin\""

    installDir "$PROGRAMFILES64\\Daedalus"                   -- Default installation directory...
    installDirRegKey HKLM "Software/Daedalus" "Install_Dir"  -- ...except when already installed.

    page Directory                   -- Pick where to install
    constant "INSTALLEDAT" $ readRegStr HKLM "Software/Daedalus" "Install_Dir"
    onPagePre Directory (iff_ (strLength "$INSTALLEDAT" %/= 0) $ abort "")

    page InstFiles                   -- Give a progress bar while installing

    _ <- section "" [Required] $ do
        setOutPath "$INSTDIR"        -- Where to install files in this section
        writeRegStr HKLM "Software/Daedalus" "Install_Dir" "$INSTDIR" -- Used by launcher batch script
        createDirectory "$APPDATA\\Daedalus\\Secrets-0.6"
        --FIXME: Make Mantis logs location configurable so as to have them be in this Logs folder
        createDirectory "$APPDATA\\Daedalus\\Logs"
        createDirectory "$APPDATA\\Daedalus\\Logs\\pub"
        createShortcut "$DESKTOP\\Daedalus.lnk" daedalusShortcut
        file [] "version.txt"
        file [] "build-certificates-win64.bat"
        file [] "build-keystore-win64.bat"
        file [] "ca.conf"
        file [] "server.conf"
        file [] "client.conf"
        file [] "wallet-topology.yaml"
        file [] "configuration.yaml"
        file [] "*genesis*.json"
        writeFileLines "$INSTDIR\\daedalus.bat" (map str mantisLauncherScript)
        file [Recursive] "libressl\\"
        file [Recursive] "..\\release\\w\\"
        setOutPath "$INSTDIR\\mantis\\"
        file [Recursive] "mantis\\"
        setOutPath "$INSTDIR"

        mapM_ unsafeInject
          [ "liteFirewall::AddRule \"$INSTDIR\\mantis\\mantis.exe\" \"Mantis Node\""
          , "Pop $0"
          , "DetailPrint \"liteFirewall::AddRule: $0\""
          ]

        execWait "build-certificates-win64.bat \"$INSTDIR\" >\"%APPDATA%\\Daedalus\\Logs\\build-certificates.log\" 2>&1"

        createDirectory "$INSTDIR\\certificate-keystore"
        execWait "build-keystore-win64.bat \"$INSTDIR\\x64\\openssl\" \"$INSTDIR\\mantis\\mantis.exe\" \"$INSTDIR\\tls\" \"$INSTDIR\\certificate-keystore\" >\"%APPDATA%\\Daedalus\\Logs\\build-keystore.log\" 2>&1"

        -- Uninstaller
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "InstallLocation" "$INSTDIR\\Daedalus"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "Publisher" "Eureka Solutions LLC"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "ProductVersion" (str fullVersion)
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "VersionMajor" (str . (!! 0). parseVersion $ fullVersion)
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "VersionMinor" (str . (!! 1). parseVersion $ fullVersion)
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "DisplayName" "Daedalus"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "DisplayVersion" (str fullVersion)
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "UninstallString" "\"$INSTDIR/uninstall.exe\""
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "QuietUninstallString" "\"$INSTDIR/uninstall.exe\" /S"
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "NoModify" 1
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "NoRepair" 1
        file [] $ (str $ tempDir <> "\\uninstall.exe")

    _ <- section "Start Menu Shortcuts" [] $ do
        createDirectory "$SMPROGRAMS/Daedalus"
        createShortcut "$SMPROGRAMS/Daedalus/Uninstall Daedalus.lnk"
          [Target "$INSTDIR/uninstall.exe", IconFile "$INSTDIR/uninstall.exe", IconIndex 0]
        createShortcut "$SMPROGRAMS/Daedalus/Daedalus.lnk" daedalusShortcut
    return ()

main :: IO ()
main = do
  echo "Writing version.txt"
  version <- fmap (fromMaybe "dev") $ lookupEnv "APPVEYOR_BUILD_VERSION"
  let fullVersion = version <> ".0"
  writeFile "version.txt" fullVersion

  signFile "mantis\\mantis.exe"

  echo "Writing uninstaller.nsi"
  writeUninstallerNSIS fullVersion
  signUninstaller

  --FIXME: mt.exe location was changed to match the one in a Windows 10 machine, this should be generalized to be dependant on the machine ran
  echo "Adding permissions manifest to mantis.exe"
  procs "C:\\Program Files (x86)\\Windows Kits\\10\\bin\\10.0.15063.0\\x64\\mt.exe" ["-manifest", "mantis.exe.manifest", "-outputresource:mantis\\mantis.exe;#1"] mempty

  echo "Writing daedalus.nsi"
  mantisWriteInstallerNSIS fullVersion

  echo "Generating NSIS installer daedalus-win64-installer.exe"
  procs "C:\\Program Files (x86)\\NSIS\\makensis" ["daedalus.nsi"] mempty
  signFile ("daedalus-win64-" <> fullVersion <> "-installer.exe")
