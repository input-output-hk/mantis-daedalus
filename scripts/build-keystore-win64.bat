@rem This script creates a p12 keystore from server.key and server.crt files, containing also the ca.crt certificate
@rem Usage:
@rem      build-keystore-win64.bat LIBRESSL_EXEC KEYTOOL_EXEC TLS_FOLDER TARGET_FOLDER

@rem Obtain parameters
set LIBRESSL_EXEC=%1
set MANTIS_EXEC=%2
set TLS_FOLDER=%3
set TARGET_FOLDER=%4

@rem Default values
set KEYSTORE_FILE=mantisKeystore.p12
set PASSWORD_FILE=keystore-password.txt
set KEYSTORE_PASSWORD=1234

@echo Create https
if not exist %TARGET_FOLDER% mkdir %TARGET_FOLDER%
pushd %TARGET_FOLDER%
  @echo Password file creation
	echo %KEYSTORE_PASSWORD% > keystore-password.txt

  @echo Create keystore from certificates
	start %LIBRESSL_EXEC% pkcs12 -export -name mantisCA -in %TLS_FOLDER%\server.crt -inkey %TLS_FOLDER%\server.key -out %KEYSTORE_FILE% -nodes -password file:%PASSWORD_FILE%
	start %MANTIS_EXEC% keytool -import -trustcacerts -v -alias ca -keystore %KEYSTORE_FILE% -keyalg RSA -keysize 4096 -keypass:env %KEYSTORE_PASSWORD% -storepass:env %KEYSTORE_PASSWORD%-ext KeyUsage:critical="keyCertSign" -ext BasicConstraints:critical="ca:true" -validity 9999 -file %TLS_FOLDER%\ca.crt -storetype pkcs12
popd
