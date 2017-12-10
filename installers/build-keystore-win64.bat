@rem This is an install-time script that creates a p12 keystore from server.key and server.crt files,
@rem containing also the ca.crt certificate
@rem Usage:
@rem      build-keystore-win64.bat LIBRESSL_EXEC MANTIS_EXEC TLS_FOLDER TARGET_FOLDER

@rem Obtain parameters
set LIBRESSL_EXEC=%1
set MANTIS_EXEC=%2
set TLS_FOLDER=%3
set TARGET_FOLDER=%4

@rem Default values
set KEYSTORE_FILE=mantisKeystore.p12
set PASSWORD_FILE=keystore-password.txt
set KEYSTORE_PASSWORD=12345678

@echo Create folder for HTTPS files
if not exist %TARGET_FOLDER% mkdir %TARGET_FOLDER%
pushd %TARGET_FOLDER%
  @echo Password file creation
	echo %KEYSTORE_PASSWORD% > PASSWORD_FILE

  @echo Create keystore from certificates
	%LIBRESSL_EXEC% pkcs12 -export -name mantisCA -in %TLS_FOLDER%\server\server.crt -inkey %TLS_FOLDER%\server\server.key -out %KEYSTORE_FILE% -nodes -password file:%PASSWORD_FILE%
	%MANTIS_EXEC% keytool -import -trustcacerts -v -alias ca-cert -keystore %KEYSTORE_FILE% -keyalg RSA -keysize 4096 -keypass %KEYSTORE_PASSWORD% -storepass %KEYSTORE_PASSWORD% -ext KeyUsage:critical="keyCertSign" -ext BasicConstraints:critical="ca:true" -validity 9999 -file %TLS_FOLDER%\ca\ca.crt -storetype pkcs12 -noprompt
popd
