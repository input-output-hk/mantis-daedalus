#!/bin/bash -x
#
# This is an install-time script that creates a p12 keystore from server.key and server.crt files,
# containing also the ca.crt certificate
# Usage:
#      ./build-keystore-unix.sh LIBRESSL_EXEC MANTS_EXEC TLS_FOLDER TARGET_FOLDER

# Obtain parameters
LIBRESSL_EXEC="$1"
MANTIS_EXEC="$2"
TLS_FOLDER="$3"
TARGET_FOLDER="$4"

# Default values
KEYSTORE_FILE="mantisKeystore.p12"
PASSWORD_FILE="keystore-password.txt"
KEYSTORE_PASSWORD="12345678"

echo Create folder for HTTPS files
if [ ! -d ${TARGET_FOLDER} ]; then
  mkdir ${TARGET_FOLDER}
fi

pushd ${TARGET_FOLDER}
  echo Password file creation
	echo ${KEYSTORE_PASSWORD} > ${PASSWORD_FILE}

  echo Create keystore from certificates
	${LIBRESSL_EXEC} pkcs12 -export -name mantisCA -in ${TLS_FOLDER}/server/server.crt -inkey ${TLS_FOLDER}/server/server.key -out ${KEYSTORE_FILE} -nodes -password file:${PASSWORD_FILE}
	${MANTIS_EXEC} keytool -import -trustcacerts -v -alias ca-cert -keystore ${KEYSTORE_FILE} -keyalg RSA -keysize 4096 -keypass ${KEYSTORE_PASSWORD} -storepass ${KEYSTORE_PASSWORD} -ext KeyUsage:critical="keyCertSign" -ext BasicConstraints:critical="ca:true" -validity 9999 -file ${TLS_FOLDER}/ca/ca.crt -storetype pkcs12 -noprompt
popd

