module InstallerUtils where

import           Control.Monad.Error
import           Data.Char          (isSpace)
import           Data.ConfigFile    (readfile, set, to_string, optionxform, emptyCP, ConfigParser, CPError)
import           Data.Either.Utils  (forceEither)
import qualified Data.List          as L
import           Data.Maybe         (fromMaybe)
import           Data.Monoid        ((<>))
import qualified Data.String.Utils  as SU

defaultCP :: ConfigParser
defaultCP = emptyCP {optionxform = id}

-- Given a string, does the following changes:
--    - Replace the first ": " on it with "="
--    - Remove the ": " if there's no configuration following it
replaceFirstColonWithEqual :: String -> String
replaceFirstColonWithEqual l
  | (dropWhile isSpace $ snd optionForm) == ""  = fst optionForm
  | otherwise                                   = fst optionForm ++ "=" ++ (snd optionForm)
    where unconsRes = fromMaybe ("", []) $ L.uncons $ SU.split ": " l
          optionForm = (fst unconsRes, SU.join ": " $ snd unconsRes)

-- Parses each line of the configuration, changing only the option forms to the supported format
reformatMaybeOptionForm :: String -> String
reformatMaybeOptionForm l
  | L.isInfixOf ": " l  = replaceFirstColonWithEqual l
  | otherwise   = l

-- Parses the configuration so that it has the format supported by Mantis
-- For this, the option forms should have the format:
--     "flag[=configuration]"
-- Instead of the default:  "flag: [configuration]"
reformatConfiguration :: String -> String
reformatConfiguration config = unlines $ map reformatMaybeOptionForm $ lines config

-- Overrides (or creates) a file with the ConfigParser configuration
writeConfigParser ::Either CPError ConfigParser -> String -> IO ()
writeConfigParser maybeCP confFile = writeFile confFile stringConf
    where stringConf = reformatConfiguration $ to_string $ forceEither maybeCP

-- Adds Mantis configuration for HTTPS support
configurationWithHTTPS :: String -> String -> IO (Either CPError ConfigParser)
configurationWithHTTPS confFile daedalusDir = runErrorT $ do
  cpWithoutHTTPS <- join $ liftIO $ readfile defaultCP confFile
  temporalCP1 <- set cpWithoutHTTPS "JVMOptions" "-Dmantis.network.rpc.mode" "https"
  temporalCP2 <- set temporalCP1 "JVMOptions" "-Dmantis.network.rpc.certificate-keystore-path" (daedalusDir <> "certificate-keystore\\mantisKeystore.p12")
  temporalCP3 <- set temporalCP2 "JVMOptions" "-Dmantis.network.rpc.certificate-keystore-type" "PKCS12"
  cpWithHTTPS <- set temporalCP3 "JVMOptions" "-Dmantis.network.rpc.certificate-password-file" (daedalusDir <> "certificate-keystore\\keystore-password.txt")
  return cpWithHTTPS
