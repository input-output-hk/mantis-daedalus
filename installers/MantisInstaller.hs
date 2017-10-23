import           Data.Text as T
import           Data.Monoid ((<>))
import           System.Info
import           Turtle
import           Turtle.Line          (unsafeTextToLine)

import qualified MantisWindowsInstaller
import qualified MacInstaller


main :: IO ()
main = do
  echo $ unsafeTextToLine . T.pack $ "Generating installer for " <>  os <> "-" <> arch
  case os of
    "linux" -> echo "No installer yet"
    "darwin" -> echo "No installer yet"
    "mingw32" -> MantisWindowsInstaller.main
    _ -> fail "No installer available for this platform."
