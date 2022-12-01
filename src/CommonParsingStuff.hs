module CommonParsingStuff where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import qualified Text.Megaparsec as MP

type Parser a = MP.Parsec Void T.Text a

withParser :: String -> Parser a -> (a -> IO ()) -> IO ()
withParser filename parser action = do
    d <- T.readFile filename
    case MP.parse parser filename d of 
        Left e -> print e
        Right res -> action res