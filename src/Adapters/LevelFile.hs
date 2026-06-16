-- | Lectura de archivos de nivel desde disco (IO).
module Adapters.LevelFile (
  readLevelFile,
)
where

import Control.Exception (IOException, try)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.IO.Error (ioeGetErrorString)

import Data.ByteString qualified as BS

-- | Lee un archivo de nivel como 'Text' UTF-8.
readLevelFile :: FilePath -> IO (Either String Text)
readLevelFile path = do
  result <- try @IOException (BS.readFile path)
  case result of
    Left err -> pure (Left ("failed to read level file: " ++ ioeGetErrorString err))
    Right bs -> pure (Right (decodeUtf8 bs))
