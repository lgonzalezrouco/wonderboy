-- | Orquestación de carga de niveles: decode JSON y 'buildWorld' (sin IO).
module UseCases.LoadLevel (
  decodeLevelDefinition,
  loadLevelFromText,
)
where

import Data.Aeson (eitherDecodeStrict)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Domain.Logic.BuildWorld (buildWorld)
import Domain.Model.LevelDefinition (LevelBuildError (..), LevelDefinition)
import Domain.Model.World (World)
import UseCases.GameMonad (GameError (..))

-- | Decodifica JSON estricto a 'LevelDefinition'.
decodeLevelDefinition :: Text -> Either GameError LevelDefinition
decodeLevelDefinition txt =
  case eitherDecodeStrict (encodeUtf8 txt) of
    Left err -> Left (GameError ("invalid level JSON: " ++ err))
    Right lvl -> Right lvl

-- | Carga un nivel desde texto JSON.
loadLevelFromText :: Text -> Either GameError World
loadLevelFromText txt =
  decodeLevelDefinition txt >>= either (Left . levelBuildToGameError) Right . buildWorld

levelBuildToGameError :: LevelBuildError -> GameError
levelBuildToGameError (LevelBuildError msg) =
  GameError ("level build error: " ++ unpack msg)
