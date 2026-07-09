module UseCases.LoadLevel (
  decodeLevelDefinition,
  worldFromDefinition,
  worldFromCatalog,
  loadLevelFromText,
)
where

import Data.Maybe (listToMaybe)
import Data.Text (Text, unpack)
import Domain.Logic.BuildWorld (buildWorld)
import Domain.Model.LevelDefinition (LevelBuildError (..), LevelDefinition)
import Domain.Model.World (World)
import UseCases.GameMonad (GameError (..))
import UseCases.Serialization.LevelCodec (decodeLevelText)

decodeLevelDefinition :: Text -> Either GameError LevelDefinition
decodeLevelDefinition txt =
  case decodeLevelText txt of
    Left err -> Left (GameError err)
    Right lvl -> Right lvl

worldFromDefinition :: LevelDefinition -> Either GameError World
worldFromDefinition = either (Left . levelBuildToGameError) Right . buildWorld

loadLevelFromText :: Text -> Either GameError World
loadLevelFromText txt = decodeLevelDefinition txt >>= worldFromDefinition

{- | Construye el mundo para el índice (base 0) del catálogo. Un índice fuera de rango produce un 'GameError'.
Ojo con el desfase de base: GameState.gsLevelIndex arranca en 1.
-}
worldFromCatalog :: [LevelDefinition] -> Int -> Either GameError World
worldFromCatalog defs idx
  | idx < 0 = Left invalidIndex
  | otherwise = maybe (Left invalidIndex) worldFromDefinition (listToMaybe (drop idx defs))
 where
  invalidIndex = GameError ("invalid level index: " ++ show idx)

levelBuildToGameError :: LevelBuildError -> GameError
levelBuildToGameError (LevelBuildError msg) =
  GameError ("level build error: " ++ unpack msg)
