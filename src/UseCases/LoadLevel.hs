-- | Orquestación de carga de niveles: decode JSON y 'buildWorld' (sin IO).
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

{- | Decodifica JSON a 'LevelDefinition' vía el codec DTO de @UseCases.Serialization.LevelCodec@.

La serialización JSON vive en @UseCases.Serialization@, no en @Domain/@.
-}
decodeLevelDefinition :: Text -> Either GameError LevelDefinition
decodeLevelDefinition txt =
  case decodeLevelText txt of
    Left err -> Left (GameError err)
    Right lvl -> Right lvl

{- | Construye el 'World' a partir de una 'LevelDefinition' ya decodificada.

Se extrae como paso propio para que el flujo de carga pueda insertar la
resolución de comportamiento (@UseCases.ResolveBehaviours@) /entre/ el decode y
el build: la resolución vive en una mónada con posible 'IO' (en @Adapters/@) y
no puede componerse dentro del 'Either' puro de 'decodeLevelDefinition'. Mapea el
'LevelBuildError' del dominio al 'GameError' del motor.
-}
worldFromDefinition :: LevelDefinition -> Either GameError World
worldFromDefinition = either (Left . levelBuildToGameError) Right . buildWorld

{- | Carga un nivel desde texto JSON: decode seguido del build puro.

Conserva el comportamiento previo a la extracción de 'worldFromDefinition'; los
flujos que necesiten resolver comportamiento componen los pasos por separado.
-}
loadLevelFromText :: Text -> Either GameError World
loadLevelFromText txt = decodeLevelDefinition txt >>= worldFromDefinition

{- | Construye el 'World' del nivel @idx@ dentro de un catálogo pre-cargado.

Índice fuera de rango → 'GameError'; el llamador con 'IO' decide cómo abortar.
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
