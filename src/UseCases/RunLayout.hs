{- | Estructura del run: única fuente de verdad de qué niveles lo componen.

Cada 'RunSlot' empareja el rol narrativo de un nivel con su archivo fijo, que
sirve a la vez de few-shot para el generador y de fallback si la generación IA
falla. Una sola definición evita que roles y rutas diverjan.

Vive en 'UseCases/' (no en 'Frameworks/') porque tanto el bucle Gloss como el
bootstrap la leen y la dependencia solo puede apuntar hacia adentro. Las rutas
son datos puros; el 'IO' que las resuelve y lee sigue en 'Adapters/'.
-}
module UseCases.RunLayout (
  RunSlot (..),
  runLayout,
  layoutRoles,
  layoutPaths,
)
where

import Domain.Model.LevelRole (LevelRole (..))

-- | Un nivel del run: su rol y el archivo fijo asociado.
data RunSlot = RunSlot
  { slotRole :: LevelRole
  -- ^ Rol narrativo y de dificultad del slot.
  , slotFile :: FilePath
  -- ^ Ruta relativa del nivel fijo (few-shot + fallback).
  }
  deriving (Eq, Show)

-- | El run estándar: introducción, desafío y jefe, en ese orden.
runLayout :: [RunSlot]
runLayout =
  [ RunSlot IntroRole "levels/level1.json"
  , RunSlot ChallengeRole "levels/level2.json"
  , RunSlot BossRole "levels/level3.json"
  ]

-- | Roles del run, en orden. Deriva de 'runLayout'.
layoutRoles :: [LevelRole]
layoutRoles = map slotRole runLayout

-- | Rutas de los niveles fijos, en orden. Deriva de 'runLayout'.
layoutPaths :: [FilePath]
layoutPaths = map slotFile runLayout
