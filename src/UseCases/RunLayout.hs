{- | Estructura del run: la única fuente de verdad de qué niveles lo componen.

Cada 'RunSlot' empareja el rol narrativo de un nivel con el archivo fijo que le
sirve a la vez de few-shot para el generador y de fallback si la generación IA
falla. Antes esta información vivía partida en dos lugares —los roles en
'UseCases.GenerateLevels' y las rutas en 'Frameworks.Gloss.GameLoop'— que había
que mantener sincronizados a mano: agregar un nivel exigía tocar ambos y, si se
desalineaban, el sistema descartaba niveles en silencio. Con 'runLayout' como
única definición, agregar o quitar un nivel es una sola edición y roles y rutas
no pueden divergir.

Vive en 'UseCases/' (no en 'Frameworks/') porque tanto el bucle Gloss como la
orquestación del bootstrap necesitan leerlo, y la dependencia solo puede apuntar
hacia adentro. Las rutas son datos puros; el 'IO' que las resuelve y lee sigue en
'Adapters/'.
-}
module UseCases.RunLayout (
  RunSlot (..),
  runLayout,
  layoutRoles,
  layoutPaths,
)
where

-- Grupo 1 — proyecto
import Domain.Model.LevelRole (LevelRole (..))

{- | Un nivel del run: su rol y el archivo fijo asociado.

El archivo cumple doble papel a propósito —few-shot del generador y fallback ante
una generación fallida—: es el nivel canónico del slot.
-}
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
