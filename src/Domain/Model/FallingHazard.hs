{- | Peligro ambiental que cae verticalmente (instancia de nivel).

Daña al jugador por solapamiento de caja de colisión; puede repetir
tras un retardo opcional en la posición de spawn.
-}
module Domain.Model.FallingHazard (
  FallingHazardPhase (..),
  FallingHazard (..),
  fallingHazardAabb,
  fallingHazardIsActive,
  spawnFallingHazard,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomCenter)
import Domain.ValueObjects.Frames (Frames)
import Domain.ValueObjects.Position (Position)

-- | Fase de simulación de un peligro que cae.
data FallingHazardPhase
  = -- | Cayendo a velocidad constante.
    HazardFalling
  | -- | Esperando en spawn antes del siguiente ciclo.
    HazardWaiting Frames
  | -- | Ciclo único terminado (se elimina del mundo).
    HazardDone
  deriving (Eq, Show, Generic)

-- | Estado de un peligro que cae en un frame.
data FallingHazard = FallingHazard
  { fallingHazardId :: Int
  , fallingHazardSpawnPos :: Position
  , fallingHazardPos :: Position
  , fallingHazardWidth :: Float
  , fallingHazardHeight :: Float
  , fallingHazardFallSpeed :: Float
  , fallingHazardLoopDelay :: Maybe Frames
  , fallingHazardPhase :: FallingHazardPhase
  }
  deriving (Eq, Show, Generic)

-- | Caja de colisión (centro inferior en 'fallingHazardPos').
fallingHazardAabb :: FallingHazard -> Aabb
fallingHazardAabb h =
  aabbFromBottomCenter
    (fallingHazardPos h)
    (fallingHazardWidth h)
    (fallingHazardHeight h)

-- | 'True' mientras el peligro sigue en el mundo (cayendo o en espera).
fallingHazardIsActive :: FallingHazard -> Bool
fallingHazardIsActive h = fallingHazardPhase h /= HazardDone

-- | Crea un peligro activo en la posición de pies de spawn.
spawnFallingHazard ::
  Int ->
  Position ->
  Float ->
  Float ->
  Float ->
  Maybe Frames ->
  FallingHazard
spawnFallingHazard hid spawnPos width height fallSpeed loopDelay =
  FallingHazard
    { fallingHazardId = hid
    , fallingHazardSpawnPos = spawnPos
    , fallingHazardPos = spawnPos
    , fallingHazardWidth = width
    , fallingHazardHeight = height
    , fallingHazardFallSpeed = fallSpeed
    , fallingHazardLoopDelay = loopDelay
    , fallingHazardPhase = HazardFalling
    }
