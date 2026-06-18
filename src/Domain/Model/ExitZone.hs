{- | Zona de salida del nivel (rectángulo no sólido, ancla bottom-left).

Cargada desde la definición del nivel; 'Domain.Logic.LevelFlow' comprueba
superposición con el jugador para la victoria híbrida.
-}
module Domain.Model.ExitZone (
  ExitZone (..),
  defaultExitZone,
  exitZoneAabb,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomLeft)
import Domain.ValueObjects.Position (Position, position)

-- | Región de salida: esquina inferior izquierda y tamaño.
data ExitZone = ExitZone
  { exitPos :: Position
  , exitWidth :: Float
  , exitHeight :: Float
  }
  deriving (Eq, Show, Generic)

-- | Zona degenerada para tests que no modelan salida.
defaultExitZone :: ExitZone
defaultExitZone = ExitZone (position 0 0) 0 0

-- | Caja de colisión de la zona de salida (ancla bottom-left).
exitZoneAabb :: ExitZone -> Aabb
exitZoneAabb exitZone =
  aabbFromBottomLeft (exitPos exitZone) (exitWidth exitZone) (exitHeight exitZone)
