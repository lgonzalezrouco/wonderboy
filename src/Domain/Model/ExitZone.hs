module Domain.Model.ExitZone (
  ExitZone (..),
  defaultExitZone,
  exitZoneAabb,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomLeft)
import Domain.ValueObjects.Position (Position, position)

-- | Región no sólida. Cuando el jugador la alcanza (habiendo cumplido las condiciones de puntaje/boss) completa el nivel.
data ExitZone = ExitZone
  { exitPos :: Position
  , exitWidth :: Float
  , exitHeight :: Float
  }
  deriving (Eq, Show, Generic)

-- | Salida de tamaño cero para mundos y tests que no modelan una salida real.
defaultExitZone :: ExitZone
defaultExitZone = ExitZone (position 0 0) 0 0

exitZoneAabb :: ExitZone -> Aabb
exitZoneAabb exitZone =
  aabbFromBottomLeft (exitPos exitZone) (exitWidth exitZone) (exitHeight exitZone)
