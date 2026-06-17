{- | Zona de salida del nivel (rectángulo no sólido, ancla bottom-left).

Cargada desde la definición del nivel; la lógica de victoria híbrida (M18)
comprobará superposición con el jugador.
-}
module Domain.Model.ExitZone (
  ExitZone (..),
  defaultExitZone,
)
where

import GHC.Generics (Generic)

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
