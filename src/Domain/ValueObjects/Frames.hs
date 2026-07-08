{- | Conteo de frames de simulación (ventanas y esperas).

Value object con invariante: nunca negativo. Unidad común de las ventanas de
ataque e invencibilidad, las esperas del DSL y los tramos de patrulla — antes
todas eran 'Int' sueltos, confundibles con salud, vidas o puntos.
-}
module Domain.ValueObjects.Frames (
  Frames,
  frames,
  frameCount,
  noFrames,
  tickFrames,
  hasFramesLeft,
)
where

import GHC.Generics (Generic)

-- | Número de frames restantes (>= 0).
newtype Frames = Frames Int
  deriving (Eq, Ord, Show, Generic)

-- | Construye 'Frames', saturando en 0.
frames :: Int -> Frames
frames n = Frames (max 0 n)

frameCount :: Frames -> Int
frameCount (Frames n) = n

-- | Cero frames (sin ventana activa).
noFrames :: Frames
noFrames = Frames 0

-- | Descuenta un frame, saturando en 0.
tickFrames :: Frames -> Frames
tickFrames (Frames n) = Frames (max 0 (n - 1))

hasFramesLeft :: Frames -> Bool
hasFramesLeft (Frames n) = n > 0
