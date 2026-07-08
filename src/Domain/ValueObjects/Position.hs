-- | Coordenada 2D (x, y) en el espacio del juego, en píxeles lógicos.
module Domain.ValueObjects.Position (
  Position (..),
  position,
  posX,
  posY,
  translate,
)
where

import GHC.Generics (Generic)

-- | Par de coordenadas (x, y) en píxeles lógicos.
newtype Position = Position (Float, Float)
  deriving (Eq, Show, Generic)

{- | Construye una 'Position' desde sus componentes. Sin invariantes que validar
(cualquier Float es válido), por eso el constructor 'Position' también se exporta.
-}
position :: Float -> Float -> Position
position x y = Position (x, y)

posX :: Position -> Float
posX (Position (x, _)) = x

posY :: Position -> Float
posY (Position (_, y)) = y

{- | Desplaza una 'Position' sumando @(dx, dy)@: forma única de mover una posición
por un offset, reutilizada por cinemática, plataformas móviles y colisión.
-}
translate :: Float -> Float -> Position -> Position
translate dx dy (Position (x, y)) = position (x + dx) (y + dy)
