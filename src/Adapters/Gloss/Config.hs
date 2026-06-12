-- | Constantes de ventana, cámara y colores para el adaptador Gloss (M8).
module Adapters.Gloss.Config (
  windowWidth,
  windowHeight,
  backgroundColor,
  cameraY,
  maxDeltaSeconds,
  playerColor,
  enemyColor,
  platformColor,
)
where

import Graphics.Gloss.Data.Color (Color, makeColor)

-- | Ancho de la ventana en píxeles.
windowWidth :: Int
windowWidth = 800

-- | Alto de la ventana en píxeles.
windowHeight :: Int
windowHeight = 600

-- | Color de fondo de la ventana.
backgroundColor :: Color
backgroundColor = makeColor 0.15 0.15 0.2 1.0

-- | Posición Y fija de la cámara (píxeles lógicos).
cameraY :: Float
cameraY = 120

-- | Tope superior del delta time en segundos.
maxDeltaSeconds :: Float
maxDeltaSeconds = 0.05

-- | Color del rectángulo del jugador.
playerColor :: Color
playerColor = makeColor 0.2 0.5 1.0 1.0

-- | Color del rectángulo de enemigos.
enemyColor :: Color
enemyColor = makeColor 1.0 0.3 0.3 1.0

-- | Color del rectángulo de plataformas.
platformColor :: Color
platformColor = makeColor 0.3 0.7 0.3 1.0
