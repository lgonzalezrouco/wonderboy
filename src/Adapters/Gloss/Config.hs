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
  hudMaxHealth,
  hudStartingLives,
  hudPanelBg,
  hudTextColor,
  hudMutedColor,
  hudLifeColor,
  hudHealthColor,
  hudHealthEmptyColor,
  hudAttackColor,
  hudOverlayDim,
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

-- | Valores HUD alineados con 'UseCases.GameMonad.defaultConfig' (M10).
hudMaxHealth :: Int
hudMaxHealth = 3

hudStartingLives :: Int
hudStartingLives = 3

-- | Paleta del HUD (adaptador; no es dominio).
hudPanelBg :: Color
hudPanelBg = makeColor 0.05 0.06 0.1 0.72

hudTextColor :: Color
hudTextColor = makeColor 0.94 0.96 1.0 1.0

hudMutedColor :: Color
hudMutedColor = makeColor 0.62 0.68 0.78 1.0

hudLifeColor :: Color
hudLifeColor = makeColor 1.0 0.82 0.2 1.0

hudHealthColor :: Color
hudHealthColor = makeColor 0.35 0.9 0.5 1.0

hudHealthEmptyColor :: Color
hudHealthEmptyColor = makeColor 0.22 0.25 0.32 1.0

hudAttackColor :: Color
hudAttackColor = makeColor 1.0 0.5 0.15 1.0

hudOverlayDim :: Color
hudOverlayDim = makeColor 0.02 0.03 0.06 0.55
