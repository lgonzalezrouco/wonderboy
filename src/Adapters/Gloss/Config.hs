module Adapters.Gloss.Config (
  windowWidth,
  windowHeight,
  renderZoom,
  backgroundColor,
  cameraY,
  maxDeltaSeconds,
  playerColor,
  enemyColorForKind,
  pickupColor,
  platformColor,
  movingPlatformColor,
  hudPanelBg,
  hudTextColor,
  hudMutedColor,
  hudLifeColor,
  hudHealthColor,
  hudHealthEmptyColor,
  hudAttackColor,
  hudBossColor,
  hudBossEmptyColor,
  hudOverlayDim,
  projectileColor,
  fallingHazardColor,
  crumblingPlatformColor,
)
where

import Graphics.Gloss.Data.Color (Color, makeColor)

import Domain.Model.EnemyKind (EnemyKind (..))

windowWidth :: Int
windowWidth = 1024

windowHeight :: Int
windowHeight = 768

-- Zoom solo de render (mundo + fondo). La simulación/física no se ve afectada.
renderZoom :: Float
renderZoom = 1.5

backgroundColor :: Color
backgroundColor = makeColor 0.15 0.15 0.2 1.0

cameraY :: Float
cameraY = 120

maxDeltaSeconds :: Float
maxDeltaSeconds = 0.05

playerColor :: Color
playerColor = makeColor 0.2 0.5 1.0 1.0

enemyColorForKind :: EnemyKind -> Color
enemyColorForKind kind = case kind of
  SnailKind -> makeColor 0.85 0.75 0.2 1.0
  BatKind -> makeColor 0.65 0.35 0.9 1.0
  GolemKind -> makeColor 0.55 0.58 0.62 1.0
  ArcherKind -> makeColor 0.9 0.45 0.2 1.0
  BossGolemKind -> makeColor 0.75 0.15 0.25 1.0
  BossBatKind -> makeColor 0.55 0.1 0.45 1.0

pickupColor :: Color
pickupColor = makeColor 1.0 0.85 0.15 1.0

platformColor :: Color
platformColor = makeColor 0.3 0.7 0.3 1.0

movingPlatformColor :: Color
movingPlatformColor = makeColor 0.45 0.85 0.55 1.0

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

hudBossColor :: Color
hudBossColor = makeColor 0.9 0.25 0.35 1.0

hudBossEmptyColor :: Color
hudBossEmptyColor = makeColor 0.28 0.12 0.16 1.0

hudAttackColor :: Color
hudAttackColor = makeColor 1.0 0.5 0.15 1.0

hudOverlayDim :: Color
hudOverlayDim = makeColor 0.02 0.03 0.06 0.55

projectileColor :: Color
projectileColor = makeColor 0.95 0.85 0.2 1.0

fallingHazardColor :: Color
fallingHazardColor = makeColor 0.95 0.35 0.15 1.0

crumblingPlatformColor :: Color
crumblingPlatformColor = makeColor 0.75 0.55 0.25 1.0
