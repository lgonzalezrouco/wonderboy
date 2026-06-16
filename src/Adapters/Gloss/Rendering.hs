-- | Adaptador de renderizado: 'GameView' → primitivas Gloss con cámara y HUD.
module Adapters.Gloss.Rendering (
  renderFrame,
)
where

import Graphics.Gloss.Data.Color (Color)
import Graphics.Gloss.Data.Picture (Picture (..), pictures, rectangleSolid, text)

import Adapters.Gloss.Config (
  cameraY,
  enemyColor,
  hudAttackColor,
  hudHealthColor,
  hudHealthEmptyColor,
  hudLifeColor,
  hudMaxHealth,
  hudMutedColor,
  hudOverlayDim,
  hudPanelBg,
  hudStartingLives,
  hudTextColor,
  platformColor,
  playerColor,
  windowHeight,
  windowWidth,
 )
import Domain.Model.Enemy (enemyAabb)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.GameView (GameView (..))
import Domain.Model.Platform (platformAabb)
import Domain.Model.Player (Player, playerAabb, playerAttackFrames, playerHealth, playerPos)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (Aabb (..))
import Domain.ValueObjects.Position (posX)

hudMargin :: Float
hudMargin = 14

hudPanelWidth :: Float
hudPanelWidth = 248

hudPanelHeight :: Float
hudPanelHeight = 108

hudLabelScale :: Float
hudLabelScale = 0.2

hudHintScale :: Float
hudHintScale = 0.16

-- | Dibuja el mundo con cámara horizontal y HUD fijo en pantalla.
renderFrame :: GameView -> Picture
renderFrame gv =
  pictures
    [ renderWorldLayer (gvWorld gv)
    , renderHud gv
    , renderGameOverOverlay gv
    ]

-- | Capa del mundo con transformación de cámara.
renderWorldLayer :: World -> Picture
renderWorldLayer w =
  let playerX = posX (playerPos (worldPlayer w))
   in Translate (-playerX) (-cameraY) $
        pictures
          [ pictures (map (aabbToPicture platformColor . platformAabb) (worldPlatforms w))
          , pictures (map (aabbToPicture enemyColor . enemyAabb) (worldEnemies w))
          , aabbToPicture playerColor (playerAabb (worldPlayer w))
          ]

-- | Panel superior izquierdo: vidas, salud, ataque y hint de controles.
renderHud :: GameView -> Picture
renderHud gv =
  let halfW = fromIntegral windowWidth / 2
      halfH = fromIntegral windowHeight / 2
      topLeftX = -halfW + hudMargin
      topLeftY = halfH - hudMargin
      panelCenterX = topLeftX + hudPanelWidth / 2
      panelCenterY = topLeftY - hudPanelHeight / 2
      contentX = topLeftX + 12
      row1Y = topLeftY - 20
      row2Y = row1Y - 28
      row3Y = row2Y - 28
      row4Y = row3Y - 22
      p = worldPlayer (gvWorld gv)
   in pictures
        [ Translate panelCenterX panelCenterY $
            Color hudPanelBg (rectangleSolid hudPanelWidth hudPanelHeight)
        , hudLabel contentX row1Y "LIVES"
        , Translate (contentX + 62) (row1Y - 6) $
            renderLifeIcons (gvLives gv) hudStartingLives
        , hudLabel contentX row2Y "HEALTH"
        , Translate (contentX + 72) (row2Y - 5) $
            renderHealthPips (playerHealth p) hudMaxHealth
        , renderAttackRow contentX row3Y p
        , hudHint contentX row4Y "Space — attack"
        ]

renderGameOverOverlay :: GameView -> Picture
renderGameOverOverlay gv
  | gvPhase gv /= GameOver = blank
  | otherwise =
      pictures
        [ Color hudOverlayDim $
            rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight)
        , Translate 0 24 $
            Scale 0.42 0.42 $
              Color hudAttackColor (text "GAME OVER")
        , Translate 0 (-18) $
            Scale hudHintScale hudHintScale $
              Color hudMutedColor (text "Press Esc to quit")
        ]

renderAttackRow :: Float -> Float -> Player -> Picture
renderAttackRow x y p
  | playerAttackFrames p <= 0 = blank
  | otherwise =
      pictures
        [ Translate (x + 34) (y - 6) $
            Color hudAttackColor (rectangleSolid 52 14)
        , Translate x y $
            Scale hudLabelScale hudLabelScale $
              Color hudTextColor (text "ATTACK")
        ]

renderLifeIcons :: Int -> Int -> Picture
renderLifeIcons lives maxLives =
  pictures
    [ lifeIconAt (fromIntegral i * 20) (i < lives) | i <- [0 .. maxLives - 1]
    ]
 where
  lifeIconAt dx filled =
    Translate dx 0 $
      Color (if filled then hudLifeColor else hudHealthEmptyColor) $
        rectangleSolid 14 14

renderHealthPips :: Int -> Int -> Picture
renderHealthPips current maxHealth =
  pictures
    [ healthPipAt (fromIntegral i * 26) (i < current) | i <- [0 .. maxHealth - 1]
    ]
 where
  healthPipAt dx filled =
    Translate dx 0 $
      Color (if filled then hudHealthColor else hudHealthEmptyColor) $
        rectangleSolid 22 10

hudLabel :: Float -> Float -> String -> Picture
hudLabel x y label =
  Translate x y $
    Scale hudLabelScale hudLabelScale $
      Color hudTextColor (text label)

hudHint :: Float -> Float -> String -> Picture
hudHint x y label =
  Translate x y $
    Scale hudHintScale hudHintScale $
      Color hudMutedColor (text label)

blank :: Picture
blank = pictures []

-- | Convierte un 'Aabb' en un rectángulo sólido centrado en su caja.
aabbToPicture :: Color -> Aabb -> Picture
aabbToPicture color box =
  let w = aabbMaxX box - aabbMinX box
      h = aabbMaxY box - aabbMinY box
      cx = (aabbMinX box + aabbMaxX box) / 2
      cy = (aabbMinY box + aabbMaxY box) / 2
   in Translate cx cy (Color color (rectangleSolid w h))
