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
  platformColor,
  playerColor,
  windowHeight,
  windowWidth,
 )
import Domain.Model.Enemy (enemyAabb)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.GameView (GameView (..))
import Domain.Model.Platform (platformAabb)
import Domain.Model.Player (playerAabb, playerHealth, playerPos)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (Aabb (..))
import Domain.ValueObjects.Position (posX)

-- | Dibuja el mundo con cámara horizontal y HUD fijo en pantalla.
renderFrame :: GameView -> Picture
renderFrame gv =
  pictures
    [ renderWorldLayer (gvWorld gv)
    , renderHud gv
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

-- | HUD fijo en pantalla (no afectado por la cámara).
renderHud :: GameView -> Picture
renderHud gv =
  let w = gvWorld gv
      margin = 20
      hudX = fromIntegral windowWidth / (-2) + margin
      hudY = fromIntegral windowHeight / 2 - margin
      label =
        "Lives: "
          ++ show (gvLives gv)
          ++ "  Health: "
          ++ show (playerHealth (worldPlayer w))
          ++ case gvPhase gv of
            GameOver -> "\nGAME OVER"
            Playing -> ""
   in Translate hudX hudY $
        Scale 0.25 0.25 $
          text label

-- | Convierte un 'Aabb' en un rectángulo sólido centrado en su caja.
aabbToPicture :: Color -> Aabb -> Picture
aabbToPicture color box =
  let w = aabbMaxX box - aabbMinX box
      h = aabbMaxY box - aabbMinY box
      cx = (aabbMinX box + aabbMaxX box) / 2
      cy = (aabbMinY box + aabbMaxY box) / 2
   in Translate cx cy (Color color (rectangleSolid w h))
