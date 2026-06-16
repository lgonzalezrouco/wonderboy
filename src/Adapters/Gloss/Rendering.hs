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
  hudMutedColor,
  hudOverlayDim,
  hudPanelBg,
  hudTextColor,
  movingPlatformColor,
  pickupColor,
  platformColor,
  playerColor,
  windowHeight,
  windowWidth,
 )
import Domain.Model.Enemy (enemyAabb)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.GameView (GameView (..))
import Domain.Model.MovingPlatform (movingPlatformAabb)
import Domain.Model.Pickup (pickupAabb)
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
hudPanelHeight = 168

hudLabelScale :: Float
hudLabelScale = 0.2

hudHintScale :: Float
hudHintScale = 0.16

-- Geometría interna del panel (px de pantalla).

-- | Margen interno del panel para el contenido.
hudContentInset :: Float
hudContentInset = 12

-- | Desplazamiento de la primera fila bajo el borde superior del panel.
hudRow1Offset :: Float
hudRow1Offset = 20

-- | Separación vertical entre filas (LIVES, HEALTH, SCORE, ATTACK).
hudRowGap :: Float
hudRowGap = 36

-- | Separación vertical de la fila de hint respecto de la fila anterior.
hudHintGap :: Float
hudHintGap = 20

-- | Altura aproximada del texto del HUD a 'hudLabelScale' (ancla inferior izquierda).
hudTextHeight :: Float
hudTextHeight = 18

-- | Ancho fijo de la columna de rótulos; los valores empiezan después.
hudLabelColumnWidth :: Float
hudLabelColumnWidth = 82

-- | Centro vertical de iconos/pips respecto de la línea base del rótulo.
hudValueCenterLift :: Float
hudValueCenterLift = hudTextHeight / 2

-- | Paso horizontal entre iconos de vida consecutivos.
hudLifeIconStride :: Float
hudLifeIconStride = 20

-- | Lado del cuadrado de un icono de vida.
hudLifeIconSize :: Float
hudLifeIconSize = 14

-- | Paso horizontal entre pips de salud consecutivos.
hudHealthPipStride :: Float
hudHealthPipStride = 26

-- | Ancho de un pip de salud.
hudHealthPipWidth :: Float
hudHealthPipWidth = 22

-- | Alto de un pip de salud.
hudHealthPipHeight :: Float
hudHealthPipHeight = 10

-- | Desplazamiento horizontal del recuadro "ATTACK" respecto del contenido.
hudAttackBoxOffsetX :: Float
hudAttackBoxOffsetX = 34

-- | Ajuste vertical del recuadro "ATTACK".
hudAttackBoxDrop :: Float
hudAttackBoxDrop = 6

-- | Ancho del recuadro indicador de ataque.
hudAttackBoxWidth :: Float
hudAttackBoxWidth = 52

-- | Alto del recuadro indicador de ataque.
hudAttackBoxHeight :: Float
hudAttackBoxHeight = 14

-- | Escala del texto "GAME OVER" en el overlay.
hudGameOverScale :: Float
hudGameOverScale = 0.42

-- | Desplazamiento vertical del texto "GAME OVER".
hudGameOverOffsetY :: Float
hudGameOverOffsetY = 24

-- | Desplazamiento vertical del hint bajo "GAME OVER".
hudGameOverHintOffsetY :: Float
hudGameOverHintOffsetY = -18

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
          , pictures (map (aabbToPicture movingPlatformColor . movingPlatformAabb) (worldMovingPlatforms w))
          , pictures (map (aabbToPicture enemyColor . enemyAabb) (worldEnemies w))
          , pictures (map (aabbToPicture pickupColor . pickupAabb) (worldPickups w))
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
      contentX = topLeftX + hudContentInset
      row1Y = topLeftY - hudRow1Offset
      row2Y = row1Y - hudRowGap
      row3Y = row2Y - hudRowGap
      row4Y = row3Y - hudRowGap
      row5Y = row4Y - hudHintGap
      valueX = contentX + hudLabelColumnWidth
      p = worldPlayer (gvWorld gv)
   in pictures
        [ Translate panelCenterX panelCenterY $
            Color hudPanelBg (rectangleSolid hudPanelWidth hudPanelHeight)
        , hudLabel contentX row1Y "LIVES"
        , Translate valueX (row1Y + hudValueCenterLift) $
            renderLifeIcons (gvLives gv) (gvStartingLives gv)
        , hudLabel contentX row2Y "HEALTH"
        , Translate valueX (row2Y + hudValueCenterLift) $
            renderHealthPips (playerHealth p) (gvMaxHealth gv)
        , hudLabel contentX row3Y "SCORE"
        , hudLabel valueX row3Y (show (gvScore gv))
        , renderAttackRow contentX row4Y p
        , hudHint contentX row5Y "Space - attack"
        ]

renderGameOverOverlay :: GameView -> Picture
renderGameOverOverlay gv
  | gvPhase gv /= GameOver = Blank
  | otherwise =
      pictures
        [ Color hudOverlayDim $
            rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight)
        , Translate 0 hudGameOverOffsetY $
            Scale hudGameOverScale hudGameOverScale $
              Color hudAttackColor (text "GAME OVER")
        , Translate 0 hudGameOverHintOffsetY $
            Scale hudHintScale hudHintScale $
              Color hudMutedColor (text "Press Esc to quit")
        ]

renderAttackRow :: Float -> Float -> Player -> Picture
renderAttackRow x y p
  | playerAttackFrames p <= 0 = Blank
  | otherwise =
      let valueX = x + hudLabelColumnWidth
       in pictures
            [ Translate (valueX + hudAttackBoxOffsetX) (y + hudValueCenterLift - hudAttackBoxDrop) $
                Color hudAttackColor (rectangleSolid hudAttackBoxWidth hudAttackBoxHeight)
            , hudLabel x y "ATTACK"
            ]

renderLifeIcons :: Int -> Int -> Picture
renderLifeIcons lives maxLives =
  pictures
    [ lifeIconAt (fromIntegral i * hudLifeIconStride) (i < lives) | i <- [0 .. maxLives - 1]
    ]
 where
  lifeIconAt dx filled =
    Translate dx 0 $
      Color (if filled then hudLifeColor else hudHealthEmptyColor) $
        rectangleSolid hudLifeIconSize hudLifeIconSize

renderHealthPips :: Int -> Int -> Picture
renderHealthPips current maxHealth =
  pictures
    [ healthPipAt (fromIntegral i * hudHealthPipStride) (i < current) | i <- [0 .. maxHealth - 1]
    ]
 where
  healthPipAt dx filled =
    Translate dx 0 $
      Color (if filled then hudHealthColor else hudHealthEmptyColor) $
        rectangleSolid hudHealthPipWidth hudHealthPipHeight

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

-- | Convierte un 'Aabb' en un rectángulo sólido centrado en su caja.
aabbToPicture :: Color -> Aabb -> Picture
aabbToPicture color box =
  let w = aabbMaxX box - aabbMinX box
      h = aabbMaxY box - aabbMinY box
      cx = (aabbMinX box + aabbMaxX box) / 2
      cy = (aabbMinY box + aabbMaxY box) / 2
   in Translate cx cy (Color color (rectangleSolid w h))
