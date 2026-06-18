-- | Adaptador de renderizado: 'GameView' → primitivas Gloss con cámara y HUD.
module Adapters.Gloss.Rendering (
  renderFrame,
)
where

import Control.Applicative ((<|>))
import Data.Maybe (isNothing)

import Graphics.Gloss.Data.Color (Color, makeColor)
import Graphics.Gloss.Data.Picture (Picture (..), circleSolid, pictures, rectangleSolid, text)

import Adapters.Gloss.Config (
  cameraY,
  enemyColorForKind,
  hudAttackColor,
  hudBossColor,
  hudBossEmptyColor,
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
  renderZoom,
  windowHeight,
  windowWidth,
 )
import Adapters.Gloss.Sprites (
  Sprite (..),
  SpriteCatalog (..),
  enemySprite,
  playerSprite,
 )
import Domain.Logic.Combat (meleeHitbox)
import Domain.Model.Enemy (Enemy, enemyAabb, enemyFacing, enemyKind, enemyPos)
import Domain.Model.ExitZone (ExitZone (..))
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.GameView (GameView (..))
import Domain.Model.MovingPlatform (MovingPlatform, movingPlatformAabb)
import Domain.Model.Pickup (Pickup, pickupAabb, pickupPos)
import Domain.Model.Platform (Platform, platformAabb)
import Domain.Model.Player (
  Player,
  playerAabb,
  playerAttackFrames,
  playerFacing,
  playerHealth,
  playerInvincibilityFrames,
  playerPos,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (Aabb (..), aabbFromBottomLeft)
import Domain.ValueObjects.BossHealth (BossHealth (..))
import Domain.ValueObjects.CombatParams (CombatParams)
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Frames (hasFramesLeft)
import Domain.ValueObjects.Health (healthPoints)
import Domain.ValueObjects.Lives (livesCount)
import Domain.ValueObjects.Position (Position, posX, posY)
import Domain.ValueObjects.Score (scorePoints)

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

-- | Barra de salud del jefe (centro superior).
bossBarWidth :: Float
bossBarWidth = 220

bossBarHeight :: Float
bossBarHeight = 14

bossBarTopOffset :: Float
bossBarTopOffset = 28

bossBarLabelOffsetY :: Float
bossBarLabelOffsetY = 22

entitySpritePadding :: Float
entitySpritePadding = 0.0

platformVisualHeight :: Float
platformVisualHeight = 35

attackCueHeight :: Float
attackCueHeight = 34

attackCueGap :: Float
attackCueGap = 7

attackCueLift :: Float
attackCueLift = 24

damageFlashColor :: Color
damageFlashColor = makeColor 1.0 0.15 0.1 0.38

hitboxOutlineThickness :: Float
hitboxOutlineThickness = 2.0

hitboxFootRadius :: Float
hitboxFootRadius = 3.0

-- | Dibuja el mundo con cámara horizontal y HUD fijo en pantalla.
renderFrame :: SpriteCatalog -> Int -> Bool -> GameView -> Picture
renderFrame catalog renderTick showHitboxes gv =
  pictures
    [ Scale renderZoom renderZoom $
        pictures
          [ renderBackground catalog
          , renderWorldLayer catalog renderTick showHitboxes (gvCombatParams gv) (gvWorld gv)
          ]
    , renderHud catalog gv showHitboxes
    , renderBossBar gv
    , renderGameOverOverlay gv
    ]

renderBackground :: SpriteCatalog -> Picture
renderBackground catalog =
  case scBackgroundGrasslands catalog of
    Nothing -> Blank
    Just sprite -> drawSpriteCover backgroundWidth backgroundHeight sprite

backgroundWidth :: Float
backgroundWidth = fromIntegral windowWidth

backgroundHeight :: Float
backgroundHeight = fromIntegral windowHeight

-- | Capa del mundo con transformación de cámara.
renderWorldLayer :: SpriteCatalog -> Int -> Bool -> CombatParams -> World -> Picture
renderWorldLayer catalog renderTick showHitboxes combatParams w =
  let playerX = posX (playerPos (worldPlayer w))
   in Translate (-playerX) (-cameraY) $
        pictures
          [ pictures (map (renderPlatform catalog) (worldPlatforms w))
          , pictures (map (renderMovingPlatform catalog) (worldMovingPlatforms w))
          , pictures (map (renderEnemy catalog renderTick) (worldEnemies w))
          , pictures (map (renderPickup catalog) (worldPickups w))
          , renderExitZone catalog (worldExit w)
          , renderPlayer catalog renderTick (worldPlayer w)
          , if showHitboxes then renderHitboxOverlay combatParams w else Blank
          ]

renderPlayer :: SpriteCatalog -> Int -> Player -> Picture
renderPlayer catalog renderTick p =
  pictures
    [ case playerSprite catalog renderTick p of
        Nothing -> aabbToPicture playerColor box
        Just sprite -> drawEntitySprite (playerFacing p) box sprite
    , renderPlayerDamageFlash p box
    , renderPlayerAttackCue catalog p box
    ]
 where
  box = playerAabb p

renderEnemy :: SpriteCatalog -> Int -> Enemy -> Picture
renderEnemy catalog renderTick e =
  case enemySprite catalog renderTick e of
    Nothing -> aabbToPicture (enemyColorForKind (enemyKind e)) box
    Just sprite -> drawEntitySprite (enemyFacing e) box sprite
 where
  box = enemyAabb e

renderPickup :: SpriteCatalog -> Pickup -> Picture
renderPickup catalog pickup =
  case scPickupGem catalog of
    Nothing -> aabbToPicture pickupColor box
    Just sprite -> drawSpriteInAabb box sprite
 where
  box = pickupAabb pickup

renderPlatform :: SpriteCatalog -> Platform -> Picture
renderPlatform catalog platform =
  case platformSprites catalog box of
    (leftSprite, Just midSprite, rightSprite) ->
      tileStrip leftSprite midSprite rightSprite box
    _ -> aabbToPicture platformColor (platformAabb platform)
 where
  box = platformAabb platform

platformSprites :: SpriteCatalog -> Aabb -> (Maybe Sprite, Maybe Sprite, Maybe Sprite)
platformSprites catalog box
  | aabbMinY box <= 0 =
      (scTileGrassLeft catalog, scTileGrassMid catalog, scTileGrassRight catalog)
  | otherwise =
      (scTileMovingLeft catalog, scTileMovingMid catalog, scTileMovingRight catalog)

renderMovingPlatform :: SpriteCatalog -> MovingPlatform -> Picture
renderMovingPlatform catalog platform =
  case scTileBridge catalog <|> scTileMovingMid catalog of
    Nothing -> aabbToPicture movingPlatformColor box
    Just sprite -> tileStrip Nothing sprite Nothing box
 where
  box = movingPlatformAabb platform

renderExitZone :: SpriteCatalog -> ExitZone -> Picture
renderExitZone catalog exitZone =
  case scExitSign catalog of
    Nothing -> aabbToPicture hudMutedColor box
    Just sprite -> drawSpriteBottomCenter box sprite
 where
  box = exitAabb exitZone

-- | Contornos de todas las cajas de colisión del mundo (debug de alineación).
renderHitboxOverlay :: CombatParams -> World -> Picture
renderHitboxOverlay combatParams w =
  let p = worldPlayer w
      playerBox = playerAabb p
      meleeOverlay =
        if hasFramesLeft (playerAttackFrames p)
          then
            [ aabbOutline
                hudAttackColor
                hitboxOutlineThickness
                (meleeHitbox combatParams playerBox (playerFacing p))
            ]
          else []
   in pictures
        ( [ aabbOutline platformColor hitboxOutlineThickness (platformAabb plat)
          | plat <- worldPlatforms w
          ]
            <> [ aabbOutline movingPlatformColor hitboxOutlineThickness (movingPlatformAabb mp)
               | mp <- worldMovingPlatforms w
               ]
            <> [ aabbOutline (enemyColorForKind (enemyKind e)) hitboxOutlineThickness (enemyAabb e)
               | e <- worldEnemies w
               ]
            <> [ aabbOutline pickupColor hitboxOutlineThickness (pickupAabb pickup)
               | pickup <- worldPickups w
               ]
            <> [ aabbOutline hudMutedColor hitboxOutlineThickness (exitAabb (worldExit w))
               , aabbOutline playerColor hitboxOutlineThickness playerBox
               , renderFootAnchor (playerPos p) playerColor
               ]
            <> meleeOverlay
            <> [ renderFootAnchor (enemyPos e) (enemyColorForKind (enemyKind e))
               | e <- worldEnemies w
               ]
            <> [ renderFootAnchor (pickupPos pickup) pickupColor
               | pickup <- worldPickups w
               ]
        )

-- | Panel superior izquierdo: vidas, salud, ataque y hint de controles.
renderHud :: SpriteCatalog -> GameView -> Bool -> Picture
renderHud catalog gv showHitboxes =
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
            renderLifeIcons catalog (livesCount (gvLives gv)) (livesCount (gvStartingLives gv))
        , hudLabel contentX row2Y "HEALTH"
        , Translate valueX (row2Y + hudValueCenterLift) $
            renderHealthPips catalog (healthPoints (playerHealth p)) (healthPoints (gvMaxHealth gv))
        , hudLabel contentX row3Y "SCORE"
        , Translate valueX (row3Y + hudValueCenterLift) $
            renderScore catalog (scorePoints (gvScore gv))
        , renderAttackRow catalog contentX row4Y p
        , hudHint contentX row5Y "Space - attack"
        , if showHitboxes then hudHint contentX (row5Y - 16) "F1 - hitboxes" else Blank
        ]

-- | Barra superior centrada con la salud del jefe.
renderBossBar :: GameView -> Picture
renderBossBar gv =
  case gvBossHealth gv of
    Nothing -> Blank
    Just bh ->
      let halfH = fromIntegral windowHeight / 2
          barY = halfH - bossBarTopOffset
          maxPoints = healthPoints (bossHealthMax bh)
          curPoints = healthPoints (bossHealthCurrent bh)
          fillRatio =
            if maxPoints <= 0
              then 0
              else fromIntegral curPoints / fromIntegral maxPoints
          fillW = bossBarWidth * fillRatio
       in pictures
            [ Translate 0 barY $
                Color hudBossEmptyColor (rectangleSolid bossBarWidth bossBarHeight)
            , Translate (-(bossBarWidth / 2) + fillW / 2) barY $
                Color hudBossColor (rectangleSolid fillW bossBarHeight)
            , Translate 0 (barY + bossBarLabelOffsetY) $
                Scale hudLabelScale hudLabelScale $
                  Color hudTextColor (text "BOSS")
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

renderAttackRow :: SpriteCatalog -> Float -> Float -> Player -> Picture
renderAttackRow catalog x y p
  | not (hasFramesLeft (playerAttackFrames p)) = Blank
  | otherwise =
      let valueX = x + hudLabelColumnWidth
       in pictures
            [ Translate (valueX + hudAttackBoxOffsetX) (y + hudValueCenterLift - hudAttackBoxDrop) $
                renderAttackIcon catalog
            , hudLabel x y "ATTACK"
            ]

renderLifeIcons :: SpriteCatalog -> Int -> Int -> Picture
renderLifeIcons catalog lives maxLives =
  case (scHudLife catalog, scHudLifeX catalog) of
    (Just lifeSprite, Just xSprite) ->
      pictures
        [ drawSpriteAtHeight hudLifeIconSize lifeSprite
        , Translate 21 0 (drawSpriteAtHeight 12 xSprite)
        , Translate 36 (-7) $
            Scale hudHintScale hudHintScale $
              Color hudTextColor (text (show lives))
        ]
    _ -> fallbackLifeIcons
 where
  fallbackLifeIcons =
    pictures
      [ lifeIconAt (fromIntegral i * hudLifeIconStride) (i < lives) | i <- [0 .. maxLives - 1]
      ]
  lifeIconAt dx filled =
    Translate dx 0 $
      Color (if filled then hudLifeColor else hudHealthEmptyColor) $
        rectangleSolid hudLifeIconSize hudLifeIconSize

renderHealthPips :: SpriteCatalog -> Int -> Int -> Picture
renderHealthPips catalog current maxHealth =
  pictures
    [ healthPipAt (fromIntegral i * hudHealthPipStride) (i < current) | i <- [0 .. maxHealth - 1]
    ]
 where
  healthPipAt dx filled =
    Translate dx 0 $
      case (filled, scHudHeartFull catalog, scHudHeartEmpty catalog) of
        (True, Just sprite, _) -> drawSpriteAtHeight 18 sprite
        (False, _, Just sprite) -> drawSpriteAtHeight 18 sprite
        _ ->
          Color (if filled then hudHealthColor else hudHealthEmptyColor) $
            rectangleSolid hudHealthPipWidth hudHealthPipHeight

renderScore :: SpriteCatalog -> Int -> Picture
renderScore catalog score =
  pictures
    [ maybe Blank (drawSpriteAtHeight 16) (scHudScoreGem catalog)
    , Translate 24 (-7) $
        Scale hudHintScale hudHintScale $
          Color hudTextColor (text (show score))
    ]

renderAttackIcon :: SpriteCatalog -> Picture
renderAttackIcon catalog =
  case scHudAttackSword catalog of
    Nothing -> Color hudAttackColor (rectangleSolid hudAttackBoxWidth hudAttackBoxHeight)
    Just sprite -> drawSpriteAtHeight 20 sprite

renderPlayerDamageFlash :: Player -> Aabb -> Picture
renderPlayerDamageFlash p box
  | not (hasFramesLeft (playerInvincibilityFrames p)) = Blank
  | otherwise =
      Translate cx cy $
        Color damageFlashColor $
          rectangleSolid (aabbMaxX box - aabbMinX box) (aabbMaxY box - aabbMinY box)
 where
  cx = (aabbMinX box + aabbMaxX box) / 2
  cy = (aabbMinY box + aabbMaxY box) / 2

renderPlayerAttackCue :: SpriteCatalog -> Player -> Aabb -> Picture
renderPlayerAttackCue catalog p box
  | not (hasFramesLeft (playerAttackFrames p)) = Blank
  | otherwise =
      Translate cueX cueY $
        case scHudAttackSword catalog of
          Nothing -> Color hudAttackColor (rectangleSolid attackCueHeight 12)
          Just sprite -> Scale faceScale 1 (drawSpriteAtHeight attackCueHeight sprite)
 where
  facing = playerFacing p
  faceScale =
    case facing of
      FacingLeft -> -1
      FacingRight -> 1
  cueX =
    case facing of
      FacingLeft -> aabbMinX box - attackCueGap
      FacingRight -> aabbMaxX box + attackCueGap
  cueY = aabbMinY box + attackCueLift

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

-- | Contorno de una caja de colisión (solo borde, sin rellenar el interior).
aabbOutline :: Color -> Float -> Aabb -> Picture
aabbOutline color thickness box =
  let minX = aabbMinX box
      maxX = aabbMaxX box
      minY = aabbMinY box
      maxY = aabbMaxY box
      w = maxX - minX
      h = maxY - minY
      cx = (minX + maxX) / 2
      cy = (minY + maxY) / 2
      t = thickness
   in pictures
        [ edge cx (minY + t / 2) w t
        , edge cx (maxY - t / 2) w t
        , edge (minX + t / 2) cy t h
        , edge (maxX - t / 2) cy t h
        ]
 where
  edge x y ew eh =
    Translate x y (Color color (rectangleSolid ew eh))

-- | Punto en el ancla de pies (centro inferior) de una entidad.
renderFootAnchor :: Position -> Color -> Picture
renderFootAnchor pos color =
  Translate (posX pos) (posY pos) (Color color (circleSolid hitboxFootRadius))

drawEntitySprite :: Facing -> Aabb -> Sprite -> Picture
drawEntitySprite facing box sprite =
  let availableW = max 1 (aabbMaxX box - aabbMinX box - entitySpritePadding)
      availableH = max 1 (aabbMaxY box - aabbMinY box - entitySpritePadding)
      spriteScale = min (availableW / spriteWidth sprite) (availableH / spriteHeight sprite)
      renderedH = spriteHeight sprite * spriteScale
      cx = (aabbMinX box + aabbMaxX box) / 2
      cy = aabbMinY box + renderedH / 2
      faceScale = case facing of
        FacingLeft -> -spriteScale
        FacingRight -> spriteScale
   in Translate cx cy $
        Scale faceScale spriteScale (spritePicture sprite)

drawSpriteInAabb :: Aabb -> Sprite -> Picture
drawSpriteInAabb box sprite =
  let boxW = max 1 (aabbMaxX box - aabbMinX box)
      boxH = max 1 (aabbMaxY box - aabbMinY box)
      spriteScale = min (boxW / spriteWidth sprite) (boxH / spriteHeight sprite)
      cx = (aabbMinX box + aabbMaxX box) / 2
      cy = (aabbMinY box + aabbMaxY box) / 2
   in Translate cx cy $
        Scale spriteScale spriteScale (spritePicture sprite)

drawSpriteBottomCenter :: Aabb -> Sprite -> Picture
drawSpriteBottomCenter box sprite =
  let boxW = max 1 (aabbMaxX box - aabbMinX box)
      boxH = max 1 (aabbMaxY box - aabbMinY box)
      spriteScale = min (boxW / spriteWidth sprite) (boxH / spriteHeight sprite)
      renderedH = spriteHeight sprite * spriteScale
      cx = (aabbMinX box + aabbMaxX box) / 2
      cy = aabbMinY box + renderedH / 2
   in Translate cx cy $
        Scale spriteScale spriteScale (spritePicture sprite)

drawSpriteAtHeight :: Float -> Sprite -> Picture
drawSpriteAtHeight targetHeight sprite =
  let spriteScale = targetHeight / spriteHeight sprite
   in Scale spriteScale spriteScale (spritePicture sprite)

drawSpriteCover :: Float -> Float -> Sprite -> Picture
drawSpriteCover targetW targetH sprite =
  let spriteScale = max (targetW / spriteWidth sprite) (targetH / spriteHeight sprite)
   in Scale spriteScale spriteScale (spritePicture sprite)

tileStrip :: Maybe Sprite -> Sprite -> Maybe Sprite -> Aabb -> Picture
tileStrip leftSprite midSprite rightSprite box =
  pictures (leftPieces ++ midPieces ++ rightPieces)
 where
  spriteScale = platformVisualHeight / spriteHeight midSprite
  tileW = spriteWidth midSprite * spriteScale
  tileH = spriteHeight midSprite * spriteScale
  topY = aabbMaxY box
  centerY = topY - tileH / 2
  minX = aabbMinX box
  maxX = aabbMaxX box
  width = max 0 (maxX - minX)
  tileCount = max 1 (ceiling (width / tileW) :: Int)
  tileAt :: Int -> Sprite -> Picture
  tileAt i sprite =
    Translate (minX + tileW / 2 + fromIntegral i * tileW) centerY $
      Scale spriteScale spriteScale (spritePicture sprite)
  leftPieces = maybe [] (\sprite -> [tileAt 0 sprite]) leftSprite
  rightPieces =
    maybe
      []
      (\sprite -> [tileAt (tileCount - 1) sprite])
      rightSprite
  midStart = if isNothing leftSprite then 0 else 1
  midEnd = if isNothing rightSprite then tileCount - 1 else tileCount - 2
  midPieces =
    [ tileAt i midSprite | i <- [midStart .. midEnd], i >= 0, i < tileCount
    ]

exitAabb :: ExitZone -> Aabb
exitAabb exitZone =
  aabbFromBottomLeft (exitPos exitZone) (exitWidth exitZone) (exitHeight exitZone)
