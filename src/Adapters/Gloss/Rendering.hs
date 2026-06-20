-- | Adaptador de renderizado: 'GameView' → primitivas Gloss con cámara y HUD.
module Adapters.Gloss.Rendering (
  renderFrame,
)
where

import Control.Applicative ((<|>))
import Data.Maybe (isNothing)

import Graphics.Gloss.Data.Color (Color, makeColor)
import Graphics.Gloss.Data.Picture (Picture (..), circleSolid, pictures, rectangleSolid, rectangleWire, text)

import Adapters.Gloss.Camera (cameraXForWorld)
import Adapters.Gloss.Config (
  cameraY,
  crumblingPlatformColor,
  enemyColorForKind,
  fallingHazardColor,
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
  projectileColor,
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
import Domain.Model.CrumblingPlatform (
  CrumblingPlatform,
  crumblingPlatformAabb,
 )
import Domain.Model.Enemy (Enemy, enemyAabb, enemyFacing, enemyKind, enemyPos)
import Domain.Model.ExitZone (ExitZone, exitZoneAabb)
import Domain.Model.FallingHazard (
  FallingHazard (..),
  fallingHazardAabb,
  fallingHazardIsActive,
 )
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
import Domain.Model.Projectile (
  Projectile (projectileOwner),
  ProjectileOwner (..),
  projectileAabb,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (Aabb (..))
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
hudPanelWidth = 280

hudPanelHeight :: Float
hudPanelHeight = 180

hudLabelScale :: Float
hudLabelScale = 0.2

hudHintScale :: Float
hudHintScale = 0.16

-- Geometría interna del panel (px de pantalla).

-- | Margen interno del panel para el contenido.
hudContentInset :: Float
hudContentInset = 14

-- | Desplazamiento de la primera fila bajo el borde superior del panel.
hudRow1Offset :: Float
hudRow1Offset = 30

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
hudLabelColumnWidth = 112

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
hudAttackBoxOffsetX = 8

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
bossBarTopOffset = 52

bossBarLabelOffsetX :: Float
bossBarLabelOffsetX = -28

bossBarLabelOffsetY :: Float
bossBarLabelOffsetY = 18

bossLabelColor :: Color
bossLabelColor = makeColor 0.02 0.03 0.06 0.92

bossLabelShadowColor :: Color
bossLabelShadowColor = makeColor 1.0 1.0 1.0 0.45

entitySpritePadding :: Float
entitySpritePadding = 0.0

platformVisualHeight :: Float
platformVisualHeight = 35

floorVisualDepth :: Float
floorVisualDepth = 190

pickupVisualHeight :: Float
pickupVisualHeight = 28

playerProjectileVisualHeight :: Float
playerProjectileVisualHeight = 28

enemyProjectileVisualHeight :: Float
enemyProjectileVisualHeight = 24

exitDoorGap :: Float
exitDoorGap = 12

exitSignVisualHeight :: Float
exitSignVisualHeight = 42

attackCueHeight :: Float
attackCueHeight = 34

attackCueGap :: Float
attackCueGap = 7

attackCueLift :: Float
attackCueLift = 24

damageFlashColor :: Color
damageFlashColor = makeColor 1.0 0.15 0.1 0.38

hitboxFootRadius :: Float
hitboxFootRadius = 3.0

-- | Dibuja el mundo con cámara horizontal y HUD fijo en pantalla.
renderFrame :: SpriteCatalog -> Int -> Bool -> GameView -> Picture
renderFrame catalog renderTick showHitboxes gv =
  pictures
    [ Scale renderZoom renderZoom $
        pictures
          [ renderBackground catalog (gvLevelIndex gv)
          , renderWorldLayer catalog renderTick showHitboxes (gvCombatParams gv) (gvWorld gv)
          ]
    , renderHud catalog gv showHitboxes
    , renderBossBar gv
    , renderGameOverOverlay gv
    , renderLevelCompleteOverlay gv
    , renderVictoryOverlay gv
    ]

-- | Índice del nivel final/jefe; usa el fondo del castillo a partir de aquí.
bossLevelIndex :: Int
bossLevelIndex = 3

renderBackground :: SpriteCatalog -> Int -> Picture
renderBackground catalog levelIndex =
  case backgroundSprite of
    Nothing -> Blank
    Just sprite -> drawSpriteCover backgroundWidth backgroundHeight sprite
 where
  backgroundSprite
    | levelIndex >= bossLevelIndex = scBackgroundCastle catalog
    | otherwise = scBackgroundGrasslands catalog

backgroundWidth :: Float
backgroundWidth = fromIntegral windowWidth

backgroundHeight :: Float
backgroundHeight = fromIntegral windowHeight

-- | Capa del mundo con transformación de cámara.
renderWorldLayer :: SpriteCatalog -> Int -> Bool -> CombatParams -> World -> Picture
renderWorldLayer catalog renderTick showHitboxes combatParams w =
  let cameraX = cameraXForWorld w
   in Translate (-cameraX) (-cameraY) $
        pictures
          [ pictures (map (renderPlatform catalog) (worldPlatforms w))
          , pictures (map (renderMovingPlatform catalog) (worldMovingPlatforms w))
          , pictures (map renderCrumblingPlatform (worldCrumblingPlatforms w))
          , pictures (map (renderEnemy catalog renderTick) (worldEnemies w))
          , pictures (map (renderPickup catalog) (worldPickups w))
          , pictures (map (renderProjectile catalog) (worldProjectiles w))
          , pictures (map renderFallingHazard (worldFallingHazards w))
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
    Nothing -> aabbCenteredPicture pickupColor pickupVisualHeight box
    Just sprite -> drawSpriteCenteredAtHeight box pickupVisualHeight sprite
 where
  box = pickupAabb pickup

renderProjectile :: SpriteCatalog -> Projectile -> Picture
renderProjectile catalog proj =
  case scProjectileRock catalog of
    Nothing -> aabbCenteredPicture projectileColor visualHeight box
    Just sprite -> drawSpriteCenteredAtHeight box visualHeight sprite
 where
  box = projectileAabb proj
  visualHeight = projectileVisualHeight proj

projectileVisualHeight :: Projectile -> Float
projectileVisualHeight proj = case projectileOwner proj of
  PlayerProjectile -> playerProjectileVisualHeight
  EnemyProjectile -> enemyProjectileVisualHeight

renderFallingHazard :: FallingHazard -> Picture
renderFallingHazard h
  | fallingHazardIsActive h = aabbToPicture fallingHazardColor (fallingHazardAabb h)
  | otherwise = Blank

renderPlatform :: SpriteCatalog -> Platform -> Picture
renderPlatform catalog platform =
  case platformKind box of
    FloorPlatform -> renderGroundPlatform catalog box
    WallPlatform -> renderWallPlatform catalog box
    LedgePlatform ->
      case platformSprites catalog box of
        (leftSprite, Just midSprite, rightSprite) ->
          tileStrip leftSprite midSprite rightSprite box
        _ -> aabbToPicture platformColor box
 where
  box = platformAabb platform

data PlatformKind
  = FloorPlatform
  | WallPlatform
  | LedgePlatform

platformKind :: Aabb -> PlatformKind
platformKind box
  | aabbMinY box <= 0
  , platformHeight' > platformWidth' * 2 =
      WallPlatform
  | aabbMinY box <= 0 = FloorPlatform
  | otherwise = LedgePlatform
 where
  platformWidth' = aabbMaxX box - aabbMinX box
  platformHeight' = aabbMaxY box - aabbMinY box

renderGroundPlatform :: SpriteCatalog -> Aabb -> Picture
renderGroundPlatform catalog box =
  case platformSprites catalog box of
    (leftSprite, Just midSprite, rightSprite) ->
      pictures
        [ tileStrip leftSprite midSprite rightSprite box
        , renderGroundFill catalog box
        ]
    _ -> aabbToPicture platformColor box

renderGroundFill :: SpriteCatalog -> Aabb -> Picture
renderGroundFill catalog box =
  case scTileGrassCenter catalog of
    Nothing -> Blank
    Just sprite ->
      tileRect
        sprite
        (aabbMinX box)
        (aabbMaxX box)
        (aabbMaxY box - floorVisualDepth)
        (aabbMaxY box - platformVisualHeight)

renderWallPlatform :: SpriteCatalog -> Aabb -> Picture
renderWallPlatform catalog box =
  case (scTileGrassCenter catalog, scTileGrassMid catalog) of
    (Just fillSprite, Just topSprite) ->
      pictures
        [ tileRect
            fillSprite
            (aabbMinX box)
            (aabbMaxX box)
            (aabbMinY box)
            (aabbMaxY box - platformVisualHeight)
        , tileStrip Nothing topSprite Nothing box
        ]
    (Just fillSprite, Nothing) ->
      tileRect fillSprite (aabbMinX box) (aabbMaxX box) (aabbMinY box) (aabbMaxY box)
    _ -> aabbToPicture platformColor box

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

renderCrumblingPlatform :: CrumblingPlatform -> Picture
renderCrumblingPlatform cp =
  aabbToPicture crumblingPlatformColor (crumblingPlatformAabb cp)

renderExitZone :: SpriteCatalog -> ExitZone -> Picture
renderExitZone catalog exitZone =
  pictures
    [ renderExitDoor catalog box
    , renderExitSign catalog box
    ]
 where
  box = exitZoneAabb exitZone

renderExitSign :: SpriteCatalog -> Aabb -> Picture
renderExitSign catalog box =
  case scExitSign catalog of
    Nothing -> aabbToPicture hudMutedColor box
    Just sprite ->
      drawSpriteBottomCenterAtHeight
        signX
        (aabbMinY box)
        exitSignVisualHeight
        sprite
 where
  signX = aabbMinX box - exitDoorGap - exitSignVisualHeight / 2

renderExitDoor :: SpriteCatalog -> Aabb -> Picture
renderExitDoor catalog box =
  case (scExitDoorTop catalog, scExitDoorMid catalog) of
    (Just topSprite, Just midSprite) ->
      drawStackedDoorBottomCenter box topSprite midSprite
    (_, Just midSprite) ->
      drawSpriteBottomCenter box midSprite
    _ -> Blank

-- | Contornos de todas las cajas de colisión del mundo (debug de alineación).
renderHitboxOverlay :: CombatParams -> World -> Picture
renderHitboxOverlay combatParams w =
  let p = worldPlayer w
      playerBox = playerAabb p
      meleeOverlay =
        [ aabbOutline
            hudAttackColor
            (meleeHitbox combatParams playerBox (playerFacing p))
        | hasFramesLeft (playerAttackFrames p)
        ]
   in pictures
        ( [ aabbOutline platformColor (platformAabb plat)
          | plat <- worldPlatforms w
          ]
            <> [ aabbOutline movingPlatformColor (movingPlatformAabb mp)
               | mp <- worldMovingPlatforms w
               ]
            <> [ aabbOutline crumblingPlatformColor (crumblingPlatformAabb cp)
               | cp <- worldCrumblingPlatforms w
               ]
            <> [ aabbOutline (enemyColorForKind (enemyKind e)) (enemyAabb e)
               | e <- worldEnemies w
               ]
            <> [ aabbOutline pickupColor (pickupAabb pickup)
               | pickup <- worldPickups w
               ]
            <> [ aabbOutline projectileColor (projectileAabb proj)
               | proj <- worldProjectiles w
               ]
            <> [ aabbOutline fallingHazardColor (fallingHazardAabb fh)
               | fh <- worldFallingHazards w
               , fallingHazardIsActive fh
               ]
            <> [ aabbOutline hudMutedColor (exitZoneAabb (worldExit w))
               , aabbOutline playerColor playerBox
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
        , renderExitHints gv contentX row5Y
        , hudHint contentX (row5Y - hudHintGap) "Space - attack"
        , hudHint contentX (row5Y - hudHintGap * 2) "X - throw"
        , if showHitboxes
            then hudHint contentX (row5Y - hudHintGap * 2 - 16) "F1 - hitboxes"
            else Blank
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
            , Translate bossBarLabelOffsetX (barY + bossBarLabelOffsetY) $
                pictures
                  [ Translate 1 (-1) $
                      Scale hudLabelScale hudLabelScale $
                        Color bossLabelShadowColor (text "BOSS")
                  , Scale hudLabelScale hudLabelScale $
                      Color bossLabelColor (text "BOSS")
                  ]
            ]

renderGameOverOverlay :: GameView -> Picture
renderGameOverOverlay gv
  | gvPhase gv /= GameOver = Blank
  | otherwise =
      renderCenteredOverlay "GAME OVER" "Enter - retry    Esc - quit"

renderLevelCompleteOverlay :: GameView -> Picture
renderLevelCompleteOverlay gv
  | gvPhase gv /= LevelComplete = Blank
  | otherwise =
      renderCenteredOverlay
        ("LEVEL " ++ show (gvLevelIndex gv) ++ " COMPLETE")
        "Press Enter to continue"

renderVictoryOverlay :: GameView -> Picture
renderVictoryOverlay gv
  | gvPhase gv /= Victory = Blank
  | otherwise =
      renderCenteredOverlay "VICTORY!" "Enter - play again    Esc - quit"

renderCenteredOverlay :: String -> String -> Picture
renderCenteredOverlay title hint =
  pictures
    [ Color hudOverlayDim $
        rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight)
    , Translate 0 hudGameOverOffsetY $
        Scale hudGameOverScale hudGameOverScale $
          Color hudAttackColor (text title)
    , Translate 0 hudGameOverHintOffsetY $
        Scale hudHintScale hudHintScale $
          Color hudMutedColor (text hint)
    ]

renderExitHints :: GameView -> Float -> Float -> Picture
renderExitHints gv x y =
  case gvExitScoreHint gv of
    Just (current, required) ->
      hudHint
        x
        y
        ( "Need "
            ++ show (scorePoints required)
            ++ " pts (have "
            ++ show (scorePoints current)
            ++ ")"
        )
    Nothing
      | gvBossExitHint gv -> hudHint x y "Defeat the boss to leave"
      | otherwise -> Blank

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

aabbCenteredPicture :: Color -> Float -> Aabb -> Picture
aabbCenteredPicture color visualHeight box =
  let cx = (aabbMinX box + aabbMaxX box) / 2
      cy = (aabbMinY box + aabbMaxY box) / 2
   in Translate cx cy (Color color (rectangleSolid visualHeight visualHeight))

-- | Contorno de una caja de colisión (solo borde, sin rellenar el interior).
aabbOutline :: Color -> Aabb -> Picture
aabbOutline color box =
  let w = aabbMaxX box - aabbMinX box
      h = aabbMaxY box - aabbMinY box
      cx = (aabbMinX box + aabbMaxX box) / 2
      cy = (aabbMinY box + aabbMaxY box) / 2
   in Translate cx cy (Color color (rectangleWire w h))

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

drawSpriteCenteredAtHeight :: Aabb -> Float -> Sprite -> Picture
drawSpriteCenteredAtHeight box targetHeight sprite =
  let spriteScale = targetHeight / spriteHeight sprite
      cx = (aabbMinX box + aabbMaxX box) / 2
      cy = (aabbMinY box + aabbMaxY box) / 2
   in Translate cx cy $
        Scale spriteScale spriteScale (spritePicture sprite)

drawStackedDoorBottomCenter :: Aabb -> Sprite -> Sprite -> Picture
drawStackedDoorBottomCenter box topSprite midSprite =
  pictures
    [ Translate cx (bottomY + midH / 2) $
        Scale spriteScale spriteScale (spritePicture midSprite)
    , Translate cx (bottomY + midH + topH / 2) $
        Scale spriteScale spriteScale (spritePicture topSprite)
    ]
 where
  boxW = max 1 (aabbMaxX box - aabbMinX box)
  boxH = max 1 (aabbMaxY box - aabbMinY box)
  doorW = max (spriteWidth topSprite) (spriteWidth midSprite)
  doorH = spriteHeight topSprite + spriteHeight midSprite
  spriteScale = min (boxW / doorW) (boxH / doorH)
  midH = spriteHeight midSprite * spriteScale
  topH = spriteHeight topSprite * spriteScale
  cx = (aabbMinX box + aabbMaxX box) / 2
  bottomY = aabbMinY box

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

drawSpriteBottomCenterAtHeight :: Float -> Float -> Float -> Sprite -> Picture
drawSpriteBottomCenterAtHeight cx bottomY targetHeight sprite =
  let spriteScale = targetHeight / spriteHeight sprite
      renderedH = spriteHeight sprite * spriteScale
      cy = bottomY + renderedH / 2
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
  if width <= 0
    then Blank
    else pictures stripPieces
 where
  spriteScaleY = platformVisualHeight / spriteHeight midSprite
  naturalTileW = spriteWidth midSprite * spriteScaleY
  topY = aabbMaxY box
  minX = aabbMinX box
  maxX = aabbMaxX box
  width = max 0 (maxX - minX)
  tileCount = max 1 (ceiling (width / naturalTileW) :: Int)
  tileW = width / fromIntegral tileCount
  tileH = platformVisualHeight
  centerY = topY - tileH / 2
  tileAt :: Int -> Sprite -> Picture
  tileAt i sprite =
    Translate (minX + tileW / 2 + fromIntegral i * tileW) centerY $
      Scale (tileW / spriteWidth sprite) spriteScaleY (spritePicture sprite)
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
  stripPieces
    | tileCount == 1 = [tileAt 0 midSprite]
    | otherwise = leftPieces ++ midPieces ++ rightPieces

tileRect :: Sprite -> Float -> Float -> Float -> Float -> Picture
tileRect sprite minX maxX minY maxY =
  if width <= 0 || height <= 0
    then Blank
    else
      pictures
        [ tileAt ix iy
        | ix <- [0 .. tileCountX - 1]
        , iy <- [0 .. tileCountY - 1]
        ]
 where
  spriteScaleY = platformVisualHeight / spriteHeight sprite
  naturalTileW = spriteWidth sprite * spriteScaleY
  naturalTileH = spriteHeight sprite * spriteScaleY
  width = max 0 (maxX - minX)
  height = max 0 (maxY - minY)
  tileCountX = max 1 (ceiling (width / naturalTileW) :: Int)
  tileCountY = max 1 (ceiling (height / naturalTileH) :: Int)
  tileW = width / fromIntegral tileCountX
  tileH = height / fromIntegral tileCountY
  tileAt ix iy =
    Translate
      (minX + tileW / 2 + fromIntegral ix * tileW)
      (maxY - tileH / 2 - fromIntegral iy * tileH)
      (Scale (tileW / spriteWidth sprite) (tileH / spriteHeight sprite) (spritePicture sprite))
