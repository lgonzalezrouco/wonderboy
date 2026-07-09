module Adapters.Gloss.Rendering (
  renderFrame,
)
where

import Control.Applicative ((<|>))
import Data.Maybe (isNothing)

import Graphics.Gloss.Data.Color (Color, makeColor)
import Graphics.Gloss.Data.Picture (Picture (..), circleSolid, pictures, rectangleSolid, rectangleWire, text)

import Adapters.Gloss.Camera (cameraXForWorld, worldHorizontalSpan)
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
import Adapters.Gloss.Tiling (tilesToCover)
import Domain.Logic.MeleeSwing (
  attackBodyLunge,
  attackCueHandInset,
  attackCueHeight,
  attackPhase,
  attackSwingAngle,
  clamp01,
  meleeImpactPhase,
 )
import Domain.Model.CrumblingPlatform (
  CrumblingPlatform,
  crumblingPlatformAabb,
 )
import Domain.Model.Enemy (Enemy, enemyAabb, enemyFacing, enemyHurtFrames, enemyKind, enemyPos)
import Domain.Model.ExitZone (ExitZone, exitZoneAabb)
import Domain.Model.FallingHazard (
  FallingHazard (..),
  fallingHazardAabb,
  fallingHazardIsActive,
 )
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.MovingPlatform (MovingPlatform, movingPlatformAabb)
import Domain.Model.Pickup (Pickup, pickupAabb, pickupPos)
import Domain.Model.Platform (Platform, platformAabb)
import Domain.Model.Player (
  Player (..),
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
import Domain.ValueObjects.CombatParams (CombatParams (..))
import Domain.ValueObjects.Facing (Facing (..), facingScale)
import Domain.ValueObjects.Frames (Frames, hasFramesLeft)
import Domain.ValueObjects.Health (healthPoints)
import Domain.ValueObjects.Lives (livesCount)
import Domain.ValueObjects.Position (Position, posX, posY)
import Domain.ValueObjects.Score (scorePoints)
import UseCases.Engine.GameView (GameView (..))

-- El layout del HUD está en píxeles de pantalla: el panel se dibuja fuera de renderZoom,
-- así que son coordenadas de ventana, no unidades de mundo.
hudMargin :: Float
hudMargin = 14

hudPanelWidth :: Float
hudPanelWidth = 280

hudPanelHeight :: Float
hudPanelHeight = 220

-- Multiplicadores de escala del 'text' de Gloss, no píxeles (los demás valores hud*Scale también).
hudLabelScale :: Float
hudLabelScale = 0.2

hudHintScale :: Float
hudHintScale = 0.16

hudContentInset :: Float
hudContentInset = 14

hudRow1Offset :: Float
hudRow1Offset = 30

hudRowGap :: Float
hudRowGap = 36

hudHintGap :: Float
hudHintGap = 20

hudTextHeight :: Float
hudTextHeight = 18

hudLabelColumnWidth :: Float
hudLabelColumnWidth = 112

-- Sube el valor de una fila media línea de texto para que quede centrado respecto de su label.
hudValueCenterLift :: Float
hudValueCenterLift = hudTextHeight / 2

hudLifeIconStride :: Float
hudLifeIconStride = 20

hudLifeIconSize :: Float
hudLifeIconSize = 14

hudHealthPipStride :: Float
hudHealthPipStride = 26

hudHealthPipWidth :: Float
hudHealthPipWidth = 22

hudHealthPipHeight :: Float
hudHealthPipHeight = 10

hudAttackBoxOffsetX :: Float
hudAttackBoxOffsetX = 8

hudAttackBoxDrop :: Float
hudAttackBoxDrop = 6

hudAttackBoxWidth :: Float
hudAttackBoxWidth = 52

hudAttackBoxHeight :: Float
hudAttackBoxHeight = 14

hudGameOverScale :: Float
hudGameOverScale = 0.42

hudGameOverOffsetY :: Float
hudGameOverOffsetY = 24

hudGameOverHintOffsetY :: Float
hudGameOverHintOffsetY = -18

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

-- | Holgura (en px lógicos) para considerar que una pared está junto al borde derecho del mapa.
wallEdgeEpsilon :: Float
wallEdgeEpsilon = 1

pickupVisualHeight :: Float
pickupVisualHeight = 28

playerProjectileVisualHeight :: Float
playerProjectileVisualHeight = 28

enemyProjectileVisualHeight :: Float
enemyProjectileVisualHeight = 24

fallingHazardVisualHeight :: Float
fallingHazardVisualHeight = 34

exitDoorGap :: Float
exitDoorGap = 12

exitSignVisualHeight :: Float
exitSignVisualHeight = 42

attackCueLift :: Float
attackCueLift = 18

attackBodyLeanDegrees :: Float
attackBodyLeanDegrees = 7

attackSparkRadius :: Float
attackSparkRadius = 3

attackSparkPhaseWidth :: Float
attackSparkPhaseWidth = 0.16

-- | Tint del destello de daño. Solo un fallback para cuando falta un sprite.
damageBodyColor :: Color
damageBodyColor = makeColor 1.0 0.2 0.2 1.0

damageFlashStride :: Int
damageFlashStride = 8

damageFlashOn :: Int -> Bool
damageFlashOn tick = even (tick `div` damageFlashStride)

hitboxFootRadius :: Float
hitboxFootRadius = 3.0

renderFrame :: SpriteCatalog -> Int -> Bool -> GameView -> Picture
renderFrame catalog renderTick showHitboxes gv =
  pictures
    [ Scale renderZoom renderZoom $
        pictures
          [ renderBackground catalog (gvLevelIndex gv)
          , renderWorldLayer catalog renderTick showHitboxes gv
          ]
    , renderHud catalog gv showHitboxes
    , renderBossBar gv
    , renderBossArenaBanner gv
    , renderGameOverOverlay gv
    , renderLevelCompleteOverlay gv
    , renderVictoryOverlay gv
    ]

-- | Desde este índice de nivel en adelante, dibuja el fondo del castillo (el nivel final/del jefe).
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

renderWorldLayer :: SpriteCatalog -> Int -> Bool -> GameView -> Picture
renderWorldLayer catalog renderTick showHitboxes gv =
  let w = gvWorld gv
      combatParams = gvCombatParams gv
      cameraX = cameraXForWorld w
      -- Borde derecho del mapa: solo se dibuja la pared que queda junto a él (la pared final).
      -- Las demás son solo barreras de colisión invisibles.
      rightEdge = snd <$> worldHorizontalSpan w
   in Translate (-cameraX) (-cameraY) $
        pictures
          [ pictures (map (renderPlatform catalog rightEdge) (worldPlatforms w))
          , pictures (map (renderMovingPlatform catalog) (worldMovingPlatforms w))
          , pictures (map renderCrumblingPlatform (worldCrumblingPlatforms w))
          , pictures (map (renderEnemy catalog renderTick) (worldEnemies w))
          , pictures (map (renderPickup catalog) (worldPickups w))
          , pictures (map (renderProjectile catalog) (worldProjectiles w))
          , pictures (map (renderFallingHazard catalog) (worldFallingHazards w))
          , renderExitZone catalog (worldExit w)
          , renderBossArenaWalls (gvBossArenaWalls gv)
          , renderPlayer catalog renderTick combatParams (worldPlayer w)
          , if showHitboxes then renderHitboxOverlay (gvMeleeHitbox gv) w else Blank
          ]

renderPlayer :: SpriteCatalog -> Int -> CombatParams -> Player -> Picture
renderPlayer catalog renderTick combatParams p =
  pictures
    [ posePlayerForAttack combatParams p box body
    , renderPlayerAttackCue catalog combatParams p box
    ]
 where
  box = playerAabb p
  body =
    case playerSprite catalog renderTick p of
      Nothing -> aabbToPicture bodyColor box
      Just sprite ->
        drawEntitySpriteWith (playerFacing p) box sprite (bodyPicture sprite)
  hurtFlash = showsHurtFlash (playerInvincibilityFrames p) renderTick
  bodyPicture sprite =
    if hurtFlash then spriteHurtPicture sprite else spritePicture sprite
  bodyColor = if hurtFlash then damageBodyColor else playerColor

renderEnemy :: SpriteCatalog -> Int -> Enemy -> Picture
renderEnemy catalog renderTick e =
  case enemySprite catalog renderTick e of
    Nothing -> aabbToPicture (enemyColorForKind (enemyKind e)) box
    Just sprite ->
      if hurtFlash
        then drawEntitySpriteWith (enemyFacing e) box sprite (spriteHurtPicture sprite)
        else drawEntitySprite (enemyFacing e) box sprite
 where
  box = enemyAabb e
  hurtFlash = showsHurtFlash (enemyHurtFrames e) renderTick

showsHurtFlash :: Frames -> Int -> Bool
showsHurtFlash frames renderTick =
  hasFramesLeft frames && damageFlashOn renderTick

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

renderFallingHazard :: SpriteCatalog -> FallingHazard -> Picture
renderFallingHazard catalog h
  | not (fallingHazardIsActive h) = Blank
  | otherwise =
      case scFallingHazard catalog of
        Nothing -> aabbToPicture fallingHazardColor box
        Just sprite -> drawSpriteCenteredAtHeight box fallingHazardVisualHeight sprite
 where
  box = fallingHazardAabb h

renderPlatform :: SpriteCatalog -> Maybe Float -> Platform -> Picture
renderPlatform catalog rightEdge platform =
  case platformKind box of
    FloorPlatform -> renderGroundPlatform catalog box
    WallPlatform
      -- La pared final (junto al borde derecho del mapa) se dibuja como una columna de tierra.
      | isFinalWall -> renderFinalWall catalog box
      -- Las demás paredes son barreras de colisión invisibles. La cámara ya está clampeada
      -- a los bordes del mapa (ver Adapters.Gloss.Camera), así que no hay nada que dibujar.
      | otherwise -> Blank
    LedgePlatform ->
      case platformSprites catalog box of
        (leftSprite, Just midSprite, rightSprite) ->
          tileStrip leftSprite midSprite rightSprite box
        _ -> aabbToPicture platformColor box
 where
  box = platformAabb platform
  -- Es la pared final si y solo si su lado derecho llega al borde derecho del mapa.
  isFinalWall = maybe False (\edge -> aabbMaxX box >= edge - wallEdgeEpsilon) rightEdge

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

{- | La pared final del mapa: una columna de tierra de altura completa (sin franja de pasto) que además
se extiende por debajo de su base para unirse con la capa de tierra del piso adyacente sin costura.
-}
renderFinalWall :: SpriteCatalog -> Aabb -> Picture
renderFinalWall catalog box =
  case scTileGrassCenter catalog of
    Nothing -> aabbToPicture platformColor box
    Just fillSprite ->
      tileRect
        fillSprite
        (aabbMinX box)
        (aabbMaxX box)
        (aabbMinY box - floorVisualDepth)
        (aabbMaxY box)

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

renderBossArenaWalls :: [Platform] -> Picture
renderBossArenaWalls =
  pictures . map (aabbToPicture bossArenaWallColor . platformAabb)

bossArenaWallColor :: Color
bossArenaWallColor = makeColor 0.95 0.45 0.2 0.55

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

renderHitboxOverlay :: Maybe Aabb -> World -> Picture
renderHitboxOverlay mMeleeHitbox w =
  let p = worldPlayer w
      playerBox = playerAabb p
      meleeOverlay =
        [ aabbOutline hudAttackColor box
        | box <- maybe [] pure mMeleeHitbox
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

renderBossArenaBanner :: GameView -> Picture
renderBossArenaBanner gv
  | gvBossArenaSealed gv =
      let halfH = fromIntegral windowHeight / 2
          bannerY = halfH - bossBarTopOffset - 28
       in Translate 0 bannerY $
            Scale hudHintScale hudHintScale $
              Color hudBossColor (text "ARENA SEALED")
  | otherwise = Blank

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
      | gvBossArenaSealed gv -> hudHint x y "Arena sealed - defeat the boss"
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

posePlayerForAttack :: CombatParams -> Player -> Aabb -> Picture -> Picture
posePlayerForAttack combatParams p box body =
  case attackPhase combatParams p of
    Nothing -> body
    Just phase ->
      let envelope = sin (pi * phase)
          faceScale = facingScale (playerFacing p)
       in Translate (faceScale * attackBodyLunge * envelope) 0 $
            rotateAround
              (aabbCenterX box)
              (aabbMinY box)
              (-(faceScale * attackBodyLeanDegrees * envelope))
              body

renderPlayerAttackCue :: SpriteCatalog -> CombatParams -> Player -> Aabb -> Picture
renderPlayerAttackCue catalog combatParams p box =
  case attackPhase combatParams p of
    Nothing -> Blank
    Just phase ->
      let angle = attackSwingAngle phase
       in Translate cueX cueY $
            Scale faceScale 1 $
              pictures
                [ renderAttackSpark phase angle
                , renderAttackBlade catalog phase angle
                ]
 where
  facing = playerFacing p
  faceScale = facingScale facing
  cueX =
    case facing of
      FacingLeft -> aabbMinX box + attackCueHandInset
      FacingRight -> aabbMaxX box - attackCueHandInset
  cueY = aabbMinY box + attackCueLift

renderAttackBlade :: SpriteCatalog -> Float -> Float -> Picture
renderAttackBlade catalog phase angle =
  Rotate angle $
    Translate 0 (-(bladeHeight / 2)) $
      Scale 1 (-1) $
        case scHudAttackSword catalog of
          Nothing -> Color hudAttackColor (rectangleSolid 6 bladeHeight)
          Just sprite -> drawSpriteAtHeight bladeHeight sprite
 where
  bladeHeight = attackCueHeight * (1 + 0.08 * sin (pi * phase))

renderAttackSpark :: Float -> Float -> Picture
renderAttackSpark phase angle
  | impact <= 0 = Blank
  | otherwise =
      let arm = attackSparkRadius * 3 * impact
       in Rotate angle $
            Translate 0 (-attackCueHeight - 2) $
              Color (makeColor 1.0 0.96 0.62 (0.42 * impact)) $
                pictures
                  [ circleSolid (attackSparkRadius * impact)
                  , rectangleSolid arm 1.5
                  , Rotate 90 (rectangleSolid arm 1.5)
                  ]
 where
  impact = clamp01 (1 - abs (phase - meleeImpactPhase) / attackSparkPhaseWidth)

rotateAround :: Float -> Float -> Float -> Picture -> Picture
rotateAround x y degrees picture =
  Translate x y $
    Rotate degrees $
      Translate (-x) (-y) picture

aabbCenterX :: Aabb -> Float
aabbCenterX box = (aabbMinX box + aabbMaxX box) / 2

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

aabbOutline :: Color -> Aabb -> Picture
aabbOutline color box =
  let w = aabbMaxX box - aabbMinX box
      h = aabbMaxY box - aabbMinY box
      cx = (aabbMinX box + aabbMaxX box) / 2
      cy = (aabbMinY box + aabbMaxY box) / 2
   in Translate cx cy (Color color (rectangleWire w h))

renderFootAnchor :: Position -> Color -> Picture
renderFootAnchor pos color =
  Translate (posX pos) (posY pos) (Color color (circleSolid hitboxFootRadius))

drawEntitySprite :: Facing -> Aabb -> Sprite -> Picture
drawEntitySprite facing box sprite =
  drawEntitySpriteWith facing box sprite (spritePicture sprite)

drawEntitySpriteWith :: Facing -> Aabb -> Sprite -> Picture -> Picture
drawEntitySpriteWith facing box sprite picture =
  let availableW = max 1 (aabbMaxX box - aabbMinX box - entitySpritePadding)
      availableH = max 1 (aabbMaxY box - aabbMinY box - entitySpritePadding)
      spriteScale = min (availableW / spriteWidth sprite) (availableH / spriteHeight sprite)
      renderedH = spriteHeight sprite * spriteScale
      cx = (aabbMinX box + aabbMaxX box) / 2
      cy = aabbMinY box + renderedH / 2
      faceScale = facingScale facing * spriteScale
   in Translate cx cy $
        Scale faceScale spriteScale picture

data SpriteAlign
  = AlignCentered
  | AlignBottomCenter

drawSpriteScaled :: Float -> Float -> Float -> Sprite -> Picture
drawSpriteScaled cx cy spriteScale sprite =
  Translate cx cy $
    Scale spriteScale spriteScale (spritePicture sprite)

heightScale :: Float -> Sprite -> Float
heightScale targetHeight sprite = targetHeight / spriteHeight sprite

drawSpriteAt :: SpriteAlign -> Float -> Float -> Float -> Sprite -> Picture
drawSpriteAt align cx anchorY spriteScale sprite =
  let renderedH = spriteHeight sprite * spriteScale
      cy = case align of
        AlignCentered -> anchorY
        AlignBottomCenter -> anchorY + renderedH / 2
   in drawSpriteScaled cx cy spriteScale sprite

drawSpriteCenteredAtHeight :: Aabb -> Float -> Sprite -> Picture
drawSpriteCenteredAtHeight box targetHeight sprite =
  drawSpriteAt
    AlignCentered
    ((aabbMinX box + aabbMaxX box) / 2)
    ((aabbMinY box + aabbMaxY box) / 2)
    (heightScale targetHeight sprite)
    sprite

drawStackedDoorBottomCenter :: Aabb -> Sprite -> Sprite -> Picture
drawStackedDoorBottomCenter box topSprite midSprite =
  pictures
    [ drawSpriteScaled cx (bottomY + midH / 2) spriteScale midSprite
    , drawSpriteScaled cx (bottomY + midH + topH / 2) spriteScale topSprite
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
   in drawSpriteAt
        AlignBottomCenter
        ((aabbMinX box + aabbMaxX box) / 2)
        (aabbMinY box)
        spriteScale
        sprite

drawSpriteBottomCenterAtHeight :: Float -> Float -> Float -> Sprite -> Picture
drawSpriteBottomCenterAtHeight cx bottomY targetHeight sprite =
  drawSpriteAt AlignBottomCenter cx bottomY (heightScale targetHeight sprite) sprite

drawSpriteAtHeight :: Float -> Sprite -> Picture
drawSpriteAtHeight targetHeight sprite =
  let spriteScale = heightScale targetHeight sprite
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
  tileCount = tilesToCover width naturalTileW
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
  tileCountX = tilesToCover width naturalTileW
  tileCountY = tilesToCover height naturalTileH
  tileW = width / fromIntegral tileCountX
  tileH = height / fromIntegral tileCountY
  tileAt ix iy =
    Translate
      (minX + tileW / 2 + fromIntegral ix * tileW)
      (maxY - tileH / 2 - fromIntegral iy * tileH)
      (Scale (tileW / spriteWidth sprite) (tileH / spriteHeight sprite) (spritePicture sprite))
