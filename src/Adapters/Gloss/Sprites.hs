-- | Carga y selección de sprites para el adaptador Gloss.
module Adapters.Gloss.Sprites (
  Sprite (..),
  SpriteCatalog (..),
  loadSpriteCatalog,
  playerSprite,
  enemySprite,
)
where

import Control.Applicative ((<|>))
import Control.Exception (AsyncException, SomeException, fromException, throwIO, try)
import Data.Maybe (catMaybes)
import System.IO (hPutStrLn, stderr)

import Graphics.Gloss (Picture, loadBMP)

import Domain.Model.Enemy (Enemy (..))
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.Model.Player (Player (..))
import Domain.ValueObjects.Frames (hasFramesLeft)
import Domain.ValueObjects.Velocity (velX, velY)
import Paths_wonderboy_hs (getDataFileName)

-- | Bitmap cargado junto con su tamaño original en píxeles.
data Sprite = Sprite
  { spritePicture :: Picture
  , spriteWidth :: Float
  , spriteHeight :: Float
  }

-- | Catálogo de sprites opcionales. Ausencias caen al render de color.
data SpriteCatalog = SpriteCatalog
  { scBackgroundGrasslands :: Maybe Sprite
  , scBackgroundCastle :: Maybe Sprite
  , scPlayerIdle :: Maybe Sprite
  , scPlayerJump :: Maybe Sprite
  , scPlayerHurt :: Maybe Sprite
  , scPlayerWalk :: [Sprite]
  , scPickupGem :: Maybe Sprite
  , scTileGrassLeft :: Maybe Sprite
  , scTileGrassMid :: Maybe Sprite
  , scTileGrassRight :: Maybe Sprite
  , scTileMovingLeft :: Maybe Sprite
  , scTileMovingMid :: Maybe Sprite
  , scTileMovingRight :: Maybe Sprite
  , scTileBridge :: Maybe Sprite
  , scExitSign :: Maybe Sprite
  , scSnailIdle :: Maybe Sprite
  , scSnailWalk :: Maybe Sprite
  , scBatIdle :: Maybe Sprite
  , scBatFly :: Maybe Sprite
  , scGolemIdle :: Maybe Sprite
  , scGolemWalk :: Maybe Sprite
  , scBossGolem :: Maybe Sprite
  , scBossBat :: Maybe Sprite
  , scHudLife :: Maybe Sprite
  , scHudLifeX :: Maybe Sprite
  , scHudHeartFull :: Maybe Sprite
  , scHudHeartHalf :: Maybe Sprite
  , scHudHeartEmpty :: Maybe Sprite
  , scHudScoreGem :: Maybe Sprite
  , scHudAttackSword :: Maybe Sprite
  }

-- | Carga todos los sprites conocidos. Los errores dejan entradas vacías.
loadSpriteCatalog :: IO SpriteCatalog
loadSpriteCatalog =
  SpriteCatalog
    <$> loadSprite "assets/sprites/backgrounds/grasslands.bmp" 1024 512
    <*> loadSprite "assets/sprites/backgrounds/castle.bmp" 1024 512
    <*> loadSprite "assets/sprites/player/player-idle.bmp" 66 92
    <*> loadSprite "assets/sprites/player/player-jump.bmp" 67 94
    <*> loadSprite "assets/sprites/player/player-hurt.bmp" 69 92
    <*> loadWalkSprites
    <*> loadSprite "assets/sprites/pickups/gem-yellow.bmp" 70 70
    <*> loadSprite "assets/sprites/tiles/grass-left.bmp" 70 70
    <*> loadSprite "assets/sprites/tiles/grass-mid.bmp" 70 70
    <*> loadSprite "assets/sprites/tiles/grass-right.bmp" 70 70
    <*> loadSprite "assets/sprites/tiles/grass-half-left.bmp" 70 70
    <*> loadSprite "assets/sprites/tiles/grass-half-mid.bmp" 70 70
    <*> loadSprite "assets/sprites/tiles/grass-half-right.bmp" 70 70
    <*> loadSprite "assets/sprites/tiles/bridge.bmp" 70 70
    <*> loadSprite "assets/sprites/tiles/sign-exit.bmp" 70 70
    <*> loadSprite "assets/sprites/enemies/snail-idle.bmp" 55 40
    <*> loadSprite "assets/sprites/enemies/snail-walk.bmp" 60 40
    <*> loadSprite "assets/sprites/enemies/bat-idle.bmp" 70 47
    <*> loadSprite "assets/sprites/enemies/bat-fly.bmp" 88 37
    <*> loadSprite "assets/sprites/enemies/golem-idle.bmp" 71 70
    <*> loadSprite "assets/sprites/enemies/golem-walk.bmp" 71 70
    <*> loadSprite "assets/sprites/bosses/boss-golem.bmp" 51 51
    <*> loadSprite "assets/sprites/bosses/boss-bat.bmp" 88 37
    <*> loadSprite "assets/sprites/ui/life-p1.bmp" 47 47
    <*> loadSprite "assets/sprites/ui/life-x.bmp" 30 28
    <*> loadSprite "assets/sprites/ui/heart-full.bmp" 53 45
    <*> loadSprite "assets/sprites/ui/heart-half.bmp" 53 45
    <*> loadSprite "assets/sprites/ui/heart-empty.bmp" 53 45
    <*> loadSprite "assets/sprites/ui/score-gem-yellow.bmp" 46 36
    <*> loadSprite "assets/sprites/ui/attack-sword.bmp" 70 70

loadWalkSprites :: IO [Sprite]
loadWalkSprites =
  catMaybes
    <$> traverse
      (\i -> loadSprite ("assets/sprites/player/player-walk-" <> pad2 i <> ".bmp") 72 97)
      [1 .. 11 :: Int]

loadSprite :: FilePath -> Float -> Float -> IO (Maybe Sprite)
loadSprite relPath width height = do
  path <- getDataFileName relPath
  loaded <- try (loadBMP path) :: IO (Either SomeException Picture)
  case loaded of
    Left err -> spriteLoadFailure relPath err
    Right picture ->
      pure
        ( Just
            Sprite
              { spritePicture = picture
              , spriteWidth = width
              , spriteHeight = height
              }
        )

spriteLoadFailure :: FilePath -> SomeException -> IO (Maybe Sprite)
spriteLoadFailure relPath err =
  case fromException err :: Maybe AsyncException of
    Just asyncErr -> throwIO asyncErr
    Nothing -> do
      hPutStrLn stderr ("Warning: failed to load sprite " <> relPath <> ": " <> show err)
      pure Nothing

-- | Sprite del jugador según estado visible y contador de render.
playerSprite :: SpriteCatalog -> Int -> Player -> Maybe Sprite
playerSprite catalog renderFrame player
  | hasFramesLeft (playerInvincibilityFrames player) =
      scPlayerHurt catalog <|> movementSprite
  | not (playerOnGround player) =
      scPlayerJump catalog <|> movementSprite
  | otherwise = movementSprite
 where
  movementSprite =
    if abs (velX (playerVel player)) > movingEpsilon
      then cyclingSprite renderFrame (scPlayerWalk catalog) <|> scPlayerIdle catalog
      else scPlayerIdle catalog

-- | Sprite de enemigo por clase y movimiento visible.
enemySprite :: SpriteCatalog -> Int -> Enemy -> Maybe Sprite
enemySprite catalog renderFrame enemy =
  case enemyKind enemy of
    SnailKind ->
      if moving
        then alternating (scSnailIdle catalog) (scSnailWalk catalog)
        else scSnailIdle catalog
    BatKind ->
      if moving
        then alternating (scBatIdle catalog) (scBatFly catalog)
        else scBatIdle catalog
    GolemKind ->
      if moving
        then alternating (scGolemIdle catalog) (scGolemWalk catalog)
        else scGolemIdle catalog
    BossGolemKind -> scBossGolem catalog
    BossBatKind -> scBossBat catalog <|> scBatFly catalog
 where
  moving = enemyMoving enemy
  alternating idle movingSprite =
    if even (renderFrame `div` enemyAnimationStride)
      then movingSprite <|> idle
      else idle <|> movingSprite

cyclingSprite :: Int -> [Sprite] -> Maybe Sprite
cyclingSprite _ [] = Nothing
cyclingSprite renderFrame sprites =
  Just (sprites !! index)
 where
  index = (renderFrame `div` playerAnimationStride) `mod` length sprites

pad2 :: Int -> String
pad2 n
  | n < 10 = "0" <> show n
  | otherwise = show n

movingEpsilon :: Float
movingEpsilon = 0.01

enemyMoving :: Enemy -> Bool
enemyMoving e
  | enemyKind e `elem` [BatKind, BossBatKind] =
      let v = enemyVel e
       in sqrt (velX v * velX v + velY v * velY v) > movingEpsilon
  | otherwise =
      abs (velX (enemyVel e)) > movingEpsilon

playerAnimationStride :: Int
playerAnimationStride = 4

enemyAnimationStride :: Int
enemyAnimationStride = 12
