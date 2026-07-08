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
import Data.Word (Word8)
import System.IO (hPutStrLn, stderr)

import Codec.BMP (BMP, bmpDimensions, packRGBA32ToBMP32, readBMP, unpackBMPToRGBA32)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Graphics.Gloss.Data.Bitmap (bitmapDataOfBMP, bitmapOfBMP, bitmapSize)
import Graphics.Gloss.Data.Picture (Picture)

import Domain.Model.Enemy (Enemy (..))
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.Model.Player (Player (..))
import Domain.ValueObjects.Velocity (velX, velY)
import Paths_wonderboy_hs (getDataFileName)

data Sprite = Sprite
  { spritePicture :: Picture
  , spriteHurtPicture :: Picture
  -- ^ Variante con tint rojo para el destello de daño. Reutiliza 'spritePicture' en los sprites sin tint (sin un segundo bitmap).
  , spriteWidth :: Float
  , spriteHeight :: Float
  }

-- | Sprites opcionales. Una entrada ausente ('Nothing') cae al renderizado con color plano.
data SpriteCatalog = SpriteCatalog
  { scBackgroundGrasslands :: Maybe Sprite
  , scBackgroundCastle :: Maybe Sprite
  , scPlayerIdle :: Maybe Sprite
  , scPlayerJump :: Maybe Sprite
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
  , scExitDoorTop :: Maybe Sprite
  , scExitDoorMid :: Maybe Sprite
  , scSnailIdle :: Maybe Sprite
  , scSnailWalk :: Maybe Sprite
  , scBatIdle :: Maybe Sprite
  , scBatFly :: Maybe Sprite
  , scGolemIdle :: Maybe Sprite
  , scGolemWalk :: Maybe Sprite
  , scArcherIdle :: Maybe Sprite
  , scBossGolem :: Maybe Sprite
  , scBossBat :: Maybe Sprite
  , scProjectileRock :: Maybe Sprite
  , scTileGrassCenter :: Maybe Sprite
  , scFallingHazard :: Maybe Sprite
  , scHudLife :: Maybe Sprite
  , scHudLifeX :: Maybe Sprite
  , scHudHeartFull :: Maybe Sprite
  , scHudHeartHalf :: Maybe Sprite
  , scHudHeartEmpty :: Maybe Sprite
  , scHudScoreGem :: Maybe Sprite
  , scHudAttackSword :: Maybe Sprite
  }

-- | Carga todos los sprites conocidos. Una falla de carga o parseo deja esa entrada en 'Nothing' en vez de abortar.
loadSpriteCatalog :: IO SpriteCatalog
loadSpriteCatalog =
  SpriteCatalog
    <$> loadSprite "assets/sprites/backgrounds/grasslands.bmp"
    <*> loadSprite "assets/sprites/backgrounds/castle.bmp"
    <*> loadHurtableSprite "assets/sprites/player/player-idle.bmp"
    <*> loadHurtableSprite "assets/sprites/player/player-jump.bmp"
    <*> loadWalkSprites
    <*> loadSprite "assets/sprites/pickups/gem-yellow.bmp"
    <*> loadSprite "assets/sprites/tiles/grass-left.bmp"
    <*> loadSprite "assets/sprites/tiles/grass-mid.bmp"
    <*> loadSprite "assets/sprites/tiles/grass-right.bmp"
    <*> loadSprite "assets/sprites/tiles/grass-half-left.bmp"
    <*> loadSprite "assets/sprites/tiles/grass-half-mid.bmp"
    <*> loadSprite "assets/sprites/tiles/grass-half-right.bmp"
    <*> loadSprite "assets/sprites/tiles/bridge.bmp"
    <*> loadSprite "assets/sprites/tiles/sign-exit.bmp"
    <*> loadSprite "assets/sprites/tiles/door-closed-top.bmp"
    <*> loadSprite "assets/sprites/tiles/door-closed-mid.bmp"
    <*> loadSprite "assets/sprites/enemies/snail-idle.bmp"
    <*> loadSprite "assets/sprites/enemies/snail-walk.bmp"
    <*> loadSprite "assets/sprites/enemies/bat-idle.bmp"
    <*> loadSprite "assets/sprites/enemies/bat-fly.bmp"
    <*> loadHurtableSprite "assets/sprites/enemies/golem-idle.bmp"
    <*> loadHurtableSprite "assets/sprites/enemies/golem-walk.bmp"
    <*> loadSprite "assets/sprites/enemies/archer-idle.bmp"
    <*> loadHurtableSprite "assets/sprites/bosses/boss-golem.bmp"
    <*> loadHurtableSprite "assets/sprites/bosses/boss-bat.bmp"
    <*> loadSprite "assets/sprites/projectiles/projectile-rock.bmp"
    <*> loadSprite "assets/sprites/tiles/grass-center.bmp"
    <*> loadSprite "assets/sprites/hazards/weight.bmp"
    <*> loadSprite "assets/sprites/ui/life-p1.bmp"
    <*> loadSprite "assets/sprites/ui/life-x.bmp"
    <*> loadSprite "assets/sprites/ui/heart-full.bmp"
    <*> loadSprite "assets/sprites/ui/heart-half.bmp"
    <*> loadSprite "assets/sprites/ui/heart-empty.bmp"
    <*> loadSprite "assets/sprites/ui/score-gem-yellow.bmp"
    <*> loadSprite "assets/sprites/ui/attack-sword.bmp"

loadWalkSprites :: IO [Sprite]
loadWalkSprites =
  catMaybes
    <$> traverse
      (\i -> loadHurtableSprite ("assets/sprites/player/player-walk-" <> pad2 i <> ".bmp"))
      [1 .. 11 :: Int]

loadSprite :: FilePath -> IO (Maybe Sprite)
loadSprite = loadSpriteWith Nothing

loadHurtableSprite :: FilePath -> IO (Maybe Sprite)
loadHurtableSprite = loadSpriteWith (Just tintRedBMP)

{- | Lee un BMP y deriva su tamaño del propio bitmap (sin dimensiones hardcodeadas).
Cuando se le pasa un tint, construye la variante de daño a partir del mismo BMP.
-}
loadSpriteWith :: Maybe (BMP -> BMP) -> FilePath -> IO (Maybe Sprite)
loadSpriteWith tintHurt relPath = do
  path <- getDataFileName relPath
  loaded <- try (readBMP path)
  case loaded of
    Left err -> spriteLoadFailure relPath err
    Right (Left bmpErr) -> do
      hPutStrLn stderr ("Warning: failed to parse sprite " <> relPath <> ": " <> show bmpErr)
      pure Nothing
    Right (Right bmp) ->
      let bitmapData = bitmapDataOfBMP bmp
          (width, height) = bitmapSize bitmapData
          normalPicture = bitmapOfBMP bmp
          hurtPicture = maybe normalPicture (\tint -> bitmapOfBMP (tint bmp)) tintHurt
       in pure
            ( Just
                Sprite
                  { spritePicture = normalPicture
                  , spriteHurtPicture = hurtPicture
                  , spriteWidth = fromIntegral width
                  , spriteHeight = fromIntegral height
                  }
            )

-- | Mezcla hacia rojo puro para el tint de daño: 0 = sin cambios, 1 = rojo plano.
tintStrength :: Float
tintStrength = 0.6

tintRedBMP :: BMP -> BMP
tintRedBMP bmp =
  let (width, height) = bmpDimensions bmp
   in packRGBA32ToBMP32 width height (tintRedRGBA (unpackBMPToRGBA32 bmp))

tintRedRGBA :: ByteString -> ByteString
tintRedRGBA = BS.pack . tintPixels . BS.unpack
 where
  tintPixels (r : g : b : a : rest) =
    tintChannel 255 r : tintChannel 0 g : tintChannel 0 b : a : tintPixels rest
  tintPixels remainder = remainder

tintChannel :: Word8 -> Word8 -> Word8
tintChannel target component =
  round
    ( fromIntegral component * (1 - tintStrength)
        + fromIntegral target * tintStrength
    )

spriteLoadFailure :: FilePath -> SomeException -> IO (Maybe Sprite)
spriteLoadFailure relPath err =
  case fromException err :: Maybe AsyncException of
    Just asyncErr -> throwIO asyncErr
    Nothing -> do
      hPutStrLn stderr ("Warning: failed to load sprite " <> relPath <> ": " <> show err)
      pure Nothing

{- | Elige el sprite del jugador según el estado visible y el contador de render. El
estado de daño deliberadamente no elige un sprite propio: la animación de caminar/idle sigue corriendo
y 'Adapters.Gloss.Rendering.renderPlayer' aplica el tint rojo, así que ningún frame se congela.
-}
playerSprite :: SpriteCatalog -> Int -> Player -> Maybe Sprite
playerSprite catalog renderFrame player
  | not (playerOnGround player) =
      scPlayerJump catalog <|> movementSprite
  | otherwise = movementSprite
 where
  movementSprite =
    if abs (velX (playerVel player)) > movingEpsilon
      then cyclingSprite renderFrame (scPlayerWalk catalog) <|> scPlayerIdle catalog
      else scPlayerIdle catalog

enemySprite :: SpriteCatalog -> Int -> Enemy -> Maybe Sprite
enemySprite catalog renderFrame enemy =
  case enemyKind enemy of
    SnailKind ->
      patrolSprite renderFrame (scSnailIdle catalog) (scSnailWalk catalog)
    BatKind ->
      patrolSprite renderFrame (scBatIdle catalog) (scBatFly catalog)
    GolemKind ->
      patrolSprite renderFrame (scGolemIdle catalog) (scGolemWalk catalog)
    ArcherKind -> scArcherIdle catalog <|> scGolemIdle catalog
    BossGolemKind -> scBossGolem catalog
    BossBatKind -> scBossBat catalog <|> scBatFly catalog
 where
  moving = enemyMoving enemy
  patrolSprite frame idle walk
    | moving = idleOrWalk frame idle walk
    | otherwise = idle

idleOrWalk :: Int -> Maybe Sprite -> Maybe Sprite -> Maybe Sprite
idleOrWalk frame idle walk
  | even (frame `div` enemyAnimationStride) = walk <|> idle
  | otherwise = idle <|> walk

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
