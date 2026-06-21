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
import Data.Word (Word8)
import System.IO (hPutStrLn, stderr)

import Codec.BMP (BMP, bmpDimensions, packRGBA32ToBMP32, readBMP, unpackBMPToRGBA32)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Graphics.Gloss.Data.Bitmap (bitmapDataOfBMP, bitmapOfBMP, bitmapSize)
import Graphics.Gloss.Data.Picture (Picture)

import Domain.Model.Enemy (Enemy (..))
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.Model.Player (Player (..))
import Domain.ValueObjects.Velocity (velX, velY)
import Paths_wonderboy_hs (getDataFileName)

-- | Bitmap cargado junto con su tamaño original en píxeles.
data Sprite = Sprite
  { spritePicture :: Picture
  -- ^ Bitmap en su color original.
  , spriteHurtPicture :: Picture
  -- ^ Variante teñida de rojo para el estado de daño. En sprites sin tinte
  --   coincide con 'spritePicture' (se comparte el mismo 'Picture', sin costo extra).
  , spriteWidth :: Float
  , spriteHeight :: Float
  }

-- | Catálogo de sprites opcionales. Ausencias caen al render de color.
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

-- | Carga todos los sprites conocidos. Los errores dejan entradas vacías.
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
    <*> loadSprite "assets/sprites/enemies/golem-idle.bmp"
    <*> loadSprite "assets/sprites/enemies/golem-walk.bmp"
    <*> loadSprite "assets/sprites/enemies/archer-idle.bmp"
    <*> loadSprite "assets/sprites/bosses/boss-golem.bmp"
    <*> loadSprite "assets/sprites/bosses/boss-bat.bmp"
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

-- | Carga un sprite sin variante de daño: la versión teñida reusa el bitmap normal.
loadSprite :: FilePath -> IO (Maybe Sprite)
loadSprite = loadSpriteWith Nothing

-- | Carga un sprite del jugador con su variante de daño teñida de rojo.
loadHurtableSprite :: FilePath -> IO (Maybe Sprite)
loadHurtableSprite = loadSpriteWith (Just tintRedBMP)

{- | Lee un BMP y deriva ancho/alto con 'bitmapSize' (sin dimensiones hardcodeadas).
La transformación opcional produce la variante de daño a partir del mismo BMP; si es
'Nothing', esa variante reusa el 'Picture' normal y no se construye un segundo bitmap.
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

-- | Intensidad de la mezcla hacia rojo puro (0 = sin cambio, 1 = rojo plano).
tintStrength :: Float
tintStrength = 0.6

{- | Tiñe de rojo un BMP preservando dimensiones y canal alfa: solo cambia los
canales de color, así la silueta y la transparencia del sprite quedan intactas.
-}
tintRedBMP :: BMP -> BMP
tintRedBMP bmp =
  let (width, height) = bmpDimensions bmp
   in packRGBA32ToBMP32 width height (tintRedRGBA (unpackBMPToRGBA32 bmp))

-- | Mezcla cada píxel RGBA hacia rojo puro, dejando intacto el alfa.
tintRedRGBA :: ByteString -> ByteString
tintRedRGBA = BS.pack . tintPixels . BS.unpack
 where
  tintPixels (r : g : b : a : rest) =
    tintChannel 255 r : tintChannel 0 g : tintChannel 0 b : a : tintPixels rest
  tintPixels remainder = remainder

-- | Acerca un canal de color hacia 'target' según 'tintStrength'.
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

{- | Sprite del jugador según estado visible y contador de render.

El estado de daño NO elige un sprite propio: durante la invencibilidad el jugador
sigue animándose con su sprite de movimiento normal (idle/caminar/salto, que cicla
con 'renderFrame'). El efecto de "recibió daño" lo aporta sólo el tinte rojo con
parpadeo en 'Adapters.Gloss.Rendering.renderPlayer'; así la animación no se congela
en un único frame mientras dura la invencibilidad.
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

-- | Sprite de enemigo por clase y movimiento visible.
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
