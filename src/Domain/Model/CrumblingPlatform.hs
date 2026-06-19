{- | Plataforma estática que se desmorona tras el primer apoyo del jugador.

Permanece sólida durante la cuenta regresiva; luego cae y se elimina bajo la
línea de muerte del nivel.
-}
module Domain.Model.CrumblingPlatform (
  -- * Tipo
  CrumblingPlatformPhase (..),
  CrumblingPlatform (..),

  -- * Construcción
  mkCrumblingPlatform,
  spawnCrumblingPlatform,

  -- * Geometría
  crumbleCountdownFrames,
  crumbleFallSpeed,
  crumblingPlatformAabb,
  crumblingPlatformAsPlatform,
  crumblingPlatformIsAnchored,
  crumblingPlatformSolidForPlayer,
)
where

import GHC.Generics (Generic)

import Domain.Model.Platform (Platform, platform)
import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomLeft)
import Domain.ValueObjects.Frames (Frames, frames)
import Domain.ValueObjects.Position (Position)

-- | Fase de simulación de una plataforma que se desmorona.
data CrumblingPlatformPhase
  = -- | Sin contacto del jugador sobre el tramo superior.
    CrumbleIntact
  | -- | Cuenta regresiva tras el primer apoyo del jugador.
    CrumbleCountingDown Frames
  | -- | Cayendo; no sólida para el jugador.
    CrumbleFalling
  deriving (Eq, Show, Generic)

-- | Plataforma que se desmorona con estado de runtime por instancia.
data CrumblingPlatform = CrumblingPlatform
  { crumblingPlatformId :: Int
  , crumblingPlatformPos :: Position
  , crumblingPlatformWidth :: Float
  , crumblingPlatformHeight :: Float
  , crumblingPlatformPhase :: CrumblingPlatformPhase
  }
  deriving (Eq, Show, Generic)

-- | Ventana fija de cuenta regresiva tras el primer apoyo del jugador.
crumbleCountdownFrames :: Frames
crumbleCountdownFrames = frames 15

-- | Velocidad de caída tras la cuenta regresiva (px/s).
crumbleFallSpeed :: Float
crumbleFallSpeed = 200

-- | Crea una plataforma intacta en la posición autoral.
spawnCrumblingPlatform ::
  Int ->
  Position ->
  Float ->
  Float ->
  CrumblingPlatform
spawnCrumblingPlatform pid pos width height =
  CrumblingPlatform
    { crumblingPlatformId = pid
    , crumblingPlatformPos = pos
    , crumblingPlatformWidth = width
    , crumblingPlatformHeight = height
    , crumblingPlatformPhase = CrumbleIntact
    }

-- | Smart constructor con ids y dimensiones válidos.
mkCrumblingPlatform ::
  Int ->
  Position ->
  Float ->
  Float ->
  Maybe CrumblingPlatform
mkCrumblingPlatform pid pos width height
  | pid > 0 && width > 0 && height > 0 =
      Just (spawnCrumblingPlatform pid pos width height)
  | otherwise = Nothing

-- | Proyección a 'Platform' para colisión en la posición actual.
crumblingPlatformAsPlatform :: CrumblingPlatform -> Platform
crumblingPlatformAsPlatform cp =
  platform
    (crumblingPlatformPos cp)
    (crumblingPlatformWidth cp)
    (crumblingPlatformHeight cp)

-- | Caja de colisión (ancla bottom-left).
crumblingPlatformAabb :: CrumblingPlatform -> Aabb
crumblingPlatformAabb cp =
  aabbFromBottomLeft
    (crumblingPlatformPos cp)
    (crumblingPlatformWidth cp)
    (crumblingPlatformHeight cp)

-- | Sólida para el jugador en intacta y durante la cuenta regresiva.
crumblingPlatformSolidForPlayer :: CrumblingPlatform -> Bool
crumblingPlatformSolidForPlayer cp = case crumblingPlatformPhase cp of
  CrumbleIntact -> True
  CrumbleCountingDown _ -> True
  CrumbleFalling -> False

-- | 'True' mientras la posición anclada afecta geometría del nivel (línea de muerte).
crumblingPlatformIsAnchored :: CrumblingPlatform -> Bool
crumblingPlatformIsAnchored = crumblingPlatformSolidForPlayer
