{- | Parámetros de vida y muerte inyectados en el dominio puro cada frame.

Evita que @Domain.Logic.PlayerLife@ importe @UseCases.GameMonad@:
'UpdateGame' construye este value object desde 'GameConfig'.
-}
module Domain.ValueObjects.LifeParams (
  LifeParams (..),
  lifeParams,
)
where

import GHC.Generics (Generic)

-- | Constantes de vida para un frame (salud máxima, margen de caída).
data LifeParams = LifeParams
  { lpMaxHealth :: Int
  -- ^ Salud tras spawn o respawn.
  , lpDeathMargin :: Float
  -- ^ Píxeles bajo la plataforma más baja antes de out-of-bounds.
  }
  deriving (Eq, Show, Generic)

-- | Construye 'LifeParams' desde componentes sueltos.
lifeParams :: Int -> Float -> LifeParams
lifeParams health margin =
  LifeParams
    { lpMaxHealth = health
    , lpDeathMargin = margin
    }
