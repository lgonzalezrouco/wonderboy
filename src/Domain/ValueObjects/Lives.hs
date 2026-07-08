{- | Stock de vidas (continues) de la partida.

Value object con invariante: nunca negativo. El constructor de datos no se
exporta; se usan 'lives' / 'loseLife'. Es run-wide (persiste entre niveles).
-}
module Domain.ValueObjects.Lives (
  Lives,
  lives,
  livesCount,
  noLives,
  loseLife,
)
where

import GHC.Generics (Generic)

-- | Vidas restantes en la partida (>= 0).
newtype Lives = Lives Int
  deriving (Eq, Ord, Show, Generic)

-- | Construye 'Lives', saturando en 0.
lives :: Int -> Lives
lives n = Lives (max 0 n)

livesCount :: Lives -> Int
livesCount (Lives n) = n

-- | Sin vidas (fin de la partida).
noLives :: Lives
noLives = Lives 0

-- | Resta una vida, saturando en 0.
loseLife :: Lives -> Lives
loseLife (Lives n) = Lives (max 0 (n - 1))
