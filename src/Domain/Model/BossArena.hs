{-# LANGUAGE OverloadedStrings #-}

{- | Arena de jefe: límites horizontales opcionales mientras el jefe vive.

Los bordes @left@ y @right@ son las aristas interiores jugables en X (la caja
del jugador no puede cruzarlas con el jefe vivo).
-}
module Domain.Model.BossArena (
  BossArena (..),
  BossArenaDef (..),
  mkBossArena,
)
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import GHC.Generics (Generic)

-- | Límites interiores jugables en X (runtime).
data BossArena = BossArena
  { bossArenaLeft :: Float
  , bossArenaRight :: Float
  }
  deriving (Eq, Show, Generic)

-- | Definición autoral en JSON del nivel.
data BossArenaDef = BossArenaDef
  { bossArenaDefLeft :: Float
  , bossArenaDefRight :: Float
  }
  deriving (Eq, Show, Generic)

instance FromJSON BossArenaDef where
  parseJSON = withObject "BossArena" $ \o ->
    BossArenaDef <$> o .: "left" <*> o .: "right"

instance ToJSON BossArenaDef where
  toJSON def =
    object
      [ "left" .= bossArenaDefLeft def
      , "right" .= bossArenaDefRight def
      ]

-- | Construye arena cuando @left < right@.
mkBossArena :: BossArenaDef -> Maybe BossArena
mkBossArena (BossArenaDef l r)
  | l < r = Just BossArena{bossArenaLeft = l, bossArenaRight = r}
  | otherwise = Nothing
