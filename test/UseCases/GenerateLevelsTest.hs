{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests de 'defaultProfiles' y 'generateCatalog' con 'LevelContentPort' mockeado
(puro vía 'Identity').
-}
module UseCases.GenerateLevelsTest where

import Data.Functor.Identity (Identity (..))

import Test.Tasty.HUnit (Assertion, (@?=))

import Domain.Model.LevelDefinition (
  LevelDefinition (..),
  RectDef (..),
 )
import Domain.ValueObjects.Position (position)
import UseCases.GenerateLevels (defaultProfiles, generateCatalog)
import UseCases.Ports.LevelContentPort (
  LevelContentPort (..),
  LevelProfile (..),
  LevelRole (BossRole, ChallengeRole, IntroRole),
 )

newtype Stub a = Stub {runStub :: a}
  deriving (Functor, Applicative, Monad) via Identity

cannedTable :: [(Int, LevelDefinition)]
cannedTable =
  [ (0, levelForIndex 0)
  , (1, levelForIndex 1)
  , (2, levelForIndex 2)
  ]

instance LevelContentPort Stub where
  generateLevel profile = Stub (lookup (profileIndex profile) cannedTable)
  resolveBehaviourHint _ _ = Stub Nothing

catalog :: [LevelProfile] -> [Maybe LevelDefinition]
catalog = runStub . generateCatalog

baseLevel :: LevelDefinition
baseLevel =
  LevelDefinition
    { levelMinScore = 0
    , levelSpawn = position 0 0
    , levelPlatforms = []
    , levelMovingPlatforms = []
    , levelEnemies = []
    , levelPickups = []
    , levelFallingHazards = []
    , levelCrumblingPlatforms = []
    , levelBossArena = Nothing
    , levelExit = RectDef{rectPos = position 0 0, rectWidth = 1, rectHeight = 1}
    }

levelForIndex :: Int -> LevelDefinition
levelForIndex idx = baseLevel{levelMinScore = idx}

{- | Roles del run estándar, inyectados a 'defaultProfiles' como lo hace el
bootstrap ('UseCases.RunLayout.layoutRoles').
-}
threeRoles :: [LevelRole]
threeRoles = [IntroRole, ChallengeRole, BossRole]

threeExamples :: [LevelDefinition]
threeExamples = map levelForIndex [0, 1, 2]

unit_defaultProfilesHasThreeLevels :: Assertion
unit_defaultProfilesHasThreeLevels =
  map (\p -> (profileIndex p, profileRole p)) (defaultProfiles Nothing threeRoles threeExamples)
    @?= [ (0, IntroRole)
        , (1, ChallengeRole)
        , (2, BossRole)
        ]

unit_defaultProfilesPropagatesTheme :: Assertion
unit_defaultProfilesPropagatesTheme =
  map profileTheme (defaultProfiles (Just "ice") threeRoles threeExamples)
    @?= [Just "ice", Just "ice", Just "ice"]

unit_defaultProfilesAttachesExamples :: Assertion
unit_defaultProfilesAttachesExamples =
  map profileExample (defaultProfiles Nothing threeRoles threeExamples)
    @?= [ Just (levelForIndex 0)
        , Just (levelForIndex 1)
        , Just (levelForIndex 2)
        ]

{- | Roles y ejemplos se recorren en lockstep: con menos ejemplos que roles salen
menos perfiles, sin perfiles fantasma con 'profileExample' vacío. No ocurre en
producción (cada slot trae su archivo), pero fija la totalidad de 'zipWith3'.
-}
unit_defaultProfilesPairsInLockstep :: Assertion
unit_defaultProfilesPairsInLockstep =
  length (defaultProfiles Nothing threeRoles [levelForIndex 0]) @?= 1

unit_generateCatalogResolvesEachProfile :: Assertion
unit_generateCatalogResolvesEachProfile =
  catalog (defaultProfiles Nothing threeRoles threeExamples)
    @?= [ Just (levelForIndex 0)
        , Just (levelForIndex 1)
        , Just (levelForIndex 2)
        ]

unit_unresolvedProfileStaysNothing :: Assertion
unit_unresolvedProfileStaysNothing =
  catalog profilesWithUnresolvedBoss
    @?= [ Just (levelForIndex 0)
        , Just (levelForIndex 1)
        , Nothing
        ]
 where
  profilesWithUnresolvedBoss :: [LevelProfile]
  profilesWithUnresolvedBoss =
    [ if profileRole p == BossRole then p{profileIndex = 99} else p
    | p <- defaultProfiles Nothing threeRoles threeExamples
    ]
