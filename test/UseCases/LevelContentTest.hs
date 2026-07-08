{-# LANGUAGE OverloadedStrings #-}

module UseCases.LevelContentTest where

import Data.Text.IO qualified as TIO

import Domain.Model.BossArena (bossArenaLeft, bossArenaRight)
import Domain.Model.Enemy (Enemy, enemyHealth, enemyKind, enemyMaxHealth)
import Domain.Model.EnemyKind (EnemyKind (..), isBossKind)
import Domain.Model.World (World, worldBossArena, worldCrumblingPlatforms, worldEnemies, worldFallingHazards, worldMinScore)
import Domain.ValueObjects.Health (healthPoints)
import Domain.ValueObjects.Score (scorePoints)
import Paths_wonderboy_hs (getDataFileName)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import UseCases.GameMonad (GameError (..))
import UseCases.LoadLevel (loadLevelFromText)

loadLevelWorld :: FilePath -> IO World
loadLevelWorld rel = do
  path <- getDataFileName rel
  txt <- TIO.readFile path
  case loadLevelFromText txt of
    Left (GameError e) -> assertFailure (rel ++ " failed to build: " ++ e)
    Right w -> pure w

bossesOf :: World -> [Enemy]
bossesOf w = filter (isBossKind . enemyKind) (worldEnemies w)

unit_level1BuildsNoBoss :: Assertion
unit_level1BuildsNoBoss = do
  w <- loadLevelWorld "levels/level1.json"
  scorePoints (worldMinScore w) @?= 300
  assertBool "level 1 has no boss" (null (bossesOf w))

unit_level2BuildsNoBoss :: Assertion
unit_level2BuildsNoBoss = do
  w <- loadLevelWorld "levels/level2.json"
  scorePoints (worldMinScore w) @?= 500
  assertBool "level 2 has no boss" (null (bossesOf w))
  assertBool "level 2 has crumbling platforms" (not (null (worldCrumblingPlatforms w)))

unit_level3BuildsWithGolemKing :: Assertion
unit_level3BuildsWithGolemKing = do
  w <- loadLevelWorld "levels/level3.json"
  scorePoints (worldMinScore w) @?= 600
  case bossesOf w of
    [b] -> do
      enemyKind b @?= BossGolemKind
      healthPoints (enemyHealth b) @?= 20
      healthPoints (enemyMaxHealth b) @?= 20
    bs -> assertFailure ("expected exactly one boss, got " ++ show (length bs))
  assertBool "level 3 has falling hazards" (not (null (worldFallingHazards w)))
  case worldBossArena w of
    Just arena -> do
      bossArenaLeft arena @?= 3620
      bossArenaRight arena @?= 4040
    Nothing -> assertFailure "level 3 should define bossArena"
