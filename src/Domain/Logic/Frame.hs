module Domain.Logic.Frame (
  FrameParams (..),
  PlayingFrame (..),
  FrameResult (..),
  advanceSimulationFrame,
)
where

import Domain.Logic.BossPhase (resolveBossPhases)
import Domain.Logic.Combat (resolveCombat)
import Domain.Logic.FallingHazards (resolveFallingHazards)
import Domain.Logic.LevelFlow (resolveFramePhase, resolvePlayingWin)
import Domain.Logic.Pickups (resolvePickups)
import Domain.Logic.PlayerLife (resolveHazardsAndDeath)
import Domain.Logic.Projectiles (resolveProjectiles)
import Domain.Logic.Step (advanceFrame)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.World (World)
import Domain.ValueObjects.CombatParams (CombatParams)
import Domain.ValueObjects.DeltaTime (DeltaTime)
import Domain.ValueObjects.Input (Input)
import Domain.ValueObjects.LevelCount (LevelCount)
import Domain.ValueObjects.LifeParams (LifeParams)
import Domain.ValueObjects.Lives (Lives)
import Domain.ValueObjects.PhysicsParams (PhysicsParams)
import Domain.ValueObjects.Score (Score)
import Domain.ValueObjects.ThrowParams (ThrowParams)
import GHC.Generics (Generic)

data FrameParams = FrameParams
  { fpPhysics :: PhysicsParams
  , fpLife :: LifeParams
  , fpCombat :: CombatParams
  , fpThrow :: ThrowParams
  }
  deriving (Eq, Show, Generic)

data PlayingFrame = PlayingFrame
  { pfWorld :: World
  , pfLives :: Lives
  , pfScore :: Score
  , pfLevelIndex :: Int
  }
  deriving (Eq, Show, Generic)

data FrameResult = FrameResult
  { frWorld :: World
  , frLives :: Lives
  , frScore :: Score
  , frPhase :: GamePhase
  }
  deriving (Eq, Show, Generic)

advanceSimulationFrame ::
  FrameParams ->
  LevelCount ->
  DeltaTime ->
  Input ->
  PlayingFrame ->
  FrameResult
advanceSimulationFrame fp levelCount dt input playing =
  let params = fpPhysics fp
      life = fpLife fp
      combat = fpCombat fp
      throwP = fpThrow fp
      livesBefore = pfLives playing
      scoreBefore = pfScore playing
      wBefore = pfWorld playing
      wAfterFrame = advanceFrame params life dt input wBefore
      wAfterCombat = resolveCombat combat input wAfterFrame
      wAfterProjectiles = resolveProjectiles throwP combat params dt input wAfterCombat
      wAfterHazards = resolveFallingHazards life combat dt wAfterProjectiles
      wAfterBoss = resolveBossPhases combat wBefore wAfterHazards
      (wAfterPickups, scoreDelta) = resolvePickups wAfterBoss
      scoreFinal = scoreBefore <> scoreDelta
      phaseFromWin =
        resolvePlayingWin (pfLevelIndex playing) levelCount scoreFinal wAfterPickups
      (wFinal, lives', phaseFromDeath) =
        resolveHazardsAndDeath life livesBefore Playing wAfterPickups
      phase' = resolveFramePhase livesBefore lives' phaseFromDeath phaseFromWin
   in FrameResult
        { frWorld = wFinal
        , frLives = lives'
        , frScore = scoreFinal
        , frPhase = phase'
        }
