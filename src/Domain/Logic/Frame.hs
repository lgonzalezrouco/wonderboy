{- | Transición de un frame de simulación completo.

Orquesta behaviour, física, combate, proyectiles, peligros, fases de jefe,
pickups, victoria híbrida y muerte en un único lugar. La política de frame
congelado y fases terminales ('GameOver', 'LevelComplete', 'Victory') la aplica
el llamador ('UseCases.UpdateGame.updateGame').

== Pipeline (orden fijo) ==

1. 'Domain.Logic.Step.advanceFrame' — behaviour + física
2. 'Domain.Logic.Combat.resolveCombat'
3. 'Domain.Logic.Projectiles.resolveProjectiles'
4. 'Domain.Logic.FallingHazards.resolveFallingHazards'
5. 'Domain.Logic.BossPhase.resolveBossPhases' — usa snapshot @wBefore@
6. 'Domain.Logic.Pickups.resolvePickups'
7. 'Domain.Logic.LevelFlow.resolvePlayingWin' — puntuación tras pickups
8. 'Domain.Logic.PlayerLife.resolveHazardsAndDeath' — sobre mundo post-pickup
9. 'Domain.Logic.LevelFlow.resolveFramePhase' — muerte anula victoria en el mismo frame
-}
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

-- | Parámetros físicos y de reglas proyectados desde configuración (sin 'GameConfig').
data FrameParams = FrameParams
  { fpPhysics :: PhysicsParams
  , fpLife :: LifeParams
  , fpCombat :: CombatParams
  , fpThrow :: ThrowParams
  }
  deriving (Eq, Show, Generic)

-- | Estado de run necesario para un frame en 'Playing'.
data PlayingFrame = PlayingFrame
  { pfWorld :: World
  , pfLives :: Lives
  , pfScore :: Score
  , pfLevelIndex :: Int
  }
  deriving (Eq, Show, Generic)

-- | Resultado de un frame de simulación en 'Playing'.
data FrameResult = FrameResult
  { frWorld :: World
  , frLives :: Lives
  , frScore :: Score
  , frPhase :: GamePhase
  }
  deriving (Eq, Show, Generic)

{- | Avanza un frame de simulación completo.

Precondición: el llamador solo invoca en 'Playing' con @not (isFrozen dt)@.
No revalida frame congelado ni fase terminal.
-}
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
