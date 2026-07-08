{- | Catálogo de jefes: stats y guiones de fases por clase.

Los niveles seleccionan la clase en JSON (@\"bossGolem\"@, @\"bossBat\"@); el
comportamiento multi-fase vive aquí, no en datos de nivel.
-}
module Domain.Logic.BossCatalog (
  bossDefinitionForKind,
)
where

import Data.Maybe (fromMaybe)
import Domain.Logic.BehaviourCatalog (patrolHorizontal, reactiveFsm)
import Domain.Model.BossKindStats (
  BossBatStats (..),
  BossGolemStats (..),
  bossBatStats,
  bossGolemStats,
 )
import Domain.Model.BossPhase (
  BossDefinition (..),
  BossPhaseCondition (..),
  BossPhaseDef (..),
 )
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.Model.EntityBehaviour (BehaviourProgram)
import Domain.ValueObjects.Frames (frames)
import Domain.ValueObjects.HealthRatio (HealthRatio, healthRatio, maxHealthRatio)

bossDefinitionForKind :: EnemyKind -> Maybe BossDefinition
bossDefinitionForKind BossGolemKind = Just golemKingDefinition
bossDefinitionForKind BossBatKind = Just batLordDefinition
bossDefinitionForKind _ = Nothing

spawnPhase :: BehaviourProgram -> BossPhaseDef
spawnPhase prog = BossPhaseDef{phaseConditions = [], phaseProgram = prog}

healthPhase :: Float -> BehaviourProgram -> BossPhaseDef
healthPhase ratioLit prog =
  BossPhaseDef
    { phaseConditions = [HealthAtOrBelowRatio (ratioFromCatalog ratioLit)]
    , phaseProgram = prog
    }

{- | Convierte un literal de catálogo a 'HealthRatio'. Un literal fuera de (0, 1] cae a
'maxHealthRatio' (100 %), que dispararía la fase apenas el jefe aparece; por eso
'Domain.BossCatalogTest' verifica en CI que todo umbral de fase sea válido.
-}
ratioFromCatalog :: Float -> HealthRatio
ratioFromCatalog r = fromMaybe maxHealthRatio (healthRatio r)

-- | Golem King — tres fases por umbral de salud.
golemKingDefinition :: BossDefinition
golemKingDefinition =
  let s = bossGolemStats
   in BossDefinition
        { bossMaxHealth = bgsMaxHealth s
        , bossWidth = bgsWidth s
        , bossHeight = bgsHeight s
        , bossPhases =
            [ spawnPhase (patrolHorizontal 20 (frames 120))
            , healthPhase 0.66 (reactiveFsm 350 25 25 12)
            , healthPhase 0.33 (reactiveFsm 350 45 45 16)
            ]
        }

-- | Bat Lord — dos fases.
batLordDefinition :: BossDefinition
batLordDefinition =
  let s = bossBatStats
   in BossDefinition
        { bossMaxHealth = bbsMaxHealth s
        , bossWidth = bbsWidth s
        , bossHeight = bbsHeight s
        , bossPhases =
            [ spawnPhase (patrolHorizontal 40 (frames 60))
            , healthPhase 0.50 (reactiveFsm 120 60 60 10)
            ]
        }
