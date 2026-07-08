{- | Configuración global del juego (inmutable durante una partida).

Proyecciones hacia value objects del dominio para la lógica de física, vida y combate.
-}
module UseCases.Engine.GameConfig (
  GameConfig (..),
  defaultConfig,
  configForLevelCatalog,
  physicsParamsFromConfig,
  lifeParamsFromConfig,
  combatParamsFromConfig,
  throwParamsFromConfig,
)
where

import GHC.Generics (Generic)

import Domain.Model.World (defaultMaxHealth)
import Domain.ValueObjects.CombatParams (CombatParams (..), combatParams)
import Domain.ValueObjects.Damage (Damage, damage)
import Domain.ValueObjects.Frames (Frames, frames)
import Domain.ValueObjects.Health (Health)
import Domain.ValueObjects.LevelCount (LevelCount, levelCount)
import Domain.ValueObjects.LifeParams (LifeParams (..), lifeParams)
import Domain.ValueObjects.Lives (Lives, lives)
import Domain.ValueObjects.PhysicsParams (PhysicsParams, physicsParams)
import Domain.ValueObjects.ThrowParams (ThrowParams (..), throwParams)

{- | Todos los parámetros que no cambian frame a frame viven aquí:
el 'ReaderT' los pone a disposición de cualquier acción en 'GameM' vía 'ask'\/'asks'.
-}
data GameConfig = GameConfig
  { gcGravity :: Float
  , gcMoveSpeed :: Float
  , gcJumpSpeed :: Float
  , gcStartingLives :: Lives
  , gcMaxHealth :: Health
  , gcDeathMargin :: Float
  , gcAttackDuration :: Frames
  , gcInvincibilityDuration :: Frames
  , gcContactDamage :: Damage
  , gcMeleeReach :: Float
  , gcMeleeDamage :: Damage
  , gcEnemyHurtFlashDuration :: Frames
  , gcLevelCount :: LevelCount
  , gcThrowCooldown :: Frames
  , gcThrowLifetime :: Frames
  , gcThrowHorizontalSpeed :: Float
  , gcThrowLiftSpeed :: Float
  , gcProjectileWidth :: Float
  , gcProjectileHeight :: Float
  }
  deriving (Eq, Show, Generic)

-- | Configuración por defecto para pruebas y el demo de @app\/Main.hs@.
defaultConfig :: GameConfig
defaultConfig =
  GameConfig
    { gcGravity = 980.0
    , gcMoveSpeed = 200.0
    , gcJumpSpeed = 400.0
    , gcStartingLives = lives 3
    , gcMaxHealth = defaultMaxHealth
    , gcDeathMargin = 64.0
    , gcAttackDuration = frames 10
    , gcInvincibilityDuration = frames 60
    , gcContactDamage = damage 1
    , gcMeleeReach = 15.0
    , gcMeleeDamage = damage 1
    , gcEnemyHurtFlashDuration = frames 24
    , gcLevelCount = levelCount 3
    , gcThrowCooldown = frames 30
    , gcThrowLifetime = frames 120
    , gcThrowHorizontalSpeed = 280.0
    , gcThrowLiftSpeed = 320.0
    , gcProjectileWidth = 12.0
    , gcProjectileHeight = 12.0
    }

-- | Ajusta 'gcLevelCount' al tamaño del catálogo de niveles del run.
configForLevelCatalog :: [a] -> GameConfig
configForLevelCatalog paths =
  defaultConfig{gcLevelCount = levelCount (length paths)}

-- | Proyecta 'GameConfig' al value object puro usado por 'Domain.Logic.Step.step'.
physicsParamsFromConfig :: GameConfig -> PhysicsParams
physicsParamsFromConfig cfg =
  physicsParams
    (gcGravity cfg)
    (gcMoveSpeed cfg)
    (gcJumpSpeed cfg)

{- | Proyecta 'GameConfig' al value object puro usado por 'Domain.Logic.PlayerLife'.

Los frames de invencibilidad de respawn usan 'gcInvincibilityDuration', el __mismo__
campo que los de contacto (ver 'combatParamsFromConfig'): hoy comparten valor a propósito.
-}
lifeParamsFromConfig :: GameConfig -> LifeParams
lifeParamsFromConfig cfg =
  lifeParams
    (gcMaxHealth cfg)
    (gcDeathMargin cfg)
    (gcInvincibilityDuration cfg)

-- | Proyecta 'GameConfig' al value object puro usado por 'Domain.Logic.Combat'.
combatParamsFromConfig :: GameConfig -> CombatParams
combatParamsFromConfig cfg =
  combatParams
    (gcAttackDuration cfg)
    (gcInvincibilityDuration cfg)
    (gcContactDamage cfg)
    (gcMeleeReach cfg)
    (gcMeleeDamage cfg)
    (gcEnemyHurtFlashDuration cfg)

-- | Proyecta 'GameConfig' al value object puro usado por 'Domain.Logic.Projectiles'.
throwParamsFromConfig :: GameConfig -> ThrowParams
throwParamsFromConfig cfg =
  throwParams
    (gcThrowCooldown cfg)
    (gcThrowLifetime cfg)
    (gcThrowHorizontalSpeed cfg)
    (gcThrowLiftSpeed cfg)
    (gcProjectileWidth cfg)
    (gcProjectileHeight cfg)
    (gcMeleeDamage cfg)
