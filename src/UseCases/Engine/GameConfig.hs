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

data GameConfig = GameConfig
  { gcGravity :: Float
  -- ^ px/s², aceleración hacia abajo aplicada en cada frame
  , gcMoveSpeed :: Float
  -- ^ px/s, velocidad horizontal mientras se mantiene una dirección
  , gcJumpSpeed :: Float
  -- ^ px/s, velocidad hacia arriba aplicada al inicio de un salto
  , gcStartingLives :: Lives
  , gcMaxHealth :: Health
  , gcDeathMargin :: Float
  -- ^ px por debajo de la plataforma más baja. Caer más allá de esta línea mata al jugador
  , gcAttackDuration :: Frames
  , gcInvincibilityDuration :: Frames
  -- ^ i-frames tras un golpe por contacto, también se reusa como invencibilidad de respawn
  , gcContactDamage :: Damage
  , gcMeleeReach :: Float
  -- ^ px que el hitbox de melee se extiende frente al cuerpo del jugador
  , gcMeleeDamage :: Damage
  , gcEnemyHurtFlashDuration :: Frames
  , gcLevelCount :: LevelCount
  , gcThrowCooldown :: Frames
  , gcThrowLifetime :: Frames
  , gcThrowHorizontalSpeed :: Float
  -- ^ px/s, velocidad horizontal de un proyectil lanzado
  , gcThrowLiftSpeed :: Float
  -- ^ px/s, velocidad inicial hacia arriba de un proyectil lanzado
  , gcProjectileWidth :: Float
  -- ^ px, ancho del hitbox del proyectil
  , gcProjectileHeight :: Float
  -- ^ px, alto del hitbox del proyectil
  }
  deriving (Eq, Show, Generic)

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

configForLevelCatalog :: [a] -> GameConfig
configForLevelCatalog paths =
  defaultConfig{gcLevelCount = levelCount (length paths)}

physicsParamsFromConfig :: GameConfig -> PhysicsParams
physicsParamsFromConfig cfg =
  physicsParams
    (gcGravity cfg)
    (gcMoveSpeed cfg)
    (gcJumpSpeed cfg)

lifeParamsFromConfig :: GameConfig -> LifeParams
lifeParamsFromConfig cfg =
  lifeParams
    (gcMaxHealth cfg)
    (gcDeathMargin cfg)
    (gcInvincibilityDuration cfg)

combatParamsFromConfig :: GameConfig -> CombatParams
combatParamsFromConfig cfg =
  combatParams
    (gcAttackDuration cfg)
    (gcInvincibilityDuration cfg)
    (gcContactDamage cfg)
    (gcMeleeReach cfg)
    (gcMeleeDamage cfg)
    (gcEnemyHurtFlashDuration cfg)

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
