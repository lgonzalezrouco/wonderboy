{- | Parámetros de combate inyectados en el dominio puro cada frame.

Evita que @Domain.Logic.Combat@ importe @UseCases.GameMonad@:
'UpdateGame' construye este value object desde 'GameConfig'.
-}
module Domain.ValueObjects.CombatParams (
  CombatParams (..),
  combatParams,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Damage (Damage)
import Domain.ValueObjects.Frames (Frames)

-- | Constantes de melee, contacto e invencibilidad para un frame.
data CombatParams = CombatParams
  { cpAttackDuration :: Frames
  -- ^ Frames que el alcance de melee permanece activo.
  , cpInvincibilityDuration :: Frames
  -- ^ Frames de invencibilidad tras contacto enemigo o respawn.
  , cpContactDamage :: Damage
  -- ^ Salud restada por un frame de contacto enemigo.
  , cpMeleeReach :: Float
  -- ^ Extensión horizontal del alcance de melee (px lógicos).
  , cpMeleeDamage :: Damage
  -- ^ Salud restada a un enemigo por un melee que conecta.
  }
  deriving (Eq, Show, Generic)

-- | Construye 'CombatParams' desde componentes sueltos.
combatParams :: Frames -> Frames -> Damage -> Float -> Damage -> CombatParams
combatParams attack invincibility contact reach meleeDamage =
  CombatParams
    { cpAttackDuration = attack
    , cpInvincibilityDuration = invincibility
    , cpContactDamage = contact
    , cpMeleeReach = reach
    , cpMeleeDamage = meleeDamage
    }
