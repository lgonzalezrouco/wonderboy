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

-- | Constantes de melee, contacto e invencibilidad para un frame.
data CombatParams = CombatParams
  { cpAttackDuration :: Int
  -- ^ Frames que el alcance de melee permanece activo.
  , cpInvincibilityDuration :: Int
  -- ^ I-frames tras contacto enemigo o respawn.
  , cpContactDamage :: Int
  -- ^ Salud restada por un tick de contacto enemigo.
  , cpMeleeReach :: Float
  -- ^ Extensión horizontal del alcance de melee (px lógicos).
  }
  deriving (Eq, Show, Generic)

-- | Construye 'CombatParams' desde componentes sueltos.
combatParams :: Int -> Int -> Int -> Float -> CombatParams
combatParams attack invincibility contact reach =
  CombatParams
    { cpAttackDuration = attack
    , cpInvincibilityDuration = invincibility
    , cpContactDamage = contact
    , cpMeleeReach = reach
    }
