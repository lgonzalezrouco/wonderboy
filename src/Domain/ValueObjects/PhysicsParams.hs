{- | Parámetros de física inyectados en el dominio puro cada frame.

Evita que @Domain.Logic@ importe @UseCases.GameMonad@: @UpdateGame@ construye
este value object desde 'GameConfig' y lo pasa a 'Domain.Logic.Step.step'.
-}
module Domain.ValueObjects.PhysicsParams (
  -- * Tipo
  PhysicsParams (..),

  -- * Construcción
  physicsParams,
)
where

import GHC.Generics (Generic)

{- | Constantes de simulación para un frame (gravedad, movimiento, salto).

Todos los valores están en unidades del juego: px/s, px/s².
-}
data PhysicsParams = PhysicsParams
  { ppGravity :: Float
  -- ^ Aceleración gravitatoria (px/s²), aplicada restando de vy cada frame.
  , ppMoveSpeed :: Float
  -- ^ Velocidad horizontal máxima con input (px/s).
  , ppJumpSpeed :: Float
  -- ^ Velocidad vertical inicial al saltar desde el suelo (px/s, hacia arriba).
  }
  deriving (Eq, Show, Generic)

physicsParams :: Float -> Float -> Float -> PhysicsParams
physicsParams g move jump =
  PhysicsParams
    { ppGravity = g
    , ppMoveSpeed = move
    , ppJumpSpeed = jump
    }
