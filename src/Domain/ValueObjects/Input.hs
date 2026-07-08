module Domain.ValueObjects.Input (
  Input (..),
  inputHorizontalSign,
  noInput,
)
where

import GHC.Generics (Generic)

-- | Intención del jugador para un frame. Izquierda/derecha se mantienen. Salto, ataque y lanzamiento son edge-triggered (verdaderos solo en el frame de la pulsación).
data Input = Input
  { inputLeft :: Bool
  , inputRight :: Bool
  , inputJump :: Bool
  , inputAttack :: Bool
  , inputThrow :: Bool
  }
  deriving (Eq, Show, Generic)

-- | Intención horizontal neta: -1 izquierda, 1 derecha, 0 cuando no hay tecla o se mantienen ambas.
inputHorizontalSign :: Input -> Float
inputHorizontalSign input = case (inputLeft input, inputRight input) of
  (True, False) -> -1
  (False, True) -> 1
  _ -> 0

noInput :: Input
noInput =
  Input
    { inputLeft = False
    , inputRight = False
    , inputJump = False
    , inputAttack = False
    , inputThrow = False
    }
