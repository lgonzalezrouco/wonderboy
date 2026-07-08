{- | Intención del jugador durante un frame: izquierda/derecha sostenidas; salto,
ataque y lanzar solo en el frame del press (edge), no mientras se mantiene la
tecla. Ver 'noInput' para el valor neutro.
-}
module Domain.ValueObjects.Input (
  -- * Tipo
  Input (..),

  -- * Intención derivada
  inputHorizontalSign,

  -- * Valor neutro
  noInput,
)
where

import GHC.Generics (Generic)

{- | Conjunto de acciones activas del jugador en un frame. Es un record de
booleanos y no un ADT de acciones porque los teclados envían izquierda+derecha a
la vez y la física resuelve la ambigüedad (velocidad neta = 0).
-}
data Input = Input
  { inputLeft :: Bool
  , inputRight :: Bool
  , inputJump :: Bool
  , inputAttack :: Bool
  , inputThrow :: Bool
  }
  deriving (Eq, Show, Generic)

{- | Signo de la intención horizontal del frame: @-1@ izquierda, @1@ derecha, @0@ sin
intención neta (ninguna o ambas teclas). Lectura única del par
@(inputLeft, inputRight)@ que comparten física y combate.
-}
inputHorizontalSign :: Input -> Float
inputHorizontalSign input = case (inputLeft input, inputRight input) of
  (True, False) -> -1
  (False, True) -> 1
  _ -> 0

-- | Frame sin ninguna acción activa; valor inicial y por defecto del adaptador de entrada.
noInput :: Input
noInput =
  Input
    { inputLeft = False
    , inputRight = False
    , inputJump = False
    , inputAttack = False
    , inputThrow = False
    }
