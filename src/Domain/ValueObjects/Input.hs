{- | Intención del jugador durante un frame.

Captura la intención del jugador en un frame: izquierda/derecha sostenidas;
salto solo en el frame del press (edge), no mientras se mantiene la tecla.
El adaptador de entrada (M7) convierte eventos del SO en este valor.

Ver 'noInput' para el valor neutro (ninguna acción activa).
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

{- | Conjunto de acciones activas del jugador en un frame.

__Por qué un record de booleanos y no un ADT?__

Una alternativa habitual es:

@
data Action = MoveLeft | MoveRight | Jump
type Input  = [Action]           -- o Set Action
@

Esa representación tiene una ventaja: es imposible tener `MoveLeft` y
`MoveRight` a la vez (si se modelan como mutuamente excluyentes).
Sin embargo, en la práctica los teclados físicos envían ambas teclas cuando
el jugador las pulsa al mismo tiempo, y la lógica de física en
@Domain.Logic.Physics@ (M3) ya resuelve esa ambigüedad (velocidad neta = 0).

El record de booleanos tiene dos ventajas prácticas aquí:

  1. __Pattern matching directo__ en la física:
     @if inputLeft input then ... else ...@
     sin necesidad de buscar en una lista.
  2. __Costo constante__ de acceso: O(1) vs O(n) para listas/sets pequeños.

'Generic' se deriva por completitud (ver 'Position' para la justificación).
-}
data Input = Input
  { inputLeft :: Bool
  -- ^ 'True' si el jugador mantiene pulsado "mover izquierda" este frame.
  , inputRight :: Bool
  -- ^ 'True' si el jugador mantiene pulsado "mover derecha" este frame.
  , inputJump :: Bool
  -- ^ 'True' solo en el frame en que se presiona saltar (no mientras se sostiene).
  --   El salto efectivo depende de `playerOnGround` (ver M3).
  , inputAttack :: Bool
  -- ^ 'True' solo en el frame en que se presiona atacar (no mientras se sostiene).
  , inputThrow :: Bool
  -- ^ 'True' solo en el frame en que se presiona lanzar (no mientras se sostiene).
  }
  deriving (Eq, Show, Generic)

-- `data` en lugar de `newtype` porque tenemos tres campos independientes.
-- `deriving` sin estrategia explícita: GHC deduce `stock` para `Eq`/`Show`/`Generic`.

{- | Signo de la intención horizontal del frame: @-1@ izquierda, @1@ derecha, @0@ sin
intención neta (ninguna o ambas teclas).

Única lectura del par @(inputLeft, inputRight)@: la física la escala por la velocidad
de movimiento y el combate la usa para orientar el facing, en vez de repetir el análisis
del par en cada sitio.
-}
inputHorizontalSign :: Input -> Float
inputHorizontalSign input = case (inputLeft input, inputRight input) of
  (True, False) -> -1
  (False, True) -> 1
  _ -> 0

{- | Frame sin ninguna acción activa.

Útil como valor inicial y en tests:

@
step params (deltaTime 0) noInput world === world   -- ver @test/Domain/StepTest.hs@
@

También es el input por defecto cuando el adaptador de entrada (M7)
aún no ha recibido eventos del sistema operativo.
-}
noInput :: Input
noInput =
  Input
    { inputLeft = False
    , inputRight = False
    , inputJump = False
    , inputAttack = False
    , inputThrow = False
    }

-- `Input{...}` es notación de record para construir un valor nombrando los campos.
-- Preferimos esta forma sobre `Input False False False` porque:
--   * El código es autoexplicativo aunque se reordenen los campos.
--   * Si en el futuro se agrega un campo (p. ej. `inputCrouch`),
--     el compilador nos avisará de que `noInput` debe actualizarse.
