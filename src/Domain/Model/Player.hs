{- | Modelo del jugador dentro del mundo del juego.

El jugador es una __entidad__: a diferencia de un value object como 'Position',
tiene identidad a lo largo del tiempo. Su estado cambia frame a frame
(posición, velocidad, vida), pero sigue siendo "el mismo jugador".

Ver 'Domain.Model.Enemy' para comparar con la entidad enemigo.
-}
module Domain.Model.Player (
  -- * Tipo
  Player (..),

  -- * Hitbox
  playerWidth,
  playerHeight,
  playerAabb,

  -- * Construcción
  spawnPlayer,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomCenter)
import Domain.ValueObjects.Position (Position)
import Domain.ValueObjects.Velocity (Velocity, velocity)

{- | Estado del jugador en un frame dado.

__Por qué `data` y no `newtype`?__

`newtype` requiere exactamente un campo. 'Player' tiene cuatro: necesitamos `data`.

__Por qué record con campos nombrados?__

Con campos posicionales (`Player Position Velocity Bool Int`) cada uso requiere
conocer el orden y el lector no puede saber qué significa el segundo `Bool`.
Con campos nombrados:

  * El código es autoexplicativo: `playerOnGround p` en lugar de `thirdField p`.
  * GHC genera automáticamente un /selector/ (función de acceso) por campo.
  * Las actualizaciones son expresivas: `p { playerHealth = playerHealth p - 1 }`.

__Por qué `playerOnGround :: Bool`?__

Este flag lo necesita 'applyJump' en @Domain.Logic.Physics@ (M3):
el jugador sólo puede saltar si estaba apoyado al inicio del frame. Se establece en esta entidad
porque es parte del estado observable del juego, no un detalle de la física.

__Entidad vs value object__ (resumen):

  * 'Position', 'Velocity' — /value objects/: sin identidad, igualdad por valor.
  * 'Player', 'Enemy' — /entities/: tienen identidad conceptual ("este jugador")
    aunque en Haskell se representen como valores inmutables que se reemplazan cada frame.
-}
data Player = Player
  { playerPos :: Position
  -- ^ Posición actual del jugador en el espacio del juego (píxeles lógicos).
  , playerVel :: Velocity
  -- ^ Velocidad actual (px/s). La física actualiza este campo cada frame.
  , playerOnGround :: Bool
  -- ^ 'True' si el jugador está apoyado sobre una superficie.
  --   Controla si puede iniciar un salto (M3).
  , playerHealth :: Int
  -- ^ Puntos de vida. 0 = muerto. Decrece al recibir daño (M2+).
  }
  deriving (Eq, Show, Generic)

-- | Ancho del hitbox del jugador en píxeles lógicos (constante M3).
playerWidth :: Float
playerWidth = 32.0

-- | Alto del hitbox del jugador en píxeles lógicos (constante M3).
playerHeight :: Float
playerHeight = 48.0

-- | Caja de colisión del jugador: @playerPos@ es el centro inferior (pies).
playerAabb :: Player -> Aabb
playerAabb p =
  aabbFromBottomCenter (playerPos p) playerWidth playerHeight

{- | Crea un jugador en su posición de spawn, en reposo y con vida completa.

Smart constructor: establece los valores por defecto razonables de un jugador
recién aparecido en el nivel — velocidad cero, en el aire, vida máxima.

La posición de spawn varía por nivel; por eso se recibe como argumento en lugar
de usar una constante.
-}
spawnPlayer :: Int -> Position -> Player
spawnPlayer maxHealth pos =
  Player
    { playerPos = pos
    , playerVel = velocity 0 0 -- en reposo: vx=0, vy=0
    , playerOnGround = False -- empieza en el aire; la gravedad (M3) lo baja
    , playerHealth = maxHealth
    }
