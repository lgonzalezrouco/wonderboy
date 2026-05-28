{- | Orquestación del ciclo de actualización del juego.

'updateGame' es el punto de entrada para un frame de simulación.
Lee la configuración con 'MonadReader', modifica el estado del mundo con 'MonadState',
y no lanza errores (nada puede fallar con los modelos de M2).

Esta firma es __casi-final__: en Milestone 3 el body de 'updateGame' llamará a
@Domain.Logic.step@ (con gravedad y colisiones) sin necesidad de cambiar la firma.
-}
module UseCases.UpdateGame
  ( -- * Ciclo de update
    updateGame
  )
where

-- Grupo 2 — terceros (mtl)
import Control.Monad.Reader (asks)
import Control.Monad.State (modify)

-- Grupo 3 — proyecto
import Domain.Model.Player (Player (..))
import Domain.Model.World (advance, mapPlayer)
import Domain.ValueObjects.DeltaTime (DeltaTime)
import Domain.ValueObjects.Input (Input (..))
import Domain.ValueObjects.Velocity (velY, velocity)
import UseCases.GameMonad (GameM, gcMoveSpeed)

{- | Actualiza el estado del mundo para un frame dado.

Secuencia de operaciones (dentro de 'GameM'):

  1. Lee @gcMoveSpeed@ de 'GameConfig' con 'asks' (acceso de solo lectura).
  2. Aplica el input del jugador: fija la velocidad horizontal según las teclas.
  3. Integra la cinemática: @pos += vel * dt@ para todas las entidades (placeholder M2).

__Firma casi-final__: en M3 el body llamará a @Domain.Logic.step@ (que agrega
gravedad y colisiones AABB) sin modificar esta firma.

__Relación con las typeclasses mtl__: aunque la firma usa el tipo concreto 'GameM',
las operaciones internas (@asks@, @modify@) están definidas por 'MonadReader' y
'MonadState'. Esto mantiene el código desacoplado de la pila concreta.
-}
updateGame :: DeltaTime -> Input -> GameM ()
updateGame dt input = do
  speed <- asks gcMoveSpeed
  -- `asks gcMoveSpeed` extrae el campo `gcMoveSpeed` de `GameConfig`
  -- en una sola operación: equivale a `ask >>= \cfg -> pure (gcMoveSpeed cfg)`.
  modify (mapPlayer (applyInput speed input))
  -- `modify f` reemplaza el estado actual por `f estado`.
  -- `mapPlayer` enfoca la transformación sólo en el jugador (ver `Domain.Model.World`).
  modify (advance dt)

-- `do` secuencia las tres acciones monádicas en orden:
-- 1. lectura (no modifica estado), 2. ajuste de velocidad, 3. integración de posición.

{- | Traduce el 'Input' en una velocidad horizontal para el 'Player'.

Reglas:

  * sólo derecha   → vx = +speed
  * sólo izquierda → vx = -speed
  * ambas o ninguna → vx = 0

La componente vy se conserva sin cambios: la gravedad (M3) la modifica.
El salto (@inputJump@) se difiere a M3 porque requiere @playerOnGround@ y gravedad.

__Por qué aquí y no en @Domain/@?__

'applyInput' usa el parámetro @speed@ que proviene de 'GameConfig', accesible
en @UseCases/@. En M3, cuando la física integre configuración y estado, puede
consolidarse en @Domain.Logic.Physics@.
-}
applyInput :: Float -> Input -> Player -> Player
applyInput speed input p =
  p{playerVel = velocity vx' vy'}
  where
    -- Conservamos la componente vy actual; en M2 la gravedad no existe todavía.
    vy' = velY (playerVel p)
    -- `velY :: Velocity -> Float` extrae la componente vertical.
    vx' = case (inputLeft input, inputRight input) of
      (True, False) -> -speed -- ← izquierda
      (False, True) -> speed -- → derecha
      _ -> 0 -- quieto: ninguna tecla o ambas a la vez

-- `case (a, b) of` hace pattern matching exhaustivo sobre el par de booleanos.
-- `_` captura (False, False) y (True, True): en ambos casos el jugador se detiene.
