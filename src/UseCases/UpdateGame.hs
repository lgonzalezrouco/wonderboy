{- | Orquestación del ciclo de actualización del juego.

'updateGame' es el punto de entrada para un frame de simulación: lee la
configuración con 'MonadReader', y eleva la transición pura de frame
('Domain.Logic.Step.advanceFrame') al estado del mundo con 'MonadState'.
'runFrames' corre @n@ frames seguidos reutilizando 'runGameM'.
-}
module UseCases.UpdateGame (
  -- * Ciclo de update
  updateGame,

  -- * Simulación de varios frames
  runFrames,
)
where

import Control.Monad.Reader (ask)
import Control.Monad.State (modify)

import Domain.Logic.Step (advanceFrame)
import Domain.Model.World (World)
import Domain.ValueObjects.DeltaTime (DeltaTime)
import Domain.ValueObjects.Input (Input)
import UseCases.GameMonad (
  GameConfig,
  GameError,
  GameM,
  physicsParamsFromConfig,
  runGameM,
 )

{- | Actualiza el estado del mundo para un frame dado.

Secuencia (dentro de 'GameM'):

  1. Lee 'GameConfig' con 'ask'.
  2. Eleva 'advanceFrame' con 'modify': behaviour step y luego física si @dt > 0@,
     o el mundo sin cambios si @dt = 0@ (política de frame congelado en un solo lugar).
-}
updateGame :: DeltaTime -> Input -> GameM ()
updateGame dt input = do
  cfg <- ask
  modify (advanceFrame (physicsParamsFromConfig cfg) dt input)

{- | Corre @n@ frames consecutivos con el mismo @dt@ e 'Input', o el primer error.

Bucle puro de "correr N frames" compartido por @app/Main.hs@ y los tests, para no
reimplementar la iteración sobre 'runGameM' en cada sitio. Con @n <= 0@ devuelve el
mundo sin tocarlo.
-}
runFrames :: GameConfig -> Int -> DeltaTime -> Input -> World -> Either GameError World
runFrames cfg n dt input = go n
 where
  go k w
    | k <= 0 = Right w
    | otherwise = do
        (_, w') <- runGameM cfg w (updateGame dt input)
        go (k - 1) w'
