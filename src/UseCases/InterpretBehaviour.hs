{- | Wrapper monádico del intérprete de comportamiento de enemigos.

El núcleo puro vive en @Domain.Logic.RunBehaviour@.
-}
module UseCases.InterpretBehaviour (
  interpretBehaviourStepM,
)
where

import Control.Monad.State (modify)

import Domain.Logic.RunBehaviour (runBehaviourStep)
import Domain.ValueObjects.DeltaTime (DeltaTime, seconds)
import UseCases.GameMonad (GameM)

{- | Avanza un behaviour step en todos los enemigos si @dt > 0@.

Sólo corre si @dt > 0@ para alinear con identidad temporal de 'step'.
-}
interpretBehaviourStepM :: DeltaTime -> GameM ()
interpretBehaviourStepM dt
  | seconds dt == 0 = pure ()
  | otherwise = modify runBehaviourStep
