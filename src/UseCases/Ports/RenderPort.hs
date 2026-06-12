{- | Puerto de renderizado del mundo.

Abstrae la presentación visual de un snapshot 'World'. El adaptador concreto
(M8) traduce entidades y plataformas a primitivas Gloss sin que 'UseCases/'
importe tipos del framework.
-}
module UseCases.Ports.RenderPort (
  -- * Puerto
  RenderPort (..),
)
where

import Domain.Model.World (World)

-- | Capacidad de dibujar el estado runtime del juego en un frame.
class Monad m => RenderPort m where
  renderWorld :: World -> m ()
