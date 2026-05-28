{-# LANGUAGE DerivingStrategies #-}

-- | Pila monádica central del motor.
--
-- 'GameM' combina estado mutable del juego ('MonadState'), configuración de
-- solo lectura ('MonadReader') y manejo de errores recuperables ('MonadError')
-- sobre una base pura ('Identity').
module UseCases.GameMonad
  ( GameConfig (..)
  , GameError (..)
  , GameState
  , GameM (..)
  , runGameM
  )
where

import Data.Functor.Identity (Identity, runIdentity)
import GHC.Generics (Generic)

import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, runStateT)

-- | Configuración global del juego (inmutable durante una partida).
-- Placeholder hasta Milestone 2.
data GameConfig = GameConfig
  deriving stock (Eq, Show, Generic)

-- | Errores recuperables del motor.
-- Placeholder hasta Milestone 2.
newtype GameError = GameError String
  deriving stock (Eq, Show, Generic)

-- | Estado mutable del juego.
-- Alias de @()@ hasta Milestone 2, donde será reemplazado por 'Domain.Model.World'.
type GameState = ()

-- | Mónada del motor: @ReaderT GameConfig (StateT GameState (ExceptT GameError Identity))@.
newtype GameM a = GameM
  { unGameM ::
      ReaderT GameConfig (StateT GameState (ExceptT GameError Identity)) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader GameConfig
    , MonadState GameState
    , MonadError GameError
    )

-- | Ejecuta una acción en 'GameM' y devuelve el resultado o un error.
runGameM
  :: GameConfig
  -> GameState
  -> GameM a
  -> Either GameError (a, GameState)
runGameM cfg st =
  runIdentity . runExceptT . flip runStateT st . flip runReaderT cfg . unGameM
