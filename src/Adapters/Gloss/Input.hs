-- | Adaptador de entrada: eventos Gloss → 'KeyState' y 'Input' de dominio.
module Adapters.Gloss.Input (
  KeyState (..),
  noKeys,
  handleKeyEvent,
  buildInput,
)
where

import Graphics.Gloss.Interface.IO.Game (
  Event (..),
  Key (..),
  SpecialKey (KeyLeft, KeyRight, KeyUp),
 )
import Graphics.Gloss.Interface.IO.Game qualified as Gloss (KeyState (Down))

import Domain.ValueObjects.Input (Input (..))

-- | Teclas sostenidas relevantes para el juego (estado del adaptador, no dominio).
data KeyState = KeyState
  { leftHeld :: Bool
  , rightHeld :: Bool
  , aHeld :: Bool
  , dHeld :: Bool
  , upHeld :: Bool
  , wHeld :: Bool
  , spaceHeld :: Bool
  }
  deriving (Eq, Show)

-- | Ninguna tecla de juego sostenida.
noKeys :: KeyState
noKeys =
  KeyState
    { leftHeld = False
    , rightHeld = False
    , aHeld = False
    , dHeld = False
    , upHeld = False
    , wHeld = False
    , spaceHeld = False
    }

-- | Actualiza 'KeyState' ante un evento de tecla (KeyDown / KeyUp).
handleKeyEvent :: Event -> KeyState -> KeyState
handleKeyEvent (EventKey key state _ _) ks =
  let held = state == Gloss.Down
   in case key of
        Char 'a' -> ks{aHeld = held}
        Char 'd' -> ks{dHeld = held}
        Char 'w' -> ks{wHeld = held}
        Char ' ' -> ks{spaceHeld = held}
        SpecialKey KeyLeft -> ks{leftHeld = held}
        SpecialKey KeyRight -> ks{rightHeld = held}
        SpecialKey KeyUp -> ks{upHeld = held}
        _ -> ks
handleKeyEvent _ ks = ks

-- | Construye 'Input' de dominio y flags de salto/ataque previos (edge detection).
buildInput :: KeyState -> Bool -> Bool -> (Input, Bool, Bool)
buildInput ks prevJumpHeld prevAttackHeld =
  let jumpHeld = upHeld ks || wHeld ks
      attackHeld = spaceHeld ks
      input =
        Input
          { inputLeft = leftHeld ks || aHeld ks
          , inputRight = rightHeld ks || dHeld ks
          , inputJump = jumpHeld && not prevJumpHeld
          , inputAttack = attackHeld && not prevAttackHeld
          }
   in (input, jumpHeld, attackHeld)
