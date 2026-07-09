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
  SpecialKey (KeyLeft, KeyRight, KeySpace, KeyUp),
 )
import Graphics.Gloss.Interface.IO.Game qualified as Gloss (KeyState (Down))

import Domain.ValueObjects.Input (Input (..))

data KeyState = KeyState
  { leftHeld :: Bool
  , rightHeld :: Bool
  , aHeld :: Bool
  , dHeld :: Bool
  , upHeld :: Bool
  , wHeld :: Bool
  , spaceHeld :: Bool
  , xHeld :: Bool
  }
  deriving (Eq, Show)

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
    , xHeld = False
    }

handleKeyEvent :: Event -> KeyState -> KeyState
handleKeyEvent (EventKey key state _ _) ks =
  let held = state == Gloss.Down
   in case key of
        Char 'a' -> ks{aHeld = held}
        Char 'd' -> ks{dHeld = held}
        Char 'w' -> ks{wHeld = held}
        Char ' ' -> ks{spaceHeld = held}
        Char 'x' -> ks{xHeld = held}
        Char 'X' -> ks{xHeld = held}
        SpecialKey KeySpace -> ks{spaceHeld = held}
        SpecialKey KeyLeft -> ks{leftHeld = held}
        SpecialKey KeyRight -> ks{rightHeld = held}
        SpecialKey KeyUp -> ks{upHeld = held}
        _ -> ks
handleKeyEvent _ ks = ks

{- | Construye el 'Input' del dominio junto con los flags de tecla mantenida actuales de salto/ataque/tiro.
El llamador los realimenta en el frame siguiente para que cada acción dispare una vez en el flanco de tecla presionada, no en cada frame.
-}
buildInput :: KeyState -> Bool -> Bool -> Bool -> (Input, Bool, Bool, Bool)
buildInput ks prevJumpHeld prevAttackHeld prevThrowHeld =
  let jumpHeld = upHeld ks || wHeld ks
      attackHeld = spaceHeld ks
      throwHeld = xHeld ks
      input =
        Input
          { inputLeft = leftHeld ks || aHeld ks
          , inputRight = rightHeld ks || dHeld ks
          , inputJump = jumpHeld && not prevJumpHeld
          , inputAttack = attackHeld && not prevAttackHeld
          , inputThrow = throwHeld && not prevThrowHeld
          }
   in (input, jumpHeld, attackHeld, throwHeld)
