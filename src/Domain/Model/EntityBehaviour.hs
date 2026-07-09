module Domain.Model.EntityBehaviour (
  EntityAction (..),
  BehaviourProgram (BehaviourProgram),
  setVelocity,
  waitFrames,
  waitThen,
  idleProgram,
  ifPlayerWithinRange,
  ifNearSpawn,
  moveToward,
  moveTowardPlayer,
  moveTowardPlayer2D,
  moveTowardSpawn,
  moveTowardSpawn2D,
  shoot,
  facePlayer,
  setFacingTowardPlayer,
  (>>>),
  waitFramesRemaining,
)
where

import Control.Monad.Free (Free (..))
import GHC.Generics (Generic)

import Domain.ValueObjects.Frames (Frames, frameCount, hasFramesLeft)
import Domain.ValueObjects.Velocity (Velocity)

data EntityAction next
  = SetVelocity Velocity next
  | WaitFrames Frames next
  | IfPlayerWithinRange Float BehaviourProgram BehaviourProgram next
  | IfNearSpawn Float BehaviourProgram BehaviourProgram next
  | MoveTowardPlayer Float next
  | MoveTowardPlayer2D Float next
  | MoveTowardSpawn Float next
  | MoveTowardSpawn2D Float next
  | FacePlayer next
  | SetFacingTowardPlayer next
  | MoveToward Float next
  | Shoot next
  deriving (Functor, Show, Generic)

newtype BehaviourProgram = BehaviourProgram
  {unBehaviourProgram :: Free EntityAction ()}
  deriving (Generic)

-- Sin instancia de Eq a propósito: los programas Free pueden ser cíclicos (patrullas construidas con fix), así que la igualdad estructural puede diverger.

instance Show BehaviourProgram where
  show (BehaviourProgram prog) = case prog of
    Pure () -> "BehaviourProgram <done>"
    Free (SetVelocity _ _) -> "BehaviourProgram <setVelocity …>"
    Free (WaitFrames n _) -> "BehaviourProgram <waitFrames " ++ show (frameCount n) ++ ">"
    Free (IfPlayerWithinRange{}) -> "BehaviourProgram <ifPlayerWithinRange …>"
    Free (IfNearSpawn{}) -> "BehaviourProgram <ifNearSpawn …>"
    Free (MoveTowardPlayer _ _) -> "BehaviourProgram <moveTowardPlayer …>"
    Free (MoveTowardPlayer2D _ _) -> "BehaviourProgram <moveTowardPlayer2D …>"
    Free (MoveTowardSpawn _ _) -> "BehaviourProgram <moveTowardSpawn …>"
    Free (MoveTowardSpawn2D _ _) -> "BehaviourProgram <moveTowardSpawn2D …>"
    Free (FacePlayer _) -> "BehaviourProgram <facePlayer>"
    Free (SetFacingTowardPlayer _) -> "BehaviourProgram <setFacingTowardPlayer>"
    Free (MoveToward _ _) -> "BehaviourProgram <moveToward …>"
    Free (Shoot _) -> "BehaviourProgram <shoot>"

infixl 1 >>>

(>>>) :: BehaviourProgram -> BehaviourProgram -> BehaviourProgram
BehaviourProgram m >>> BehaviourProgram n = BehaviourProgram (m >> n)

idleProgram :: BehaviourProgram
idleProgram = BehaviourProgram (Pure ())

setVelocity :: Velocity -> BehaviourProgram
setVelocity vel =
  BehaviourProgram (Free (SetVelocity vel (Pure ())))

waitFrames :: Frames -> BehaviourProgram
waitFrames n
  | hasFramesLeft n = BehaviourProgram (Free (WaitFrames n (Pure ())))
  | otherwise = idleProgram

waitThen :: Frames -> BehaviourProgram -> BehaviourProgram
waitThen n prog
  | hasFramesLeft n = waitFrames n >>> prog
  | otherwise = prog

ifPlayerWithinRange ::
  Float ->
  BehaviourProgram ->
  BehaviourProgram ->
  BehaviourProgram
ifPlayerWithinRange range thenBranch elseBranch =
  BehaviourProgram (Free (IfPlayerWithinRange range thenBranch elseBranch (Pure ())))

ifNearSpawn :: Float -> BehaviourProgram -> BehaviourProgram -> BehaviourProgram
ifNearSpawn radius thenBranch elseBranch =
  BehaviourProgram (Free (IfNearSpawn radius thenBranch elseBranch (Pure ())))

moveToward :: Float -> BehaviourProgram
moveToward speed =
  BehaviourProgram (Free (MoveToward speed (Pure ())))

moveTowardPlayer :: Float -> BehaviourProgram
moveTowardPlayer speed =
  BehaviourProgram (Free (MoveTowardPlayer speed (Pure ())))

moveTowardPlayer2D :: Float -> BehaviourProgram
moveTowardPlayer2D speed =
  BehaviourProgram (Free (MoveTowardPlayer2D speed (Pure ())))

moveTowardSpawn :: Float -> BehaviourProgram
moveTowardSpawn speed =
  BehaviourProgram (Free (MoveTowardSpawn speed (Pure ())))

moveTowardSpawn2D :: Float -> BehaviourProgram
moveTowardSpawn2D speed =
  BehaviourProgram (Free (MoveTowardSpawn2D speed (Pure ())))

facePlayer :: BehaviourProgram
facePlayer = BehaviourProgram (Free (FacePlayer (Pure ())))

setFacingTowardPlayer :: BehaviourProgram
setFacingTowardPlayer =
  BehaviourProgram (Free (SetFacingTowardPlayer (Pure ())))

shoot :: BehaviourProgram
shoot = BehaviourProgram (Free (Shoot (Pure ())))

waitFramesRemaining :: BehaviourProgram -> Maybe Frames
waitFramesRemaining (BehaviourProgram prog) = case prog of
  Free (WaitFrames n _) -> Just n
  _ -> Nothing
