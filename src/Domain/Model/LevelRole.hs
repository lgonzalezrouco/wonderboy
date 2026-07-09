module Domain.Model.LevelRole (
  LevelRole (..),
)
where

-- Los generadores de nivel e intérpretes de prompts lo usan para adaptar el contenido al contexto del run.
data LevelRole
  = IntroRole
  | ChallengeRole
  | BossRole
  deriving (Eq, Show)
