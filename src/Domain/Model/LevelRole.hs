{- | Rol de un nivel dentro de un run (concepto de dominio).

Distingue el propósito narrativo y de dificultad de cada slot del catálogo:
introductorio, desafío o jefe. Los generadores de nivel e intérpretes de
prompts lo usan para adaptar el contenido al contexto del run.
-}
module Domain.Model.LevelRole (
  LevelRole (..),
)
where

-- | Arquetipo de nivel según su posición en el run.
data LevelRole
  = -- | Primer nivel: layout simple, enemigos básicos, introduce mecánicas.
    IntroRole
  | -- | Segundo nivel: plataformas móviles, peligros, dificultad media.
    ChallengeRole
  | -- | Último nivel: incluye arena de jefe y un enemigo jefe.
    BossRole
  deriving (Eq, Show)
