{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests del orquestador de generación de niveles ('defaultProfiles' y
'generateCatalog') con el puerto 'LevelGeneratorPort' __mockeado__.

Todo es __puro__: el stub del puerto ('Stub') deriva su maquinaria monádica vía
'Identity' con @DerivingVia@, así no entra 'IO' en los tests de @UseCases/@ (la
implementación real con HTTP vive en @Adapters/@). El stub responde desde una
tabla canned indexada por 'profileIndex' ('cannedTable'); un índice ausente de la
tabla modela "el generador no pudo producir este nivel" devolviendo 'Nothing', la
señal con la que @Frameworks/@ hace el fallback granular al @level{N}.json@.

Cada caso valida una pieza del contrato documentado en el plan de generación:

  1. 'defaultProfiles' arma 3 perfiles con índices 0/1/2 y roles Intro/Challenge/Boss.
  2. 'defaultProfiles' propaga el tema del usuario a los tres perfiles.
  3. 'generateCatalog' mapea cada perfil a su 'Just' nivel, en orden.
  4. un perfil que el stub no resuelve queda 'Nothing' sin afectar a los demás.
-}
module UseCases.GenerateLevelsTest where

-- Grupo 1 — stdlib / base
import Data.Functor.Identity (Identity (..))

-- Grupo 2 — third-party
import Test.Tasty.HUnit (Assertion, (@?=))

-- Grupo 3 — proyecto
import Domain.Model.LevelDefinition (
  LevelDefinition (..),
  RectDef (..),
 )
import Domain.ValueObjects.Position (position)
import UseCases.GenerateLevels (defaultProfiles, generateCatalog)
import UseCases.Ports.LevelGeneratorPort (
  LevelGeneratorPort (..),
  LevelProfile (..),
  LevelRole (BossRole, ChallengeRole, IntroRole),
 )

{- | Mónada de test que implementa el puerto con respuestas pregrabadas.

Es un 'newtype' sobre 'Identity' (vía @DerivingVia@): la instancia de
'LevelGeneratorPort' no es huérfana porque 'Stub' es local a este módulo, y al
correr en 'Identity' garantiza que la generación es totalmente pura.

@deriving ... via Identity@ requiere importar el __constructor__ @Identity (..)@;
sin el @(..)@ GHC tira GHC-10283 ("data constructor Identity not in scope").
-}
newtype Stub a = Stub {runStub :: a}
  deriving (Functor, Applicative, Monad) via Identity

{- | Tabla canned 'profileIndex' → nivel generado. Un índice que no figura acá
representa un perfil que el generador no pudo resolver; la instancia devuelve
'Nothing' en ese caso (la señal de fallback granular). Cada nivel se distingue por
@levelMinScore = profileIndex@, de modo que los tests pueden ubicar qué perfil
produjo cada slot del catálogo.
-}
cannedTable :: [(Int, LevelDefinition)]
cannedTable =
  [ (0, levelForIndex 0)
  , (1, levelForIndex 1)
  , (2, levelForIndex 2)
  ]

{- | El stub ignora el rol y el tema y genera por 'lookup' del 'profileIndex' en
la tabla canned; un índice ausente degrada a 'Nothing'.
-}
instance LevelGeneratorPort Stub where
  generateLevel profile = Stub (lookup (profileIndex profile) cannedTable)

-- | Corre el orquestador en la mónada pura del stub y extrae el resultado.
catalog :: [LevelProfile] -> [Maybe LevelDefinition]
catalog = runStub . generateCatalog

{- | 'LevelDefinition' base mínima y válida: todas las colecciones vacías. Evita
repetir el record gigante; cada nivel canned solo varía 'levelMinScore'.
-}
baseLevel :: LevelDefinition
baseLevel =
  LevelDefinition
    { levelMinScore = 0
    , levelSpawn = position 0 0
    , levelPlatforms = []
    , levelMovingPlatforms = []
    , levelEnemies = []
    , levelPickups = []
    , levelFallingHazards = []
    , levelCrumblingPlatforms = []
    , levelBossArena = Nothing
    , levelExit = RectDef{rectPos = position 0 0, rectWidth = 1, rectHeight = 1}
    }

{- | Nivel canned distinguible por índice: usa @levelMinScore = idx@ como marca
para verificar el orden de resolución sin construir geometría real.
-}
levelForIndex :: Int -> LevelDefinition
levelForIndex idx = baseLevel{levelMinScore = idx}

{- | Caso 1: 'defaultProfiles' arma los tres perfiles estándar de una partida con
índices 0/1/2 y roles Intro/Challenge/Boss en ese orden (la progresión fija de
dificultad).
-}
unit_defaultProfilesHasThreeLevels :: Assertion
unit_defaultProfilesHasThreeLevels =
  map (\p -> (profileIndex p, profileRole p)) (defaultProfiles Nothing)
    @?= [ (0, IntroRole)
        , (1, ChallengeRole)
        , (2, BossRole)
        ]

{- | Caso 2: 'defaultProfiles' propaga la directiva temática del usuario a los
tres perfiles, de modo que el tema incide en todo el catálogo, no solo en el
primer nivel.
-}
unit_defaultProfilesPropagatesTheme :: Assertion
unit_defaultProfilesPropagatesTheme =
  map profileTheme (defaultProfiles (Just "ice"))
    @?= [Just "ice", Just "ice", Just "ice"]

{- | Caso 3: con un stub que resuelve cada perfil a su nivel canned,
'generateCatalog' devuelve @[Just d0, Just d1, Just d2]@ en orden (cada nivel se
reconoce por su 'levelMinScore').
-}
unit_generateCatalogResolvesEachProfile :: Assertion
unit_generateCatalogResolvesEachProfile =
  catalog (defaultProfiles Nothing)
    @?= [ Just (levelForIndex 0)
        , Just (levelForIndex 1)
        , Just (levelForIndex 2)
        ]

{- | Caso 4: un perfil que el stub no resuelve queda 'Nothing' sin afectar a los
demás slots. Se toma el 'BossRole' (índice 2) y se le reescribe el 'profileIndex'
a un valor ausente de la tabla canned, de modo que la instancia degrade ese único
perfil a 'Nothing'. Verifica que el fallback granular tiene la señal correcta
__por slot__ y que un nivel no generado no contamina a los demás.
-}
unit_unresolvedProfileStaysNothing :: Assertion
unit_unresolvedProfileStaysNothing =
  catalog profilesWithUnresolvedBoss
    @?= [ Just (levelForIndex 0)
        , Just (levelForIndex 1)
        , Nothing
        ]
 where
  -- Reapunta el perfil del 'BossRole' a un índice que la tabla canned no tiene
  -- (99), forzando que la instancia devuelva 'Nothing' solo para ese slot.
  profilesWithUnresolvedBoss :: [LevelProfile]
  profilesWithUnresolvedBoss =
    [ if profileRole p == BossRole then p{profileIndex = 99} else p
    | p <- defaultProfiles Nothing
    ]
