{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests del orquestador 'resolveLevelBehaviours' (M28) con el puerto
'BehaviourResolverPort' __mockeado__.

Todo es __puro__: el stub del puerto ('Stub') deriva su maquinaria monádica vía
'Identity' con @DerivingVia@, así no entra 'IO' en los tests de @UseCases/@. El
stub responde desde una tabla canned ('cannedTable'); un hint ausente de la tabla
modela "el resolver no pudo decidir" devolviendo 'Nothing'.

Cada caso valida una rama de la precedencia documentada en M28:

  1. hint conocido sin preset  → se rellena el preset con el arquetipo resuelto.
  2. preset explícito + hint   → el preset queda intacto (no se pisa).
  3. sin hint ni preset        → queda 'Nothing'.
  4. hint desconocido          → el resolver da 'Nothing' → preset 'Nothing'.
  5. dos enemigos (mismo par)  → ambos quedan resueltos al mismo arquetipo.
-}
module UseCases.ResolveBehavioursTest where

-- Grupo 1 — stdlib / base
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)

-- Grupo 2 — third-party
import Test.Tasty.HUnit (Assertion, (@?=))

-- Grupo 3 — proyecto
import Domain.Model.EnemyKind (EnemyKind (SnailKind))
import Domain.Model.LevelDefinition (
  BehaviourArchetype (ChaseArchetype, GuardArchetype, PatrolArchetype),
  EnemyDef (..),
  LevelDefinition (..),
  RectDef (..),
 )
import Domain.ValueObjects.Position (position)
import UseCases.Ports.BehaviourResolverPort (BehaviourResolverPort (..))
import UseCases.ResolveBehaviours (resolveLevelBehaviours)

{- | Mónada de test que implementa el puerto con respuestas pregrabadas.

Es un 'newtype' sobre 'Identity' (vía @DerivingVia@): la instancia de
'BehaviourResolverPort' no es huérfana porque 'Stub' es local a este módulo, y al
correr en 'Identity' garantiza que la resolución es totalmente pura.
-}
newtype Stub a = Stub {runStub :: a}
  deriving (Functor, Applicative, Monad) via Identity

{- | Tabla canned hint → arquetipo. Un hint que no figura acá representa una pista
que el resolver no reconoce; la instancia devuelve 'Nothing' en ese caso.
-}
cannedTable :: [(Text, BehaviourArchetype)]
cannedTable =
  [ ("hunts the player", ChaseArchetype)
  , ("guards the gate", GuardArchetype)
  ]

-- | El stub ignora el 'EnemyKind' y resuelve por 'lookup' en la tabla canned.
instance BehaviourResolverPort Stub where
  resolveBehaviourHint _ hint = Stub (lookup hint cannedTable)

-- | Corre el orquestador en la mónada pura del stub y extrae el resultado.
resolve :: LevelDefinition -> LevelDefinition
resolve = runStub . resolveLevelBehaviours

{- | 'LevelDefinition' base mínima y válida: todas las colecciones vacías salvo
@levelEnemies@, que cada test sobreescribe. Evita repetir el record gigante.
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

{- | Constructor de enemigo de prueba: clase fija ('SnailKind') y posición fija;
solo varían el id, el preset explícito y la pista textual.
-}
mkEnemy :: Int -> Maybe BehaviourArchetype -> Maybe Text -> EnemyDef
mkEnemy eid preset hint =
  EnemyDef
    { enemyDefId = eid
    , enemyDefKind = SnailKind
    , enemyDefPos = position 0 0
    , enemyDefBehaviourPreset = preset
    , enemyDefBehaviourHint = hint
    }

-- | Nivel con la lista de enemigos dada, resuelto; devuelve los enemigos finales.
resolvedEnemies :: [EnemyDef] -> [EnemyDef]
resolvedEnemies enemies = levelEnemies (resolve baseLevel{levelEnemies = enemies})

-- | Preset del primer enemigo de la lista resuelta (los tests de un enemigo).
firstPreset :: [EnemyDef] -> Maybe BehaviourArchetype
firstPreset enemies = case resolvedEnemies enemies of
  e : _ -> enemyDefBehaviourPreset e
  [] -> Nothing

{- | Caso 1: enemigo con hint conocido y sin preset → el preset queda seteado al
arquetipo que devuelve el puerto.
-}
unit_resolvesKnownHintFillsPreset :: Assertion
unit_resolvesKnownHintFillsPreset =
  firstPreset [mkEnemy 1 Nothing (Just "hunts the player")]
    @?= Just ChaseArchetype

{- | Caso 2: un preset explícito tiene precedencia y __no__ se pisa, aun cuando el
hint resolvería a otro arquetipo (el resolver ni se consulta para este enemigo).
-}
unit_explicitPresetTakesPrecedence :: Assertion
unit_explicitPresetTakesPrecedence =
  firstPreset [mkEnemy 1 (Just PatrolArchetype) (Just "hunts the player")]
    @?= Just PatrolArchetype

{- | Caso 3: sin preset y sin hint → no hay nada que resolver, el preset queda
'Nothing' (el build aplicará el default del kind).
-}
unit_noHintNoPresetStaysNothing :: Assertion
unit_noHintNoPresetStaysNothing =
  firstPreset [mkEnemy 1 Nothing Nothing] @?= Nothing

{- | Caso 4: hint desconocido (ausente de la tabla canned) → el resolver devuelve
'Nothing' → el preset queda 'Nothing'.
-}
unit_unknownHintStaysNothing :: Assertion
unit_unknownHintStaysNothing =
  firstPreset [mkEnemy 1 Nothing (Just "blah blah desconocido")] @?= Nothing

{- | Caso 5: dos enemigos con el mismo par @(kind, hint)@ → ambos quedan resueltos
al mismo arquetipo (la dedup interna no debe perder ninguna asignación).
-}
unit_sameKindHintResolvesAllEnemies :: Assertion
unit_sameKindHintResolvesAllEnemies =
  map
    enemyDefBehaviourPreset
    ( resolvedEnemies
        [ mkEnemy 1 Nothing (Just "guards the gate")
        , mkEnemy 2 Nothing (Just "guards the gate")
        ]
    )
    @?= [Just GuardArchetype, Just GuardArchetype]
