-- | 'enemyDefBehaviourTuning' es salida del resolver, no input del JSON del autor.
module Domain.EnemyDefJsonTest where

import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (pack)
import Test.Tasty.HUnit (Assertion, (@?=))

import Domain.Model.LevelDefinition (EnemyDef (..))

unit_tuningDefaultsToNothing :: Assertion
unit_tuningDefaultsToNothing =
  (enemyDefBehaviourTuning <$> decoded) @?= Just Nothing
 where
  decoded =
    decode (pack "{\"id\":1,\"kind\":\"snail\",\"pos\":{\"x\":0,\"y\":0}}") ::
      Maybe EnemyDef
