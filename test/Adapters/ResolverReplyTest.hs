{-# LANGUAGE OverloadedStrings #-}

{- | Tests del mapeo puro respuesta-del-modelo → 'ResolvedBehaviour': clampea los
factores, default-ea los ausentes a 1.0, y descarta arquetipos no reconocidos.
-}
module Adapters.ResolverReplyTest where

import Data.Maybe (isNothing)
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

import Adapters.BehaviourResolver (ResolverReply (..), resolvedFromReply)
import Domain.Model.LevelDefinition (
  BehaviourArchetype (ChaseArchetype),
  ResolvedBehaviour (..),
 )
import Domain.ValueObjects.BehaviourTuning (BehaviourTuning (..))
import Domain.ValueObjects.Multiplier (identityMultiplier, unMultiplier)

unit_clampsAndMapsArchetype :: Assertion
unit_clampsAndMapsArchetype =
  fmap rbArchetype (resolvedFromReply reply) @?= Just ChaseArchetype
 where
  reply = ResolverReply "chase" (Just 9.0) (Just 0.5) (Just 1.0)

unit_clampsSpeedToMax :: Assertion
unit_clampsSpeedToMax =
  fmap (unMultiplier . tuningSpeed . rbTuning) (resolvedFromReply reply) @?= Just 3.0
 where
  reply = ResolverReply "chase" (Just 9.0) Nothing Nothing

unit_missingNumbersDefaultToIdentity :: Assertion
unit_missingNumbersDefaultToIdentity =
  fmap (tuningReach . rbTuning) (resolvedFromReply reply) @?= Just identityMultiplier
 where
  reply = ResolverReply "guard" Nothing Nothing Nothing

unit_unknownArchetypeIsNothing :: Assertion
unit_unknownArchetypeIsNothing =
  assertBool "arquetipo desconocido => Nothing" (isNothing (resolvedFromReply reply))
 where
  reply = ResolverReply "rampage" (Just 1.0) (Just 1.0) (Just 1.0)
