{-# LANGUAGE OverloadedStrings #-}

module Adapters.ResolverReplyTest where

import Data.Maybe (isNothing)
import Data.Text (Text)
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

import Adapters.Anthropic.Content (ResolverReply (..), extractJsonObject, resolvedFromReply)
import Domain.Model.LevelDefinition (
  BehaviourArchetype (ChaseArchetype),
  ResolvedBehaviour (..),
 )
import Domain.ValueObjects.Amplifier (identityAmplifier, unAmplifier)
import Domain.ValueObjects.BehaviourTuning (BehaviourTuning (..))
import Domain.ValueObjects.Multiplier (unMultiplier)

unit_clampsAndMapsArchetype :: Assertion
unit_clampsAndMapsArchetype =
  fmap rbArchetype (resolvedFromReply reply) @?= Just ChaseArchetype
 where
  reply = ResolverReply "chase" (Just 9.0) (Just 2.0) (Just 1.0)

unit_clampsSpeedToMax :: Assertion
unit_clampsSpeedToMax =
  fmap (unMultiplier . tuningSpeed . rbTuning) (resolvedFromReply reply) @?= Just 3.0
 where
  reply = ResolverReply "chase" (Just 9.0) Nothing Nothing

unit_missingNumbersDefaultToIdentity :: Assertion
unit_missingNumbersDefaultToIdentity =
  fmap (tuningReach . rbTuning) (resolvedFromReply reply) @?= Just identityAmplifier
 where
  reply = ResolverReply "guard" Nothing Nothing Nothing

unit_reachBelowOneClampsToBase :: Assertion
unit_reachBelowOneClampsToBase =
  fmap (unAmplifier . tuningReach . rbTuning) (resolvedFromReply reply) @?= Just 1.0
 where
  reply = ResolverReply "guard" Nothing (Just 0.3) Nothing

unit_toughnessBelowOneClampsToBase :: Assertion
unit_toughnessBelowOneClampsToBase =
  fmap (unAmplifier . tuningToughness . rbTuning) (resolvedFromReply reply) @?= Just 1.0
 where
  reply = ResolverReply "guard" Nothing Nothing (Just 0.4)

unit_unknownArchetypeIsNothing :: Assertion
unit_unknownArchetypeIsNothing =
  assertBool "arquetipo desconocido => Nothing" (isNothing (resolvedFromReply reply))
 where
  reply = ResolverReply "rampage" (Just 1.0) (Just 1.0) (Just 1.0)

unit_extractJsonObject_markdownFences :: Assertion
unit_extractJsonObject_markdownFences =
  extractJsonObject "```json\n{\"archetype\":\"chase\"}\n```"
    @?= Just ("{\"archetype\":\"chase\"}" :: Text)

unit_extractJsonObject_prose :: Assertion
unit_extractJsonObject_prose =
  extractJsonObject "Aquí está: {\"speed\":1.0} listo."
    @?= Just ("{\"speed\":1.0}" :: Text)

unit_extractJsonObject_noBraces :: Assertion
unit_extractJsonObject_noBraces =
  assertBool "sin llaves => Nothing" (isNothing (extractJsonObject "sin json aquí"))
