module ScoringSpec (spec) where

import Test.Hspec

import Scoring (
  score,
  scoreAgentInteractions
  )

import React (
  Action (..)
  )

import Interaction (
  Interaction (..)
  )

import World (
  RewardsVector (..)
  )

spec :: Spec
spec = do
  describe "score" $ do
    let vec = RewardsVector 2 4 6 8
    it "correctly applies a RewardsVector to an Interaction" $ do
      shouldBe (score vec 1 (Interaction 1 2 Cooperate Cooperate)) 2
      shouldBe (score vec 1 (Interaction 1 2 Cooperate Defect)) 4
      shouldBe (score vec 1 (Interaction 1 2 Defect Cooperate)) 6
      shouldBe (score vec 1 (Interaction 1 2 Defect Defect)) 8
      shouldBe (score vec 2 (Interaction 1 2 Cooperate Cooperate)) 2
      shouldBe (score vec 2 (Interaction 1 2 Cooperate Defect)) 6
      shouldBe (score vec 2 (Interaction 1 2 Defect Cooperate)) 4
      shouldBe (score vec 2 (Interaction 1 2 Defect Defect)) 8
    it "returns 0 if the Interaction does not apply to the AgentID" $
      shouldBe (score vec 1 (Interaction 2 3 Cooperate Cooperate)) 0

  describe "scoreAgentInteractions" $ do
    let interactions = [ Interaction 1 2 Cooperate Defect
                       , Interaction 2 4 Defect Defect
                       , Interaction 1 3 Cooperate Cooperate
                       , Interaction 2 3 Defect Cooperate
                       , Interaction 4 1 Cooperate Cooperate
                       ]
        vec          = RewardsVector 2 4 6 8
        scorer       = scoreAgentInteractions vec interactions
    it "generates a score for an Agent over all interactions" $ do
      shouldBe (scorer 1) 8
      shouldBe (scorer 2) 20
      shouldBe (scorer 3) 6
      shouldBe (scorer 4) 10
