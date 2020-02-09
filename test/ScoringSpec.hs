module ScoringSpec (spec) where

import Test.Hspec

import Scoring

import React (
  Action (..)
  )

import Interaction (
  Interaction (..)
  )

import World (
  RewardsVector (..)
  )

import Agent (
  AgentID,
  Agent (..)
  )

mkAgent :: AgentID -> Agent
mkAgent i = Agent { name=i, generosity=0, selfishness=0, score=0 }

spec :: Spec
spec = do
  describe "calcScore" $ do
    let vec = RewardsVector 2 4 6 8
    it "correctly applies a RewardsVector to an Interaction" $ do
      shouldBe (calcScore vec 1 (Interaction 1 2 Cooperate Cooperate)) 2
      shouldBe (calcScore vec 1 (Interaction 1 2 Cooperate Defect)) 4
      shouldBe (calcScore vec 1 (Interaction 1 2 Defect Cooperate)) 6
      shouldBe (calcScore vec 1 (Interaction 1 2 Defect Defect)) 8
      shouldBe (calcScore vec 2 (Interaction 1 2 Cooperate Cooperate)) 2
      shouldBe (calcScore vec 2 (Interaction 1 2 Cooperate Defect)) 6
      shouldBe (calcScore vec 2 (Interaction 1 2 Defect Cooperate)) 4
      shouldBe (calcScore vec 2 (Interaction 1 2 Defect Defect)) 8
    it "returns 0 if the Interaction does not apply to the AgentID" $
      shouldBe (calcScore vec 1 (Interaction 2 3 Cooperate Cooperate)) 0

  describe "scoreAgents" $ do
    let vec = RewardsVector 2 4 6 8
        agents = map mkAgent [1..4]
        interactions = [ Interaction 1 2 Cooperate Defect
                       , Interaction 1 3 Defect    Defect
                       , Interaction 1 4 Cooperate Cooperate
                       , Interaction 2 3 Defect    Cooperate
                       , Interaction 2 4 Cooperate Defect
                       , Interaction 3 4 Cooperate Cooperate
                       ]
        scored = scoreAgents vec interactions agents

    it "adjusts the scores of Agents correctly given Interactions" $ do
      let [a1, a2, a3, a4] = scored
      shouldBe agents scored -- Only checks names
      shouldBe (score a1) (0 + 4 + 8 + 2)
      shouldBe (score a2) (0 + 6 + 6 + 4)
      shouldBe (score a3) (0 + 8 + 4 + 2)
      shouldBe (score a4) (0 + 2 + 6 + 2)
