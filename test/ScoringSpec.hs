module ScoringSpec (spec) where

import Test.Hspec

import Scoring (
  Interaction (..),
  findMyInteractions,
  findInteraction,
  score,
  scoreAgentInteractions
  )

import Interaction (
  Action (..)
  )

import World (
  RewardsVector (..)
  )

spec :: Spec
spec = do
  describe "findMyInteractions" $
    it "finds all interactions for a given AgentID" $ do
      let as = [ Interaction 1 2 Cooperate Cooperate
               , Interaction 2 3 Cooperate Cooperate
               , Interaction 5 4 Cooperate Cooperate
               , Interaction 5 1 Cooperate Cooperate
               ]
      shouldBe (findMyInteractions 1 as)
               [ Interaction 1 2 Cooperate Cooperate
               , Interaction 5 1 Cooperate Cooperate
               ]
      shouldBe (findMyInteractions 2 as)
               [ Interaction 1 2 Cooperate Cooperate
               , Interaction 2 3 Cooperate Cooperate
               ]

  describe "findInteraction" $ do
    let as = [ Interaction 1 2 Cooperate Cooperate
           , Interaction 2 3 Cooperate Cooperate
           , Interaction 5 4 Cooperate Cooperate
           , Interaction 5 1 Cooperate Cooperate
           ]
    it "finds a the first interaction for a pair of agents" $ do
      shouldBe (findInteraction 1 5 as)
               (Just (Interaction 5 1 Cooperate Cooperate))
      shouldBe (findInteraction 5 1 as)
               (Just (Interaction 5 1 Cooperate Cooperate))
    it "returns Nothing if no interaction exists" $
      shouldBe (findInteraction 1 4 as) Nothing

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
