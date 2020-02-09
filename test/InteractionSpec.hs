module InteractionSpec (spec) where

import Test.Hspec
import Interaction

import React (
  Action (..),
  simpleFactory
  )

import Agent (
  Agent (..),
  AgentID
  )

import World (
  World (..),
  RewardsVector (..),
  deterministicGenEvent
  )

mkAgent :: AgentID -> Agent
mkAgent n = Agent { name=n, generosity=0, selfishness=0, score=0 }
mkWorld :: Double -> World
mkWorld v = World { reproduction_multiplier=0
                  , rewards=RewardsVector 0 0 0 0
                  , mistake_rate=0
                  , initial_size=1
                  , iterations=1
                  , generator=deterministicGenEvent v
                  }
mkFaultyWorld v = World { reproduction_multiplier=0
                        , rewards=RewardsVector 0 0 0 0
                        , mistake_rate=0.25
                        , initial_size=1
                        , iterations=1
                        , generator=deterministicGenEvent v
                        }

spec :: Spec
spec = do
  let react = simpleFactory . mkWorld $ 0.1
      [a1, a2, a3, a4] = map mkAgent [1..4]
      hist = [ Interaction 1 2 Cooperate Cooperate
             , Interaction 1 3 Defect    Cooperate
             , Interaction 1 4 Cooperate Defect
             , Interaction 2 3 Defect    Defect
             , Interaction 2 4 Defect    Cooperate
             , Interaction 3 4 Cooperate Cooperate
             ]
  describe "findMyInteractions" $
    it "finds all interactions for a given AgentID" $ do
      shouldBe (findMyInteractions 1 hist)
               [ Interaction 1 2 Cooperate Cooperate
               , Interaction 1 3 Defect    Cooperate
               , Interaction 1 4 Cooperate Defect
               ]
      shouldBe (findMyInteractions 2 hist)
               [ Interaction 1 2 Cooperate Cooperate
               , Interaction 2 3 Defect    Defect
               , Interaction 2 4 Defect    Cooperate
               ]

  describe "findInteraction" $ do
    it "finds a the first interaction for a pair of agents" $ do
      shouldBe (findInteraction 1 3 hist)
               (Just (Interaction 1 3 Defect Cooperate))
      shouldBe (findInteraction 3 2 hist)
               (Just (Interaction 2 3 Defect Defect))
    it "returns Nothing if no interaction exists" $
      shouldBe (findInteraction 1 5 hist) Nothing

  describe "interactionFactory" $ do
    it "generates an Interactor based on the interaction history" $ do
      let interact = interactionFactory react hist
      -- Could do this with subsequences but prefer explicit
      r1_2 <- interact a1 a2
      r1_3 <- interact a1 a3
      r1_4 <- interact a1 a4
      r2_3 <- interact a2 a3
      r2_4 <- interact a2 a4
      r3_4 <- interact a3 a4

      shouldBe r1_2 (Interaction 1 2 Cooperate Cooperate)
      shouldBe r1_3 (Interaction 1 3 Cooperate Defect)
      shouldBe r1_4 (Interaction 1 4 Defect    Cooperate)
      shouldBe r2_3 (Interaction 2 3 Defect    Defect)
      shouldBe r2_4 (Interaction 2 4 Cooperate Defect)
      shouldBe r3_4 (Interaction 3 4 Cooperate Cooperate)

    it "is commutative over the Agents" $ do
      r1_4 <- interactionFactory react hist a1 a4
      r4_1 <- interactionFactory react hist a4 a1
      shouldBe r1_4 r4_1

  describe "interactAll" $
    it "interacts all agents together given a reactor" $ do
      res <- interactAll react hist [a1, a2, a3, a4]
      shouldBe
        res
        [ Interaction 1 2 Cooperate Cooperate
        , Interaction 1 3 Cooperate Defect
        , Interaction 1 4 Defect    Cooperate
        , Interaction 2 3 Defect    Defect
        , Interaction 2 4 Cooperate Defect
        , Interaction 3 4 Cooperate Cooperate
        ]
