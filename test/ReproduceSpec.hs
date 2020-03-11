module ReproduceSpec (spec) where

import Test.Hspec
import Reproduce

import World (
  ReproductionAssumptions (..),
  basicReproAssumptions,
  deterministicGenEvent
  )

import Agent (
  Agent (..),
  copyAgentWithName
  )

spec :: Spec
spec = do
  describe "scoreToProbabilityDensity" $
    it "returns a probability for a given score" $
      shouldBe 0.45
               (scoreToProbabilityDensity basicReproAssumptions (8, 10) 9)

  describe "getRange" $
    it "returns the range of a list of numbers" $
      shouldBe (2, 9) . getRange $ [4, 6, 3, 8, 2, 4, 2, 5, 9, 3]


  describe "shouldItReproduce" $ do
    it "returns True if an Agent should reproduce" $
      shouldBe True =<< shouldItReproduce (deterministicGenEvent 0.4)
                                          basicReproAssumptions
                                          (8, 10)
                                          9

    it "returns False if an Agent should not reproduce" $
      shouldBe False =<< shouldItReproduce (deterministicGenEvent 0.5)
                                          basicReproAssumptions
                                          (8, 10)
                                          9


  describe "reproduce" $ do
    let agent = Agent { name = 42
                      , generosity = 0
                      , selfishness = 0
                      , score = 9
                      }

    it "returns just the Agent if it should not reproduce" $
      shouldBe ([agent], 43) =<< reproduce (deterministicGenEvent 0.5)
                                           basicReproAssumptions
                                           (8, 10)
                                           43
                                           agent

    it "returns the agent plus a clone with a new id if it should reproduce" $
      let child = copyAgentWithName agent 43
       in shouldBe ([agent, child], 44)
                   =<< reproduce (deterministicGenEvent 0.4)
                       basicReproAssumptions
                       (8, 10)
                       43
                       agent

  describe "reproduceAll" $ do
    let mkAgent (n, s) = Agent { name = n
                               , generosity = 0
                               , selfishness = 0
                               , score = s }
        scores = [19, 25, 14, 36] -- p ~= [0.23, 0.31, 0.17, 0.44]
        names  = [1, 2, 3, 4]
        agents = zipWith (curry mkAgent) names scores

    it "reproduces Agents according to probabilities" $ do
      newList <- reproduceAll (deterministicGenEvent 0.3)
                              basicReproAssumptions
                              agents
      shouldBe newList
               [ Agent { name=1, selfishness=0, generosity=0, score=19 }
               , Agent { name=2, selfishness=0, generosity=0, score=25 }
               , Agent { name=5, selfishness=0, generosity=0, score=25 }
               , Agent { name=3, selfishness=0, generosity=0, score=14 }
               , Agent { name=4, selfishness=0, generosity=0, score=36 }
               , Agent { name=6, selfishness=0, generosity=0, score=36 } ]
