module AgentSpec (spec) where

import Test.Hspec
import Agent

spec :: Spec
spec = do
  describe "agent equality" $
    it "tests only the name property" $ do
      let a1 = Agent { name = 1
                     , generosity = 0.1
                     , selfishness = 0.2
                     , score = 0.3
                     }
          a2 = Agent { name = 1
                     , generosity = 0.3
                     , selfishness = 0.4
                     , score = 0.5
                     }
          a3 = Agent { name = 3
                     , generosity = 0.3
                     , selfishness = 0.4
                     , score = 0.5
                     }
      shouldBe (a1 == a2) True
      shouldBe (a1 == a3) False
      shouldBe (a2 == a3) False
      shouldBe (a1 /= a2) False
      shouldBe (a1 /= a3) True
      shouldBe (a2 /= a3) True

  describe "copyAgentWithName" $
    it "copies an agent preserving all except a new name" $ do
      let agent = Agent { name=42, selfishness=0.2, generosity=0.3, score=42 }
          doppel  = copyAgentWithName agent 2712
      shouldBe (name doppel) 2712
      shouldBe (selfishness doppel) 0.2
      shouldBe (generosity doppel) 0.3
      shouldBe (score doppel) 42

  describe "copyAgentWithScore" $
    it "copies an agent preserving all except adding onto the score" $ do
      let agent = Agent { name=42, selfishness=0.1, generosity=0.2, score=42 }
          doppel  = copyAgentWithScoreAdjustment agent 2
      shouldBe (name doppel) 42
      shouldBe (selfishness doppel) 0.1
      shouldBe (generosity doppel) 0.2
      shouldBe (score doppel) 44
