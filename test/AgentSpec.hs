module AgentSpec (spec) where

import Test.Hspec
import Agent (Agent(..), simple_reactor)
import World (Action (Cooperate, Defect))

spec :: Spec
spec = do
  describe "simple_reactor" $ do
    let basic_agent = Agent {react=simple_reactor, niceness=0.0, selfishness=0.0 }
    it "always cooperates on the first move" $ do
      simple_reactor Nothing basic_agent `shouldBe` Cooperate

    it "responds to a defection with a defection" $ do
      simple_reactor (Just Defect) basic_agent `shouldBe` Defect

    it "responds to a cooperation with a cooperation" $ do
      simple_reactor (Just Cooperate) basic_agent `shouldBe` Cooperate
