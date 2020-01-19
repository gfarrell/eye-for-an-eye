module AgentSpec (spec) where

import Test.Hspec
import Agent (
  Agent(..),
  simpleReactor,
  probabilisticFactory
  )

import World (
  Action (Cooperate, Defect),
  World (..),
  EventGenerator,
  deterministicGenEvent,
  RewardsVector(..)
  )

spec :: Spec
spec = do
  describe "simpleReactor" $ do
    let basic_agent = Agent {react=simpleReactor, niceness=0.0, selfishness=0.0 }
    it "always cooperates on the first move" $ do
      res <- simpleReactor Nothing basic_agent
      shouldBe res Cooperate

    it "responds to a defection with a defection" $ do
      res <- simpleReactor (Just Defect) basic_agent
      shouldBe res Defect

    it "responds to a cooperation with a cooperation" $ do
      res <- simpleReactor (Just Cooperate) basic_agent
      shouldBe res Cooperate

  describe "probabilistic reactions" $ do
    let mkAgent :: World -> Agent
        mkAgent w = Agent { niceness=0.25
                          , selfishness=0.25
                          , react=probabilisticFactory w
                          }
        mkWorld :: Double -> World
        mkWorld v = World { reproduction_multiplier=0
                          , rewards=RewardsVector 0 0 0 0
                          , mistake_rate=0
                          , initial_size=1
                          , iterations=1
                          , generator=deterministicGenEvent v
                          }

    it "cooperates on the first move, except when being selfish" $ do
      let vs = [0.2, 0.4, 0.6, 0.8] :: [Double]
          as = map (mkAgent . mkWorld) vs
      rs <- mapM (\a -> react a Nothing a) as
      shouldBe 3 (length . filter (Cooperate ==) $ rs)
      shouldBe 1 (length . filter (Defect ==) $ rs)

    it "has a tolerance for counterparty defections" $ do
      let vs = [0.2, 0.4, 0.6, 0.8] :: [Double]
          as = map (mkAgent . mkWorld) vs
      rs <- mapM (\a -> react a (Just Defect) a) as
      shouldBe 1 (length . filter (Cooperate ==) $ rs)
      shouldBe 3 (length . filter (Defect ==) $ rs)

    it "sometimes is selfish even when the counterparty cooperates" $ do
      let vs = [0.2, 0.4, 0.6, 0.8] :: [Double]
          as = map (mkAgent . mkWorld) vs
      rs <- mapM (\a -> react a (Just Cooperate) a) as
      shouldBe 3 (length . filter (Cooperate ==) $ rs)
      shouldBe 1 (length . filter (Defect ==) $ rs)
