module InteractionSpec (spec) where

import Test.Hspec
import Interaction (
  simpleFactory,
  probabilisticFactory,
  Action (..),
  Interaction (..),
  interactionFactory
  )

import Agent (
  Agent (..)
  )

import World (
  World (..),
  RewardsVector (..),
  deterministicGenEvent
  )

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
  describe "simple reactions" $ do
    let agent = Agent { name=1
                      , generosity=0
                      , selfishness=0
                      }
    it "always cooperate on the first move" $ do
      res <- simpleFactory (mkWorld 1) Nothing agent
      shouldBe res Cooperate

    it "respond to a defection with a defection" $ do
      res <- simpleFactory (mkWorld 1) (Just Defect) agent
      shouldBe res Defect

    it "respond to a cooperation with a cooperation" $ do
      res <- simpleFactory (mkWorld 1) (Just Cooperate) agent
      shouldBe res Cooperate

  describe "probabilistic reactions" $ do
    let agent = Agent { name=1
                      , generosity=0.25
                      , selfishness=0.25
                      }
        vs    = [0.2, 0.4, 0.6, 0.8] :: [Double]
        ws    = map (probabilisticFactory . mkWorld) vs

    it "cooperate on the first move, except when being selfish" $ do
      rs <- mapM (\react -> react Nothing agent) ws
      shouldBe (length . filter (Cooperate ==) $ rs) 3
      shouldBe (length . filter (Defect ==) $ rs) 1

    it "have a tolerance for counterparty defections" $ do
      rs <- mapM (\react -> react (Just Defect) agent) ws
      shouldBe (length . filter (Cooperate ==) $ rs) 1
      shouldBe (length . filter (Defect ==) $ rs) 3

    it "sometimes are selfish even when the counterparty cooperates" $ do
      rs <- mapM (\react -> react (Just Cooperate) agent) ws
      shouldBe (length . filter (Cooperate ==) $ rs) 3
      shouldBe (length . filter (Defect ==) $ rs) 1

    it "sometimes mess up" $ do
      let agent2 = Agent { name=2, generosity=0, selfishness=0 }
          w2s    = map (probabilisticFactory . mkFaultyWorld) vs
      rs <- mapM (\react -> react Nothing agent2) w2s
      shouldBe (length . filter (Cooperate ==) $ rs) 3
      shouldBe (length . filter (Defect ==) $ rs) 1

  describe "interactions" $
    it "generates an Action based on the counter-Agent's history" $ do
      let react = simpleFactory . mkWorld $ 0.1
          a1    = Agent { name=1, generosity=0, selfishness=0 }
          a2    = Agent { name=2, generosity=0, selfishness=0 }
          a3    = Agent { name=3, generosity=0, selfishness=0 }
          a4    = Agent { name=4, generosity=0, selfishness=0 }
          a2hst = (a2, [Interaction 2 1 Cooperate])
          a3hst = (a3, [Interaction 3 1 Defect])
          a4hst = (a4, []) :: (Agent, [Interaction])
      r1 <- interactionFactory react a1 a2hst
      r2 <- interactionFactory react a1 a3hst
      r3 <- interactionFactory react a1 a4hst
      shouldBe r1 Cooperate
      shouldBe r2 Defect
      shouldBe r3 Cooperate
