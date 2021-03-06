module ReactSpec (spec) where

import Test.Hspec
import React

import Agent (
  Agent (..)
  )

import World (
  World (..),
  RewardsVector (..),
  deterministicGenEvent,
  basicReproAssumptions
  )

mkWorld :: Double -> World
mkWorld v = World { rewards=RewardsVector 0 0 0 0
                  , mistake_rate=0
                  , initial_size=1
                  , iterations=1
                  , generator=deterministicGenEvent v
                  , reproduction_assumptions=basicReproAssumptions
                  }
mkFaultyWorld v = World { rewards=RewardsVector 0 0 0 0
                        , mistake_rate=0.25
                        , initial_size=1
                        , iterations=1
                        , generator=deterministicGenEvent v
                        , reproduction_assumptions=basicReproAssumptions
                        }

spec :: Spec
spec = do
  describe "simple reactions" $ do
    let agent = Agent { name=1
                      , generosity=0
                      , selfishness=0
                      , score=0
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
                      , score=0
                      }
        vs    = [0.2, 0.4, 0.6, 0.8] :: [Double]
        ws    = map (probabilisticFactory . mkWorld) vs
        check :: Int -> Int -> [Action] -> Expectation
        check coop defect a = do
            shouldBe (length . filter (Cooperate ==) $ a) coop
            shouldBe (length . filter (Defect ==) $ a) defect


    it "cooperate on the first move, except when being selfish" $ do
      rs <- mapM (\react -> react Nothing agent) ws
      check 3 1 rs

    it "have a tolerance for counterparty defections" $ do
      rs <- mapM (\react -> react (Just Defect) agent) ws
      check 1 3 rs

    it "sometimes are selfish even when the counterparty cooperates" $ do
      rs <- mapM (\react -> react (Just Cooperate) agent) ws
      check 3 1 rs

    it "sometimes mess up" $ do
      let agent2 = Agent { name=2, generosity=0, selfishness=0, score=0 }
          w2s    = map (probabilisticFactory . mkFaultyWorld) vs
      rs <- mapM (\react -> react Nothing agent2) w2s
      check 3 1 rs
