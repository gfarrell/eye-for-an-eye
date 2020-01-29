module InteractionSpec (spec) where

import Test.Hspec
import Interaction (
  simpleFactory,
  probabilisticFactory,
  Action (..),
  Interaction (..),
  InteractionHistory,
  interactionFactory,
  interactAll
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
mkAgent n = Agent { name=n, generosity=0, selfishness=0 }
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
mkHist :: Agent -> InteractionHistory
mkHist a = (a, [])
pushHist :: Interaction -> InteractionHistory -> InteractionHistory
pushHist x (a, xs) = (a, x:xs)

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
      let agent2 = Agent { name=2, generosity=0, selfishness=0 }
          w2s    = map (probabilisticFactory . mkFaultyWorld) vs
      rs <- mapM (\react -> react Nothing agent2) w2s
      check 3 1 rs

  describe "interactions" $
    it "generates an Interaction based on the counter-Agent's history" $ do
      let react = simpleFactory . mkWorld $ 0.1
          [a1, a2, a3, a4] = map mkAgent [1..4]
          a2hst = (a2, [Interaction 2 1 Cooperate])
          a3hst = (a3, [Interaction 3 1 Defect])
          a4hst = (a4, []) :: (Agent, [Interaction])
      r1 <- interactionFactory react a1 a2hst
      r2 <- interactionFactory react a1 a3hst
      r3 <- interactionFactory react a1 a4hst
      shouldBe r1 (Interaction 1 2 Cooperate)
      shouldBe r2 (Interaction 1 3 Defect)
      shouldBe r3 (Interaction 1 4 Cooperate)

  describe "interactAll" $
    it "interacts all agents together given a reactor" $ do
      let react = simpleFactory . mkWorld $ 0.1
          [a1, a2, a3, a4] = map mkAgent [1..4]
          a1hist = pushHist (Interaction 1 3 Cooperate) (mkHist a1)
          a2hist = pushHist (Interaction 2 1 Defect) (mkHist a2)
          a3hist = pushHist (Interaction 3 4 Defect) (mkHist a3)
          a4hist = mkHist a4
      res <- interactAll react [a1hist, a2hist, a3hist, a4hist]
      -- TODO: this test is a bit rigid, and tightly coupled to the
      -- algorithm implementation which is unfortunate.
      shouldBe
        (map snd res)
        [
          [Interaction 1 2 Defect, Interaction 1 3 Cooperate, Interaction 1 4 Cooperate],
          [Interaction 2 1 Cooperate, Interaction 2 3 Cooperate, Interaction 2 4 Cooperate],
          [Interaction 3 1 Cooperate, Interaction 3 2 Cooperate, Interaction 3 4 Cooperate],
          [Interaction 4 1 Cooperate, Interaction 4 2 Cooperate, Interaction 4 3 Defect]
        ]
