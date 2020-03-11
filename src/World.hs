module World (
    World (..),
    RewardsVector (..),
    EventGenerator,
    genEvent,
    deterministicGenEvent,
    ReproductionAssumptions (..),
    basicReproAssumptions
) where

import System.Random

-- Definition of a World
-- ---------------------
--
-- Parameters:
--   rewards contains a vector of reward additives for different scenarios
--
--   mistake_rate indicates the probability of an intended action
--   becoming the opposite
--
--   initial_size is the starting number of Agents
--
--   iterations is the number of iterations of the simulation to run
--
--   generator is a random event generator (a Double between 0 and 1)
--
data World = World {
  rewards :: RewardsVector
, mistake_rate :: Double
, initial_size :: Int
, iterations :: Int
, generator :: EventGenerator
, reproduction_assumptions :: ReproductionAssumptions
}

instance Show World where
  show w = "World { "
      ++ "rewards: " ++ shows (rewards w) ", "
      ++ "mistake_rate: " ++ shows (mistake_rate w) ", "
      ++ "initial_size: " ++ shows (initial_size w) ", "
      ++ "iterations: " ++ shows (iterations w) ", "

type EventGenerator = IO Double
genEvent :: EventGenerator
genEvent = getStdRandom (randomR (0, 1))

deterministicGenEvent :: Double -> EventGenerator
deterministicGenEvent result = detGenEvent
  where detGenEvent = return result

-- RewardsVector contains the score effects for different outcomes
--
-- For an agent A, and counteragent B, the reward/punishment for A in
-- the order of the RewardsVector is as follows:
--
--   A cooperates, B cooperates -> RewardsVector x _ _ _
--   A cooperates, B defects    -> RewardsVector _ x _ _
--   A defects,    B cooperates -> RewardsVector _ _ x _
--   A defects,    B defects    -> RewardsVector _ _ _ x
--
data RewardsVector =
  RewardsVector Double Double Double Double
  deriving Show

data ReproductionAssumptions = ReproductionAssumptions {
  minReproProb :: Double
, maxReproProb :: Double
, getMinScoreScale :: Double -> Double
, getMaxScoreScale :: Double -> Double
-- do we need reproduction_multiplier?
}

basicReproAssumptions = ReproductionAssumptions {
  minReproProb = 0.1
, maxReproProb = 0.9
, getMinScoreScale = (/ 2)
, getMaxScoreScale = (* 2)
}
