module World (
    World,
    Action (..)
) where

-- Definition of a World
-- ---------------------
--
-- Parameters:
--   reproduction_multiplier dictates the influence of the agent's score on
--   reproductive success
--
--   rewards contains a vector of reward additives for different scenarios
--
--   mistake_rate indicates the probability of an intended action becoming the
--   opposite
--
--   initial_size is the starting number of Agents
--
--   iterations is the number of iterations of the simulation to run
data World = World { reproduction_multiplier :: Double
                   , rewards :: RewardsVector
                   , mistake_rate :: Double
                   , initial_size :: Integer
                   , iterations :: Integer
                   } deriving Show

data Action = Cooperate | Defect
  deriving (Show, Eq)

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