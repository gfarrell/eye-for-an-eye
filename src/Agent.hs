module Agent (
    Agent (..),
    simpleFactory,
    simpleReactor,
    probabilisticFactory,
) where

import World (World (..), Action (..), EventGenerator)
import System.Random

data Agent = Agent { react :: Reactor
                   , niceness :: Double
                   , selfishness :: Double
                   }

type Reactor = Maybe Action -> Agent -> IO Action
type ReactorFactory = World -> Reactor

simpleReactor :: Reactor
simpleReactor Nothing _      = return Cooperate
simpleReactor (Just input) _ = case input of Cooperate -> return Cooperate
                                             Defect    -> return Defect

simpleFactory :: ReactorFactory
simpleFactory _ = simpleReactor

-- Produces a random choice with a probability weighting. The
-- probability is the chance of achieving a Cooperation (instead of a
-- Defection).
weightedChoice :: EventGenerator -> Double -> IO Action
weightedChoice _ 0 = return Defect
weightedChoice _ 1 = return Cooperate
weightedChoice gen p = do
  x <- gen
  if x < p then return Cooperate else return Defect

probabilisticFactory :: ReactorFactory
probabilisticFactory w = probabilisticReactor
  where gen = generator w
        probabilisticReactor :: Reactor
        probabilisticReactor Nothing me = weightedChoice gen (1 - selfishness me)
        probabilisticReactor (Just input) me =
          let p = case input of Cooperate -> 1 - selfishness me
                                Defect    -> niceness me
          in weightedChoice gen p
