module Interaction (
  simpleFactory,
  probabilisticFactory,
  interact,
  Action (..)
) where

import World (
  World (..),
  EventGenerator,
  )

import Agent (
  Agent (..),
  AgentID
  )

import System.Random

-- Actions describe how Agents behave to one another. As such, every
-- Action has a subject and an object when used. An Agent (subject) can
-- either Cooperate with or Defect from a counter-Agent (object).
data Action = Cooperate | Defect
  deriving (Show, Eq)

-- Reactors are functions which, based on how the counter-Agent
-- previously behaved with the Agent, dictate how the Agent will now
-- behave with the counter-Agent. Since some of these Reactors will use
-- randomness to generate their behaviours, the resulting action must be
-- wrapped in the IO monad. ReactorFactories create Reactor functions
-- given a World description.
type Reactor = Maybe Action -> Agent -> IO Action
type ReactorFactory = World -> Reactor

-- The simplest Reactor is a basic tit-for-tat algorithm which initially
-- Cooperates (it is fundamentally nice) but after that will just
-- reflect whatever Action is taken with respect to the Agent back at
-- the counter-Agent.
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

-- A more sophisticated Reactor uses a random number generator and has a
-- couple of probabilities using the parameters of the Agent: generosity
-- (the propensity to forgive prior Defections) and selfishness
-- (the propensity to Defect even in the face of a cooperative
-- counter-Agent). There is also a `mistake_rate` in the world which
-- can flip the Action to its opposite.
probabilisticFactory :: ReactorFactory
probabilisticFactory w = probabilisticReactor
  where gen = generator w
        p_mistake = mistake_rate w
        probabilisticReactor :: Reactor
        probabilisticReactor input me =
          let p   = case input of (Just Cooperate) -> 1 - selfishness me
                                  (Just Defect)    -> generosity me
                                  Nothing          -> 1 - selfishness me
              act = weightedChoice gen p
          in do
            x <- gen
            a <- act
            if x > p_mistake then act else
              case a of Cooperate -> return Defect
                        Defect    -> return Cooperate
