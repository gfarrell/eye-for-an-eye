module Interaction (
  simpleFactory,
  probabilisticFactory,
  Action (..),
  interactionFactory,
  interactAll,
  Interaction (..),
  InteractionHistory,
  Reactor
) where

import World (
  World (..),
  EventGenerator,
  RewardsVector
  )

import Agent (
  Agent (..),
  AgentID
  )

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
probabilisticFactory w input me =
  let gen = generator w
      p   = case input of (Just Cooperate) -> 1 - selfishness me
                          (Just Defect)    -> generosity me
                          Nothing          -> 1 - selfishness me
      act = weightedChoice gen p
  in do
    x <- gen
    a <- act
    if x > mistake_rate w then act else
      case a of Cooperate -> return Defect
                Defect    -> return Cooperate

--
-- This section it all about interactions between Agents
--

data Interaction = Interaction AgentID AgentID Action
  deriving (Show, Eq)
type InteractionHistory = (Agent, [Interaction])

-- Some helper functions for getting things out of Interactions
getSubject :: Interaction -> AgentID
getSubject (Interaction a _ _) = a
getObject :: Interaction -> AgentID
getObject (Interaction _ a _) = a
getAction :: Interaction -> Action
getAction (Interaction _ _ a) = a

-- Find the last Action by the counter-Agent with respect to the given
-- Agent from within the counter-Agent's Interaction history.
findPreviousAction :: Agent -> [Interaction] -> Maybe Action
findPreviousAction agent interactions =
  let found = filter (\i -> Agent.name agent == getObject i) interactions
  in case length found of 0 -> Nothing
                          _ -> Just (getAction . head $ found)

notMine :: Agent -> [InteractionHistory] -> [InteractionHistory]
notMine me = filter (\ (a, _) -> me /= a)

-- Given a Reactor function, this generates an interact function which
-- takes an Agent, and then a counter-Agent's InteractionHistory, and
-- generates an Action using the Reactor function.
interactionFactory :: Reactor -> Agent -> InteractionHistory -> IO Interaction
interactionFactory react me (other, hist) = do
    action <- react (findPreviousAction me hist) me
    return (Interaction (name me) (name other) action)

-- Given a Reactor function, this acts on a list of InteractionHistories
-- and interacts each agent with each other agent, providing the next
-- iteration of the InteractionHistories.
interactAll :: Reactor -> [InteractionHistory] -> IO [InteractionHistory]
interactAll react histories = interactAll' histories
  where interactAll' :: [InteractionHistory] -> IO [InteractionHistory]
        interactAll' []           = return []
        interactAll' ((me, _):hs) = do
          -- TODO: this implementation feels ugly with all the unboxing of IO
          new_histories  <- sequence [interactionFactory react me x | x <- histories, fst x /= me]
          tail_histories <- interactAll' hs
          return ((me, new_histories) : tail_histories)
