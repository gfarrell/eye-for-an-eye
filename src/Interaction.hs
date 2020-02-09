module Interaction (
  Interaction (..),
  findInteraction,
  findMyInteractions,
  getMyAction,
  getCounterAgentAction,
  interactionFactory,
  interactAll,
) where

import Agent (
  Agent (..),
  AgentID
  )

import React (
  Reactor,
  Action (..)
  )

data Interaction = Interaction AgentID AgentID Action Action
  deriving Show

instance Eq Interaction where
  (==) (Interaction a b x y)
       (Interaction c d u v) = a == c && b == d && x == u && y == v
                            || a == d && b == c && x == v && y == u

-- Helper function to find all Interactions for a given AgentID
findMyInteractions :: AgentID -> [Interaction] -> [Interaction]
findMyInteractions me = filter (\ (Interaction a b _ _) -> (a==me) || (b==me))
-- Helper function to find the Interaction for a pair of Agents
findInteraction :: AgentID -> AgentID -> [Interaction] -> Maybe Interaction
findInteraction a b interactions =
  let found = findMyInteractions a . findMyInteractions b $ interactions
  in case length found of 0 -> Nothing
                          _ -> Just (head found)

-- Helper function to get the Action for a given Agent from an
-- Interaction
-- TODO: it would be nice to be able to guarantee that this action
-- contains the AgentID of the given Agent, then we could get rid of the
-- Maybe.
getMyAction :: AgentID -> Maybe Interaction -> Maybe Action
getMyAction _ Nothing = Nothing
getMyAction me (Just (Interaction a b x y))
  | a == me   = Just x
  | b == me   = Just y
  | otherwise = Nothing

-- Helper function to get the Action for the counter-Agent from an
-- Interaction
-- TODO: it would be nice to be able to guarantee that this action
-- contains the AgentID of the given Agent, then we could get rid of the
-- Maybe.
getCounterAgentAction :: AgentID -> Maybe Interaction -> Maybe Action
getCounterAgentAction _ Nothing = Nothing
getCounterAgentAction me (Just (Interaction a b x y))
  | a == me   = Just y
  | b == me   = Just x
  | otherwise = Nothing

-- Create an Interactor function given a Reactor function and the
-- Interaction history.
type Interactor = Agent -> Agent -> IO Interaction
interactionFactory :: Reactor -> [Interaction] -> Interactor
interactionFactory react history me them = do
  let myId            = name me
      theirId         = name them
      prevInteraction = findInteraction myId theirId history
      theirPrevAction = getCounterAgentAction myId prevInteraction
      myPrevAction    = getMyAction myId prevInteraction
  myAction    <- react theirPrevAction me
  theirAction <- react myPrevAction them
  return (Interaction myId theirId myAction theirAction)

-- Produces a new list of Interactions using a Reactor function and a
-- list of previous Interactions, by operating on a list of Agents.
interactAll :: Reactor -> [Interaction] -> [Agent] -> IO [Interaction]
interactAll react history = interactAll'
  where interact = interactionFactory react history
        interactAll' :: [Agent] -> IO [Interaction]
        interactAll' []     = return []
        interactAll' (x:xs) = do
          head_i <- mapM (interact x) xs
          tail_i <- interactAll' xs
          return (head_i ++ tail_i)
