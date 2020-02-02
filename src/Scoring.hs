module Scoring (
  findMyInteractions,
  findInteraction,
  score,
  scoreAgentInteractions,
  Interaction (..)
) where

import Interaction (
  Action (..)
  -- Interaction
  )

import World (
  RewardsVector (..)
  )

import Agent (
  AgentID
  )

-- TODO:
-- The entire structure needs a rethink, probably should be a list of
-- interactions and a list of agents but separately so, and interactions
-- should contain the entire interaction between the agents, not the
-- unidirectional actions. The nice thing about this is we can then use
-- a single x:xs style pass, since we compute each entire interaction
-- each time.
-- For now we'll use our own defined Interaction here:

-- TEMP --
data Interaction = Interaction AgentID AgentID Action Action
  deriving (Show, Eq)
-- PMET --

findMyInteractions :: AgentID -> [Interaction] -> [Interaction]
findMyInteractions me = filter (\ (Interaction a b _ _) -> (a==me) || (b==me))
findInteraction :: AgentID -> AgentID -> [Interaction] -> Maybe Interaction
findInteraction a b interactions =
  let found = findMyInteractions a . findMyInteractions b $ interactions
  in case length found of 0 -> Nothing
                          _ -> Just (head found)

score :: RewardsVector -> AgentID -> Interaction -> Double
score (RewardsVector a b c d) me (Interaction x y u v)
  | x == me = calc u v
  | y == me = calc v u
  | otherwise = 0
  where calc :: Action -> Action -> Double
        calc Cooperate Cooperate = a
        calc Cooperate Defect    = b
        calc Defect Cooperate    = c
        calc Defect Defect       = d

scoreAgentInteractions :: RewardsVector -> [Interaction] -> AgentID -> Double
scoreAgentInteractions vec interactions me =
   sum . map (score vec me) $ interactions
