module Scoring (
  calcScore,
  scoreAgents
) where

import Interaction (
  Interaction (..)
  )

import React (
  Action (..)
  )

import World (
  RewardsVector (..)
  )

import Agent (
  AgentID,
  Agent (name),
  copyAgentWithScoreAdjustment
  )

calcScore :: RewardsVector -> AgentID -> Interaction -> Double
calcScore (RewardsVector a b c d) me (Interaction x y u v)
  | x == me = calc u v
  | y == me = calc v u
  | otherwise = 0
  where calc :: Action -> Action -> Double
        calc Cooperate Cooperate = a
        calc Cooperate Defect    = b
        calc Defect Cooperate    = c
        calc Defect Defect       = d

scoreAgent :: RewardsVector -> [Interaction] -> Agent -> Agent
scoreAgent vec interactions me =
   copyAgentWithScoreAdjustment me
   . sum
   . map (calcScore vec (name me))
   $ interactions

scoreAgents :: RewardsVector -> [Interaction] -> [Agent] -> [Agent]
scoreAgents vec history agents =
  let scorer   = scoreAgent vec history
  in map scorer agents
