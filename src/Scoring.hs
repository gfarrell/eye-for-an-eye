module Scoring (
  score,
  scoreAgentInteractions,
  Interaction (..)
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
  AgentID
  )

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
