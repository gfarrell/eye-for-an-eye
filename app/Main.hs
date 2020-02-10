module Main where

import Simulation (
  runSimulation,
  Dist (..),
  Frame (..)
  )

import World (
  World (..),
  RewardsVector (..),
  genEvent
  )

import Interaction (
  Interaction
  )

import React (
  probabilisticFactory
  )

import Agent (
  Agent
  )

import PrettyPrint

getAgents :: Frame -> [Agent]
getAgents (Frame _ agents _) = agents

getInteractions :: Frame -> [Interaction]
getInteractions (Frame _ _ interactions) = interactions

main :: IO ()
main = do
  let world = World { reproduction_multiplier=0.25
                    , mistake_rate=0.1
                    , initial_size=50
                    , iterations=50
                    , generator=genEvent
                    , rewards=RewardsVector 10 (-2) 15 (-5)
                    }
      reactor = probabilisticFactory world
  putStrLn "Running Simulation with parameters:"
  print . show $ world
  putStrLn "..."
  results <- runSimulation world reactor (Flat 0.1 0.9, Flat 0.1 0.9)
  let agents = getAgents . last $ results
  putStrLn . getCsvHeader $ agents
  putStr . rowsToString . map (rowToString . getCsvRow) $ agents
