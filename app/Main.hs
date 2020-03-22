module Main where

import Simulation (
  runSimulation,
  Dist (..),
  Frame (..)
  )

import World (
  World (..),
  RewardsVector (..),
  genEvent,
  basicReproAssumptions
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

import System.IO
import System.Environment

getAgents :: Frame -> [Agent]
getAgents (Frame _ agents _) = agents

getInteractions :: Frame -> [Interaction]
getInteractions (Frame _ _ interactions) = interactions

writeToFile :: Show a => String -> a -> String -> IO ()
writeToFile name index = writeFile (name ++ "_data/iter_" ++ show index ++ ".csv")

frameToCsv :: Frame -> String
frameToCsv frame =
  let agents = getAgents frame
   in rowsToString (getCsvHeader agents : map (rowToString . getCsvRow) agents)

writeFramesData :: String -> [Frame] -> IO ()
writeFramesData name frames = write' frames 0
  where write' :: [Frame] -> Int -> IO ()
        write' [] i = return ()
        write' (frame:others) i = do
          writeToFile name i (frameToCsv frame)
          write' others (i + 1)

main :: IO ()
main = do
  [name, initialSize, iterationCount] <- getArgs
  let world = World { mistake_rate=0.1
                    , initial_size=read initialSize
                    , iterations=read iterationCount
                    , generator=genEvent
                    , rewards=RewardsVector 1 (-2) 2 (-1)
                    , reproduction_assumptions=basicReproAssumptions
                    }
      reactor = probabilisticFactory world
  putStrLn ("Running simulation (output=" ++ name ++ "), with parameters:")
  print . show $ world
  putStr "..."
  results <- runSimulation world reactor (Flat 0.1 0.9, Flat 0.1 0.9)
  writeFramesData name results
  putStrLn "DONE!"
