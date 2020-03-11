module Reproduce (
  scoreToProbabilityDensity,
  shouldItReproduce,
  reproduce,
  getRange,
  reproduceAll
) where

import World (
  ReproductionAssumptions (..),
  World (..),
  EventGenerator
  )

import Agent (
  AgentID,
  Agent (Agent, score, name),
  copyAgentWithName
  )

-- Reproduction Module
-- ===================
--
-- This module handles the reproducing of Agents on a probabilistic
-- basis using their scores as probability densities. Most
-- of the assumptions behind the model are loaded into the
-- ReproductionAssumptions in the World, however there are some
-- assumptions built into the implementation of the module. Firstly,
-- we assume that reproduction is a probabilistic process but that no
-- Agent has a 0% chance, nor does any have a 100% chance as those do
-- not tend to exist in nature. We are also assuming that Agents will
-- inherit the scores of their parents (although this is built into the
-- `copyAgentWithName` function in the Agent module).
--

-- scoreToProbabilityDensity
-- -------------------------
--
-- This takes a set of ReproductionAssumptions and a tuple of min and
-- max values (in the set of agents), and then converts a score to a
-- probability density for reproduction (i.e. a number p such that for
-- some random variable X, sampled as x, x <= p => R where R represents
-- a reproduction event taking place for the given Agent).

scoreToProbabilityDensity :: ReproductionAssumptions -> (Double, Double) -> Double -> Double
scoreToProbabilityDensity assumptions (minScore, maxScore) score =
  (maxReproProb assumptions - minReproProb assumptions) * score / (getMaxScoreScale assumptions maxScore - getMinScoreScale assumptions minScore)

-- getRange
-- --------
--
-- Gets the range of a list (min and max)

getRange :: Ord a => [a] -> (a, a)
getRange zs = foldl minMax (head zs, head zs) zs
  where minMax (x, y) z | z < x     = (z, y)
                        | z > y     = (x, z)
                        | otherwise = (x, y)

-- shouldItReproduce (?)
-- -----------------
--
-- This takes the scoring statistics, ReproductionAssumptions, and an
-- EventGenerator and returns a boolean value for a given agent as to
-- whether it should reproduce. This is probabilistic, so a True value
-- implies that the Agent will reproduce, not that it might reproduce.

shouldItReproduce :: EventGenerator -> ReproductionAssumptions -> (Double, Double) -> Double -> IO Bool
shouldItReproduce gen assumptions scoreRange s =
  let p = scoreToProbabilityDensity assumptions scoreRange s
   in (p >=) <$> gen

-- reproduce
-- ---------
--
-- Takes an Agent and some parameters and returns a list of Agent, which
-- will have two Agents if the Agent reproduced, and one if it didn't.

reproduce :: EventGenerator -> ReproductionAssumptions -> (Double, Double) -> AgentID -> Agent -> IO ([Agent], AgentID)
reproduce gen assumptions scoreRange maxId agent = do
  r <- shouldItReproduce gen assumptions scoreRange (score agent)
  if r then return ([agent, copyAgentWithName agent maxId], maxId + 1)
       else return ([agent], maxId)

-- reproduceAll
-- ------------
--
-- Takes a list of Agents, ReproductionAssumptions, and an
-- EventGenerator and then returns a new list (or IO [Agent]) in which
-- some Agents will have reproduced.

reproduceAll :: EventGenerator -> ReproductionAssumptions -> [Agent] -> IO [Agent]
reproduceAll gen assumptions agents = run' (1 + (maximum . map name $ agents)) agents
  where factory = reproduce gen assumptions . getRange . map score $ agents
        run' :: AgentID -> [Agent] -> IO [Agent]
        run' _ [] = return []
        run' maxId' (agent:others) = do
            (newAgents, newMax) <- factory maxId' agent
            (newAgents ++) <$> run' newMax others
