module Simulation (
  runSimulation,
  Dist (..),
  Frame (..)
) where

import Agent (
  AgentID,
  Agent(..),
  copyAgentWithScoreAdjustment,
  copyAgentWithName
  )

import React (
  Reactor,
  Action (..)
  )

import World (
  World (..),
  ReproductionAssumptions (..)
  )

import Interaction (
  Interaction,
  interactAll
  )

import Scoring (
  scoreAgents
  )

import System.Random

type AgentCreator = AgentID -> IO Agent

-- Distributions are functions which will be used to choose the
-- properties of Agents at random, given the parameters describing the
-- Distribution. There are two types of distribution at present.
--
-- Normal Distribution:
-- Given a mean and standard deviation
-- Given some parameters (mean and standard deviation) this will sample
-- a the described normal probability distribution and return a value
-- from it.
--
-- Flat Distribution:
-- Given some parameters (min and max) this will sample an even
-- probability distribution and return a value between the min and max
-- values.

data Dist = Norm Double Double
          | Flat Double Double
distribution :: Dist -> IO Double
distribution (Norm mean std) = return mean -- TODO: make this a real norm
distribution (Flat min max) = getStdRandom . randomR $ (min, max)

-- Agent Creation
-- This factory function takes a distribution each for generosity and
-- selfishness, then returns an AgentCreator function which takes an
-- AgentID and returns an Agent (wrapped in IO because the distribution
-- functions require a source of randomness.
agentCreatorFactory :: Dist -> Dist -> AgentCreator
agentCreatorFactory gen_dist sel_dist n = do
  g <- distribution gen_dist
  s <- distribution sel_dist
  return Agent { name=n, generosity=g, selfishness=s, score=0 }

-- This function takes a number of Agents to spawn, and an AgentCreator
-- function, and returns a list of Agents (wrapped in IO).
spawnAgents :: Int -> AgentCreator -> IO [Agent]
spawnAgents 0 _             = return []
spawnAgents count makeAgent = mapM makeAgent [1..count]

-- A simulation Frame encapsulates the state of the simulation at some
-- point in "time". That means it contains a list of Agents in that
-- Frame, and a list of the Interactions that occurred in it as well. It
-- also contains the state of the World, as well as an AgentID counter
-- (so we can easily increment for generating new Agents).
data Frame = Frame AgentID [Agent] [Interaction]
  deriving Show

-- Generate the next Frame from the current one, including scoring the
-- Agents based on the outcomes *in this Frame*.
-- TODO: add in reproduction.
nextFrame :: Reactor -> World -> Frame -> IO Frame
nextFrame react w (Frame i agents history) = do
  interactions <- interactAll react history agents
  let scoredAgents = scoreAgents (rewards w) interactions agents
  return (Frame i scoredAgents interactions)

-- Iterate over IO lists to prevent recalculation of every step.
-- See https://stackoverflow.com/q/60137468
iterateM :: Monad m => (a -> m a) -> a -> Int -> m [a]
iterateM fn = iterateM'
  where iterateM' _ 0           = return []
        iterateM' current limit = do
          next <- fn current
          (current:) <$> iterateM' next (pred limit)

-- Run a simulation for a number of iterations
runSimulation :: World -> Reactor -> (Dist, Dist) -> IO [Frame]
runSimulation world react (gen_dist, sel_dist) = do
  startingAgents <- spawnAgents (initial_size world) (agentCreatorFactory gen_dist sel_dist)
  let firstFrame = Frame (length startingAgents) startingAgents []
      next       = nextFrame react world
  iterateM next firstFrame (iterations world)
