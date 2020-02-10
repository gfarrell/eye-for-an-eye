module Agent (
  Agent (..),
  AgentID,
  copyAgentWithName,
  copyAgentWithScoreAdjustment
) where

import PrettyPrint (Csv (..))

type AgentID = Int
data Agent = Agent { name :: AgentID
                   , generosity :: Double
                   , selfishness :: Double
                   , score :: Double
                   } deriving Show

instance Eq Agent where
  (==) a b = name a == name b
  (/=) a b = name a /= name b

instance Csv Agent where
  getCsvHeader _ = "name,generosity,selfishness,score"
  getCsvRow a = [ show (name a)
                , show (generosity a)
                , show (selfishness a)
                , show (score a)
                ]

copyAgentWithName :: Agent -> AgentID -> Agent
copyAgentWithName agent name' =
  Agent { name        = name'
        , generosity  = generosity agent
        , selfishness = selfishness agent
        , score       = score agent
        }

copyAgentWithScoreAdjustment :: Agent -> Double -> Agent
copyAgentWithScoreAdjustment agent score' =
  Agent { name        = name agent
        , generosity  = generosity agent
        , selfishness = selfishness agent
        , score       = score agent + score'
        }
