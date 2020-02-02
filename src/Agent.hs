module Agent (
    Agent (..),
    AgentID
) where

type AgentID = Integer
data Agent = Agent { name :: AgentID
                   , generosity :: Double
                   , selfishness :: Double
                   , score :: Double
                   } deriving Show

instance Eq Agent where
  (==) a b = name a == name b
  (/=) a b = name a /= name b
