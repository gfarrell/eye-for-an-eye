module Agent (
    Agent (..),
    AgentID
) where

type AgentID = Integer
data Agent = Agent { name :: AgentID
                   , generosity :: Double
                   , selfishness :: Double
                   }

instance Show Agent where
  show a = "Agent { "
      ++ shows (generosity a) ", "
      ++ shows (selfishness a) " }"

instance Eq Agent where
  (==) a b = name a == name b
  (/=) a b = name a /= name b
