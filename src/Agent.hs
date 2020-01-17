module Agent (
    Agent (..),
    simple_factory,
    simple_reactor
) where

import World (World, Action (..))

data Agent = Agent { react :: Reactor
                   , niceness :: Double
                   , selfishness :: Double
                   }

type Reactor = (Maybe Action) -> Agent -> Action
type ReactorFactory = World -> Reactor

simple_reactor :: Reactor
simple_reactor Nothing _      = Cooperate
simple_reactor (Just input) _ = case input of Cooperate -> Cooperate
                                              Defect    -> Defect
simple_factory :: World -> Reactor
simple_factory _ = simple_reactor
