module LogChecker where

import Data.MultiSet (MultiSet, member)
import qualified Data.MultiSet as MultiSet
import Data.Map (Map)
import qualified Data.Map as Map
import Card(Card)
import Player (PlayerId)

newtype Hand = Hand (MultiSet Card)

data LogState = LogState
    {
    hands :: Map PlayerId Hand,
    curPlayer :: Int
    }

playerHadCard logState idOfPlayer card = case maybePlayer of 
    Nothing -> False
    Just (Hand cards) -> MultiSet.member card cards
    where 
        maybePlayer = Map.lookup idOfPlayer (hands logState) 

checkLogs logs = True

