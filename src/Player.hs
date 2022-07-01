{-# LANGUAGE TemplateHaskell #-}


module Player where

import Control.Lens hiding (element)
import Control.Lens.TH

import Card ( Card, canPlace )
import Data.List (intercalate)

type PlayerId = Int

data Player = Player
    {
    _playerId :: PlayerId,
    _cards :: [Card],
    _choose :: [Card] -> Card -> Maybe Card
    -- select :: [Card] -> Color
    }

$(makeLenses ''Player)

instance Eq Player
    where (==) pl1 pl2 = (view playerId pl1 == view playerId pl2) && (view cards pl1 == view cards pl2)

instance Show Player where
    show pl = show (_playerId pl) ++ ": ["++ intercalate ", " (map show (_cards pl)) ++ "]"

takeCardToHand :: Player -> Card -> Player
takeCardToHand pl card = over cards (card :) pl

haveWon :: Player -> Bool
haveWon pl = null (view cards pl)

chooseFirstMatching :: [Card] -> Card -> Maybe Card
chooseFirstMatching cards cardOnTop = case [card | card <- cards, canPlace card cardOnTop] of
    [] -> Nothing
    (h : t) -> Just h

generatePrimitivePlayers :: Int -> [Player]
generatePrimitivePlayers count = [Player{_playerId = id1, _cards = [], _choose = chooseFirstMatching} | id1 <- [0 .. (count - 1)]]
