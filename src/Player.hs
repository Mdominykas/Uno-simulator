{-# LANGUAGE TemplateHaskell #-}


module Player where

import Control.Lens hiding (element)
import Control.Lens.TH

import Cards
import Data.List (intercalate)

data Player = Player
    {
    _playerId :: Int,
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
