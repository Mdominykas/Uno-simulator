{-# LANGUAGE TemplateHaskell #-}


module Player where

import Control.Lens hiding (element)
import Control.Lens.TH

import Card ( Card, canPlace, Color (Yellow, Black), cardColor )
import Data.List (intercalate)
import CardPlacement (CardPlacement, placementFits)

type PlayerId = Int

data Player = Player
    {
    _playerId :: PlayerId,
    _cards :: [Card],
    _choose :: [Card] -> CardPlacement -> Maybe Card,
    chooseColor :: [Card] -> Color
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

chooseFirstMatching :: [Card] -> CardPlacement -> Maybe Card
chooseFirstMatching cards topPlacedCard = case [card | card <- cards, placementFits topPlacedCard card] of
    [] -> Nothing
    (h : t) -> Just h

chooseFirstColorOrYellow :: [Card] -> Color
chooseFirstColorOrYellow cards
    | null cards = Yellow
    | cardColor (head cards) == Black = chooseFirstColorOrYellow (tail cards)
    | otherwise = cardColor (head cards)

generatePrimitivePlayers :: Int -> [Player]
generatePrimitivePlayers count = [Player{_playerId = id1, _cards = [], _choose = chooseFirstMatching, chooseColor = chooseFirstColorOrYellow} | id1 <- [0 .. (count - 1)]]
