{-# LANGUAGE TemplateHaskell #-}


module Lib where

import Control.Lens hiding (element)
import Control.Lens.TH


import System.Random(StdGen, Random (randomR))
import Control.Monad.State.Lazy

import qualified Control.Monad.State.Lazy as ST (get, put, runState, State)
import System.Random.Stateful (getStdGen)
import Data.List ((\\), intercalate)
import Data.Maybe (isJust, isNothing, fromJust)

data Color = Red | Green | Blue | Yellow | Black
    deriving (Show, Eq, Ord)

data Card = Card Color Int
    deriving (Show, Eq)

data Player = Player
    {
    _playerId :: Int,
    _cards :: [Card],
    _choose :: [Card] -> Card -> Maybe Card
    -- select :: [Card] -> Color
    }

data GameState = GameState
    {
    _deck :: [Card],
    _discardPile :: [Card],
    _randomGenerator :: StdGen
    } deriving(Eq, Show)

$(makeLenses ''GameState)
$(makeLenses ''Player)

instance Show Player where
    show pl = show (_playerId pl) ++ ": ["++ intercalate ", " (map show (_cards pl)) ++ "]"

chooseFirstMatching :: [Card] -> Card -> Maybe Card
chooseFirstMatching cards (Card color number) = case [Card col num | (Card col num) <- cards, col == color || num == number] of
    [] -> Nothing
    (h : t) -> Just h

drawCardFromGameState :: GameState -> (Card, GameState)
drawCardFromGameState GameState{_deck = [], _discardPile = [], _randomGenerator = rnd}  = drawCardFromGameState GameState{_deck = tail cds, _discardPile = [head cds], _randomGenerator = newGen}
    where (cds, newGen) = newDeck rnd
drawCardFromGameState GameState{_deck = [], _discardPile = (h:t), _randomGenerator = rnd} = drawCardFromGameState GameState{_deck = cds, _discardPile = [h], _randomGenerator = newGen}
    where (cds, newGen) = shuffle t rnd
drawCardFromGameState GameState{_deck = (h:t), _discardPile = disPile, _randomGenerator = rnd} = (h, GameState{_deck = t, _discardPile = disPile, _randomGenerator = rnd})

takeCardToHand :: Player -> Card -> Player
takeCardToHand pl card = over cards (card :) pl

placeCard :: GameState -> Card -> GameState
placeCard gs card = over discardPile (card : ) gs

playerDrawCard :: Player -> GameState -> (Player, GameState)
playerDrawCard pl gs = (takeCardToHand pl drawnCard, newGameState)
    where (drawnCard, newGameState) = drawCardFromGameState gs

topCard :: GameState -> Card
topCard gs = head $ _discardPile gs

remove:: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (h:t) = if x == h then t else h : remove x t

newDeck :: StdGen -> ([Card], StdGen)
newDeck = shuffle generateDeck

selectStartingCard :: GameState -> GameState
selectStartingCard gs = placeCard remState card
    where (card, remState) = drawCardFromGameState gs

createStartingGameState :: StdGen -> GameState
createStartingGameState rng = selectStartingCard GameState {_deck = cds, _discardPile = [], _randomGenerator = newRng}
    where (cds, newRng) = newDeck rng

elementById :: [a] -> Int -> Maybe a
elementById [] num = Nothing
elementById (h:t) 0 = Just h
elementById (h:t) n = elementById t (n - 1)

shuffle :: Eq a => [a] -> StdGen -> ([a], StdGen)
shuffle [] rng = ([], rng)
shuffle as rng = (selectedVal : aTail, finalGen)
    where
        (selectedId, newGen) = randomR (0, length as - 1) rng
        selectedVal = case elementById as selectedId of
            Nothing -> head as
            Just val -> val
        (aTail, finalGen) = shuffle (remove selectedVal as) newGen

generateDeck :: [Card]
generateDeck = map decodeCard ([0 .. 39] ++ [0 .. 39])

generatePrimitivePlayers :: Int -> [Player]
generatePrimitivePlayers count = [Player{_playerId = id1, _cards = [], _choose = chooseFirstMatching} | id1 <- [1 .. count]]

startingNumberOfCards :: Int
startingNumberOfCards = 7

numberOfPlayers :: Int
numberOfPlayers = 8

fillWithCardsFromGameState :: Player -> GameState -> (Player, GameState)
fillWithCardsFromGameState Player{_playerId = i, _choose = chs} gs = (Player{_playerId = i, _choose = chs, _cards = cds}, newGs)
    where (cds, newGs) = drawMultipleCards gs startingNumberOfCards

drawMultipleCards :: GameState -> Int -> ([Card], GameState)
drawMultipleCards gs 0 = ([], gs)
drawMultipleCards gs n = (cd : remCds, newGs)
    where
        (cd, midGs) = drawCardFromGameState gs
        (remCds, newGs) = drawMultipleCards midGs (n -1)

decodeCard :: Int -> Card
decodeCard x
    | x >= 40 = decodeCard $ x `mod` 40
    | otherwise = Card (decodeColor $ x `div` 10) (x `mod` 10)

decodeColor :: Int -> Color
decodeColor 0 = Red
decodeColor 1 = Green
decodeColor 2 = Blue
decodeColor 3 = Yellow
decodeColor _ = Black

canPlace :: Card -> Card -> Bool
canPlace (Card Black _) _ = True
canPlace _ (Card Black _) = True
canPlace (Card col1 num1) (Card col2 num2) = col1 == col2 || num1 == num2

haveWon :: Player -> Bool
haveWon pl = null (view cards pl)

canPlaceFromGameState :: Card -> GameState -> Bool
canPlaceFromGameState cd gs = canPlace (topCard gs) cd

placeCardIfPossible :: Player -> GameState -> (Bool, (Player, GameState))
placeCardIfPossible pl gs = case selectedCard of
    Just card -> let playerWithoutCard = over cards (remove card) pl in (True, (playerWithoutCard, placeCard gs (fromJust selectedCard))) 
    Nothing -> (False, (pl, gs))
    where 
        selectedCard = _choose pl (view cards pl) (topCard gs)
    
makeMove :: Player -> GameState -> (Player, GameState)
makeMove pl gs = if haveMadeMove then (newPl, newGs) else snd $ placeCardIfPossible playerHavingDrawn gameStateAfterDraw
    where 
        (haveMadeMove, (newPl, newGs)) = placeCardIfPossible pl gs
        (playerHavingDrawn, gameStateAfterDraw) = playerDrawCard pl gs

makeEveryTurn :: [Player] -> GameState -> (([Player], GameState), Maybe Int)
makeEveryTurn [] gs = (([], gs), Nothing)
makeEveryTurn (h:t) gs = if haveWon newPl then ((newPl : t, newGs), Just 0) else  ((newPl : remPl, remGs), fmap (+1) remRes)
    where
        (newPl, newGs) = makeMove h gs
        ((remPl, remGs), remRes) = makeEveryTurn t newGs