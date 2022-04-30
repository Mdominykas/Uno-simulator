{-# LANGUAGE TemplateHaskell #-}


module Lib where

import Control.Lens hiding (element)
import Control.Lens.TH


import System.Random(StdGen, Random (randomR))
import Control.Monad.State.Lazy ( MonadState(put, get), when, evalState, State )

import qualified Control.Monad.State.Lazy as ST (get, put, runState, State)
import System.Random.Stateful (getStdGen)
import Data.List ((\\), intercalate)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Bifunctor as BF

import Cards(Color (..), Card (..), canPlace, cardColor, cardNumber)

data AfterEffect = NoTurn | Draw Int
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
    _randomGenerator :: StdGen,
    _afterEffects :: [AfterEffect]
    } deriving(Eq, Show)

$(makeLenses ''GameState)
$(makeLenses ''Player)

-- alternatyva lenses naudojimui
changeDeck :: GameState -> [Card] -> GameState
changeDeck gs dc = gs {_deck = dc}

instance Eq Player
    where (==) pl1 pl2 = (view playerId pl1 == view playerId pl2) && (view cards pl1 == view cards pl2)

instance Show Player where
    show pl = show (_playerId pl) ++ ": ["++ intercalate ", " (map show (_cards pl)) ++ "]"

chooseFirstMatching :: [Card] -> Card -> Maybe Card
chooseFirstMatching cards cardOnTop = case [card | card <- cards, canPlace card cardOnTop] of
    [] -> Nothing
    (h : t) -> Just h

-- TODO perrasyti su guards
drawCardFromGameState :: GameState -> (Card, GameState)
drawCardFromGameState GameState{_deck = [], _discardPile = [], _randomGenerator = rnd, _afterEffects = aff}  = drawCardFromGameState GameState{_deck = tail cds, _discardPile = [head cds], _randomGenerator = newGen,  _afterEffects = aff}
    where (cds, newGen) = newDeck rnd
drawCardFromGameState GameState{_deck = [], _discardPile = (h:t), _randomGenerator = rnd, _afterEffects = aff} = drawCardFromGameState GameState{_deck = cds, _discardPile = [h], _randomGenerator = newGen,  _afterEffects = aff}
    where (cds, newGen) = shuffle t rnd
drawCardFromGameState GameState{_deck = (h:t), _discardPile = disPile, _randomGenerator = rnd, _afterEffects = aff} = (h, GameState{_deck = t, _discardPile = disPile, _randomGenerator = rnd,  _afterEffects = aff})

takeCardToHand :: Player -> Card -> Player
takeCardToHand pl card = over cards (card :) pl

playerDrawCard :: (Player, GameState) -> (Player, GameState)
playerDrawCard (pl, gs) = (takeCardToHand pl drawnCard, newGameState)
    where (drawnCard, newGameState) = drawCardFromGameState gs

playerDrawCards :: (Player, GameState) -> Int -> (Player, GameState)
playerDrawCards (pl, gs) n = iterate playerDrawCard (pl, gs) !! n

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
createStartingGameState rng = selectStartingCard GameState {_deck = cds, _discardPile = [], _randomGenerator = newRng, _afterEffects = []}
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
generateDeck = map decodeCard ([0 .. 48] ++ [0 .. 48])
    where
        decodeCard x
            | x >= 48 = decodeCard $ x `mod` 48
            | x >= 44 = PlusTwo (decodeColor $ x `mod` 4)
            | x >= 40 = SkipTurn (decodeColor $ x `mod` 4)
            | otherwise = Card (decodeColor $ x `div` 10) (x `mod` 10)

        decodeColor 0 = Red
        decodeColor 1 = Green
        decodeColor 2 = Blue
        decodeColor 3 = Yellow
        decodeColor _ = Black

generatePrimitivePlayers :: Int -> [Player]
generatePrimitivePlayers count = [Player{_playerId = id1, _cards = [], _choose = chooseFirstMatching} | id1 <- [0 .. (count - 1)]]

startingNumberOfCards :: Int
startingNumberOfCards = 7

numberOfPlayers :: Int
numberOfPlayers = 5

fillWithCardsFromGameState :: Player -> GameState -> (Player, GameState)
fillWithCardsFromGameState Player{_playerId = i, _choose = chs} gs = (Player{_playerId = i, _choose = chs, _cards = cds}, newGs)
    where (cds, newGs) = drawMultipleCards gs startingNumberOfCards

drawMultipleCards :: GameState -> Int -> ([Card], GameState)
drawMultipleCards gs 0 = ([], gs)
drawMultipleCards gs n = (cd : remCds, newGs)
    where
        (cd, midGs) = drawCardFromGameState gs
        (remCds, newGs) = drawMultipleCards midGs (n -1)

haveWon :: Player -> Bool
haveWon pl = null (view cards pl)

canPlaceFromGameState :: Card -> GameState -> Bool
canPlaceFromGameState cd gs = canPlace (topCard gs) cd

generateAfterEffects :: Card -> [AfterEffect]
generateAfterEffects (Card _ _) = []
generateAfterEffects (SkipTurn _) = [NoTurn]
generateAfterEffects (PlusTwo _) = [NoTurn, Draw 2]

addAfterEffectsOfCard :: GameState -> Card -> GameState
addAfterEffectsOfCard gs card = over afterEffects (generateAfterEffects card ++ ) gs

placeCardIfPossible :: Player -> GameState -> (Bool, (Player, GameState))
placeCardIfPossible pl gs = case selectedCard of
    Just card -> let playerWithoutCard = over cards (remove card) pl in (True, (playerWithoutCard, placeCard gs (fromJust selectedCard)))
    Nothing -> (False, (pl, gs))
    where
        selectedCard = view choose pl (view cards pl) (topCard gs)

placeCard :: GameState -> Card -> GameState
placeCard gs card = addAfterEffectsOfCard cardDiscardedGs card
    where
        cardDiscardedGs = over discardPile (card : ) gs

placesCard :: Player -> Card -> GameState -> (Player, GameState)
placesCard pl card gs = (playerWithoutCard, placeCard gs card)
    where
        playerWithoutCard = over cards (remove card) pl

makeMove :: Player -> GameState -> (Player, GameState)
makeMove pl gs = if haveMadeMove then (newPl, newGs) else snd $ placeCardIfPossible playerHavingDrawn gameStateAfterDraw
    where
        (haveMadeMove, (newPl, newGs)) = placeCardIfPossible pl gs
        (playerHavingDrawn, gameStateAfterDraw) = playerDrawCard (pl, gs)

sumDrawCards :: [AfterEffect] -> Int
sumDrawCards [] = 0
sumDrawCards ((Draw n) : t) = n + sumDrawCards t
sumDrawCards (_ : t) = sumDrawCards t

addAfterEffectsToGameState :: GameState -> [AfterEffect] -> GameState
addAfterEffectsToGameState gs aff = over afterEffects (aff ++) gs

clearAfterEffects :: GameState -> GameState
clearAfterEffects = over afterEffects (const [])

applyAfterEffects :: Player -> GameState -> (Bool, (Player, GameState))
applyAfterEffects pl gs = (NoTurn `elem` effects, BF.second clearAfterEffects $ playerDrawCards (pl, gs) cardsToDraw)
    where
        effects = view afterEffects gs
        cardsToDraw = sumDrawCards effects

findWinner :: [Player] -> GameState -> Int
findWinner players gameState = evalState findWinner' (gameState, players, [])

findWinner' :: State (GameState, [Player], [Player]) Int
findWinner' = do
    (curGs, remPl, prevPl) <- get
    when (null remPl) (put (curGs, reverse prevPl, remPl))

    (curGs, remPl, prevPl) <- get
    let efects = view afterEffects curGs
        curPl = head remPl
    let (skipsTurn, (newPl, newGs)) = applyAfterEffects curPl curGs
    if skipsTurn then put (newGs, tail remPl, newPl : prevPl) >> findWinner' else findWinner'
    let curPl = head remPl
        (newPl, newGs) = makeMove curPl curGs
    put (newGs, tail remPl, newPl : prevPl)
    if haveWon newPl then return (view playerId newPl) else findWinner'