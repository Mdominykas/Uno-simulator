{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Card(Color (..), Card (..), canPlace, cardColor, cardNumber, newDeck)
import Player(Player (..), takeCardToHand, haveWon, cards, choose, playerId)
import System.Random (StdGen)
import AfterEffect (AfterEffect (..), sumDrawCards, generateAfterEffects)
import Control.Lens ( over, makeLenses, view )
import Utils (shuffle, removeOne)
import qualified Data.Bifunctor as BF
import Constants (startingNumberOfCards)
import Data.Maybe (fromJust)
import Control.Monad.Writer (Writer, MonadWriter (tell))
import GameLog (LogMessage (SkippedTurn, DrewCard, PlacedCard))

data GameState = GameState
    {
    _deck :: [Card],
    _discardPile :: [Card],
    _randomGenerator :: StdGen,
    _afterEffects :: [AfterEffect]
    } deriving(Eq, Show)

$(makeLenses ''GameState)

selectStartingCard :: GameState -> GameState
selectStartingCard gs = placeCard remState card
    where (card, remState) = takeCardFromGameState gs

createStartingGameState :: StdGen -> GameState
createStartingGameState rng = selectStartingCard GameState {_deck = cds, _discardPile = [], _randomGenerator = newRng, _afterEffects = []}
    where (cds, newRng) = newDeck rng

takeCardFromGameState :: GameState -> (Card, GameState)
takeCardFromGameState gs
    | null (_deck gs) && null (_discardPile gs) = takeCardFromGameState gs{_deck = tail newCards, _discardPile = [head newCards], _randomGenerator = newGen}
    | null (_deck gs) = takeCardFromGameState gs{_deck = shuffledDeck, _discardPile = [head $ _discardPile gs], _randomGenerator = shuffledGen}
    | otherwise = (head $ _deck gs, gs{_deck = tail $ _deck gs})
        where
            (newCards, newGen) = newDeck $ view randomGenerator gs
            (shuffledDeck, shuffledGen) = shuffle (tail $ _discardPile gs) (view randomGenerator gs)

takeMultipleCards :: GameState -> Int -> ([Card], GameState)
takeMultipleCards gs 0 = ([], gs)
takeMultipleCards gs n = (cd : remCds, newGs)
    where
        (cd, midGs) = takeCardFromGameState gs
        (remCds, newGs) = takeMultipleCards midGs (n -1)

topCard :: GameState -> Card
topCard gs = head $ view discardPile gs

canPlaceFromGameState :: Card -> GameState -> Bool
canPlaceFromGameState cd gs = canPlace (topCard gs) cd

fillWithCardsFromGameState :: Player -> GameState -> (Player, GameState)
fillWithCardsFromGameState Player{_playerId = i, _choose = chs} gs = (Player{_playerId = i, _choose = chs, _cards = cds}, newGs)
    where (cds, newGs) = takeMultipleCards gs startingNumberOfCards

playerDrawCard :: (Player, GameState) -> Writer [LogMessage] (Player, GameState)
playerDrawCard (pl, gs) = do
    tell [DrewCard (view playerId pl) drawnCard]
    return (takeCardToHand pl drawnCard, newGameState)
    where (drawnCard, newGameState) = takeCardFromGameState gs

playerDrawCards :: (Player, GameState) -> Int -> Writer [LogMessage] (Player, GameState)
playerDrawCards (pl, gs) 0 = return (pl, gs)
playerDrawCards (pl, gs) n = do
    (newPl, newGs) <- playerDrawCard (pl, gs)
    playerDrawCards (newPl, newGs) (n - 1)

placeCard :: GameState -> Card -> GameState
placeCard gs card = addAfterEffectsOfCard cardDiscardedGs card
    where
        cardDiscardedGs = over discardPile (card : ) gs

placesCard :: Player -> Card -> GameState -> Writer [LogMessage] (Player, GameState)
placesCard pl card gs = do
    tell [PlacedCard (view playerId pl) card]
    return (playerWithoutCard, placeCard gs card)
    where
        playerWithoutCard = over cards (removeOne card) pl

addAfterEffectsOfCard :: GameState -> Card -> GameState
addAfterEffectsOfCard gs card = over afterEffects (generateAfterEffects card ++ ) gs

addAfterEffectsToGameState :: GameState -> [AfterEffect] -> GameState
addAfterEffectsToGameState gs aff = over afterEffects (aff ++) gs

clearAfterEffects :: GameState -> GameState
clearAfterEffects = over afterEffects (const [])

applyAfterEffects :: Player -> GameState -> Writer [LogMessage] (Bool, (Player, GameState))
applyAfterEffects pl gs = do
    if skipsTurn 
        then tell [SkippedTurn $ view playerId pl] 
        else tell []
    (newPl, newGs) <- playerDrawCards (pl, gs) cardsToDraw
    return (skipsTurn, (newPl, clearAfterEffects newGs))
    where
        skipsTurn = NoTurn `elem` effects
        effects = view afterEffects gs
        cardsToDraw = sumDrawCards effects

placeCardIfPossible :: Player -> GameState -> Writer [LogMessage] (Bool, (Player, GameState))
placeCardIfPossible pl gs =  case selectedCard of
    Just card -> do
        (newPl, newGs) <- placesCard pl card gs
        -- let playerWithoutCard = over cards (removeOne card) pl in 
        return (True, (newPl, newGs))
    Nothing -> return (False, (pl, gs))
    where
        selectedCard = view choose pl (view cards pl) (topCard gs)
