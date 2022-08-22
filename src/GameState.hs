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
import GameLog (LogMessage (SkippedTurn, DrewCard, PlacedCard), createPlacementLog)
import CardPlacement (CardPlacement (Normal, WithColorChange), placementFits, canChangeColor, getCardFromPlacement)
import Debug.Trace (trace, traceM)

data GameState = GameState
    {
    _deck :: [Card],
    _discardPile :: [Card],
    _randomGenerator :: StdGen,
    _afterEffects :: [AfterEffect],
    topCardPlacement :: CardPlacement
    } deriving(Eq, Show)

$(makeLenses ''GameState)

createPlacement :: Player -> Card -> CardPlacement
createPlacement pl card =
    if canChangeColor card then
        WithColorChange card (chooseColor pl (view cards pl))
        else
            Normal card

selectStartingCard :: [Card] -> ([Card], [Card])
selectStartingCard cds = if canChangeColor card then (tailDeck, tailDiscardPile ++ [card]) else (tail cds, [card])
    where
        card = head cds
        (tailDeck, tailDiscardPile) = selectStartingCard (tail cds)

extractTopCardPlacement :: Card -> CardPlacement
extractTopCardPlacement card = if canChangeColor card then error "gameState cannot choose color of card" else Normal card

createStartingGameState :: StdGen -> GameState
createStartingGameState rng = GameState {_deck = startingDeck, _discardPile = startingDiscardPile, _randomGenerator = newRng, _afterEffects = [], topCardPlacement = extractTopCardPlacement (head startingDiscardPile)}
    where
        (cds, newRng) = newDeck rng
        (startingDeck, startingDiscardPile) = selectStartingCard cds

takeCardFromGameState :: GameState -> (Card, GameState)
takeCardFromGameState gs
    | null (_deck gs) && null (_discardPile gs) = error "I didn't want to take new deck" --takeCardFromGameState gs{_deck = tail newCards, _discardPile = [head newCards], _randomGenerator = newGen}
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

canPlaceFromGameState :: Card -> GameState -> Bool
canPlaceFromGameState card gs = placementFits (topCardPlacement gs) card

fillWithCardsFromGameState :: Player -> GameState -> (Player, GameState)
fillWithCardsFromGameState pl gs = (pl{_cards = cds}, newGs)
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

placeCard :: GameState -> CardPlacement -> GameState
placeCard gs cardPlacement = addAfterEffectsOfCard finalGs card
    where
        card = getCardFromPlacement cardPlacement
        cardDiscardedGs = gs{_discardPile = card : _discardPile gs}
        finalGs = cardDiscardedGs{topCardPlacement = cardPlacement}

placesCard :: Player -> Card -> GameState -> Writer [LogMessage] (Player, GameState)
placesCard pl card gs = do
    tell (createPlacementLog pl cardPlacement)
    return (playerWithoutCard, placeCard gs cardPlacement)
    where
        cardPlacement = createPlacement pl card
        playerWithoutCard = over cards (removeOne card) pl


addAfterEffectsOfCard :: GameState -> Card -> GameState
addAfterEffectsOfCard gs card = gs{_afterEffects = generateAfterEffects card ++ _afterEffects gs}

clearAfterEffects :: GameState -> GameState
clearAfterEffects gs = gs{_afterEffects = []}

applyAfterEffects :: Player -> GameState -> Writer [LogMessage] (Bool, (Player, GameState))
applyAfterEffects pl gs = do
    -- traceM ("calling from applyAfterEffects  where {turn is skipped} = " ++ show skipsTurn)
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
        selectedCard = view choose pl (view cards pl) (topCardPlacement gs)

-- function trace is good for debugging (like print)