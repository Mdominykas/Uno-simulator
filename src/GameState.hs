module GameState where

import Card(Color (..), Card (..), canPlace, cardColor, cardNumber, newDeck)
import Player(Player (..), takeCardToHand, haveWon, cards, choose, playerId)
import System.Random (StdGen)
import AfterEffect (AfterEffect (..), sumDrawCards, generateAfterEffects)
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
    deck :: [Card],
    discardPile :: [Card],
    randomGenerator :: StdGen,
    afterEffects :: [AfterEffect],
    topCardPlacement :: CardPlacement
    } deriving(Eq, Show)

createPlacement :: Player -> Card -> CardPlacement
createPlacement pl card =
    if canChangeColor card then
        WithColorChange card (chooseColor pl (cards pl))
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
createStartingGameState rng = GameState {deck = startingDeck, discardPile = startingDiscardPile, randomGenerator = newRng, afterEffects = [], topCardPlacement = extractTopCardPlacement (head startingDiscardPile)}
    where
        (cds, newRng) = newDeck rng
        (startingDeck, startingDiscardPile) = selectStartingCard cds

takeCardFromGameState :: GameState -> (Card, GameState)
takeCardFromGameState gs
    | null (deck gs) && null (discardPile gs) = error "I didn't want to take new deck" --takeCardFromGameState gs{deck = tail newCards, discardPile = [head newCards], randomGenerator = newGen}
    | null (deck gs) = takeCardFromGameState gs{deck = shuffledDeck, discardPile = [head $ discardPile gs], randomGenerator = shuffledGen}
    | otherwise = (head $ deck gs, gs{deck = tail $ deck gs})
        where
            (newCards, newGen) = newDeck $ randomGenerator gs
            (shuffledDeck, shuffledGen) = shuffle (tail $ discardPile gs) (randomGenerator gs)

takeMultipleCards :: GameState -> Int -> ([Card], GameState)
takeMultipleCards gs 0 = ([], gs)
takeMultipleCards gs n = (cd : remCds, newGs)
    where
        (cd, midGs) = takeCardFromGameState gs
        (remCds, newGs) = takeMultipleCards midGs (n -1)

canPlaceFromGameState :: Card -> GameState -> Bool
canPlaceFromGameState card gs = placementFits (topCardPlacement gs) card

fillWithCardsFromGameState :: Player -> GameState -> (Player, GameState)
fillWithCardsFromGameState pl gs = (pl{cards = cds}, newGs)
    where (cds, newGs) = takeMultipleCards gs startingNumberOfCards

playerDrawCard :: (Player, GameState) -> Writer [LogMessage] (Player, GameState)
playerDrawCard (pl, gs) = do
    tell [DrewCard (playerId pl) drawnCard]
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
        cardDiscardedGs = gs{discardPile = card : discardPile gs}
        finalGs = cardDiscardedGs{topCardPlacement = cardPlacement}

placesCard :: Player -> Card -> GameState -> Writer [LogMessage] (Player, GameState)
placesCard pl card gs = do
    tell (createPlacementLog pl cardPlacement)
    return (playerWithoutCard, placeCard gs cardPlacement)
    where
        cardPlacement = createPlacement pl card
        playerWithoutCard = pl{cards = removeOne card $ cards pl}


addAfterEffectsOfCard :: GameState -> Card -> GameState
addAfterEffectsOfCard gs card = gs{afterEffects = generateAfterEffects card ++ afterEffects gs}

clearAfterEffects :: GameState -> GameState
clearAfterEffects gs = gs{afterEffects = []}

applyAfterEffects :: Player -> GameState -> Writer [LogMessage] (Bool, (Player, GameState))
applyAfterEffects pl gs = do
    if skipsTurn
        then tell [SkippedTurn $ playerId pl]
        else tell []
    (newPl, newGs) <- playerDrawCards (pl, gs) cardsToDraw
    return (skipsTurn, (newPl, clearAfterEffects newGs))
    where
        skipsTurn = NoTurn `elem` effects
        effects = afterEffects gs
        cardsToDraw = sumDrawCards effects

placeCardIfPossible :: Player -> GameState -> Writer [LogMessage] (Bool, (Player, GameState))
placeCardIfPossible pl gs =  case selectedCard of
    Just card -> do
        (newPl, newGs) <- placesCard pl card gs
        return (True, (newPl, newGs))
    Nothing -> return (False, (pl, gs))
    where
        selectedCard = choose pl (cards pl) (topCardPlacement gs)

-- function trace is good for debugging (like print)