{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Card(Color (..), Card (..), canPlace, cardColor, cardNumber, newDeck)
import Player(Player (..), takeCardToHand, haveWon, cards, choose, playerId)
import System.Random (StdGen)
import AfterEffect (AfterEffect (..), sumDrawCards, generateAfterEffects)
import Control.Lens ( over, makeLenses, view )
import Utils (shuffle, remove)
import qualified Data.Bifunctor as BF
import Constants (startingNumberOfCards)
import Data.Maybe (fromJust)

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
    where (card, remState) = drawCardFromGameState gs

createStartingGameState :: StdGen -> GameState
createStartingGameState rng = selectStartingCard GameState {_deck = cds, _discardPile = [], _randomGenerator = newRng, _afterEffects = []}
    where (cds, newRng) = newDeck rng

-- alternatyva lenses naudojimui
changeDeck :: GameState -> [Card] -> GameState
changeDeck gs dc = gs {_deck = dc}

-- TODO perrasyti su guards
drawCardFromGameState :: GameState -> (Card, GameState)
drawCardFromGameState GameState{_deck = [], _discardPile = [], _randomGenerator = rnd, _afterEffects = aff}  = drawCardFromGameState GameState{_deck = tail cds, _discardPile = [head cds], _randomGenerator = newGen,  _afterEffects = aff}
    where (cds, newGen) = newDeck rnd
drawCardFromGameState GameState{_deck = [], _discardPile = (h:t), _randomGenerator = rnd, _afterEffects = aff} = drawCardFromGameState GameState{_deck = cds, _discardPile = [h], _randomGenerator = newGen,  _afterEffects = aff}
    where (cds, newGen) = shuffle t rnd
drawCardFromGameState GameState{_deck = (h:t), _discardPile = disPile, _randomGenerator = rnd, _afterEffects = aff} = (h, GameState{_deck = t, _discardPile = disPile, _randomGenerator = rnd,  _afterEffects = aff})

drawMultipleCards :: GameState -> Int -> ([Card], GameState)
drawMultipleCards gs 0 = ([], gs)
drawMultipleCards gs n = (cd : remCds, newGs)
    where
        (cd, midGs) = drawCardFromGameState gs
        (remCds, newGs) = drawMultipleCards midGs (n -1)

topCard :: GameState -> Card
topCard gs = head $ _discardPile gs

canPlaceFromGameState :: Card -> GameState -> Bool
canPlaceFromGameState cd gs = canPlace (topCard gs) cd

fillWithCardsFromGameState :: Player -> GameState -> (Player, GameState)
fillWithCardsFromGameState Player{_playerId = i, _choose = chs} gs = (Player{_playerId = i, _choose = chs, _cards = cds}, newGs)
    where (cds, newGs) = drawMultipleCards gs startingNumberOfCards

playerDrawCard :: (Player, GameState) -> (Player, GameState)
playerDrawCard (pl, gs) = (takeCardToHand pl drawnCard, newGameState)
    where (drawnCard, newGameState) = drawCardFromGameState gs

playerDrawCards :: (Player, GameState) -> Int -> (Player, GameState)
playerDrawCards (pl, gs) n = iterate playerDrawCard (pl, gs) !! n

placeCard :: GameState -> Card -> GameState
placeCard gs card = addAfterEffectsOfCard cardDiscardedGs card
    where
        cardDiscardedGs = over discardPile (card : ) gs

placesCard :: Player -> Card -> GameState -> (Player, GameState)
placesCard pl card gs = (playerWithoutCard, placeCard gs card)
    where
        playerWithoutCard = over cards (remove card) pl

addAfterEffectsOfCard :: GameState -> Card -> GameState
addAfterEffectsOfCard gs card = over afterEffects (generateAfterEffects card ++ ) gs

addAfterEffectsToGameState :: GameState -> [AfterEffect] -> GameState
addAfterEffectsToGameState gs aff = over afterEffects (aff ++) gs

clearAfterEffects :: GameState -> GameState
clearAfterEffects = over afterEffects (const [])

applyAfterEffects :: Player -> GameState -> (Bool, (Player, GameState))
applyAfterEffects pl gs = (NoTurn `elem` effects, BF.second clearAfterEffects $ playerDrawCards (pl, gs) cardsToDraw)
    where
        effects = view afterEffects gs
        cardsToDraw = sumDrawCards effects

placeCardIfPossible :: Player -> GameState -> (Bool, (Player, GameState))
placeCardIfPossible pl gs = case selectedCard of
    Just card -> let playerWithoutCard = over cards (remove card) pl in (True, (playerWithoutCard, placeCard gs (fromJust selectedCard)))
    Nothing -> (False, (pl, gs))
    where
        selectedCard = view choose pl (view cards pl) (topCard gs)
