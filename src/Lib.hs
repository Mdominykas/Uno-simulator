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

import Card(Color (..), Card (..), canPlace, cardColor, cardNumber)
import Player(Player (..), takeCardToHand, haveWon, cards, choose, playerId)
import GameState(GameState (..), deck, discardPile, afterEffects, drawCardFromGameState, topCard, canPlaceFromGameState, fillWithCardsFromGameState, applyAfterEffects, placeCardIfPossible, playerDrawCard)

makeMove :: Player -> GameState -> (Player, GameState)
makeMove pl gs = if haveMadeMove then (newPl, newGs) else snd $ placeCardIfPossible playerHavingDrawn gameStateAfterDraw
    where
        (haveMadeMove, (newPl, newGs)) = placeCardIfPossible pl gs
        (playerHavingDrawn, gameStateAfterDraw) = playerDrawCard (pl, gs)

findWinner :: [Player] -> GameState -> Int
findWinner players gameState = evalState findWinner' (gameState, players)

findWinner' :: State (GameState, [Player]) Int
findWinner' = do
    (curGs, players) <- get

    let efects = view afterEffects curGs
        curPl = head players
        (skipsTurn, (newPl, newGs)) = applyAfterEffects curPl curGs
    if skipsTurn 
        then put (newGs, tail players ++ [newPl]) >> findWinner' 
            else
        do
            let (newPl, newGs) = makeMove curPl curGs
            if haveWon newPl 
                then return (view playerId newPl) 
                else put (newGs, tail players ++ [newPl]) >> findWinner'