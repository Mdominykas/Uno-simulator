module Lib where

import System.Random(StdGen, Random (randomR))
import Control.Monad.State.Lazy ( MonadState(put, get), when, evalState, State )

import qualified Control.Monad.State.Lazy as ST (get, put, runState, State)
import System.Random.Stateful (getStdGen)
import Data.List ((\\), intercalate)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Bifunctor as BF

import Card(Color (..), Card (..), canPlace, cardColor, cardNumber)
import Player(Player (..), takeCardToHand, haveWon, choose, playerId)
import GameState(GameState (..), deck, discardPile, afterEffects, takeCardFromGameState, canPlaceFromGameState, fillWithCardsFromGameState, applyAfterEffects, placeCardIfPossible, playerDrawCard)
import Control.Monad.Writer (WriterT, Writer, MonadWriter (tell), runWriter)
import GameLog (LogMessage (SkippedTurn, StartOfTurn, WonGame, EndOfTurn))
import Debug.Trace (trace, traceId, traceM)

makeMove :: Player -> GameState -> Writer [LogMessage] (Player, GameState)
makeMove pl gs = do

    (haveMadeMove, (newPl, newGs)) <- placeCardIfPossible pl gs
    if haveMadeMove 
        then 
            return (newPl, newGs) 
        else do
            (playerHavingDrawn, gameStateAfterDraw) <- playerDrawCard (pl, gs)
            (_, (finalPlayer, finalGameState)) <- placeCardIfPossible playerHavingDrawn gameStateAfterDraw
            return (finalPlayer, finalGameState)

findWinner :: [Player] -> GameState -> (Int, [LogMessage])
findWinner players gameState = runWriter $ findWinner'' (gameState, players)

-- TODO Writer [LogMessage] atrodo, kad kiekvienu sujungia listus (kvadratinis)
findWinner'' :: (GameState, [Player]) -> Writer [LogMessage] Int
findWinner'' (curGs, players) = do
    traceM "\nhello\n"
    let curPl = head players
        plId = playerId curPl

    (skipsTurn, (newPl, newGs)) <- applyAfterEffects curPl curGs
    if skipsTurn 
        then findWinner'' (newGs, tail players ++ [newPl])
        else do
            tell [StartOfTurn plId]
            (newPl, newGs) <- makeMove curPl curGs
            if haveWon newPl 
                then do 
                    tell [WonGame plId]
                    return (playerId newPl) 
                else do 
                    tell [EndOfTurn plId]
                    findWinner'' (newGs, tail players ++ [newPl])