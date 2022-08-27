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
import GameState(GameState (..), deck, discardPile, afterEffects, takeCardFromGameState, canPlaceFromGameState, fillWithCardsFromGameState, applyAfterEffects, placeCardIfPossible, playerDrawCard, playerDrawCards, createStartingGameState)
import Control.Monad.Writer (WriterT, Writer, MonadWriter (tell), runWriter)
import GameLog (LogMessage (SkippedTurn, StartOfTurn, WonGame, EndOfTurn, GameStart))
import Debug.Trace (trace, traceId, traceM)
import Control.Monad.Writer (replicateM)
import Control.Monad (replicateM_)
import Constants (startingNumberOfCards)
import Control.Monad (foldM)
import Data.Foldable (foldlM)

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

findWinner :: [Player] -> StdGen -> (Int, [LogMessage])
findWinner noCardPlayers initialGameState = runWriter $ findWinner' (initialGameState, noCardPlayers)

findWinner' :: (StdGen, [Player]) -> Writer [LogMessage] Int
findWinner' (stdGen, noCardPlayers) = do
    initialGameState <- createStartingGameState stdGen
    (preparedPlayers, gameState1) <- foldlM (\(oldPl, gs) player -> do
        (newPl, newGs) <- playerDrawCards (player, gs) startingNumberOfCards
        return (oldPl ++ [newPl], newGs)) ([], initialGameState) noCardPlayers

    -- tell [WonGame (playerId $ head players1)]

    tell [GameStart]

    playTurnByTurn (gameState1, preparedPlayers)

playTurnByTurn :: (GameState, [Player]) -> Writer [LogMessage] Int
playTurnByTurn (curGs, players) = do
    let curPl = head players
        plId = playerId curPl

    tell [StartOfTurn plId]
    (skipsTurn, (newPl, newGs)) <- applyAfterEffects curPl curGs
    if skipsTurn 
        then do
            tell [EndOfTurn plId]
            playTurnByTurn (newGs, tail players ++ [newPl])
        else do
            (newPl, newGs) <- makeMove curPl curGs
            if haveWon newPl 
                then do 
                    tell [WonGame plId]
                    return (playerId newPl) 
                else do 
                    tell [EndOfTurn plId]
                    playTurnByTurn (newGs, tail players ++ [newPl])