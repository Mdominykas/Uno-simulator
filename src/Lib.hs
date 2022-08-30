module Lib where

import System.Random(StdGen, Random (randomR))
import Control.Monad.State.Lazy ( MonadState(put, get), when, evalState, State )

import qualified Control.Monad.State.Lazy as ST (get, put, runState, State)
import System.Random.Stateful (getStdGen)
import Data.List ((\\), intercalate)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Bifunctor as BF

import Card(Color (..), Card (..), canPlace, cardColor, cardNumber, canBeResponded)
import Player(Player (..), takeCardToHand, haveWon, choose, playerId)
import GameState(GameState (..), deck, discardPile, takeCardFromGameState, canPlaceFromGameState, fillWithCardsFromGameState, applyAfterEffects, placeCardIfPossible, playerDrawCard, playerDrawCards, createStartingGameState, addResponseToActive)
import Control.Monad.Writer (WriterT, Writer, MonadWriter (tell), runWriter)
import GameLog (LogMessage (..))
import Debug.Trace (trace, traceId, traceM)
import Control.Monad.Writer (replicateM)
import Control.Monad (replicateM_)
import Constants (startingNumberOfCards)
import Control.Monad (foldM)
import Data.Foldable (foldlM)

-- returns: (Changes-Direction, Player, GameState)
makeMove :: Player -> GameState -> Writer [LogMessage] (Bool, Player, GameState)
makeMove pl gs = do

    (haveMadeMove, changesDir, (newPl, newGs)) <- placeCardIfPossible pl gs
    if haveMadeMove
        then
            return (changesDir, newPl, newGs)
        else do
            (playerHavingDrawn, gameStateAfterDraw) <- playerDrawCard (pl, gs)
            (_, changesDir, (finalPlayer, finalGameState)) <- placeCardIfPossible playerHavingDrawn gameStateAfterDraw
            return (changesDir, finalPlayer, finalGameState)

findWinner :: [Player] -> StdGen -> (Int, [LogMessage])
findWinner noCardPlayers initialGameState = runWriter $ findWinner' (initialGameState, noCardPlayers)

findWinner' :: (StdGen, [Player]) -> Writer [LogMessage] Int
findWinner' (stdGen, noCardPlayers) = do
    initialGameState <- createStartingGameState stdGen
    (preparedPlayers, gameState1) <- foldlM (\(oldPl, gs) player -> do
        (newPl, newGs) <- playerDrawCards (player, gs) startingNumberOfCards
        return (oldPl ++ [newPl], newGs)) ([], initialGameState) noCardPlayers

    tell [GameStart]

    playTurnByTurn (gameState1, preparedPlayers)

canRespondToActiveCard :: [Card] -> Bool
canRespondToActiveCard cds = not (null cds) && canBeResponded (head cds)

checkForWinnersAndPlayFurther :: Player -> GameState -> [Player] -> Writer [LogMessage] Int
checkForWinnersAndPlayFurther curPl curGs remPlayers = do
    let plId = playerId curPl
    if haveWon curPl
        then do
            tell [WonGame plId]
            return plId
        else do
            tell [EndOfTurn plId]
            playTurnByTurn (curGs, remPlayers ++ [curPl])

playTurnByTurn :: (GameState, [Player]) -> Writer [LogMessage] Int
playTurnByTurn (curGs, players) = do

    let curPl = head players
        plId = playerId curPl

    if canRespondToActiveCard (activeCards curGs)
        then
            case respondToActive curPl (cards curPl) (head $ activeCards curGs) of
                Nothing -> do
                    tell [StartOfTurn plId]
                    (skipsTurn, (newPl, newGs)) <- applyAfterEffects curPl curGs
                    checkForWinnersAndPlayFurther newPl newGs (tail players)
                Just card -> do
                    tell [StartOfTurn plId]
                    (newGs, newPl) <- addResponseToActive curGs curPl card
                    checkForWinnersAndPlayFurther newPl newGs (tail players)
        else do
            tell [StartOfTurn plId]
            (skipsTurn, (newPl, newGs)) <- applyAfterEffects curPl curGs
            if skipsTurn
                then do
                    tell [EndOfTurn plId]
                    playTurnByTurn (newGs, tail players ++ [newPl])
                else do
                    (changesDir, newPl, newGs) <- makeMove curPl curGs
                    let changedPlayersTail = if changesDir then reverse (tail players) else tail players
                    checkForWinnersAndPlayFurther newPl newGs changedPlayersTail
