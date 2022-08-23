module Main where

import qualified Control.Monad.State.Lazy as ST (get, put, runState, State)

import Control.Concurrent (newChan, Chan, readChan, writeChan, forkIO, threadDelay)
import Lib (makeMove, findWinner)
import Player(Player (..), takeCardToHand, haveWon, cards, playerId, generatePrimitivePlayers)
import GameState(fillWithCardsFromGameState, createStartingGameState)
import Constants(numberOfPlayers, startingNumberOfCards)
import Utils(incrementByIndex, rotate)

import System.Random (getStdGen, mkStdGen)
import Data.Maybe
import Control.Monad ( when, forM_ )
import Numeric (showFFloat)
import Control.Monad.State (State, MonadState (get, put))
import Control.Monad.State.Lazy (evalState)

main :: IO ()
main = do
    print "I started a program"
    ch <- newChan
    forkIO $ playGame ch 0
    let winners = [0 | _ <- [1 .. numberOfPlayers]]
    findResults ch winners

findPercentage :: (Fractional b, Integral a) => [a] -> [b]
findPercentage as = map ((/ cumSum) . fromIntegral) as
    where cumSum = if sum as == 0 then 1 else fromIntegral $ sum as

intsAsTableRow :: [Int] -> String
intsAsTableRow [] = ""
intsAsTableRow (h:t) = " | " ++ show h ++ intsAsTableRow t

floatsAsPercentageInTableRow :: [Float] -> String
floatsAsPercentageInTableRow [] = ""
floatsAsPercentageInTableRow (h:t) = "| " ++ someVal ++ "% " ++ floatsAsPercentageInTableRow t
    where
        someVal = showFFloat (Just 2) (100 * h) ""

findResults :: Chan Int -> [Int] -> IO ()
findResults ch oldWinners = do
    val <- readChan ch
    let winners = incrementByIndex oldWinners val
    when ((sum winners `mod` 1000) == 0) $ print (intsAsTableRow winners) >> print (floatsAsPercentageInTableRow $ findPercentage winners)
    findResults ch winners

playGame :: Chan Int -> Int -> IO ()
playGame ch gameNum = do
    -- print $ "started a round number" ++ show gameNum
    ans <- oneGame gameNum
    -- print $ "winner is " ++ show ans
    writeChan ch (fst ans)
    print $ snd ans
    threadDelay 10 -- to slow down game playing, since otherwise findResults falls behind
    playGame ch (gameNum + 1)

oneGame gameNum = do
    let stdGen = mkStdGen gameNum
    let noCardPlayers = rotate gameNum (generatePrimitivePlayers numberOfPlayers)
    let initialGameState = createStartingGameState stdGen
    let ans = findWinner noCardPlayers initialGameState
    return ans
