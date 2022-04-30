{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens hiding (element)
import Control.Lens.TH

import qualified Control.Monad.State.Lazy as ST (get, put, runState, State)

-- import System.Random
import Control.Concurrent (newChan, Chan, readChan, writeChan, forkIO, threadDelay)
import Lib (Player(_cards), generatePrimitivePlayers, numberOfPlayers, createStartingGameState, fillWithCardsFromGameState, GameState (GameState), makeMove, haveWon, findWinner)
import Cards(Card (..))

import System.Random (getStdGen, mkStdGen)
import Data.Maybe
import Control.Monad (when)
import Numeric (showFFloat)
import Control.Monad.State (State, MonadState (get, put))
import Control.Monad (forM_)
import Control.Monad.State.Lazy (evalState)

main :: IO ()
main = do
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

incrementByIndex :: (Eq t, Num t, Num a) => [a] -> t -> [a]
incrementByIndex [] _ = []
incrementByIndex (h : t) 0 = h + 1 : t
incrementByIndex (h : t) val = h : incrementByIndex t (val - 1)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs
    | n > length xs = rotate (n `mod` length xs) xs
    | otherwise = take (length xs) (drop n (cycle xs))

playGame :: Chan Int -> Int -> IO ()
playGame ch gameNum = do
    let stdGen = mkStdGen gameNum
    let noCardPlayers = rotate gameNum (generatePrimitivePlayers numberOfPlayers)
    let initialGameState = createStartingGameState stdGen
    let (players, gameState) = foldl (\(oldPl, gs) player -> (fst (fillWithCardsFromGameState player gs) : oldPl, snd (fillWithCardsFromGameState player gs))) ([], initialGameState) noCardPlayers
    let ans = findWinner players gameState
    writeChan ch ans
    threadDelay 10 -- to slow down game playing, since otherwise findResults falls behind
    playGame ch (gameNum + 1)