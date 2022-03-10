{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens hiding (element)
import Control.Lens.TH

import qualified Control.Monad.State.Lazy as ST (get, put, runState, State)

-- import System.Random
import Control.Concurrent (newChan, Chan, readChan, writeChan, forkIO)
import Lib (Player(_cards), Card, generatePrimitivePlayers, numberOfPlayers, createStartingGameState, fillWithCardsFromGameState, GameState (GameState), makeMove, haveWon)
import System.Random (getStdGen, mkStdGen)
import Data.Maybe
import Lib (makeEveryTurn)

main :: IO ()
main = do
    ch <- newChan
    forkIO $ playGame ch 0
    let winners = [0 | _ <- [1 .. numberOfPlayers]]
    findResults ch winners


findResults :: Chan Int -> [Int] -> IO ()
findResults ch winners = do
    val <- readChan ch
    print (incrementByIndex winners val)
    findResults ch (incrementByIndex winners val)

incrementByIndex :: (Eq t, Num t, Num a) => [a] -> t -> [a]
incrementByIndex [] _ = []
incrementByIndex (h : t) 0 = h + 1 : t
incrementByIndex (h : t) val = h : incrementByIndex t (val - 1)

playGame :: Chan Int -> Int -> IO ()
playGame ch gameNum = do
    let stdGen = mkStdGen gameNum
    let noCardPlayers = generatePrimitivePlayers numberOfPlayers
    let initialGameState = createStartingGameState stdGen
    let (players, gameState) = foldl (\(oldPl, gs) player -> (fst (fillWithCardsFromGameState player gs) : oldPl, snd (fillWithCardsFromGameState player gs))) ([], initialGameState) noCardPlayers
    let ans = findWinner players gameState
    writeChan ch ans
    playGame ch (gameNum + 1)

findWinner :: [Player] -> GameState -> Int
findWinner playerList gs = fromMaybe (findWinner newPlayerList newGs) roundRes
    where ((newPlayerList, newGs), roundRes) = makeEveryTurn playerList gs