{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens hiding (element)
import Control.Lens.TH

import qualified Control.Monad.State.Lazy as ST (get, put, runState, State)

-- import System.Random
import Lib (generatePlayers1, fillWithCards, Player (playerId, cards), Card, drawCard, makeMove)
import Control.Concurrent (newChan, Chan, readChan, writeChan, forkIO)

-- main :: IO ()
-- main = do
--     print "Hello, friends"

main :: IO ()
main = do
    ch <- newChan
    forkIO $ playGame ch
    let winners = [0, 0, 0, 0, 0, 0, 0, 0]
    findResults ch winners
    -- x <- getStdGen
    -- print (next x)


findResults :: Chan Int -> [Int] -> IO ()
findResults ch winners = do
    val <- readChan ch
    print (addWin winners val)
    findResults ch (addWin winners val)

addWin :: (Eq t, Num t, Num a) => [a] -> t -> [a]
addWin [] _ = []
addWin (h : t) 0 = h + 1 : t
addWin (h : t) val = h : addWin t (val - 1)
-- performExperiment1 :: IO ()
-- performExperiment1 = do
--     print ""
--     let winners = [0, 0, 0]
--     print ""

playGame :: Chan Int -> IO ()
playGame ch = do
    let noCardPlayers = generatePlayers1
    players <- mapM fillWithCards noCardPlayers
    firstCard <- drawCard

    ans <- findWinner (players, firstCard)
    -- print ans
    writeChan ch ans
    playGame ch

findWinner :: ([Player], Card) -> IO Int
findWinner cur = do
    -- print "pradinis:"
    -- print $ show $ fst cur
    rez <- makeEveryTurn cur
    -- print "galinis:"
    -- print $ show rez
    case snd rez of
        Nothing -> findWinner $ fst rez
        Just num -> return num

makeEveryTurn :: ([Player], Card) -> IO (([Player], Card), Maybe Int)
makeEveryTurn ([], c) = return (([], c), Nothing)
makeEveryTurn (h:t, c) = do
    -- print h
    -- print "->"
    -- print "starting position:"
    -- print h
    newHead <- makeMove h c
    -- print "ending position:"
    -- print newHead
    -- print newHead
    rem <- makeEveryTurn (t, snd newHead)
    case cards $ fst newHead of
        [] -> return (([], c), Just $ playerId $ fst newHead)
        _ -> return ((fst newHead : remPlayers, remCard), snd rem)
            where
                remPlayers = fst $ fst rem
                remCard = snd $ fst rem


data Atom = Atom { _element :: String, _point :: Point } deriving(Show)

data Point = Point { _x :: Double, _y :: Double } deriving (Show)

$(makeLenses ''Atom)
$(makeLenses ''Point)

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

data Molecule = Molecule {_atoms :: [Atom] } deriving (Show)

$(makeLenses ''Molecule)

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms. traverse . point . x) (+ 1)