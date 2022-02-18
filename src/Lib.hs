{-# LANGUAGE TemplateHaskell #-}


module Lib where

import System.Random(StdGen, mkStdGen, next, uniform, setStdGen, Random (randomR))
import Control.Monad.State.Lazy

import qualified Control.Monad.State.Lazy as ST (get, put, runState, State)
import System.Random.Stateful (getStdGen)
import Data.List ((\\), intercalate)
import Data.Maybe (isJust)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Color = Red | Green | Blue | Yellow | Black
    deriving (Show, Eq, Ord)

data Card = Card Color Int
    deriving (Show, Eq)

data Player = Player
    {
    playerId :: Int,
    cards :: [Card],
    choose :: [Card] -> Card -> Maybe Card
    -- select :: [Card] -> Color
    }


data GameState = GameState
    {
    deck :: [Card],
    discardPile :: [Card],
    randomGenerator :: StdGen
    }


instance Show Player where
    show pl = show (playerId pl) ++ ": ["++ intercalate ", " (map show (cards pl)) ++ "]"

chooseFirstMatching :: [Card] -> Card -> Maybe Card
chooseFirstMatching cards (Card color number) = case [Card col num | (Card col num) <- cards, col == color || num == number] of
    [] -> Nothing
    (h : t) -> Just h

drawCard :: IO Card
drawCard = do
    gen <- getStdGen
    let next = uniform gen :: (Int, StdGen)
    setStdGen $ snd next
    return $ decodeCard $ fst next

drawCardFromGameState :: GameState -> (Card, GameState)
drawCardFromGameState GameState{deck = [], discardPile = [], randomGenerator = rnd}  = drawCardFromGameState GameState{deck = tail cds, discardPile = [head cds], randomGenerator = newGen}
    where (cds, newGen) = newDeck rnd
drawCardFromGameState GameState{deck = [], discardPile = (h:t), randomGenerator = rnd} = drawCardFromGameState GameState{deck = cds, discardPile = [h], randomGenerator = newGen}
    where (cds, newGen) = shuffle t rnd
drawCardFromGameState GameState{deck = (h:t), discardPile = disPile, randomGenerator = rnd} = (h, GameState{deck = t, discardPile = disPile, randomGenerator = rnd})

topCard :: GameState -> Card
topCard gs = head $ discardPile gs

remove :: Eq a => [a] -> a -> [a]
remove [] _ = []
remove (h:t) x = if x == h then t else h : remove t x

newDeck :: StdGen -> ([Card], StdGen)
newDeck = shuffle generateDeck

elementById :: [a] -> Int -> Maybe a
elementById [] num = Nothing
elementById (h:t) 0 = Just h
elementById (h:t) n = elementById t (n - 1)

shuffle :: Eq a => [a] -> StdGen -> ([a], StdGen)
shuffle [] rng = ([], rng)
shuffle as rng = (selectedVal : aTail, finalGen)
    where
        (selectedId, newGen) = randomR (0, length as - 1) rng
        selectedVal = case elementById as selectedId of
            Nothing -> head as
            Just val -> val
        (aTail, finalGen) = shuffle (remove as selectedVal) newGen

generateDeck :: [Card]
generateDeck = map decodeCard ([0 .. 39] ++ [0 .. 39])

generatePlayers1 :: [Player]
generatePlayers1 = [Player{playerId = 0, cards = [], choose = chooseFirstMatching},
                    Player{playerId = 1, cards = [], choose = chooseFirstMatching},
                    Player{playerId = 2, cards = [], choose = chooseFirstMatching},
                    Player{playerId = 3, cards = [], choose = chooseFirstMatching},
                    Player{playerId = 4, cards = [], choose = chooseFirstMatching},
                    Player{playerId = 5, cards = [], choose = chooseFirstMatching},
                    Player{playerId = 6, cards = [], choose = chooseFirstMatching},
                    Player{playerId = 7, cards = [], choose = chooseFirstMatching}]

startingNumberOfCards :: Int
startingNumberOfCards = 7

fillWithCards :: Player -> IO Player
fillWithCards Player {playerId = i, choose = chs} = do
    newCards <- replicateM startingNumberOfCards drawCard
    return Player{playerId = i, cards = newCards, choose = chs}

fillWithCardsFromGameState :: Player -> GameState -> (Player, GameState)
fillWithCardsFromGameState Player{playerId = i, choose = chs} gs = (Player{playerId = i, choose = chs, cards = cds}, newGs)
    where (cds, newGs) = drawAllCards gs startingNumberOfCards

drawAllCards :: GameState -> Int -> ([Card], GameState)
drawAllCards gs 0 = ([], gs)
drawAllCards gs n = (cd : remCds, newGs)
    where
        (cd, midGs) = drawCardFromGameState gs
        (remCds, newGs) = drawAllCards midGs (n -1)

decodeCard :: Int -> Card
decodeCard x
    | x >= 40 = decodeCard $ x `mod` 40
    | otherwise = Card (decodeColor $ x `div` 10) (x `mod` 10)

decodeColor :: Int -> Color
decodeColor 0 = Red
decodeColor 1 = Green
decodeColor 2 = Blue
decodeColor 3 = Yellow
decodeColor _ = Black

canPlace :: Card -> Card -> Bool
canPlace (Card Black _) _ = True
canPlace _ (Card Black _) = True
canPlace (Card col1 num1) (Card col2 num2) = col1 == col2 || num1 == num2

canPlaceFromGameState :: Card -> GameState -> Bool
canPlaceFromGameState cd gs = canPlace (topCard gs) cd

-- makeMoveFromGameState :: Player -> GameState -> (Player, GameState)
-- makeMoveFromGameState Player{playerId = playId, cards = cds, choose = chs} gs = if can

--TODO reikia ne monadinio
makeMove :: Player -> Card -> IO (Player, Card)
makeMove Player{playerId = playId, cards = cds, choose = chs} topCard = do
    let card = chs cds topCard
    selectedCard <- maybe drawCard pure card

    let newCards = if canPlace selectedCard topCard then if isJust card then cds \\ [selectedCard] else cds else selectedCard : cds
    return (Player{playerId = playId, cards = newCards, choose = chs}, head ([selectedCard | canPlace selectedCard topCard] ++ [topCard]))
