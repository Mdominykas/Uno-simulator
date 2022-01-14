module Lib where

import System.Random(StdGen, mkStdGen, next, uniform, setStdGen)
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
    }

instance Show Player where
    show pl = show (playerId pl) ++ ": ["++ intercalate ", " (map show (cards pl)) ++ "]"

chooseFirstMatching :: [Card] -> Card -> Maybe Card
chooseFirstMatching cards (Card color number) = case [Card col num | (Card col num) <- cards, col == color || num == number] of
    [] -> Nothing
    (h : t) ->Just h

-- newtype GameState = GameState {cardGen :: StdGen}

-- generateCard :: 

drawCard :: IO Card
drawCard = do
    gen <- getStdGen
    let next = uniform gen :: (Int, StdGen)
    setStdGen $ snd next
    -- ans <- selectCard $ fst next
    return $ selectCard $ fst next
    -- print "valio"
    -- (GameState (snd nxt), selectCard $ fst nxt)
    -- where
        -- nxt = uniform gen :: (Int, StdGen)

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

selectCard :: Int -> Card
selectCard x
    | x >= 40 = selectCard $ x `mod` 40
    | otherwise = Card (selectColor $ x `div` 10) (x `mod` 10)

selectColor :: Int -> Color
selectColor 0 = Red
selectColor 1 = Green
selectColor 2 = Blue
selectColor 3 = Yellow
selectColor _ = Black

canPlace :: Card -> Card -> Bool
canPlace (Card Black _) _ = True
canPlace _ (Card Black _) = True
canPlace (Card col1 num1) (Card col2 num2) = col1 == col2 || num1 == num2

test :: [Int]
test = [1, 2, 3, 1] \\ [1]

makeMove :: Player -> Card -> IO (Player, Card)
makeMove Player{playerId = playId, cards = cds, choose = chs} topCard = do
    let card = chs cds topCard
    selectedCard <- maybe drawCard pure card

    let newCards = if canPlace selectedCard topCard then if isJust card then cds \\ [selectedCard] else cds else selectedCard : cds 
    return (Player{playerId = playId, cards = newCards, choose = chs}, head ([selectedCard | canPlace selectedCard topCard] ++ [topCard]))
