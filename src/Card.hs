module Card where
import System.Random (StdGen)
import Utils (shuffle)

data Color = Red | Green | Blue | Yellow | Black
    deriving (Show, Eq, Ord)

data Card = Card Color Int | PlusTwo Color | SkipTurn Color
    deriving (Show, Eq)

cardColor :: Card -> Color
cardColor (Card col num) = col
cardColor (PlusTwo col) = col
cardColor (SkipTurn col) = col

cardNumber :: Card -> Int
cardNumber (Card _ num) = num
cardNumber (PlusTwo _) = 10
cardNumber (SkipTurn _) = 11

canPlace :: Card -> Card -> Bool
canPlace card1 card2 = (cardColor card1 == cardColor card2) || (cardNumber card1 == cardNumber card2)

generateDeck :: [Card]
generateDeck = map decodeCard ([0 .. 48] ++ [0 .. 48])
    where
        decodeCard x
            | x >= 48 = decodeCard $ x `mod` 48
            | x >= 44 = PlusTwo (decodeColor $ x `mod` 4)
            | x >= 40 = SkipTurn (decodeColor $ x `mod` 4)
            | otherwise = Card (decodeColor $ x `div` 10) (x `mod` 10)

        decodeColor 0 = Red
        decodeColor 1 = Green
        decodeColor 2 = Blue
        decodeColor 3 = Yellow
        decodeColor _ = Black

newDeck :: StdGen -> ([Card], StdGen)
newDeck = shuffle generateDeck
