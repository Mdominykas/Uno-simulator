module Cards where

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
