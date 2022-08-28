module Card where
import System.Random (StdGen)
import Utils (shuffle)
import Constants (numberedCardCount, plusTwoCount, skipTurnCardCount, changeColorCardCount, plus4CardCount)

data Color = Red | Green | Blue | Yellow | Black
    deriving (Show, Eq, Ord)

data Card = Card Color Int | PlusTwo Color | SkipTurn Color | ChangeColor | PlusFour
    deriving (Show, Eq, Ord)

cardColor :: Card -> Color
cardColor (Card col num) = col
cardColor (PlusTwo col) = col
cardColor (SkipTurn col) = col
cardColor ChangeColor = Black
cardColor PlusFour = Black

cardNumber :: Card -> Int
cardNumber (Card _ num) = num
cardNumber (PlusTwo _) = 10
cardNumber (SkipTurn _) = 11
cardNumber ChangeColor = 12
cardNumber PlusFour = 13

hasNonBlackColor :: Card -> Bool
hasNonBlackColor (Card _ num) = True
hasNonBlackColor (PlusTwo _) = True
hasNonBlackColor (SkipTurn _) = True
hasNonBlackColor ChangeColor = False
hasNonBlackColor PlusFour = False


canPlace :: Card -> Card -> Bool
canPlace deckCard playerCard = (cardColor deckCard == cardColor playerCard) 
                    || (cardNumber deckCard == cardNumber playerCard)
                    || (cardColor playerCard == Black)

generateDeck :: [Card]
generateDeck = numberedCards ++ plusTwoCards ++ skipCards ++ changeColourCards ++ plus4Cards
    where
        nonBlackColors = [Red, Green, Blue, Yellow]
        numberedCards = [Card color number | number <- [0 .. 10], color <-nonBlackColors, it <- [0 .. numberedCardCount]]
        plusTwoCards = [PlusTwo color | color <- nonBlackColors, it <- [0 .. plusTwoCount]]
        skipCards = [SkipTurn color | color <- nonBlackColors, it <- [0 .. skipTurnCardCount]]
        changeColourCards = [ChangeColor | it <- [0 .. changeColorCardCount]]
        plus4Cards = [PlusFour | it <- [0 .. plus4CardCount]]

newDeck :: StdGen -> ([Card], StdGen)
newDeck = shuffle generateDeck
