module Card where
import System.Random (StdGen)
import Utils (shuffle)
import Constants (numberedCardCount, plusTwoCount, skipTurnCardCount, changeColorCardCount, plus4CardCount, reverseDirectionCardCount)

data Color = Red | Green | Blue | Yellow | Black
    deriving (Show, Eq, Ord)

data Card = Card Color Int | PlusTwo Color | SkipTurn Color | ReverseDirection Color | ChangeColor | PlusFour
    deriving (Show, Eq, Ord)

cardColor :: Card -> Color
cardColor (Card col num) = col
cardColor (PlusTwo col) = col
cardColor (SkipTurn col) = col
cardColor (ReverseDirection col) = col
cardColor ChangeColor = Black
cardColor PlusFour = Black

-- unique number for each type of cards
cardNumber :: Card -> Int
cardNumber (Card _ num) = num
cardNumber (PlusTwo _) = 10
cardNumber (SkipTurn _) = 11
cardNumber (ReverseDirection _) = 12
cardNumber ChangeColor = 13
cardNumber PlusFour = 14

hasNonBlackColor :: Card -> Bool
hasNonBlackColor (Card _ num) = True
hasNonBlackColor (PlusTwo _) = True
hasNonBlackColor (SkipTurn _) = True
hasNonBlackColor (ReverseDirection _) = True
hasNonBlackColor ChangeColor = False
hasNonBlackColor PlusFour = False

isChangingDirection :: Card -> Bool
isChangingDirection (ReverseDirection _) = True
isChangingDirection _ = False

canPlace :: Card -> Card -> Bool
canPlace deckCard playerCard = 
    (cardColor deckCard == cardColor playerCard) ||
    (cardNumber deckCard == cardNumber playerCard) || 
    (cardColor playerCard == Black)

canBeResponded PlusFour = True
canBeResponded (PlusTwo _) = True
canBeResponded _ = False

generateDeck :: [Card]
generateDeck = numberedCards ++ plusTwoCards ++ skipCards ++ reverseDirecionCards ++ changeColourCards ++ plus4Cards
    where
        nonBlackColors = [Red, Green, Blue, Yellow]
        numberedCards = [Card color number | number <- [0 .. 10], color <-nonBlackColors, it <- [0 .. numberedCardCount]]
        plusTwoCards = [PlusTwo color | color <- nonBlackColors, it <- [0 .. plusTwoCount]]
        skipCards = [SkipTurn color | color <- nonBlackColors, it <- [0 .. skipTurnCardCount]]
        reverseDirecionCards = [ReverseDirection color | color <- nonBlackColors, it <- [0 .. reverseDirectionCardCount]]
        changeColourCards = [ChangeColor | it <- [0 .. changeColorCardCount]]
        plus4Cards = [PlusFour | it <- [0 .. plus4CardCount]]

newDeck :: StdGen -> ([Card], StdGen)
newDeck = shuffle generateDeck

isValidResponse PlusFour PlusFour = True
isValidResponse (PlusTwo _) (PlusTwo _) = True
isValidResponse _ _ = False