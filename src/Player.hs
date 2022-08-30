module Player where

import Card ( Card (..), canPlace, Color (Yellow, Black), cardColor )
import Data.List (intercalate)
import CardPlacement (CardPlacement, placementFits)

type PlayerId = Int

data Player = Player
    {
    playerId :: PlayerId,
    cards :: [Card],
    choose :: [Card] -> CardPlacement -> Maybe Card,
    chooseColor :: [Card] -> Color,
    respondToActive :: [Card] -> Card -> Maybe Card
    -- select :: [Card] -> Color
    }

instance Eq Player
    where (==) pl1 pl2 = (playerId pl1 == playerId pl2) && (cards pl1 == cards pl2)

instance Show Player where
    show pl = show (playerId pl) ++ ": ["++ intercalate ", " (map show (cards pl)) ++ "]"

takeCardToHand :: Player -> Card -> Player
takeCardToHand pl card = pl{cards = card : cards pl} 

haveWon :: Player -> Bool
haveWon pl = null (cards pl)

chooseFirstMatching :: [Card] -> CardPlacement -> Maybe Card
chooseFirstMatching cards topPlacedCard = case [card | card <- cards, placementFits topPlacedCard card] of
    [] -> Nothing
    (h : t) -> Just h

chooseFirstColorOrYellow :: [Card] -> Color
chooseFirstColorOrYellow cardList
    | null cardList = Yellow
    | cardColor (head cardList) == Black = chooseFirstColorOrYellow (tail cardList)
    | otherwise = cardColor (head cardList)

generatePrimitivePlayers :: Int -> [Player]
generatePrimitivePlayers count = [Player{playerId = id1, cards = [], choose = chooseFirstMatching, chooseColor = chooseFirstColorOrYellow, respondToActive = alwaysRespondToActive} | id1 <- [0 .. (count - 1)]]

alwaysRespondToActive :: [Card] -> Card -> Maybe Card
alwaysRespondToActive [] activeCard = Nothing
alwaysRespondToActive cards PlusFour = 
    if head cards == PlusFour
        then Just PlusFour
        else alwaysRespondToActive (tail cards) PlusFour
alwaysRespondToActive cards (PlusTwo col) = 
    case head cards of 
        (PlusTwo col) -> Just (PlusTwo col)
        _ -> alwaysRespondToActive (tail cards) (PlusTwo col)
alwaysRespondToActive (_ : remCards) card = alwaysRespondToActive remCards card