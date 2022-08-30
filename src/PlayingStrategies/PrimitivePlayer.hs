module PlayingStrategies.PrimitivePlayer where
import Card
import CardPlacement (CardPlacement, placementFits)

chooseFirstMatching :: [Card] -> CardPlacement -> Maybe Card
chooseFirstMatching cards topPlacedCard = case [card | card <- cards, placementFits topPlacedCard card] of
    [] -> Nothing
    (h : t) -> Just h

chooseFirstColorOrYellow :: [Card] -> Color
chooseFirstColorOrYellow cardList
    | null cardList = Yellow
    | cardColor (head cardList) == Black = chooseFirstColorOrYellow (tail cardList)
    | otherwise = cardColor (head cardList)

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