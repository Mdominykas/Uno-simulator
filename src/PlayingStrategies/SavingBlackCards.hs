module PlayingStrategies.SavingBlackCards where

import Card
import CardPlacement (CardPlacement, placementFits)
import PlayingStrategies.PrimitivePlayer (chooseFirstMatching)
import Data.Maybe (isJust)

takeChangeColors [] = []
takeChangeColors (ChangeColor : t) = ChangeColor : takeChangeColors t
takeChangeColors (h:t) = takeChangeColors t

takePlusFours [] = []
takePlusFours (PlusFour : t) = PlusFour : takePlusFours t
takePlusFours (h:t) = takePlusFours t

savingChooseMatching :: [Card] -> CardPlacement -> Maybe Card
savingChooseMatching cards topPlacedCard
  | not (null nonBlackCards) = Just (head nonBlackCards)
  | not (null changeColorCards) = Just (head changeColorCards)
  | not (null plusFourCards) = Just $ head plusFourCards
  | otherwise = Nothing
  where
      changeColorCards = takeChangeColors cards
      plusFourCards = takePlusFours cards
      nonBlackCards = [card | card <- cards, hasNonBlackColor card, placementFits topPlacedCard card]

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