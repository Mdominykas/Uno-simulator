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
