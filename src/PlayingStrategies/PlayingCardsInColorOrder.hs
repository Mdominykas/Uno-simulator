module PlayingStrategies.PlayingCardsInColorOrder where
import Card (Card(..), cardColor, Color (..))
import CardPlacement

colorOrderChoose :: [Card] -> CardPlacement -> Maybe Card
colorOrderChoose cards topPlacedCard = 
    if null possibleCards then Nothing else Just $ head possibleCards
        where
            redCards = [card | card <- cards, cardColor card == Red]
            greenCards = [card | card <- cards, cardColor card == Green]
            blueCards = [card | card <- cards, cardColor card == Blue]
            yellowCards = [card | card <- cards, cardColor card == Yellow]
            blackCards = [card | card <- cards, cardColor card == Black]
            possibleCards = [card | card <- redCards ++ greenCards ++ blueCards ++ yellowCards ++ blackCards, 
                placementFits topPlacedCard card]