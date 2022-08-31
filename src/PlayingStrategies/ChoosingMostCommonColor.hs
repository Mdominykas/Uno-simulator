module PlayingStrategies.ChoosingMostCommonColor where
import Card (Card(..), cardColor, Color (..))
import CardPlacement
import Data.List (sort)

chooseMostFrequentColor :: [Card] -> Color
chooseMostFrequentColor cardList = fst $ head colorList
        where
            redCards = [card | card <- cardList, cardColor card == Red]
            greenCards = [card | card <- cardList, cardColor card == Green]
            blueCards = [card | card <- cardList, cardColor card == Blue]
            yellowCards = [card | card <- cardList, cardColor card == Yellow]
            colorList = reverse $ sort [(Red, length redCards), (Green, length greenCards), (Blue, length blueCards), (Yellow, length yellowCards)]