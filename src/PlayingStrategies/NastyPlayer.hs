module PlayingStrategies.NastyPlayer where
import Card (Card, Color (Black))
import CardPlacement (CardPlacement)

-- I decided that it is better to let game play out exactly as it goes and then LogChecker will report an error,
-- since otherwise program may provide incorrect results. Therefore any of nasty player function will cause
-- LogChecker to report an error and stop experiment

nastyChooseCard :: [Card] -> CardPlacement -> Maybe Card
nastyChooseCard cards topPlacedCard = Just $ head cards

nastyChooseCol :: [Card] -> Color
nastyChooseCol cardList = Black

nastyRespondToActive :: [Card] -> Card -> Maybe Card
nastyRespondToActive cards activeCard = Just $ head cards