module GameLog where
import Player (PlayerId)
import Card (Card)

data LogMessage = PlacedCard PlayerId Card 
                | DrewCard PlayerId Card 
                | SkippedTurn PlayerId 
                | ShuffledDeck
                | StartOfTurn PlayerId
                | EndOfTurn PlayerId
                | WonGame PlayerId
                deriving (Show)
