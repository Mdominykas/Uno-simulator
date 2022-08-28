module GameLog where

import Player (PlayerId, Player, playerId)
import Card (Card (ReverseDirection), Color)
import CardPlacement (CardPlacement(..))
import Control.Monad.Writer

data LogMessage = GameStart
                | PlacedCard PlayerId Card 
                | DrewCard PlayerId Card 
                | SkippedTurn PlayerId 
                | ShuffledDeck
                | StartOfTurn PlayerId
                | EndOfTurn PlayerId
                | WonGame PlayerId
                | ChangedColor PlayerId Color
                | ReversedDirection PlayerId
                | InitialCard Card
                deriving (Show, Eq)

createPlacementLog :: Player -> CardPlacement -> [LogMessage]
createPlacementLog player (Normal (ReverseDirection col)) = [PlacedCard (playerId player) (ReverseDirection col), ReversedDirection (playerId player)]
createPlacementLog player (Normal card) = [PlacedCard (playerId player) card]
createPlacementLog player (WithColorChange card color) = [PlacedCard (playerId player) card, ChangedColor (playerId player) color]