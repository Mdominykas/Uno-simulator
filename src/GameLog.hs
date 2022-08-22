module GameLog where
import Player (PlayerId, Player, playerId)
import Card (Card, Color)
import CardPlacement (CardPlacement(..))
import Control.Lens (view)
import Control.Monad.Writer

data LogMessage = PlacedCard PlayerId Card 
                | DrewCard PlayerId Card 
                | SkippedTurn PlayerId 
                | ShuffledDeck
                | StartOfTurn PlayerId
                | EndOfTurn PlayerId
                | WonGame PlayerId
                | ChangedColor PlayerId Color
                deriving (Show, Eq)

createPlacementLog :: Player -> CardPlacement -> [LogMessage]
createPlacementLog player (Normal card) = [PlacedCard (view playerId player) card]
createPlacementLog player (WithColorChange card color) = [PlacedCard (view playerId player) card, ChangedColor (view playerId player) color]