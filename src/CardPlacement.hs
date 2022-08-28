module CardPlacement where
import Card (Card(..), Color (Black), canPlace, cardColor)
import Control.Lens (view)
import Debug.Trace (trace)

data CardPlacement = Normal Card | WithColorChange Card Color
    deriving (Show, Eq)

canChangeColor :: Card -> Bool
canChangeColor ChangeColor = True
canChangeColor PlusFour = True
canChangeColor _ = False

placementFits :: CardPlacement -> Card -> Bool
placementFits (Normal topCard) card = canPlace topCard card
placementFits (WithColorChange topCard col) card = (cardColor card == Black) || (col == cardColor card)

getCardFromPlacement :: CardPlacement -> Card
getCardFromPlacement (Normal card) = card
getCardFromPlacement (WithColorChange card _) = card