module PlayerGenerator where
import Player (PlayerId, Player (..))
import PlayingStrategies.PrimitivePlayer (chooseFirstMatching, chooseFirstColorOrYellow, alwaysRespondToActive)
import PlayingStrategies.NastyPlayer (nastyChooseCol, nastyChooseCard, nastyRespondToActive)
import PlayingStrategies.SavingBlackCards (savingChooseMatching)

generatePrimitivePlayer :: PlayerId -> Player
generatePrimitivePlayer id1 = Player{playerId = id1, cards = [], choose = chooseFirstMatching, chooseColor = chooseFirstColorOrYellow, respondToActive = alwaysRespondToActive}

generateNastyPlayer :: PlayerId -> Player
generateNastyPlayer id1 = Player{playerId = id1, cards = [], choose = nastyChooseCard, chooseColor = nastyChooseCol, respondToActive = nastyRespondToActive}

generateSavingBlackCardPlayer :: PlayerId -> Player
generateSavingBlackCardPlayer id1 = Player{playerId = id1, cards = [], choose = savingChooseMatching, chooseColor = chooseFirstColorOrYellow, respondToActive = alwaysRespondToActive}

generatePrimitivePlayers :: Int -> [Player]
generatePrimitivePlayers count = [generatePrimitivePlayer id1 | id1 <- [0 .. (count - 1)]]

generateAllPrimitiveOneNasty :: Int -> [Player]
generateAllPrimitiveOneNasty count = generatePrimitivePlayers (count - 1) ++ [generateNastyPlayer (count - 1)]

generateAllPrimitiveOneSaving :: Int -> [Player]
generateAllPrimitiveOneSaving count = generatePrimitivePlayers (count - 1) ++ [generateSavingBlackCardPlayer (count - 1)]
