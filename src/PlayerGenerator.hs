module PlayerGenerator where
import Player (PlayerId, Player (..))
import PlayingStrategies.PrimitivePlayer (chooseFirstMatching, chooseFirstColorOrYellow, alwaysRespondToActive)
import PlayingStrategies.NastyPlayer (nastyChooseCol, nastyChooseCard, nastyRespondToActive)
import PlayingStrategies.SavingBlackCards (savingChooseMatching)
import PlayingStrategies.PlayingCardsInColorOrder (colorOrderChoose)
import PlayingStrategies.ChoosingMostCommonColor (chooseMostFrequentColor)

generatePrimitivePlayer :: PlayerId -> Player
generatePrimitivePlayer id1 = Player{playerId = id1, cards = [], choose = chooseFirstMatching, chooseColor = chooseFirstColorOrYellow, respondToActive = alwaysRespondToActive}

generateNastyPlayer :: PlayerId -> Player
generateNastyPlayer id1 = Player{playerId = id1, cards = [], choose = nastyChooseCard, chooseColor = nastyChooseCol, respondToActive = nastyRespondToActive}

generateSavingBlackCardPlayer :: PlayerId -> Player
generateSavingBlackCardPlayer id1 = Player{playerId = id1, cards = [], choose = savingChooseMatching, chooseColor = chooseFirstColorOrYellow, respondToActive = alwaysRespondToActive}

generateColorOrderPlayer :: PlayerId -> Player
generateColorOrderPlayer id1 = Player{playerId = id1, cards = [], choose = colorOrderChoose, chooseColor = chooseFirstColorOrYellow, respondToActive = alwaysRespondToActive}

generatePlayerChoosingMostFrequentColor :: PlayerId -> Player
generatePlayerChoosingMostFrequentColor id1 = Player{playerId = id1, cards = [], choose = colorOrderChoose, chooseColor = chooseMostFrequentColor, respondToActive = alwaysRespondToActive}

generateGoodPlayer :: PlayerId -> Player
generateGoodPlayer id1 = Player{playerId = id1, cards = [], choose = savingChooseMatching, chooseColor = chooseMostFrequentColor, respondToActive = alwaysRespondToActive}


generatePrimitivePlayers :: Int -> [Player]
generatePrimitivePlayers count = [generatePrimitivePlayer id1 | id1 <- [0 .. (count - 1)]]

generateAllPrimitiveOneNasty :: Int -> [Player]
generateAllPrimitiveOneNasty count = generatePrimitivePlayers (count - 1) ++ [generateNastyPlayer (count - 1)]

generateAllPrimitiveOneSaving :: Int -> [Player]
generateAllPrimitiveOneSaving count = generatePrimitivePlayers (count - 1) ++ [generateSavingBlackCardPlayer (count - 1)]

generateAllPrimiteOneColorOrdering :: Int -> [Player]
generateAllPrimiteOneColorOrdering count = generatePrimitivePlayers (count - 1) ++ [generateColorOrderPlayer (count - 1)]

generateAllPrimitiveOneChoosingFreqColor :: Int -> [Player]
generateAllPrimitiveOneChoosingFreqColor count = generatePrimitivePlayers (count - 1) ++ [generatePlayerChoosingMostFrequentColor (count - 1)]

generateAllPrimitiveOneGood :: Int -> [Player]
generateAllPrimitiveOneGood count = generatePrimitivePlayers (count - 1) ++ [generateGoodPlayer (count - 1)]