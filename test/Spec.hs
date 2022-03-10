module Main where

import Lib (chooseFirstMatching, Card (Card), Color (Green, Blue, Red, Yellow), drawCardFromGameState, GameState (GameState, _deck, _discardPile, _randomGenerator), deck, topCard, remove, elementById, Player (Player, _playerId, _cards, _choose), cards, playerId, makeEveryTurn, placeCardIfPossible)
import Test.HUnit
import System.Random (mkStdGen, StdGen, Random (randomR))
import Control.Lens

sampleGameState :: GameState
sampleGameState = GameState{_deck = [Card Red 5, Card Blue 2, Card Green 3], _discardPile = [Card Blue 5, Card Yellow 8], _randomGenerator = mkStdGen 42 }

rngAfterUses :: StdGen -> Int -> StdGen
rngAfterUses rng 0 = rng
rngAfterUses rng n = rngAfterUses (snd $ randomR (0, n) rng) (n - 1)

samplePlayer1 :: Player
samplePlayer1 = Player{_playerId = 1, _cards = [Card Red 5, Card Blue 2], _choose = chooseFirstMatching }

samplePlayer2 :: Player
samplePlayer2 = Player{_playerId = 2, _cards = [Card Red 3], _choose = chooseFirstMatching }

samplePlayer3 :: Player
samplePlayer3 = Player{_playerId = 3, _cards = [Card Green 7, Card Red 2], _choose = chooseFirstMatching }

samplePlayerList1 :: [Player]
samplePlayerList1 = [samplePlayer1, samplePlayer3]

samplePlayerList2 :: [Player]
samplePlayerList2 = [samplePlayer1, samplePlayer2, samplePlayer3]

comparablePlayerParts :: Player -> (Int, [Card])
comparablePlayerParts pl = (view playerId pl, view cards pl)


test_ChooseFirstMatching_WithColor :: Test
test_ChooseFirstMatching_WithColor = TestCase (assertEqual "choose first card of same color" (Just $ Card Red 5) (chooseFirstMatching [Card Red 5, Card Blue 2, Card Green 3] (Card Red 6)))

test_ChooseFirstMatching_WithNumber :: Test
test_ChooseFirstMatching_WithNumber = TestCase (assertEqual "choose first card of number" (Just $ Card Blue 2) (chooseFirstMatching [Card Red 5, Card Blue 2, Card Green 3] (Card Green 2)))

test_ChooseFirstMatching_WhenNoneAvailable :: Test
test_ChooseFirstMatching_WhenNoneAvailable = TestCase (assertEqual "choose first card of number" Nothing (chooseFirstMatching [Card Red 5, Card Blue 2, Card Green 3] (Card Yellow 4)))

testListChooseFirstMatching :: [Test]
testListChooseFirstMatching = [test_ChooseFirstMatching_WithColor, test_ChooseFirstMatching_WithNumber, test_ChooseFirstMatching_WhenNoneAvailable]


test_DrawCardFromGameState_WhenCardsAreAvailableChoosesFirst :: Test
test_DrawCardFromGameState_WhenCardsAreAvailableChoosesFirst = TestCase (assertEqual "draw card from available" (Card Red 5) (fst $ drawCardFromGameState sampleGameState))

test_DrawCardFromGameState_WhenCardsAreAvailableUpdatesGameState :: Test
test_DrawCardFromGameState_WhenCardsAreAvailableUpdatesGameState = TestCase (assertEqual "update deck in gamestate" [Card Blue 2, Card Green 3] (view deck $ snd $ drawCardFromGameState sampleGameState))

testListDrawCardFromGameState :: [Test]
testListDrawCardFromGameState = [test_DrawCardFromGameState_WhenCardsAreAvailableChoosesFirst, test_DrawCardFromGameState_WhenCardsAreAvailableUpdatesGameState]


test_TopCardFromGameState_WhenThereIsCard :: Test
test_TopCardFromGameState_WhenThereIsCard = TestCase (assertEqual "return card when there is one" (Card Blue 5) (topCard sampleGameState))


test_Remove_WhenElementIs :: Test
test_Remove_WhenElementIs = TestCase (assertEqual "remove single element when it is" [1, 3, 2] (remove 2 [1, 2, 3, 2]))

test_Remove_WhenNoElement :: Test
test_Remove_WhenNoElement = TestCase (assertEqual "remove when element isn't there" [1, 2, 3, 2] (remove 5 [1, 2, 3, 2]))

testListRemove :: [Test]
testListRemove = [test_Remove_WhenElementIs, test_Remove_WhenNoElement]


test_ElementById_WhenItis :: Test
test_ElementById_WhenItis = TestCase (assertEqual "accessing element when it is" (Just 5) (elementById [1, 3, 5, 2, 4] 2))

test_ElementById_WhenItsTooMuch :: Test
test_ElementById_WhenItsTooMuch = TestCase (assertEqual "accessing element when it is" Nothing (elementById [1, 3, 5, 2, 4] 5))

testListElementById :: [Test]
testListElementById = [test_ElementById_WhenItis, test_ElementById_WhenItsTooMuch]


test_PlaceCardIfPossible_WhenCanPlace :: Test
test_PlaceCardIfPossible_WhenCanPlace = TestCase (assertEqual "player plays card from hand" correctResult testResult)
    where
        (hasPlaced, (newPl, newGs)) = placeCardIfPossible samplePlayer1 sampleGameState
        testResult = (hasPlaced, (comparablePlayerParts newPl, newGs))
        correctGameState = GameState{_deck = [Card Red 5, Card Blue 2, Card Green 3], _discardPile = [Card Red 5, Card Blue 5, Card Yellow 8], _randomGenerator = mkStdGen 42 }
        correctPlayer = Player{_playerId = 1, _cards = [Card Blue 2], _choose = chooseFirstMatching }
        correctResult = (True, (comparablePlayerParts correctPlayer, correctGameState))

test_PlaceCardIfPossible_WhenCanNotPlace :: Test
test_PlaceCardIfPossible_WhenCanNotPlace = TestCase (assertEqual "player plays card from hand" correctResult testResult)
    where
        (hasPlaced, (newPl, newGs)) = placeCardIfPossible samplePlayer1 sampleGameState
        testResult = (hasPlaced, (comparablePlayerParts newPl, newGs))
        correctGameState = GameState{_deck = [Card Red 5, Card Blue 2, Card Green 3], _discardPile = [Card Red 5, Card Blue 5, Card Yellow 8], _randomGenerator = mkStdGen 42 }
        correctPlayer = Player{_playerId = 1, _cards = [Card Blue 2], _choose = chooseFirstMatching }
        correctResult = (True, (comparablePlayerParts correctPlayer, correctGameState))

testListPlaceCardIfPossible :: [Test]
testListPlaceCardIfPossible = [test_PlaceCardIfPossible_WhenCanPlace]

test_MakeEveryTurn_WhenEveryoneHasCards :: Test
test_MakeEveryTurn_WhenEveryoneHasCards = TestCase (assertEqual "playing one round" (correctComparablePlayerList, correctGameState) (comparablePlayerList, (snd $ fst result)))
    where
        result = makeEveryTurn samplePlayerList1 sampleGameState
        comparablePlayerList = map comparablePlayerParts (fst $ fst result)
        correctComparablePlayerList = [(1, [Card Blue 2]), (3, [Card Green 7])]
        correctGameState = GameState{_deck = [Card Red 5, Card Blue 2, Card Green 3], _discardPile = [Card Red 2, Card Red 5, Card Blue 5, Card Yellow 8], _randomGenerator = mkStdGen 42 }

testListMakeEveryTurn :: [Test]
testListMakeEveryTurn = [test_MakeEveryTurn_WhenEveryoneHasCards]

main :: IO Counts
main = runTestTT $ TestList (testListChooseFirstMatching ++
                            testListDrawCardFromGameState ++
                            [test_TopCardFromGameState_WhenThereIsCard] ++
                            testListRemove ++
                            testListElementById ++
                            testListPlaceCardIfPossible
                            )
                            --testListMakeEveryTurn)
