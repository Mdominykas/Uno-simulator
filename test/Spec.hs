module Main where

import Card(Color (..), Card (..), canPlace, cardColor, cardNumber)
import Player(Player (..), takeCardToHand, haveWon, cards, choose, playerId, chooseFirstMatching)
import GameState(deck, topCard, GameState (..), drawCardFromGameState, addAfterEffectsOfCard, addAfterEffectsToGameState, playerDrawCards, applyAfterEffects, placeCardIfPossible)
import AfterEffect(AfterEffect (..))
import Utils(remove, elementById,)

import Test.HUnit( assertEqual, runTestTT, Counts, Test(TestList, TestCase) )
import System.Random (mkStdGen, StdGen, Random (randomR))
import Control.Lens ( view, over )

sampleGameState :: GameState
sampleGameState = GameState{_deck = [Card Red 5, Card Blue 2, Card Green 3], _discardPile = [Card Blue 5, Card Yellow 8], _randomGenerator = mkStdGen 42, _afterEffects = [] }

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
        correctGameState = GameState{_deck = [Card Red 5, Card Blue 2, Card Green 3], _discardPile = [Card Red 5, Card Blue 5, Card Yellow 8], _randomGenerator = mkStdGen 42, _afterEffects = []}
        correctPlayer = Player{_playerId = 1, _cards = [Card Blue 2], _choose = chooseFirstMatching }
        correctResult = (True, (comparablePlayerParts correctPlayer, correctGameState))

test_PlaceCardIfPossible_WhenCanNotPlace :: Test
test_PlaceCardIfPossible_WhenCanNotPlace = TestCase (assertEqual "player plays card from hand" correctResult testResult)
    where
        (hasPlaced, (newPl, newGs)) = placeCardIfPossible samplePlayer1 sampleGameState
        testResult = (hasPlaced, (comparablePlayerParts newPl, newGs))
        correctGameState = GameState{_deck = [Card Red 5, Card Blue 2, Card Green 3], _discardPile = [Card Red 5, Card Blue 5, Card Yellow 8], _randomGenerator = mkStdGen 42, _afterEffects = []}
        correctPlayer = Player{_playerId = 1, _cards = [Card Blue 2], _choose = chooseFirstMatching }
        correctResult = (True, (comparablePlayerParts correctPlayer, correctGameState))

testListPlaceCardIfPossible :: [Test]
testListPlaceCardIfPossible = [test_PlaceCardIfPossible_WhenCanPlace]


test_AddAfterEffectsToGameState_WhenNoneAdded :: Test
test_AddAfterEffectsToGameState_WhenNoneAdded = TestCase (assertEqual "none afterEffects are added incorrectly" correctResult testResult)
    where
        correctResult = GameState{_deck = [Card Red 5, Card Blue 2, Card Green 3], _discardPile = [Card Blue 5, Card Yellow 8], _randomGenerator = mkStdGen 42, _afterEffects = [] }
        testResult = addAfterEffectsToGameState sampleGameState []

test_AddAfterEffectsToGameState_WhenMultipleAdded :: Test
test_AddAfterEffectsToGameState_WhenMultipleAdded = TestCase (assertEqual "multiple afterEffects are added incorrectly" correctResult testResult)
    where
        correctResult = GameState{_deck = [Card Red 5, Card Blue 2, Card Green 3], _discardPile = [Card Blue 5, Card Yellow 8], _randomGenerator = mkStdGen 42, _afterEffects = [NoTurn, Draw 5, NoTurn, Draw 4] }
        testResult = addAfterEffectsToGameState sampleGameState [NoTurn, Draw 5, NoTurn, Draw 4]

testListAddAfterEffectsToGameState :: [Test]
testListAddAfterEffectsToGameState = [test_AddAfterEffectsToGameState_WhenNoneAdded, test_AddAfterEffectsToGameState_WhenMultipleAdded]


test_AddAfterEffectsOfCard_CardWithNoAfterEffect :: Test
test_AddAfterEffectsOfCard_CardWithNoAfterEffect = TestCase (assertEqual "added no efects of simpleCard" correctResult testResult)
    where
        correctResult = GameState{_deck = [Card Red 5, Card Blue 2, Card Green 3], _discardPile = [Card Blue 5, Card Yellow 8], _randomGenerator = mkStdGen 42, _afterEffects = [] }
        testResult = addAfterEffectsOfCard sampleGameState (Card Red 3)

test_AddAfterEffectsOfCard_SkipTurnCard :: Test
test_AddAfterEffectsOfCard_SkipTurnCard = TestCase (assertEqual "added no efects of simpleCard" correctResult testResult)
    where
        correctResult = GameState{_deck = [Card Red 5, Card Blue 2, Card Green 3], _discardPile = [Card Blue 5, Card Yellow 8], _randomGenerator = mkStdGen 42, _afterEffects = [NoTurn] }
        testResult = addAfterEffectsOfCard sampleGameState (SkipTurn Green)

test_AddAfterEffectsOfCard_DrawCard :: Test
test_AddAfterEffectsOfCard_DrawCard = TestCase (assertEqual "added no efects of simpleCard" correctResult testResult)
    where
        correctResult = GameState{_deck = [Card Red 5, Card Blue 2, Card Green 3], _discardPile = [Card Blue 5, Card Yellow 8], _randomGenerator = mkStdGen 42, _afterEffects = [NoTurn, Draw 2] }
        testResult = addAfterEffectsOfCard sampleGameState (PlusTwo Yellow)

test_ApplyAfterEffects_WhenEmpty :: Test
test_ApplyAfterEffects_WhenEmpty = TestCase (assertEqual "no effects when there arent" correctResult (applyAfterEffects samplePlayer1 sampleGameState))
    where
        correctResult = (False, (samplePlayer1, sampleGameState))

test_ApplyAfterEffects_WhenSkipTurn :: Test        
test_ApplyAfterEffects_WhenSkipTurn = TestCase (assertEqual "player skips turn" correctResult (applyAfterEffects samplePlayer1 (addAfterEffectsToGameState sampleGameState [NoTurn])))
    where
        correctResult = (True, (samplePlayer1, sampleGameState))

-- TODO infinite loopas
test_ApplyAfterEffects_WhenDrawCards :: Test
test_ApplyAfterEffects_WhenDrawCards = TestCase (assertEqual "player skips turn" correctResult testResult)
    where
        correctResult = (True, playerDrawCards (samplePlayer1, sampleGameState) 2)
        testResult = applyAfterEffects samplePlayer1 (addAfterEffectsToGameState sampleGameState [Draw 1, Draw 1, NoTurn])

testListApplyAfterEffects :: [Test]
testListApplyAfterEffects = [test_ApplyAfterEffects_WhenEmpty, test_ApplyAfterEffects_WhenSkipTurn, test_ApplyAfterEffects_WhenDrawCards]

-- TODO parasyti realiu testu findWinner

main :: IO Counts
main = runTestTT $ TestList (testListChooseFirstMatching ++
                            testListDrawCardFromGameState ++
                            [test_TopCardFromGameState_WhenThereIsCard] ++
                            testListRemove ++
                            testListElementById ++
                            testListPlaceCardIfPossible ++
                            testListAddAfterEffectsToGameState ++
                            testListApplyAfterEffects)
