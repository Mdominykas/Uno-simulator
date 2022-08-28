module Main where

import Card(Color (..), Card (..), canPlace, cardColor, cardNumber)
import Player(Player (..), takeCardToHand, haveWon, cards, choose, playerId, chooseFirstMatching, chooseFirstColorOrYellow)
import GameState(deck, GameState (..), takeCardFromGameState, addAfterEffectsOfCard, addAfterEffectsOfCard, playerDrawCards, applyAfterEffects, placeCardIfPossible)
import CardPlacement (CardPlacement (Normal, WithColorChange))
import AfterEffect(AfterEffect (..))
import Utils(removeOne, elementById,)

import Control.Monad.Writer (WriterT, Writer, MonadWriter (tell), runWriter)
import Control.Lens ( view, over )
import System.Random (mkStdGen, StdGen, Random (randomR))
import Test.HUnit( assertEqual, runTestTT, Counts, Test(TestList, TestCase) )

sampleGameState :: GameState
sampleGameState = GameState{deck = [Card Red 5, Card Blue 2, Card Green 3], discardPile = [Card Blue 5, Card Yellow 8], randomGenerator = mkStdGen 42, afterEffects = [], topCardPlacement = Normal (Card Red 5)}

rngAfterUses :: StdGen -> Int -> StdGen
rngAfterUses rng 0 = rng
rngAfterUses rng n = rngAfterUses (snd $ randomR (0, n) rng) (n - 1)

samplePlayer1 :: Player
samplePlayer1 = Player{playerId = 1, cards = [Card Red 5, Card Blue 2], choose = chooseFirstMatching, chooseColor = chooseFirstColorOrYellow }

samplePlayer2 :: Player
samplePlayer2 = Player{playerId = 2, cards = [Card Red 3], choose = chooseFirstMatching, chooseColor = chooseFirstColorOrYellow }

samplePlayer3 :: Player
samplePlayer3 = Player{playerId = 3, cards = [Card Green 7, Card Red 2], choose = chooseFirstMatching, chooseColor = chooseFirstColorOrYellow}

samplePlayerList1 :: [Player]
samplePlayerList1 = [samplePlayer1, samplePlayer3]

samplePlayerList2 :: [Player]
samplePlayerList2 = [samplePlayer1, samplePlayer2, samplePlayer3]

comparablePlayerParts :: Player -> (Int, [Card])
comparablePlayerParts pl = (playerId pl, cards pl)


test_ChooseFirstMatching_WithColor :: Test
test_ChooseFirstMatching_WithColor = TestCase (assertEqual "choose first card of same color" (Just $ Card Red 5) (chooseFirstMatching [Card Red 5, Card Blue 2, Card Green 3] (Normal (Card Red 6))))

test_ChooseFirstMatching_WithNumber :: Test
test_ChooseFirstMatching_WithNumber = TestCase (assertEqual "choose first card of number" (Just $ Card Blue 2) (chooseFirstMatching [Card Red 5, Card Blue 2, Card Green 3] (Normal (Card Green 2))))

test_ChooseFirstMatching_WhenNoneAvailable :: Test
test_ChooseFirstMatching_WhenNoneAvailable = TestCase (assertEqual "choose first card of number" Nothing (chooseFirstMatching [Card Red 5, Card Blue 2, Card Green 3] (Normal (Card Yellow 4))))

testListChooseFirstMatching :: [Test]
testListChooseFirstMatching = [test_ChooseFirstMatching_WithColor, test_ChooseFirstMatching_WithNumber, test_ChooseFirstMatching_WhenNoneAvailable]


test_TakeCardFromGameState_WhenCardsAreAvailableChoosesFirst :: Test
test_TakeCardFromGameState_WhenCardsAreAvailableChoosesFirst = TestCase (assertEqual "draw take from available" (Card Red 5) (fst $ takeCardFromGameState sampleGameState))

test_TakeCardFromGameState_WhenCardsAreAvailableUpdatesGameState :: Test
test_TakeCardFromGameState_WhenCardsAreAvailableUpdatesGameState = TestCase (assertEqual "update deck in gamestate" [Card Blue 2, Card Green 3] (deck $ snd $ takeCardFromGameState sampleGameState))

testListTakeCardFromGameState :: [Test]
testListTakeCardFromGameState = [test_TakeCardFromGameState_WhenCardsAreAvailableChoosesFirst, test_TakeCardFromGameState_WhenCardsAreAvailableUpdatesGameState]


test_Remove_WhenElementIs :: Test
test_Remove_WhenElementIs = TestCase (assertEqual "remove single element when it is" [1, 3, 2] (removeOne 2 [1, 2, 3, 2]))

test_Remove_WhenNoElement :: Test
test_Remove_WhenNoElement = TestCase (assertEqual "remove when element isn't there" [1, 2, 3, 2] (removeOne 5 [1, 2, 3, 2]))

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
        (hasPlaced, (newPl, newGs)) = fst $ runWriter $ placeCardIfPossible samplePlayer1 sampleGameState
        testResult = (hasPlaced, (comparablePlayerParts newPl, newGs))
        correctGameState = GameState{deck = [Card Red 5, Card Blue 2, Card Green 3], discardPile = [Card Red 5, Card Blue 5, Card Yellow 8], randomGenerator = mkStdGen 42, afterEffects = [], topCardPlacement = Normal (Card Red 5)}
        correctPlayer = Player{playerId = 1, cards = [Card Blue 2], choose = chooseFirstMatching }
        correctResult = (True, (comparablePlayerParts correctPlayer, correctGameState))

test_PlaceCardIfPossible_WhenCanNotPlace :: Test
test_PlaceCardIfPossible_WhenCanNotPlace = TestCase (assertEqual "player plays card from hand" correctResult testResult)
    where
        (hasPlaced, (newPl, newGs)) = fst $ runWriter $ placeCardIfPossible samplePlayer1 sampleGameState
        testResult = (hasPlaced, (comparablePlayerParts newPl, newGs))
        correctGameState = GameState{deck = [Card Red 5, Card Blue 2, Card Green 3], discardPile = [Card Red 5, Card Blue 5, Card Yellow 8], randomGenerator = mkStdGen 42, afterEffects = [], topCardPlacement = Normal (Card Red 5)}
        correctPlayer = Player{playerId = 1, cards = [Card Blue 2], choose = chooseFirstMatching }
        correctResult = (True, (comparablePlayerParts correctPlayer, correctGameState))

testListPlaceCardIfPossible :: [Test]
testListPlaceCardIfPossible = [test_PlaceCardIfPossible_WhenCanPlace]


test_addAfterEffectsOfCard_WhenNoneAdded :: Test
test_addAfterEffectsOfCard_WhenNoneAdded = TestCase (assertEqual "none afterEffects are added incorrectly" correctResult testResult)
    where
        correctResult = GameState{deck = [Card Red 5, Card Blue 2, Card Green 3], discardPile = [Card Blue 5, Card Yellow 8], randomGenerator = mkStdGen 42, afterEffects = [], topCardPlacement = Normal (Card Red 5)}
        testResult = addAfterEffectsOfCard sampleGameState (Card Red 5)

test_addAfterEffectsOfCard_WhenMultipleAdded :: Test
test_addAfterEffectsOfCard_WhenMultipleAdded = TestCase (assertEqual "multiple afterEffects are added incorrectly" correctResult testResult)
    where
        correctResult = GameState{deck = [Card Red 5, Card Blue 2, Card Green 3], discardPile = [Card Blue 5, Card Yellow 8], randomGenerator = mkStdGen 42, afterEffects = [NoTurn, Draw 4], topCardPlacement = Normal (Card Red 5)}
        testResult = addAfterEffectsOfCard sampleGameState PlusFour

testListaddAfterEffectsOfCard :: [Test]
testListaddAfterEffectsOfCard = [test_addAfterEffectsOfCard_WhenNoneAdded, test_addAfterEffectsOfCard_WhenMultipleAdded]


test_AddAfterEffectsOfCard_CardWithNoAfterEffect :: Test
test_AddAfterEffectsOfCard_CardWithNoAfterEffect = TestCase (assertEqual "added no efects of simpleCard" correctResult testResult)
    where
        correctResult = GameState{deck = [Card Red 5, Card Blue 2, Card Green 3], discardPile = [Card Blue 5, Card Yellow 8], randomGenerator = mkStdGen 42, afterEffects = [], topCardPlacement = Normal (Card Red 5)}
        testResult = addAfterEffectsOfCard sampleGameState (Card Red 3)

test_AddAfterEffectsOfCard_SkipTurnCard :: Test
test_AddAfterEffectsOfCard_SkipTurnCard = TestCase (assertEqual "added no efects of simpleCard" correctResult testResult)
    where
        correctResult = GameState{deck = [Card Red 5, Card Blue 2, Card Green 3], discardPile = [Card Blue 5, Card Yellow 8], randomGenerator = mkStdGen 42, afterEffects = [NoTurn], topCardPlacement = Normal (Card Red 5)}
        testResult = addAfterEffectsOfCard sampleGameState (SkipTurn Green)

test_AddAfterEffectsOfCard_DrawCard :: Test
test_AddAfterEffectsOfCard_DrawCard = TestCase (assertEqual "added no efects of simpleCard" correctResult testResult)
    where
        correctResult = GameState{deck = [Card Red 5, Card Blue 2, Card Green 3], discardPile = [Card Blue 5, Card Yellow 8], randomGenerator = mkStdGen 42, afterEffects = [NoTurn, Draw 2], topCardPlacement = Normal (Card Red 5)}
        testResult = addAfterEffectsOfCard sampleGameState (PlusTwo Yellow)

test_ApplyAfterEffects_WhenEmpty :: Test
test_ApplyAfterEffects_WhenEmpty = TestCase (assertEqual "no effects when there arent" correctResult testResult)
    where
        testResult = fst $ runWriter $ applyAfterEffects samplePlayer1 sampleGameState
        correctResult = (False, (samplePlayer1, sampleGameState))

test_ApplyAfterEffects_WhenSkipTurn :: Test        
test_ApplyAfterEffects_WhenSkipTurn = TestCase (assertEqual "player skips turn" correctResult testResult)
    where
        testResult = fst $ runWriter $ applyAfterEffects samplePlayer1 (addAfterEffectsOfCard sampleGameState (SkipTurn Blue))
        correctResult = (True, (samplePlayer1, sampleGameState))

test_ApplyAfterEffects_WhenDrawCards :: Test
test_ApplyAfterEffects_WhenDrawCards = TestCase (assertEqual "player skips turn" correctResult testResult)
    where
        correctResult = (True, fst $ runWriter $ playerDrawCards (samplePlayer1, sampleGameState) 2)
        testResult = fst $ runWriter $ applyAfterEffects samplePlayer1 (addAfterEffectsOfCard sampleGameState (PlusTwo Green))

testListApplyAfterEffects :: [Test]
testListApplyAfterEffects = [test_ApplyAfterEffects_WhenEmpty, test_ApplyAfterEffects_WhenSkipTurn, test_ApplyAfterEffects_WhenDrawCards]

main :: IO Counts
main = runTestTT $ TestList (testListChooseFirstMatching ++
                            testListTakeCardFromGameState ++
                            testListRemove ++
                            testListElementById ++
                            testListPlaceCardIfPossible ++
                            testListaddAfterEffectsOfCard ++
                            testListApplyAfterEffects)
