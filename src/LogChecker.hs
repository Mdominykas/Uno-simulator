module LogChecker where

import Data.MultiSet (MultiSet, member)
import qualified Data.MultiSet as MultiSet
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Card(Card (..), Color, hasNonBlackColor, cardColor, cardNumber)
import Player (PlayerId, Player (..))
import GameLog (LogMessage(..))
import Data.Maybe (isNothing, isJust, fromJust)
import Control.Monad (when, unless)
import Data.Either (isLeft, isRight)

newtype Hand = Hand (MultiSet Card)

instance Show Hand where
    show (Hand h) = show h

-- kazin ar labai reikalingas modulis, nes Haskelio kodas gan neblogai patikrina savybes
-- bei pagal tai, kad nera side effectu galima speti, kad likes kodas neturetu sugadinti
-- taciau padejo rasti nemazai bugu, tai gal ir naudingas

emptyHand :: Hand
emptyHand = Hand MultiSet.empty

data PreviousPlacement = NotSelectedColor | SelectedColor Color | SimpleCard Card
    deriving (Eq)

data LogState = LogState
    {
    hands :: Map PlayerId Hand,
    gameStarted :: Bool,
    hasToSkip :: Maybe PlayerId,
    hasToDraw :: Maybe (PlayerId, Int),
    changedColor :: Maybe Color,
    needsToSetColor :: Bool,
    waitingForReversal :: Bool,
    needsToBeReversed :: Bool,
    orderOfPlayers :: [PlayerId],
    previousPlacement :: PreviousPlacement
    }

instance Show LogState where
    show logState = show (hands logState)

addPlayerToHands :: LogState -> PlayerId -> LogState
addPlayerToHands logState plId = logState{hands = Map.insert plId emptyHand (hands logState)}

addCardToLogStateHand :: LogState -> PlayerId -> Card -> LogState
addCardToLogStateHand logState idOfPlayer card = logState{hands = Map.insert idOfPlayer (Hand (MultiSet.insert card prevHand)) (hands logState)}
    where Hand prevHand = hands logState ! idOfPlayer

removeCardFromLogStateHand :: LogState -> PlayerId -> Card -> Either [Char] LogState
removeCardFromLogStateHand logState idOfPlayer card = do
    unless (Map.member idOfPlayer (hands logState)) (Left handNotFound)
    unless (MultiSet.member card prevHand) (Left playerHadntCard)
    Right logState{hands = Map.insert idOfPlayer (Hand (MultiSet.delete card prevHand)) (hands logState)}
        where Hand prevHand = hands logState ! idOfPlayer

initialHands :: [PlayerId] -> Map PlayerId Hand
initialHands = foldr (`Map.insert` emptyHand) Map.empty

createInitialLogState :: [PlayerId] -> LogState
createInitialLogState playerOrder =
    LogState{hands = initialHands playerOrder,
    gameStarted = False,
    hasToSkip = Nothing,
    hasToDraw = Nothing,
    changedColor = Nothing,
    needsToSetColor = False,
    orderOfPlayers = playerOrder,
    waitingForReversal = False,
    needsToBeReversed = False,
    previousPlacement = NotSelectedColor}

addSkipping :: LogState -> Card -> LogState
addSkipping logState card = case card of
    PlusTwo _ -> logState{hasToSkip = Just (getNextPlayer logState)}
    PlusFour -> logState{hasToSkip = Just (getNextPlayer logState)}
    SkipTurn _ -> logState{hasToSkip = Just (getNextPlayer logState)}
    _ -> logState

addDrawing :: LogState -> Card -> LogState
addDrawing logState card = case card of
    PlusTwo _ -> logState{hasToDraw = Just (getNextPlayer logState, 2)}
    PlusFour -> logState{hasToDraw = Just (getNextPlayer logState, 4)}
    _ -> logState

addColorChange :: LogState -> Card -> LogState
addColorChange logState card = case card of
    ChangeColor -> logState{needsToSetColor = True, changedColor = Nothing}
    PlusFour -> logState{needsToSetColor = True, changedColor = Nothing}
    _ -> logState{changedColor = Nothing}

addReversing :: LogState -> Card -> LogState
addReversing logState card = case card of
    ReverseDirection _ -> logState{waitingForReversal = True}
    _ -> logState

processCard :: LogState -> Card -> LogState
processCard logState card = addReversing (addColorChange (addDrawing (addSkipping logState card) card) card) card

rotatePlayerOrder :: LogState -> LogState
rotatePlayerOrder logState = logState{orderOfPlayers = tail prevOrder ++ [head prevOrder]}
    where prevOrder = orderOfPlayers logState

getNextPlayer :: LogState -> PlayerId
getNextPlayer logState = if length (orderOfPlayers logState) < 2 then error "incorrect number of players" else head $ tail $ orderOfPlayers logState

getCurrentPlayer :: LogState -> PlayerId
getCurrentPlayer logState = if null (orderOfPlayers logState) then error "incorrect number of players" else head $ orderOfPlayers logState

checkLogs :: [LogMessage] -> [Player] -> Either [Char] LogState
checkLogs logMessages playerOrder = checkLogFromState (createInitialLogState playerIds) logMessages
    where
        playerIds = [playerId pl | pl <- playerOrder]

logStatePlayerIdMatches :: LogState -> PlayerId -> Either [Char] Bool
logStatePlayerIdMatches logState plId =
    if not (gameStarted logState) || head (orderOfPlayers logState) == plId
        then Right True
        else Left incorrectPlayerInLogs

drewRequiredCards :: LogState -> Either [Char] Bool
drewRequiredCards logState =
    if isNothing (hasToDraw logState) || pl /= getCurrentPlayer logState || cardCount == 0
        then Right True
        else Left didNotDrewCards
            where (pl, cardCount) = fromJust $ hasToDraw logState

correctFinalState :: LogState -> Either [Char] Bool
correctFinalState logState =
    if gameStarted logState && isRight (drewRequiredCards logState) && emptyHandCount == 1
        then Right True
        else Left incorrectFinalState
            where
                handsList = Map.elems (hands logState)
                emptyHandCount = length [h | Hand h <- handsList, MultiSet.null h]

couldPlaced :: LogState -> Card -> Bool
couldPlaced logState card = case previousPlacement logState of
    NotSelectedColor -> False
    SelectedColor col -> not (hasNonBlackColor card) || (cardColor card == col)
    SimpleCard prevCard -> (cardNumber card == cardNumber prevCard) || (cardColor card == cardColor prevCard) || not (hasNonBlackColor card)

updatePreviousPlacementFromCard :: LogState -> Card -> Either [Char] LogState
updatePreviousPlacementFromCard logState card = do
    unless (couldPlaced logState card) (Left impossibleCard)
    if hasNonBlackColor card
        then Right logState{previousPlacement = SimpleCard card}
        else Right logState{previousPlacement = NotSelectedColor}

playerHadCard :: LogState -> Int -> Card -> Either [Char] Bool
playerHadCard logState idOfPlayer card = case maybePlayer of
    Nothing -> Left incorrectPlayerInLogs
    Just (Hand cards) -> if MultiSet.member card cards then Right True else Left playerHadntCard
    where
        maybePlayer = Map.lookup idOfPlayer (hands logState)

endOfTurnStateUpdate :: LogState -> Int -> Either [Char] LogState
endOfTurnStateUpdate logState plId = do
    drewRequiredCards logState
    let skipFixedState = if hasToSkip logState == Just plId
            then logState{hasToSkip = Nothing}
            else logState
    let drawFixedState = if isJust (hasToDraw skipFixedState) && (fst . fromJust) (hasToDraw skipFixedState) == plId
        then skipFixedState{hasToDraw = Nothing}
        else skipFixedState
    let rotationFixedState = if needsToBeReversed drawFixedState
        then rotatePlayerOrder skipFixedState{needsToBeReversed = False, orderOfPlayers =  head (orderOfPlayers skipFixedState) : reverse (tail $ orderOfPlayers skipFixedState) }
        else rotatePlayerOrder drawFixedState
    when (waitingForReversal rotationFixedState) (Left orderWasNotRotated)
    when (previousPlacement logState == NotSelectedColor) (Left colorNotSelected)
    Right $ rotationFixedState

updateRemainingDraws :: LogState -> PlayerId -> Either [Char] LogState
updateRemainingDraws logState plId = case hasToDraw logState of
    Nothing -> Right logState
    Just (drawingPlId, cardCount) ->
        if plId == drawingPlId
            then Right logState{hasToDraw = Just (drawingPlId, cardCount - 1)}
            else Left wrongPlayerDrawn

skippedPlayerDoesNotPlay :: LogState -> PlayerId -> Either [Char] ()
skippedPlayerDoesNotPlay logState plId = do
    when (hasToSkip logState == Just plId) (Left skippedMadeTurn)

checkLogFromState :: LogState -> [LogMessage] -> Either [Char] LogState
checkLogFromState logState [] = Right logState
checkLogFromState logState (GameStart : remLogs) = do
    when (gameStarted logState) (Left gameAlreadyStarted)
    checkLogFromState logState{gameStarted = True} remLogs
checkLogFromState logState (StartOfTurn plId : remLogs) = do
    logStatePlayerIdMatches logState plId
    checkLogFromState logState remLogs
checkLogFromState logState (EndOfTurn plId : remLogs) = do
    logStatePlayerIdMatches logState plId
    newLogState <- endOfTurnStateUpdate logState plId
    checkLogFromState newLogState remLogs
checkLogFromState logState (ShuffledDeck : remLogs) = do
    checkLogFromState logState remLogs
checkLogFromState logState (DrewCard plId card : remLogs) = do
    logStatePlayerIdMatches logState plId
    logState <- updateRemainingDraws logState plId
    checkLogFromState (addCardToLogStateHand logState plId card) remLogs
checkLogFromState logState (PlacedCard plId card : remLogs) = do
    logStatePlayerIdMatches logState plId
    playerHadCard logState plId card
    skippedPlayerDoesNotPlay logState plId
    newLogState <- removeCardFromLogStateHand cardProcessedState plId card
    finalLogState <- updatePreviousPlacementFromCard newLogState card
    checkLogFromState finalLogState remLogs
        where cardProcessedState = processCard logState card
checkLogFromState logState (SkippedTurn plId : remLogs) = do
    logStatePlayerIdMatches logState plId
    unless (hasToSkip logState == Just plId) (Left wrongPlayerSkipped)
    checkLogFromState logState remLogs
checkLogFromState logState (WonGame plId : remLogs) = do
    logStatePlayerIdMatches logState plId
    correctFinalState logState
    if null remLogs
        then Right logState
        else Left wrongPlayerWon
checkLogFromState logState (ChangedColor plId col : remLogs) = do
    unless (needsToSetColor logState) (Left changedColorWhenCouldNot)
    unless (previousPlacement logState == NotSelectedColor) (Left changedColorWhenCouldNot)
    checkLogFromState logState{needsToSetColor = False, changedColor = Just col, previousPlacement = SelectedColor col} remLogs
checkLogFromState logState (InitialCard card : remLogs) = do
    checkLogFromState logState{previousPlacement = SimpleCard card} remLogs
checkLogFromState logState (ReversedDirection plId : remLogs) = do
    logStatePlayerIdMatches logState plId
    checkLogFromState logState{waitingForReversal = False, needsToBeReversed = True} remLogs
-- checkLogFromState logState logs = Left unexpectedLogType

unexpectedLogType :: [Char]
unexpectedLogType = "Unexpected type of logs entry"

gameAlreadyStarted :: [Char]
gameAlreadyStarted = "Game has already started"

incorrectPlayerInLogs :: [Char]
incorrectPlayerInLogs = "incorrect player move in logs"

didNotDrewCards :: [Char]
didNotDrewCards = "Player did not drew required cards"

incorrectFinalState :: [Char]
incorrectFinalState = "Final State is not correct"

playerHadntCard :: [Char]
playerHadntCard = "Player did not have placed card"

wrongPlayerSkipped :: [Char]
wrongPlayerSkipped = "Wrong player has skipped turn"

wrongPlayerWon :: [Char]
wrongPlayerWon = "Wrong player has won the game"

handNotFound :: [Char]
handNotFound = "Hand does not exists in the state of gameLogs"

changedColorWhenCouldNot :: [Char]
changedColorWhenCouldNot = "Player changed color when he could not"

drewTooMany :: [Char]
drewTooMany = "Player has drawn too many cards"

skippedMadeTurn :: [Char]
skippedMadeTurn = "Player that was skipped made a move"

wrongPlayerDrawn :: [Char]
wrongPlayerDrawn = "Wrong player has drawn cards"

impossibleCard :: [Char]
impossibleCard = "Player placed impossible card"

colorNotSelected :: [Char]
colorNotSelected = "Color was not selected"

orderWasNotRotated :: [Char]
orderWasNotRotated = "Rotation card was placed, but it was not rotated"