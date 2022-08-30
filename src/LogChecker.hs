module LogChecker where

import Data.MultiSet (MultiSet, member)
import qualified Data.MultiSet as MultiSet
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Card(Card (..), Color (..), hasNonBlackColor, cardColor, cardNumber, isValidResponse)
import Player (PlayerId, Player (..))
import GameLog (LogMessage(..))
import Data.Maybe (isNothing, isJust, fromJust)
import Control.Monad (when, unless)
import Data.Either (isLeft, isRight)
import Control.Monad.State ( evalState, State, runState )
import Control.Monad.Except (throwError, ExceptT, runExceptT)
import Control.Monad.State.Lazy ( get, put )
import Control.Monad.State.Lazy (modify)
import Debug.Trace (traceM)

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
    previousPlacement :: PreviousPlacement,
    cardHistory :: [Card]
    }

instance Show LogState where
    show logState = show (hands logState)

addPlayerToHands :: LogState -> PlayerId -> LogState
addPlayerToHands logState plId = logState{hands = Map.insert plId emptyHand (hands logState)}

addCardToLogStateHand :: LogState -> PlayerId -> Card -> LogState
addCardToLogStateHand logState idOfPlayer card = logState{hands = Map.insert idOfPlayer (Hand (MultiSet.insert card prevHand)) (hands logState)}
    where Hand prevHand = hands logState ! idOfPlayer

removeCardFromLogStateHand :: PlayerId -> Card -> ExceptT [Char] (State LogState) ()
removeCardFromLogStateHand idOfPlayer card = do
    logState <- get
    unless (Map.member idOfPlayer (hands logState)) (throwError handNotFound)
    let Hand prevHand = hands logState ! idOfPlayer
    unless (MultiSet.member card prevHand) (throwError playerHadntCard)
    put logState{hands = Map.insert idOfPlayer (Hand (MultiSet.delete card prevHand)) (hands logState)}

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
    previousPlacement = NotSelectedColor,
    cardHistory = []
    }

extendCardHistory :: Card -> LogState -> LogState
extendCardHistory card logState = logState{cardHistory = card : cardHistory logState}

addSkipping :: Card -> ExceptT [Char] (State LogState) ()
addSkipping  card = do
    logState <- get
    case card of
        PlusTwo _ -> put logState{hasToSkip = Just (getNextPlayer logState)}
        PlusFour -> put logState{hasToSkip = Just (getNextPlayer logState)}
        SkipTurn _ -> put logState{hasToSkip = Just (getNextPlayer logState)}
        _ -> return ()

addDrawing :: Card -> ExceptT [Char] (State LogState) ()
addDrawing card = do
    logState <- get
    let prevDraws = maybe 0 snd (hasToDraw logState)
    case card of
        PlusTwo _ -> put logState{hasToDraw = Just (getNextPlayer logState, prevDraws + 2)}
        PlusFour -> put logState{hasToDraw = Just (getNextPlayer logState, prevDraws + 4)}
        _ -> return ()

addColorChange :: Card -> ExceptT [Char] (State LogState) ()
addColorChange card = do
    logState <- get
    case card of
        ChangeColor -> put logState{needsToSetColor = True, changedColor = Nothing}
        PlusFour -> put logState{needsToSetColor = True, changedColor = Nothing}
        _ -> put logState{changedColor = Nothing}

addReversing :: Card -> ExceptT [Char] (State LogState) ()
addReversing card = do
    logState <- get
    case card of
        ReverseDirection _ -> put logState{waitingForReversal = True}
        _ -> return ()

processCard :: Card -> ExceptT [Char] (State LogState) ()
processCard card = do
    addReversing card
    addColorChange card
    addDrawing card
    addSkipping card

rotatePlayerOrder :: LogState -> LogState
rotatePlayerOrder logState = logState{orderOfPlayers = tail prevOrder ++ [head prevOrder]}
    where prevOrder = orderOfPlayers logState

getNextPlayer :: LogState -> PlayerId
getNextPlayer logState = if length (orderOfPlayers logState) < 2 then error "incorrect number of players" else head $ tail $ orderOfPlayers logState

getCurrentPlayer :: LogState -> PlayerId
getCurrentPlayer logState = if null (orderOfPlayers logState) then error "incorrect number of players" else head $ orderOfPlayers logState

checkLogs :: [LogMessage] -> [Player] -> Either [Char] LogState
checkLogs logMessages playerOrder = evalState (runExceptT (checkLogFromState logMessages)) (createInitialLogState playerIds)
    where
        playerIds = [playerId pl | pl <- playerOrder]

-- checkLogs :: [LogMessage] -> [Player] -> Either [Char] LogState

logStatePlayerIdMatches :: PlayerId -> ExceptT [Char] (State LogState) Bool
logStatePlayerIdMatches plId = do
    logState <- get
    if not (gameStarted logState) || head (orderOfPlayers logState) == plId
        then return True
        else throwError incorrectPlayerInLogs

drewRequiredCards :: ExceptT [Char] (State LogState) Bool
drewRequiredCards = do
    logState <- get
    let (pl, cardCount) = fromJust $ hasToDraw logState
    if isNothing (hasToDraw logState) || pl /= getCurrentPlayer logState || cardCount == 0
        then return True
        else throwError (didNotDrewCards ++ " there are " ++ show cardCount ++ " left")

checkFinalStateCorrectness :: ExceptT [Char] (State LogState) Bool
checkFinalStateCorrectness = do
    logState <- get
    let handsList = Map.elems (hands logState)
        emptyHandCount = length [h | Hand h <- handsList, MultiSet.null h]

    drewReq <- drewRequiredCards
    if gameStarted logState && drewReq && emptyHandCount == 1
        then return True
        else throwError incorrectFinalState

couldPlaced :: LogState -> Card -> Bool
couldPlaced logState card = case previousPlacement logState of
    NotSelectedColor -> False
    SelectedColor col -> not (hasNonBlackColor card) || (cardColor card == col)
    SimpleCard prevCard -> (cardNumber card == cardNumber prevCard) || (cardColor card == cardColor prevCard) || not (hasNonBlackColor card)

updatePreviousPlacementFromCard :: Card -> ExceptT [Char] (State LogState) ()
updatePreviousPlacementFromCard card = do
    logState <- get
    unless (couldPlaced logState card) (throwError impossibleCard)
    if hasNonBlackColor card
        then put logState{previousPlacement = SimpleCard card}
        else put logState{previousPlacement = NotSelectedColor}

playerHadCard :: Int -> Card -> ExceptT [Char] (State LogState) Bool
playerHadCard idOfPlayer card = do
    logState <- get
    let maybePlayer = Map.lookup idOfPlayer (hands logState)
    case maybePlayer of
        Nothing -> throwError incorrectPlayerInLogs
        Just (Hand cards) -> if MultiSet.member card cards then return True else throwError playerHadntCard

endOfTurnStateUpdate :: PlayerId -> ExceptT [Char] (State LogState) ()
endOfTurnStateUpdate plId = do
    drewRequiredCards
    logState <- get
    when (hasToSkip logState == Just plId) (put logState{hasToSkip = Nothing})

    logState <- get
    when (isJust (hasToDraw logState) && (fst . fromJust) (hasToDraw logState) == plId) (put logState{hasToDraw = Nothing})

    logState <- get
    when (needsToBeReversed logState) (put logState{needsToBeReversed = False, orderOfPlayers =  head (orderOfPlayers logState) : reverse (tail $ orderOfPlayers logState) })
    modify rotatePlayerOrder

    logState <- get
    when (waitingForReversal logState) (throwError orderWasNotRotated)
    when (previousPlacement logState == NotSelectedColor) (throwError colorNotSelected) -- sitas galimai gadins

updateRemainingDraws :: PlayerId ->  ExceptT [Char] (State LogState) LogState
updateRemainingDraws plId = do
    logState <- get
    case hasToDraw logState of
        Nothing -> return logState
        Just (drawingPlId, cardCount) ->
            if plId == drawingPlId
                then return logState{hasToDraw = Just (drawingPlId, cardCount - 1)}
                else throwError wrongPlayerDrawn

skippedPlayerDoesNotPlay :: PlayerId -> ExceptT [Char] (State LogState) ()
skippedPlayerDoesNotPlay plId = do
    logState <- get
    when (hasToSkip logState == Just plId) (throwError skippedMadeTurn)

isResponding :: PlayerId -> Card -> ExceptT [Char] (State LogState) Bool
isResponding plId card = do
    logState <- get
    -- traceM ("he need to draw: " ++ show (hasToDraw logState))
    return $ (hasToSkip logState == Just plId) && isValidResponse (head (cardHistory logState)) card

checkLogFromState :: [LogMessage] -> ExceptT [Char] (State LogState) LogState
checkLogFromState [] = do
    get
checkLogFromState (GameStart : remLogs) = do
    logState <- get
    when (gameStarted logState) (throwError gameAlreadyStarted)
    put logState{gameStarted = True}
    checkLogFromState remLogs
checkLogFromState (StartOfTurn plId : remLogs) = do
    logStatePlayerIdMatches  plId
    checkLogFromState remLogs
checkLogFromState (EndOfTurn plId : remLogs) = do
    logState <- get
    logStatePlayerIdMatches plId
    endOfTurnStateUpdate plId
    checkLogFromState remLogs
checkLogFromState (ShuffledDeck : remLogs) = do
    checkLogFromState remLogs
checkLogFromState (DrewCard plId card : remLogs) = do
    logState <- get
    logStatePlayerIdMatches plId
    logState <- updateRemainingDraws plId
    put (addCardToLogStateHand logState plId card)
    checkLogFromState remLogs
checkLogFromState (PlacedCard plId card : remLogs) = do
    logStatePlayerIdMatches  plId
    playerHadCard plId card
    isResp <- isResponding plId card
    unless isResp (skippedPlayerDoesNotPlay plId)
    processCard card
    removeCardFromLogStateHand plId card
    updatePreviousPlacementFromCard card
    modify (extendCardHistory card)
    checkLogFromState remLogs
checkLogFromState (SkippedTurn plId : remLogs) = do
    logState <- get
    logStatePlayerIdMatches plId
    unless (hasToSkip logState == Just plId) (throwError wrongPlayerSkipped)
    checkLogFromState remLogs
checkLogFromState (WonGame plId : remLogs) = do
    logState <- get
    logStatePlayerIdMatches plId
    checkFinalStateCorrectness
    if null remLogs
        then return logState
        else throwError wrongPlayerWon
checkLogFromState (ChangedColor plId col : remLogs) = do
    logState <- get
    unless (needsToSetColor logState) (throwError changedColorWhenCouldNot)
    unless (previousPlacement logState == NotSelectedColor) (throwError changedColorWhenCouldNot)
    when (col == Black) (throwError choseBlackColor)
    put logState{needsToSetColor = False, changedColor = Just col, previousPlacement = SelectedColor col}
    checkLogFromState remLogs
checkLogFromState (InitialCard card : remLogs) = do
    logState <- get
    modify (extendCardHistory card)
    put logState{previousPlacement = SimpleCard card}
    checkLogFromState remLogs
checkLogFromState (ReversedDirection plId : remLogs) = do
    logState <- get
    logStatePlayerIdMatches plId
    put logState{waitingForReversal = False, needsToBeReversed = True}
    checkLogFromState remLogs
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

choseBlackColor :: [Char]
choseBlackColor = "Player chosed black color"