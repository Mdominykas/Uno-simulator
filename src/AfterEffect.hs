module AfterEffect where
import Card (Card(..), Color)

-- AfterEffects deal with how game changes after player end his turn

data AfterEffect = NoTurn | Draw Int
    deriving (Show, Eq)

sumDrawCards :: [AfterEffect] -> Int
sumDrawCards [] = 0
sumDrawCards ((Draw n) : t) = n + sumDrawCards t
sumDrawCards (_ : t) = sumDrawCards t

generateAfterEffects :: Card -> [AfterEffect]
generateAfterEffects (Card _ _) = []
generateAfterEffects (SkipTurn _) = [NoTurn]
generateAfterEffects (PlusTwo _) = [NoTurn, Draw 2]
generateAfterEffects (ReverseDirection _) = []
generateAfterEffects ChangeColor = []
generateAfterEffects PlusFour = [NoTurn, Draw 4]