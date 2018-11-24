module Bob (responseFor) where

responseFor :: String -> String
responseFor msg
    | isYelling msg && isQuestion msg = "Calm down, I know what I'm doing!"
    | isYelling msg = "Whoa, chill out!"
    | isQuestion msg = "Sure."
    | isSilence msg = "Fine. Be that way!"
    | otherwise = "Whatever."

isQuestion :: String -> Bool
isQuestion [] = False
isQuestion msg = msgNoSpace /= "" && '?' == lastCharacter msgNoSpace
    where msgNoSpace = [x | x <- msg, x /= ' ']

lastCharacter :: [a] -> a
lastCharacter (x:[]) = x
lastCharacter (_:xs) = lastCharacter xs

isYelling :: String -> Bool
isYelling msg = letters /= "" && checkAllUpper letters
    where letters = [x | x <- msg, (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')]

checkAllUpper :: String -> Bool
checkAllUpper [] = True
checkAllUpper (x:xs) = (not (x >= 'a' && x <= 'z')) && checkAllUpper xs

isSilence :: String -> Bool
isSilence [] = True
isSilence (x:xs) = (x == ' ' || x == '\t' || x == '\n' || x == '\r') && isSilence xs