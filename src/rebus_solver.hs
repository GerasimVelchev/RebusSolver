import Data.Char
import System.IO

-- Just helper functions for the map operations. Their names describe their functionality.

type Mapping = [(Char, Int)]

addIfNotExist :: (Char, Int) -> Mapping -> Mapping
addIfNotExist a theMap = if (containsInMap (fst a) theMap) then theMap else (theMap ++ [a])

getValueFromMap :: Char -> Mapping -> Int
getValueFromMap x theMap =
               if (fst (head theMap) == x) then snd (head theMap)
               else getValueFromMap x (tail theMap)

containsInMap :: Char -> Mapping -> Bool
containsInMap x theMap = if (null theMap) then False
                         else if (fst (head theMap) == x) then True
                         else containsInMap x (tail theMap)

removeIfExist :: Int -> [Int] -> [Int]
removeIfExist x lst =
               if (null lst) then []
               else if (head lst == x) then (tail lst)
               else ([head lst] ++ removeIfExist x (tail lst))

removeArray :: [Int] -> [Int] -> [Int]
removeArray x lst = 
    if (null x) then lst
    else removeIfExist (head x) (removeArray (tail x) lst)

addArray :: Mapping -> Mapping -> Mapping
addArray x lst = 
    if (null x) then lst
    else addIfNotExist (head x) (addArray (tail x) lst)

-- Decart product of two arrays.
decartProduct :: [Int] -> [Int] -> [(Int, Int)]
decartProduct a b = concatMap (\x -> (map (\y -> (x, y)) b)) a

-- Returns True if two variables are equal and False otherwise.
equality :: Eq a => a -> a -> Bool
equality c d = if (c == d) then True else False

-- Given an array of letters and array of their corresponding digits. 
-- Tells whether there is a conflict in the mapping.
notConflicts :: [Char] -> [Int] -> Bool
notConflicts a b = (   equality c1 c2 == equality i1 i2
                    && equality c1 c3 == equality i1 i3 
                    && equality c2 c3 == equality i2 i3)

    where c1 = a !! 0
          c2 = a !! 1
          c3 = a !! 2
          i1 = b !! 0
          i2 = b !! 1
          i3 = b !! 2

-- Return the integer corresponding to a letter according to a particular mapping.
replaceLetterWithInt :: Mapping -> ( Char -> Char )
replaceLetterWithInt mapping = (\x -> chr (48 + getValueFromMap x mapping))

-- Given the three words of the cryptogramme and a mapping. Returns the word presentations in digits.
getOutput :: String -> String -> String -> Mapping -> String
getOutput word1 word2 word3 mapping = (map (replaceLetterWithInt mapping) word1) ++ "+" ++ 
                                      (map (replaceLetterWithInt mapping) word2) ++ "=" ++ 
                                      (map (replaceLetterWithInt mapping) word3)

-- Calculates the solution for the state.
-- The state is described by:
-- 1) the three words - parts of the numbers that leave to be processed,
-- 2) leftToAdd - the value of what we call in Bulgarian "наум",
-- 3) freeDigits - integer array of the digits that have not been used yet,
-- 4) letterToDigitMapping - the mapping of letters to digits
-- 5) an array of three boolean values showing if each of the word corresponds to a digit, this we need to 
--    differentiate this case from the main one which does not allow a zero to be a leading digit

-- Returns (Bool, Mapping) - a pair, first field tells whether the state has solution, second field is the mapping. 
-- of that solution (if exists).

getSolution :: String -> String -> String -> Int -> [Int] -> [(Char, Int)] -> [Bool] -> (Bool, Mapping)

-- This is the terminating case when we have exhauste word1 and word2 and can tell if we have a solution or no.
getSolution [] [] word3 leftToAdd freeDigits letterToDigitMapping isNumberADigit = 

    if (length word3 == 0 && leftToAdd == 0)
    then (True, letterToDigitMapping)
    else if ( length word3 == 1 && leftToAdd == 1 && 
              (
              (containsInMap hword3 letterToDigitMapping && getValueFromMap hword3 letterToDigitMapping == 1)
              || (containsInMap hword3 letterToDigitMapping == False && elem 1 freeDigits == True)))
         then (True, addIfNotExist (hword3, 1) letterToDigitMapping)
         else (False, [])
         
     where
        hword3 = head word3

-- if we have exhausted the free digits
getSolution _ _ _ _ [] _ _ = (False, [])

-- if the third word is empty
getSolution _ _ [] _ _ _ _ = (False, [])

getSolution word1 word2 word3 leftToAdd freeDigits letterToDigitMapping isNumberADigit =
    foldr aggregateTwoStates (False, []) (map tryWithPair allPossiblePairs)
    
    where
	-- Aggregating function for the foldr function.
        aggregateTwoStates :: (Bool, Mapping) -> (Bool, Mapping) -> (Bool, Mapping)
        aggregateTwoStates a b
            | (fst a == True) = a
            | (fst b == True) = b
            | otherwise = (False, [])

        getPossibleValuesOfLetters :: Char -> [Int]
        getPossibleValuesOfLetters letter =
            if (containsInMap letter letterToDigitMapping)
            then [getValueFromMap letter letterToDigitMapping]
            else freeDigits

        possibleValues1 = getPossibleValuesOfLetters (last word1)
        possibleValues2 = getPossibleValuesOfLetters (last word2)
        allPossiblePairs = decartProduct possibleValues1 possibleValues2
	
	-- Tries to add this digits in the end of the two words.
        tryWithPair :: (Int, Int) -> (Bool, Mapping)

        tryWithPair p =
            if (actualResult /= expectedResult
                || (notConflicts [last1,last2,last3] [value1,value2,actualResult] == False)
                || (value1 == 0 && length word1 == 1 && isNumberADigit !! 0 == False)
                || (value2 == 0 && length word2 == 1 && isNumberADigit !! 1 == False)
                || (actualResult == 0 && length word3 == 1 && isNumberADigit !! 2 == False))
            then (False, [])
            else
                getSolution
                (take (length1 - 1) word1)
                (take (length2 - 1) word2)
                (take (length3 - 1) word3)
                newLeftToAdd  
                newFreeDigits
                newLetterToDigitMapping
		isNumberADigit

            where
                value1 = fst p
                value2 = snd p
                last1 = last word1
                last2 = last word2
                last3 = last word3
                length1 = length word1
                length2 = length word2
                length3 = length word3
                sum = value1 + value2 + leftToAdd
		-- the expected result from the addition
                expectedResult = mod sum 10
                newLeftToAdd = div sum 10
                newFreeDigits = removeArray [value1,value2,expectedResult] freeDigits
                newLetterToDigitMapping = addArray [(last1, value1), (last2, value2), (last3, expectedResult)] letterToDigitMapping
		-- the actual result is the value of the digit if it is already set, otherwise the expected
                actualResult =
                    if (containsInMap last3 letterToDigitMapping)
                    then getValueFromMap last3 letterToDigitMapping
                    else if (elem expectedResult freeDigits) then expectedResult
                    else -1

-- Prints the solution in an appropriate way.
printSolution :: String -> String -> String -> (Bool, Mapping) -> IO ()
printSolution operand1 operand2 result solution =
    if (fst solution == True)
        then putStr ((++"\n") (getOutput operand1 operand2 result (snd solution)))
        else putStr "No solution\n"

str11 = "SEND"
str12 = "MORE"
str13 = "MONEY"
str21 = "DONALD"
str22 = "GERALD"
str23 = "ROBERT"
isNumberDigit1 = [(length str11 == 1), (length str12 == 1), (length str13 == 1)]
isNumberDigit2 = [(length str21 == 1), (length str22 == 1), (length str23 == 1)]
availableDig = [0..9]

solution1 = getSolution str11 str12 str13 0 availableDig [] isNumberDigit1
solution2 = getSolution str21 str22 str23 0 availableDig [] isNumberDigit2

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    operand1 <- getLine
    operand2 <- getLine
    lineDividor <- getLine
    result <- getLine
    printSolution 
	operand1 (tail operand2) result 
	(getSolution operand1 (tail operand2) result 0 availableDig [] [(length operand1 == 1), (length operand2 == 2), (length result == 1)])
    --printSolution str11 str12 str13 solution1
    --printSolution str21 str22 str23 solution2