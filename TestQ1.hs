module TestQ1 (pairFirst,encryptChar,encryptString,howManyValues,numInvalid,distinctMap,ownInverse,subset,allMapped,mapLetters) where

-- Code is a type synonym
-- it says that a Code is a list of Pairs of Chars
type Code = [(Char,Char)]

-- domain of our code
domain1 :: [Char]
domain1 = ['a'..'z']
domain2 = ['a','b','a']

-- associated range
range1 :: [Char]
range1 = ['z','y'..'a']
range2 = ['a','c','c']

-- Turns two strings into a code
makeCode :: [Char] -> [Char] -> Code
makeCode domain range = zip domain range

-- create a code out of our domain and range
-- I will call each pair a mapping from the first element of the pair to the second
code1 :: Code
code1 = makeCode domain1 range1
code2 = makeCode domain2 range2

-- pairFirst takes a Code and Char
-- returns the list of all Pairs which have that Char as first element
pairFirst :: Code -> Char -> Code
pairFirst code ch
-- Question 1 for homework
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  =  [n | n <- code, fst n == ch]

-- uses a code to encrypt a Char
-- if the Char has no mapping then the Char encrypts as itself
-- if more than one mapping just use the first value
encryptChar :: Code -> Char -> Char
encryptChar code ch
-- Question 2 for homework
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = let x = pairFirst code ch
	in if length x == 0 then ch else snd (head x)

-- uses a Code to encrypt a String
encryptString :: Code -> String -> String
encryptString code chars
-- Question 3 for homework
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = [encryptChar code z | z <- chars]

-- takes a Code and returns the number of elements a Char is mapped to in that Code
howManyValues :: Code -> Char -> Int
howManyValues code ch
-- Question 4 for homework
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = length [n | n <- code, fst n == ch]

-- takes a Code and returns the number of Chars mapped to more than one element
numInvalid :: Code -> Int
numInvalid code
-- Question 5 for homework
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = length [n | n <-['a'..'z'], howManyValues code n > 1]

-- takes a Code and returns True if no Char maps to itself
distinctMap :: Code -> Bool
distinctMap code
-- Question 6 for homework
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = and [fst (n) /= snd (n) | n <- code]

-- checks that whenever you encrypt a small letter twice you get the original letter back
ownInverse :: Code -> Bool
ownInverse code
-- Question 7 for homework
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = and [encryptChar code (encryptChar code n) == n | n <- ['a'..'z']]

-- checks if everything in first String is contained in second String
subset :: String -> String -> Bool
subset s1 s2
-- Question 8 for homework
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = and [elem n s2 |n <- s1 ]

-- checks if every small letter is the first element of a pair in Code
allMapped :: Code -> Bool
allMapped code
-- Question 9 for homework
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = and [ length (pairFirst code n)  > 0 | n <- ['a'..'z']]

-- checks if everything in Code maps to a small letter
mapLetters :: Code -> Bool
mapLetters code
-- Question 10 for homework
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = let seconds = [snd n | n <- code]
    in and [elem n seconds | n <- ['a'..'z']]
