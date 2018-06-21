--  File        : Proj1.hs

--  Author      : Yuqian Shi

--  Origin      : Wed Aug 23 -8:46:04 2017

--  Purpose     : program for proj1 project submissions


--  Overall impelement:

--  The main idea is 1)using reduceCandidates function to

--                     shrink the possible answer space, then

--                   2)using pick function to try more possible candidates.

--  Using [0..20] to represent [A1..G3]

--  On avarage, it needs 5 attempts to get the

--  right answer from all 1330 possible answers.

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List

import Data.Char

import Data.Map (fromListWith, toList)


-- GameState is a list of possible candidates,

-- each candidates is a list consists of:

-- 1) a list represents the chord,

-- 2) a list represents the octaves in that chord, and

-- 3) a list represents the notes in that chord.

type GameState   = [[[Int]]]

numNotes   = 7

numOctave  = 3



-- | Returns the conmbinations of the given list

-- (pick out n elements from the list)

combs :: Int -> [a] -> [[a]]

combs 0 xs = [ [] ]

combs n [] = [ ]

combs n (x:xs) = map (x:) (combs (n-1) xs) ++ combs n xs




-- | Returns the initial 1330 Candidates

initCandidates :: [[Int]] -> [[[Int]]]

initCandidates [] = []

initCandidates (x:xs) =

 [x , getNotes x , getOctaves x ] : initCandidates xs






--------------------------start of transfering functions----------------

-- The following functions are responsible for

-- transferring pitches to octaves & notes or transfering them back to pitches.

-- In this case I use [0..20] to represent [A1..G3]



-- | Returns the int of octave

otc2Int :: String -> Int

otc2Int str = ((read [str!!1] :: Int)-1) * numNotes



-- | Returns the int of note

note2Int :: String -> Int

note2Int n = (ord (n!!0)) - (ord 'A')



-- | Returns the string of note

int2Note :: Int -> String

int2Note n = [chr ((n `mod` numNotes) + (ord 'A'))]



-- | Returns the string of octave

int2Oct :: Int -> String

int2Oct n = [chr ((n `quot` numNotes) + (ord '1'))]



-- | Returns the int of pitch

pitchStr2Int :: [String] -> [Int]

pitchStr2Int [] = []

pitchStr2Int (x:xs) = (note2Int x + otc2Int x): pitchStr2Int xs



-- | Returns the string of pitch

pitchInt2Str :: [Int] -> [String]

pitchInt2Str [] = []

pitchInt2Str (x:xs) = (int2Note x ++ int2Oct x): pitchInt2Str xs



-- | Return a list contains the octaves in that candidate

getOctaves :: [Int] -> [Int]

getOctaves n = map (`quot` numNotes) n



-- | Return a list contains the notes in that candidate

getNotes :: [Int] -> [Int]

getNotes n = map (`mod` numNotes) n

--------------------------end of transfering functions---------------------











--------------------------start of reducing functions----------------------

-- The following functions are responsible for
-- reducing the length of GameState,
-- which is the collection of all candidates.





-- | converts a list to tuple

tuplify3 :: [a] -> (a,a,a)

tuplify3 [x,y,z] = (x,y,z)



-- | if the candidate in Gamestate do not have the same feedback for,
--   the given attempt, then delete it

reduceCandidates :: [String] -> GameState -> (Int,Int,Int) -> GameState

reduceCandidates attempt [] feedback  = []

reduceCandidates attempt state (x,y,z)

 | (selfTest  (head (initCandidates [pitchStr2Int attempt])) (head state) == [x,y,z])

   = head state : reduceCandidates attempt (tail state) (x,y,z)

 | otherwise =

   reduceCandidates attempt (tail state) (x,y,z)



-- | Return the minimal of intersect operation, used for checkY and checkZ

minInsersect :: [Int] -> [Int] -> [Int]

minInsersect [] []  = []

minInsersect a  []  = []

minInsersect [] b   = []

minInsersect a  b   =

 min (a `intersect` b ) (b `intersect` a)
 --------------------------end of reducing functions---------------









 --------------------------start of picking functions---------------

-- | Caculating the possibility for every candidate according to Hint 5,
--  then pick the most possible one. the initial current socre is set to 99999
--  and the current candidate to ["A1","A1","A1"].
--  They will be replaced after first iteration.

--  | Returns a candidate from the current GameState
pick :: GameState -> [String]

pick state =

 pitchInt2Str (head (thinkPossible state state (

  [[fromIntegral 0,fromIntegral 0,fromIntegral 0]],99999)))





--  | select the most possible one (incomplete)

-- here states refers to the whole gamestate,

-- whilst state refers to the remaining collection of candidates

-- need to caculate score.

thinkPossible :: GameState -> GameState -> ([[Int]],Float) -> [[Int]]

thinkPossible state states (cur_candi,cur_score)



 | (cur_score > getResult (head state) states) && (length state > 1)

   = thinkPossible (tail state) states

    (head state,getResult (head state) states)

 | (cur_score > getResult (head state) states) && (length state == 1)

   = head state

 | (cur_score <= getResult (head state) states) && (length state > 1)

   = thinkPossible (tail state) states (cur_candi,cur_score)

 | (cur_score <= getResult (head state) states) && (length state == 1)

   = cur_candi





--  | Count the frequecy  of all distinct elements in a list

frequency :: (Ord a) => [a] -> [(a, Int)]

frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])



-- | returns the score of the given candidate in the current GameState.

getResult :: [[Int]] -> GameState -> Float

getResult candidate state = result

 where

  temp      = frequency (compCandi candidate state)

  result    = getScore temp (fromIntegral (length temp))



-- | Get all possible (remaining) feedback for the given candidate

compCandi :: [[Int]] -> GameState -> [[Int]]

compCandi candidate []      = []

compCandi candidate state   =

 (selfTest candidate (head state)) : compCandi candidate (tail state)





-- | Ge the count out of the tuple

getCount :: (a, Int) -> Float

getCount (_,a) = fromIntegral a



-- | Caculate the possible results (score)

-- first argument refers to the count for all possible results,

-- second refers  to the length of the above list

getScore :: [(a, Int)] -> Float -> Float

getScore [] len = 0.0

getScore results len

 | fromIntegral (length results) > len  =

    getScore results (fromIntegral (length results))

 | otherwise                            =

    (((getCount (head results))**2)/len) +

    getScore (tail results) len



-- Find the indentical elements

eqNth :: Eq a => Int -> [a] -> [a] -> Bool

eqNth index list1 list2 = (list1!!index == list2!!index)



-- | SelfTest, returns the feedback for given attempt and answer.

selfTest :: [[Int]] -> [[Int]] -> [Int]

selfTest attempt answer = [x, y, z]

 where

  str_attempt   = pitchInt2Str (head attempt)

  str_answer    = pitchInt2Str (head answer)

  x             = length (minInsersect (attempt!!0) (answer!!0))

  y             = 3 - x - (length (deleteFirstsBy (eqNth 0) str_attempt str_answer))

  z             = 3 - x - (length (deleteFirstsBy (eqNth 1) str_attempt str_answer))



 --------------------------end of picking functions---------------







 -- | Initialize GameState

candidates = initCandidates (combs 3 [0..20])



 -- | Make the first Guess

 --  Statiscal results suggest this would be the best choice

initialGuess :: ([String],GameState)

initialGuess = (["A1","B1","C2"], candidates)



 -- | Make the next Guess

nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)

nextGuess (attempt,state) feedback =

 (newAttempt,newState)

  where

   newState = (reduceCandidates attempt state feedback)

   newAttempt = (pick newState)
