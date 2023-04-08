-- |
-- Module                  : Lecture2
-- Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
-- SPDX-License-Identifier : MPL-2.0
-- Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
-- Stability               : Stable
-- Portability             : Portable
--
-- Exercises for the Lecture 2 of the Haskell Beginners course.
--
-- As in the previous section, implement functions and provide type
-- signatures. If the type signature is not already written, write the
-- most polymorphic type signature you can.
--
-- Unlike exercises to Lecture 1, this module also contains more
-- challenging exercises. You don't need to solve them to finish the
-- course but you can if you like challenges :)
module Lecture2
  ( -- * Normal
    lazyProduct,
    duplicate,
    removeAt,
    evenLists,
    dropSpaces,
    Knight (..),
    dragonFight,

    -- * Hard
    isIncreasing,
    merge,
    mergeSort,
    Expr (..),
    Variables,
    EvalError (..),
    eval,
    constantFolding,
  )
where

-- VVV If you need to import libraries, do it after this line ... VVV
import Data.Char (isSpace)

-- ^ ^^ and before this line. Otherwise the test suite might fail  ^^^

-- | Implement a function that finds a product of all the numbers in
-- the list. But implement a lazier version of this function: if you see
-- zero, you can stop calculating product and return 0 immediately.
--
-- >>> lazyProduct [4, 3, 7]
-- 84

-- NOTE (Student) Return the product identity on an empty list
lazyProduct :: [Int] -> Int
lazyProduct [] = 1
lazyProduct (0 : _) = 0
lazyProduct (x : xs) = x * lazyProduct xs

-- | Implement a function that duplicates every element in the list.
--
-- >>> duplicate [3, 1, 2]
-- [3,3,1,1,2,2]
-- >>> duplicate "cab"
-- "ccaabb"
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x : xs) = x : x : duplicate xs

-- | Implement function that takes index and a list and removes the
-- element at the given position. Additionally, this function should also
-- return the removed element.
--
-- >>> removeAt 0 [1 .. 5]
-- (Just 1,[2,3,4,5])
--
-- >>> removeAt 10 [1 .. 5]
-- (Nothing,[1,2,3,4,5])

-- NOTE (Student) After many iterations, I found that splitAt is total.
-- It makes it easier to then pattern match on each part of the initial list
-- to test specific cases (index out of bound, ...)
-- We still have a guard with n < 0 because we cannot distinguish this case
-- from splitAt 0.
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n l
  | n < 0 = (Nothing, l)
  | otherwise =
      let (xs, ys) = splitAt n l
       in case (xs, ys) of
            (_, []) -> (Nothing, l) -- Either l is empty or n >= length l
            (_, w : ws) -> (Just w, xs ++ ws) -- n is a valid index

-- | Write a function that takes a list of lists and returns only
-- lists of even lengths.
--
-- >>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
-- [[3,1,2,7],[]]
--
-- ♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
--  in this function.
evenLists :: [[a]] -> [[a]]
evenLists = filter (even . length)

-- | The @dropSpaces@ function takes a string containing a single word
-- or number surrounded by spaces and removes all leading and trailing
-- spaces.
--
-- >>> dropSpaces "   hello  "
-- "hello"
-- >>> dropSpaces "-200            "
-- "-200"
--
-- ♫ NOTE: As in the previous task, use eta-reduction and function
--  composition (the dot (.) operator) in this function.
--
-- 🕯 HINT: look into Data.Char and Prelude modules for functions you may use.

-- NOTE (Student) We cannot use filter only. In case of infinite trailing spaces,
-- it hangs because it must go through the whole list.
-- We are only interested in the substring up till the last non-space char.
dropSpaces :: String -> String
dropSpaces = takeWhile (not . isSpace) . dropWhile isSpace

-- |
--
-- The next task requires to create several data types and functions to
-- model the given situation.
--
-- An evil dragon attacked a village of innocent citizens! After
-- returning to its lair, the dragon became hungry and ate one of its
-- treasure chests by accident.
--
-- The guild in the village found a brave knight to slay the dragon!
-- As a reward, the knight can take the treasure chest.
--
-- Below is the description of the fight and character specifications:
--
--   * A chest contains a non-zero amount of gold and a possible treasure.
--     When defining the type of a treasure chest, you don't know what
--     treasures it stores inside, so your chest data type must be able
--     to contain any possible treasure.
--   * As a reward, the knight takes all the gold, the treasure and experience.
--   * Experience is calculated based on the dragon type. A dragon can be
--     either red, black or green.
--   * Red dragons grant 100 experience points, black dragons — 150, and green — 250.
--   * Stomachs of green dragons contain extreme acid and they melt any
--     treasure except gold. So green dragons have only gold as reward.
--     All other dragons always contain treasure in addition to gold.
--   * Knight tries to slay a dragon with their sword. Each sword strike
--     decreases dragon health by the "sword attack" amount. When the
--     dragon health becomes zero or less, a dragon dies and the knight
--     takes the reward.
--   * After each 10 sword strikes, the dragon breathes fire and decreases
--     knight health by the amount of "dragon fire power". If the
--     knight's health becomes 0 or less, the knight dies.
--   * Additionally, each sword strike decreases "knight's endurance" by one.
--     If a knight's endurance becomes zero, they become tired and are not
--     able to continue the fight so they run away.
--
-- Implement data types to describe treasure, knight and dragon.
-- And implement a function that takes a knight and a dragon and returns
-- one of the three possible fight outcomes.
--
-- You're free to define any helper functions.
--
-- 🕯 HINT: If you find the description overwhelming to implement entirely
--   from scratch, try modelling the problem in stages.
--
--     1. Implement all custom data types without using polymorphism.
--     2. Add @newtype@s for safety where you think is appropriate.
--     3. Encode the fight result as a sum type.
--     4. Add polymorphism.
--     5. Make all invalid states unrepresentable. Think, how you can
--        change your types to prevent green dragons from having any
--        treasure besides gold (if you already haven't done this).

-- some help in the beginning ;)

data Knight = Knight
  { knightHealth :: Health,
    knightAttack :: Attack,
    knightEndurance :: Endurance
  }

data Dragon a
  = RedDragon DragonStats (Chest a)
  | BlackDragon DragonStats (Chest a)
  | GreenDragon DragonStats Gold

data DragonStats = DragonStats
  { dragonStatsHealth :: Health,
    dragonStatsAttack :: Attack
  }

dragonStats :: Dragon a -> DragonStats
dragonStats (RedDragon stats _) = stats
dragonStats (BlackDragon stats _) = stats
dragonStats (GreenDragon stats _) = stats

dragonHealth :: Dragon a -> Health
dragonHealth = dragonStatsHealth . dragonStats

dragonAttack :: Dragon a -> Attack
dragonAttack = dragonStatsAttack . dragonStats

setDragonStats :: DragonStats -> Dragon a -> Dragon a
setDragonStats s d = case d of
  RedDragon _ loot -> RedDragon s loot
  BlackDragon _ loot -> BlackDragon s loot
  GreenDragon _ loot -> GreenDragon s loot

data Chest a = Chest
  { chestGold :: Gold,
    chestTreasure :: Treasure a
  }

data Treasure a = NoTreasure | SomeTreasure a

data Reward a = Reward
  { rewardExp :: Experience,
    rewardGold :: Gold,
    rewardTreasure :: Treasure a
  }

data FightResult a
  = KnightWin (Reward a)
  | KnightLose
  | KnightFlee

-- Newtypes to avoid ambiguities in the calculateDamage phase
newtype Attack = Attack {unAttack :: Int}

-- NOTE (Student) Deriving typeclasses makes things easier for calculations
-- but we will use records members for now instead of
-- deriving (Num, Eq, Ord)
newtype Health = Health {unHealth :: Int}

type Endurance = Int

-- NOTE (Student) Ideally, we should represent the fact that
-- gold is a positive int. I found the following answer from
-- Gabriella Gonzalez https://stackoverflow.com/a/11910221
-- However, it doesn't seem possible to prevent creating a Positive Int
-- with a wrapped negative int if we are in the same module.
type Gold = Int

type Experience = Int

calculateRewardExp :: Dragon a -> Experience
calculateRewardExp d = case d of
  RedDragon {} -> 100
  BlackDragon {} -> 150
  GreenDragon {} -> 250

calculateReward :: Dragon a -> Reward a
calculateReward d = case d of
  GreenDragon _ gold -> Reward {rewardExp = calculateRewardExp d, rewardGold = gold, rewardTreasure = NoTreasure}
  BlackDragon _ chest -> Reward {rewardExp = calculateRewardExp d, rewardGold = chestGold chest, rewardTreasure = chestTreasure chest}
  RedDragon _ chest -> Reward {rewardExp = calculateRewardExp d, rewardGold = chestGold chest, rewardTreasure = chestTreasure chest}

calculateDamage :: Attack -> Health -> Health
calculateDamage (Attack atk) (Health hp) = Health (hp - atk)

-- Resolve the new state of Knight and Dragon after a round
-- of fighting, depending on which round we are in
resolveRound :: Int -> Knight -> Dragon a -> (Knight, Dragon a)
resolveRound rnd k d =
  -- If we are in a round multiple of 10, the dragon attacks the knight
  -- else the knight only loses endurance
  let kAfter = case rnd `mod` 10 of
        0 -> k {knightHealth = calculateDamage (dragonAttack d) (knightHealth k), knightEndurance = knightEndurance k - 1}
        _ -> k {knightEndurance = knightEndurance k - 1}

      dAfter =
        let newStats = (dragonStats d) {dragonStatsHealth = calculateDamage (knightAttack k) (dragonHealth d)}
         in setDragonStats newStats d
   in (kAfter, dAfter)

-- Fight loop and resolution
dragonFight :: Knight -> Dragon a -> FightResult a
dragonFight = go 1
  where
    go :: Int -> Knight -> Dragon a -> FightResult a
    go rnd kn dr
      | unHealth (dragonHealth dr) <= 0 = KnightWin (calculateReward dr)
      | unHealth (knightHealth kn) <= 0 = KnightLose
      | knightEndurance kn <= 0 = KnightFlee
      | otherwise =
          let (knext, dnext) = resolveRound rnd kn dr
           in go (rnd + 1) knext dnext

----------------------------------------------------------------------------
-- Extra Challenges
----------------------------------------------------------------------------

{- The following exercises are considered optional. Some of them might be more
challenging. However, you still may find some of them easier than some of the
previous ones. Difficulty is a relative concept.
-}

-- | Write a function that takes a list of numbers and returns 'True'
-- if all the numbers are in the increasing order (i.e. the list is
-- sorted).
--
-- >>> isIncreasing [3, 1, 2]
-- False
-- >>> isIncreasing [1 .. 10]
-- True
isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing [_] = True
isIncreasing [x, y] = x <= y
isIncreasing (x : y : ys) = x <= y && isIncreasing (y : ys)

-- | Implement a function that takes two lists, sorted in the
-- increasing order, and merges them into new list, also sorted in the
-- increasing order.
--
-- The lists are guaranteed to be given sorted, so you don't need to
-- verify that.
--
-- >>> merge [1, 2, 4] [3, 7]
-- [1,2,3,4,7]
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
-- function takes a list of numbers and returns a new list containing the
-- same numbers but in the increasing order.
--
-- The algorithm of merge sort is the following:
--
--  1. If the given list has less than 2 elements, it's already sorted.
--  2. Otherwise, split list into two lists of the same size.
--  3. Sort each of two lists recursively.
--  4. Merge two resulting sorted lists to get a new sorted list.
--
-- >>> mergeSort [3, 1, 2]
-- [1,2,3]
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l =
  let (firstHalf, secondHalf) = splitAt (length l `div` 2) l
   in merge (mergeSort firstHalf) (mergeSort secondHalf)

-- | Haskell is famous for being a superb language for implementing
-- compilers and interpreters to other programming languages. In the next
-- tasks, you need to implement a tiny part of a compiler.
--
-- We're going to work on a small subset of arithmetic operations.
--
-- In programming we write expressions like "x + 1" or "y + x + 10".
-- Such expressions can be represented in a more structured way (than a
-- string) using the following recursive Algebraic Data Type:
data Expr
  = Lit Int
  | Var String
  | Add Expr Expr
  deriving (Show, Eq)

{- Now, you can use this data type to describe such expressions:

> x + 1
Add (Var "x") (Lit 1)

> y + x + 10
Add (Var "y") (Add (Var "x") (Lit 10))
-}

-- | We want to evaluate such expressions. We can associate a value
-- with a variable using a list of pairs.
--
-- You can use the @lookup@ function to search in this list by a variable name:
--
-- * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
type Variables = [(String, Int)]

-- | Unfortunately, it's not guaranteed that variables in our @Expr@
-- data type are present in the given list. So we're going to introduce a
-- separate data for possible evaluation errors.
--
-- Normally, this would be a sum type with several constructors
-- describing all possible errors. But we have only one error in our
-- evaluation process.
data EvalError
  = VariableNotFound String
  deriving (Show, Eq)

-- | Having all this set up, we can finally implement an evaluation function.
-- It returns either a successful evaluation result or an error.
eval :: Variables -> Expr -> Either EvalError Int
eval _ (Lit i) = Right i
eval vs (Var v) = case lookup v vs of
  Just val -> Right val
  Nothing -> Left (VariableNotFound v)
eval vs (Add e1 e2) = case (eval vs e1, eval vs e2) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right i, Right j) -> Right (i + j)

-- | Compilers also perform optimizations! One of the most common
-- optimizations is "Constant Folding". It performs arithmetic operations
-- on all constants known during compile time. This way you can write
-- more verbose and clear code that works as efficient as its shorter
-- version.
--
-- For example, if you have an expression:
--
-- x + 10 + y + 15 + 20
--
-- The result of constant folding can be:
--
-- x + y + 45
--
-- It also can be:
--
-- x + 45 + y
--
-- Write a function that takes and expression and performs "Constant
-- Folding" optimization on the given expression.
constantFolding :: Expr -> Expr
constantFolding e = case e of
  Lit i -> Lit i
  Var x -> Var x
  Add (Lit i) (Lit j) -> Lit (i + j)
  Add (Lit 0) e2 -> constantFolding e2
  Add e1 (Lit 0) -> constantFolding e1
  -- When there are "recursive" add expressions containing literals,
  -- we group literals together.
  -- Form 1: 10 + (x + 20)
  Add (Lit i) e2 -> case e2 of
    Add expr (Lit j) -> constantFolding (Add expr (Lit (i + j)))
    Add (Lit j) expr -> constantFolding (Add expr (Lit (i + j)))
    _ -> Add (constantFolding e2) (Lit i)
  Add e1 (Lit i) -> case e1 of
    Add expr (Lit j) -> constantFolding (Add expr (Lit (i + j)))
    Add (Lit j) expr -> constantFolding (Add expr (Lit (i + j)))
    _ -> Add (constantFolding e1) (Lit i)
  -- Form 2: (x + 10) + (y + 20)
  Add (Add (Lit i) e1) (Add (Lit j) e2) -> constantFolding (Add (Add e1 e2) (Lit (i + j)))
  Add (Add e1 (Lit i)) (Add e2 (Lit j)) -> constantFolding (Add (Add e1 e2) (Lit (i + j)))
  Add (Add (Lit i) e1) (Add e2 (Lit j)) -> constantFolding (Add (Add e1 e2) (Lit (i + j)))
  Add (Add e1 (Lit i)) (Add (Lit j) e2) -> constantFolding (Add (Add e1 e2) (Lit (i + j)))
  Add e1 e2 -> Add (constantFolding e1) (constantFolding e2)
