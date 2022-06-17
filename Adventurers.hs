{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import DurationMonad

-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)
-- Adventurers + the lantern
type Objects = Either Adventurer ()

-- The time that each adventurer needs to cross the bridge
-- To implement
getTimeAdv :: Adventurer -> Int
getTimeAdv P1 = 1
getTimeAdv P2 = 2
getTimeAdv P5 = 5
getTimeAdv P10 = 10

{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state
of the game, with all adventurers and the lantern on the left side of
the bridge. Similarly, the function (const True) represents the end
state of the game, with all adventurers and the lantern on the right
side of the bridge.  --}
type State = Objects -> Bool

instance Show State where
  show s = (show . (fmap show)) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (Right ())]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]



-- The initial state of the game
gInit :: State
gInit = const False

-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)

-- Changes the state of the game of a list of objects
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os

-- Filter the adventures in the same side as the lantern
advWLantern :: State -> [Objects]
advWLantern s = filter (\a -> (s a) == (s (Right ()))) [Left P1, Left P2, Left P5, Left P10]

-- Combine all adventurers on the same side
advComb :: [a] -> [[a]]
advComb [] = []
advComb (x:xs) = [[x]] ++ map (\y -> [x,y]) xs ++ advComb xs

-- Get the Adventure from Objects
getAdventurer :: [Objects] -> [Adventurer]
getAdventurer [] = []
getAdventurer ((Left x):xs) = [x] ++ (getAdventurer xs)
getAdventurer ((Right ()):xs) = getAdventurer xs

-- The time that a group of adventurers needs to cross the bridge
crossingT :: [Objects] -> Int
crossingT = maximum . map getTimeAdv . getAdventurer

{-- For a given state of the game, the function presents all changes
that can be applied to the state together with the time that adventurers
take to cross the bridge in that change. --}
changeAllStates :: State -> ListDur State
changeAllStates = LD . map (\l -> Duration(crossingT l, mChangeState (l ++ [Right()]))) . advComb . advWLantern

{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
allValidPlays :: State -> ListDur State
allValidPlays = changeAllStates <*> return

{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
exec :: Int -> State -> ListDur State
exec 0 s = return s
exec n s = let l = exec (n-1) s in
   l >>= allValidPlays


-------------------------------------------- Extra teste 1 ----------------------------------------------------
check :: Int -> Int -> ListDur State -> Bool
check 0 t l = False
check n t (LD []) = False
check n t l = let l' = remLD $ l >>= allValidPlays in
   if any (\x -> (getDuration x <= t) && (getValue x == (const True))) l'
      then True
      else check (n-1) t $ LD $ filter (\x -> (getDuration x <= t)) l'

--------------------------------------------------------------------------------------------------------


{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement
leq17 :: Bool
leq17 = check 5 17 $ return gInit


--------------------------------------------- Extra teste 2 --------------------------------------------

check2 :: Int -> ListDur State -> Bool
check2 t (LD []) = False
check2 t l = let l' = remLD $ l >>= allValidPlays in
   if any (\x -> (getDuration x < t) && (getValue x == (const True))) l'
      then True
      else check2 t $ LD $ filter (\x -> (getDuration x < t)) l'

--------------------------------------------------------------------------------------------------------

{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- To implement
l17 :: Bool
l17 = check2 17 $ return gInit


--------------------------------------------------------------------------
-- Implementation of the monad used for the problem of the adventurers.

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

instance Functor ListDur where
  fmap f = let f' = fmap f in
    LD . (map f') . remLD

instance Applicative ListDur where
  pure x = LD [pure x]
  l1 <*> l2 = LD $ do x <- remLD l1
                      y <- remLD l2
                      g(x,y) where
                        g(x,y) = return $ x <*> y

instance Monad ListDur where
  return = pure
  l >>= k = LD $ do x <- remLD l
                    g x where
                      g(Duration(d,x)) = let u = (remLD (k x)) in map (wait d) u

manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)
--------------------------------------------------------------------------
