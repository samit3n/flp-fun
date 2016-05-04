
{-|
      FLP-FUN Projekt 2016
      Formula-2-BDD

      Module implements logic expression processing
      and BDD and RBDD generation

      author: Vojtech Dvoracek (xdvora0y)

      email: xdvora0y@stud.fit.vutbr.cz

      date: 5.4.2016

-}

module Dnf2Bdd where


import Data.Maybe
import qualified Data.Map.Strict as M

-- ####################
--      Data types definition
-- ####################

-- | "show Char" equivalent, omitting  trailing quotes 

showx :: Char -> String
showx c = filter (\x -> x /= '\'') $ show c

-- | Type 'Expr' for propositional logic in DNF or CNF form
-- (it can hold a bit more than DNF)

data Expr = Empty
    | Tr Char 
    | Neg Char
    | AND [Expr]
    | OR [Expr]
    deriving ( Eq)


instance Show Expr where 

   show ( Neg c ) = '-':showx c
   show ( Tr c ) = showx c
   show (AND ls) = map (\c -> if c == ' ' then ',' else c ) (unwords $ map show ls)
   show (OR ls) = (map (\c -> if c == ' ' then '\n' else c) (unwords $ map show ls) ) ++ "\n"


-- | Remove duplicates from given list

rmdups :: (Eq a) => [a] -> [a]
rmdups = foldl (\seen x -> if elem x seen 
                             then seen 
                             else seen ++ [x]) []

-- | Extract logical variables from expression

getVars:: Expr -> [Char]
getVars e = rmdups $ getVars' e

getVars':: Expr -> [Char]
getVars' Empty = []
getVars' (Tr c) =  [c]
getVars' (Neg c) =  [c]
getVars' (AND []) = []
getVars' (AND e) = concat $ map getVars' e
getVars' (OR []) = []
getVars' (OR e) = concat $  map getVars' e


-- | Type 'VTable' for  boolean table of possible assignments 
-- and optionally evaluation results (in last column)

data VTable = VT [[Bool]] | VTEmpty deriving (Eq)

-- | Shows 'VTable' in specified format

showTable:: Vars -> VTable -> String
showTable vars vtab = (shvars vars ) ++ "\n" ++  show vtab
    where   shvars [v] = showx v
            shvars (v:vs) = showx v ++ " " ++ shvars vs

-- | Concatenates VTable contents

catVT :: VTable -> VTable -> VTable
catVT (VT x) (VT y) = VT $ x ++ y
catVT VTEmpty y = y
catVT x VTEmpty = x

-- | Selects only those lines of VTable having given Bool in first column
-- and returns tails of those lines

select::VTable -> Bool -> VTable
select (VT []) _  = VTEmpty
select (VT (x:xs)) b
    | head x == b = VT [tail x] `catVT` (select (VT xs) b)
    | otherwise  = select (VT xs) b


instance Show (VTable) where
    show VTEmpty = "VTable empty"
    show (VT []) = "VTable empty"
    show  (VT x) =  dropLast ( unlines $ map showTabLine x) ++ "\n"
        where showTabLine ln = unwords $ map (show . bool2Int) ln


-- | Logical variables data type

type Vars = [Char]

-- | Data representing value-> assignment pairs
-- specific assignment is taken from VTable

type VMap = M.Map Char Bool

-- ####################
--  Function definitions
-- ####################


-- | Cut last char from string

dropLast::String -> String
dropLast [] = []
dropLast [x] = [] 
dropLast (x:xs) = x:dropLast xs

int2Bool :: Integer -> Bool
int2Bool 0 = False
int2Bool _ = True

bool2Int:: Bool -> Int
bool2Int True = 1
bool2Int False = 0

-- | Generates binary form of Integer
-- into list of Boolean
-- the MSB is leftmost in this binary representation
-- 

dec2Bin::Integer -> Int -> [Bool]
dec2Bin x len  
    | x < 0 = []
    | x == 0 = replicate len False
    | x == 2^len = replicate len True
    | otherwise = map int2Bool (  reverse $ take len  [mod  (x `div` y) 2  | y <- 1:scanl (*) 2 [2,2..]] )


-- | Performs logical table generation for given
--  number of binary digits
--  e.g. genVal 4 generates 2^4 possible assignments 

genVTab:: Int -> VTable
genVTab len = VT $ map  ( `dec2Bin` len)   [ vals | vals <- [0..(2^len-1)]] 


-- | Looks up for given key Char in VMap
-- if not found, critical error is returned
-- since this should never happen

getVal:: Char -> VMap -> Bool
getVal c m = if res == Nothing
             then error "Critical error: key not found"
             else  (\ (Just x) -> x ) res

             where res = M.lookup c m


-- | Evaluates single expression against Boolean assignment

eval:: VMap -> Expr -> Bool
eval mlist (AND ex) = foldl (&&) True $ map (eval mlist) ex
eval mlist (OR ex) = foldl (||) False $ map (eval mlist)  ex 
eval mlist (Tr c) = getVal c mlist
eval mlist (Neg c) = not $ getVal c mlist


-- | Evaluates given expression against all assignemnts from  VTable
-- produces given VTable extended by result on each line

evalAll:: Vars -> VTable -> Expr -> VTable
evalAll [] _ _ = VT [] 
evalAll _ _ Empty = VT []
evalAll v (VT []) _ = VT []
evalAll v (VT x) e = VT $ map (\l -> l ++ [eval (M.fromList (zip v l) )  e] ) x


-- ############################
--
--  BDD  construction functions
--
-- ############################


-- | Type 'BDD' for hodling Binary Decision Diagrams

data BDD = Node Char BDD BDD | Leaf Bool | BEmpty deriving (Eq)

instance Show BDD where
    show b = unlines $ showBDD b


showBDD :: BDD -> [String]
showBDD (Leaf b) = [show $ bool2Int b]
showBDD (Node c f t) =  (map (\s -> (showx c) ++ "->" ++ s) $ showBDD f ) ++ 
                        (map (\s -> (showx c) ++ "=>" ++ s) $ showBDD t)

-- | Function generating BDD from truth table
-- Vars speicifies order of variables in truth table
-- and in BDD as well

genBDD:: Vars -> VTable -> BDD
genBDD [c] (VT (x:y:xs)) = Node c (Leaf $ head (tail x) ) (Leaf ( head (tail y) ))
genBDD (v:vs) vt = Node v (genBDD vs vtF) (genBDD vs vtT)
    where vtF = select vt False
          vtT = select vt True

-- | Function reducing general BDD

reduceBDD :: BDD -> BDD
reduceBDD BEmpty = BEmpty
reduceBDD nd@(Node c 
                (Leaf lb) 
                (Leaf rb) ) = if lb == rb
                              then (Leaf lb)
                              else nd
reduceBDD (Node c f t) = if redF == redT
                         then redF 
                         else Node c redF redT

    where redF  = reduceBDD f
          redT = reduceBDD t
