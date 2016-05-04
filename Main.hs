
{-|
      FLP-FUN Project 2016
      Variant: Formula-2-BDD

      Project implements conversion from propositional calculus formula in DNF form
      to Binary Decision Diagram, or Reduced BDD

      author: Vojtech Dvoracek (xdvora0y)

      email:  xdvora0y@stud.fit.vutbr.cz

      date:  5.4.2016

 
-}

module Main where

import Control.Monad (when)

import Data.Maybe
import System.IO
import System.Environment
import System.Directory (doesFileExist)
import qualified  Data.ByteString.Char8 as Str

import Dnf2Bdd


-- | Program parameters data type

data Param = P_Parse
        | P_Table
        | P_ProcBDD
        | P_ProcRBDD
        | P_NoPar 
        deriving (Show, Eq)

-- | returns contents from FilePath or stdin if "" given

readInput :: FilePath -> IO String
readInput "" = do 
            cont <- Str.hGetContents stdin
            return $ Str.unpack cont


readInput fpath = do
            cont <- Str.readFile fpath
            return $ Str.unpack cont
 

-- | CLI argument -> Param type

procOpts:: String -> Param
procOpts "-i" = P_Parse      -- "tisknu DNF"
procOpts "-t" = P_Table      -- "tisknu tabulku"
procOpts "-b" = P_ProcBDD    -- "tisknu BDD"
procOpts "-r" = P_ProcRBDD   -- "tisknu RBDD"
procOpts  _   = P_NoPar      -- arg error

procArgs :: [String] -> (Param, FilePath)
procArgs (x:y:xs) 
    | not $ null xs = error "Too many arguments given"
    | opt == P_NoPar = error $ "Argument " ++ x ++ " not recognized"
    | otherwise  = (opt, y)

        where opt = procOpts x

procArgs (x:xs) 
    | opt == P_NoPar = error "Argument not recognized"
    | otherwise = (opt, "")
        
        where opt = procOpts x

procArgs _ = error "More arguments expected - 1 or 2"
    

-- | splits CSV string into list of chunks (eg. logical variables)

splitCSV :: String -> [String]
splitCSV s 
    | null $ fst split = []
    | not $  null (snd split) = (fst split): (splitCSV $ tail (snd split))
    | otherwise = [fst split]
        where split = span (\c -> c /= ',') s

-- | parses variable to expression

toExpr :: String -> Maybe Expr
toExpr "" = Nothing
toExpr (n:c:t) 
        |  not $ null t = Nothing
        |  n == '-'  && elem c ['a'..'z'] = Just (Neg c)

toExpr (c:xc) 
    | elem c ['a'..'z'] && null xc = Just (Tr c)
    | otherwise = Nothing


-- | Handles whole input parsing to Expr representation
-- Input fomat: variable [a-z], negation -[a-z]
-- Variables in conjunction on single line
-- Variables in disjunction separated by newline

parseInput:: String -> Expr
parseInput x = if elem Nothing res
               then error "Parsing error"
               else OR $ map (\(Just a) -> a) res


                    where res = map (parseLine . splitCSV) (lines x)

-- | Single input line parsing
--  conjuction of variables

parseLine :: [String] -> Maybe Expr
parseLine [] = Nothing
parseLine ls = if elem Nothing pls 
               then Nothing
               else Just $ AND  (map (\(Just i) -> i) pls)
               
                   where pls = map toExpr ls

-- | Executes desired functionality

process:: Param -> Expr -> String
process P_Parse ex = show ex
process P_Table ex  = showTable vars (evalAll vars tab ex)
            where vars = getVars ex
                  len = length vars
                  tab = genVTab len

process P_ProcBDD ex  = show $ procBDD ex 

process P_ProcRBDD ex = show $ reduceBDD ( procBDD ex)

process P_NoPar _ = "No param given"

-- | Function genrating BDD from parsed
-- expression

procBDD:: Expr -> BDD
procBDD ex = genBDD vars vtab
    where vars = getVars ex
          tab = genVTab (length vars)
          vtab = evalAll vars tab ex


main :: IO()
main = do

    largs <- getArgs
    let x = length largs

    when (x > 2 || x == 0) $ error "Error: Wrong number of arguments, 1  or 2 expected"
    let param = procArgs $ head largs
    let fpath = case x of 
            1 -> "" 
            2 -> head $ drop 1 largs

    inp <- readInput fpath
    if null inp
    then error "Error:empty input"
    else do
        let dnf = parseInput inp 
        let param = procArgs (head largs)

        if param == P_NoPar
        then error "Error: argument not recognized"
        else do 
            putStr $ process param dnf
            return ()
