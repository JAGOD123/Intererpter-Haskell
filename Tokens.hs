
---------------------------------------------------------------------------
-- HASKELL TOKENS FOR EXPRESSIONS AND COMMANDS FOR IMPERATIVE LANGUAGE IMP                          
-- Roy L. Crole and Paula Severi 2024                                            
---------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use isAsciiUpper" #-}
{-# HLINT ignore "Use isAsciiLower" #-}
{-# HLINT ignore "Use isDigit" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant if" #-}


module Tokens where

import Basic

type IMPFile = String
type IMPword = String
type IMPwords = [IMPword]

-- a token is an identifier, keyword or integer

data Token = Id IMPword | Key IMPword | Num IMPword
             deriving (Show, Eq)
             
type Tokens = [Token]


keywords  :: IMPwords
keywords = ["true","false","while","do","if","then","else","run","eval","trans","help","quit","C","E"]

-- Special symbols 

symbols :: IMPwords
symbols = ["(",")","+","-","*","<=",">=","<",">",";",":=","[",",","]","[]"]

-- COMPLETE  the code for is_letter


is_letter :: Char -> Bool
is_letter c = 'A'<=c && c<='Z' || 'a' <= c && c <= 'z'


-- COMPLETE  the code for is_digit


is_digit c = '0'<=c && c <='9' 

is_neg c = '-' == c 

specials = ",!@#$%^&*()_-+=|[]:;'~`<>.?/"

-- delete the comment from is_special if you have already coded mem in Basics.hs

is_special c = c `mem` specials


-- COMPLETE the code for the function alpha

alpha :: (String, IMPFile) -> (String, IMPFile)
alpha (al, c:cs) = if is_letter c 
  then alpha(al++[c],cs) 
  else (al, c:cs)
alpha (al,[]) = (al, [])


-- COMPLETE the code for the function numeric

numeric :: (String, IMPFile) -> (String, IMPFile)
numeric (nu, c:cs) = if is_digit c
  then numeric(nu++[c], cs)
  else (nu, c:cs)
numeric (nu, []) = (nu, [])


-- COMPLETE  the code for symbolic


symbolic :: (String, IMPFile) -> (String, IMPFile)
symbolic (sy, c:cs) = 
  if is_special c && length sy < 2 
  then symbolic (sy ++ [c], cs) -- makes it two chars
  else
    if (sy++[c]) `mem` ["+", "-", "*", "/", "<", ">", "<=", ">="]
    then symbolic (sy ++ [c], cs)
    else (sy, c:cs)
symbolic (sy, []) = (sy, [])


----------------------------------
-- complete the code for scanning
-----------------------------------


scanning :: (Tokens, IMPFile) -> Tokens 
scanning (toks, []) = toks
scanning (toks, c:cs) = 
      if is_letter c then
        let (al, cs2) = alpha([c],cs) in 
        if al `mem` keywords then
          scanning (toks++[Key al],cs2) -- checks the keywords and adds it
        else 
          scanning (toks++[Id al],cs2)
      
      else if is_digit c then
        let (num, cs2) = numeric ([c], cs) in 
        scanning (toks ++ [Num num], cs2)
      else if is_special c then
        let (sy, cs2) = symbolic ([c], cs) in 
        scanning (toks ++ [Key sy], cs2)
      else
        scanning (toks,cs)
            

tokenize :: IMPFile -> Tokens 
tokenize impf = scanning([], impf)

