
--------------------------------------------------------------------
-- HASKELL IMP LANGUAGE PARSING LIBRARY                   
-- Roy Crole and Paula Severi 2024                                           
--------------------------------------------------------------------

-- *****************************************************************
-- EXAMPLES OF THE FUNCTIONS IN THIS FILE CAN BE FOUND IN BLACKBOARD
-- *****************************************************************
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Eta reduce" #-}

module ImpPar where

import Basic
import AST
-- import Pretty 
import Tokens
import Parse

----------------------------------
-- Bookkeeping Functions        --
----------------------------------

str_to_z :: String -> Z
str_to_z = read

str_to_v :: String -> V
str_to_v s = s

str_to_b :: String -> B
str_to_b s = if s == "true" then True else False


----------------------------------
-- Top Level Parsing Functions  --
----------------------------------

-- These functions are instances of reader. 
-- They are not used explicitly in IMP but you may find them useful for testing 

{-- 

readins :: IMPFile -> Instruction
readins = reader ins 

readstate :: IMPFile -> State 
readstate = reader state

readprog :: IMPFile -> Prog
readprog = reader prog 

readcom :: IMPFile -> Com
readcom  = reader com
--}
readiexp :: IMPFile -> IntExp 
readiexp = reader iexp

readbexp :: IMPFile -> BoolExp 
readbexp = reader bexp


---------------------
-- Build Functions --
---------------------

-- parser build functions for the State
makeEmptystate s = []
makeState  ("[",("(",(v,(",",(z,(")",(s,"]"))))))) =
                (v, str_to_z z) : (map aux s)
                where
                aux (",",("(",(v,(",",(z,")"))))) = (v,str_to_z z)

-- parser build functions for programs
makeProg ("(",(code,(",",(s,")")))) = case code of
                                           C c -> (C c,s)
                                           E e -> (E e,s)

-- COMPLETE   makeCCode and makeECode 


makeCCode ("C",c) = C c
makeECode ("E",e) = E e


-- parser build functions for commands and expressions
makeExpFromAtom  (_,(e,_)) = e
makeNegNum :: (String, [Char]) -> [Char]
makeNegNum ("-",n) = "-"++n
makeVar = Var . str_to_v
makeInt = Int . str_to_z

makeNegInt ("-",ie) = (Int (-(str_to_z ie)))

makePMT :: Foldable t => (IntExp, t (String, IntExp)) -> IntExp
makePMT (a, la) = let mPM e1 (op,e2) =
                       case op of
                          "+" -> IopExp (Plus,e1,e2)
                          "-" -> IopExp (Minus,e1,e2)
                          "*" -> IopExp (Times,e1,e2)
                      in foldl mPM a la


makeBool (ie1,(op,ie2)) =
  case op of
    "<"   -> BopExp (Le, ie1, ie2)
    ">"   -> BopExp (Gr, ie1, ie2)
    "<="  -> BopExp (LeEq, ie1, ie2)
    ">=" -> BopExp (GrEq, ie1, ie2)

makeComFromAtom  ("(",(c,")")) = c
makeAss (v,(":=",e)) = Ass (v,e)

-- COMPLETE makeSeq using foldl 

makeSeq (c,lc) = let mSeq c1 (";",c2) = Seq (c1,c2)
  in foldl mSeq c lc

makeIfte ("if",(be,("then",(c1,("else",c2))))) = If (be,c1,c2)

makeWhile ("while", (be, ("do", c))) = While (be, c)




----------------------------
-- The Combinatory Parser --
----------------------------


{--
-- parser for an IMP instruction
ins :: Parse Instruction
ins = key "run" `next` idr  `build` (\("run",p)-> Run p)
          `alt`
           key "eval" `next` prog `build` (\("eval",p)-> Eval p)
          `alt`
           key "help" `next` num `build` (\("help", k)-> Help (str_to_z k))
           `alt`
           key "quit" `build` (\i -> Quit)
           `alt`
           key "help" `build` (\i -> Helpp)

-- parser for an IMP state
state :: Parse State
state = key "[]" `build` makeEmptystate
        `alt`
        key "[" `next`
        key "(" `next` idr `next` key ","
        `next`
        integer
        `next` key ")"
        `next`
        many (key "," `next`
              key "(" `next` idr `next` key "," `next` num `next` key ")")
        `next` key "]"
        `build` makeState

-- parser for IMP programs
prog :: Parse Prog
prog toks = (key "(" `next` code `next` key "," `next` state `next` key ")" `build` makeProg) toks



-- COMPLETE the parser for IMP code
code :: Parse Code
code toks =  (com `next` many (key ";" `next` com) `build` makeSeq) toks


-- COMPLETE the parser for IMP commands
com :: Parse Com
com toks =
  (ifORwh `next` many (key ";" `next` ifORwh) `build` makeSeq
  ) toks


ifORwh :: Parse Com
ifORwh toks =
  (???  `build` makeIfte
  `alt`
  ??? `build` makeWhile
  `alt`
  catom 
  ) toks
  
catom  :: Parse Com
catom toks = 
  (??? `build` makeAss 
  `alt` 
   ???  `build` makeComFromAtom
  ) toks
  

--}     
-- COMPLETE the parser for IMP Boolean expressions
bexp :: Parse BoolExp
bexp toks = 
  ( iexp `next` (key "<=" `alt` key ">=" `alt` key "<" `alt` key ">") `next` iexp 
    `build` makeBool
    `alt` batom
  ) toks

batom :: Parse BoolExp
batom toks = 
  (key"true"  `build` (Bool . str_to_b)
   `alt`
   key"false" `build` (Bool . str_to_b)
   `alt`
   key"(" `next` bexp `next` key")"  `build` makeExpFromAtom
  )  toks
  

-- parser for IMP integer expressions

iexp :: Parse IntExp
iexp toks = 
  (((factor `next` many (key "+" `next` factor)) 
    `alt` 
    (factor `next` many (key "-" `next` factor))) 
    `build` makePMT
  ) toks

factor :: Parse IntExp
factor toks =
  ((iatom `next` many (key "*" `next` iatom)) `build` makePMT
  ) toks


iatom :: Parse IntExp
iatom toks =
  (idr `build` makeVar
  `alt`
   integer  `build` makeInt
  `alt`
   key "(" `next` iexp `next` key ")"  `build` makeExpFromAtom
  ) toks


-- THIS WORKS
-- parser for IMP integers
-- NOTE the type
integer :: Parse IMPFile
integer toks = ( num
                `alt`
                 key "-" `next` num  `build` makeNegNum ) toks

