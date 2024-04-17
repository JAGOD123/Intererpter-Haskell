
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
{-# HLINT ignore "Redundant bracket" #-}

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


readins :: IMPFile -> Instruction
readins = reader ins 

readstate :: IMPFile -> State 
readstate = reader state

readprog :: IMPFile -> Prog
readprog = reader prog 

readcom :: IMPFile -> Com
readcom  = reader com

readiexp :: IMPFile -> IntExp 
readiexp = reader iexp

readbexp :: IMPFile -> BoolExp 
readbexp = reader bexp


---------------------
-- Build Functions --
---------------------

-- parser build functions for the State
makeEmptystate s = []
makeState :: (String,  (String,   (a,    (String,     (String,      (String,       ([(String, (String, (a, (String, (String, String)))))],        String))))))) -> [(a, Z)]
makeState  ("[",("(",(v,(",",(z,(")",(s,"]"))))))) =
                (v, str_to_z z) : (map aux s)
                where
                aux (",",("(",(v,(",",(z,")"))))) = (v,str_to_z z)

-- parser build functions for programs
makeProg :: (String, (Code, (String, (b, String)))) -> (Code, b)
makeProg ("(",(code,(",",(s,")")))) = 
  case code of
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
makeAss :: (V, (String, IntExp)) -> Com
makeAss (v,(":=",e)) = Ass (v,e)

-- COMPLETE makeSeq using foldl 

makeSeq :: Foldable t => (Com, t (String, Com)) -> Com
makeSeq (c,lc) = 
  let mSeq c1 (";",c2) = Seq (c1,c2)
  in foldl mSeq c lc



makeIfte :: (String, (BoolExp, (String, (Com, (String, Com))))) -> Com
makeIfte ("if",(be,("then",(c1,("else",c2))))) = If (be,c1,c2)

makeWhile ("while", (be, ("do", c))) = While (be, c)




----------------------------
-- The Combinatory Parser --
----------------------------

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



--  the parser for IMP code
code :: Parse Code
code toks = ((key "C" `next` com `build` makeCCode)
            `alt` 
            (key "E" `next` iexp `build` makeECode)) toks





--  the parser for IMP commands
com :: Parse Com
com toks =
  (ifORwh `next` many (key ";" `next` ifORwh) `build` makeSeq
  ) toks


ifORwh :: Parse Com
ifORwh toks =
  (key "if" `next` bexp `next` key "then" `next` catom `next` key "else" `next` catom `build` makeIfte
  `alt`
  key "while" `next` bexp `next` key "do" `next` catom `build` makeWhile
  `alt`
  catom 
  ) toks

catom  :: Parse Com
catom toks = 
  ((idr  `next` key ":=" `next` iexp) `build` makeAss
  `alt` 
   (key "(" `next` com `next` key ")") `build` makeComFromAtom 
  ) toks
  

    
--  the parser for IMP Boolean expressions
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
  (expression `build` makePMT) toks
  where
    expression = factor `next` many (opAndFactor `next` factor)
    opAndFactor = key "+" `alt` key "-"

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


-- parser for IMP integers
-- NOTE the type
integer :: Parse IMPFile
integer toks = ( num
                `alt`
                 key "-" `next` num  `build` makeNegNum ) toks

