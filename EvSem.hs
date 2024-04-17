
--------------------------------------------------------------------- 
-- SML EVALUATION SEMANTICS FOR IMP                                   
-- Roy Crole and Paula Severi 2024                                            
--------------------------------------------------------------------- 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module EvSem

where

import AST
import Basic


----------------------------------------------------------------
--  Code the function evalint for evaluating Integer Expressions
----------------------------------------------------------------


evalint :: IntExp -> State -> Int 
evalint (Int m) s = m --base case
evalint (Var v) s =  (lookUp s v) 
evalint (IopExp (op, e1, e2)) s =
    let p1 = evalint e1 s
        p2 = evalint e2 s
    in case op of
        Plus  -> (p1 + p2)
        Minus -> (p1 - p2)
        Times -> (p1 * p2)
 
 ---------------------------------------------------------------------
-- Code the function evalbool for evaluating Boolean Expressions
-----------------------------------------------------------------------


evalbool :: BoolExp -> State -> Bool
evalbool  (Bool b) s = b -- base case
evalbool  (BopExp (op, e1, e2)) s =
    let p1 = evalint e1 s
        p2 = evalint e2 s
    in case op of
        Le   -> p1  < p2
        Gr   -> p1  > p2
        LeEq -> p1 <= p2
        GrEq -> p1 >= p2


                                                                                    
--------------------------------------------------------------------------
-- Code the function evalcom for evaluating Commands
---------------------------------------------------------------------------

evalcom :: Com -> State -> State 
evalcom (Ass (var, item)) state       = update state var (evalint item state)
evalcom (Seq (com1, com2)) state      = evalcom com2 (evalcom com1 state)
evalcom (If (bool, com1, com2)) state = if evalbool bool state 
                                        then evalcom com1 state 
                                        else evalcom com2 state 
evalcom (While (bool, com)) state     = if evalbool bool state 
                                        then evalcom (While (bool, com)) (evalcom com state)
                                        else state
