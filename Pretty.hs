  
--------------------------------------------------------------------
-- HASKELL IMP pretty PRINTER                
-- Roy Crole and Paula Severi  2024                                            
--------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Pretty where


import AST


class Pretty a where 
     pp ::  a -> String     -- pretty printer 
     precedence :: a -> Int     -- precedence for the constructors 
     bracket :: Int ->  a -> String 
     
--  test precedence and on lower precedence wrap in brackets 
     bracket n e 
       | (precedence e) >= n = pp e
       | otherwise           = "("++(pp e)++")"


  
       
----------------------------
-- Code a pretty Printer  --
----------------------------


-- a pretty printer for commands 

instance Pretty Com where 
   pp (Ass (v,e)) =  v ++ ":=" ++ (  bracket 4 e)
   pp (Seq (c1,c2)) = (  bracket 1 c1)++ " ; " ++ (  bracket 1 c2)
   pp (If(e,c1,c2)) =  "if " ++ (  bracket 3 e) ++ " then " ++ (  bracket 3 c1) ++ " else " ++ (  bracket 3 c2)
   pp (While(e,c)) = "while " ++ (  bracket 2 e) ++ " do " ++ (bracket 2 c)
   
   precedence (Ass (v,ie)) = 4
   precedence (If (be,co,co')) = 3
   precedence (While (co,co')) = 2
   precedence (Seq (co,co')) = 1
 

-- a pretty printer for integer  expressions 

instance Pretty IntExp where 
      pp (Int z)  = show z
      pp (Var v)  = v
      pp (IopExp(Plus,e1,e2)) =  case e2 of
                                  IopExp(Plus,x,y) -> 
                                   (  bracket 1 e1) ++ "+" ++ ("(" ++ (  bracket 1 x) ++ "+" ++ (  bracket 1 y) ++ ")")
                                  IopExp(Minus,x,y) -> 
                                   (  bracket 1 e1) ++ "-" ++  ("(" ++ (  bracket 1 x) ++ "-" ++ (  bracket 1 y) ++ ")")
                                  _ -> (  bracket 1 e1) ++ "+" ++ (  bracket 1 e2)

      pp (IopExp(Minus,e1,e2)) =  case e2 of
                                   IopExp(Minus,x,y) -> 
                                    (  bracket 1 e1) ++ "-" ++ ("(" ++ (  bracket 1 x) ++ "-" ++ (  bracket 1 y) ++ ")")
                                   IopExp(Plus,x,y) -> 
                                    (  bracket 1 e1) ++ "+" ++ ("(" ++ (  bracket 1 x) ++ "+" ++ (  bracket 1 y) ++ ")")
                                   _ -> (  bracket 1 e1) ++ "-" ++ (  bracket 1 e2)
                                 
      pp (IopExp(Times,e1,e2)) = (  bracket 3 e1) ++ "*" ++ (  bracket 3 e2)
     
      
      precedence (Int z) = 4
      precedence (Var v) = 4
      precedence (IopExp (Times,ie,ie')) = 3
      precedence (IopExp (Plus,ie,ie')) = 1
      precedence (IopExp (Minus,ie,ie')) = 1

-- a pretty printer for Boolean expressions

instance Pretty BoolExp where 
      pp (Bool b) = if b then "true" else "false"
      pp (BopExp(Le,e1,e2)) = (  bracket 1 e1) ++ "<" ++ (  bracket 1 e2)
      pp (BopExp(Gr,e1,e2)) = (  bracket 1 e1) ++ ">" ++ (  bracket 1 e2)
      pp (BopExp(LeEq,e1,e2)) = (  bracket 1 e1) ++ "<=" ++ (  bracket 1 e2)
      pp (BopExp(GrEq,e1,e2)) = (  bracket 1 e1) ++ ">=" ++ (  bracket 1 e2)
      
      precedence (Bool b) = 4
      precedence (BopExp (Le,ie,ie')) = 1
      precedence (BopExp (Gr,ie,ie')) = 1
      precedence (BopExp (LeEq,ie,ie')) = 1
      precedence (BopExp (GrEq,ie,ie')) = 1

