  
--------------------------------------------------------------------
-- HASKELL ABSTRACT SYNTAX TREES FOR IMP                  
-- Roy Crole and Paula Severi 2024                                             
--------------------------------------------------------------------

module AST where 

type V = String
type B = Bool
type Z = Int
type FileName = String

data Bop =   Le | Gr | LeEq | GrEq
             deriving (Eq,Show)

data Iop = Plus | Minus | Times 
             deriving (Eq,Show)
             
data Com = Ass(V, IntExp)
        | Seq(Com, Com)
        | If (BoolExp, Com, Com)
        | While (BoolExp, Com)
        deriving (Eq,Show)
             
data BoolExp = Bool B
             | BopExp (Bop, IntExp, IntExp)
             deriving (Eq, Show)
             
data IntExp = Int Z
            | Var V
            | IopExp (Iop, IntExp, IntExp)
            deriving (Eq, Show)




type State = [(V, Z)]


data Instruction = Run FileName | Eval Prog | Helpp | Help Int | Quit 
                   deriving (Eq,Show)

type Prog  = (Code,State)  


data Code  = E IntExp | C Com
             deriving (Eq,Show)




