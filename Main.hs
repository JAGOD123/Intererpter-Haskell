 
---------------------------------------------------------------------
-- MAIN FILE: HASKELL USERS' FUNCTIONS FOR IMP                                 
-- Roy L. Crole and Paula Severi 2024                                              
---------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Main where


import AST
-- import Basic
import Pretty 
import Tokens
import Parse
import ImpPar 
import EvSem
import Help
import Files 


-- complete ppev 

ppev :: Prog -> String
ppev p = case p of
             (C c,s) ->  arrow ++ show (evalcom c s) 
             (E e,s) ->  arrow ++ show (evalint e s)
             

-- COMPLETE prompt


prompt :: IO()
prompt = do putStr "\n >IMP> \n"
            inputstr <- getLine
            if inputstr=="" then prompt else
             case  ins  (tokenize inputstr) of
              Success (Run  s, []) -> do 
                processFile s
                prompt                      
              Success (Eval p, []) -> do 
                putStr  (ppev p) 
                prompt
              Success (Helpp, [])  -> do 
                help 0
                prompt
              Success (Help k, []) ->  do 
                help k
                prompt
              Success (Quit, [])   -> putStrLn "Exit\n\n"
              -- other deals with the case of Failing parsers
              other   -> putStrLn "Error in your input.\n" >> prompt

------------------------
-- Only main here
-------------------------

main :: IO()
main = do introduction
          prompt 