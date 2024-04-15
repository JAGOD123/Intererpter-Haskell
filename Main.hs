 
---------------------------------------------------------------------
-- MAIN FILE: HASKELL USERS' FUNCTIONS FOR IMP                                 
-- Roy L. Crole and Paula Severi 2024                                              
---------------------------------------------------------------------

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
             (C c,s) ->  arrow ++ ???
             (E e,s) ->  arrow ++ ???
             

-- COMPLETE prompt

prompt :: IO()
prompt = do putStr "\n >IMP> \n"
            inputstr <- getLine
            if inputstr=="" then prompt else
             case  (???  inputstr) of
              Success (Run  s, [])    -> do 
                                          ???
                                         
              Success (??? , [])    -> do 
                                            putStr  (ppev p) 
                                            ???
              Success (Helpp, [])     -> do 
                                           help 0
                                           prompt
              Success (Help k, [])    ->  do 
                                           ???
                                           prompt
              Success (Quit, [])      -> ???
              -- other deals with the case of Failing parsers
              other   -> putStrLn "Error in your input.\n" >> prompt

------------------------
-- Only main here
-------------------------

main :: IO()
main = do introduction
          prompt 
          
