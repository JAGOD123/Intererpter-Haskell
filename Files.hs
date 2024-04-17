 
---------------------------------------------------------------------
-- MAIN FILE: HASKELL USERS' FUNCTIONS FOR IMP                                 
-- Roy L. Crole and Paula Severi 2024                                              
---------------------------------------------------------------------

module Files  where

import System.Directory

import AST
-- import Basic
import Pretty 
import Tokens
import Parse
import ImpPar 
import EvSem
import Help


--------------------------------------------------------------------- 
-- Processing each line of the file 
---------------------------------------------------------------------

arrow = " ==> "


 
---  COMPLETE processLine            

processLine :: String -> String
processLine x = if x == "" then x
                else case prog (tokenize x) of 
                       Success ((C c,s), [])  -> x ++  arrow ++ show (evalcom c s) 
                       Success ((E e,s), [])  -> x ++  arrow ++ show (evalint e s)
                       other -> "\n Error in your input file.\n" 
                     

processLines:: [String] -> [String]
processLines =  map processLine
   
---  COMPLETE processAll
      
processAll :: String -> String
processAll input = unlines $ processLines (lines input)



processFile :: FilePath -> IO()
processFile x =  do  existsFile <-  doesFileExist x 
                     if existsFile then 
                        do filestr <- readFile x
                           putStrLn (processAll filestr)  
                     else putStrLn "The file does not exist" 
