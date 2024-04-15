 
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
{--
processLine :: String -> String
processLine x = if x == "" then x
                else case ???  of 
                       Success ((C c,s), [])  -> x ++  arrow ++ ???
                       Success ((E e,s), [])  -> x ++  arrow ++ ???
                       other ->  "\n Error in your input file.\n" 
                     


processLines:: [String] -> [String]
processLines =  ??? 
   
---  COMPLETE processAll
      
processAll :: String -> String
processAll  ???

processFile :: FilePath -> IO()
processFile x =  do  existsFile <-  doesFileExist x 
                     if existsFile then 
                        do filestr <- ???
                           putStrLn (??? )  
                     else putStrLn "The file does not exist" 




--}



