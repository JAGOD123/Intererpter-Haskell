 
---------------------------------------------------------------------
-- HASKELL HELP FILE FOR IMP              
--  Roy L. Crole and Paula Severi 2024                                              
---------------------------------------------------------------------

module Help where

introduction = (putStr . unlines) [

 "\n ",
 "                           I         M              M         PPPP                   ",
 "                           I         MM           M M         P    P                 ",
 "                           I         M  M       M   M         P     P                ",
 "                           I         M    M   M     M         P     P                ",
 "                           I         M      M       M         P    P                 ",
 " Welcome to the world of   I         M              M         PPPP                   ",
 "                           I         M              M         P                      ",
 "                           I         M              M         P                      ",
 "                           I         M              M         P                      ",
 "                           I         M              M         P                      ",
 "                           I         M              M         P                      ",

 "\n Copyright (c) Leicester University, 2024 ",

 "\n Please type help  for help" ]


help 0 = (putStr . unlines) [

 "\n please type 'help 1' for a description of IMP ",

 "\n please type 'help 2' for the BNF grammars constituting IMP programs",

 "\n please type 'help 3' to see some example programs",

 "\n That's it folks! You're on yer own now ...." ]


help 1 = (putStr . unlines) [

 "\n This Haskell program parses and interprets a language IMP.",

 " The parser is combinatory. The interpreter follows an operational semantics style",

 "\n An IMP program takes the form:",

 "\n      (<com>, <state>)   or (<intexp>, <state>) ",

 "\n The semantics take the form", 
 
 "\n    (<com>, <state>) ==>  <state>  or    (<intexp>, state) =>  int", 
 

 "\n See help 2 for BNF grammars"
 ]


help 2 = (putStr . unlines) [
  
  "\n The IMP grammar for state <s> is",

  "\n <state> ::= [(<v1>, <int1>) , ... , (<vk>,<intk>)] ", 

  "\n The IMP grammar for program <prog> is",

  "\n <prog> ::= (<code>,<state>)", 

  "\n The IMP grammar for code <code> is",

  "\n <code> ::= C <com> | E <intexp>  ", 

  "\n The IMP grammar for command <c> and expression <e> is",

  "\n <com> ::= <v> := <intexp> | com ; com | if <boolexp> then com else com | while <boolexp> do com ",
  
  "\n <boolexp> ::=   true | false |  <intexp> <= <intexp> | <intexp> >= <intexp> | <intexp> < <intexp> | <intexp> > <intexp>",
  
  "\n  <intexp> := <int> | <v> | <intexp> + <intexp> | <intexp> - <intexp> | <intexp> * <intexp> ",

  "\n where v is an alphabetic string  and int is an integer "]


help 3 = (putStr . unlines) [

   "\n PLEASE TYPE IN EITHER",

   "\n run <filename>  or   eval <prog>   where <filename> is an alphabetic string and <prog> is as in (help 2)",

   "\n For example",
    
    "\n  run fileA",
   
   "\n eval (C x:=4+5+6,[])"]





