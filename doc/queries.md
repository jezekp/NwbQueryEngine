Supported Queries 
==

Operators:
-
 - and &
 - or  |
 - equal ==
 - assignment =
 - brackets ()
 - substrings in strings LIKE
 
 
Syntax:
-

- group_name=(expression)
- expression=(expression | expression & expression)
- expression=(expression<expression) | (expression>expression) | (expression>=expression) | (expression<=expression | (expression LIKE expression)

Examples:
-

 Query                                 |      Description
-------------------------------------|---------------------------------------------------------------------------------------
 analysis=(description LIKE whisker) | selects all datasets description from an analysis group which contains a whisker string 
 processing=(electrode_idx>30)       | selects all electrode_idx datasets from a processing group which value > 30  
 epochs=(start_time>200 & stop_time<400 | stop_time>1600) | selects all epochs which start_time > 200 or stop_time < 400
 epochs=(start_time)                    | selects all epochs with a start_time dataset                                                             
 



 
