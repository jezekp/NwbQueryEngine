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

- gd = group|dataset
- da = dataset|attribute
- gd = (expression)
- expression = expression | expression & expression
- expression = da<const | da<const | da<const | da<const | da<const | gda

Examples:
-

 Query                                 |      Description
-------------------------------------|---------------------------------------------------------------------------------------
 analysis=(description LIKE whisker) | selects all datasets description from an analysis group which contains a whisker string 
 processing=(electrode_idx>30)       | selects all electrode_idx datasets from a processing group which value > 30  
 epochs=(start_time>200 & stop_time<400 &#124; stop_time>1600) | selects all epochs which start_time > 200 or stop_time < 400 or stop_time > 1600
 epochs=(start_time)                    | selects all epochs with a start_time dataset        
 data=(unit LIKE unkno) | selects all datasets data with attributes containing a substring unkno
 pole_in/data=(unit LIKE unkno) | takes into account only data within a pole_in group
 
 



 
