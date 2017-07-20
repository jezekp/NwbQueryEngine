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

* analysis=(description LIKE whisker)
* processing=(electrode_idx>30)
* epochs=(start_time>200 & stop_time<400 | stop_time>1600)

 