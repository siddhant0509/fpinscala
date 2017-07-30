1. Functional programming - Creating programs with the help of pure functions. 
 Pure Functions(Referentially Transparent, Idempotent) 
 a. Functions who can be substituted by their values wherever they are present.
  b. Functions which will return the same value always for the same set of input 
  c. Function who do not depend on the context in which they are called
  d. Function who do not change their response with time
  
  Functional Programmers often speak of implementing programs with a ure core and a thin layer on the outside that handles effects.
  
  If a function has to do a side effect it should not be observable from outside. For ex if a data structure of a variable is mutated noone outside the function should have a reference
  to it. If we are writing to a file, it should not be observable from outside the function by anybody
  
  
  
  
  An expression e is referentially transparent if, for all programs p, all occurrences of e can be replaced by its result, without effecting the meaning of program p
  A function f is said to be referentially transparent if expression f(x) is referentially transparent for all referentially transparent x
   
   
   Referentially transparent function force the invariant that whatever function does(which is observable from outside of function) is represented by the value it returns 