# lambda-calculus
lambda calculus interpreter written in Haskell

An interpreter program written in Haskell. To run use runghc as follows.

Prelude> runghc lambda.hs  
lambda> ((lx.x) a)  
a  

Greek character "lambda" is substituted by English lowercase 'l'. An abstraction term or an application term needs parentheses on both side of the term. Parentheses of abstraction terms and application terms are not to be omitted.  And an application term needs space beteween consisting two terms.

Lambda caluculus interpreter can load presetting file by ":load" command.

lambda> :load  
file name: defined.txt  
lambda> ((Y g) 3)  
(ls.(lz.(s (s (s (s (s (s z))))))))  

Lambda term can be bound to the variable by the bind term.

lambda> (= id (lx.x))  
lambda> (id a)  
a  
