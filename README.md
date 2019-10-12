# lambda-calculus
lambda calculus interpreter written in Haskell

An interpreter program written in Haskell. To run use runghc as follows.

Prelude> runghc lambda.hs  
lambda> ((lx.x) a)  
a  

Greek character "lamda" is substituted by English lowercase 'l'. Parentheses are not to ommitted. Abstreact term and Application terms need Parentheses both side. Application term needs space beteween terms.

Lambda caluculus interpreter can load presetting file by :load command.

lambda> :load  
file name: defined.txt  
lambda> ((Y g) 3)  
(ls.(lz.(s (s (s (s (s (s z))))))))  

Lambda term can be bind to a variable by bind term.

lambda> (= id (lx.x))  
lambda> (id a)  
a  
