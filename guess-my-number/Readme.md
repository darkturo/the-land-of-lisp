The exercise
============
Program a simple game in CLisp, that can guess a number between 1 and 100. 
This is the first example of the book, and it provides a good start by learning
to define functions in lisp, use and define global variables, and learning to
call functions as well, 

How it works
============
The user should call guess-my-number, and the computer will say a number
between 1 and 100. Then the user should tell the computer if it is bigger, or
smaller, using the corresponding functions, until the number is found.
To start the game again, the function start-over can be used.

Example, where the user has choosen the number 41.
```
[1]> (guess-my-number)
50
[2]> (smaller)
26
[3]> (bigger)
38
[4]> (bigger)
44
[5]> (smaller)
41
```
