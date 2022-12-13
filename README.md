Your task this week is to implement an interpreter for a simple language along the lines of what is presented in Chapter 3 of TaPL. More specifically, you will need to implement a lexical scanner, a recursive descent parser, and a small-step evaluator for that language.

The language contains the same term forms as in Chapter 3, with the addition of two binary boolean operators, and and or.

The term grammar in Chapter 3 is abstract syntax, which is to say it is not concerned with how an actual programmer would type the terms, character for character. In order to design a working programming language, we'll need a concrete syntax. Our concrete syntax for HW2 is modeled on Scheme (~Racket), not because it's anyone's preferred syntax for sheer beauty, but it is (like Scheme's) especially easy to parse, if one is a compiler writer.

The concrete syntax of the language looks like this:

true
false
0
(if t t t)
(succ t)
(pred t)
(iszero t)
(and t t)
(or t t)

(The if form omits keywords then and else, for brevity.)

Note that every term t that appears in this sketch is a metavariable, and every t needs to be replaced, in a concrete program, by a concrete term.  For example, this is a concrete term:

(if (and (or true false) false) 0 (pred 0))
The evaluation system is just as given in the text (and the last few lectures), supplemented by the following rules for and and or:


A full sketch of a working interpreter is given below (at the bottom of this post). The parts of the code that you are to complete are indicated with a thrown exception, like

throw new Exception("todo: nextToken")
Please create a directory in your repository called hw2, and then save this Scala code in a file named HW2.scala inside that directory. You should complete the assignment entirely in the HW2.scala file.

The functions you need to complete for this assignment are exactly these: isV, isNV, nextToken, scan, nextTerm, parse, step, steps, and classify. The type of each function will give you strong guidance for how to proceed with each one. Read the comment above each function, think about its type, and proceed from there.

The main program interpret is already written for you. Here is a transcript of some interactions with the finished interpreter for your reference.

bash-3.2$ scala interpret "true"
   True
Value(True)
bash-3.2$ scala interpret "(and true false)"
   And(True,False)
-> False
Value(False)
bash-3.2$ scala interpret "(and true (and true (or false true)))"
   And(True,And(True,Or(False,True)))
-> And(True,Or(False,True))
-> Or(False,True)
-> True
Value(True
bash-3.2$ scala interpret "(pred (if true 0 (succ 0)))"
   Pred(If(True,Zero,Succ(Zero)))
-> Pred(Zero)
-> Zero
Value(Zero)
bash-3.2$ scala interpret "(pred (if false 0 (succ 0)))"
   Pred(If(False,Zero,Succ(Zero)))
-> Pred(Succ(Zero))
-> Zero
Value(Zero)
bash-3.2$ scala interpret "(if (and (or true false) false) 0 (pred 0))"
   If(And(Or(True,False),False),Zero,Pred(Zero))
-> If(And(True,False),Zero,Pred(Zero))
-> If(False,Zero,Pred(Zero))
-> Pred(Zero)
-> Zero
Value(Zero)
bash-3.2$ scala interpret "(if (and (or true false) false) 0 (pred true))"
   If(And(Or(True,False),False),Zero,Pred(True))
-> If(And(True,False),Zero,Pred(True))
-> If(False,Zero,Pred(True))
-> Pred(True)
Stuck(Pred(True))

There will be time for questions and answers about this programming in the next lecture, and I plan to address scanning and parsing (about which I anticipate some questions) at that time.
