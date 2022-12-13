// HW2, CMSC 22100, Autumn 2022

// ====== data definitions

enum Token:
  case LParen
  case RParen
  case KW_true
  case KW_false
  case Zero
  case KW_if
  case KW_succ
  case KW_pred
  case KW_iszero
  case KW_and
  case KW_or

enum Term:
  case True
  case False
  case Zero
  case If(t1:Term,t2:Term,t3:Term)
  case Succ(t1:Term)
  case Pred(t1:Term)
  case IsZero(t1:Term)
  case And(t1:Term,t2:Term)
  case Or(t1:Term,t2:Term)

enum NormalForm:
  case Value(t:Term)
  case Stuck(t:Term)

// ====== utilities

// If you see fit to write any utility functions, use this space.

// ====== value judgments

// isV determines if a term is a value
//def isV(t: Term): Boolean = throw new Exception("todo: isV")
def isV(t: Term): Boolean = t match
    case Term.Zero => true
    case Term.True => true
    case Term.False => true
    case _ => false

// isNV determines if a term is a numeric value
//def isNV(t: Term): Boolean = throw new Exception("todo: isNV")
def isNV(t: Term): Boolean = t match
    case Term.Zero => true
    case Term.Succ(v) => true
    //case Term.Pred(v) => true
    case _ => false

// ====== lexical scanner

// nextToken should return the next token from a list of characters, along with the characters thereafter
// - return None if the list of chars is empty
// - skip whitespace characters (use the isWhitespace char method for this)
// - throw an exception if the characters cannot be tokenized
//def nextToken(cs: List[Char]): Option[(Token, List[Char])] = throw new Exception("todo: nextToken")
def nextToken(cs: List[Char]): Option[(Token, List[Char])] = 
    
    if(cs.isEmpty)
        return None
    else
        var tail = cs.tail
        var head = cs.head
        
        //println(cs)

        if(Character.isWhitespace(head))
            nextToken(tail)
        else
            cs match
                case Nil => None
                case '(' :: tail => Some(Token.LParen, tail)
                case ')' :: tail => Some(Token.RParen, tail)
                case 't' :: 'r' :: 'u' :: 'e' :: tail => Some(Token.KW_true, tail)
                case 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tail => Some(Token.KW_false, tail)
                case '0' :: tail => Some(Token.Zero, tail)
                case 'i' :: 'f' :: tail => Some(Token.KW_if, tail)
                case 's' :: 'u' :: 'c' :: 'c' :: tail => Some(Token.KW_succ, tail)
                case 'p' :: 'r' :: 'e' :: 'd' :: tail => Some(Token.KW_pred, tail)
                case 'i' :: 's' :: 'z' :: 'e' :: 'r' :: 'o' :: tail => Some(Token.KW_iszero, tail)
                case 'a' :: 'n' :: 'd' :: tail => Some(Token.KW_and, tail)
                case 'o' :: 'r' :: tail => Some(Token.KW_or, tail)
                //case Character.isWhitespace(head) :: tail => nextToken(tail) //skipping white space
                case _ => throw new Exception("Token Error: input cannot be tokenized")
    
// turn a string of code (like "(pred 0)" into a list of tokens (like List(LParen,Pred,Zero,RParen)) 
//def scan(code: String): List[Token] = throw new Exception("todo: scan")
def scan(code: String): List[Token] = 
    var chars = code.toList
    var token = nextToken(chars)
    //println(chars)
    token match
        //case Some((x, y)) => scan(y.mkString) :+ x
        case Some((x, y)) => List[Token](x).concat(scan(y.mkString))
        case _ => List[Token]()


// ====== parser

// nextTerm should return the next term from a list of tokens, along with the tokens thereafter
// - return None if the list of tokens is empty
// - throw an exception if the characters cannot be tokenized
//def nextTerm(ts: List[Token]): Option[(Term, List[Token])] = throw new Exception("todo: nextTerm")
def nextTerm(ts: List[Token]): Option[(Term, List[Token])] =
    if(ts.isEmpty)
        return None
    else
        var tail = ts.tail
        var head = ts.head// remove before this
        ts match
            case Nil => None
            case Token.KW_true :: tail => Some(Term.True, tail)
            case Token.KW_false :: tail => Some(Term.False, tail)
            case Token.Zero :: tail => Some(Term.Zero, tail)
            //(succ t)
            case Token.LParen :: Token.KW_succ :: tail =>
                nextTerm(tail) match
                    case None => throw new Exception("nextTerm Error: succ ended unexpectedly")
                    //we are type checking tail1 to be a right parenthesis
                    case Some(t1, Token.RParen :: tail1) => Some(Term.Succ(t1), tail1)
                    case _ => throw new Exception("nextTerm Error: succ missing right parenthesis")
            //(pred t)
            case Token.LParen :: Token.KW_pred :: tail =>
                nextTerm(tail) match
                    case None => throw new Exception("nextTerm Error: pred ended unexpectedly")
                    case Some(t1, Token.RParen :: tail1) => Some(Term.Pred(t1), tail1)
                    case _ => throw new Exception("nextTerm Error: pred missing right parenthesis")
            //(iszero t)
            case Token.LParen :: Token.KW_iszero :: tail => 
                nextTerm(tail) match
                    case None => throw new Exception("nextTerm Error: iszero ended unexpectedly")
                    case Some(t1, Token.RParen :: tail1) => Some(Term.IsZero(t1), tail1)
                    case _ => throw new Exception("nextTerm Error: iszero missing right parenthesis")
            //(and t t)
            case Token.LParen :: Token.KW_and :: tail =>
                nextTerm(tail) match
                    case None => throw new Exception("nextTerm Error: and ended unexpectedly")
                    case Some(t1, tail1) =>
                        nextTerm(tail1) match
                            case Some(t2, t3::tail2) => t3 match
                                case Token.RParen => Some(Term.And(t1, t2), tail2)
                                case _ => throw new Exception("nextTerm Error: and missing right parenthesis")
                            case None => throw new Exception("nextTerm Error: and missing a term")
                            case _ => throw new Exception("nextTerm Error: and missing a term")
                            //case Some(t2, Token.RParen :: tail2) => Some(Term.And(t1, t2), tail2)
                            //case _ => throw new Exception("nextTerm Error: and ended unexpectedly")
            //(or t t)
            case Token.LParen :: Token.KW_or :: tail =>
                nextTerm(tail) match
                    case None => throw new Exception("nextTerm Error: or ended unexpectedly")
                    case Some(t1, tail1) =>
                        nextTerm(tail1) match
                            case Some(t2, t3::tail2) => t3 match
                                case Token.RParen => Some(Term.Or(t1, t2), tail2)
                                case _ => throw new Exception("nextTerm Error: or missing right parenthesis")
                            case None => throw new Exception("nextTerm Error: or missing a term")
                            case _ => throw new Exception("nextTerm Error: or missing a term")
            //(if t t t)
            case Token.LParen :: Token.KW_if :: tail =>
                nextTerm(tail) match
                    case Some(t1, tail1) => nextTerm(tail1) match
                        case Some(t2, tail2) => nextTerm(tail2) match
                            case Some(t3, t4 :: tail3) => t4 match
                                case Token.RParen => Some(Term.If(t1, t2, t3), tail3)
                                case _ => None
                            case _ => None
                        case None => None
                    case None => None
            case _ => throw new Exception("not here yet")




// turn a list of tokens (like List(LParen,Pred,Zero,RParen)) into a term (list Pred(Zero))
//def parse(tokens: List[Token]): Term = throw new Exception("todo: parse")
def parse(tokens: List[Token]): Term = 
    var term = nextTerm(tokens)
    term match 
        case Some((x, y)) => x
        case _ => throw new Exception("parse Error: incorrect values")



// ====== small-step evaluator

// if a term is not a normal form, take the next possible step
// - i.e., if t -> u, then step(t) should return Some(u)
// if a term is a normal form, return None
//def step(t: Term): Option[Term] = throw new Exception("todo: step")
def step(t: Term): Option[Term] = 
    //True
    //False
    //Zero
    if(isV(t))
        return None
    else
        t match
            //Succ(t1)
            case Term.Succ(t1) => 
            //does this need to be t1
                if(isNV(t1))
                    None
                else
                    step(t1) match
                        case Some(u) => Some(Term.Succ(u))
                        case _ => None
            
            //Pred(t1)
            //                                                              Office Hours for Pred!!!!
            case Term.Pred(t1) => t1 match
                case Term.Zero => Some(Term.Zero)
                //case Some(u) => Some(Term.Succ(u))
                case _ => None
            
            //isZero(t1)
            case Term.IsZero(t1) => t1 match 
                case Term.Zero => Some(Term.True)
                case t if isNV(t) => Some(Term.False)
                case t1 => step(t1) match
                    case Some(t1) => Some(Term.IsZero(t1))
                    case None => None
                /*if(isNV(t1))
                        Some(Term.False)
                    else
                        case Term.Zero => Some(Term.True)
                        case t1 => step(t1) match
                            case Some(t1) => Some(Term.IsZero(t2))
                            case None => None*/
                        
            //And(t1, t2)
            case Term.And(t1, t2) => t1 match
                //if the first term is True
                case Term.True => Some(t2)
                //if the first term is False
                case Term.False => Some(Term.False)
                case t1 => step(t1) match  
                    //t3 is functionally t1' 
                    case Some(t3) => Some(Term.And(t3, t2))
                    case _ => None

            //Or(t1, t2)
            case Term.Or(t1, t2) =>  t1 match
                case Term.True => Some(Term.True)
                case Term.False => Some(t2)
                case t1 => step(t1) match
                    //t3 functionally acts as t1'
                    case Some(t3) => Some(Term.Or(t3, t2))
                    case None => None

            //If(t1, t2, t3)
            case Term.If(t1, t2, t3) => t1 match
                case Term.True => Some(t2)
                case Term.False => Some(t3)
                case t1 => step(t1) match
                    //t4 is functionally t1'
                    case Some(t4) => Some(Term.If(t4, t2, t3))
                    case None => None

            case _ => None




// build the whole list of steps from the original term to the normal form
//def steps(t: Term): List[Term] = throw new Exception("todo: steps")
def steps(t: Term): List[Term] = 
    var s = step(t)
    s match 
        case Some(x) => List[Term](x).concat(step(t))
        case None => List[Term]()

// ===== interpreter

def classify(t: Term): NormalForm = throw new Exception("todo: classify")

@main def interpret(code: String): Unit =
  val tokens = scan(code)
  val ast    = parse(tokens)
  val terms  = steps(ast)
  println("   " + terms.head)
  for term <- terms.tail do
    println("-> " + term)
  println(classify(terms.last))



//                                     test functions

//testing isV()
@main def test_isV() = 
    printf("testing isV with True: %b\n", isV(Term.True))
    printf("testing isV with False: %b\n", isV(Term.False))
    printf("testing isV with 0: %b\n", isV(Term.Zero))
    printf("testing isV with succ(0): %b\n", isV(Term.Succ(Term.Zero)))

//testing isNV()
@main def test_isNV() = 
    printf("testing isNV with 0: %b\n", isNV(Term.Zero))
    printf("testing isNV with succ(0): %b\n", isNV(Term.Succ(Term.Zero)))
    printf("testing isNV with pred(0): %b\n", isNV(Term.Pred(Term.Zero)))
    printf("testing isNV with pred(pred(0)): %b\n", isNV(Term.Pred(Term.Pred(Term.Zero))))
    printf("testing isNV with True: %b\n", isNV(Term.True))
    printf("testing isNV with False: %b\n", isNV(Term.False))

//testing nextToken()
@main def test_nextToken() = 
    val leftparen = List('(', 's', 'u', 'c', 'c', ')')
    printf(s"testing nextToken with a left parenthesis: ${nextToken(leftparen)}\n")

    val rightparen = List(')', 'a', 'n', 'd')
    printf(s"testing nextToken with a right parenthesis: ${nextToken(rightparen)}\n")

    val kwtrue = List('t', 'r', 'u', 'e')
    printf(s"testing nextToken with true: ${nextToken(kwtrue)}\n")

    val kwfalse = List('f', 'a', 'l', 's', 'e')
    printf(s"testing nextToken with true: ${nextToken(kwfalse)}\n")

    val zero = List('0', 'a', 'n', 'd')
    printf(s"testing nextToken with 0: ${nextToken(zero)}\n")

    val kwif = List('i', 'f', '(', ')')
    printf(s"testing nextToken with if: ${nextToken(kwif)}\n")

    val succ = List('s', 'u', 'c', 'c', '(', ')')
    printf(s"testing nextToken with succ: ${nextToken(succ)}\n")

    val pred = List('p', 'r', 'e', 'd', '(', ')')
    printf(s"testing nextToken with pred: ${nextToken(pred)}\n")

    val iszero = List('i', 's', 'z', 'e', 'r', 'o', '(', ')')
    printf(s"testing nextToken with iszero: ${nextToken(iszero)}\n")

    val and = List('a', 'n', 'd', '(', ')')
    printf(s"testing nextToken with and: ${nextToken(and)}\n")

    val or = List('o', 'r', '(', ')')
    printf(s"testing nextToken with or: ${nextToken(or)}\n")

    val whitespace = List(' ', '(', ')')
    printf(s"testing nextToken with whitespace folllowed by left parenthesis: ${nextToken(whitespace)}\n")

    val none = List[Char]()
    println(none.length)
    //println("made it here!!!!!")
    printf(s"testing nextToken with empty list: ${nextToken(none)}\n")


@main def test_scan() =     
    printf(s"testing scan with (pred 0): ${scan("(pred 0)")}\n")
    printf(s"testing scan with (and (if 0) 0): ${scan("(and (if 0) 0)")}\n")
    printf(s"testing scan with (if (and (or true false) false) 0 (pred true)): ${scan("(if (and (or true false)false) 0 (pred true))")}\n")

@main def test_nextTerm() =     
    printf(s"testing nextTerm with List(LParen,Succ,Zero,RParen): ${nextTerm(List(Token.LParen,Token.KW_succ,Token.Zero,Token.RParen))}\n")
    printf(s"testing nextTerm with List(LParen,Pred,Zero,RParen): ${nextTerm(List(Token.LParen,Token.KW_pred,Token.Zero,Token.RParen))}\n")
    printf(s"testing nextTerm with List(LParen,IsZero,Zero,RParen): ${nextTerm(List(Token.LParen,Token.KW_iszero,Token.Zero,Token.RParen))}\n")
    //printf(s"testing nextTerm with List(LParen,Pred,Zero): ${nextTerm(List(Token.LParen,Token.KW_pred,Token.Zero))}\n")
    printf(s"testing nextTerm with List(LParen,And,True,True,RParen: ${nextTerm(List(Token.LParen,Token.KW_and,Token.KW_true,Token.KW_true,Token.RParen))}\n")
    printf(s"testing nextTerm with List(LParen,Or,True,True,RParen: ${nextTerm(List(Token.LParen,Token.KW_or,Token.KW_true,Token.KW_true,Token.RParen))}\n")
    //not: does not properly work with If
    printf(s"testing nextTerm with List(LParen,If,True,True,True,RParen: ${nextTerm(List(Token.LParen,Token.KW_if,Token.KW_true,Token.KW_true,Token.KW_true,Token.RParen))}\n")

@main def test_parse() =
    printf(s"testing parse with List(LParen,Pred,Zero,RParen): ${parse(List(Token.LParen,Token.KW_pred,Token.Zero,Token.RParen))}\n")

@main def test_step() = 
    print(s"testing step with (if (and f t) 0 (succ 0)): ${step(Term.If(Term.And(Term.False, Term.True), Term.Zero, Term.Succ(Term.Zero)))}")

@main def test_steps() = 
    print(s"testing steps with (if (and f t) 0 (succ 0)): ${steps(Term.If(Term.And(Term.False, Term.True), Term.Zero, Term.Succ(Term.Zero)))}")