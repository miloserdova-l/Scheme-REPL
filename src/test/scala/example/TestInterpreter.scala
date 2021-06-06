package example

import org.scalatest._

import scala.reflect.ClassTag

//noinspection NameBooleanParameters
class TestInterpreter extends FlatSpec with Matchers {

  private def interpret(s: String, returnValue: Any,
                        functions: Map[String, Map[List[Name], List[Expression]]] = Map.empty): Assertion = {
    Parser(s) match {
      case Right(value) => assert(new Interpreter(functions)(value).toString == returnValue.toString)
      case Left(_) =>
        assert(false)
    }
  }

  private def interpretWithException[T <: Exception : ClassTag]
  (s: String, functions: Map[String, Map[List[Name], List[Expression]]] = Map.empty): Assertion = {
    Parser(s) match {
      case Right(value) => assertThrows[T] {
        Interpreter(functions)(value).toString
      }
      case Left(_) => fail()
    }
  }


  "Standard number operations" should "be interpreted" in {
    var input: String = "(+ 1 1 1 1 1 1)"
    interpret(input, 6)

    input = "(+ 1 (+ 2 (+ 3 (+ 4 5))))"
    interpret(input, 15)

    input = "(+ 1)"
    interpret(input, 1)

    input = "(- 2 1)"
    interpret(input, 1)

    input = "(- 2)"
    interpret(input, -2)

    input = "(- 5 2 3)"
    interpret(input, 0)

    input = "(* 2 3)"
    interpret(input, 6)

    input = "(* 2)"
    interpret(input, 2)

    input = "(* 1 2 3 4 5)"
    interpret(input, 120)


    input = "(/ 20 5 2)"
    interpret(input, 2)

    input = "(/ (+ 3 (* 2 3)) (- 5 2))"
    interpret(input, 3)

    input = "(/ 2 0)"
    interpret(input, "+inf.0") // Scheme тоже возвращает бесконечность

    input = "(/ -2 0)"
    interpret(input, "-inf.0")
  }

  "Comparison" should "be interpreted" in {
    var input = "(< 1 2)"
    interpret(input, "#t")

    input = "(> 10 8 4 2 0)"
    interpret(input, "#t")

    input = "(<= 1 2 2)"
    interpret(input, "#t")

    input = "(>= 10 8 4 2 0 0)"
    interpret(input, "#t")

    input = "(= 1 1 1)"
    interpret(input, "#t")

    input = "(= 1 1 2 2)"
    interpret(input, "#f")

    input = "(< 1 2)"
    interpret(input, "#t")

    input = "(> 10 8 4 2 0)"
    interpret(input, "#t")

    input = "(<= 1 2 2)"
    interpret(input, "#t")

    input = "(>= 10 8 4 2 0 0)"
    interpret(input, "#t")

    input = "(<= 1 2 2 1)"
    interpret(input, "#f")

    input = "(= 1 1 1)"
    interpret(input, "#t")

    input = "(= 1 1 2 2)"
    interpret(input, "#f")

    input = "(>= 10 8 11 2 0 0)"
    interpret(input, "#f")

    input = "(>= 10)"
    interpretWithException[ArgumentsNumberError](input)
  }

  "List operations" should "be interpreted" in {
    var input = "(car (list 1 2 3))"
    interpret(input, 1)

    input = "(cdr (list 1 2 3))"
    interpret(input, "(2 3)")

    input = "(car (list (cdr (list 1 2 3)) (list 4 5 6)))"
    interpret(input, "(2 3)")

    input = "(cdr (list (cdr (list 1 2 3)) (list 4 5 6) (list 7 8)))"
    interpret(input, "((4 5 6) (7 8))")

    input = "(car (list #\\space #\\a #\\b))"
    interpret(input, "#\\ ")

    input = "(car (list (list \"hello\" #\\a #\\b)))"
    interpret(input, "(\"hello\" #\\a #\\b)")

    input = "(car 1)"
    interpretWithException[ArgumentsTypeError](input)

    input = "(car (list 1) (list 2))"
    interpretWithException[ArgumentsNumberError](input)

    input = "(cdr 1)"
    interpretWithException[ArgumentsTypeError](input)

    input = "(cdr (list 1) (list 2))"
    interpretWithException[ArgumentsNumberError](input)
  }


  "Variables" should "be interpreted" in {
    var input = "(+ 1 x)"
    interpret(input, 2, Map("x" -> Map(List[Name]() -> List(IntTerm(1)))))

    input = "(list 1 (* x x))"
    interpret(input, "(1 25)", Map("x" -> Map(List[Name]() -> List(IntTerm(5)))))
  }


  "Functions" should "be interpreted" in {
    var input = "(sum 1 2)"
    interpret(input, 3, functions = Map("sum" -> Map(List(Name("x"), Name("y")) -> List(Function(Name("+"), List(Name("x"), Name("y")))))))

    input = "(sum (+ (+ 1 2) 15) (* 10 2))"
    interpret(input, 38, functions = Map("sum" -> Map(List(Name("x"), Name("y")) -> List(Function(Name("+"), List(Name("x"), Name("y")))))))

    input = "(f 5 6)"
    interpret(input, 65, Map("z" -> Map(List.empty -> List(IntTerm(10))), "f" ->
      Map(List(Name("x"), Name("y")) -> List(Function(Name("+"), List(Name("x"), Function(Name("*"), List(Name("y"), Name("z")))))))))

    input = "(f z 6)"
    interpret(input, 70, Map("z" -> Map(List.empty -> List(IntTerm(10))), "f" ->
      Map(List(Name("x"), Name("y")) -> List(Function(Name("+"), List(Name("x"), Function(Name("*"), List(Name("y"), Name("z")))))))))

    input = "(f x y)"
    interpretWithException[UndefinedVariableError](input, Map("z" -> Map(List.empty[Name] -> List(IntTerm(10))),
      "f" -> Map(List(Name("x"), Name("y")) -> List(Function(Name("+"), List(Name("x"), Function(Name("*"), List(Name("y"), Name("z")))))))))

    input = "(f (+ 1 x) z)"
    interpretWithException[UndefinedVariableError](input, Map("z" -> Map(List.empty[Name] -> List(IntTerm(10))), "f" ->
      Map(List(Name("x"), Name("y")) -> List(Function(Name("+"), List(Name("x"), Function(Name("*"), List(Name("y"), Name("z")))))))))

    input = "(f 1 z)"
    interpretWithException[UndefinedFunctionError](input, Map("z" -> Map(List.empty[Name] -> List(IntTerm(10)))))

    input = "(f z 2 3)"
    interpretWithException[ArgumentsNumberError](input, Map("z" -> Map(List.empty[Name] -> List(IntTerm(10))), "f" ->
      Map(List(Name("x"), Name("y")) -> List(Function(Name("+"), List(Name("x"), Function(Name("*"), List(Name("y"), Name("z")))))))))

    input = "(f (g 2 3) 4)"
    interpret(input, 10, functions = Map(
      "f" -> Map(List(Name("x"), Name("y")) -> List(Function(Name("+"), List(Name("x"), Name("y"))))),
      "g" -> Map(List(Name("x"), Name("y")) -> List(Function(Name("*"), List(Name("x"), Name("y"))))))) // перекрытие переменных

  }


  "Lambdas" should "be interpreted" in {
    var input = "(lambda (x) (* x x))"
    interpret(input, "#<Closure>")

    input = "((lambda (x) (* x x)) 5)"
    interpret(input, 25)

    input = "(+ 1 ((lambda (y) (* 10 y)) 5) 10)"
    interpret(input, 61)

    input = "(+ 1 ((lambda (y) (* 10 y)) 5 10))"
    interpretWithException[ArgumentsNumberError](input)
  }

  "Square function" should "be interpreted" in {
    var interpreter: Interpreter = new Interpreter()
    val defineA = Parser("(define a 3)")
    interpreter = defineA match {
      case Right(value) => interpreter(value).asInstanceOf[Interpreter]
    }
    val defineB = Parser("(define b 4)")
    interpreter = defineB match {
      case Right(value) => interpreter(value).asInstanceOf[Interpreter]
    }
    val defineFunction = Parser("(define (square x) (* x x))")
    interpreter = defineFunction match {
      case Right(value) => interpreter(value).asInstanceOf[Interpreter]
    }
    val input = Parser("(+ (square 3) (square 4))")
    input match {
      case Right(value) => assert(interpreter(value).toString == 25.toString)
      case _ => assert(false)
    }
  }

  "Quote, Quasiquote" should "be interpreted" in {
    var input = "'(list 1 #\\a 42 13 (+ 10 20 (* 5 15)))"
    interpret(input, input.drop(1))

    input = "'(define a 4)"
    interpret(input, input.drop(1))

    input = "'(define (f x y) (+ x y))"
    interpret(input, input.drop(1))

    input = "'(lambda (a b) (f a b))"
    interpret(input, input.drop(1))

    input = "'((lambda (x) (* x x)) 5)"
    interpret(input, input.drop(1))

    input = "`(list 1 #\\a 42 13 (+ 10 20 ,(* 5 15)))"
    interpret(input, "(list 1 #\\a 42 13 (+ 10 20 75))")

    input = "'(+ 1 ,(+ 3 4))"
    interpret(input, "(+ 1 (unquote (+ 3 4)))")

    input = "'(+ 1 '(+ 3 4))"
    interpret(input, "(+ 1 (quote (+ 3 4)))")

    input = "'(+ 1 `(+ 3 4))"
    interpret(input, "(+ 1 (quasiquote (+ 3 4)))")

    input = "`(+ 1 '(+ 3 4))"
    interpret(input, "(+ 1 (quote (+ 3 4)))")

    input = "`(+ 1 `(+ 3 4))"
    interpret(input, "(+ 1 (#<Syntax quasiquote> (+ 3 4)))")

    input = "`(+ 1 ,(+ 3 4))"
    interpret(input, "(+ 1 7)")

    input = "`(+ 1 '(+ 3 4 ,(* 3 5)))"
    interpret(input, "(+ 1 (quote (+ 3 4 15)))")

    input = "(list 7 6 5 ,(+ 1 2 3))"
    interpretWithException[UnquoteError](input)

    input = "(call/cc (lambda (c) (c ((lambda (c) `(call/cc (lambda (c) (c (,c ',c))))) '(lambda (c) `(call/cc (lambda (c) (c (,c ',c)))))))))"
    interpret(input, "(call/cc (lambda (c) (c ((lambda (c) (quasiquote (call/cc (lambda (c) (c ((unquote c) (quote (unquote c)))))))) (quote (lambda (c) (quasiquote (call/cc (lambda (c) (c ((unquote c) (quote (unquote c)))))))))))))")

    input = "(call/cc (lambda (c)  (call/cc (lambda (cc)  (c ((lambda (c) `(call/cc (lambda (c) (call/cc (lambda (cc) (c (,c ',c))))))) '(lambda (c) `(call/cc (lambda (c) (call/cc (lambda (cc) (c (,c ',c)))))))))))))"
    interpret(input, "(call/cc (lambda (c) (call/cc (lambda (cc) (c ((lambda (c) (quasiquote (call/cc (lambda (c) (call/cc (lambda (cc) (c ((unquote c) (quote (unquote c)))))))))) (quote (lambda (c) (quasiquote (call/cc (lambda (c) (call/cc (lambda (cc) (c ((unquote c) (quote (unquote c)))))))))))))))))")
  }

  "If" should "be interpreted" in {
    var input = "(if (> 2 1) \"true\" \"false\")"
    interpret(input, "\"true\"")

    input = "(if (< 2 1) \"true\" \"false\")"
    interpret(input, "\"false\"")
  }

  "Factorial" should "be interpreted" in {
    var interpreter: Interpreter = new Interpreter()
    var defineF = Parser("(define (fac n) (if (<= n 1) 1 (* n (fac (- n 1)))))")
    interpreter = defineF match {
      case Right(value) => interpreter(value).asInstanceOf[Interpreter]
    }
    var input = Parser("(fac 5)")
    input match {
      case Right(value) => assert(interpreter(value).toString == 120.toString)
      case _ => assert(false)
    }

    interpreter = new Interpreter()
    defineF = Parser("(define fac (lambda (n) (if (< n 1) 1 (* n (fac (- n 1))))))")
    interpreter = defineF match {
      case Right(value) => interpreter(value).asInstanceOf[Interpreter]
    }
    input = Parser("(fac 5)")
    input match {
      case Right(value) => assert(interpreter(value).toString == 120.toString)
      case _ => assert(false)
    }

    interpreter = new Interpreter()
    defineF = Parser("(define factorial-aps (lambda (n answer) (if (= n 0) answer (factorial-aps (- n 1) (* n answer)))))")
    interpreter = defineF match {
      case Right(value) => interpreter(value).asInstanceOf[Interpreter]
    }
    defineF = Parser("(define fac2 (lambda (n) (factorial-aps n 1)))")
    interpreter = defineF match {
      case Right(value) => interpreter(value).asInstanceOf[Interpreter]
    }
    input = Parser("(fac2 5)")
    input match {
      case Right(value) => assert(interpreter(value).toString == 120.toString)
      case _ => assert(false)
    }
  }

  "Call/cc" should "be interpreted" in {
    var input = "(+ 1 (call/cc (lambda (k) (+ 2 (k 3)))))"
    interpret(input, 4)

    input = "(+ 1 (call/cc (lambda (l) (define a 1) (+ 10 (l a) 15))))"
    interpret(input, 2)

    input = "(+ 1 (call/cc (lambda (l) (* 10 (l (call/cc (lambda (l) (/ 15 (l 5)))))))))"
    interpret(input, 6)

    input = "(+ 1 (call/cc (lambda (l) (* 10 (l (* 10 (call/cc (lambda (l) (/ 15 (l 5))))))))))"
    interpret(input, 51)

    input = "(+ 1 (call/cc (lambda (l) (* 10 (l (call/cc (lambda (k) (/ 15 (k 5)))))))))"
    interpret(input, 6)
  }

  "Odd/Even-positive" should "be interpreted" in {
    var interpreter: Interpreter = new Interpreter()
    var defineF = Parser("(define error-message-positive \"Error. x must be a nonnegative number\")")
    interpreter = defineF match {
      case Right(value) => interpreter(value).asInstanceOf[Interpreter]
    }
    defineF = Parser("(define odd-positive? (lambda (x) (if (= x 0) 0 (if (< x 0) error-message-positive (if (= (* (/ x 2) 2) x) 0 1)))))")
    interpreter = defineF match {
      case Right(value) => interpreter(value).asInstanceOf[Interpreter]
    }
    defineF = Parser("(define even-positive? (lambda (x) (if (= x 0) 1 (if (< x 0) error-message-positive (if (= (* (/ x 2) 2) x) 1 0)))))")
    interpreter = defineF match {
      case Right(value) => interpreter(value).asInstanceOf[Interpreter]
    }
    var input = Parser("(odd-positive? 4)")
    input match {
      case Right(value) => assert(interpreter(value).toString == 0.toString)
      case _ => assert(false)
    }
    input = Parser("(even-positive? 4)")
    input match {
      case Right(value) => assert(interpreter(value).toString == 1.toString)
      case _ => assert(false)
    }

    input = Parser("(odd-positive? 5)")
    input match {
      case Right(value) => assert(interpreter(value).toString == 1.toString)
      case _ => assert(false)
    }
    input = Parser("(even-positive? 7)")
    input match {
      case Right(value) => assert(interpreter(value).toString == 0.toString)
      case _ => assert(false)
    }

    input = Parser("(odd-positive? 0)")
    input match {
      case Right(value) => assert(interpreter(value).toString == 0.toString)
      case _ => assert(false)
    }
    input = Parser("(even-positive? 0)")
    input match {
      case Right(value) => assert(interpreter(value).toString == 1.toString)
      case _ => assert(false)
    }

    input = Parser("(odd-positive? -1)")
    input match {
      case Right(value) => assert(interpreter(value).toString == "\"Error. x must be a nonnegative number\"")
      case _ => assert(false)
    }
    input = Parser("(even-positive? -6)")
    input match {
      case Right(value) => assert(interpreter(value).toString == "\"Error. x must be a nonnegative number\"")
      case _ => assert(false)
    }
  }

}
