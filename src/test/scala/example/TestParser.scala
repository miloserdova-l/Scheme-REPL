package example

import org.scalatest._

class TestParser extends FlatSpec with Matchers {

  "+" should "be parsed" in {
    var v = Parser("(+ 1 2)")
    assert(v == Right(Function(Name("+"), List(IntTerm(1), IntTerm(2)))))

    v = Parser("(+ 1 2 3 4 5)")
    assert(v == Right(Function(Name("+"), List(IntTerm(1), IntTerm(2), IntTerm(3), IntTerm(4), IntTerm(5)))))

    v = Parser("(+ 2 (* 3 4))")
    assert(v == Right(Function(Name("+"), List(IntTerm(2), Function(Name("*"), List(IntTerm(3), IntTerm(4)))))))
  }

  "Other standard operations on numbers" should "be parsed" in {
    var v = Parser("(- 1 2)")
    assert(v == Right(Function(Name("-"), List(IntTerm(1), IntTerm(2)))))

    v = Parser("(* 2 3)")
    assert(v == Right(Function(Name("*"), List(IntTerm(2), IntTerm(3)))))

    v = Parser("(/ 1 2)")
    assert(v == Right(Function(Name("/"), List(IntTerm(1), IntTerm(2)))))

    v = Parser("(< 1 2)")
    assert(v == Right(Function(Name("<"), List(IntTerm(1), IntTerm(2)))))
  }

  "Define variable" should "be parsed" in {
    var v = Parser("(define src 2)")
    assert(v == Right(Define(Name("src"), List(), List(IntTerm(2)))))

    v = Parser("(define a (+ 1 3))")
    assert(v == Right(Define(Name("a"), List(), List(Function(Name("+"), List(IntTerm(1), IntTerm(3)))))))
  }

  "Unary functions" should "be parsed" in {
    var v = Parser("(+ (square 1) (square 2))")
    assert(v == Right(Function(Name("+"), List(Function(Name("square"), List(IntTerm(1))), Function(Name("square"), List(IntTerm(2)))))))

    v = Parser("(sin (/ pi 2))")
    assert(v == Right(Function(Name("sin"), List(Function(Name("/"), List(Name("pi"), IntTerm(2)))))))

    v = Parser("(define (~ x y) (+ x y))")
    assert(v == Right(Define(Name("~"), List(Name("x"), Name("y")), List(Function(Name("+"), List(Name("x"), Name("y")))))))
  }

  "Define functions" should "be parsed" in {
    val v = Parser("(define (square x) (* x x))")
    assert(v == Right(Define(Name("square"), List(Name("x")), List(Function(Name("*"), List(Name("x"), Name("x")))))))
  }

  "Lambdas" should "be parsed" in {
    var v = Parser("(lambda (x) (* x x))")
    assert(v == Right(Lambda(List(Name("x")), List(Function(Name("*"), List(Name("x"), Name("x")))))))

    v = Parser("(lambda () (display \"hello\"))")
    assert(v == Right(Lambda(List(), List(Function(Name("display"), List(StrTerm("\"hello\"")))))))

    v = Parser("((lambda (x) (* x x)) 5)")
    assert(v == Right(LambdaFunction(Lambda(List(Name("x")), List(Function(Name("*"), List(Name("x"), Name("x"))))), List(IntTerm(5)))))

  }

  "List" should "be parsed" in {
    var v = Parser("(list 1 \"string\" 2)")
    assert(v == Right(ListTerm(List(IntTerm(1), StrTerm("\"string\""), IntTerm(2)))))

    v = Parser("(list 12345 (list 1 \"string\" #\\c))")
    assert(v == Right(ListTerm(List(IntTerm(12345), ListTerm(List(IntTerm(1), StrTerm("\"string\""), CharTerm("c")))))))

    v = Parser("(define lst (list \"hello\" \"my\" \"world\"))")
    assert(v == Right(Define(Name("lst"), List(), List(ListTerm(List(StrTerm("\"hello\""), StrTerm("\"my\""), StrTerm("\"world\"")))))))
  }

  "Quote, Quasiquote" should "be parsed" in {
    var v = Parser("'3")
    assert(v == Right(Quote(IntTerm(3))))

    v = Parser("'\"hi\"")
    assert(v == Right(Quote(StrTerm("\"hi\""))))

    v = Parser("'(+ 3 4)")
    assert(v == Right(Quote(Function(Name("+"), List(IntTerm(3), IntTerm(4))))))

    v = Parser("'(lambda (x) (+ x 3))")
    assert(v == Right(Quote(Lambda(List(Name("x")), List(Function(Name("+"), List(Name("x"), IntTerm(3))))))))

    v = Parser("`(+ 1 ,(+ 3 4))")
    assert(v == Right(Quasiquote(Function(Name("+"), List(IntTerm(1), Unquote(Function(Name("+"), List(IntTerm(3), IntTerm(4)))))))))

    v = Parser("'(,c 5)")
    assert(v == Right(Quote(Function(UnquotedName(Name("c")), List(IntTerm(5))))))
  }

}
