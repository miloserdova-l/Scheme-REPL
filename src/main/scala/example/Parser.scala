package example

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object Parser extends RegexParsers with PackratParsers {

  case class ParserError(msg: String)

  def apply(code: String): Either[ParserError, Expression] =
    parse(expression, new PackratReader(new CharSequenceReader(code))) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(ParserError(msg))
    }

  private def expression: Parser[Expression] = (
    term
      | callcc
      | define
      | lambda
      | member
      | id
      | "'" ~> expression ^^ Quote
      | "," ~> expression ^^ Unquote
      | "`" ~> expression ^^ Quasiquote
    )

  private def term = (
    int ^^ IntTerm
      | char ^^ CharTerm
      | string ^^ StrTerm
      | p("list" ~> expression.+) ^^ ListTerm
    )

  private def int = "-?[0-9]+".r ^^ (_.toInt)
  private def char = "#\\\\".r ~> "[0-9]+|[a-zA-Z][a-zA-Z]*".r
  private def string = "\"[a-zA-Z0-9][a-zA-Z0-9_\\-\\s?!.+/*<=>^~%]*\"".r

  private def define = p(
    "define" ~> fullId ~ expression.+ ^^ { case name ~ v => Define(name, List(), v) }
      | "define" ~> p(fullId ~ fullId.+) ~ expression.* ^^ { case name ~ params ~ v => Define(name, params, v) }
  )

  private def id = "[+\\-*/<=>^~.!%?a-zA-Z0-9_]+".r ^^ Name
  private def fullId: Parser[Names] =
    id |
      "," ~> fullId ^^ UnquotedName |
      "`" ~> fullId ^^ QuasiquotedName

  private def lambda = p("lambda" ~> p(fullId.*) ~ expression.*) ^^ { case args ~ v => Lambda(args, v) }

  private def member = p(
    fullId ~ expression.+ ^^ { case op ~ args => Function(op, args) }
      | lambda ~ expression.+ ^^ { case lambda ~ args => LambdaFunction(lambda, args) }
  )

  private def callcc = p("call/cc" ~> p("lambda" ~> p(fullId) ~ expression.*)) ^^ { case name ~ expressions => CallCC(name, expressions) }

  private def p[T](p: => Parser[T]): Parser[T] = "(" ~> p <~ ")"
}
