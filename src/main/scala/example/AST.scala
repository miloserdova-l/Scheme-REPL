package example

sealed trait Term extends Expression
case class IntTerm(value: Int) extends Term
case class StrTerm(value: String) extends Term
case class CharTerm(value: String) extends Term
case class ListTerm(value: List[Expression]) extends Term


sealed trait Expression

sealed trait Names extends Expression
case class Name(name: String) extends Names
case class UnquotedName(name: Names) extends Names
case class QuasiquotedName(name: Names) extends Names

case class Define(name: Names, params: List[Names], body: List[Expression]) extends Expression
case class Function(name: Names, args: List[Expression]) extends Expression
case class LambdaFunction(lambda: Lambda, args: List[Expression]) extends Expression
case class Lambda(params: List[Names], body: List[Expression]) extends Expression
case class CallCC(param: Names, expressions: List[Expression]) extends Expression
case class Quote(data: Expression) extends Expression
case class Unquote(data: Expression) extends Expression
case class Quasiquote(data: Expression) extends Expression

case class Cont(value: Expression) extends Expression with Value
