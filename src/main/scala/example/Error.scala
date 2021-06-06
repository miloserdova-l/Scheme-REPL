package example

sealed abstract class InterpreterException extends Exception
final case class ArgumentsTypeError(msg: String = "Wrong type of arguments") extends InterpreterException
final case class ArgumentsNumberError(msg: String = "Wrong number of arguments") extends InterpreterException
final case class UndefinedFunctionError(msg: String = "Undefined function") extends InterpreterException
final case class UndefinedVariableError(msg: String = "Undefined variable") extends InterpreterException
final case class DoubleDefinitionError(msg: String = "Double definition error") extends InterpreterException
final case class UnquoteError(msg: String = "Unquote must be inside quasiquote") extends InterpreterException
final case class QuasiquoteError(msg: String = "Quasiquoted name of variable or function must be inside quasiquote") extends InterpreterException
final case class OutputError(msg: String = "Expression must have end result") extends InterpreterException

sealed abstract class ParseException extends Exception
final case class LambdaParseError(msg: String = "Error while parsing lambda") extends ParseException
final case class CharParseError(msg: String = "Incorrect data to parse of char type") extends ParseException