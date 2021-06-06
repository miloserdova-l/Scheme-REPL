package example

import example.Interpreter.Context

object Interpreter {
  type Context = Map[String, Map[List[Name], List[Expression]]]

  def apply(ctx: Context = Map.empty)(expression: Expression): Value = new Interpreter(ctx)(expression)
}

class Interpreter(private val context: Context = Map.empty) extends (Expression => Value) with Value {

  override def apply(expression: Expression): Value = {
    expression match {
      case IntTerm(value) => NumVal(value)
      case StrTerm(value) => StrVal(value)
      case CharTerm(value) => new CharVal(value)
      case ListTerm(value) => ListVal(value.map(Interpreter(context)))
      case Name(name) => if (context.isDefinedAt(name)) compute(context(name)(List.empty)) else throw UndefinedVariableError()
      case UnquotedName(_) => throw UnquoteError()
      case QuasiquotedName(_) => throw QuasiquoteError()
      case Function(name, args) => func(convertToName(name), args)
      case Define(name, params, body) => defineFunction(convertToName(name), params.map(p => convertToName(p)), body)
      case Lambda(params, body) => FunVal(Map(params.map(p => convertToName(p)) -> body))
      case LambdaFunction(lambda, args) => computeDefinedFunction(Map(lambda.params.map(p => convertToName(p)) -> lambda.body), args)
      case Quote(data) => quasiquote(data, quote = true)
      case Quasiquote(data) => quasiquote(data, quote = false)
      case Unquote(_) => throw UnquoteError()
      case CallCC(param, expressions) =>
        val newCtx = Map(convertToName(param).name -> Map(List(Name(" x")) -> List[Expression](Cont(Name(" x")))))
        compute(expressions, newCtx) match {
          case Cont(value) => compute(List(value), newCtx)
          case x => x
        }
      case Cont(x) => Cont(toExpression(apply(x)))
    }
  }

  private def convertToName(name: Names): Name = {
    name match {
      case n: Name => n
      case _: UnquotedName => throw UnquoteError()
      case _: QuasiquotedName => throw QuasiquoteError()
    }
  }

  private def compute(code: List[Expression], localVar: Context = Map.empty): Value = {
    val newInterpreter = new Interpreter(mergeMaps(context, localVar))
    val v: Value = null
    val res = code.foldLeft((newInterpreter, v)) {(ans, x) => ans._1(x) match {
      case newInter: Interpreter => (newInter, ans._2)
      case value: Value => (ans._1, value)
    }}._2
    if (res == null)
      throw OutputError()
    res
  }

  private def mergeMaps[E, T](first: Map[E, T], second: Map[E, T]): Map[E, T] = {
    val res: Map[E, T] = first.map {
      case v if second.contains(v._1) => v._1 -> second(v._1)
      case v => v
    }
    (res.toSeq ++ second.toSeq)
      .groupBy(_._1)
      .mapValues(x =>
        x.map(_._2).toSet match {
          case it if it.size == 1 => it.head
          case _ => throw DoubleDefinitionError()
        }
      )
  }

  private def func(name: Name, args: List[Expression]): Value = {
    name.name match {
      case "+" => numberOperation((a, b) => a + b, NumVal(0), checkNumberOperation(args))
      case "-" => numberOperation((a, b) => a - b, NumVal(0), checkNumberOperation(args))
      case "*" => numberOperation((a, b) => a * b, NumVal(1), checkNumberOperation(args))
      case "/" => numberOperation((a, b) => a / b, NumVal(1), checkNumberOperation(args))
      case "<" => compare((a, b) => a < b, checkNumberOperation(args))
      case ">" => compare((a, b) => a > b, checkNumberOperation(args))
      case "<=" => compare((a, b) => a <= b, checkNumberOperation(args))
      case ">=" => compare((a, b) => a >= b, checkNumberOperation(args))
      case "=" => compare((a, b) => a == b, checkNumberOperation(args))
      case lstFun@("car" | "cdr") =>
        if (args.length != 1)
          throw ArgumentsNumberError()
        if (args.exists(x => new Interpreter(context)(x) match {
          case _: ListVal => false
          case cont: Cont => return cont
          case _ => true
        }))
          throw ArgumentsTypeError()
        Interpreter(context)(args.head) match {
          case ListVal(h :: t) => if (lstFun == "car") h else ListVal(t)
          case _ => throw ArgumentsNumberError()
        }
      case f if context.isDefinedAt(f) => computeDefinedFunction(context(f), args)
      case "if" => conditionals(args)
      case _ => throw UndefinedFunctionError()
    }
  }

  private def checkNumberOperation(args: List[Expression]): List[Value] = {
    val interpretedArgs: List[Value] = args.map(x => apply(x))
    interpretedArgs.map {
      case value: NumVal => value
      case cont: Cont => return List(cont)
      case _ => throw ArgumentsTypeError()
    }
  }

  private def numberOperation(f: (NumVal, NumVal) => NumVal, first: NumVal, args: List[Value]): Value = {
    if (args.isEmpty)
      throw ArgumentsNumberError()
    var terms: List[NumVal] = args.map {
      case value: NumVal => value
      case cont: Cont => return cont
    }
    if (terms.length == 1)
      terms = first :: terms
    terms.reduceLeft(f)
  }

  private def compare(f: (NumVal, NumVal) => BoolVal, args: List[Value]): Value = {
    if (args.length < 2)
      throw ArgumentsNumberError()
    val terms: List[NumVal] = args.map {
      case value: NumVal => value
      case cont: Cont => return cont
    }
    BoolVal(terms.sliding(2).forall {
      case Seq(x, y) => f(x, y).value
    })
  }

  private def defineFunction(name: Name, params: List[Name], body: List[Expression]): Interpreter = {
    new Interpreter(mergeMaps(context, Map(name.name -> Map(params -> body))))
  }

  private def computeDefinedFunction(m: Map[List[Name], List[Expression]], args: List[Expression]): Value = {
    val interpretedArgs = args.map(x => Interpreter(context)(x) match {
      case cont: Cont => return cont
      case a => a
    }) // исключаем добавление в мапу x -> x
    val localVars = interpretedArgs.map(x => toExpression(x))
    m.foreach {
      case (key, value) =>
        val env = value.last match {
          // Случай когда в теле функции лямбда.
          case l: Lambda if args.length == key.length + l.params.length =>
            val lambdaResult = toExpression(Interpreter(context)(LambdaFunction(l, args.drop(key.length))))
            Some(value.dropRight(1) :+ lambdaResult, localVars.dropRight(args.length - key.length))

          case _ if key.length == args.length => Some(value, localVars)
          case _ => None
        }
        env match {
          case Some((code, localVars)) =>
            return compute(code, key.zip(localVars).map(x => (x._1.name, Map(List.empty[Name] -> List(x._2)))).toMap)
          case None =>
        }
      case _ =>
    }
    throw ArgumentsNumberError()
  }

  private def toExpression(value: Value): Expression = {
    value match {
      case NumVal(x) => IntTerm(x)
      case StrVal(x) => StrTerm(x)
      case CharVal(x) => CharTerm(x.toString)
      case ListVal(x) => ListTerm(x.map(x => toExpression(x)))
    }
  }

  def conditionals(args: List[Expression]): Value = {
    if (args.length != 2 && args.length != 3)
      throw ArgumentsNumberError()
    compute(List(args.head)) match {
      case BoolVal(condition) => Interpreter(context)(args(if (condition) 1 else 2))
      case cont: Cont => cont
      case _ => throw ArgumentsTypeError()
    }
  }

  private def quasiquote(data: Expression, quote: Boolean): StrVal = {
    StrVal(data match {
      case IntTerm(value) => value.toString
      case StrTerm(value) => value
      case CharTerm(value) => new CharVal(value).toString
      case ListTerm(values) => p("list " + listToString(values, quote).strip())
      case Name(name) => name
      case UnquotedName(name) => quasiquote(Unquote(name), quote).value
      case QuasiquotedName(name) => quasiquote(Quasiquote(name), quote).value
      case Function(name, args) => p(quasiquote(name, quote) + listToString(args, quote))
      case Define(name, params, body) if params.isEmpty => p("define " + quasiquote(name, quote) + listToString(body, quote))
      case Define(name, params, body) => p("define " + p(quasiquote(name, quote) + listToString(params, quote)) + listToString(body, quote))
      case Lambda(params, body) => p("lambda " + p(listToString(params, quote)) + listToString(body, quote))
      case LambdaFunction(lambda, args) => p(quasiquote(lambda, quote) + listToString(args, quote))
      case Unquote(data) => if (!quote) Interpreter(context)(data).toString else p("unquote " + quasiquote(data, quote))
      case Quote(data) => p("quote " + quasiquote(data, quote))
      case Quasiquote(data) => if (quote) p("quasiquote " + quasiquote(data, quote)) else p("#<Syntax quasiquote> " + quasiquote(data, quote))
      case CallCC(param, expressions) => p("call/cc " + p("lambda " + p(quasiquote(param, quote).value) + listToString(expressions, quote)))
      case Cont(x) => quasiquote(x, quote).value
    })
  }

  private def listToString(list: List[Expression], quote: Boolean): String = {
    list.foldLeft("")((cur, value) => cur + " " + quasiquote(value, quote))
  }

  private def p(p: => String): String = "(" + p.strip() + ")"
}
