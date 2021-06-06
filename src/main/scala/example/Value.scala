package example

trait Value

case class NumVal(value: Int) extends Value {
  def +(b: NumVal): NumVal = NumVal(value + b.value)

  def -(b: NumVal): NumVal = NumVal(value - b.value)

  def *(b: NumVal): NumVal = NumVal(value * b.value)

  def /(b: NumVal): NumVal = {
    b.value match {
      case 0 => if (value >= 0) NumVal(Int.MaxValue) else NumVal(Int.MinValue)
      case _ => NumVal(value / b.value)
    }
  }

  def >(b: NumVal): BoolVal = BoolVal(value > b.value)

  def <(b: NumVal): BoolVal = BoolVal(value < b.value)

  def >=(b: NumVal): BoolVal = BoolVal(value >= b.value)

  def <=(b: NumVal): BoolVal = BoolVal(value <= b.value)

  def ==(b: NumVal): BoolVal = BoolVal(value == b.value)

  override def toString: String = {
    value match {
      case Int.MaxValue => "+inf.0"
      case Int.MinValue => "-inf.0"
      case v => v.toString
    }
  }
}

case class BoolVal(value: Boolean) extends Value {
  override def toString: String = if (value) "#t" else "#f"
}

case class CharVal(value: Char) extends Value {
  def this(s: String) = this(s match {
    case x if x.length == 1 => x.head
    case "space" => ' '
    case "newline" => '\n'
    case _ => throw CharParseError()
  })

  override def toString: String = "#\\" + value
}

case class ListVal(value: List[Value]) extends Value {
  override def toString: String = value.map(_.toString).mkString("(", " ", ")")
}

case class FunVal(value: Map[List[Name], List[Expression]]) extends Value {
  override def toString: String = "#<Closure>"
}

case class StrVal(value: String) extends Value {
  override def toString: String = value
}