package dvh.cek

trait Expression
case class App(m: Expression, n: Expression) extends Expression {
  override def toString = "("+m+" "+n+")"
}
case class Oper(o: Ops.Value, ms: List[Expression]) extends Expression {
  override def toString = {
    val tmp = "("+Ops.toString(o)+" "+ms.foldRight("")(_+" "+_)
    tmp.substring(0, tmp.length-1) + ")"
  }
}
case class Var(name: Symbol) extends Expression {
  override def toString = name.name
}
case class Set(x: Var, m: Expression) extends Expression {
  override def toString = "(set "+x+" "+m+")"
}

trait Value extends Expression
case class Fun(x: Var, m: Expression) extends Value {
  override def toString = "(λ "+x+" . "+m+")"
}
case class Con(value: Double) extends Value {
  override def toString =
    if (value < Int.MaxValue)
      value.asInstanceOf[Int].toString
    else
      value.toString
}
object Ops extends Enumeration {
  type Ops = Value
  val Add1, Sub1, IsZero, Add, Sub, Mul, Exp = Value
  def toString(v: Value) = v match {
    case Add1   => "add1"
    case Sub1   => "sub1"
    case IsZero => "isZero"
    case Add    => "+"
    case Sub    => "-"
    case Mul    => "*"
    case Exp    => "^"
  }
}
