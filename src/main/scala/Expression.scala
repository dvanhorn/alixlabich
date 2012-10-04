package dvh.cek

trait Expression
case class App(m: Expression, n: Expression) extends Expression
case class Oper(o: Ops.Value, ms: List[Expression]) extends Expression

trait Value extends Expression
case class Var(name: Symbol) extends Value
case class Fun(param: Var, m: Expression) extends Value
case class Con(value: Double) extends Value
object Ops extends Enumeration {
  type Ops = Value
  val Add1, Sub1, IsZero, Add, Sub, Mul, Exp = Value
}
