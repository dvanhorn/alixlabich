package dvh.cek

trait Expression
trait Value extends Expression
case class Var(name: Symbol) extends Value
case class Fun(param: Var, ex: Expression) extends Value
case class Con(value: Int) extends Value
case class App(m: Expression, n: Expression) extends Expression
object Ops extends Enumeration {
  type Ops = Value
  val Add1, Sub1, IsZero, Add, Sub, Mul, Exp = Value
}
case class Oper(o: Ops.Value, ms: List[Expression]) extends Expression
