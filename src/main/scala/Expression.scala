package dvh.cek

trait Expression
trait Value extends Expression
trait Operation extends Expression
case class Var(name: Symbol) extends Value
case class Fun(param: Var, ex: Expression) extends Value
case class Con(value: Int) extends Value
case class App(ex1: Expression, ex2: Expression) extends Expression
case class Add1(ex: Expression) extends Operation
case class Sub1(ex: Expression) extends Operation
case class IsZero(ex: Expression) extends Operation
case class Add(ex1: Expression, ex2: Expression) extends Operation
case class Sub(ex1: Expression, ex2: Expression) extends Operation
