package dvh.cek

trait Closure
case class ExpClosure(m: Expression, en: Environment) extends Closure
case class ValClosure(v: Value, en: Environment) extends Closure
