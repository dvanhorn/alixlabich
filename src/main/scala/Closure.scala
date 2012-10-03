package dvh.cek

trait Closure
case class ExpClosure(ex: Expression, en: Environment) extends Closure
case class ValClosure(v: Value, en: Environment) extends Closure
