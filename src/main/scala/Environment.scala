package dvh.cek

trait Environment extends Function1[Var, Closure] {
  def domain: List[Var] = Nil
  def range: List[Closure] = Nil
  def bind(v: Var, c: Closure): Environment
}
trait ListEnv extends Environment
case object EmptyEnv extends ListEnv {
  def apply(v1: Var) =
    throw new RuntimeException("The variable " + v1.name + " is not in the environment.")
  def bind(v: Var, c: Closure) = ConsEnv(v, c, EmptyEnv)
  override def toString = "[]"
}
case class ConsEnv(v: Var, c: Closure, e: Environment) extends ListEnv {
  def apply(v1: Var) = if (v.name == v1.name) c else e(v1)
  def bind(v1: Var, c1: Closure) =
    if (v == v1) ConsEnv(v, c1, e) else ConsEnv(v, c, e.bind(v1, c1))
  override def domain = v :: e.domain
  override def range = c :: e.range
  override def toString = "[" + v + ", " + c + ", " + e + "]"
}
