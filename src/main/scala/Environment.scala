package dvh.cek

trait Environment extends Function1[Var, Location] {
  def domain: List[Var] = Nil
  def range: List[Location] = Nil
}
trait ListEnv extends Environment
case object EmptyEnv extends ListEnv {
  def apply(v1: Var) =
    throw new RuntimeException("The variable " + v1.name + " is not in the environment.")
  override def toString = "[]"
}
case class ConsEnv(v: Var, l: Location, e: Environment) extends ListEnv {
  def apply(v1: Var) = if (v.name == v1.name) l else e(v1)
  override def domain = v :: e.domain
  override def range = l :: e.range
  override def toString = "[" + v + ", " + l + ", " + e + "]"
}
