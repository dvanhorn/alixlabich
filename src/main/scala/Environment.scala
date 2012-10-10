package dvh.cek

trait Environment extends Function1[Var, Location] {
  def domain: List[Var] = Nil
  def range: List[Location] = Nil
  def bind(v: Var, l: Location): Environment
}
trait ListEnv extends Environment
case object EmptyEnv extends ListEnv {
  def apply(v1: Var) =
    throw new RuntimeException("The variable " + v1.name + " is not in the environment.")
  def bind(v: Var, l: Location) = ConsEnv(v, l, EmptyEnv)
  override def toString = "[]"
}
case class ConsEnv(v: Var, l: Location, e: Environment) extends ListEnv {
  def apply(v1: Var) = if (v.name == v1.name) l else e(v1)
  def bind(v1: Var, l1: Location) =
    if (v == v1) ConsEnv(v, l1, e) else ConsEnv(v, l, e.bind(v1, l1))
  override def domain = v :: e.domain
  override def range = l :: e.range
  override def toString = "[" + v + ", " + l + ", " + e + "]"
}
