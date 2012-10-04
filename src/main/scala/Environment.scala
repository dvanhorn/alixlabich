package dvh.cek

trait Environment extends Function1[Symbol, Closure] {
  def domain: List[Var] = Nil
  def range: List[Closure] = Nil
  def bind(v: Var, c: ValClosure): Environment
}
trait ListEnv extends Environment
case object EmptyEnv extends ListEnv {
  def apply(n: Symbol) =
    throw new RuntimeException("The variable " + n + " is not in the environment.")
  def bind(v: Var, c: ValClosure) =
    throw new RuntimeException("The variable " + v.name + " is not in the environment.")
  override def toString = "[]"
}
case class ConsEnv(v: Var, c: Closure, e: Environment) extends ListEnv {
  def apply(n: Symbol) = if (v.name == n) c else e(n)
  def bind(v1: Var, c1: ValClosure) =
    if (v == v1) ConsEnv(v, c1, e) else ConsEnv(v, c, e.bind(v1, c1))
  override def domain = v :: e.domain
  override def range = c :: e.range
  override def toString = "[" + v + ", " + c + ", " + e + "]"
}
