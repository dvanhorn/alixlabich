package dvh.cek

trait Environment extends Function1[Var, Closure] {
  def domain: List[Var] = Nil
  def range: List[Closure] = Nil
}
trait ListEnv extends Environment
object EmptyEnv extends ListEnv {
  def apply(v: Var) =
    throw new RuntimeException("The variable " + v.name + " is not in this environment.")
}
class ConsEnv(v: Var, c: Closure, e: Environment) extends ListEnv {
  def apply(v1: Var) = if (v.name == v1.name) c else e(v1)
  def domain = v :: e.domain
  def range = c :: e.range
}
