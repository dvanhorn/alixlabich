package dvh.cek

case class Closure(m: Expression, en: Environment) {
  override def toString = "<"+m+" "+en+">"
}
