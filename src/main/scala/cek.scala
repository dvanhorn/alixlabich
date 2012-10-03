package dvh.cek

package object cek {

  def eval(ex: Expression): Value = {
    evalCek(ExpClosure(ex, EmptyEnv), EmptyKon)
  }
  private def evalCek(c: Closure, k: Kontinuation): Value = c match {
    case ExpClosure(App(m, n), e) =>
      evalCek(ExpClosure(m, e), Ar(ExpClosure(n, e), k))
    case ExpClosure(Oper(o, m::ms), e) =>
      evalCek(ExpClosure(m, e), Op(o, Nil, ms map { n => ExpClosure(n, e) }, k))
  }

}
