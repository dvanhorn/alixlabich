package dvh.cek

package object cek {

  def eval(ex: Expression): Value = {
    evalCek(ExpClosure(ex, EmptyEnv), EmptyKon)
  }
  private def evalCek(c: Closure, k: Kontinuation): Value = (c, k) match {
    case (ExpClosure(App(m, n), e), k) =>
      evalCek(ExpClosure(m, e), Ar(ExpClosure(n, e), k))
    case (ExpClosure(Oper(o, m::ms), e), k) =>
      evalCek(ExpClosure(m, e), Op(o, Nil, ms map { n => ExpClosure(n, e) }, k))
    case (ValClosure(Var(n), e), k) =>
      evalCek(e(n), k)
    case (v: ValClosure, Fn(ValClosure(Fun(x, m), e1), k1)) =>
      evalCek(ExpClosure(m, e1.bind(x, v)), k)
    case (v: ValClosure, Ar(ExpClosure(m, e1), k1)) =>
      evalCek(ExpClosure(m, e1), Fn(v, k1))
//    case (v: ValClosure, Op(o, vs, c::cs, k1)) =>
      
  }

}
