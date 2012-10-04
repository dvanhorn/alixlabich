package dvh.cek

import scala.math.pow

import cek._
import Ops._

package object cek {

  def parse(s: String): Expression =
    ISWIMParser.parse(ISWIMParser.expr, s).getOrElse[Expression](Con(-1))

  def eval(ex: Expression): Value = ex match {
    case b: Con => b
    case v: Value => evalCek(ValClosure(v, EmptyEnv), EmptyKon)
    case x: Expression => evalCek(ExpClosure(ex, EmptyEnv), EmptyKon)
  }

  private def evalCek(c: Closure, k: Kontinuation): Value = { 
    println("eval("+c+", "+k+")")
    (c, k) match {
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
    case (v: ValClosure, Op(o, vs, c::cs, k1)) =>
      evalCek(c, Op(o, v::vs, cs, k1))
    case (v: ValClosure, Op(o, vs, Nil, k1)) =>
      evalCek(ValClosure(reduce(o, (v::vs).reverse), EmptyEnv), k1)
    case _ => throw new RuntimeException("Bad code!")
  }}
  private def reduce(o: Ops, vs: List[ValClosure]): Value = (o, vs map { v => v.v }) match {
      case (Add1, Con(n)::Nil) => Con(n+1)
      case (Sub1, Con(n)::Nil) => Con(n-1)
      case (IsZero, Con(0)::Nil) => Fun(Var('x), Fun(Var('y), Var('x)))
      case (IsZero, Con(n)::Nil) => Fun(Var('x), Fun(Var('y), Var('y)))
      case (Add, Con(m)::Con(n)::Nil) => Con(m+n)
      case (Sub, Con(m)::Con(n)::Nil) => Con(m-n)
      case (Mul, Con(m)::Con(n)::Nil) => Con(m*n)
      case (Exp, Con(m)::Con(n)::Nil) => Con(pow(m,n))
      case _ => throw new RuntimeException("Bad primitive application!")
  }

}
