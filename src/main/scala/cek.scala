package dvh.cek

import scala.math.pow

import cek._
import Ops._

package object cek {

  def parse(s: String): Expression =
    ISWIMParser.parse(ISWIMParser.expr, s).getOrElse[Expression](Con(-1))

  def eval(m: Expression): Value = evalCek(Closure(m, EmptyEnv), EmptyKon)

  private def evalCek(c: Closure, k: Kontinuation): Value = (c, k) match {
    case (Closure(App(m, n), e), k) =>
      evalCek(Closure(m, e), Ar(Closure(n, e), k))
    case (Closure(Oper(o, m::ms), e), k) =>
      evalCek(Closure(m, e), Op(o, Nil, ms map { n => Closure(n, e) }, k))
    case (Closure(Var(n), e), k) =>
      evalCek(e(n), k)
    case (Closure(v: Value, e), Fn(Closure(Fun(x, m), e1), k1)) =>
      evalCek(Closure(m, e1.bind(x, Closure(v, e))), k1)
    case (Closure(v: Value, e), Ar(Closure(m, e1), k1)) =>
      evalCek(Closure(m, e1), Fn(Closure(v, e), k1))
    case (Closure(v: Value, e), Op(o, vs, c::cs, k1)) =>
      evalCek(c, Op(o, Closure(v, e)::vs, cs, k1))
    case (Closure(v: Value, e), Op(o, vs, Nil, k1)) =>
      evalCek(Closure(reduce(o, (Closure(v, e)::vs).reverse), EmptyEnv), k1)
    case (Closure(v: Value, EmptyEnv), EmptyKon) => v
    case _ => throw new RuntimeException("Bad code!")
  }

  private def reduce(o: Ops, vs: List[Closure]): Value = (o, vs map { c => c.m }) match {
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
