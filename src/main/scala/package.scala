package dvh

import scala.annotation.tailrec
import scala.math.pow

import cek.Ops._

package object cek {

  def parse(s: String): Expression =
    ISWIMParser.parse(ISWIMParser.expr, s).getOrElse(Con(-1))

  def eval(m: Expression): Value = evalCek(Closure(m, EmptyEnv), EmptyStore, EmptyKon)

  @tailrec private def evalCek(ce: Closure, s: Store, k: Kontinuation): Value = (ce, s, k) match {
    case (Closure(App(m, n), e), s, k) => // cesk1
      evalCek(Closure(m, e), s, Ar(Closure(n, e), k))
    case (Closure(Oper(o, m::ms), e), s, k) => // cesk2
      evalCek(Closure(m, e), s, Op(o, Nil, ms map { n => Closure(n, e) }, k))
    case (Closure(v: Var, e), s, k) =>  // cesk7
      evalCek(s(e(v)), s, k)
    case (Closure(v: Value, e), s, Fn(Closure(Fun(x, m), e1), k1)) => // cesk3
      evalCek(Closure(m, e1.bind(x, s.next)), ConsStore(s.next, Closure(v, e), s), k1)
    case (Closure(v: Value, e), s, Ar(Closure(m, e1), k1)) => // cesk4
      evalCek(Closure(m, e1), s, Fn(Closure(v, e), k1))
    case (Closure(v: Value, e), s, Op(o, vcs, c::cs, k1)) => // cesk6
      evalCek(c, s, Op(o, Closure(v, e)::vcs, cs, k1))
    case (Closure(v: Value, e), s, Op(o, vcs, Nil, k1)) => // cesk5
      evalCek(Closure(reduce(o, (Closure(v, e)::vcs).reverse), EmptyEnv), s, k1)
    case (Closure(v: Value, _), _, EmptyKon) => v // al1
    case _ => throw new RuntimeException("Bad code!") // al2
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
