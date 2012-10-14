package dvh

import scala.annotation.tailrec
import scala.math.pow

import cek.Ops._

package object cek {

  def parse(s: String): Expression =
    ISWIMParser.parse(ISWIMParser.expr, s).getOrElse(Con(-1))

  def eval(m: Expression, d: Boolean = false): Value = evalCesk(Closure(m, EmptyEnv), EmptyStore, EmptyKon, d)

  private def printDebug(ce: Closure, s: Store, k: Kontinuation) {
    println("=========== DEBUG ===========")
    println("<M, ρ>: "+ce)
    println("     σ: "+s)
    println("     κ: "+k)
  }

  private def collectGarbage(grays: List[Location],
                             blacks: List[Location],
                             whites: List[Location],
                             s: Store): List[Location] = {
//   println((grays, blacks, whites))
   (grays, blacks, whites) match {
     case (gs, bs, w::ws) if gs.contains(w) => s(w) match {
       case Some(ce) =>
         collectGarbage(gs:::(ce.ll).filter(!(w::bs).contains(_)), w::bs, ws, s)
       case _ => collectGarbage(gs.filter(!(w::bs).contains(_)), w::bs, ws, s)
     }
     case (gs, bs, w::ws) =>
       collectGarbage(gs, bs, ws, s)
     case (_, bs, Nil) => bs
   }
                             }

  @tailrec private def evalCesk(ce: Closure, s: Store, k: Kontinuation, d: Boolean = false): Value = {
    if (d) { printDebug(ce, s, k) }
    val bs = collectGarbage(ce.ll:::k.ll, Nil, s.domain, s)
    (ce, bs.foldLeft[ListStore](EmptyStore)((a, n) => ConsStore(n, s(n), a)), k) match {
      case (Closure(App(m, n), e), s, k) => // cesk1
        evalCesk(Closure(m, e), s, Ar(Closure(n, e), k), d)
      case (Closure(Oper(o, m::ms), e), s, k) => // cesk2
        evalCesk(Closure(m, e), s, Op(o, Nil, ms map { n => Closure(n, e) }, k), d)
      case (Closure(v: Value, e), s, Fn(Closure(Fun(x, m), e1), k1)) => // cesk3
        val l = s.next
        evalCesk(Closure(m, e1.bind(x, l)), s.bind(l, Closure(v, e)), k1, d)
      case (Closure(v: Value, e), s, Ar(Closure(m, e1), k1)) => // cesk4
        evalCesk(Closure(m, e1), s, Fn(Closure(v, e), k1), d)
      case (Closure(v: Value, e), s, Op(o, vcs, Nil, k1)) => // cesk5
        evalCesk(Closure(reduce(o, (Closure(v, e)::vcs).reverse), EmptyEnv), s, k1, d)
      case (Closure(v: Value, e), s, Op(o, vcs, c::cs, k1)) => // cesk6
        evalCesk(c, s, Op(o, Closure(v, e)::vcs, cs, k1), d)
      case (Closure(v: Var, e), s, k) =>  // cesk7
        s(e(v)) match {
          case Some(ce1) => evalCesk(ce1, s, k, d)
          case _ => throw new RuntimeException("Dereferencing an unbound location for var: "+v+".")
        }
      case (Closure(Set(x, m), e), s, k) => // cesk8
        evalCesk(Closure(m, e), s, St(e(x), k), d)
      case (Closure(v: Value, e), s, St(l, k1)) => // cesk9
        s(l) match {
          case Some(ce1) => evalCesk(ce1, s.rebind(l, ce), k1, d)
          case _ => throw new RuntimeException("Dereferencing an unbound location: "+l+".")
        }
      case (Closure(Letrec((x, m)::xms, n), e), s, k) => // al3
        val l = s.next
        val e1 = e.bind(x, l)
        evalCesk(Closure(m, e1), s.alloc(l), Lr(x::xms.map(_._1), Nil, xms.map(_._2), e1, n, k), d)
      case (Closure(v: Value, e), s, Lr(x::y::xs, vvs, m::ms, e1, n, k1)) =>
        val l = s.next
        val e2 = e1.bind(y, l)
        evalCesk(Closure(m, e2), s.rebind(e(x), Closure(v, e2)).alloc(l), Lr(y::xs, (x, v)::vvs, ms, e2, n, k1), d)
      case (Closure(v: Value, e), s, Lr(x::Nil, vvs, Nil, e1, n, k1)) =>
        evalCesk(Closure(n, e1), ((x, v)::vvs).reverse.foldRight(s)((n, a) => a.rebind(e1(n._1), Closure(n._2, e1))), k1, d)
      case (Closure(v: Value, e), s, EmptyKon) => { // al1
        if (d) println("=> "+v)
        v
      }
      case (ce, s, k) =>
        throw new RuntimeException("Bad code!")
    }
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
    case _ => throw new RuntimeException("Bad primitive application: "+Ops.toString(o)+" "+vs)
  }

}
