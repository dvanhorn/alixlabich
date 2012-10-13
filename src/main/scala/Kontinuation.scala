package dvh.cek

import Ops._

trait Kontinuation {
  val sep = ", "
  protected def toString(l: List[_]): String = toString(l, "(")
  protected def toString(l: List[_], a: String): String = l match {
    case x::xs => toString(xs, a+x+sep)
    case _ =>
      if (a.length >= sep.length)
        a.substring(0, a.length-sep.length)+")"
      else
        a+")"
  }
}
case object EmptyKon extends Kontinuation {
  override def toString = "mt"
}
case class Fn(v: Closure, k: Kontinuation) extends Kontinuation {
  override def toString = "fn("+v+sep+"κ)"
}
case class Ar(c: Closure, k: Kontinuation) extends Kontinuation {
  override def toString = "ar("+c+sep+"κ)"
}
case class Op(op: Ops,
              vs: List[Closure],
              cs: List[Closure],
              k: Kontinuation) extends Kontinuation {
  override def toString =
    "op("+Ops.toString(op)+", "+toString(vs)+sep+toString(cs)+", κ)"
}
case class St(l: Location, k: Kontinuation) extends Kontinuation {
  override def toString = "st("+l+sep+"κ)"
}
case class Lr(xs: List[Var],
              vvs: List[(Var, Value)],
              ms: List[Expression],
              e: Environment,
              n: Expression,
              k: Kontinuation) extends Kontinuation {
  override def toString =
    "lr("+toString(xs)+sep+toString(vvs)+sep+toString(ms)+sep+e+sep+n+sep+"κ)"
}
