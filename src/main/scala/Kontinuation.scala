package dvh.cek

import Ops._

trait Kontinuation
case object EmptyKon extends Kontinuation
case class Fn(v: ValClosure, k: Kontinuation) extends Kontinuation
case class Ar(c: ExpClosure, k: Kontinuation) extends Kontinuation
case class Op(op: Ops,
              vs: List[ValClosure],
              cs: List[ExpClosure],
              k: Kontinuation) extends Kontinuation
