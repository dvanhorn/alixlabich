package dvh.cek

import Ops._

trait Kontinuation
case object EmptyKon extends Kontinuation
case class Fn(v: Closure, k: Kontinuation) extends Kontinuation
case class Ar(c: Closure, k: Kontinuation) extends Kontinuation
case class Op(op: Ops,
              vs: List[Closure],
              cs: List[Closure],
              k: Kontinuation) extends Kontinuation
