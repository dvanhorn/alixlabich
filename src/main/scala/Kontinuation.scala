package dvh.cek

trait Kontinuation
case object EmptyKon extends Kontinuation
case class Fn(v: ValClosure, k: Kontinuation) extends Kontinuation
case class Ar(c: ExpClosure, k: Kontinuation) extends Kontinuation
case class Op(op: Operation,
              vs: List[ValClosure],
              cs: List[ExpClosure],
              k: Kontinuation) extends Kontinuation
