package dvh.cek

import scala.annotation.tailrec

trait Expression {
  override def toString = prettyString
  def plug(x: Var, v: Value): Expression
  def prettyString: String
  def av: List[Var]
  def fv: List[Var]
}
case class Letrec(xms: List[(Var, Expression)], n: Expression) extends Expression {
  def plug(x: Var, v: Value) = {
    def plugr(xms1: List[(Var, Expression)], a: List[(Var, Expression)] = Nil): Letrec = 
      xms match {
        case (y, m)::xms2 if x != y => plugr(xms2, (y, m.plug(x, v))::a)
        case p::xms2 => plugr(xms2, p::a)
        case Nil => Letrec(a.reverse, n.plug(x, v))
      }
    plugr(xms, Nil)
  }
  def prettyString = "(letrec {"+toString(xms)+"} "+n+")"
  @tailrec private def toString(xms: List[(Var, Expression)], a: String = ""): String =
    xms match {
      case (x, m)::yms => toString(yms, a+"("+x+", "+m+") ")
      case _ => a.substring(0, a.length-1).toString
    }
  def av = (xms.flatMap(_._2.av):::n.av).filter(xms.map(_._1).contains(_))
  def fv = (xms.flatMap(_._2.fv):::n.fv).filter(xms.map(_._1).contains(_))
}
case class App(m: Expression, n: Expression) extends Expression {
  def plug(x: Var, v: Value) = App(m.plug(x, v), n.plug(x, v))
  def prettyString = "("+m+" "+n+")"
  def av = m.av:::n.av
  def fv = m.fv:::n.fv
}
case class Oper(o: Ops.Value, ms: List[Expression]) extends Expression {
  def plug(x: Var, v: Value) = Oper(o, ms.map(_.plug(x, v)))
  def prettyString = {
    val tmp = "("+Ops.toString(o)+" "+ms.foldRight("")(_+" "+_)
    tmp.substring(0, tmp.length-1) + ")"
  }
  def av = ms flatMap { _.av }
  def fv = ms flatMap { _.fv }
}
case class Var(name: Symbol) extends Expression {
  def plug(x: Var, v: Value) = if (this == x) v else this
  def prettyString = name.name
  def av = Nil
  def fv = List(this)
}
case class Set(x: Var, m: Expression) extends Expression {
  // TODO: Might have to replace x here, per FV def on p155.
  def plug(y: Var, v: Value) = Set(x, m.plug(y, v))
  def prettyString = "(set "+x+" "+m+")"
  def av = x::m.av
  def fv = x::m.fv
}

trait Value extends Expression
case class Fun(x: Var, m: Expression) extends Value {
  def plug(y: Var, v: Value) = Fun(x, if (x == y) m else m.plug(y, v))
  def prettyString = "(λ"+x+"."+m+")"
  def av = m.fv.filter(_ != x)
  def fv = m.fv.filter(_ != x)
}
case class Con(value: Double) extends Value {
  def plug(x: Var, v: Value) = this
  def prettyString =
    if (value < Int.MaxValue)
      value.asInstanceOf[Int].toString
    else
      value.toString
  def av = Nil
  def fv = Nil
}
object Ops extends Enumeration {
  type Ops = Value
  val Add1, Sub1, IsZero, Add, Sub, Mul, Exp = Value
  def toString(v: Value) = v match {
    case Add1   => "add1"
    case Sub1   => "sub1"
    case IsZero => "isZero"
    case Add    => "+"
    case Sub    => "-"
    case Mul    => "*"
    case Exp    => "^"
  }
}
