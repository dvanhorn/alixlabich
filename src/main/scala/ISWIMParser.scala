package dvh.cek

import scala.util.parsing.combinator._
import dvh.cek.Ops._

object ISWIMParser extends JavaTokenParsers {

  override def skipWhitespace = true

  // entry point, generic expression
  def expr: Parser[Expression] = (value
                                | oper
                                | app)

  def app: Parser[App] = (
    "(" ~ expr ~ expr ~ ")" ^^ { case s ~ l ~ r ~ s1 => App(l, r) }
  )

  def oper: Parser[Oper] = (
    "(" ~ primOp ~ rep1(expr) ~ ")" ^^ {
      case s ~ o ~ ms ~ s1 => Oper(o, ms)
    }
  )

  def value: Parser[Value] = (variable
                            | fun
                            | con)

  def variable: Parser[Var] = (
    ident ^^ { x => Var(Symbol(x)) }
  )

  // AMIRITE??
  def fun: Parser[Fun] = (
    "(lambda " ~ variable ~ "." ~ expr ~ ")" ^^ {
      case s ~ v ~ s1 ~ m ~ s2 => Fun(v, m)
    }
  )

  def con: Parser[Con] = (
    wholeNumber ^^ { case n => Con(Integer.parseInt(n)) }
  )

  def primOp: Parser[Ops] = (
    "add1" ^^ { case x => Ops.Add1}
    | "sub1" ^^ { case x => Ops.Sub1 }
    | "-" ^^ { case x => Ops.Sub }
    | "+" ^^ { case x => Ops.Add }
    | "^" ^^ { case x => Ops.Exp }
    // TODO(adam): make it so that this doesn't explode when isZero is fed
    // multiple exprs
    | "isZero" ^^ { case x => Ops.IsZero }
  )

}
