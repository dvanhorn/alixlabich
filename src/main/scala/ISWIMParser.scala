package dvh.cek

import scala.util.parsing.combinator._
import dvh.cek.Ops._

object ISWIMParser extends JavaTokenParsers {

  override def skipWhitespace = true

  // entry point, generic expression
  def expr: Parser[Expression] = ( value | oper | app | variable )

  def app: Parser[App] = (
    "(" ~ expr ~ expr ~ ")" ^^ { case lparen ~ l ~ r ~ rparen => App(l, r) }
  )

  def oper: Parser[Oper] = (
    "(" ~ primOp ~ rep1(expr) ~ ")" ^^ {
      case lparen ~ o ~ ms ~ rparen => Oper(o, ms)
    }
  )

  def value: Parser[Value] = ( fun | con )

  def variable: Parser[Var] = (
    ident ^^ { x => Var(Symbol(x)) }
  )

  // AMIRITE??
  def fun: Parser[Fun] = (
    "(lambda " ~ variable ~ "." ~ expr ~ ")" ^^ {
      case lambda ~ v ~ dot ~ m ~ rparen => Fun(v, m)
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
    | "*" ^^ { case x => Ops.Mul }
    // TODO(adam): make it so that this doesn't explode when isZero is fed
    // multiple exprs
    | "isZero" ^^ { case x => Ops.IsZero }
  )

}
