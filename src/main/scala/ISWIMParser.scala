package dvh.cek

import scala.util.parsing.combinator._
import dvh.cek.Ops._

object ISWIMParser extends JavaTokenParsers {

  override def skipWhitespace = true

  // entry point, generic expression
  def expr: Parser[Expression] = (
    value ^^ { case value => println("matched value: " + value);value }
    | oper ^^ { case oper => oper }
    | app ^^ { case app => app }
  )

  def app: Parser[App] = (
    expr ~ expr ^^ { case expr1 ~ expr2 => App(expr1, expr2) }
  )

  def oper: Parser[Oper] = (
    "(" ~ primOp ~ rep1(expr) ~ ")" ^^ {
      case "(" ~ primop ~ exprs ~ ")" => println("matched an oper");Oper(primop, exprs)
    }
  )

  def value: Parser[Value] = (
    variable ^^ { case variable => variable }
    | fun ^^ { case fun => fun }
    | con ^^ { case con => con }
  )

  def variable: Parser[Var] = (
    ident ^^ { case ident => println("matching vars succeeded!" + ident); Var(Symbol(ident)) }
  )

  // AMIRITE??
  def fun: Parser[Fun] = (
    "(lambda " ~ variable ~ "." ~ expr ~ ")" ^^ {
      case "(lambda" ~ variable ~ "." ~ expr ~ ")" => println("MATCHING FUN YO"); Fun(variable, expr)
    }
  )

  def con: Parser[Con] = (
    wholeNumber ^^ { case number => Con(Integer.parseInt(number)) }
  )

  def primOp: Parser[Ops] = (
    "add1" ^^ { case add1 => Ops.Add1}
    | "sub1" ^^ { case sub1 => Ops.Sub1 }
    | "-" ^^ { case sub => Ops.Sub }
    | "+" ^^ { case add => Ops.Add }
    | "^" ^^ { case exp => Ops.Exp }
    // TODO(adam): make it so that this doesn't explode when isZero is fed
    // multiple exprs
    | "isZero" ^^ { case isZero => Ops.IsZero }
  )

}
