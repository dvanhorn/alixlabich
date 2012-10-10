package dvh.cek

import scala.util.parsing.combinator._
import dvh.cek.Ops._

object ISWIMParser extends JavaTokenParsers {

  override def skipWhitespace = true

  def expr: Parser[Expression] = value | oper | variable | app

  def app: Parser[Expression] =
    "(" ~ rep1(expr) ~ ")" ^^ {
      case l ~ (m::ms) ~ r => ms.foldLeft(m)((a, n) => App(a, n))
      case l ~ Nil ~ r => throw new RuntimeException("Won't occur.")
    }

  def oper: Parser[Oper] =
    "(" ~ primOp ~ rep1(expr) ~ ")" ^^ {
      case lparen ~ o ~ ms ~ rparen => Oper(o, ms)
    }
  
  def value: Parser[Value] = fun | con
  
  def variable: Parser[Var] = ident ^^ { x => Var(Symbol(x)) }
  
  // AMIRITE??
  def fun: Parser[Fun] =
    "(" ~ ("lambda" | "λ") ~ rep1(variable) ~ "." ~ expr ~ ")" ^^ {
      case l ~ lam ~ vs ~ dot ~ m ~ r => vs match {
        case w::ws => Fun(w, ws.foldRight(m)((y, n) => Fun(y, n)))
        case _ => throw new RuntimeException("Cannot create a function " +
                                             "without any parameters.")
      }
    }
  
  def con: Parser[Con] =
    wholeNumber ^^ { case n => Con(Integer.parseInt(n)) }

  // TODO(adam): make it so that this doesn't explode when isZero is fed      
  // multiple exprs
  def primOp: Parser[Ops] = (
      "add1" ^^ { case x => Ops.Add1 }
  |   "sub1" ^^ { case x => Ops.Sub1 }
  |      "-" ^^ { case x => Ops.Sub }
  |      "+" ^^ { case x => Ops.Add }
  |      "^" ^^ { case x => Ops.Exp }
  |      "*" ^^ { case x => Ops.Mul }
  | "isZero" ^^ { case x => Ops.IsZero }
  )

}
