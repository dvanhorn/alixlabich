package dvh.cek

import scala.util.parsing.combinator._
import dvh.cek.Ops._

object ISWIMParser extends JavaTokenParsers {

  override def skipWhitespace = true

  def expr: Parser[Expression] = letrec | value | set | oper | variable | app

  def oparen: Parser[String] = "(" | "[" | "{"
  def cparen: Parser[String] = ")" | "]" | "}"

  def letrec: Parser[Expression] =
    oparen ~ "letrec " ~ oparen ~ rep1(lrclause) ~ cparen ~ expr ~ cparen ^^ {
      case _ ~ _ ~ _ ~ xvs ~ _ ~ m ~ _ => Letrec(xvs, m)
    }

  def lrclause: Parser[(Var, Expression)] =
    oparen ~ variable ~ expr ~ cparen ^^ {
      case _ ~ vari ~ valu ~ _ => (vari, valu)
    }

  def set: Parser[Expression] =
    oparen ~ "set" ~ variable ~ expr ~ cparen ^^ {
      case l ~ x ~ m ~ r => Set(x, m)
    }

  def app: Parser[Expression] =
    oparen ~ rep1(expr) ~ cparen ^^ {
      case l ~ (m::ms) ~ r => appLeft(m, ms)
      case l ~ Nil ~ r => throw new RuntimeException("Won't occur.")
    }
  private def appLeft(m: Expression, ms: List[Expression]): Expression =
    ms.foldLeft(m)((a, n) => App(a, n))

  def oper: Parser[Oper] =
    oparen ~ "" ~ primOp ~ rep1(expr) ~ cparen ^^ {
      case l ~ o ~ ms ~ r => Oper(o, ms)
    }
  
  def value: Parser[Value] = fun | con
  
  def variable: Parser[Var] = ident ^^ { x => Var(Symbol(x)) }
  
  // AMIRITE??
  def fun: Parser[Fun] =
    oparen ~ ("lambda" | "λ") ~ rep1(variable) ~ "." ~ rep1(expr) ~ cparen ^^ {
      case l ~ lam ~ vs ~ dot ~ (m::ms) ~ r => vs match {
        case w::ws => Fun(w, ws.foldRight(appLeft(m, ms))((y, n) => Fun(y, n)))
        case _ => throw new RuntimeException("Will not create a function " +
                                             "without any parameters.")
      }
      case _ => throw new RuntimeException("Will not create a function with no body.")
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
