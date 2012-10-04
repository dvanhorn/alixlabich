package dvh.cek

import org.scalatest.FunSuite

import cek.{eval, parse}
import Ops._

class TestSuite extends FunSuite {

  val var1: String = "x"
  val var2: String = "y"
  val var3: String = "z"
  val var4: String = "adamalix"
  val fun1: String = "(lambda x . x)"
  val fun2: String = "(lambda x . (lambda y . x))"
  val fun3: String = "(lambda foo . (lambda bar . bar))"
  val pri1: String = "0"
  val pri2: String = "42"
  val pri3: String = "-3"
  val app1: String = "("+fun1+" "+pri1+")"
  val app2: String = "("+var1+" "+pri2+")"
  val app3: String = "("+fun3+" "+pri3+")"
  val pop1: String = "(add1 "+pri1+")"
  val pop2: String = "(^ "+pri3+" "+pri1+")"
  val pop3: String = "(- "+pri2+" "+pri1+")"

  test("parsing variables") {
    assert(parse(var1) === Var('x))
    assert(parse(var2) === Var('y))
    assert(parse(var3) === Var('z))
    assert(parse(var4) === Var('adamalix))
  }


  test("parsing functions") {
    assert(parse(fun1) === Fun(Var('x), Var('x)))
    assert(parse(fun2) === Fun(Var('x), Fun(Var('y), Var('x))))
    assert(parse(fun3) === Fun(Var('foo), Fun(Var('bar), Var('bar))))
  }


  test("parsing primitives") {
    assert(parse(pri1) === Con(0))
    assert(parse(pri2) === Con(42))
    assert(parse(pri3) === Con(-3))
  }


  test("parsing applications") {
    assert(parse(app1) === App(parse(fun1), parse(pri1)))
    assert(parse(app2) === App(parse(var1), parse(pri2)))
    assert(parse(app3) === App(parse(fun3), parse(pri3)))
  }


  test("parsing primitive operations") {
    assert(parse(pop1) === Oper(Add1, List(Con(0))))
    assert(parse(pop2) === Oper(Exp, List(Con(-3), Con(0))))
    assert(parse(pop3) === Oper(Sub, List(Con(42), Con(0))))
  }
/*
  test("evaluating values") {
    assert(eval(parse(pri1)) === Con(0))
    assert(eval(parse(var1)) === Var('x))
    assert(eval(parse(fun3)) === Fun(Var('foo), Fun(Var('bar), Var('bar))))
  }

  test("evaluating expressions") {
    assert(eval(parse("("+fun1+" "+pri1+")")) === Con(0))
  }
*/
}
