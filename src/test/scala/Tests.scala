package dvh.cek

import org.scalatest.FunSuite

import Ops._

class TestSuite extends FunSuite {

  val var1: String = "x"
  val var2: String = "y"
  val var3: String = "z"
  val var4: String = "adamalix"
  val fun1: String = "(lambda x . x)"
  val fun2: String = "(λ x . (lambda y . x))"
  val fun3: String = "(lambda foo . (λ bar . bar))"
  val fun4: String = "(λ x . ((((λ p . (λ a . (λ x . ((p a) x)))) " +
                               "(isZero x)) " +
                              "(add1 x)) " +
                             "0))"
  val fun5: String = "(λx.((λp a x.(p a x)) (isZero x) (add1 x) 0))"
  val pri1: String = "0"
  val pri2: String = "42"
  val pri3: String = "-3"
  val app1: String = "("+fun1+" "+pri1+")"
  val app2: String = "("+var1+" "+pri2+")"
  val app3: String = "("+fun3+" "+pri3+")"
  val pop1: String = "(add1 "+pri1+")"
  val pop2: String = "(^ "+pri3+" "+pri1+")"
  val pop3: String = "(- "+pri2+" "+pri1+")"

  test("location equality") {
    assert(IntLocation(0) === IntLocation(0))
    assert(IntLocation(1) === IntLocation(1))
    assert(IntLocation(2) != IntLocation(3))
  }

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
    assert(parse(fun4) === parse(fun5))
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

  test("evaluating values") {
    assert(eval(parse(pri1)) === Con(0))
    val ex = intercept[RuntimeException] { eval(parse(var1)) }
    assert(ex.getMessage === "The variable 'x is not in the environment.")
    assert(eval(parse(fun3)) === Fun(Var('foo), Fun(Var('bar), Var('bar))))
    assert(eval(parse(fun4)) === parse(fun4))
  }

  test("evaluating expressions") {
    assert(eval(parse("("+fun1+" "+pri1+")")) === Con(0))
    assert(eval(parse("(isZero ((λ x . x) 0))")) === Fun(Var('x), Fun(Var('y), Var('x))))
    assert(eval(parse("(isZero (- 5 (+ 5 ((lambda x . x) 0))))")) === Fun(Var('x), Fun(Var('y), Var('x))))
    assert(eval(parse("(isZero (* 42 ((λ x . x) (- 6 5))))")) === Fun(Var('x), Fun(Var('y), Var('y))))
    assert(eval(parse("("+fun4+" 0)")) === Con(1))
    assert(eval(parse("("+fun4+" 1)")) === Con(0))
    assert(eval(parse("("+fun5+" 0)")) === Con(1))
    assert(eval(parse("("+fun5+" 1)")) === Con(0))
    assert(eval(parse("((λx.((λy.x) (set x (+ x 1)))) 12)")) === Con(13))
  }



}
