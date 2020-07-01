import gcmdlang._
import org.scalatest.flatspec.AnyFlatSpec


class ExpressionTest extends AnyFlatSpec {

  "Add Expr" should "run successfully" in {

    val a = Lit(10)
    val b = Lit(20)
    val aPlusB = Add(a, b)
    assert(Interpreter.evalIntExpr(aPlusB) == 30)
  }

  "Sub Expr" should "run successfully" in {

    val a = Lit(10)
    val b = Lit(20)
    val bMinA = Sub(b, a)
    assert(Interpreter.evalIntExpr(bMinA) == 10)
  }

  "AddSub Expr" should "run successfully" in {
    val a = Lit(10)
    val b = Lit(20)
    val expr = Add(Sub(b, a), a)
    assert(Interpreter.evalIntExpr(expr) == b.value)
  }

  "Mul Expr" should "run successfully" in {

    val a = Lit(10)
    val b = Lit(20)
    val aMulB = Mul(a, b)
    assert(Interpreter.evalIntExpr(aMulB) == 200)
  }

  "Div Expr" should "run successfully" in {

    val a = Lit(10)
    val b = Lit(20)
    val bDivA = Div(b, a)
    assert(Interpreter.evalIntExpr(bDivA) == 2)
  }

  "MulDiv Expr" should "run successfully" in {
    val a = Lit(10)
    val b = Lit(20)
    val expr = Mul(Div(b, a), a)
    assert(Interpreter.evalIntExpr(expr) == b.value)
  }

}
