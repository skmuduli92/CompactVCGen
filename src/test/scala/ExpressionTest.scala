import gcmdlang._
import org.scalatest.flatspec.AnyFlatSpec


class ExpressionTest extends AnyFlatSpec {

  "Add Expr" should "run successfully" in {
    
    val a = Lit(10)
    val b = Lit(20)
    val aPlusB = Add(a, b)
    assert(Interpreter.evalIntExpr(aPlusB) == 30)
  }
}
