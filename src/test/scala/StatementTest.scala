import org.scalatest.flatspec.AnyFlatSpec
import gcmdlang._

class StatementTest extends AnyFlatSpec {

  "IfStmt Test" should "run successfully" in {

    /*
      if (10 + 20 != 30) {
        x := 2
      } else {
        x := 3
      }
   */

    val a = Lit(10)
    val b = Lit(20);
    val c = Lit(2);
    val d = Lit(3);
    val varX = IntVar("x")
    val thenBody = AssignStmt(varX, c);
    val elseBody = AssignStmt(varX, d);

    val rel = Equal(Add(a, b), Lit(30))
    val cond = Not(rel)
    val ifStmt = IfStmt(cond, List(thenBody), List(elseBody))


    Interpreter.executeStmt(ifStmt);
    assert(Interpreter.valueOf(varX) == 3)
  }

  "whileStmt Test" should "run successfully" in {

    /*
        x := 1;
        while(x < 20) {
          x := x + 1;
        }
   */

    val a = Lit(10)
    val varX = IntVar("x")

    val whileCond = Lesser(varX, a)
    val whileBody = AssignStmt(varX, Add(varX, Lit(1)))
    val whileStmt = WhileStmt(whileCond, List(whileBody))

    Interpreter.executeStmt(AssignStmt(varX, Lit(0)))
    Interpreter.executeStmt(whileStmt)
    assert(Interpreter.valueOf(varX) == a.value)

  }

  "InnerWhileLoop Test" should "run successfully" in {

    /*
        int x := 0;
        int y := 0;
        int a := 0;

        while(x < 10) {
          y := 0;
          while(y < 10) {
            a := a + 1; // will be executed 100 times
            y := y + 1;
          }
          x := x + 1
        }

     */

    val lit10 = Lit(10)
    val varX = IntVar("x")
    val varY = IntVar("y")
    val varA = IntVar("a")

    val outerLoopCond = Lesser(varX, lit10)
    val innerLoopCond = Lesser(varY, lit10)

    Interpreter.addStmt(AssignStmt(varX, Lit(0)))
    Interpreter.addStmt(AssignStmt(varA, Lit(0)))

    val innerLoopBody = List(
      AssignStmt(varA, Add(varA, Lit(1))),
      AssignStmt(varY, Add(varY, Lit(1)))
    )

    val outerLoopBody = List(
      AssignStmt(varY, Lit(0)),
      WhileStmt(innerLoopCond, innerLoopBody),
      AssignStmt(varX, Add(varX, Lit(1)))
    )

    val outerLoop = WhileStmt(outerLoopCond, outerLoopBody)
    Interpreter.addStmt(outerLoop)

    Interpreter.execute
    assert(Interpreter.valueOf(varA) == 100)
  }

}
