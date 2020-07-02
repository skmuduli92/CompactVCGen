
import gcmdlang._


object Main {

  def main(args: Array[String]): Unit = {
    val a = Lit(10)
    val b = Lit(20);
    val c = Lit(2);
    val d = Lit(3);

    /*
      if (10 + 20 != 30) {
        x := 2
      } else {
        x := 3
      }
     */

    val varX = IntVar("x")
    val rel = Equal(Add(a, b), Lit(30))
    val thenBody = AssignStmt(varX, c);
    val elseBody = AssignStmt(varX, d);
    val ifStmt = IfStmt(Not(rel), List(thenBody), List(elseBody))

    Interpreter.executeStmt(ifStmt);
    println(varX.toString + " = " + Interpreter.valueOf(varX))

    /*
        x := 1;
        while(x < 20) {
          x := x + 1;
        }
     */

    val whileCond = Lesser(varX, a)
    val whileBody = AssignStmt(varX, Add(varX, Lit(1)))
    val whileStmt = WhileStmt(whileCond, List(whileBody))

    Interpreter.executeStmt(AssignStmt(varX, Lit(0)))
    Interpreter.executeStmt(whileStmt)
    println(varX.toString + " = " + Interpreter.valueOf(varX))

  }
}
