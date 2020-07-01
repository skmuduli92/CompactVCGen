
import gcmdlang._


object Main {

  def main(args: Array[String]): Unit = {
    val a = Lit(10)
    val b = Lit(20);
    val c = Lit(2);
    val d = Lit(3);

    val rel = Equal(Add(a, b), Lit(30))
    println(rel.toString)

    val varX = IntVar("x")
    val thenBody = AssignStmt(varX, c);
    val elseBody = AssignStmt(varX, d);
    val ifStmt = IfStmt(Not(rel), List(thenBody), List(elseBody))

    Interpreter.executeStmt(ifStmt);
    println(varX.toString + " = " + Interpreter.valueOf(varX))

    val whileCond = Lesser(varX, b)
    val whileBody = AssignStmt(varX, Add(varX, Lit(1)))
    val whileStmt = WhileStmt(whileCond, List(whileBody))

    Interpreter.executeStmt(whileStmt)
    println(varX.toString + " = " + Interpreter.valueOf(varX))

  }
}
