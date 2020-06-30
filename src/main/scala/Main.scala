
import gcmdlang._


object Main {

  def main(args: Array[String]): Unit = {
    val a = Lit(10)
    val b = Lit(20);
    val c = Lit(2);
    val d = Lit(3);

    val ivar = IntVar("x")
    val assgnStmt = Assign(ivar, Lit(20))
    Interpreter.addStmt(assgnStmt)

    val ex = Div(Mul(Add(a, b), c), ivar);
    val assgnStmt2 = Assign(ivar, ex)
    Interpreter.addStmt(assgnStmt2)

    println(assgnStmt.toString)
    println(assgnStmt2.toString)

    Interpreter.evalAssign(assgnStmt)
    Interpreter.evalAssign(assgnStmt2)

    println("x = " + Interpreter.valueOf(ivar))
  }
}
