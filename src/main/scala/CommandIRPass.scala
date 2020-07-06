
import gcmdlang._

object CommandIRPass {

  def transform(prog : List[Stmt]): List[Stmt] = {
    prog.map {
      case IfStmt(cond, ifTrue, ifFalse) => NonDet(
        List(AssumeStmt(cond)) ++ transform(ifTrue),
        List(AssumeStmt(Not(cond))) ++ transform(ifFalse)
      )

      case assertStmt: AssertStmt => assertStmt
      case whileStmt: WhileStmt => whileStmt
      case assignStmt: AssignStmt => assignStmt
      case nonDet: NonDet => nonDet
    }
  }
}
