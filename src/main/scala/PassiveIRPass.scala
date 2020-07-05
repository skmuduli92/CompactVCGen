
import gcmdlang._

object PassiveIRPass {

  def versionVar(varname: String, version: Int) = IntVar(varname + version.toString)

  def passifyAssumeStmt(assumeStmt: AssumeStmt, varmap: Map[String, Int]) =
    throw new NotImplementedError("Error")

  def passifyAssertStmt(assertStmt: AssertStmt, varmap: Map[String, Int]) =
    throw new NotImplementedError("Error")

  def passifyAssignStmt(assignStmt: AssignStmt, varmap: Map[String, Int]) =
    throw new NotImplementedError("Error")

  def passifyNonDetStmt(nonDet: NonDet, varmap: Map[String, Int]) =
    throw new NotImplementedError("Error")



  def initVarMap(prog: List[Stmt]): Map[String, Int] =
    prog match {
      case ::(head, next) => (head match {
        case AssignStmt(intVar, right) => Map(intVar.name -> 0)
        case VarDeclStmt(varname) => Map(varname -> 0)
        case IfStmt(cond, ifTrue, ifFalse) => initVarMap(ifTrue) ++ initVarMap(ifFalse)
        case WhileStmt(cond, whileBody) => initVarMap(whileBody)
        case _ => initVarMap(next)
      }) ++ initVarMap(next)

      case Nil => Map.empty
    }
}
