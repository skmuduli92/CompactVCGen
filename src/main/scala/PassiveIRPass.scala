
import gcmdlang._

object PassiveIRPass {

  def versionVar(varname: String, version: Int): IntVar = IntVar(varname + version.toString)

  def updateIntExpr(expr: Expr[Int], versionMap : Map[String, Int]): Expr[Int] = expr match {
    case lit: Lit[Int] => lit
    case IntVar(name) => versionVar(name, versionMap(name))
    case Neg(expr) => Neg(updateIntExpr(expr, versionMap))
    case Mul(left, right) => Mul(updateIntExpr(left, versionMap), updateIntExpr(right, versionMap))
    case Sub(left, right) => Sub(updateIntExpr(left, versionMap), updateIntExpr(right, versionMap))
    case Add(left, right) => Add(updateIntExpr(left, versionMap), updateIntExpr(right, versionMap))
    case Div(left, right) => Div(updateIntExpr(left, versionMap), updateIntExpr(right, versionMap))
  }

  def updateBoolExpr(expr: Expr[Boolean], versionMap: Map[String, Int]) : Expr[Boolean] = expr match {
    case lit: Lit[Boolean] => lit
    case Not(left) => Not(updateBoolExpr(left, versionMap))
    case Or(left, right) => Or(updateBoolExpr(left, versionMap), updateBoolExpr(right, versionMap))
    case And(left, right) => And(updateBoolExpr(left, versionMap), updateBoolExpr(right, versionMap))
    case GEq(left, right) => GEq(updateIntExpr(left, versionMap), updateIntExpr(right, versionMap))
    case LEq(left, right) => LEq(updateIntExpr(left, versionMap), updateIntExpr(right, versionMap))
    case Equal(left, right) => Equal(updateIntExpr(left, versionMap), updateIntExpr(right, versionMap))
    case Lesser(left, right) => Lesser(updateIntExpr(left, versionMap), updateIntExpr(right, versionMap))
    case Greater(left, right) => Greater(updateIntExpr(left, versionMap), updateIntExpr(right, versionMap))
  }


  def passifyAssumeStmt(assumeStmt: AssumeStmt, versionMap: Map[String, Int]): (AssumeStmt, Map[String, Int]) =
    if(assumeStmt.expr == Lit(false)){
      println("False Expression : returning NULL map")
      (assumeStmt, Map.empty)
    } else {
      val newExpr = updateBoolExpr(assumeStmt.expr, versionMap)
      (AssumeStmt(newExpr), versionMap)
    }

  def passifyAssertStmt(assertStmt: AssertStmt, versionMap: Map[String, Int]): (AssertStmt, Map[String, Int]) =
    if(assertStmt.expr == Lit(false)){
      println("False Expression : returning NULL map")
      (assertStmt, Map.empty)
    } else {
      val newExpr = updateBoolExpr(assertStmt.expr, versionMap)
      (AssertStmt(newExpr), versionMap)
    }


  def passifyAssignStmt(assignStmt: AssignStmt, versionMap: Map[String, Int]): (AssumeStmt, Map[String, Int]) = {
    val newVersion = versionMap(assignStmt.varname) + 1
    val freshVar = versionVar(assignStmt.varname, newVersion)
    val newversionMap = versionMap + (assignStmt.varname -> newVersion)
    val newExpr = updateIntExpr(assignStmt.right, versionMap)
    (AssumeStmt(Equal(freshVar, newExpr)), newversionMap)
  }

  def passifyNonDetStmt(nonDet: NonDet, versionMap: Map[String, Int]) : (NonDet, Map[String, Int]) = {
    var (firstList, firstMap) = passifyStmtList(nonDet.first, versionMap)
    var (secondList, secondMap) = passifyStmtList(nonDet.second, firstMap)
    var freshMap = Map.empty[String, Int]

    // TODO : write tests for each of the below conditions
    if (firstMap.isEmpty) {
      freshMap = secondMap
    }
    else if (secondMap.isEmpty) {
      freshMap = firstMap
    }
    else {
      val delta = secondMap filter (entry => {
        entry._2 != firstMap(entry._1)
      })
      freshMap = secondMap
      delta.foreachEntry((k, v) => {
        firstList = firstList ++ List(AssumeStmt(Equal(versionVar(k, v + 1), versionVar(k, firstMap(k)))))
        secondList = secondList ++ List(AssumeStmt(Equal(versionVar(k, v + 1), versionVar(k, secondMap(k)))))
        freshMap = freshMap + (k -> (freshMap(k) + 1))
      })
    }

    (NonDet(firstList, secondList), freshMap)
  }

  def passifyStmt(stmt: Stmt, versionMap: Map[String, Int]): (Stmt, Map[String, Int]) = stmt match {
    case assertStmt: AssertStmt => passifyAssertStmt(assertStmt, versionMap)
    case assumeStmt: AssumeStmt => passifyAssumeStmt(assumeStmt, versionMap)
    case assignStmt: AssignStmt => passifyAssignStmt(assignStmt, versionMap)
    case nonDet: NonDet => passifyNonDetStmt(nonDet, versionMap)
  }

  def passifyStmtList(list: List[Stmt], versionMap: Map[String, Int]): (List[Stmt], Map[String, Int]) = list match {
    case ::(head, next) =>
      val (newHeadStmt, newMap) = passifyStmt(head, versionMap)
      val (newTail, tailVersionMap) = passifyStmtList(next, newMap)
      (List(newHeadStmt) ++ newTail, tailVersionMap)
    case Nil =>(List.empty[Stmt], versionMap)
  }

  def initVersionMap(prog: List[Stmt]): Map[String, Int] =
    prog match {
      case ::(head, next) => (head match {
        case AssignStmt(intVar, _) => Map(intVar.name -> 0)
        case VarDeclStmt(varname) => Map(varname -> 0)
        case IfStmt(_, ifTrue, ifFalse) => initVersionMap(ifTrue) ++ initVersionMap(ifFalse)
        case WhileStmt(_, whileBody) => initVersionMap(whileBody)
        case _ => initVersionMap(next)
      }) ++ initVersionMap(next)

      case Nil => Map.empty
    }
}
