import gcmdlang._

object Interpreter {

  // symbol table for Int type only now
  var SymbolTable: Map[String, Expr[Int]] = Map.empty

  // list of statements
  var program : List[Stmt] = List.empty

  def addStmt(stmt: Stmt) = {
    program = program ++ List(stmt)
  }

  // evaluate Boolean Expression
  def evalBoolExpr(expr: Expr[Boolean]) : Boolean = expr match {
    case Lesser(left, right) => evalIntExpr(left) < evalIntExpr(right)
    case Greater(left, right) => evalIntExpr(left) > evalIntExpr(right)
    case Equal (left, right) => evalIntExpr(left) == evalIntExpr(right)
    case GEq (left, right) => evalIntExpr(left) >= evalIntExpr(right)
    case LEq (left, right) => evalIntExpr(left) <= evalIntExpr(right)

    case And(left, right) => evalBoolExpr(left) && evalBoolExpr(right)
    case Or(left, right) => evalBoolExpr(left) || evalBoolExpr(right)
    case Not(expr) => !evalBoolExpr(expr)
  }

  // evaluate Integer expressions
  def evalIntExpr(expr: Expr[Int]): Int = expr match {
    case Lit(value) => value
    case IntVar(name) => evalIntExpr(SymbolTable(name))
    case Add(left, right) => evalIntExpr(left) + evalIntExpr(right)
    case Sub(left, right) => evalIntExpr(left) - evalIntExpr(right)
    case Mul(left, right) => evalIntExpr(left) * evalIntExpr(right)
    case Div(left, right) => evalIntExpr(left) / evalIntExpr(right)
  }

  // evaluate Boolean expressions

  def evalAssign(assign: Assign) = {
    SymbolTable = SymbolTable + (assign.varname -> Lit(evalIntExpr(assign.right)))
  }

  def evalIfStmt(ifStmt: IfStmt): Unit = {
    val condVal = evalBoolExpr(ifStmt.cond)
    if(condVal) {
      ifStmt.ifTrue.foreach(stmt => executeStmt(stmt))
    } else {
      ifStmt.ifElse.foreach(stmt => executeStmt(stmt))
    }
  }

  def executeStmt(stmt: Stmt): Unit = stmt match {
    case assign: Assign => evalAssign(assign)
    case ifStmt: IfStmt => evalIfStmt(ifStmt)
  }

  def execute: Unit = {
    program.foreach( stmt => stmt match {
      case assign: Assign => evalAssign(assign)
      case _ => Nil
    })
  }

  def valueOf(intVar: IntVar) = evalIntExpr(SymbolTable(intVar.name))

}
