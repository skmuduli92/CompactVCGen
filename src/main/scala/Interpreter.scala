import gcmdlang._

object Interpreter {

  // symbol table for Int type only now
  var SymbolTable: Map[String, Expr[Int]] = Map.empty

  // list of statements
  var program : List[Stmt] = List.empty

  def addStmt(stmt: Stmt) = {
    program = program ++ List(stmt)
  }

  // evaluate Integer expressions
  def evalExpr(expr: Expr[Int]): Int = expr match {
    case Lit(value) => value
    case IntVar(name) => evalExpr(SymbolTable(name))
    case Add(left, right) => evalExpr(left) + evalExpr(right)
    case Sub(left, right) => evalExpr(left) - evalExpr(right)
    case Mul(left, right) => evalExpr(left) * evalExpr(right)
    case Div(left, right) => evalExpr(left) / evalExpr(right)
  }

  // evaluate Boolean expressions

  def evalAssign(assign: Assign) = {
    SymbolTable = SymbolTable + (assign.varname -> Lit(evalExpr(assign.right)))
  }

  def execute = {
    program.foreach( stmt => stmt match {
      case assign: Assign => evalAssign(assign)
      case _ => Nil
    })
  }

  def valueOf(intVar: IntVar) = evalExpr(SymbolTable(intVar.name))

}
