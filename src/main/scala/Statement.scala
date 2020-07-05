
package gcmdlang;

trait Node;
trait Stmt extends Node;

case class VarDeclStmt(varname : String) extends Stmt

case class AssignStmt(intVar: IntVar, right: Expr[Int]) extends Stmt {
  def varname: String = intVar.name
  override def toString: String = intVar.name + " := " + right.toString
}

case class IfStmt(cond : Expr[Boolean], ifTrue : List[Stmt], ifFalse : List[Stmt]) extends Stmt

case class WhileStmt(cond : Expr[Boolean], whileBody : List[Stmt]) extends Stmt

case class AssumeStmt(expr: Expr[Boolean]) extends Stmt {
  override def toString: String = "assume "+ expr.toString
}

case class AssertStmt(expr: Expr[Boolean]) extends Stmt

case class NonDet(first : List[Stmt], second : List[Stmt]) extends Stmt {
  override def toString: String = "{ " +
      first.foldLeft("")((a,b) => a + b.toString +"; " ) +
      " } [] { " +
      second.foldLeft("")((a,b) => a + b.toString +"; " ) +
      " }"
}
