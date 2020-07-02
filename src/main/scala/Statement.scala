
package gcmdlang;

trait Node;
trait Stmt extends Node;

case class VarDeclStmt(varname : String) extends Stmt

case class AssignStmt(intVar: IntVar, right: Expr[Int]) extends Stmt {
  def varname: String = intVar.name
  override def toString: String = intVar.name + " := " + right.toString
}

case class IfStmt(cond : Expr[Boolean], ifTrue : List[Stmt], ifElse : List[Stmt]) extends Stmt

case class WhileStmt(cond : Expr[Boolean], whileBody : List[Stmt]) extends Stmt

case class AssumeStmt(expr: Expr[Boolean]) extends Stmt

case class AssertStmt(expr: Expr[Boolean]) extends Stmt