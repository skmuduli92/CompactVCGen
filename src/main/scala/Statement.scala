
package gcmdlang;

trait Node;
trait Stmt extends Node;

case class AssignStmt(intVar: IntVar, right: Expr[Int]) extends Stmt {
  def varname: String = intVar.name
  override def toString: String = intVar.name + " := " + right.toString
}

case class IfStmt(cond : Expr[Boolean], ifTrue : List[Stmt], ifElse : List[Stmt]) extends Stmt

