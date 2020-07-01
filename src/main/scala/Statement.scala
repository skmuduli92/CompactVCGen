
package gcmdlang;

case class IfStmt(cond : Expr[Boolean], ifTrue : List[Stmt], ifElse : List[Stmt]) extends Stmt

