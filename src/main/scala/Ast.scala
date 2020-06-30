
package gcmdlang;

trait Node;
trait Stmt extends Node;
trait Expr[T] extends Node;

case class IntVar(name : String) extends Expr[Int] {
  override def toString: String = "(" + name + ": int)"
}

case class Lit[T](value : T) extends Expr[T] {
  override def toString: String = value.toString
}

// -- Unary Operators --
case class Neg(expr: Expr[Int]) extends Expr[Int] {
  override def toString: String = "-" + expr.toString
}


// -- Binary Arithmetic Operators --

case class Add(left: Expr[Int], right: Expr[Int]) extends Expr[Int] {
  override def toString: String = "( " + left.toString + " + " + right.toString + " )"
}

case class Sub(left: Expr[Int], right: Expr[Int]) extends Expr[Int] {
  override def toString: String = "( " + left.toString + " - " + right.toString + " )"
}

case class Mul(left: Expr[Int], right: Expr[Int]) extends Expr[Int] {
  override def toString: String = "( " + left.toString + " * " + right.toString + " )"
}

case class Div(left: Expr[Int], right: Expr[Int]) extends Expr[Int] {
  override def toString: String = "( " + left.toString + " / " + right.toString + " )"
}

case class Assign(intVar: IntVar, right: Expr[Int]) extends Stmt {
  def varname: String = intVar.name
  override def toString: String = intVar.name + " := " + right.toString
}

// -- Binary Logic Operators --

// -- Relational Operators --




