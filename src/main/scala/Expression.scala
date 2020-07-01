
package gcmdlang;


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

// -- Binary Logic Operators --

case class And(left : Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean] {
  override def toString: String = "( " + left.toString + " && " + right.toString + " )"
}

case class Or(left : Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean] {
  override def toString: String = "( " + left.toString + " || " + right.toString + " )"
}

case class Not(left : Expr[Boolean]) extends Expr[Boolean] {
  override def toString: String = "( ~" + left.toString + " )"
}

// -- Relational Operators --

case class Equal(left: Expr[Int], right: Expr[Int]) extends Expr[Boolean] {
  override def toString: String = "( " + left.toString + " == " + right.toString + " )"
}

case class Lesser(left: Expr[Int], right: Expr[Int]) extends Expr[Boolean] {
  override def toString: String = "( " + left.toString + " < " + right.toString + " )"
}

case class Greater(left: Expr[Int], right: Expr[Int]) extends Expr[Boolean] {
  override def toString: String = "( " + left.toString + " > " + right.toString + " )"
}

case class GEq(left: Expr[Int], right: Expr[Int]) extends Expr[Boolean] {
  override def toString: String = "( " + left.toString + " >= " + right.toString + " )"
}

case class LEq(left: Expr[Int], right: Expr[Int]) extends Expr[Boolean] {
  override def toString: String = "( " + left.toString + " <= " + right.toString + " )"
}



