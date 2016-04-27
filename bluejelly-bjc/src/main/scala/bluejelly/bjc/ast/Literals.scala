package bluejelly.bjc.ast

trait Literal[A]
case class IntLiteral[A](x:Int) extends Literal[A]
case class DoubleLiteral[A](x:Double) extends Literal[A]
case class StringLiteral[A](x:String) extends Literal[A]
case class CharLiteral[A](x:Char) extends Literal[A]
case class BoolLiteral[A](x:Boolean) extends Literal[A]
case class ListLiteral[A](elems:Seq[A]) extends Literal[A]
case class TupleLiteral[A](elems:Seq[A]) extends Literal[A]
