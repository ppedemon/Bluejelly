package bluejelly.bjc.ast

trait Literal[A]
case class IntLiteral[A](val x:Int) extends Literal[A]
case class DoubleLiteral[A](val x:Double) extends Literal[A]
case class StringLiteral[A](val x:String) extends Literal[A]
case class CharLiteral[A](val x:Char) extends Literal[A]
case class BoolLiteral[A](val x:Boolean) extends Literal[A]
case class ListLiteral[A](val elems:Seq[A]) extends Literal[A]
case class TupleLiteral[A](val elems:Seq[A]) extends Literal[A]
