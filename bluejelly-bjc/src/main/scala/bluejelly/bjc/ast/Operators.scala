package bluejelly.bjc.ast

trait Assoc
case object Infixl extends Assoc
case object Infixr extends Assoc
case object Infix extends Assoc

case class Fixity(val assoc:Assoc, val precedence:Int)
