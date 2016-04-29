package bluejelly.bjc.ast

import bluejelly.bjc.{Ident,Qualified,ProperName,ConstructorName}
import bluejelly.bjc.Type

trait Binder
case object NullBinder extends Binder
case class LiteralBinder(val lit:Literal[Binder]) extends Binder
case class VarBinder(val ident:Ident) extends Binder
case class ConstructorBinder(val ctor:Qualified[ProperName[ConstructorName.type]], val args:Seq[Binder]) extends Binder
case class OpBinder(val ident:Qualified[Ident]) extends Binder
case class BinaryNoParensBinder(val left:Binder, val op:Binder, val right:Binder) extends Binder
case class ParensInBinder(val binder:Binder) extends Binder
case class NamedBinder(val ident:Ident, val binder:Binder) extends Binder
case class PositionedBinder(val posn:SourceSpan, val binder:Binder) extends Binder
case class TypedBinder(val ty:Type, val binder:Binder) extends Binder

object Binder {
  def binderNames(binder:Binder):Seq[Ident] = {
    def goLit:Seq[Ident] => Literal[Binder] => Seq[Ident] = ns => {
      case ListLiteral(binders) => binders.foldLeft(ns)(Function.uncurried(go))
      case TupleLiteral(binders) => binders.foldLeft(ns)(Function.uncurried(go))
      case _ => ns 
    }
    def go:Seq[Ident] => Binder => Seq[Ident] = ns => {
      case LiteralBinder(lit) => goLit(ns)(lit)
      case VarBinder(ident) => ident +: ns
      case ConstructorBinder(_,binders) => binders.foldLeft(ns)(Function.uncurried(go))
      case ParensInBinder(binder) => go(ns)(binder)
      case NamedBinder(ident,binder) => go(ident +: ns)(binder)
      case PositionedBinder(_,binder) => go(ns)(binder)
      case TypedBinder(_,binder) => go(ns)(binder)
      case _ => ns
    }
    go(Seq())(binder)
  }
}