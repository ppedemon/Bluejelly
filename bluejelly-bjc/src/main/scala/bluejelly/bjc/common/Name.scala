/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.common

import scala.text.Document.text

/**
 * The <code>SimpleName</code> class models a name found in source code.
 * @author ppedemon
 */
sealed abstract class SimpleName {
  def name:Symbol
  override def toString = name.toString.drop(1)
  override def hashCode = name.hashCode
  override def equals(other:Any) = other match {
    case x:SimpleName => x.getClass() == getClass() && x.name == name
    case _ => false
  }
}

/**
 * A name can be an identifier, like <code>f</code>.
 * @author ppedemon
 */
case class Id(val name:Symbol) extends SimpleName

/**
 * A name can also be an operator, like <code>+</code>.
 */
case class Op(val name:Symbol) extends SimpleName

/**
 * Tuples are a special case, they have the form <code>(,...,)</code>.
 * The zero-ary <code>()</code> tuple also falls in this case.
 * @author ppedemon
 */
case class Tuple(arity:Int) extends SimpleName {
  def name = Symbol(toString)
  override def toString = "(%s)" format (","*arity)
  override def hashCode = arity
  override def equals(other:Any) = other match {
    case x:Tuple => arity == x.arity
    case _ => false
  }
}

/**
 * A <code>Name</code> is the common class for simple and qualified names.
 * @author ppedemon
 */
sealed abstract class Name(val name:SimpleName) extends PrettyPrintable {
  def isId = name match {case Id(_) => true; case _ => false}
  def isOp = name match {case Op(_) => true; case _ => false}
  def isTuple = name match {case Tuple(_) => true; case _ => false}
  def qualified:Boolean
  def qualifier:Option[Symbol]
  def ppr = text(toString)
}

/**
 * A <code>Name</code> can refer to a un-qualified name.
 * @author ppedemon
 */
case class Unqual(override val name:SimpleName) extends Name(name) {
  override def qualified = false
  override def qualifier = None
  override def toString = name.toString
  override def hashCode = name.hashCode
  override def equals(other:Any) = other match {
    case x:Unqual => x.name == name
    case _ => false
  }
}

/**
 * Alternatively, a <code>Name</code> may refer to a qualified name.
 * @author ppedemon
 */
case class Qual(
    val modId:Symbol, 
    override val name:SimpleName) extends Name(name) {
  override def qualified = true
  override def qualifier = Some(modId)
  override def toString = "%s.%s" format (modId.toString.drop(1), name)
  override def hashCode = modId.hashCode*17 + name.hashCode
  override def equals(other:Any) = other match {
    case x:Qual => x.modId == modId && x.name == name
    case _ => false
  }
}

/**
 * Factory object for names.
 * @author ppedemon
 */
object Name {
  def unqualOp(name:Symbol) = Unqual(Op(name))
  def unqualId(name:Symbol) = Unqual(Id(name))
  def unqualTuple(arity:Int) = Unqual(Tuple(arity))
  def qualOp(modId:Symbol, name:Symbol) = Qual(modId, Op(name))
  def qualId(modId:Symbol, name:Symbol) = Qual(modId, Id(name))
  def qualTuple(modId:Symbol, arity:Int) = Qual(modId, Tuple(arity))
  
  def asId(n:Name) = new PrettyPrintable {
    def ppr = n.name match {
      case Id(_)|Tuple(_) => n.ppr
      case Op(_) => text("(%s)" format n)
    }
  }

  def asOp(n:Name) = new PrettyPrintable {
    def ppr = n.name match {
      case Id(_) => text("`%s`" format n)
      case Op(_)|Tuple(_) => n.ppr
    }
  }

}
