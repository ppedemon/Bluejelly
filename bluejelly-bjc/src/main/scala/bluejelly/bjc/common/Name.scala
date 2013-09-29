/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.common

import scala.text.Document.text

/**
 * Names can be identifiers or operators.
 * @author ppedemon
 */
sealed trait NameClass
case object Id extends NameClass
case object Op extends NameClass

/**
 * A <code>Name</code> is the common class for simple and qualified names.
 * @author ppedemon
 */
sealed abstract class Name(val nc:NameClass) extends PrettyPrintable {
  def name:Symbol
  def qualified:Boolean
  def qualifier:Option[Symbol]
  def isId = nc match {case Id => true; case _ => false}
  def isOp = nc match {case Op => true; case _ => false}
  def ppr = text(toString)
}

/**
 * A <code>Name</code> can refer to a un-qualified name.
 * @author ppedemon
 */
case class Unqual(val name:Symbol,override val nc:NameClass) extends Name(nc) {
  override def qualified = false
  override def qualifier = None
  override def toString = name.toString.drop(1)
  override def hashCode = nc.hashCode*17 + name.hashCode
  override def equals(other:Any) = other match {
    case x:Unqual => x.nc == nc && x.name == name 
    case _ => false
  }
}

/**
 * Alternatively, a <code>Name</code> may refer to a qualified name.
 * @author ppedemon
 */
case class Qual(
    val modId:Symbol, 
    val name:Symbol, 
    override val nc:NameClass) extends Name(nc) {
  override def qualified = true
  override def qualifier = Some(modId)
  override def toString = "%s.%s" format (modId.toString.drop(1), name.toString.drop(1))
  override def hashCode = (nc.hashCode*17 + modId.hashCode)*17 + name.hashCode
  override def equals(other:Any) = other match {
    case x:Qual => x.nc == nc && x.modId == modId && x.name == name
    case _ => false
  }
}

/**
 * Factory object for names.
 * @author ppedemon
 */
object Name {
  def unqualOp(name:Symbol) = Unqual(name,Op)
  def unqualId(name:Symbol) = Unqual(name,Id)
  def qualOp(modId:Symbol, name:Symbol) = Qual(modId, name, Op)
  def qualId(modId:Symbol, name:Symbol) = Qual(modId, name, Id)
  
  def asId(n:Name) = n.nc match {
    case Id => n
    case Op => new PrettyPrintable { def ppr = text("(%s)" format n) }
  } 

  def asOp(n:Name) = n.nc match {
    case Op => n
    case Id => new PrettyPrintable { def ppr = text("`%s`" format n) }
  } 
}
