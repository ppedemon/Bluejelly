/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.utils

/**
 * A possibly qualified name.
 * @author ppedemon
 */
class Name(val qual:Option[Symbol], val name:Symbol) {
  def isQual = !(qual.isEmpty)

  def qualEquals(s:Symbol) = isQual && qual.get == s
  
  override def toString = {
    if (!isQual) name.toString substring 1
    else "%s.%s" format 
      (qual.get.toString substring 1, name.toString substring 1)
  }
  
  override def equals(other:Any) = other match {
    case n:Name => n.qual == qual && n.name == name
    case _ => false
  }
  
  override def hashCode() = 31*((qual getOrElse 0).hashCode) + (name.hashCode)
}

object Name {
  def apply(qual:String, name:String):Name = apply(Symbol(qual), Symbol(name))
  def apply(qual:Symbol, name:Symbol):Name = new Name(Some(qual), name)
  def apply(name:String):Name = apply(Symbol(name))
  def apply(name:Symbol):Name = new Name(None, name)
}
