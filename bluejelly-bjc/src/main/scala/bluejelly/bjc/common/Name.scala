/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.common

import bluejelly.bjc.common.PprUtils.symbolToPrettyPrintableSymbol
import bluejelly.utils.Document.{group,text}

import java.io.DataOutputStream
import java.io.DataInputStream

/**
 * Abstract class for possibly qualified names.
 * @author ppedemon
 */
abstract class Name(val name:ScopedName) 
    extends PrettyPrintableVar with Serializable {
  def isId = name.isId
  def isQual:Boolean
  def qualify(qual:Symbol):QualName
  def unqualify:LocalName
  def toSymbol = name.toSymbol
}

/**
 * A local name. Essentially, a scoped name with no qualified.
 * @author ppedemon
 */
case class LocalName(override val name:ScopedName) extends Name(name) {
  def ppr = name.ppr

  def serialize(out:DataOutputStream) {
    out.writeByte(0)
    name.serialize(out)
  }

  def isQual = false
  def qualify(qual:Symbol) = QualName(qual, name)
  def unqualify = this

  override def equals(o:Any) = o match {
    case lcl:LocalName => lcl.name == name
    case _ => false
  }

  override def hashCode = name.hashCode
}

/**
 * A qualified name. A scoped name plus a module qualifier.
 */
case class QualName(
    val qual:Symbol, 
    override val name:ScopedName) extends Name(name) {

  def ppr = group(qual.ppr :: "." :: name.ppr)

  def serialize(out:DataOutputStream) {
    out.writeByte(1)
    out.writeUTF(qual.name)
    name.serialize(out)
  }

  def isQual = true
  def qualify(qual:Symbol) = 
    if (qual == this.qual) this else 
      sys.error("Invalid qualifier for already qualified name: %s" format qual.name)
  def unqualify = LocalName(name)

  override def equals(o:Any) = o match {
    case q:QualName => q.qual == qual && q.name == name
    case _ => false
  }

  override def hashCode = 31*qual.hashCode + name.hashCode
}

object Name extends Loadable[Name] {
  def localName(n:Symbol, scope:Scope) = LocalName(ScopedName(n,scope))
  def qualName(q:Symbol, n:Symbol, scope:Scope) = QualName(q, ScopedName(n,scope))
  
  def localName(n:ScopedName) = LocalName(n)
  def qualName(q:Symbol, n:ScopedName) = QualName(q,n)

  def idName(name:Symbol) = localName(name, IdScope)
  def idName(qual:Symbol, name:Symbol) = qualName(qual, name, IdScope)
  def tcName(name:Symbol) = localName(name, TcScope)
  def tcName(qual:Symbol, name:Symbol) = qualName(qual, name, TcScope)
  def tvName(name:Symbol) = localName(name, TvScope)
  def tvName(qual:Symbol, name:Symbol) = qualName(qual, name, TvScope)

  def load(in:DataInputStream) = in.readByte() match {
    case 0 => new LocalName(ScopedName.load(in))
    case 1 => new QualName(Symbol(in.readUTF()), ScopedName.load(in))
  }

  def loadQual(in:DataInputStream) = {
    in.readByte();
    new QualName(Symbol(in.readUTF()), ScopedName.load(in))
  } 
}
