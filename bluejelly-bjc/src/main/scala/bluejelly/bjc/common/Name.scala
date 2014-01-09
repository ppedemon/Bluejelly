/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.common

import scala.text.Document.text
import java.io.DataOutputStream
import java.io.DataInputStream
import java.io.DataInput

/**
 * A <code>Name</code> is the common class for simple and qualified names.
 * @author ppedemon
 */
sealed abstract class Name extends PrettyPrintable with Serializable {
  def name:Symbol
  def qualified:Boolean
  def qualifier:Option[Symbol]
  def isId = name.toString.substring(1).matches("""^[_\p{Ll}\p{Lu}\p{Lt}].*""")
  def isOp = !isId
  def ppr = text(toString)
}

/**
 * A <code>Name</code> can refer to a un-qualified name.
 * @author ppedemon
 */
case class Unqual(val name:Symbol) extends Name {
  override def qualified = false
  override def qualifier = None
  override def toString = name.toString.drop(1)
  override def hashCode = name.hashCode
  override def equals(other:Any) = other match {
    case x:Unqual => x.name == name 
    case _ => false
  }
  def serialize(out:DataOutputStream) {
    out.writeByte(0)
    out.writeUTF(toString)
  }
}

/**
 * Alternatively, a <code>Name</code> may refer to a qualified name.
 * @author ppedemon
 */
case class Qual(val modId:Symbol, val name:Symbol) extends Name {
  override def qualified = true
  override def qualifier = Some(modId)
  override def toString = "%s.%s" format (modId.toString.drop(1), name.toString.drop(1))
  override def hashCode = modId.hashCode*17 + name.hashCode
  override def equals(other:Any) = other match {
    case x:Qual => x.modId == modId && x.name == name
    case _ => false
  }
  def serialize(out:DataOutputStream) {
    out.writeByte(1)
    out.writeUTF(modId.toString.drop(1))
    out.writeUTF(name.toString.drop(1))
  }
}

/**
 * Factory object for names.
 * @author ppedemon
 */
object Name extends Loadable[Name] {
  def apply(name:Symbol) = Unqual(name)
  def apply(modId:Symbol, name:Symbol) = Qual(modId, name)
    
  def asId(n:Name) = 
    if (n.isId) n else new PrettyPrintable { def ppr = text("(%s)" format n) }

  def asOp(n:Name) = 
    if (n.isOp) n else new PrettyPrintable { def ppr = text("`%s`" format n) }
  
  def load(in:DataInputStream) = in.readByte match {
    case 0 => Name(Symbol(in.readUTF()))
    case 1 => Name(Symbol(in.readUTF()), Symbol(in.readUTF()))
  }
}
