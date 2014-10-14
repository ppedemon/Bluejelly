/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.common

import java.io.DataOutputStream
import java.io.DataInputStream
import java.io.DataInput

import bluejelly.utils.Document.text
import bluejelly.utils.{Name => N}

/**
 * A bluejelly.utils name incorporating serialization and pretty printing
 * capabilities.
 * @author ppedemon
 */
class Name(qual:Option[Symbol], name:Symbol) 
    extends N(qual,name) with PrettyPrintable with Serializable {
  
  def qualified = isQual
  def isId = name.toString.drop(1).matches("""^[_\p{Ll}\p{Lu}\p{Lt}].*""")
  def isOp = !isId

  def qualify(q:Symbol):Name = qual match {
    case None => Name(q,name)
    case Some(qual) if qual == q => this
    case _ => sys.error(s"Incompatible qualifier (current: $qual, new: $q)")
  }

  def qualify(q:Name):Name = qualify(q.name)

  def unqualify = qual match {
    case None => this
    case _ => Name(name)
  }

  def ppr = if (qualified)
    text("%s.%s" format (qual.get.toString.drop(1),name.toString.drop(1))) else
    text(name.toString.drop(1))
  
  def serialize(out:DataOutputStream) = qual match {
    case None => 
      out.writeByte(0)
      out.writeUTF(name.toString.drop(1))
    case _ =>
      out.writeByte(1)
      out.writeUTF(qual.get.toString.drop(1))
      out.writeUTF(name.toString.drop(1))
  }
}

/**
 * Factory object for names.
 * @author ppedemon
 */
object Name extends Loadable[Name] {
  def apply(name:Symbol) = new Name(None, name)
  def apply(modId:Symbol, name:Symbol) = new Name(Some(modId), name)
    
  def asId(n:Name) = 
    if (n.isId) n else new PrettyPrintable { def ppr = text("(%s)" format n) }

  def asOp(n:Name) = 
    if (n.isOp) n else new PrettyPrintable { def ppr = text("`%s`" format n) }
  
  def load(in:DataInputStream) = in.readByte match {
    case 0 => Name(Symbol(in.readUTF()))
    case 1 => Name(Symbol(in.readUTF()), Symbol(in.readUTF()))
  }
}
