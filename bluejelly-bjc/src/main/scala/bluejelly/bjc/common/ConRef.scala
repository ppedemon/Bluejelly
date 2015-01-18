/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.common

import bluejelly.utils.Document.text

import java.io.{DataInputStream,DataOutputStream}

/**
 * A reference to a built-in or custom type constructor.
 * Reference to custom type constructors sre using a
 * fully qualified name.
 *
 * @author ppedemon
 */
trait TcRef extends PrettyPrintable with Serializable

case class ConRef(val n:QualName) extends TcRef {
  def ppr = n.ppr
  def serialize(out:DataOutputStream) { out.writeByte(0); n.serialize(out) }
}
case object ListRef extends TcRef {
  def ppr = text("[]")
  def serialize(out:DataOutputStream) { out.writeByte(1) }
}
case object UnitRef extends TcRef {
  def ppr = text("()")
  def serialize(out:DataOutputStream) { out.writeByte(2) }
}
case object ArrowRef extends TcRef {
  def ppr = text("->")
  def serialize(out:DataOutputStream) { out.writeByte(3) }
}
case class TupleRef(val arity:Int) extends TcRef {
  def ppr = text("(%s)" format (","*(arity-1)))
  def serialize(out:DataOutputStream) { out.writeByte(4); out.writeInt(arity) }
}

object TcRef extends Loadable[TcRef] {
  def load(in:DataInputStream) = in.readByte() match {
    case 0 => ConRef(Name.loadQual(in))
    case 1 => ListRef
    case 2 => UnitRef
    case 3 => ArrowRef
    case 4 => TupleRef(in.readInt())
  }
}
