/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.common

import bluejelly.bjc.common.PprUtils.symbolToPrettyPrintableSymbol

import bluejelly.utils.Document
import bluejelly.utils.Document.{group,text}

import java.io.{DataInputStream,DataOutputStream}

/**
 * The three scoped a name can belong to.
 * @author ppedemon
 */
trait Scope
case object TcScope extends Scope
case object IdScope extends Scope
case object TvScope extends Scope

/**
 * Scoped name class: a symbol plus a name.
 * @author ppedemon
 */
class ScopedName(val name:Symbol, val scope:Scope) 
    extends PrettyPrintableVar with Serializable {

  def ppr = name.ppr
  /*
  def ppr = group(name.ppr :: text("[%s]" format (scope match {
    case TcScope => "T"
    case IdScope => "I"
    case TvScope => "V"
  })))
  */
  def isId = name.isId
  def toSymbol = name

  def serialize(out:DataOutputStream) {
    out.writeUTF(name.name)
    scope match {
      case TcScope => out.writeByte(0)
      case IdScope => out.writeByte(1)
      case TvScope => out.writeByte(2)
    }
  }

  override def equals(o:Any) = o match {
    case sn:ScopedName => sn.name == name && sn.scope == scope
    case _ => false
  }

  override def hashCode = 31*name.hashCode + scope.hashCode
}

object ScopedName extends Loadable[ScopedName] {
  def apply(name:Symbol, scope:Scope) = new ScopedName(name, scope)
  def apply(name:String, scope:Scope) = new ScopedName(Symbol(name), scope)

  def idName(name:Symbol) = new ScopedName(name, IdScope)
  def tcName(name:Symbol) = new ScopedName(name, TcScope)
  def tvName(name:Symbol) = new ScopedName(name, TvScope)

  def load(in:DataInputStream) = {
    val name = in.readUTF()
    val scope = in.readByte() match {
      case 0 => TcScope
      case 1 => IdScope
      case 2 => TvScope
    }
    ScopedName(name, scope)
  }
}
