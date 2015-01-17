/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.common

import bluejelly.bjc.ast.decls.{Assoc,LeftAssoc,NoAssoc,RightAssoc}
import bluejelly.bjc.common.Binary._

import bluejelly.utils.Document.{group,text}

import java.io.{DataInputStream, DataOutputStream}

/**
 * Fixity type.
 * @author ppedemon
 */
class Fixity(val assoc:Assoc, val prec:Int) 
    extends PrettyPrintable with Serializable {
  
  def ppr =  group(text(assoc match {
    case NoAssoc => "infix" 
    case LeftAssoc => "infixl"
    case RightAssoc => "infixr"
  }) :/: text(prec.toString))
  
  def serialize(out:DataOutputStream) {
    Fixity.serializeAssoc(assoc, out)
    out.writeByte(prec)
  }
}

object Fixity extends Loadable[Fixity] {
  private[common] def serializeAssoc(a:Assoc, out:DataOutputStream) = a match {
    case NoAssoc => out.writeByte(0)
    case LeftAssoc => out.writeByte(1)
    case RightAssoc => out.writeByte(2)
  }

  private def loadAssoc(in:DataInputStream) = in.readByte match {
    case 0 => NoAssoc 
    case 1 => LeftAssoc 
    case 2 => RightAssoc
  }

  def load(in:DataInputStream) = 
    new Fixity(loadAssoc(in), in.readByte)
}
