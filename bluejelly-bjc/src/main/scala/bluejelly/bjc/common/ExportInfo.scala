/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.common

import bluejelly.bjc.common.Binary._
import bluejelly.bjc.common.PprUtils._
import bluejelly.utils.Document.{group,empty}

import java.io.{DataInputStream,DataOutputStream}

/**
 * Things exported by an interface file. Important invariants:
 *  
 * 1. Any id having a parent (a class operation or record selector) is
 * *not* exported as an ExportedId(id), but as an ExportedTc(parent,[id]).
 *
 * 2.Invariant (1) poses (for example) the following question: how do we 
 * know if class C is exported in ExportedTc(C,[op])? Answer is invariant
 * (2): an exported parent is *always* the first element of the list in
 * ExportedTc.
 *
 * Example: say we have `class C a where { op :: a -> a}'. Then:
 *
 *  - exporting C() or C is: ExportedTc(C,[C])
 *  - exporting op alone is: ExportedTc(C,[op])
 *  - exporting C(op) or C(..) is: ExportedTc(C,[C,op]) 
 *
 * These three invariants significantly simplify computing visible
 * stuff when chasing imports.
 *
 * @author ppedemon
 */
abstract class ExportInfo(val name:QualName) 
  extends PrettyPrintable with Serializable 

case class ExportedId(override val name:QualName) extends ExportInfo(name) { 
  def ppr = name.pprAsId
  def serialize(out:DataOutputStream) {
    out.writeByte(0)
    name.serialize(out)
  }
}

case class ExportedTc(
    override val name:QualName, 
    val children:List[QualName]) extends ExportInfo(name) {

  def exportsParent = children.head == name

  def ppr = { 
    val d = children match {
      case Nil => 
        empty
      case _ if exportsParent => 
        if (children.tail.isEmpty) empty else 
          between("{",pprMany(children.tail.map(asId(_)),","),"}")
      case _ =>
        between("|{",pprMany(children.map(asId(_)),","),"}") 
    }
    group(name.pprAsId :: d)
  }
  
  def serialize(out:DataOutputStream) {
    out.writeByte(1)
    name.serialize(out)
    children.serialize(out)
  }
}

object ExportInfo extends Loadable[ExportInfo] {
  def load(in:DataInputStream) = in.readByte match {
    case 0 => new ExportedId(Name.loadQual(in))
    case 1 => new ExportedTc(Name.loadQual(in), Binary.loadList(Name.loadQual, in))
  }
}
