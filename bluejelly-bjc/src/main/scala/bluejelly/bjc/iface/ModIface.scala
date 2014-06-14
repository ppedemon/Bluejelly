/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.iface

import java.io.{DataInputStream,DataOutputStream}

import scala.text.Document.{empty,group,text}

import bluejelly.bjc.common.Name.asOp
import bluejelly.bjc.common.Name
import bluejelly.bjc.common.Binary._
import bluejelly.bjc.common.{Loadable,Serializable,Binary}
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.ast.decls.{Assoc,NoAssoc,LeftAssoc,RightAssoc}

/**
 * Things exported by an interface file.
 * @author ppedemon
 */
abstract class IfaceExport(val name:Name) 
  extends PrettyPrintable with Serializable 

case class ExportedId(override val name:Name) extends IfaceExport(name) { 
  def ppr = name.ppr
  def serialize(out:DataOutputStream) {
    out.writeByte(0)
    name.serialize(out)
  }
}

case class ExportedTc(
    override val name:Name, 
    val children:List[Name]) extends IfaceExport(name) {
  def ppr = group(name.ppr :: between("{",pprMany(children,","),"}"))
  def serialize(out:DataOutputStream) {
    out.writeByte(1)
    name.serialize(out)
    children.serialize(out)
  }
}

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
    ModIface.serializeAssoc(assoc, out)
    out.writeByte(prec)
  }
}

/**
 * A module interface.
 * @author ppedemon
 */
class ModIface(
    val name:Name,
    val deps:List[Name],
    val exports:List[IfaceExport],
    val fixities:List[(Name,Fixity)],
    val decls:List[IfaceDecl],
    val insts:List[IfaceClsInst]) extends PrettyPrintable with Serializable {
  def ppr = {
    val fds = fixities map Function.tupled((n,f) => new PrettyPrintable {
      def ppr = group(f.ppr :/: asOp(n).ppr)
    })
    val fd = if (fixities.isEmpty) empty else 
      nl::gnest("Fixities:" :/: pprMany(fds, ","))
    val ed = if (exports.isEmpty) empty else 
      nl::gnest("Exports:" :/: pprMany(exports))
    val ds = if (deps.isEmpty) empty else
      nl::gnest("Dependencies:" :/: pprMany(deps, ","))
    cat(List(
      group("module" :/: name.ppr :/: text("where")), 
      ds, ed, fd,       
      if (decls.isEmpty) empty else nl :: vppr(decls),
      if (insts.isEmpty) empty else nl :: vppr(insts)))
  }
  
  def serialize(out:DataOutputStream) {
    name.serialize(out)
    deps.serialize(out)
    exports.serialize(out)
    fixities.serialize(out)
    decls.serialize(out)
    insts.serialize(out)
  }
}

object ModIface extends Loadable[ModIface]{
  
  def load(in:DataInputStream) = 
    new ModIface(
        Name.load(in),
        Binary.loadList(Name.load, in),
        Binary.loadList(loadExport, in),
        Binary.loadList(Binary.loadTuple(Name.load, loadFixity), in),
        Binary.loadList(IfaceDecl.load, in),
        Binary.loadList(IfaceDecl.loadIfaceClsInst, in))
  
  private[iface] def serializeAssoc(a:Assoc, out:DataOutputStream) = a match {
    case NoAssoc => out.writeByte(0)
    case LeftAssoc => out.writeByte(1)
    case RightAssoc => out.writeByte(2)
  }
  
  private def loadAssoc(in:DataInputStream) = in.readByte match {
    case 0 => NoAssoc 
    case 1 => LeftAssoc 
    case 2 => RightAssoc
  }
  
  private def loadExport(in:DataInputStream) = in.readByte match {
    case 0 => new ExportedId(Name.load(in))
    case 1 => new ExportedTc(Name.load(in), Binary.loadList(Name.load, in))
  }
  
  private def loadFixity(in:DataInputStream) = 
    new Fixity(loadAssoc(in), in.readByte)
  
}
