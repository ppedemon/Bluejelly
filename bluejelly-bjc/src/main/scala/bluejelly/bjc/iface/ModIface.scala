/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.iface

import java.io.{DataInputStream,DataOutputStream}

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.Binary._
import bluejelly.bjc.common.{Loadable,Serializable,Binary}
import bluejelly.bjc.common.{ExportInfo,ExportedId,ExportedTc}
import bluejelly.bjc.common.Fixity
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable

import bluejelly.bjc.ast.decls.{Assoc,NoAssoc,LeftAssoc,RightAssoc}

import bluejelly.utils.Document.{empty,group,text}

/**
 * A module interface.
 * @author ppedemon
 */
class ModIface(
    val name:Symbol,
    val deps:List[Symbol],
    val exports:List[ExportInfo],
    val fixities:List[(Symbol,Fixity)],
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
    val ds = if (deps.isEmpty) empty else {
      val ps = deps map {new PrettyPrintableSymbol(_)}
      nl::gnest("Dependencies:" :/: pprMany(ps, ","))
    }
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
        Binary.loadSymbol(in),
        Binary.loadList(Binary.loadSymbol, in),
        Binary.loadList(ExportInfo.load, in),
        Binary.loadList(Binary.loadTuple(Binary.loadSymbol, Fixity.load), in),
        Binary.loadList(IfaceDecl.load, in),
        Binary.loadList(IfaceDecl.loadIfaceClsInst, in))
}
