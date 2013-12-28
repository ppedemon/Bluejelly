/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.iface

import scala.text.Document.{empty,group,text}

import bluejelly.bjc.common.Name.asOp
import bluejelly.bjc.common.{Name,Qual}
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.ast.decls.{Assoc,NoAssoc,LeftAssoc,RightAssoc}

/**
 * Things exported by an interface file.
 * @author ppedemon
 */
abstract class IfaceExport(val name:Name) extends PrettyPrintable
case class ExportedId(override val name:Name) extends IfaceExport(name) { 
  def ppr = name.ppr 
}
case class ExportedTc(
    override val name:Name, 
    val children:List[Name]) extends IfaceExport(name) {
  def ppr = group(name.ppr :/: between("{",pprMany(children,","),"}"))
}

/**
 * Fixity type.
 * @author ppedemon
 */
class Fixity(val assoc:Assoc, val prec:Int) extends PrettyPrintable {
  def ppr =  group(text(assoc match {
    case NoAssoc => "infix" 
    case LeftAssoc => "infixl"
    case RightAssoc => "infixr"
  }) :/: text(prec.toString))
}

/**
 * A module interface.
 * @author ppedemon
 */
class ModIface(
    val name:Name,
    val exports:List[IfaceExport],
    val fixities:List[(Name,Fixity)],
    val decls:List[IfaceDecl],
    val insts:List[IfaceClsInst]) extends PrettyPrintable {
  def ppr = {
    val fds = fixities map Function.tupled((n,f) => new PrettyPrintable {
      def ppr = group(f.ppr :/: asOp(n).ppr)
    })
    val fd = if (fixities.isEmpty) empty else 
      gnest("Fixities:" :/: pprMany(fds, ","))
    val ed = if (exports.isEmpty) empty else 
      gnest("Exports:" :/: pprMany(exports))
    cat(List(
      group("module" :/: name.ppr :/: text("where")), 
      nl :: ed, 
      nl :: fd,       
      if (decls.isEmpty) empty else nl :: vppr(decls),
      if (insts.isEmpty) empty else nl :: vppr(insts)))
  }
}
