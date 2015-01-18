/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast
package module

import bluejelly.bjc.common.{Name,LocalName}
import bluejelly.bjc.common.Name._
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable

import bluejelly.utils.Document
import bluejelly.utils.Document._

import decls._

// -----------------------------------------------------------------------
// Syntax tree fa module exports
// -----------------------------------------------------------------------
sealed trait ESpec extends AstElem;
case class EVar(name:Name) extends ESpec {
  def ppr = asId(name).ppr
}
case class EAll(name:Name) extends ESpec {
  def ppr = asId(name).ppr :: text("(..)")
}
case class EAbs(name:Name) extends ESpec {
  def ppr = asId(name).ppr
}
case class ESome(name:Name,children:List[Name]) extends ESpec {
  def ppr = asId(name).ppr :: pprTuple(children map asId)
}
case class EMod(modName:Symbol) extends ESpec {
  def ppr = "module" :/: modName.ppr
}

sealed trait Exports extends PrettyPrintable;
case object ExportAll extends Exports { def ppr = empty }
case class ExportSome(es:List[ESpec]) extends Exports { 
  def ppr = pprTuple(es) 
}

// -----------------------------------------------------------------------
// Syntax tree for module imports
// -----------------------------------------------------------------------
sealed abstract class ISpec(val name:LocalName) extends PrettyPrintable;
case class IVar(override val name:LocalName) extends ISpec(name) {
  def ppr = asId(name).ppr
}
case class INone(override val name:LocalName) extends ISpec(name) {
  def ppr = name.ppr
}
case class IAll(override val name:LocalName) extends ISpec(name) {
  def ppr = name.ppr :: text("(..)")
}
case class ISome(
    override val name:LocalName,
    children:List[LocalName]) extends ISpec(name) {
  def ppr = name.ppr :: pprTuple(children map asId)
}

sealed trait Imports extends PrettyPrintable;
case object ImportAll extends Imports { def ppr = empty }
case class ImportSome(val is:List[ISpec]) extends Imports {
  def ppr = pprTuple(is)
}
case class HideSome(val is:List[ISpec]) extends Imports {
  def ppr = "hiding" :: pprTuple(is)
}

class ImpDecl(
    val modId:Symbol, 
    val qualified:Boolean,
    val alias:Option[Symbol],
    val imports:Imports) extends AstElem {
  def ppr = {
    val d0 = if (qualified) text("import qualified") else text("import")
    val d1 = group(if (alias.isEmpty) 
      d0 :/: modId.ppr else 
      d0 :/: modId.ppr :/: "as" :/: alias.get.ppr)
    gnest(imports match {
      case HideSome(_) => d1 :/: imports.ppr
      case _ => d1 :: imports.ppr
    })
  }
}

// -----------------------------------------------------------------------
// A top-level module.
// -----------------------------------------------------------------------
class Module(
    val name:Symbol, 
    val exports:Exports,
    val impDecls:List[ImpDecl],
    val topDecls:List[TopDecl]) extends PrettyPrintable {
  
  def this(name:Symbol,exports:Exports) = this(name,exports,Nil,Nil)
  
  def addImpDecl(i:ImpDecl) = new Module(name,exports,i::impDecls,topDecls)
  def addTopDecl(d:TopDecl) = new Module(name,exports,impDecls,d::topDecls)
  
  def ppr = cat(List(
    gnest("module" :/: name.ppr :: exports.ppr :/: text("where")),
    if (impDecls.isEmpty) empty else nl :: vppr(impDecls),
    if (topDecls.isEmpty) empty else nl :: vppr(topDecls)
  ))
}

object Module {
  private def defaultName = 'Main
  private def defaultExports = ExportSome(List(EVar(idName('main))))
  def defaultModule = new Module(defaultName, defaultExports)
}
