/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast
package module

import scala.text.Document
import scala.text.Document._

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.Name._
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable

// -----------------------------------------------------------------------
// Syntax tree for module exports
// -----------------------------------------------------------------------
sealed trait ESpec extends AstElem;
case class EVar(name:Name) extends ESpec {
  def ppr = asId(name).ppr
}
case class EAll(name:Name) extends ESpec {
  def ppr = asId(name).ppr :: text("(..)")
}
case class ESome(name:Name,children:List[Name]) extends ESpec {
  def ppr = asId(name).ppr :: pprTuple(children map asId)
}
case class EMod(name:Name) extends ESpec {
  def ppr = "module" :/: name.ppr
}

sealed trait Exports extends PrettyPrintable;
case object ExportAll extends Exports { def ppr = empty }
case class ExportSome(es:List[ESpec]) extends Exports { 
  def ppr = pprTuple(es) 
}

// -----------------------------------------------------------------------
// Syntax tree for module imports
// -----------------------------------------------------------------------
sealed trait ISpec extends AstElem;
case class IVar(name:Name) extends ISpec {
  def ppr = asId(name).ppr
}
case class INone(name:Name) extends ISpec {
  def ppr = name.ppr
}
case class IAll(name:Name) extends ISpec {
  def ppr = name.ppr :: text("(..)")
}
case class ISome(name:Name,children:List[Name]) extends ISpec {
  def ppr = name.ppr :: pprTuple(children map asId)
}

sealed trait Imports extends PrettyPrintable;
case object ImportAll extends Imports { def ppr = empty}
case class ImportSome(val is:List[ISpec]) extends Imports {
  def ppr = pprTuple(is)
}
case class HideSome(val is:List[ISpec]) extends Imports {
  def ppr = "hiding" :: pprTuple(is)
}

class ImpDecl(
    val modId:Name, 
    val qualified:Boolean,
    val alias:Option[Name],
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
    val name:Name, 
    val exports:Exports,
    val impDecls:List[ImpDecl],
    val topDecls:List[TopDecl]) extends PrettyPrintable {
  
  def this(name:Name,exports:Exports) = this(name,exports,Nil,Nil)
  
  def addImpDecl(i:ImpDecl) = new Module(name,exports,i::impDecls,topDecls)
  def addTopDecl(d:TopDecl) = new Module(name,exports,impDecls,d::topDecls)
  
  def ppr = cat(List(
    gnest("module" :/: name.ppr :: exports.ppr :/: text("where")),
    if (impDecls.isEmpty) empty else nl :: vppr(impDecls),
    if (topDecls.isEmpty) empty else nl :: vppr(topDecls)
  ))
}

object Module {
  private def defaultName = unqualId('Main)
  private def defaultExports = ExportSome(List(EVar(unqualId('main))))
  def defaultModule = new Module(defaultName, defaultExports)
}
