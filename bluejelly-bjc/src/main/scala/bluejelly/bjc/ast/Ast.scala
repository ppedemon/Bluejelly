/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast

import scala.text.Document
import scala.text.Document._
import scala.util.parsing.input.Positional

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.Name._
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.common.PprUtils
import bluejelly.bjc.common.PprUtils._

// Some useful name constants
object NameConstants {
  import bluejelly.bjc.common.Name.{unqualId,unqualOp}
  val nmMinus     = unqualOp('-)
  val nmAs        = unqualId('as)
  val nmForall    = unqualId('forall)
  val nmHiding    = unqualId('hiding)
  val nmQualified = unqualId('qualified)
}

/**
 * Every syntax tree node has a position and is {@link PrettyPrintable}.
 * We record such constraints in the <code>AstElem</code> trait.
 * 
 * @author ppedemon
 */
trait AstElem extends Positional with PrettyPrintable

// -----------------------------------------------------------------------
// Exports
// -----------------------------------------------------------------------

abstract sealed class Export extends AstElem;
case class EVar(name:Name) extends Export {
  def ppr = asId(name).ppr
}
case class EAll(name:Name) extends Export {
  def ppr = asId(name).ppr :: text("(..)")
}
case class ESome(name:Name,children:List[Name]) extends Export {
  def ppr = asId(name).ppr :: pprTuple(children map asId)
}
case class EMod(name:Name) extends Export {
  def ppr = "module" :/: name.ppr
}

// -----------------------------------------------------------------------
// Imports
// -----------------------------------------------------------------------

abstract sealed class Import extends AstElem;
case class IVar(name:Name) extends Import {
  def ppr = asId(name).ppr
}
case class INone(name:Name) extends Import {
  def ppr = name.ppr
}
case class IAll(name:Name) extends Import {
  def ppr = name.ppr :: text("(..)")
}
case class ISome(name:Name,children:List[Name]) extends Import {
  def ppr = name.ppr :: pprTuple(children map asId)
}

class ImpDecl(
    val modId:Name, 
    val qualified:Boolean,
    val alias:Option[Name],
    val hidden:Boolean,
    val imports:List[Import]) extends AstElem {
  def ppr = {
    val d0 = if (qualified) text("import qualified") else text("import")
    val d1 = group(if (alias.isEmpty) 
      d0 :/: modId.ppr else 
      d0 :/: modId.ppr :/: "as" :/: alias.get.ppr)
    val d2 = if (hidden) 
      d1 :/: "hiding" :: pprTuple(imports) else 
      d1 :: pprTuple(imports)
    d2
  }
}

// -----------------------------------------------------------------------
// Top level module
// -----------------------------------------------------------------------

class Module(
    val name:Name, 
    val exports:List[Export],
    val impDecls:List[ImpDecl]) extends PrettyPrintable {
  def ppr = 
    gnest("module" :/: name.ppr :: pprTuple(exports) :/: text("where")) :/:
    gnest(nl :: vppr(impDecls))
}
