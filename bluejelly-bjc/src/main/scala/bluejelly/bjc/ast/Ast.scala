/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast

import scala.text.Document
import scala.text.Document.{text}
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
// Top level modules
// -----------------------------------------------------------------------

class Module(val name:Name, val exports:List[Export]) extends PrettyPrintable {
  def ppr = "module" :/: name.ppr :: pprTuple(exports)
}
