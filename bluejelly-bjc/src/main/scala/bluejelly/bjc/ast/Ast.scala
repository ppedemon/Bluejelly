/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast

import scala.math.BigInt
import scala.text.Document.{text}
import scala.util.parsing.input.Positional

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.Name._
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.common.PprUtils.{pprChrLit,pprStrLit}

// Some useful name constants
object NameConstants {
  import bluejelly.bjc.common.Name
  val nmMinus     = Name('-)
  val nmAs        = Name('as)
  val nmHiding    = Name('hiding)
  val nmQualified = Name('qualified)
}

/**
 * Every syntax tree node has a position and is {@link PrettyPrintable}.
 * We record such constraints in the <code>AstElem</code> trait.
 * 
 * @author ppedemon
 */
trait AstElem extends Positional with PrettyPrintable

/**
 * Common trait for top-level module declarations.
 * @author ppedemon
 */
trait TopDecl extends AstElem

/**
 * Common trait for declarations that can be local (e.g., they can
 * appear inside a "where" or a "let"). This is a refinement of
 * trait [[TopLevelDecl]].
 * @author ppedemon
 */
trait Decl extends TopDecl

/*
 * Literals
 */
trait Lit extends AstElem;
case class IntLit(val x:BigInt) extends Lit { def ppr = text(x.toString) }
case class FloatLit(val x:Double) extends Lit { def ppr = text(x.toString) }
case class CharLit(val x:Char) extends Lit { def ppr = pprChrLit(x) }
case class StringLit(val x:String) extends Lit { def ppr = pprStrLit(x) }

/*
 * Modeling generic Type/Data constructor types.
 */
trait GCon extends PrettyPrintable;
case class Con(val n:Name) extends GCon { def ppr = n.ppr }
case object ListCon extends GCon { def ppr = text("[]") }
case object UnitCon extends GCon { def ppr = text("()") }
case object ArrowCon extends GCon { def ppr = text("->") }
case class TupleCon(val arity:Int) extends GCon { 
  def ppr = text("(%s)" format (","*(arity-1)))
}
