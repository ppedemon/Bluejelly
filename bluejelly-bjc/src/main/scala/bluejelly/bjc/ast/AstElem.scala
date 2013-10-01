/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast

import scala.util.parsing.input.Positional

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.Name._
import bluejelly.bjc.common.PrettyPrintable

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

/**
 * Common trait for top-level module declarations.
 * @author ppedemon
 */
trait TopLevelDecl extends AstElem

/**
 * Common trait for declarations that can be local (e.g., they can
 * appear inside a "where" or a "let"). This is a refinement of
 * trait [[TopLevelDecl]].
 * @author ppedemon
 */
trait Decl extends TopLevelDecl