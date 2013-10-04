/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.Name.{asId}
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable

import scala.text.Document.{group}

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

class TySigDecl(val vars:List[Name], val ty:types.Type) extends TopDecl {
  def ppr = gnest(pprMany(vars map asId, ",") :/: "::" :/: ty.ppr)
}
