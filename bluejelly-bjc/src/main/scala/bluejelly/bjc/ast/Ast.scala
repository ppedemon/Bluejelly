/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast

import scala.util.parsing.input.Positional
import bluejelly.bjc.common.PrettyPrintable

/**
 * Every syntax tree node has a position and is {@link PrettyPrintable}.
 * We record such constraints in the <code>AstElem</code> trait.
 * 
 * @author ppedemon
 */
trait AstElem extends Positional with PrettyPrintable

// Some useful name constants
object NameConstants {
  import bluejelly.bjc.common.Name.{unqualId,unqualOp}
  val nmMinus     = unqualOp('-)
  val nmAs        = unqualId('as)
  val nmForall    = unqualId('forall)
  val nmHiding    = unqualId('hiding)
  val nmQualified = unqualId('qualified)
}

// ----------------------------------------------------------------------
// Types
// ----------------------------------------------------------------------
sealed abstract class AstType extends AstElem

/*
case class ForAll(val vs:List[VarId], ty:TypeSyn) extends TypeSyn {
  def ppr = group("forall" :/: pprList(vs) :/: ty.ppr)
}
*/
