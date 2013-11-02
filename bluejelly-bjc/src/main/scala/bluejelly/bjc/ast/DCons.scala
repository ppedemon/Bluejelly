/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast
package dcons

import bluejelly.bjc.common.{Name,Id,Op}
import bluejelly.bjc.common.PprUtils._

import scala.text.Document.text

trait DCon extends AstElem;

case class PolyDCon(val tyvars:List[Name], val dcon:DCon) extends DCon {
  def ppr = gnest("forall" :/: pprMany(tyvars) :/: "." :/: dcon.ppr)
}

case class QualDCon(val ctx:List[types.Pred], val dcon:DCon) extends DCon {
  def ppr = gnest(
      (if (ctx.length == 1) ctx.head.ppr else pprTuple(ctx)) :/: 
      "=>" :/: dcon.ppr)
}

case class AlgDCon(val n:Name, args:List[DConArg]) extends DCon {
  def ppr = n.nc match {
    case Id => pprMany(n :: args)
    case Op => pprMany(List(args(0), n, args(1)))
  }
}

case class RecDCon(val n:Name, groups:List[LabelGroup]) extends DCon {
  def ppr = gnest(n.ppr :/: between("{", pprMany(groups,","), "}"))
}

class DConArg(val ty:types.Type, val strict:Boolean) extends AstElem {
  def ppr = {
    val d = ty match {
      case types.PolyType(_,_)|types.AppType(_,_) => par(ty.ppr)
      case _ => ty.ppr
    }
    gnest(if (strict) "!"::d else d)
  }
}

class LabelGroup(
    val labels:List[Name], 
    ty:types.Type, 
    val strict:Boolean) extends AstElem {
  def ppr = gnest(cat(List(
      pprMany(labels, ","), 
      text("::"), 
      gnest(if (strict) "!"::ty.ppr else ty.ppr)
    )))
}
