/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast
package dcons

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.PprUtils._

import bluejelly.utils.Document.text

trait DCon extends AstElem;

case class AlgDCon(val n:Name, args:List[DConArg]) extends DCon {
  def ppr = if (n.isId) pprMany(n :: args) else 
    pprMany(List(args(0), n, args(1)))
}

case class RecDCon(val n:Name, groups:List[LabelGroup]) extends DCon {
  def ppr = gnest(n.ppr :/: between("{", pprMany(groups,","), "}"))
  
  // Produce a list of pairs (label,type,strict)
  def flatten = groups.flatMap(grp => grp.labels map ((_,grp.ty,grp.strict)))
}

class DConArg(val ty:types.Type, val strict:Boolean) extends AstElem {
  def ppr = {
    val d = ty match {
      case types.AppType(_,_) => par(ty.ppr)
      case _ => ty.ppr
    }
    gnest(if (strict) "!"::d else d)
  }
}

class LabelGroup(
    val labels:List[Name], 
    val ty:types.Type, 
    val strict:Boolean) extends AstElem {  
  def ppr = gnest(cat(List(
      pprMany(labels, ","), 
      text("::"), 
      gnest(if (strict) "!"::ty.ppr else ty.ppr)
    )))
}
