/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.ast
package decls

import bluejelly.bjc.common.{Id,Op}
import bluejelly.bjc.common.Name
import bluejelly.bjc.common.Name.{asId}
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable

import scala.text.Document.{empty,group,text}

// -----------------------------------------------------------------------
// Type signatures
// -----------------------------------------------------------------------
case class TySigDecl(val vars:List[Name], val ty:types.Type) extends TopDecl {
  def ppr = gnest(group(pprMany(vars map asId, ",") :/: text("::")) :/: ty.ppr)
}

// -----------------------------------------------------------------------
// Type synonyms, algebraic data types and new types
// -----------------------------------------------------------------------
case class TySynDecl(
    val n:Name, 
    val vars:List[Name], 
    val rhs:types.Type) extends TopDecl {
  def ppr = gnest(
      "type" :/: 
      group(n.ppr :/: pprMany(vars) :/: text("=")) :/: 
      rhs.ppr)
}

case class DataDecl(
    val n:Name, 
    val vars:List[Name], 
    val ctx:Option[List[types.Pred]],
    val rhs:List[DCon],
    val derivings:List[Name]) extends TopDecl {
  def ppr = {
    val dctx = ctx.map(ctx => group(
      (if (ctx.length == 1) ctx.head.ppr else pprTuple(ctx)) :/: text("=>")))
      .getOrElse(empty)
    val dlhs = gnest(cat(List(
        text("data"), 
        dctx, 
        group(n.ppr :/: pprMany(vars)),
        if (rhs.isEmpty) empty else text("="))))
    val drhs = pprMany(rhs," |")
    val ders = derivings match { 
      case Nil => empty 
      case _ => group("deriving" :: pprTuple(derivings)) 
    }
    gnest(cat(List(dlhs, drhs, ders)))
  }
}

case class NewTyDecl(
    val n:Name, 
    val v:Name, 
    val ctx:Option[List[types.Pred]],
    val rhs:DCon,
    val derivings:List[Name]) extends TopDecl {
  def ppr = {
    val dctx = ctx.map(ctx => group(
      (if (ctx.length == 1) ctx.head.ppr else pprTuple(ctx)) :/: text("=>")))
      .getOrElse(empty)
    val dlhs = gnest(cat(List(
        text("newtype"), 
        dctx, 
        group(n.ppr :/: v.ppr),
        text("="))))
    val ders = derivings match { 
      case Nil => empty 
      case _ => group("deriving" :: pprTuple(derivings)) 
    }
    gnest(cat(List(dlhs, rhs.ppr, ders)))
  }
}

// -----------------------------------------------------------------------
// Data constructors
// -----------------------------------------------------------------------

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
