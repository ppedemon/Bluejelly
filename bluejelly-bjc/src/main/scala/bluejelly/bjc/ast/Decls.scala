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
import bluejelly.bjc.common.Name.{asId,asOp}
import bluejelly.bjc.common.PprUtils._
import bluejelly.bjc.common.PrettyPrintable

import pat.Pat
import exp.{Exp,Guarded}
import dcons.DCon

import scala.text.Document.{empty,group,text}

// -----------------------------------------------------------------------
// Declarations (can appear inside a where, let or at the top level)
// -----------------------------------------------------------------------

/*
 * Fixity
 */
sealed trait Assoc
case object NoAssoc extends Assoc
case object LeftAssoc extends Assoc
case object RightAssoc extends Assoc

case class FixityDecl(
    val assoc:Assoc, 
    val prec:Int, 
    val ops:List[Name]) extends Decl{
  def ppr =
    gnest(text(assoc match {
    case NoAssoc => "infix" 
    case LeftAssoc => "infixl"
    case RightAssoc => "infixr"
  }) :/: prec.toString :/: pprMany(ops map asOp, ","))
}

object FixityDecl {
  val maxPrec = 9
  val defPrec = maxPrec
}

/*
 * Type signatures
 */
case class TySigDecl(val vars:List[Name], val ty:types.Type) extends Decl {
  def ppr = gnest(group(pprMany(vars map asId, ",") :/: text("::")) :/: ty.ppr)
}

trait Rhs extends AstElem;
case class FunRhs(val exp:Exp) extends Rhs {
  def ppr = exp.ppr
}
case class GrdRhs(val gs:List[Guarded]) extends Rhs {
  def ppr = group(cat(gs map (_.ppr("="))))
}

/*
 * Function bindings
 */
case class FunBind(
    val fun:Name, 
    val args:List[pat.Pat], 
    val rhs:Rhs,
    val ds:List[Decl]) extends Decl {
  def ppr = {
    val w = if (ds.isEmpty) empty else gnest("where" :/: pprBlock(ds))
    val lhs = (fun.nc,args) match {
      case (Id,_) => pprMany(fun::args)
      case (Op,List(a0,a1)) => a0.ppr :/: fun.ppr :/: a1.ppr
      case (Op,a0::a1::as) if args.length > 2 =>
        val d = a0.ppr :/: fun.ppr :/: a1.ppr
        group(par(d) :/: pprMany(as))
      case _ => empty // Impossible, just to avoid a compilation warning
    }
    val d = rhs match {
      case FunRhs(_) => group(group(lhs :/: text("=")) :/: rhs.ppr)
      case GrdRhs(_) => group(lhs :/: rhs.ppr)
    }
    gnest(cat(d, w))
  }
}

/*
 * Pattern bindings
 */
case class PatBind(val pat:Pat, val rhs:Rhs, val ds:List[Decl]) extends Decl {
  def ppr = {
    val w = if (ds.isEmpty) empty else gnest("where" :/: pprBlock(ds))
    val d = rhs match {
      case FunRhs(_) => group(group(pat.ppr :/: text("=")) :/: rhs.ppr)
      case GrdRhs(_) => group(pat.ppr :/: rhs.ppr)
    }
    gnest(cat(d, w))
  }
}

// -----------------------------------------------------------------------
// Top level declarations
// -----------------------------------------------------------------------

/*
 * Type synonyms
 */
case class TySynDecl(
    val n:Name, 
    val vars:List[Name], 
    val rhs:types.Type) extends TopDecl {
  def ppr = gnest(
      "type" :/: 
      group(n.ppr :/: pprMany(vars) :/: text("=")) :/: 
      rhs.ppr)
}

/*
 * Type constructors
 */
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

/*
 * New types
 */
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
