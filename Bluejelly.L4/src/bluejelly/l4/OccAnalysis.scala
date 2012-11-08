/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4
import scala.util.control.Exception._

/**
 * Occurrence analysis for bindings in a function.
 * @author ppedemon
 */
object OccAnalysis {
  private type OccMap = Map[Var,Int]
  
  private def addOcc(map:OccMap, v:Var, times:Int = 1) =
    if (map isDefinedAt v) map updated (v,map(v)+times) else map + (v->times)
  
  private def getOcc(map:OccMap, v:Var):Occ =
    (handling(classOf[NoSuchElementException]) by (_ => 0))(map(v)) match {
      case 0 => Never
      case 1 => Once
      case _ => Many
    }
  
  private def union(m:OccMap, n:OccMap) = 
    (m /: n){case (m,(v,n)) => addOcc(m,v,n)}
  
  private def union(maps:List[OccMap]):OccMap = 
    ((Map():OccMap) /: maps)(union)
      
  private def occExpr(expr:Expr):(Expr,OccMap) = expr match {
    case ELit(_) => (expr,Map())
    case ECon(c,args) => val (es,m) = occExps(args); (ECon(c,es) at expr,m)
    case App(f,args)  => val (es,m) = occExps(args); (App(f,es) at expr, addOcc(m,f))
    case NApp(f,args) => val (es,m) = occExps(args); (NApp(f,es) at expr, addOcc(m,f))
    case Note(occ,e)  => val (e1,m) = occExpr(e); (Note(occ,e1) at expr,m)
    case Let(v,e,b) => {
      val (e1,m) = occExpr(e)
      val (b1,n) = occExpr(b) 
      val u = union(m,n)
      (Let(v, Note(getOcc(u,v),e1) at e,b1) at expr, u - v)
    }
    case Eval(v,e,b) => {
      val (e1,m) = occExpr(e)
      val (b1,n) = occExpr(b)
      val u = union(m,n)
      (Eval(v, Note(getOcc(u,v),e1) at e,b1) at expr, u - v)
    }
    case LetRec(decls,e) => {
      val (e1,m1) = occExpr(e)
      val (ids,es) = decls unzip
      val (es1,ms) = es map occExpr unzip
      val m = union(m1::ms)
      val decls1 = (ids,es1).zipped map {case (id,e) => (id, Note(getOcc(m,id),e) at e)}
      (LetRec(decls1,e1 at e) at expr, m -- ids)
    }
    case Match(v,alts) => {
      val (alts1,m) = alts map occAlt unzip;
      (Match(v,alts1) at expr, addOcc(union(m),v))
    }
  }
   
  private def occExps(exprs:List[Expr]):(List[Expr],OccMap) = {
    val (es,ms) = (exprs map occExpr).unzip
    (es, union(ms))
  }
  
  private def occAlt(alt:Alt) = {
    val (e,m) = occExpr(alt.e)
    (new Alt(alt.p,e), m -- alt.p.vars)
  }
  
  private def occFun(f:FunDecl) = {
    val (e,_) = occExpr(f.body)
    FunDecl(f.n, f.args, e) at f
  }
  
  def analyze(m:Module) = new Module(m.n, m.decls map {
    case d@DataDecl(_,_) => d
    case f@FunDecl(_,_,_) => occFun(f)
  })
  
}
