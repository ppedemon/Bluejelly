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
  
  private implicit val times:Int = 1
  
  private def addOcc(map:OccMap, v:Var)(implicit times:Int) = 
    if (map isDefinedAt v) map updated (v,map(v)+times) else map + (v->times)
  
  private def getOcc(map:OccMap, v:Var):Occ =
    (handling(classOf[NoSuchElementException]) by (_ => 0))(map(v)) match {
      case 0 => Never
      case 1 => Once
      case _ => Many
    }
  
  private def union(m:OccMap, n:OccMap) = 
    (m /: n){case (m,(v,n)) => addOcc(m,v)(n)}
  
  private def union(maps:List[OccMap]):OccMap = 
    ((Map():OccMap) /: maps)(union)
  
  private def del(map:OccMap, vars:List[Var]) = 
    (map /: vars)(_ - _)
    
  private def occExp(expr:Expr):(Expr,OccMap) = expr match {
    case ELit(_) => (expr,Map())
    case ECon(c,args) => val (es,m) = occExps(args); (ECon(c,es),m)
    case App(f,args)  => val (es,m) = occExps(args); (App(f,es), addOcc(m,f))
    case NApp(f,args) => val (es,m) = occExps(args); (NApp(f,es), addOcc(m,f))
    case Note(occ,e)  => val (e1,m) = occExp(e); (Note(occ,e1),m)
    case Let(v,e,b) => {
      val (e1,m) = occExp(e)
      val (b1,n) = occExp(b)
      val u = union(m,n)
      (Let(v, Note(getOcc(u,v),e1),b1), u - v)
    }
    case Eval(v,e,b) => {
      val (e1,m) = occExp(e)
      val (b1,n) = occExp(b)
      val u = union(m,n)
      (Eval(v, Note(getOcc(u,v),e1),b1), u - v)      
    }
    case LetRec(decls,e) => {
      val (e1,m1) = occExp(e)
      val (ids,es) = decls unzip
      val (es1,ms) = es map occExp unzip
      val m = union(m1::ms)
      val decls1 = (ids,es1).zip map {case (id,e) => (id, Note(getOcc(m,id),e))}
      (LetRec(decls1,e1), del(m,ids))
    }
    case Match(v,alts) => {
      val (alts1,m) = alts map occAlt unzip;
      (Match(v,alts1), addOcc(union(m),v))
    }
  }
   
  private def occExps(exprs:List[Expr]):(List[Expr],OccMap) = {
    val (es,ms) = (exprs map occExp).unzip
    (es, union(ms))
  }
  
  private def occAlt(alt:Alt) = {
    val (e,m) = occExp(alt.e)
    (new Alt(alt.p,e), del(m,alt.p.vars))
  }
  
  private def occFun(f:FunDecl) = {
    val (e,_) = occExp(f.body)
    FunDecl(f.n, f.args, e)
  }
  
  def analyze(m:Module) = new Module(m.n, m.decls map {
    case d@DataDecl(_,_) => d
    case f@FunDecl(_,_,_) => occFun(f)
  })
  
}
