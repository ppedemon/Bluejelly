/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import scala.text._
import scala.text.Document._
import java.io.StringWriter
import bluejelly.utils.Errors

/**
 * Custom error bag.
 * @author ppedemon
 */
private class L4Errors extends Errors(false) {
  def quote[T](v:T):String = "`%s'" format v
  
  def ppr(d:Document):String = {
    val s = new StringWriter
    d.format(75, s)
    s flush ()
    s toString
  }
  
  def gnest(d:Document):Document = group(nest(2,d))
  
  def pprList[T](xs:List[T]):Document = text(xs mkString ("[",",","]"))
  
  def dupDataCon(d:DataDecl, prev:DataDecl) {
    val doc = gnest(
      gnest("duplicated data declaration" :/: quote(d.ref) :/: "at:" :/: text(d.pos.toString)) :/: 
      gnest("(previous declaration was at:" :/: prev.pos.toString :: text(")")))
    error(ppr(doc))
  }
  
  def dupFun(f:FunDecl, prev:FunDecl) {
    val doc = gnest(
      gnest("duplicated function declaration" :/: quote(f.n) :/: "at:" :/: text(f.pos.toString)) :/: 
      gnest("(previous declaration was at:" :/: prev.pos.toString :: text(")")))
    error(ppr(doc))
  }
  
  def patError(p:Pat, d:Document) = {
    val doc = gnest(d :/: 
        gnest("in pattern:" :/: PrettyPrinter.ppr(p)) :/: 
        gnest("at:" :/: text(p.pos.toString)))
    error(ppr(doc))
  }
  
  def invalidPat(p:Pat, c:ConRef) {
    val msg = gnest("undefined constructor:" :/: text(quote(c)))
    patError(p, msg)
  }
  
  def invalidPat(p:Pat, vs:List[Var]) {
    patError(p, gnest(text("non-linear variable(s):") :/: pprList(vs)))
  }
  
  def nonLinearParams(f:FunDecl, vs:List[Var]) {
    val doc = gnest(
      gnest("non-linear parameter(s):" :/: pprList(vs)) :/:
      gnest("in function:" :/: quote(f.n) :/: "at:" :/: text(f.pos.toString)))
    error(ppr(doc))
  }
}

/**
 * Simple static analysis phase.
 * @author ppedemon
 */
class StaticAnalysis(m:Module) {
  
  private val err = new L4Errors()
  
  def hasErrors = err hasErrors

  def analyze[T >: Errors]:Either[Env,T] = {
    val env = collectDecls(m)
    if (hasErrors) return Right(err)
    
    m.decls foreach {
      case f@FunDecl(_,_,_) => analyzeFun(env)(f)
      case _ => ()
    }
    if (hasErrors) return Right(err) else Left(env)
  }

  private def repeated[T](xs:List[T]):List[T] = xs diff (xs distinct)
  
  private def collectDecls(m:Module):Env = {
    (Env(m.n) /: m.decls) ((env,d) => d match {
      case d@DataDecl(c,_) if env hasDataCon c => err dupDataCon (d,env(c)); env
      case d@DataDecl(_,_) => env addDataCon d
      case f@FunDecl(v,_,_) if env hasFun v => err dupFun(f, env(v)); env
      case f@FunDecl(_,_,_) => env addFun f   
    })
  }
  
  private def analyzePat(env:Env)(p:Pat):Env = p match {
    case PLit(_) => env
    case PVar(v) => env addLocal v
    case PCon(c,_) if !(env hasDataCon c) => err invalidPat (p,c); env
    case PCon(_,vs) => repeated(vs) match {
      case Nil => env addLocals vs
      case vs => err invalidPat (p,vs); env
    }
  }
  
  private def analyzeFun(env:Env)(f:FunDecl) {
    val rep = repeated(f.args)
    if (!(rep isEmpty)) err nonLinearParams (f,rep)
  }   
  
}

object StaticAnalysis {
  def analyze(m:Module) = new StaticAnalysis(m).analyze
}
