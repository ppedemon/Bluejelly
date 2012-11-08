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
private class L4Errors extends bluejelly.utils.Errors {
  import PrettyPrinter._
    
  def quote[T](v:T):String = "`%s'" format v
  
  def ppr(d:Document):String = {
    val s = new StringWriter
    d.format(75, s)
    s flush ()
    s toString
  }
  
  def dupDataCon(d:DataDecl, prev:DataDecl) {
    val doc = group(nest(2,
        group(nest(2,text("duplicated data declaration") 
          :/: quote(d.ref) :/: "at:" :/: text(d.pos.toString))) :/: 
        group(nest(2,text("(previous declaration was at:") 
          :/: prev.pos.toString :: text(")")))))
    error(this.ppr(group(doc)))
  }
  
  def dupFun(f:FunDecl, prev:FunDecl) {
    val doc = group(nest(2,
        group(nest(2,text("duplicated function declaration") 
          :/: quote(f.n) :/: "at:" :/: text(f.pos.toString))) :/: 
        group(nest(2,text("(previous declaration was at:") 
          :/: prev.pos.toString :: text(")")))))
    error(this.ppr(group(doc)))
  }
}

/**
 * Simple static analysis phase.
 * @author ppedemon
 */
class StaticAnalysis(m:Module) {
  
  private val err = new L4Errors()
  
  private def collectDecls(m:Module):Env = {
    (Env(m.n) /: m.decls) ((env,d) => d match {
      case d@DataDecl(c,_) if env hasDataCon c => err dupDataCon (d,env(c)); env
      case d@DataDecl(_,_) => env addDataCon d
      case f@FunDecl(v,_,_) if env hasFun v => err dupFun(f, env(v)); env
      case f@FunDecl(_,_,_) => env addFun f   
    })
  }
  
  def hasErrors = err hasErrors
  
  def analyze[T >: Errors]:Either[Env,T] = {
    val env = collectDecls(m)
    if (hasErrors) Right(err) else Left(env)
  }
}

object StaticAnalysis {
  def analyze(m:Module) = new StaticAnalysis(m).analyze
}
