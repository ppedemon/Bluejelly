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
  
  private def quote[T](v:T):String = "`%s'" format v
  
  private def ppr(d:Document):String = {
    val s = new StringWriter
    d.format(75, s)
    s flush ()
    s toString
  }
  
  private def gnest(d:Document):Document = group(nest(2,d))
  
  private def pprList[T](xs:List[T]):Document = text(xs mkString ("[",",","]"))
  
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
  
  private def wrongPat(f:FunDecl, p:Pat, d:Document) = {
    val doc = gnest(d :/: 
      gnest(
        group("in pattern:" :/: PrettyPrinter.ppr(p)) :/: 
        group("at:" :/: text(p.pos.toString))) :/:
      gnest(
        group("in function:" :/: text(f.n.toString)) :/: 
        group("at:" :/: text(f.pos.toString))))
    error(ppr(doc))
  }
  
  def undefPat(f:FunDecl, p:Pat, c:ConRef) {
    val msg = gnest("undefined constructor:" :/: text(quote(c)))
    wrongPat(f, p, msg)
  }
  
  def nonLinearPat(f:FunDecl, p:Pat, vs:List[Var]) {
    wrongPat(f, p, gnest(text("non-linear variable(s):") :/: pprList(vs)))
  }
  
  def unsaturatedPat(f:FunDecl, p:Pat, c:ConRef) {
    wrongPat(f, p, gnest("unsaturated constructor:" :/: text(quote(c))))
  }
  
  def nonLinearParams(f:FunDecl, vs:List[Var]) {
    val doc = gnest(
      gnest("non-linear parameter(s):" :/: pprList(vs)) :/:
      gnest("in function:" :/: quote(f.n) :/: "at:" :/: text(f.pos.toString)))
    error(ppr(doc))
  }
  
  private def genExprMsg(act:String=>Unit)(f:FunDecl, expr:Expr, d:Document) {
    val doc = gnest(d :/: 
      gnest(
        group("in expression:" :/: PrettyPrinter.ppr(expr)) :/: 
        group("at:" :/: text(expr.pos.toString))) :/:
      gnest(
        group("in function:" :/: text(f.n.toString)) :/: 
        group("at:" :/: text(f.pos.toString))))
      error(ppr(doc))
  }
  
  def wrongExpr = genExprMsg(error)_
  def fishyExpr = genExprMsg(warning)_
  
  def undefDataCon(f:FunDecl, expr:Expr, c:ConRef) {
    wrongExpr(f, expr, gnest("undefined data constructor" :/: text(quote(c))))
  }
  
  def unsaturatedDataCon(f:FunDecl, expr:Expr) {
    wrongExpr(f, expr, text("wrong number of parameters for data constructor application"))
  }
  
  def nonAtomicExpr(f:FunDecl, parent:Expr, expr:Expr) {
    wrongExpr(f, parent, 
        gnest("unexpected non-atomic expression" :/: PrettyPrinter.ppr(expr)))
  }
  
  def invalidLit(f:FunDecl, parent:Expr, expr:Expr) {
    wrongExpr(f, parent, 
      gnest("literal" :/: PrettyPrinter.ppr(expr)) :/: 
        text("is not a recursive expression"))
  }
  
  def undefVar(f:FunDecl, expr:Expr, v:Var) {
    wrongExpr(f, expr, gnest("undefined variable" :/: text(quote(v))))
  }
   
  def wrongType(f:FunDecl, expr:Expr, alt:Alt) {
    val d = gnest("incompatible alternative:" :/: PrettyPrinter.pprAlt(alt))
    wrongExpr(f, expr, d)
  }
  
  def duplicatedAlt(f:FunDecl, expr:Expr, alt:Alt) {
    val d = gnest("uplicated alternative:" :/: PrettyPrinter.pprAlt(alt))
    fishyExpr(f, expr, d)
  }
  
  def unreachableAlts(f:FunDecl, expr:Expr, alt:Alt) {
    val d = gnest("unreachable alternatives after" :/: PrettyPrinter.ppr(alt.p))
    fishyExpr(f, expr, d)
  }
}

/**
 * Simple static analysis phase.
 * @author ppedemon
 */
class StaticAnalysis(m:Module) {
  
  private val err = new L4Errors()
  
  /**
   * Answer if we found validation errors.
   */
  def hasErrors = err hasErrors

  /**
   * Analyze the module passed in the class constructor.
   */
  def analyze[T >: Errors]:Either[T,Env] = {
    val env = collectDecls(m)
    if (hasErrors) return Left(err)
    
    m.decls foreach {
      case f@FunDecl(_,_,_) => analyzeFun(env)(f)
      case _ => ()
    }
    if (hasErrors) return Left(err) else Right(env)
  }

  // Get repeated elements in the given list. 
  // If every element is unique, return Nil.
  private def repeated[T](xs:List[T]):List[T] = xs diff (xs distinct)
  
  // Is the given expression a literal?
  private def isLit(expr:Expr):Boolean = expr match {
    case ELit(_) => true
    case Note(_,e) => isLit(e)
    case _ => false
  }
  
  // Check that all the expressions in es are not literals
  private def allNonLit(f:FunDecl, parent:Expr, es:List[Expr]):Boolean = es match {
    case Nil => true
    case expr::es if isLit(expr) => err invalidLit (f, parent, expr); false
    case _::es => allNonLit(f, parent, es)
  }
  
  // Is the given expression atomic?
  private def isAtom(expr:Expr):Boolean = expr match {
    case Eval(_,_,_) | Match(_,_) => false
    case Note(_,e) => isAtom(e)
    case Let(_,_,e) => isAtom(e)
    case LetRec(_,e) => isAtom(e)
    case _ => true
  }
  
  // Check that all the expressions in es are atomic, 
  // giving proper error messages if not
  private def allAtoms(f:FunDecl, parent:Expr, es:List[Expr]):Boolean = es match {
    case Nil => true
    case expr::es if isAtom(expr) => allAtoms (f, expr, es)
    case expr::_ => err nonAtomicExpr (f, parent, expr); false
  }
  
  // Check if the given constructor application is saturated
  private def isSaturated[T](env:Env, c:ConRef, args:List[T]):Boolean = 
    env(c).con.arity == args.length
  
  // Collect top-level declarations, checking for duplicates
  private def collectDecls(m:Module):Env = {
    (Env(m.n) /: m.decls) ((env,d) => d match {
      case d@DataDecl(c,_) if env hasDataCon c => err dupDataCon (d,env(c)); env
      case d@DataDecl(_,_) => env addDataCon d
      case f@FunDecl(v,_,_) if env hasFun v => err dupFun(f, env(v)); env
      case f@FunDecl(_,_,_) => env addFun f   
    })
  }
  
  // Check if the given path is valid
  private def analyzePat(env:Env, f:FunDecl)(p:Pat):Env = p match {
    case PLit(_) => env
    case PVar(v) => env addLocal v
    case PCon(c,_) if !(env hasDataCon c) => err undefPat (f,p,c); env
    case PCon(c,args) if !isSaturated(env,c,args) => err unsaturatedPat (f,p,c); env
    case PCon(c,vs) => repeated(vs) match {
      case Nil => env addLocals vs
      case vs => err nonLinearPat (f,p,vs); env
    }
  }
  
  // Analyze an application
  // NB: lacking an import mechanism, we blatantly ignore external variables
  private def analyzeApp(env:Env, f:FunDecl, parent:Expr, fun:Var, args:List[Expr]) {
    if (env.isLocal(fun) && !(env inScope fun)) err undefVar (f, parent, fun)
    val argsOk = allAtoms(f, parent, args)
    if (argsOk) args foreach analyzeExpr(env,f)
  }
   
  // Check that types are compatible in the given list of alternatives
  private def typesOk(f:FunDecl, expr:Expr, alts:List[Alt]):Boolean = {
    def check(alts:List[Alt]):Boolean = alts match {
      case Nil => true
      case List(a) => true
      case a::b::as if Pat.sameType(a.p,b.p) => check(b::as)
      case _::b::_ => err wrongType (f, expr, b); false
    }
    check(alts filter {!_.isVarAlt})
  }
  
  // Check that there are no duplicated in the given list of alternatives
  private def dupsOk(env:Env, f:FunDecl, expr:Expr, alts:List[Alt]):Boolean = {
    def check(alts:List[Alt], values:Set[Any]):Boolean = alts match {
      case Nil => true
      case a::as => a.p match {
        case PVar(_) => check(as, values)
        case PCon(c,_) if (env hasDataCon c) && (values contains env(c).con.tag) => 
          err duplicatedAlt(f, expr, a); false
        case PLit(x) if values contains Lit.value(x) => 
          err duplicatedAlt(f, expr, a); false
        case _ => check(as, values)
      }
    }
    check(alts, Set())
  }
  
  // Check that all alternatives are reachable
  private def reachableOk(f:FunDecl, expr:Expr, alts:List[Alt]):Boolean = alts match {
    case Nil => true
    case List(a) => true
    case a::_ if a isVarAlt => err unreachableAlts (f, expr, a); false
    case _::as => reachableOk(f, expr, as)
  }
  
  // Analyze a case alternative
  private def analyzeAlt(env:Env, f:FunDecl)(alt:Alt) {
    val extEnv = analyzePat(env, f)(alt.p)
    analyzeExpr(extEnv, f)(alt.e)
  }

  // Analyze a list of case alternatives
  private def analyzeAlts(env:Env, f:FunDecl, expr:Expr, alts:List[Alt]) {
    val tysOk = typesOk(f, expr, alts)
    if (!tysOk) return;
    val reachOk = reachableOk(f, expr, alts)
    if (reachOk) dupsOk(env, f, expr, alts)
    alts foreach analyzeAlt(env,f)
  }
  
  // Validate an expression
  private def analyzeExpr(env:Env, f:FunDecl)(expr:Expr):Unit = expr match {
    case ELit(_) =>
    case App(fun,args)  => analyzeApp(env, f, expr, fun, args)
    case NApp(fun,args) => analyzeApp(env, f, expr, fun, args)
    case Note(_,e) => analyzeExpr(env,f)(e)
    
    case con@ECon(c,args) => {
      if (!(env hasDataCon c)) err undefDataCon (f,expr,c)
      if (!isSaturated(env,c,args)) err unsaturatedDataCon (f,expr)
      val argsOk = allAtoms(f, expr, args)
      if (argsOk) args foreach analyzeExpr(env,f)
    }

    case Let(v,e,b) => {
      if (!isAtom(e)) { err nonAtomicExpr(f, expr, e); return }
      analyzeExpr(env,f)(e)
      analyzeExpr(env addLocal v, f)(b)
    }
    case Eval(v,e,b) => {
      analyzeExpr(env,f)(e)
      analyzeExpr(env addLocal v, f)(b)
    }
    case LetRec(decls,b) => {
      val (vs,es) = (decls unzip)
      if (!allAtoms(f,expr,es)) return
      if (!allNonLit(f,expr,es)) return
      val extEnv = env addLocals vs
      es foreach analyzeExpr(extEnv,f)
      analyzeExpr(extEnv,f)(b)
    }
    
    case Match(v,alts) => {
      if (!(env inScope v)) {
        err undefVar (f, expr, v)
        return
      }
      analyzeAlts(env, f, expr, alts)
    }
  }  
 
  // Validate a function
  private def analyzeFun(env:Env)(f:FunDecl) {
    val rep = repeated(f.args)
    if (!(rep isEmpty)) err nonLinearParams (f,rep)
    analyzeExpr(env addLocals(f.args), f)(f.body)
  }   
}

/**
 * Associated static object for the static analyzer. Provides a 
 * convenient entry point for triggering the analysis.
 * 
 * @author ppedemon
 */
object StaticAnalysis {
  /**
   * Do static analysis on the given module.
   */
  def analyze(m:Module) = new StaticAnalysis(m).analyze
}
