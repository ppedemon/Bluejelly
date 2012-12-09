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
  
  def dupExtern(e:ExtDecl, prev:ExtDecl) {
    val doc = gnest(
      gnest("duplicated extern declaration" :/: quote(e.n) :/: "at:" :/: text(e.pos.toString)) :/: 
      gnest("(previous declaration was at:" :/: prev.pos.toString :: text(")")))
    error(ppr(doc))
  }
  
  def localExtern(e:ExtDecl) {
    val doc = gnest("extern declaration for local function" :/: quote(e.n) 
      :/: "at:" :/: text(e.pos.toString))
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
  
  def unsaturatedPat(f:FunDecl, p:Pat, c:ConRef, over:Boolean) {
    val s = "%ssaturated constructor" format (if (over) "over" else "un")
    wrongPat(f, p, gnest(s :/: text(quote(c))))
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
      act(ppr(doc))
  }
  
  def wrongExpr = genExprMsg(error)_
  def fishyExpr = genExprMsg(warning)_
  
  def undefDataCon(f:FunDecl, expr:Expr, c:ConRef) {
    wrongExpr(f, expr, gnest("undefined data constructor" :/: text(quote(c))))
  }
  
  def unsaturatedDataCon(f:FunDecl, expr:Expr, c:ConRef, over:Boolean) {
    val s = "%ssaturated constructor" format (if (over) "over" else "un")
    wrongExpr(f, expr, gnest(s :/: text(quote(c))))
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
  
  def invalidCon(f:FunDecl, parent:Expr, expr:Expr) {
    wrongExpr(f, parent, 
        gnest("zero-ary constructor" :/: PrettyPrinter.ppr(expr) :/: 
          text("is not a recursive expression")))
  }
  
  def undefVar(f:FunDecl, expr:Expr, v:Var) {
    wrongExpr(f, expr, gnest("undefined variable" :/: text(quote(v))))
  }

  def multipleDefaults(f:FunDecl, expr:Expr, alts:List[Alt]) {
    val a = (alts.tail foldLeft (PrettyPrinter.pprAlt(alts.head)))((d,a) => 
      d :/: group("and:" :/: PrettyPrinter.pprAlt(a)))
    val d = gnest(text("mutltiple default alternatives:") :/: group(a))
    wrongExpr(f, expr, d)
  }

  def ambiguousMatch(f:FunDecl, expr:Expr) {
    val d = text("Ambiguous match expression")
    wrongExpr(f, expr, d)
  }
  
  def wrongType(f:FunDecl, expr:Expr, alt:Alt) {
    val d = gnest("incompatible alternative:" :/: PrettyPrinter.pprAlt(alt))
    wrongExpr(f, expr, d)
  }
  
  def duplicatedAlt(f:FunDecl, expr:Expr, alt:Alt) {
    val d = gnest("duplicated alternative:" :/: PrettyPrinter.pprAlt(alt))
    fishyExpr(f, expr, d)
  }
  
  def unreachableAlts(f:FunDecl, expr:Expr, alt:Alt) {
    val d = gnest("unreachable alternatives after:" :/: PrettyPrinter.pprAlt(alt))
    fishyExpr(f, expr, d)
  }
  
  def undefExt(f:FunDecl, expr:Expr, v:Var) {
    val d = gnest("undeclared extern" :/: text(quote(v))) :/: 
      text("(the compiler will generate suboptimal code for this call)")
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

  // Check for over or unsaturated constructors
  private def arity(env:Env, c:ConRef):Int = env(c).con.arity

  // Get repeated elements in the given list. 
  // If every element is unique, return Nil.
  private def repeated[T](xs:List[T]):List[T] = xs diff (xs distinct)
  
  // Is the given expression a literal?
  private def isLit(expr:Expr):Boolean = expr match {
    case ELit(_) => true
    case Note(_,e) => isLit(e)
    case _ => false
  }
  
  // Id the given expression a zero-ary constructor?
  private def isZCon(expr:Expr):Boolean = expr match {
    case ECon(c,args) if args.length == 0 => true
    case Note(_,e) => isZCon(e)
    case _ => false
  }
  
  // Check that all the expressions in es are valid "recursive" expressions
  // E.g., literals and zero-ary constructors can be recursive
  private def allRec(f:FunDecl, parent:Expr, es:List[Expr]):Boolean = es match {
    case Nil => true
    case expr::es if isLit(expr) => err invalidLit(f, parent, expr); false
    case expr::ex if isZCon(expr) => err invalidCon(f, parent, expr); false
    case _::es => allRec(f, parent, es)
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
    case expr::es if isAtom(expr) => allAtoms (f, parent, es)
    case expr::_ => err nonAtomicExpr (f, parent, expr); false
  }
  
  // Collect top-level declarations, checking for duplicates
  private def collectDecls(m:Module):Env = {
    (Env(m.n) /: m.decls) ((env,d) => d match {
      case d@DataDecl(c,_) if env hasDataCon c => err dupDataCon (d,env(c)); env
      case d@DataDecl(_,_) => env addDataCon d
      case e@ExtDecl(v,_) if env hasExtern(v) => err dupExtern(e,env ext v); env
      case e@ExtDecl(v,_) if env isLocalId(v) => err localExtern(e); env
      case e@ExtDecl(_,_) => env addExtern(e)
      case f@FunDecl(v,_,_) if env hasFun v => err dupFun(f, env(v)); env
      case f@FunDecl(_,_,_) => env addFun f   
    })
  }

  // Check if the given path is valid
  private def analyzePat(env:Env, f:FunDecl)(p:Pat):(Env,Boolean) = p match {
    case PLit(_) => (env,true)
    case PVar(v) => (env addLocal v,true)
    case PCon(c,_) if !(env hasDataCon c) => err undefPat (f,p,c); (env,false)
    case PCon(c,args) if args.length != arity(env,c) => {
      val a = arity(env,c)
      err unsaturatedPat (f, p, c, args.length > a)
      (env,false) 
    }
    case PCon(c,vs) => repeated(vs) match {
      case Nil => (env addLocals vs,true)
      case vs => err nonLinearPat (f,p,vs); (env,false)
    }
  }
  
  // Analyze an application
  private def analyzeApp(env:Env, f:FunDecl, parent:Expr, fun:Var, args:List[Expr]) {
    if (env.isLocalId(fun) && !env.inScope(fun)) err undefVar (f, parent, fun)
    if (!env.isLocalId(fun) && !env.hasExtern(fun)) err undefExt(f, parent, fun)
    val argsOk = allAtoms(f, parent, args)
    // Treat arg variables specially, so we get better error messages
    if (argsOk) args foreach {
      case App(v,Nil) => analyzeApp(env, f, parent, v,Nil)
      case expr => analyzeExpr(env,f)(expr)
    }
  }
  
  // Check that there is at most one default alternative
  private def defOk(f:FunDecl, expr:Expr, alts:List[Alt]):Boolean = {
    val (vs,as) = alts.partition(_.isVarAlt)
    if (vs.length > 1) err multipleDefaults(f, expr, vs)
    if (as.isEmpty) err ambiguousMatch(f,expr)
    vs.length <= 1
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
        case PCon(c,_) if (env hasDataCon c) && (values contains c) => 
          err duplicatedAlt(f, expr, a); false
        case PCon(c,_) => check(as, values + c)
        case PLit(x) if values contains Lit.value(x) => 
          err duplicatedAlt(f, expr, a); false
        case PLit(x) => check(as, values + Lit.value(x))
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
    val (extEnv,patOk) = analyzePat(env, f)(alt.p)
    if (patOk) analyzeExpr(extEnv, f)(alt.e)
  }

  // Analyze a list of case alternatives
  private def analyzeAlts(env:Env, f:FunDecl, expr:Expr, alts:List[Alt]) {
    val dOk = defOk(f, expr, alts)
    if (!dOk) return 
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
      if (!(env hasDataCon c)) { err undefDataCon (f,expr,c); return }
      val a = arity(env,c)
      if (args.length != a) err unsaturatedDataCon(f, expr, c, args.length > a)
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
      if (!allRec(f,expr,es)) return
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
