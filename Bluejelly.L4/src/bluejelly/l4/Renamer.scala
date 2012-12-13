/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import bluejelly.utils.Name

/**
 * Rename variable names, eliminating variable shadowing. Why? Consider:
 * 
 * <code>
 * fun f x = 
 *   let y = h x in 
 *   let x = w 1 1 in
 *   g y x x
 * </code>
 * 
 * <p>Since y occurs only once the inliner will inline this binding, 
 * thus getting:
 * 
 * <code>fun x = let x = w 1 1 in g (h x) x x</code>
 * 
 * <p>As you can see, the argument to h was captured by the let. Even 
 * though the inliner could rename before inlining let bindings, it's 
 * simpler to just eliminate variable shadowing in a separate phase, so 
 * the inliner can can happily process let bindings without complicating 
 * the inlining code.
 * 
 * <p>The renaming phase will transform f into the following definition:
 * 
 *  <code>
 *  fun f x = 
 *    let y = h x in
 *    let x#1 = w 1 1 in
 *    g y x#1 x#1
 *  </code>
 * 
 * <p>And now, we can safely inline without worrying about variable capture:
 * 
 * <code>fun x = let x#1 = w 1 1 in g (h x) x#1 x#1</code>
 * 
 * @author ppedemon
 */
class Renamer {
  
  private type E = Map[Var,Var]
  
  private var n = 0
  
  def rename(m:Module):Module = new Module(m.n, m.decls map renameDecl)
  
  private def renameDecl(d:Decl):Decl = d match {
    case DataDecl(_,_) => d
    case ExtDecl(_,_) => d
    case f@FunDecl(_,_,_) => renameFun(f)
  }
  
  private def renameFun(f:FunDecl):FunDecl = {
    n = 0
    val env = (f.args foldLeft (Map():E))((e,v) => e + (v->v))
    new FunDecl(f.n, f.args, rename(env)(f.body))
  }
  
  // Variable generated here is guaranteed to be unique (function scope).
  // Note that the character '#' can't be part of an identified accepted by
  // the parser, so appending "#n" (for non-repeating n) has to generate a
  // unique variable inside a function
   private def fresh(v:Var) = {
    val name = Name(v.n.toString + "#" + n)
    n = n+1
    new Var(name)
  }
  
  private def subst(env:E,v:Var) = 
    (env orElse ({case v=>v}:PartialFunction[Var,Var]))(v)
  
  // Rename expressions: real work done here
  private def rename(env:E)(e:Expr):Expr = e match {
    // Basic cases
    case ELit(_) => e
    case ECon(c,args) => ECon(c, args map rename(env))
    case Note(occ,e) => Note(occ, rename(env)(e))
    
    // Here we substitute variables
    case App(v,args) => App(subst(env,v), args map rename(env))
    case NApp(v,args) => NApp(subst(env,v), args map rename(env))
    
    // Let and eval expressions might shadow variables
    case Let(x,e,b) if env isDefinedAt x =>
      val freshv = fresh(x)
      val extEnv = env + (x->freshv)
      Let(freshv, rename(env)(e), rename(extEnv)(b))
    case Let(x,e,b) => 
      Let(x, rename(env)(e), rename(env + (x->x))(b))
    
    case Eval(x,e,b) if env isDefinedAt x =>
      val freshv = fresh(x)
      val extEnv = env + (x->freshv)
      Eval(freshv, rename(env)(e), rename(extEnv)(b))
    case Eval(x,e,b) => 
      Let(x, rename(env)(e), rename(env + (x->x))(b))
    
    // Each declaration in a let rec might have to be renamed
    case LetRec(decls,e) =>
      val (vs,es) = decls unzip
      val extEnv = (vs foldLeft env) {
        case (e,v) if e isDefinedAt v => e + (v -> fresh(v))
        case (e,v) => e + (v->v)
      }
      var vs1 = vs map {subst(extEnv,_)}
      val es1 = es map rename(extEnv)
      LetRec(vs1 zip es1, rename(extEnv)(e))
    
    // Match: pay attention to renames in the matched var, traverse alts
    case Match(v,alts) => Match(subst(env,v), alts map renameAlt(env))
  }

  // Rename alternatives
  private def renameAlt(env:E)(alt:Alt) = alt.p match {
    case PLit(_) => new Alt(alt.p, rename(env)(alt.e))
    
    case PVar(v) if env isDefinedAt v =>
      val freshv = fresh(v)
      val extEnv = env + (v->freshv)
      new Alt(PVar(freshv),rename(extEnv)(alt.e))
    case p@PVar(_) => new Alt(p, rename(env)(alt.e))
    
    case PCon(c,vs) =>
      val extEnv = (vs foldLeft env) {
        case (e,v) if e isDefinedAt v => e + (v -> fresh(v))
        case (e,v) => e + (v->v)
      }
      var vs1 = vs map {subst(extEnv,_)}
      new Alt(PCon(c,vs1), rename(extEnv)(alt.e))
  }
}

object Renamer {
  def rename(m:Module) = new Renamer rename m
}
