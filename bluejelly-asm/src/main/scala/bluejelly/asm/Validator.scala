/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm

import java.io.StringWriter

import scala.text.Document
import scala.text.Document.group
import scala.text.Document.nest
import scala.text.Document.text

import bluejelly.utils.Errors

/**
 * Custom error bag.
 * @author ppedemon
 */
class AsmErrors extends Errors(false) {
  
  private def quote[T](v:T):String = "`%s'" format v
  
  private def ppr(d:Document):String = {
    val s = new StringWriter
    d.format(75, s)
    s.flush()
    s.toString
  }
  
  private def gnest(d:Document):Document = group(nest(2,d))
  
  private def pprList[T](xs:List[T]):Document = text(xs mkString ("[",",","]"))
  
  def dupFunction(f:Function, prev:Function) {
    val doc = gnest(
      gnest("duplicated function declaration" :/: quote(f.name) :/: "at:" :/: text(f.pos.toString)) :/: 
      gnest("(previous declaration was at:" :/: prev.pos.toString :: text(")")))
    error(f.pos, ppr(doc))
  }
  
  def invalidRefInFun(f:Function, i:Instr, ref:String) {
    val doc = gnest(
      group("invalid function reference: " :/: text(quote(ref))) :/:
      gnest(
        group("in instruction:" :/: text(i.toString)) :/:
        group("at: " :/: text(i.pos.toString))) :/:
      gnest(
        group("in function:" :/: text(quote(f.name))) :/: 
        group("at:" :/: text(f.pos.toString))))
      error(f.pos, ppr(doc))
  }

  def repeatedAltsInFun[U](f:Function, i:Instr, vs:List[U]) {
    val v = if (vs.size == 1) "value" else "values"
    val msg = "repeated %s in case alternatives:" format v
    val doc = gnest(
      gnest(
        group(msg :/: pprList(vs)) :/:
        group("in match instruction at:" :/: text(i.pos.toString))) :/:
      gnest(
        group("in function:" :/: text(quote(f.name))) :/: 
        group("at:" :/: text(f.pos.toString))))
      error(f.pos, ppr(doc))
  }
}

/** 
 * Very simple validation routines for {@link Module} instances.
 * @author ppedemon
 */
class Validator(val m:Module) {

  private type Env = Map[String,Function]
  
  private val errors = new AsmErrors

  /*
   * Collect functions in the module to validate.
   */
  private def collectFunctions():Env = {
    (m.funcs foldLeft Map[String,Function]()){case (map,f) => 
      if (map.contains(f.name)) {
        errors.dupFunction(f, map(f.name))
        map
      } else {
        map + (f.name -> f)
      }
    }
  }
  
  /*
   * Separate an identifier in qualifier and local id. If the id is 
   * unqualified, return module's name.
   */
  private def unqual(s:String):(String,String) = {
    val ix = s lastIndexOf '.'
    if (ix == -1) (m.name,s) else (s substring (0,ix), s substring (ix+1))
  } 
  
  /*
   * Check whether the given reference is valid. References are valid
   * if they are external (referring to another module) or if they
   * refer to an existing local function.
   */
  private def validateInstrRef(env:Env,f:Function,i:Instr, ref:String) {
    val (q,id) = unqual(ref)
    if (q == m.name && !(env contains id))
      errors.invalidRefInFun(f, i, ref)
  }
    
  /*
   * Validate case alternatives.
   */
  private def validateAlts[U](env:Env,f:Function)(alts:List[Alt[U]]):Unit = 
    alts foreach {a => validateBlock(env,f)(a.b)}

  /*
   * Check for repeated alternatives in some alts. Return whether there
   * are repeated alternatives, and the list of repeated values in order
   * of appearance.
   */
  private def repeatedAlts[U](alts:List[Alt[U]]):(Boolean,List[U]) = {
    val (_,_,rep) = (alts foldLeft (Set[U](),Set[U](),List[U]())) {
      case (t@(_,rep,_),alt) if rep contains alt.v => t
      case ((seen,rep,repList),alt) if seen contains alt.v => 
        (seen,rep + alt.v, alt.v::repList)
      case ((seen,rep,repList),alt) => (seen + alt.v, rep, repList)
    }
    (!rep.isEmpty,rep.reverse)
  }

  /*
   * Validate a match instruction.
   */
  private def validateMatch[U](
      env:Env,
      f:Function,
      i:Instr,
      alts:List[Alt[U]],
      mdef:Option[Block]) {
    val (hasReps,reps) = repeatedAlts(alts)
    if (hasReps) {
      errors.repeatedAltsInFun(f, i, reps)
    } else {
      validateAlts(env,f)(alts)
      mdef foreach validateBlock(env,f)
    }
  }

  /*
   * Validate an instruction inside a function.
   */
  private def validateInstr(env:Env,f:Function)(i:Instr):Unit = i match {
    case Jump(funId)
      => validateInstrRef(env, f, i, funId)
    case EvalVar(_,funId) 
      => validateInstrRef(env, f, i, funId)
    case PushCode(funId)  
      => validateInstrRef(env, f, i, funId)
    case PushCont(funId)
      => validateInstrRef(env, f, i, funId)
    case MatchCon(alts,mdef)
      => validateMatch(env, f, i, alts, mdef)
    case MatchInt(alts,mdef)
      => validateMatch(env, f, i, alts, mdef)
    case MatchDbl(alts,mdef)
      => validateMatch(env, f, i, alts, mdef)
    case MatchChr(alts,mdef)
      => validateMatch(env, f, i, alts, mdef)
    case _ => ()
  }
    
  /*
   * Validate a block.
   */
  private def validateBlock(env:Env,f:Function)(b:Block):Unit = 
    b.is foreach validateInstr(env,f)
  
  /*
   * Validate all functions in a module.
   */
  private def validateFuns(env:Env,fs:List[Function]):Unit = 
    fs foreach {f => validateBlock(env,f)(f.b)}

  /**
   * Validate the given module.
   * @return Some[AsmErrors] if validation fails, Nothing otherwise.
   */
  def validate[T >: Errors]():Option[T] = {
    val env = collectFunctions()
    if (errors.hasErrors) return Some(errors)
    validateFuns(env, m.funcs)
    if (errors.hasErrors) Some(errors) else None
  }
}
