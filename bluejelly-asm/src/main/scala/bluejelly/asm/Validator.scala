/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm

import scala.collection.immutable.Queue
import java.io.PrintWriter

/**
 * An item describing an error found in a module.
 * @author ppedemon
 */
class ErrorItem(val what:String, val where:String, val msg:String) {
  override def toString = "In %s `%s': %s" format (what, where, msg)
}

/** 
 * Very simple validation routines for {@link Module} instances.
 * @author ppedemon
 */
class Validator(val m:Module) {

  import bluejelly.utils.State
  import bluejelly.utils.St._
  
  // State for collecting names and detecting duplicate declarations
  type E = Queue[ErrorItem]
  
  // Simple state for validating a module:
  //  E: error queue where we will accumulate errors as we traverse a module
  //  Set[String]: namespace for declared functions
  type T = (E,Set[String])
  
  // Nop over the T state
  private def nop:State[T,Unit] = upd(identity:T=>T)
  
  // Quote a string
  private def quote(s:String) = "`%s'" format s
    
  // Collect names from a list, checking for repeated stuff
  private def collectNames(
      ns:List[String],
      f:String => E => E):State[E,Set[String]] = ns match {
    case List() => ret(Set())
    case n::ns  => for {
      seen <- collectNames(ns,f)
      _ <- upd((e:E) => if (seen contains n) f(n)(e) else e)
    } yield (seen + n)
  }
  
  // Collect function names
  private def collect():State[E,Set[String]] = {
     def funDup(s:String)(e:E) = 
       new ErrorItem("module", m.name, "duplicated function " + quote(s)) +: e
     collectNames(m.funcs.map(_.name), funDup)
  }

  /*
   * Separate an identifier in qualifier and local id. If the id is 
   * unqualified, return module's name.
   */
  private def unqual(s:String):(String,String) = {
    val ix = s lastIndexOf '.'
    if (ix == -1) (m.name,s) else (s substring (0,ix), s substring (ix+1))
  } 
  
  // Is the given name a valid function reference?
  private def validateFunRef(d:String):State[T,Boolean] = {
    val (q,id) = unqual(d)
    if (q != m.name) ret(true) else for {st <- get} yield (st._2 contains id)
  }

  // Handle an invalid reference to a function in the function named [funName]
  private def invalidRefInFun(
      funName:String, 
      what:String, 
      refName:String):State[T,Unit] = {
    val msg = "invalid %s reference `%s'" format (what,refName)
    val item = new ErrorItem("function", funName, msg)
    upd {case (e,fs) => (item +: e,fs)}    
  }
    
  // Validate the reference [refName] found in an instruction 
  // inside function [funName] definition
  private def validateInstrRef(
      funName:String, 
      what:String, 
      refName:String,
      v:String => State[T,Boolean]):State[T,Boolean] = 
    for {
      ok <- v(refName)
      _  <- if (ok) nop else invalidRefInFun(funName,what,refName)
    } yield ok
    
  // Validate an instruction inside a function named [funName]
  private def validateInstr(funName:String, i:Instr):State[T,Boolean] = i match {
    case Jump(funId)
      => validateInstrRef(funName, "function", funId, validateFunRef)
    case EvalVar(_,funId) 
      => validateInstrRef(funName, "function", funId, validateFunRef)
    case PushCode(funId)  
      => validateInstrRef(funName, "function", funId, validateFunRef)
    case PushCont(funId)
      => validateInstrRef(funName, "function", funId, validateFunRef)
    case MatchCon(alts,mdef)
      => validateMatch(funName, alts, mdef)
    case MatchInt(alts,mdef)
      => validateMatch(funName, alts, mdef)
    case MatchDbl(alts,mdef)
      => validateMatch(funName, alts, mdef)
    case MatchChr(alts,mdef)
      => validateMatch(funName, alts, mdef)
    case _ => ret(true)
  }

  // Validate a match instruction body
  private def validateMatch[U](
      where:String,
      alts:List[Alt[U]], 
      mdef:Option[Block]):State[T,Boolean] = {
    if (repeatedAlts(alts)) {
      val item = new ErrorItem("function", where, "repeated values in case alternatives")
      for {_ <- upd {s:T => (item +: s._1,s._2)}} yield false
    } else {
      for {
        defltOk <- validateBlock(where, mdef map (_.is) getOrElse List())
        altsOk  <- validateAlts(where, alts)
      } yield (altsOk && defltOk)
    }
  }
  
  // Check for repeated alternatives in some alts
  private def repeatedAlts[U](alts:List[Alt[U]]):Boolean =
    (((Set[U](),false) /: alts) {
      case (p@(_,true),_) => p
      case ((seen,false),alt) if seen contains alt.v => (seen,true)
      case ((seen,_),alt) => (seen + alt.v,false)
    })._2
  
  // Validate a case alternatives
  private def validateAlts[U](
      where:String, 
      alts:List[Alt[U]]):State[T,Boolean] = alts match {
    case List() => ret(true)
    case i::is  => for {
      restOk <- validateAlts(where,is)
      thisOk <- validateBlock(where, i.b.is)
    } yield (thisOk && restOk)
  }
    
  // Validate a list of instructions (i.e., a block)
  private def validateBlock(where:String, is:List[Instr]):State[T,Boolean] = is match {
    case List() => ret(true)
    case i::is  => for {
      restOk <- validateBlock(where, is)
      thisOk <- validateInstr(where, i)
    } yield (thisOk && restOk)
  }
  
  // Validate a function
  private def validateFun(f:Function):State[T,Boolean] = 
    validateBlock(f.name, f.b.is)
  
  // Validate all functions in a module
  private def validateFuns(fs:List[Function]):State[T,Boolean] = fs match {
    case List() => ret(true)
    case f::fs  => for {
      restOk <- validateFuns(fs)
      thisOk <- validateFun(f)
    } yield (thisOk && restOk)
  }
    
  /**
   * Dump the given errors to some writer sink.
   * @param e    errors to dump
   * @param w    writer sink where errors will be dumped
   */
  def dumpErrs(e:E, w:PrintWriter) { e foreach (w println _); w flush }

  /**
   * Validate the given module.
   * 
   * @return a pair consisting a flag indicating whether validation
   * succeeded, and a queue of error items (empty on successful validation)
   */
  def validate():(Boolean,Queue[ErrorItem]) = {
    val (fs,e) = collect()(Queue())
    if (!(e isEmpty)) return (false,e)
    
    val st = (e,fs)
    val (ok,(e1,_)) = validateFuns(m.funcs)(st)
    return (ok, e1)
  }
}
