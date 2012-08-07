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
  import bluejelly.utils.States._
  
  // State for collecting names and detecting duplicate declarations
  type E = Queue[ErrorItem]
  
  // Simple state for validating dictionaries and instructions:
  //  E: error queue where we will accumulate errors as we traverse a module
  //  1st Set[String]: namespace for declared dictionaries
  //  2nd Set[String]: namespace for declared functions
  type T = (E,Set[String],Set[String])
  
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
  
  // Collect dictionary and function names
  private def collect():State[E,(Set[String],Set[String])] = {
     def dictDup(s:String)(e:E) = 
       new ErrorItem("module", m.name, "duplicated dictionary " + quote(s)) +: e
     def funDup(s:String)(e:E) = 
       new ErrorItem("module", m.name, "duplicated function " + quote(s)) +: e
     for {
       fs <- collectNames(m.funcs.map(_.name), funDup)
       ds <- collectNames(m.dicts.map(_.name), dictDup)
     } yield (ds,fs)
  }

  /*
   * Separate an identifier in qualifier and local id. If the id is 
   * unqualified, return module's name.
   */
  private def unqual(s:String):(String,String) = {
    val ix = s lastIndexOf '.'
    if (ix == -1) (m.name,s) else (s substring (0,ix), s substring (ix+1))
  } 
  
  // Is the given name a valid dictionary reference?
  private def validateDictRef(d:String):State[T,Boolean] = {
    val (q,id) = unqual(d)
    if (q != m.name) ret(true) else for {st <- get} yield (st._2 contains id)
  }

  // Is the given name a valid function reference?
  private def validateFunRef(d:String):State[T,Boolean] = {
    val (q,id) = unqual(d)
    if (q != m.name) ret(true) else for {st <- get} yield (st._3 contains id)
  }

  // Handle an invalid reference to a method or a 
  // dictionary in the dictionary named [dictName]
  private def invalidRefInDict(
      dictName:String, 
      what:String, 
      refName:String):State[T,Unit] = {
    val msg = "invalid %s reference `%s'" format (what,refName)
    val item = new ErrorItem("dictionary", dictName, msg)
    upd {case (e,ds,fs) => (item +: e,ds,fs)}
  }

  // Handle an invalid reference to a function or 
  // a dictionary in the function named [funName]
  private def invalidRefInFun(
      funName:String, 
      what:String, 
      refName:String):State[T,Unit] = {
    val msg = "invalid %s reference `%s'" format (what,refName)
    val item = new ErrorItem("function", funName, msg)
    upd {case (e,ds,fs) => (item +: e,ds,fs)}    
  }
  
  // Is some dictionary component in a dictionary named [dictName] valid? 
  private def validateDictComponent(
      dictName:String,
      what:String,
      ns:List[String],
      v:String => State[T,Boolean]):State[T,Boolean] = ns match {
    case List() => ret(true)
    case n::ns  => for {
      restOk <- validateDictComponent(dictName,what,ns,v)
      thisOk <- v(n)
      _ <- (if (thisOk) nop else invalidRefInDict(dictName,what,n))
    } yield (thisOk && restOk)
  }
  
  // Is the given dictionary valid?
  private def validateDict(d:Dictionary):State[T,Boolean] = {
    for {
      specsOk   <- validateDictComponent(d.name, "specific", d.specifics, validateDictRef)
      methodsOk <- validateDictComponent(d.name, "method", d.methods, validateFunRef)
      supersOk  <- validateDictComponent(d.name, "super", d.supers, validateDictRef)
    } yield (supersOk && methodsOk && specsOk)
  }
  
  // Validate all dictionaries in a module
  private def validateDicts(ds:List[Dictionary]):State[T,Boolean] = ds match {
    case List() => ret(true)
    case d::ds  => for {
      restOk <- validateDicts(ds)
      thisOk <- validateDict(d)
    } yield (thisOk && restOk)
  }
  
  // Validate the reference [refName] found inside function [funName]
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
    case PushDict(dictId)
      => validateInstrRef(funName, "dictionary", dictId, validateDictRef)
    case JumpMethod(dictId,_) 
      => validateInstrRef(funName, "dictionary", dictId, validateDictRef)
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
      for {_ <- upd {s:T => (item +: s._1,s._2,s._3)}} yield false
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
    val ((ds,fs),e) = collect()(Queue())
    if (!(e isEmpty)) return (false,e)
    
    val st = (e,ds,fs)
    val (ok1,(e1,_,_)) = validateDicts(m.dicts)(st)    
    val (ok2,(e2,_,_)) = validateFuns(m.funcs)(st)
    return (ok1 && ok2, e1 ++ e2)
  }
}
