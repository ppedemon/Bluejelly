/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4
import bluejelly.utils.Name

/**
 * No-arg exception signaling a compiler error.
 */
case class CompilerException extends Exception

/**
 * Compile a L4 module to an Assembler module.
 * @author ppedemon
 */
class L4ToAsm(val m:Module) {
  
  // ---------------------------------------------------------------------
  // Error handling
  // ---------------------------------------------------------------------
  
  var hasErrors = false
  
  @throws(classOf[CompilerException])
  def abort() { throw new CompilerException }
  
  def warning(msg:String) { System.err.println("warning: " + msg) }
  def error(msg:String)   { hasErrors = true; System.err.println("error: " + msg) }
  
  @throws(classOf[CompilerException])
  def fatal(msg:String)   { error(msg); abort() }
  
  // ---------------------------------------------------------------------
  // Name utilities
  // ---------------------------------------------------------------------
  
  def isLocal(id:Id) = !(id.n isQual) || (id.n qualEquals m.n.name)
 
  def unqualLocal(c:ConRef) = {
    assert(isLocal(c))
    if (c.n isQual) new ConRef(Name(c.n.name)) else c
  }
  
  def qualLocal(c:ConRef) = {
    assert(isLocal(c))
    if (c.n isQual) c else new ConRef(Name(m.n.name, c.n.name))
  }
  
  // ---------------------------------------------------------------------
  // Collect data constructor declarations, check for repetitions
  // ---------------------------------------------------------------------

  def isDataConDeclared(c:ConRef, map:Map[ConRef,ConDef]) = 
    if (isLocal(c)) (map contains qualLocal(c)) || (map contains unqualLocal(c)) 
    else (map isDefinedAt c)
  
  def addDataConDecl(c:ConRef, d:ConDef, map:Map[ConRef,ConDef]) = 
    if (isLocal(c)) map ++ List(unqualLocal(c) -> d, qualLocal(c) -> d)
    else map + (c->d)
      
  def dataDecls(m:Module):Map[ConRef,ConDef] = {
    (Map[ConRef,ConDef]() /: m.decls) ((m,d) => d match {
      case FunDecl(_,_,_) => m
      case DataDecl(c,d) => if (isDataConDeclared(c,m)) {
        error("Duplicated data constructor declaration: '%s'" format c); m
      } else addDataConDecl(c,d,m)
    })
  }

  def compile = dataDecls(m)

}