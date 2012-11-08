/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import bluejelly.utils.Name

/**
 * Compilation environment.
 * @author ppedemon
 */
class Env(
    val mname:Name, 
    val ddecls:Map[ConRef, DataDecl], 
    val fdecls:Map[Var, FunDecl]) {
  
  // Is the given Id referring to the current module?
  def isLocal(id:Id) = 
    !(id.n isQual) || (id.n qualEquals mname.name)
  
  // Unqualify a local data constructor reference (if necessary)
  private def unqualLocal(c:ConRef) = {
    assert(isLocal(c))
    if (c.n isQual) new ConRef(Name(c.n.name)) else c
  }
  
  // Qualify a local data constructor reference (if necessary)
  private def qualLocal(c:ConRef) = {
    assert(isLocal(c))
    if (c.n isQual) c else new ConRef(Name(mname.name, c.n.name))
  }
  
  // Add a data declaration to the environment under two locations:
  // qualified and unqualified name.
  def addDataCon(d:DataDecl):Env = {
    val c = d.ref
    val decls = 
      if (isLocal(c)) ddecls ++ List(unqualLocal(c)->d, qualLocal(c)->d)
      else ddecls + (c->d)
    new Env(mname, decls, fdecls)
  }
  
  // Add a function declaration to the environment under two locations:
  // qualified and unqualified name.  
  def addFun(f:FunDecl):Env = {
    val q = new Var(Name(mname.name, f.n.n.name))
    new Env(mname, ddecls, fdecls ++ List(f.n->f, q->f))
  }
  
  def hasDataCon(c:ConRef) = ddecls isDefinedAt c
  def hasFun(v:Var) = fdecls isDefinedAt v
  
  def apply(c:ConRef) = ddecls(c)
  def apply(v:Var) = fdecls(v)
}

object Env {
  def apply(moduleName:Name):Env = {
    new Env(moduleName, Map(), Map())
  }
}
