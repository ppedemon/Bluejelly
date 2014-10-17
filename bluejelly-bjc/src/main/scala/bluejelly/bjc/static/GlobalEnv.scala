/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.static

import bluejelly.bjc.ast.module.ImpDecl
import bluejelly.bjc.common.Name
import bluejelly.bjc.core.BjcEnv

class GlobalEnvEntry(
    val qname:Name,       // *Qualified* name referred by this entry
    val impQual:Boolean,  // Is the entry imported as a qualified import?
    val impAs:Name,       // `as' qualified for the import, default to module name
    val imp:ImpDecl) {

  override def equals(other:Any) = other match {
    case e:GlobalEnvEntry => 
      e.qname == qname && e.impAs == impAs && e.impQual == impQual
    case _ => false
  }
  
  override def hashCode() = 
    31*(31*qname.hashCode + impAs.hashCode) + impQual.hashCode

  override def toString() = 
    "(%s%s) -> %s" format (if (impQual) "*" else "", impAs, qname)
}

class GlobalEnv(
    val bjcEnv:BjcEnv,
    val ntab:Map[Name,Set[GlobalEnvEntry]] = Map.empty) {
}
