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
import bluejelly.bjc.iface.{IfaceExport,ExportedId,ExportedTc}

/**
 * Name entry. This class allows to map a name in a module
 * to an imported name.
 *
 * @author ppedemon
 */
class NameEntry(
    val qname:Name,       // *Qualified* name referred by this entry
    val impQual:Boolean,  // Is the entry imported as a qualified import?
    val impAs:Name,       // `as' qualified for the import, default to module name
    val imp:ImpDecl) {

  override def equals(other:Any) = other match {
    case e:NameEntry => e.qname == qname && e.impAs == impAs && 
      e.impQual == impQual
    case _ => false
  }
  
  override def hashCode() = 
    31*(31*qname.hashCode + impAs.hashCode) + impQual.hashCode

  override def toString() = 
    "(%s%s) -> %s" format (if (impQual) "*" else "", impAs, qname)
}

/**
 * Convenience [[GlobalEnv]] object.
 * @author ppedemon
 */
object GlobalEnv {
  type NameTab = Map[Name,Set[NameEntry]]
}

import GlobalEnv.NameTab

/**
 * Global environment. This table holds a [[BjcEnv]] with the
 * module definitions for the interfaces imported by the module
 * being compiled, and a table (the NameTab) allowing to solve
 * names occuring in current module.
 *
 * @author ppedemon
 */
class GlobalEnv(val bjcEnv:BjcEnv, val nameTab:GlobalEnv.NameTab = Map.empty) {

  def grow(bjcEnv:BjcEnv, exps:List[IfaceExport], imp:ImpDecl) = {
    val n_nameTab = exps.foldLeft(nameTab)((nameTab,e) => e match {
      case ExportedId(n) => 
        addToNameTab(nameTab, nameEntry(n, imp))
      case ExportedTc(_,ns) => 
        ns.foldLeft(nameTab)((nameTab,n) => 
          addToNameTab(nameTab,nameEntry(n,imp)))
    })
    new GlobalEnv(bjcEnv, n_nameTab)
  }

  override def toString = nameTab.toString

  private def addToNameTab(nameTab:NameTab, entry:NameEntry) = {
    val uName = entry.qname.unqualify
    nameTab + (uName -> (nameTab.get(uName).getOrElse(Set.empty) + entry))
  }

  private def nameEntry(qname:Name, imp:ImpDecl) = 
    new NameEntry(
      qname, 
      imp.qualified, 
      imp.alias.getOrElse(imp.modId), 
      imp)
}
