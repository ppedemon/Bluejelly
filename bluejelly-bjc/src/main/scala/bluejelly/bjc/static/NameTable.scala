/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.static

import bluejelly.bjc.ast.module.ImpDecl
import bluejelly.bjc.common.Name
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.common.PprUtils.{pprPos,par}
import bluejelly.bjc.core.{BjcEnv,BuiltIns}
import bluejelly.bjc.iface.{IfaceExport,ExportedId,ExportedTc}

import bluejelly.utils.Document
import bluejelly.utils.Document._

import scala.util.parsing.input.Position

/**
 * Class hierarchy explaining why something is in scope.
 * @author ppedemon
 */
abstract class Origin extends PrettyPrintable
case object WiredInOrigin extends Origin {
  def ppr = text("(wired in)")
}
case class LocalOrigin(private val pos:Position) extends Origin {
  def ppr = group(par("defined at:" :/: pprPos(pos)))
}
case class ImportedOrigin(val imp:ImpDecl) extends Origin {
  def ppr = group(par(imp.ppr))
}

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
    val origin:Origin) {

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
 * Name table. This table maps unqualified in-scope names to their 
 * corresponding qualified names and import details explaining how
 * the name was brought into scope. As we analyze the module being
 * compiled, the table will also include the declarations found in
 * the module. 
 * 
 * The goal is to provide a way to determine is a name is in-scope 
 * and whether it is ambiguous.
 *
 * @author ppedemon
 */
class NameTable(val nameTab:Map[Name,Set[NameEntry]] = Map.empty) {

  def grow(exps:List[IfaceExport], imp:ImpDecl) = {
    val n_nameTab = exps.foldLeft(nameTab)((nameTab,e) => e match {
      case ExportedId(n) => 
        addToNameTab(nameTab, nameEntry(n, imp))
      case ExportedTc(_,ns) => 
        ns.foldLeft(nameTab)((nameTab,n) => 
          addToNameTab(nameTab,nameEntry(n,imp)))
    })
    new NameTable(n_nameTab)
  }

  def hasName(n:Name) = nameTab.contains(n)

  override def toString = nameTab.toString

  private def addToNameTab(nameTab:Map[Name,Set[NameEntry]], entry:NameEntry) = {
    val uName = entry.qname.unqualify
    nameTab + (uName -> (nameTab.get(uName).getOrElse(Set.empty) + entry))
  }

  private def nameEntry(qname:Name, imp:ImpDecl) = { 
    val origin = if (qname.qual.get == BuiltIns.wiredInModName) 
      WiredInOrigin else ImportedOrigin(imp)
    new NameEntry(
      qname, 
      imp.qualified, 
      imp.alias.getOrElse(imp.modId), 
      origin)
  }
}
