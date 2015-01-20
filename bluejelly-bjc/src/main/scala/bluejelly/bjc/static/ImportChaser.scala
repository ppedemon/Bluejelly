/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.static

import bluejelly.bjc.ast.module
import bluejelly.bjc.common.{Name,LocalName,QualName,ScopedName}
import bluejelly.bjc.common.{ExportInfo,ExportedId,ExportedTc}
import bluejelly.bjc.core.{ModuleLoader,LoaderException,BjcEnv,BjcErrors,BuiltIns}

// For fake positions in added imports
import scala.util.parsing.input.Position

/**
 * Chase imports in some module. Use a designated loader for 
 * loding module interfaces into a [[BjcEnv]] (so we can test
 * this class by plugging in a test loader) and dump errors to
 * some [[BjcErrors]] instance.
 *
 * @author ppedemon
 */
class ImportChaser(modLoader:ModuleLoader, errors:BjcErrors) {

  /**
   * Chase imports for the given module.

   * @return a [[GlobalEnv]] with the visible exports resulting
   * from processing the import declarations in the module
   */
  def chaseImports(bjcEnv:BjcEnv, mod:module.Module):(BjcEnv,NameTable) = {
    val allImpDecls = addBuiltIns(mod.impDecls)
    allImpDecls.foldLeft((bjcEnv,new NameTable())){
      case ((bjcEnv,nt),imp) => 
      val (n_bjcEnv,exps) = chaseOne(bjcEnv, imp)
      val n_nt = nt.grow(exps,imp)
      (n_bjcEnv, n_nt)
    }
  }

  // Create fake import declarations for built-in stuff
  private def createImpDecl(modName:Symbol) = {
    val i = new module.ImpDecl(modName,false,None,module.ImportAll)
    i.pos = new Position {
      val line = 1
      val column = 1
      lazy val lineContents = i.toString
    }
    i
  }  

  // Add wired-in module and necessary primitive stuff to 
  // module's import declaration
  private def addBuiltIns(imps:List[module.ImpDecl]) = {
    var allBuiltIns = BuiltIns.wiredInModName::(BuiltIns.primMods map (_.name)) 
    val diff = allBuiltIns diff (imps map (_.modId))
    (diff map createImpDecl) ++ imps
  }

  // Chase a single import declaration.
  private def chaseOne(
      env:BjcEnv,
      imp:module.ImpDecl):(BjcEnv,List[ExportInfo]) = {
    try {
      val n_env = modLoader.load(env, imp.modId)
      val exps = visibleExports(imp, n_env.getModDefn(imp.modId).exports)
      (n_env, normalizeExports(exps))
    } catch {
      case e:LoaderException => 
        errors.ifaceLoadError(imp, e.msg)
        (env,List.empty)
    }
  }

  // Normalize exports: Either{Left} + Either{Right} = Either{Left,Right}
  private def normalizeExports(exps:List[ExportInfo]) = 
    exps.foldLeft(Map.empty[QualName,ExportInfo])((m,e) => (e,m.get(e.name)) match {
      case (ExportedTc(n,ns),Some(ExportedTc(_,cs))) =>
        m + (n -> ExportedTc(n, (cs ++ ns).distinct))
      case _ => m + (e.name -> e)
    }).values.toList

  // Given a `universe' of exports and a import declaration, compute
  // the subset of the universe corresponding to the given import.
  private def visibleExports(
      imp:module.ImpDecl,
      exports:List[ExportInfo]) = {
    // This assumes that ExportedTc's are grouped by constructor name
    // That is, no `module Test(C(f),C(g))' but module `Test(C(f,g))'
    lazy val exps = exports.foldLeft(Map.empty[LocalName,ExportInfo])((m,e) => 
      m + (e.name.unqualify -> e))
    imp.imports match {
      case module.ImportAll => 
        exports
      case module.ImportSome(is) => 
        val n_is = normalizeISpecs(is) map {case (i,_) => i}
        n_is.foldLeft(List.empty[ExportInfo])((es,i) => select(imp,exps,i) match {
          case Some(e) => e::es
          case None => es
        })
      case module.HideSome(is) =>
        val n_is = normalizeISpecs(is)
        n_is.foldLeft(exps)(hide(imp,_,_)).values.toList
    }
  }

  // Normalize specs: for example, normalizing Either(Left) 
  // and Either(Right) gives Either(Left,Right)
  private def normalizeISpecs(
      ispecs:List[module.ISpec]):List[(module.ISpec,Boolean)] = {
    val map = Map.empty[Name,(module.ISpec,Boolean)]
    val map1 = ispecs.foldLeft(map)((m,i) => if (!m.contains(i.name)) 
      m + (i.name -> (i,getDConFlag(i)))
    else {
      val (j,dconFlag) = m(i.name)
      m + (i.name -> combine(i,j,dconFlag))
    })
    map1.values.toList
  }

  // Get DCon flag: this flag is set to true iif Ispec is INone(c), meaning 
  // that in a hiding import the ispec applies to data constructors as well
  private def getDConFlag(ispec:module.ISpec) = ispec match {
    case module.INone(_) => true
    case _ => false
  }

  // Combine two import specs, keeping track of the current
  // DCon flag for the current constructor name
  private def combine(
      i:module.ISpec,
      j:module.ISpec,
      dconFlag:Boolean) = i match {
    case x@module.IAll(_) => (x,dconFlag)
    case module.INone(c) => (j,true)
    case x@module.ISome(c,cs) => j match {
      case module.IAll(_) => (j,dconFlag)
      case module.INone(_) => (x,true)
      case module.ISome(_,ns) => (module.ISome(c, (cs ++ ns).distinct), dconFlag)
      case module.IVar(_) => (j,dconFlag)  // Impossible
    }
    case module.IVar(_) => (j,dconFlag)
  }

  // Given a map of module exports, compute those 
  // selected by the given [[ISpec]]
  private def select(
      imp:module.ImpDecl, 
      exps:Map[LocalName,ExportInfo],
      i:module.ISpec) = i match {
    case module.IAll(c) => exportForCon(c,exps) match {
      case e@Some(_) => e
      case _ => err(imp, i)
    }
    case module.INone(c) => exportForCon(c,exps) match {
      case Some(ExportedTc(n,_)) => Some(ExportedTc(n,List(n)))
      case _ => err(imp,i)
    }
    case module.ISome(c,cs) => exportForCon(c,exps) match {
      case Some(e@ExportedTc(_,_)) =>
        val n_exp = retain(c::cs,e)
        val ns = n_exp.children.tail.map(_.unqualify)
        if (ns != cs) err(imp,i) else Some(n_exp)
      case _ => err(imp,i)
    }
    case module.IVar(id) => findId(id,exps) match {
      case r@Some(_) => r
      case _ => err(imp, i)
    }
  }

  // Given a map of module exports, compute what's left after
  // taking into account a hiding [[ISpec]].
  private def hide(
      imp:module.ImpDecl,
      exps:Map[LocalName,ExportInfo],
      itup:(module.ISpec,Boolean)) = itup._1 match {
    case module.IAll(c) => 
      val n_exps = exportForCon(c,exps) match {
        case Some(_) => exps - c
        case None => exps
      }
      if (itup._2) removeCon(c,n_exps) else n_exps
    case module.ISome(c,cs) => 
      val n_exps = exportForCon(c,exps) match {
        case Some(e@ExportedTc(_,_)) =>
          val n_exp = remove(c::cs,e)
          if (n_exp.children.isEmpty) exps - c else exps + (c -> n_exp)
        case _ => exps
      }
      if (itup._2) removeCon(c,n_exps) else n_exps
    case module.INone(c) => removeCon(c,exps)
    case module.IVar(id) => removeId(id,exps)
  }

  // Find an export exporting some constructor name (and possibly 
  // some subordinate stuff)
  private def exportForCon(
       c:LocalName, 
       exps:Map[LocalName,ExportInfo]) = exps.get(c) match {
     case Some(e@ExportedTc(_,ns)) if e.exportsParent => Some(e)
     case _ => None
  }

  // Retrieve the fully qualified id corresponding to the
  // [[id]] in some map.
  private def findId(
      id:LocalName, 
      exps:Map[LocalName,ExportInfo]) = exps.get(id) match {
    case Some(e@ExportedId(_)) => Some(e)
    case Some(_) => None // Impossible
    case None => 
      val es = exps.values.filter {
        case ExportedTc(_,ns) => ns.map(_.unqualify) contains id
        case _ => false 
      }
      es match {
        case Nil => None
        case List(ExportedTc(c,ns)) => 
          val n = ns.dropWhile(_.unqualify != id).head
          Some(ExportedTc(c,List(n)))
      }
  }

  // Remove occurrences of constructor name [[c]] from an export map
  // Precondition: [[c]] belongs to the TcScope
  private def removeCon(c:LocalName, exps:Map[LocalName,ExportInfo]) = {
    exps.foldLeft(Map.empty[LocalName,ExportInfo])((m,kv) => {
      val k = kv._1
      kv._2 match {
        case e@ExportedId(_) => m + (k -> e)
        case e@ExportedTc(_,_) => 
          val n_exp = remove(List(c,Name.idName(c.toSymbol)), e)
          if (n_exp.children.isEmpty) m else m + (k -> n_exp)
      }
    })
  }

  // Remove occurrences of id names [[id]] from an export map
  private def removeId(id:LocalName, exps:Map[LocalName,ExportInfo]) = {
    exps.foldLeft(Map.empty[LocalName,ExportInfo])((m,kv) => {
      val k = kv._1
      kv._2 match {
        case ExportedId(n) if n.unqualify == id => m
        case e@ExportedTc(_,_) =>
          val n_exp = remove(List(id),e)
          if (n_exp.children.isEmpty) m else m + (k -> n_exp)
        case e => m + (k -> e)
      }
    })
  }

  // Retain only the given names from the children of a ExportedTc
  private def retain(names:List[LocalName], e:ExportedTc) = {
    val retained = e.children filter (n => names.contains(n.unqualify))
    ExportedTc(e.name, retained)
  }

  // Remove the given names from the children of a ExportedTc
  private def remove(names:List[LocalName], e:ExportedTc) = {
    val removed = e.children filterNot (n => {
      names.contains(n.unqualify)
    })
    ExportedTc(e.name, removed)
  }

  // Utility function for signaling an import error
  private def err(imp:module.ImpDecl, i:module.ISpec):Option[ExportInfo] = {
    errors.ifaceImportError(imp, i)
    None
  }
}
