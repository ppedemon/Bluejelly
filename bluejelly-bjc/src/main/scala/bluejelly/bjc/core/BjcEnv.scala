/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.{UnitCon,TupleCon,ArrowCon,ListCon,Con}
import bluejelly.bjc.common.{Name,QualName}
import bluejelly.bjc.common.ScopedName

import scala.collection.mutable.{Map => MutableMap}

/**
 * The compiler environment. Everything of interest
 * during compilation is held in this class.
 *
 * @author ppedemon
 */
class BjcEnv(
  val modName: Symbol,
  val bjcErrors:BjcErrors,
  private val modLoader:ModuleLoader,
  private val modEnv:MutableMap[Symbol,ModDefn] = MutableMap.empty,
  private val globalInstEnv:MutableMap[QualName,List[Inst]] = MutableMap.empty) {

  @throws[LoaderException]
  def load(modName:Symbol) {
    if (!hasModDefn(modName)) {
      val modDefn = modLoader.load(modName)
      addModDefn(modDefn)    
    }
  }

  private def addModDefn(modDefn:ModDefn) {
    modEnv += (modDefn.name -> modDefn)
    modDefn.insts.foreach {inst => 
      globalInstEnv += (inst.clsName -> (inst::globalInstEnv.get(inst.clsName).getOrElse(Nil)))
    }
  } 
  
  def hasModDefn(modName:Symbol) = modEnv.contains(modName)
  def getModDefn(modName:Symbol) = {
    if (!hasModDefn(modName)) load(modName)
    modEnv(modName)
  }

  private def getModDecl[T <: ModDecl](
      extractor:PartialFunction[(ModDefn,ScopedName),T]):PartialFunction[QualName,T] = {
    case n => 
      val (q,s) = (n.qual, n.name)
      extractor((getModDefn(q),s))
  }

  def getId:PartialFunction[QualName,Id] = 
    getModDecl({case (m,n) => m.getId(n)})
  def getTyCon:PartialFunction[QualName,TyCon] = 
    getModDecl({case (m,n) => m.getTyCon(n)})
  def getTySyn:PartialFunction[QualName,TySyn] = 
    getModDecl({case (m,n) => m.getTySyn(n)})
  def getCls:PartialFunction[QualName,Cls] = 
    getModDecl({case (m,n) => m.getCls(n)})
  def getDataCon:PartialFunction[QualName,DataCon] = 
    getModDecl({case (m,n) => m.getDataCon(n)})
}

object BjcEnv {
  def apply(modName:Symbol, bjcErrors:BjcErrors, modLoader:ModuleLoader) = {
    val currMod = new ModDefn(modName)
    val env = new BjcEnv(modName, bjcErrors, modLoader)
    env.addModDefn(BuiltIns.wiredInMod)
    env.addModDefn(currMod)
    BuiltIns.primMods.foreach(env.addModDefn(_))
    env
  }
}
