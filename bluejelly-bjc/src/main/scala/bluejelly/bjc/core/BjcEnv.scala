/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.{UnitCon,TupleCon,ArrowCon,ListCon,Con}
import bluejelly.bjc.common.Name
import bluejelly.bjc.common.ScopedName

/**
 * The compiler environment. Everything of interest
 * during compilation is held in this class.
 *
 * @author ppedemon
 */
class BjcEnv(
    private val modName:Symbol, // Name of module being compiled
    private val modEnv:Map[Symbol,ModDefn],
    private val instEnv:Map[Name,List[Inst]]) {

  def this(modName:Symbol) = this(modName, Map.empty, Map.empty) 

  def addModDefn(modDefn:ModDefn) = {
    val n_modEnv = modEnv + (modDefn.name -> modDefn)
    val n_instEnv = modDefn.insts.foldLeft(instEnv)((instEnv,inst) => {
      instEnv + (inst.clsName -> (inst::instEnv.get(inst.clsName).getOrElse(Nil)))
    })
    new BjcEnv(modName, n_modEnv, n_instEnv)
  } 
  
  def getModDefn(modName:Symbol) = modEnv(modName)
  def hasModDefn(modName:Symbol) = modEnv.contains(modName)

  private def getModDecl[T <: ModDecl](
      extractor:PartialFunction[(ModDefn,ScopedName),T]):PartialFunction[Name,T] = {
    case n => 
      val (q,s) = (Name.qualifier(n), n.name)
      extractor((getModDefn(q),s))
  }

  def getId:PartialFunction[Name,Id] = getModDecl({case (m,n) => m.getId(n)})
  def getTyCon:PartialFunction[Name,TyCon] = getModDecl({case (m,n) => m.getTyCon(n)})
  def getTySyn:PartialFunction[Name,TySyn] = getModDecl({case (m,n) => m.getTySyn(n)})
  def getCls:PartialFunction[Name,Cls] = getModDecl({case (m,n) => m.getCls(n)})
  def getDataCon:PartialFunction[Name,DataCon] = getModDecl({case (m,n) => m.getDataCon(n)})

  override def toString = modEnv.toString  
}

object BjcEnv {
  def apply(modName:Symbol) = {
    val currMod = new ModDefn(modName)
    val env = new BjcEnv(modName)
      .addModDefn(BuiltIns.wiredInMod)
      .addModDefn(currMod)
    BuiltIns.primMods.foldLeft(env)(_ addModDefn _)
  }
}
