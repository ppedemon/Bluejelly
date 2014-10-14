/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.{UnitCon,TupleCon,ArrowCon,ListCon,Con}
import bluejelly.bjc.common.Name
import bluejelly.bjc.iface.Fixity

/**
 * The compiler environment. Everything of interest
 * during compilation is held in this class.
 *
 * @author ppedemon
 */
class BjcEnv(
    private val modEnv:Map[Name,ModDefn],
    private val fixEnv:Map[Name,Fixity],
    private val idEnv:Map[Name,Id],
    private val tcEnv:Map[Name,TcDecl],
    private val dcEnv:Map[Name,DataCon],
    private val instEnv:Map[Name,List[Inst]]) {

  def this() = this(Map.empty, Map.empty, 
    Map.empty, Map.empty, Map.empty, Map.empty) 

  def addModDefn(modDefn:ModDefn) = {
    val n_modEnv = modEnv + (modDefn.name -> modDefn)
    val n_fixEnv = modDefn.fixities.foldLeft(fixEnv)((env,fix) =>
      env + (fix._1.qualify(modDefn.name) -> fix._2))
    val n_idEnv = modDefn.ids.foldLeft(idEnv)((env,id) => 
      env + (id.name.qualify(modDefn.name) -> id))
    val n_tcEnv = modDefn.tcs.foldLeft(tcEnv)((env,tc) => 
      env + (tc.name.qualify(modDefn.name) -> tc))
    val n_dcEnv = modDefn.dcons.foldLeft(dcEnv)((env,dcon) => 
      env + (dcon.name.qualify(modDefn.name) -> dcon))
    val insts = modDefn.insts.map(inst => 
      new Inst(
        classFromEnv(n_tcEnv, inst.name), 
        inst.tys.head.get, 
        n_idEnv(inst.dfunId.qualify(modDefn.name)))
    )
    new BjcEnv(
      modEnv + (modDefn.name -> modDefn), 
      n_fixEnv,
      n_idEnv, 
      n_tcEnv,
      n_dcEnv,
      insts.foldLeft(instEnv)((instEnv,inst) =>
        instEnv + (inst.cls.name -> 
          (inst::instEnv.get(inst.cls.name).getOrElse(Nil)))
      ))
  } 
  
  def getModDefn(name:Name) = modEnv(name)
  def hasModDefn(name:Name) = modEnv.contains(name)

  def getId:PartialFunction[Name,Id] = idEnv(_) match {
    case id@Id(_,_,_) => id  
  }

  def getCls:PartialFunction[Name,Cls] = 
    classFromEnv(tcEnv,_) match {case c@Cls(_,_,_,_) => c}

  override def toString = modEnv.toString

  private def classFromEnv:PartialFunction[(Map[Name,TcDecl],Name),Cls] = 
    tup => tup._1(tup._2) match { case c@Cls(_,_,_,_) => c }  
}

object BjcEnv {
  def withBuiltIns = {
    val env = new BjcEnv().addModDefn(BuiltIns.wiredInMod)
    BuiltIns.primMods.foldLeft(env)(_ addModDefn _)
  }
}
