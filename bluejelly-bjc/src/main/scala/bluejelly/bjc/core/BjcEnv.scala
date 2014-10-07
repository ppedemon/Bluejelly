/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.{UnitCon,TupleCon,ArrowCon,ListCon,Con}
import bluejelly.bjc.common.Name

/**
 * The compiler environment. Everything of interest
 * during compilation is held in this class.
 */
class BjcEnv(
    val loadedMods:Set[Name],
    val ids:Map[Name,Id],
    val tysyns:Map[Name,TySyn],
    val tycons:Map[Name,TyCon],
    val dcons:Map[Name,DataCon],
    val classes:Map[Name,Cls],
    val insts:Map[Name,List[Inst]]) {

  def this() = this(Set.empty, Map.empty, Map.empty, 
    Map.empty, Map.empty, Map.empty, Map.empty)

  def isLoaded(modName:Name) = loadedMods(modName)

  def register(modName:Name) = new BjcEnv(loadedMods + modName,
    ids, tysyns, tycons, dcons, classes, insts)

  def addId(id:Id) = new BjcEnv(
    loadedMods, 
    ids + (id.name -> id), 
    tysyns, tycons, dcons, classes, insts)

  def addTySyn(tysyn:TySyn) = new BjcEnv(
    loadedMods, ids, tysyns + (tysyn.name -> tysyn), tycons, dcons, classes, insts)

  def addTyCon(tycon:TyCon) = new BjcEnv(
    loadedMods, 
    ids ++ tycon.dcons.flatMap(_.fields.map(id => (id.name,id))), 
    tysyns, 
    tycons + (tycon.name -> tycon), 
    dcons ++ tycon.dcons.map(dc => (dc.name,dc)), classes, insts)

  def addClass(cls:Cls) = new BjcEnv(
    loadedMods, 
    ids ++ cls.ops.map(op => (op.id.name,op.id)), 
    tysyns, tycons, dcons, 
    classes + (cls.name -> cls), insts)

  def addInst(inst:Inst) = new BjcEnv(
    loadedMods, ids, tysyns, tycons, dcons, classes, 
    insts + (inst.cls.name -> (inst :: insts.get(inst.cls.name).getOrElse(Nil))))
}
