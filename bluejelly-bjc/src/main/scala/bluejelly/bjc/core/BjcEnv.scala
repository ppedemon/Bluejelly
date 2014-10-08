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
 *
 * @author ppedemon
 */
class BjcEnv(private val loadedMods:Map[Name,ModDefn] = Map.empty) {

  def addModDefn(modDefn:ModDefn) = 
    new BjcEnv(loadedMods + (modDefn.name -> modDefn))
  
  def getModDefn(name:Name) = loadedMods(name)
  def hasModDefn(name:Name) = loadedMods.contains(name)
}
