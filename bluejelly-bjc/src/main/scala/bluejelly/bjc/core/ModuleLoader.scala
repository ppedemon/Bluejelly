/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.common.Name
import bluejelly.bjc.iface.{ModIface,ModIfaceIO}

import java.io.IOException

case class LoaderException(msg:String) extends Exception(msg)

/**
 * Abstract class for objects knowing how to transform a module name into
 * a ModIface. The idea is to allow for loader mocks building ModIfaces 
 * from  a .hi file, hence making possible to test the module system.
 *
 * @author ppedemon 
 */
abstract class IfaceLoader {
  @throws[LoaderException]
  def load(modName:Name):ModIface
}

/**
 * `Producton-ready' IfaceLoader: this is the interface loader that
 * will be used in production code: it solves modules using the class
 * path, and delegates on ModIfaceIO for the extracting the interface
 * from a class bytecode.
 *
 * @author ppedemon 
 */
object ProdLoader extends IfaceLoader {
  def load(modName:Name) = try {
    val iface = ModIfaceIO.load(modName.toString)
    if (iface.name != modName) 
      throw LoaderException(s"Invalid module: `$modName' (it declares `$iface.name')") 
    iface
  } catch {
    case e:LoaderException => 
      throw e
    case e:IOException => throw new LoaderException(
      s"Module not found: `$modName'")
    case _:Exception => throw new LoaderException(
      s"Error loading: `$modName' (class doesn't look like a Bluejelly module)")
  }
}

/**
 * ModuleLoader: recursively load a module and its dependencies.
 * Module declarations are stored in a given [[BjcEnv]] instance.
 *
 * @author ppedemon
 */
class ModuleLoader(val loader:IfaceLoader = ProdLoader) {

  import scala.collection.mutable.{Map => MutableMap}

  private val modCache:MutableMap[Name,ModDefn] = MutableMap.empty

  def getModCache = modCache

  def load(modName:Name):ModDefn = 
    if (modCache.contains(modName)) modCache(modName) else {
      val iface = loader.load(modName)
      val modDefn = translate(iface)
      modCache += (modName -> modDefn)
      for (dep <- iface.deps) load(dep)
      modDefn
    }

  // TODO Implement me!
  private def translate(iface:ModIface) = new ModDefn(
    iface.name,
    List.empty, List.empty, List.empty, List.empty, 
    List.empty, List.empty, List.empty, List.empty)
}
