/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.common.Name

/**
 * The compiler environment. Everything of interest
 * during compilation is held in this class.
 */
class BjcEnv(val loadedMods:Set[Name] = Set()) {
  def register(modName:Name) = new BjcEnv(loadedMods + modName)
  def isLoaded(modName:Name) = loadedMods(modName)
}
