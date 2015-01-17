/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core.test

import bluejelly.bjc.TestResourceReader
import bluejelly.bjc.common.Name
import bluejelly.bjc.core._
import bluejelly.bjc.iface.test.ModIfaceParser

import java.io.File

import org.scalatest.FunSuite

/**
 * Test module system.
 * @author ppedemon
 */
class ModuleLoaderTest extends FunSuite with TestResourceReader {
  
  private val base = new File("/iface.tests")
  
  test("ModuleLoader must load properly module interfaces in test mode") {
    val loader = new ModuleLoader(new TestLoader(base))
    val env = loader.load(BjcEnv('Test), 'Simple)
    assert(env.hasModDefn('Simple))
    assert(env.hasModDefn('Helper))
    println(env)
  }
}
