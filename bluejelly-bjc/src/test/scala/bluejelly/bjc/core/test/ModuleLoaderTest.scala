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
    val bjcEnv = BjcEnv('Test, new BjcErrors("<unnamed>"), new ModuleLoader(new TestLoader(base)))
    bjcEnv.load('Simple)
    bjcEnv.load('Helper)
    assert(bjcEnv.hasModDefn('Simple))
    assert(bjcEnv.hasModDefn('Helper))
  }
}
