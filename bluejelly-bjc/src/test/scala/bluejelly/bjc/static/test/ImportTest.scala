/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.static.test

import bluejelly.bjc.Bjc
import bluejelly.bjc.TestResourceReader
import bluejelly.bjc.common.Name
import bluejelly.bjc.core._
import bluejelly.bjc.static._

import bluejelly.bjc.core.test.TestLoader
import bluejelly.bjc.iface.test.ModIfaceParser

import java.io.{File,StringReader,PrintWriter}

import org.scalatest.FunSuite

/**
 * Test import chasing.
 * @author ppedemon
 */
class ImportTest extends FunSuite with TestResourceReader {

  private val base = new File("/import.tests")
  
  test("Import Chaser must handle vanilla imports") {
    val mod = """import qualified A as Q hiding(op1,C); --import A hiding()"""
    val r = new StringReader(mod)
    val bjc = new Bjc("<unnamed>",new TestLoader(base))
    val result = bjc.parse(r)
    result match {
      case None => 
        bjc.dumpErrors
        fail("Test failed")
      case Some(mod) =>
        val gblEnv = new GlobalEnv(BjcEnv.withBuiltIns)
        val n_gblEnv = bjc.chaseImports(gblEnv, mod)
        if (bjc.hasErrors) {
          bjc.dumpErrors
          fail("Test failed")
        } else {
          println(n_gblEnv)
        }
    }
  }
}
