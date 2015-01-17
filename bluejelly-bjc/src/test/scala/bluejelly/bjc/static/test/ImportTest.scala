/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.static.test

import bluejelly.bjc.Bjc
import bluejelly.bjc.TestResourceReader
import bluejelly.bjc.common.Name.{idName,tcName}
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
    val mod = """import A"""
    val unnamed = "<unnamed>"
    val r = new StringReader(mod)
    val bjc = new Bjc(unnamed, new TestLoader(base))
    val result = bjc.parse(r)
    result match {
      case None => 
        bjc.dumpErrors
        fail("Test failed")
      case Some(mod) =>
        val bjcEnv = BjcEnv(Symbol(unnamed))
        val (n_bjcEnv,nameTab) = bjc.chaseImports(bjcEnv, mod)
        if (bjc.hasErrors) {
          bjc.dumpErrors
          fail("Test failed")
        } else {
          assert(nameTab.hasName(tcName('D)))
          assert(nameTab.hasName(idName('C)))
          assert(nameTab.hasName(idName('Y)))
          assert(nameTab.hasName(idName('op1)))
          assert(nameTab.hasName(idName('op2)))
          assert(nameTab.hasName(tcName('Arrow)))
          assert(nameTab.hasName(idName('arr)))
          assert(nameTab.hasName(tcName('Color)))
          assert(nameTab.hasName(idName('Red)))
          assert(nameTab.hasName(idName('Black)))
        }
    }
  }
}
