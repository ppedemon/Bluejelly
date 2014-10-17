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
    val mod = 
    """
      import A hiding(C(),D())
      import A(C)
    """
    val r = new StringReader(mod)
    val errors = new BjcErrors("Main.hs")
    val bjc = new Bjc(new TestLoader(base))
    val result = bjc.parse(r)
    result match {
      case Left(err) => fail(err)
      case Right(mod) => 
        val exps = bjc.chaseImports(mod,errors)
        if (errors.hasErrors) {
          errors.dumpTo(new PrintWriter(System.out))
          fail("Test failed")
        } else {
          println(exps)
        }
    }
  }
}
