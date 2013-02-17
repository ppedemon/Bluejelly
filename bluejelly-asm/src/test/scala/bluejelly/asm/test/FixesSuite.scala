/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.asm.test

import org.scalatest._

/**
 * We test here for fixed bugs.
 * @author ppedemon
 */
class FixesSuite extends FunSuite with AsmRunner with BeforeAndAfterEach {
 
  override def beforeEach() {
    // Fixes.jas uses functions in Test.jas, so we assemble it first 
    assemble("Test.jas")
  }

  test("the assembler must handle evaluation of NApp code like functions") {
    check("Fixes.jas", "Fixes.idEval", 1 toString)
  }

  test("the assembler must handle evaluation of NApp code like partial apps") {
    check("Fixes.jas", "Fixes.funEval", 1 toString)
  }
}
