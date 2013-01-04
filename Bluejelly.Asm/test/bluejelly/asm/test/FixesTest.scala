/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.asm.test

import org.junit.Test

/**
 * We test here for fixed bugs.
 * @author ppedemon
 */
class FixesTest extends AsmTest {
  @Test def testIdEval() {
    check("Fixes.jas", "Fixes.idEval", 1 toString)
  }
  @Test def testFunEval() {
    check("Fixes.jas", "Fixes.funEval", 1 toString)
  }
}
