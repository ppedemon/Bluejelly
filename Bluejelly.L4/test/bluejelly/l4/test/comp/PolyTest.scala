/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test.comp

import org.junit.Test

/**
 * Test ad-hoc polymrphism emulation.
 * @author ppedemon
 */
class PolyTest extends L4CompilerTest {

  @Test def testIntEq() {
    check("Poly.l4","^Poly.testIntEq", "True")
  }
  
  @Test def testIntNeq() {
    check("Poly.l4","^Poly.testIntNeq", "False")
  }

  @Test def testIntListEq() {
    check("Poly.l4", Seq(
      ("^Poly.testEqList0","True"),
      ("^Poly.testEqList1","True")
    ))
  }
  
  @Test def testIntListNeq() {
    check("Poly.l4", Seq(
      ("^Poly.testNeqList0","False"),
      ("^Poly.testNeqList1","False"),
      ("^Poly.testNeqList2","False"),
      ("^Poly.testNeqList3","False")
    ))
  }

}
