/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test

import org.scalatest._

/**
 * Test ad-hoc polymrphism emulation.
 * @author ppedemon
 */
class PolySuite extends FunSuite with L4Runner {

  test("polymorphic equality must work on ints") {
    check("Poly.l4","^Poly.testIntEq", "True")
  }
  
  test("polymorphic inequality must work on ints") {
    check("Poly.l4","^Poly.testIntNeq", "False")
  }

  test("polymorphic equality must work on list of ints") {
    check("Poly.l4", Seq(
      ("^Poly.testEqList0","True"),
      ("^Poly.testEqList1","True")
    ))
  }
  
  test("polymorphic inequality must work on lists of ints") {
    check("Poly.l4", Seq(
      ("^Poly.testNeqList0","False"),
      ("^Poly.testNeqList1","False"),
      ("^Poly.testNeqList2","False"),
      ("^Poly.testNeqList3","False")
    ))
  }
}

