/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.asm.test

import org.scalatest._

/**
 * Test ad-hoc polmorphism emulation in Poly.jas.
 * @author ppedemon
 */
class PolySuite extends FunSuite with AsmRunner {

  test("preliminary check: boolean negation must work as expected") {
    check("Poly.jas", Seq(("^Poly.notF","True"),("^Poly.notT","False")))
  }
  
  test("preliminary check: boolean 'and' must work as expected") {
    check("Poly.jas", Seq(
      ("^Poly.andFF","False"),
      ("^Poly.andTF","False"),
      ("^Poly.andFT","False"),
      ("^Poly.andTT","True")
    ))
  }
  
  test("polymorphic equality must work on plain integers") {
    check("Poly.jas", Seq(("^Poly.eqInt","True"),("^Poly.neqInt","False")))
  }
  
  test("polymorphic equality must work on compound types, like list of ints") {
    check("Poly.jas", Seq(
      ("^Poly.eqListEE","True"),
      ("^Poly.neqListEE","False"),
      ("^Poly.eqListNN","True"),
      ("^Poly.neqListNN","False"),
      ("^Poly.eqListEN","False"),
      ("^Poly.neqListEN","True"),
      ("^Poly.eqList1","False"),
      ("^Poly.eqList2","True")
    ))
  }
}

