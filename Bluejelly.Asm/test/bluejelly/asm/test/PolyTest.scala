/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.asm.test
import org.junit.Test

/**
 * Test ad-hoc polmorphism emulation in Poly.jas.
 * @author ppedemon
 */
class PolyTest extends AsmTest {

  @Test
  def testNot() {
    check("Poly.jas", Seq(("^Poly.notF","True"),("^Poly.notT","False")))
  }
  
  @Test
  def testAnd() {
    check("Poly.jas", Seq(
      ("^Poly.andFF","False"),
      ("^Poly.andTF","False"),
      ("^Poly.andFT","False"),
      ("^Poly.andTT","True")
    ))
  }
  
  @Test 
  def testEqInt() {
    check("Poly.jas", Seq(("^Poly.eqInt","True"),("^Poly.neqInt","False")))
  }
  
  @Test
  def testEqList() {
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