/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm.test

import org.junit.Test
import org.junit.Assert

import junit.framework.TestCase

/**
 * Test assembler + runtime by executing several 
 * functions in the Test module.
 * 
 * @author ppedemon
 */
class BasicTest extends AsmTest {
  
  @Test
  def testNuggets() {
    check("Test.jas", Seq(
        ("Test.main","It works!"),
        ("Test.i","42"),
        ("Test.s","Hey!"),
        ("Test.d","3.14159"),
        ("Test.c","c"),
        (":Test.t","[]"),
        (":Test.list","[1,2,3]"),
        ("Test.trycatch","error!")
    ))
  }
  
  @Test
  def testApps() {
    check("Test.jas", Seq(
      ("Test.app","3"),
      ("Test.napp","N{<Test:const>,2}"),
      ("Test.packapp","2")
    ))
  }

  @Test
  def testIntArith() {
    check("Test.jas", Seq(
      ("Test.add","21"),
      ("Test.sub","-3"),
      ("Test.div","2"),
      ("Test.divByZero","You are not supposed to divide by zero"),
      ("Test.rem","1"),
      ("Test.neg","-42")
    ))
  }

  @Test
  def testIntBits() {
    check("Test.jas", Seq(
      ("Test.not","-1"),
      ("Test.or","15"),
      ("Test.and","2"),
      ("Test.xor","0")
    ))
  }

  @Test
  def testShifts() {
    check("Test.jas", Seq(
      ("Test.shl","8"),
      ("Test.shr","-64"),
      ("Test.lshr",Int.MaxValue.toString)
    ))
  }

  @Test
  def testIntCmp() {
    check("Test.jas", Seq(
      ("^Test.eq","True"),
      ("^Test.neq","True"),
      ("^Test.lt","False"),
      ("^Test.gt","True"),
      ("^Test.leq","True"),
      ("^Test.geq","True")
    ))
  }

  @Test
  def testDblArith() {
    val x = (3.14159 + 2.71828)*(0.110001+1.61803)
    check("Test.jas", Seq(
      ("Test.addd", x toString),
      ("Test.subd", (3.18 - 4.56) toString),
      ("Test.divd", (2.28/4.56) toString),
      ("Test.divinf", (4.56/0) toString),
      ("Test.remd", -(x % 3.14159) toString)
    ))
  }
  
  @Test
  def testDblCmp() {
    check("Test.jas", Seq(
      ("^Test.eqd", "True"),
      ("^Test.neqd", "True"),
      ("^Test.ltd", "True"),
      ("^Test.gtd", "False"),
      ("^Test.leqd", "True"),
      ("^Test.geqd", "True")
    ))    
  }
}
