/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test.comp

import java.io.File
import junit.framework.TestCase
import org.junit.Test

/**
 * Battery of tests consisting of programs to be compiled and executed
 * Tests will pass if program output matches a expected string output.
 *  
 * @author ppedemon
 */
class BasicTest extends L4CompilerTest {
  
  @Test def testBasic() {
    check("Test.l4", Seq(
        ("Test.testId","1"),
        ("Test.testConst","1"),
        ("Test.testWeirdAdd", "6"),
        ("Test.testWeirdId","weird!"),
        ("Test.testRecursive","-2"),
        ("Test.testAddProxy", "0")
    ))
  }
  
  @Test def testContrivedCases() {
    check("Test.l4", Seq(
      ("Test.testNoInline", "7"),
      ("Test.lazyAlias", "3"),
      ("Test.testStrictInline", "10"),
      ("Test.testNestedEval", "5"),
      ("Test.testFishy0", "2"),
      ("Test.testFishyDef", "2"),
      ("Test.nestedMatch", "2")
    ))
  }
  
  @Test def testMatches() {
    check("Test.l4", Seq(
      ("Test.testIntMatch","2"),
      ("Test.testIntMatchDef","0")
    ))    
  }
  
  def fact(x:Int):BigInt = x match {
    case 0 => 1
    case x => x*fact(x-1)
  }
  
  @Test def testRealCode() {
    check("Test.l4", Seq(
      ("^Test.testEven","True"),
      ("^Test.testOdd","True"),
      ("^Test.testEvenFalse","False"),
      ("^Test.testOddFalse","False"),
      ("Test.testFact", fact(100) toString)
    ))
  }
}
