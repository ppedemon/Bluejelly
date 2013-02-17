/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test

import org.scalatest._

/**
 * Battery of tests consisting of programs to be compiled and executed
 * Tests will pass if program output matches a expected string output.
 *  
 * @author ppedemon
 */
class BasicSuite extends FunSuite with L4Runner {
  
  test("L4 must compile correctly basic functions") {
    check("Test.l4", Seq(
        ("Test.testId","1"),
        ("Test.testConst","1"),
        ("Test.testWeirdAdd", "6"),
        ("Test.testWeirdId","weird!"),
        ("Test.testRecursive","-2"),
        ("Test.testAddProxy", "0")
    ))
  }
  
  test("L4 must compile correctly contrived cases") {
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
  
  test("L4 must compile correctly code matching on int constants") {
    check("Test.l4", Seq(
      ("Test.testIntMatch","2"),
      ("Test.testIntMatchDef","0")
    ))    
  }
  
  test("L4 must compile correctly code matching on floating point constants") {
    check("Test.l4", Seq(
      ("Test.testDblMatch0","first\u5f62"),
      ("Test.testDblMatch1","ok"),
      ("Test.testDblMatch2","whatever")
    ))
  }

  test("L4 must compile correctly code matching in character constants") {
    check("Test.l4", Seq(
      ("Test.testChrMatch0","0"),
      ("Test.testChrMatch1","1"),
      ("Test.testChrMatch2","2"),
      ("Test.testChrMatch3","-1")
    ))
  }

  test("L4 must compile correctly code matching on string constants") {
    check("Test.l4", Seq(
      ("Test.testStrMatch0","red"),
      ("Test.testStrMatch1","blue"),
      ("Test.testStrMatch2","yellow"),
      ("Test.testStrMatch3","white"),
      ("Test.testStrMatch4","black")
    ))
  }

  def fact(x:Int):BigInt = x match {
    case 0 => 1
    case x => x*fact(x-1)
  }
  
  test("L4 must compile correctly code that you'd find in a real program") {
    check("Test.l4", Seq(
      ("^Test.testEven","True"),
      ("^Test.testOdd","True"),
      ("^Test.testEvenFalse","False"),
      ("^Test.testOddFalse","False"),
      ("Test.testFact", fact(100) toString)
    ))
  }
}

