/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.asm.test

import java.io.File
import java.util.Properties

import org.scalatest._

import bluejelly.utils.BrtRunner

/**
 * Suite of basic assembler tests.
 * @author ppedemon
 */
class BasicSuite extends FunSuite with AsmRunner {

  test("the assembler must compile correctly basic functions") {
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

  test("it must handle correctly partial applications") {
    check("Test.jas", Seq(
      ("Test.app","3"),
      ("Test.napp","N{<Test:const>,2}"),
      ("Test.packapp","2")
    ))
  }

  test("it must compile correctly int arithmetic, handling div by zero") {
    check("Test.jas", Seq(
      ("Test.add","21"),
      ("Test.sub","-3"),
      ("Test.div","2"),
      ("Test.divByZero","You are not supposed to divide by zero"),
      ("Test.rem","1"),
      ("Test.neg","-42")
    ))
  }

  test("it must generate correct code for bit-logical operators") {
    check("Test.jas", Seq(
      ("Test.not","-1"),
      ("Test.or","15"),
      ("Test.and","2"),
      ("Test.xor","0")
    ))
  }

  test("it must generate correct code for bit shift operators") {
    check("Test.jas", Seq(
      ("Test.shl","8"),
      ("Test.shr","-64"),
      ("Test.lshr",Int.MaxValue.toString)
    ))
  }

  test("it must handle correctly integer comparisons") {
    check("Test.jas", Seq(
      ("^Test.eq","True"),
      ("^Test.neq","True"),
      ("^Test.lt","False"),
      ("^Test.gt","True"),
      ("^Test.leq","True"),
      ("^Test.geq","True")
    ))
  }

  test("the assembler must also compile correctly floating point arithmetic") {
    val x = (3.14159 + 2.71828)*(0.110001+1.61803)
    check("Test.jas", Seq(
      ("Test.addd", x.toString),
      ("Test.subd", (3.18 - 4.56).toString),
      ("Test.divd", (2.28/4.56).toString),
      ("Test.divinf", (4.56/0).toString),
      ("Test.remd", (-(x % 3.14159)).toString)
    ))
  }
  
  test("it must generate correct code for floating point comparisons") {
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

