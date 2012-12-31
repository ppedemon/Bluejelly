/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test

import java.io.File
import junit.framework.TestCase
import org.junit.Test

/**
 * Battery of tests consisting of programs to be compiled and executed
 * by the {@link TestRunner} class. Tests will pass if program output
 * matches some given string.
 *  
 * @author ppedemon
 */
class FullExecTest extends TestCase {
  val runner = new TestRunner
  
  private def doTest(src:String, fun:String, expected:String) {
    val output = runner run (new File("test.src",src),fun)
    assert(output == expected)
  }
  
  private def doTest(src:String, funs:Seq[String], expected:String, sep:String = "#") {
    val output = runner run (new File("test.src",src), funs, sep)
    assert(output == expected)
  }  
  
  @Test def testBasic() {
    doTest("Basic.l4", Seq("Basic.testId","Basic.testConst"), "1#1")
  }
}
