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
// TODO More tests!
class BasicTest extends L4CompilerTest {
  @Test def testBasic() {
    check("Basic.l4", Seq(
        ("Basic.testId","1"),
        ("Basic.testConst","1")
    ))
  }
  @Test def testTorture() {
    check("Torture.l4", "Torture.testNoInline", "7")
  }
}
