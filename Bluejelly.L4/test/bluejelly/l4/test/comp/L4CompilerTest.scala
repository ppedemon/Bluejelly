/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test.comp

import junit.framework.TestCase
import org.junit.Assert

/**
 * Test the L4 compiler by compiling modules and executing the resulting class.
 * @author ppedemon
 */
class L4CompilerTest extends TestCase {
  
  private val l4Runner = new L4Runner
  
  protected def check(mod:String, cmd:String, expected:String) {
    val out = l4Runner.run(mod, cmd)
    println(out)
    Assert.assertEquals(out, expected)
  }

  protected def check(mod:String, cmds:Seq[(String,String)]) {
    val (cs,es) = cmds unzip
    val out = l4Runner.run(mod, cs)
    println(out)
    Assert.assertEquals(out, es)
  }
}