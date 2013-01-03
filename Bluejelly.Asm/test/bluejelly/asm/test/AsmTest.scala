/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.asm.test

import junit.framework.TestCase
import junit.framework.Assert

/**
 * Common code for all assembler tests.
 * @author ppedemon
 */
class AsmTest extends TestCase {
  
  private val asmRunner = new AsmRunner
  
  protected def check(mod:String, cmd:String, expected:String) {
    val out = asmRunner.run(mod, cmd)
    println(out)
    Assert.assertEquals(out, expected)
  }

  protected def check(mod:String, cmds:Seq[(String,String)]) {
    val (cs,es) = cmds unzip
    val out = asmRunner.run(mod, cs)
    println(out)
    Assert.assertEquals(out, es)
  }
}
