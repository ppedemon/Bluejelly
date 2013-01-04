/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm.test

import java.io.File
import bluejelly.asm.AsmConfig
import bluejelly.utils.BrtRunner
import bluejelly.asm.Assembler

/**
 * Run assembler and runtime on the generated class.
 * @author ppedemon
 */
class AsmRunner {

  private val usrDir = System getProperty "user.dir"
  
  private val bin = new File(usrDir, "test.bin")
  private val src = new File(usrDir, "test.src")
  
  private val brt = new BrtRunner(bin)
  
  private def prepare(modName:String) {
    if (!bin.exists) bin.mkdirs
    new File(bin,modName replaceFirst ("jas$","class")).delete
  }
  
  private def assemble(modName:String) {
    prepare(modName)
    val cfg = new AsmConfig
    cfg.outDir = bin.toString
    Assembler.assemble(cfg, new File(src,modName) toString)
  }
  
  def run(modName:String, cmd:String):String = {
    assemble(modName)
    brt runBrt(bin, cmd)
  }
  
  def run(modName:String, cmds:Seq[String]):Seq[String] = {
    assemble(modName)
    brt runBrt(bin, cmds)
  }
}
