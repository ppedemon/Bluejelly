/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test.comp

import java.io.File
import java.io.FilenameFilter
import scala.sys.process.stringSeqToProcess
import bluejelly.l4.Config
import bluejelly.l4.L4C
import bluejelly.utils.BrtRunner

/**
 * Execute the compiler and the runtime as an external process.
 * Return the runtime's output string.
 * 
 * @author ppedemon
 */
class L4Runner {
  
  private val usrDir = System getProperty "user.dir"
  
  private val src = new File(usrDir, "test.src")
  private val bin = new File(usrDir, "test.bin")

  private val brt = new BrtRunner(bin)
  
  private lazy val l4c = {
    val cfg = new Config
    cfg.outDir = bin.toString
    new L4C(cfg)    
  }
  
  private def prepare(modName:String) {
    if (!bin.exists) bin.mkdirs
    new File(bin,modName replaceFirst ("l4$","class")).delete
  }

  def compile(modName:String) {
    prepare(modName)
    l4c.compile(new File(src,modName) toString)
  }
  
  def run(modName:String, cmd:String):String = {
    compile(modName)
    brt runBrt(bin, cmd)
  }
  
  def run(modName:String, cmds:Seq[String]):Seq[String] = {
    compile(modName)
    brt runBrt(bin, cmds)
  }  
}
