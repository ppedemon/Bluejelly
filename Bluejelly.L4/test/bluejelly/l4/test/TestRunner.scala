/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test

import java.io.File
import java.io.FilenameFilter

import scala.sys.process.stringSeqToProcess

import bluejelly.l4.Config
import bluejelly.l4.L4C

/**
 * Execute the compiler and the runtime as an external process.
 * Return the runtime's output string.
 * 
 * @author ppedemon
 */
class TestRunner {
  
  val usrDir = System getProperty "user.dir"
  val sep    = System getProperty "path.separator"
  val base   = new File(usrDir).getParentFile()
  val bin    = new File(usrDir, "test.bin")

  val brt = "bluejelly.runtime.Runtime"
  val bp  = Seq(new File(base, "Bluejelly.Runtime/bin")) ++ allLibs
  
  def allLibs = {
    val f = new File(base, "Bluejelly.Libs/lib")
    f.listFiles(new FilenameFilter() {
      def accept(f:File, s:String) = s.endsWith("jar")
    })
  }
  
  lazy val l4c = {
    val cfg = new Config
    cfg.outDir = bin.toString
    new L4C(cfg)    
  }
  
  def runL4c(in:File,bin:File) {
    l4c.compile(in.toString)
  }
  
  def runBrt(mdir:File, what:String) = {
    val cp = (bp :+ mdir) mkString sep
    Seq("java", "-cp", cp, brt, what)
  }
  
  def run(in:File, what:String) = {
    prepare()
    runL4c(in, bin)
    val brt = runBrt(bin, what)
    brt !!
  }
  
  private def prepare() {
    if (!bin.exists()) bin.mkdirs()
  }
}

/*
 * Just for testing the runner.
 */
object TestRunner {
  val usrDir = System getProperty "user.dir"
  val src = new File(usrDir, "test.src")

  def main(args:Array[String]) {
    {
      val runner = new TestRunner
      val output = runner run (new File(src,"Basic.l4"),"bluejelly.test.Basic.id")
      print(output)      
    }
    {
      val runner = new TestRunner
      val output = runner run (new File(src,"Basic.l4"),"bluejelly.test.Basic.const")
      print(output)
    }
  }
}
