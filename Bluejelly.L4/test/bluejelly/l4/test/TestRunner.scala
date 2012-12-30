/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test

import scala.sys.process._
import java.io.File
import java.io.FilenameFilter

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
  
  val cp = Seq(
      new File(base, "Bluejelly.L4/bin"),
      new File(base, "Bluejelly.Asm/bin"),
      new File(base, "Bluejelly.Utils/bin")) ++ allLibs
  val cpStr= cp mkString sep
  
  val bp = Seq(new File(base, "Bluejelly.Runtime/bin")) ++ allLibs
  
  val l4c = "bluejelly.l4.L4C"
  val brt = "bluejelly.runtime.Runtime"
      
  def allLibs = {
    val f = new File(base, "Bluejelly.Libs/lib")
    f.listFiles(new FilenameFilter() {
      def accept(f:File, s:String) = s.endsWith("jar")
    })
  }
               
  def runL4c(in:File,bin:File) = {
    Seq("java", "-cp", cpStr, l4c, "-d", bin.toString, in.toString)
  }
  
  def runBrt(mdir:File, what:String) = {
    val cp = (bp :+ mdir) mkString sep
    Seq("java", "-cp", cp, brt, what)
  }
  
  def run(in:File, what:String) = {
    prepare()
    val l4c = runL4c(in, bin)
    val brt = runBrt(bin, what)
    l4c #&& brt !!
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
    val runner = new TestRunner
    val output = runner run (new File(src,"Basic.l4"),"bluejelly.test.Basic.id")
    println(output)
  }
}
