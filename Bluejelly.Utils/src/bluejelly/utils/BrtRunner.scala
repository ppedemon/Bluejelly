/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.utils

import java.io.File
import java.io.FilenameFilter

import scala.sys.process.stringSeqToProcess

/**
 * Run the bluejelly runtime as an external process whose output can 
 * be read and compared with an expected value. Ideal for testing.
 * 
 * @author ppedemon
 */
class BrtRunner(private val bin:File) {

  private val usrDir = System getProperty "user.dir"
  private val sep    = System getProperty "path.separator"
  private val base   = new File(usrDir) getParentFile

  private val brt = "bluejelly.runtime.Runtime"
  private val bp  = Seq(new File(base, "Bluejelly.Runtime/bin")) ++ allLibs
  
  private def allLibs = {
    val f = new File(base, "Bluejelly.Libs/lib")
    f.listFiles(new FilenameFilter() {
      def accept(f:File, s:String) = s.endsWith("jar")
    })
  }

  /*
   * :M.f => M.f returns a list, execute runtime in list mode
   * ^M.f => M.f returns a boolean, execute runtime in boolean mode
   * otherwise, just execute the function in normal mode
   */
  private def decode(cmd:String):Seq[String] = cmd charAt 0 match {
    case ':' => Seq("-l",cmd substring 1)
    case '^' => Seq("-b",cmd substring 1)
    case _ => Seq(cmd)
  }
  
  private def fmtOut(s:String) = s trim
  
  def runBrt(modulePath:File, cmd:String):String = {
    val cp = (bp :+ modulePath) mkString sep
    val c = Seq("java", "-cp", cp, brt) ++ decode(cmd)
    fmtOut(c!!)
  }
  
  def runBrt(modulePath:File, cmds:Seq[String]):Seq[String] = {
    cmds map {case s => runBrt(modulePath,s)}
  }
}
