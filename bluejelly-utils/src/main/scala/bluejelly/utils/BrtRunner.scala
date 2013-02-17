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
class BrtRunner(private val cp:String) {

  private val mainClass = "bluejelly.runtime.Runtime"
  private val sep = System getProperty "path.separator"

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
    val fullCp = "%s%s%s" format (cp, sep, modulePath.toString)
    val c = Seq("java", "-cp", fullCp, mainClass) ++ decode(cmd)
    fmtOut(c!!)
  }
  
  def runBrt(modulePath:File, cmds:Seq[String]):Seq[String] = {
    cmds map {case s => runBrt(modulePath,s)}
  }
}

