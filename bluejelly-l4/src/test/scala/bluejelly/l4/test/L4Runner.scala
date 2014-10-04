/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.l4.test

import java.io.File
import java.util.Properties
import org.scalatest._
import bluejelly.l4.{Config,L4C}
import bluejelly.utils.{BrtRunner,FileUtils}
import java.io.FileReader
import java.io.PrintWriter

/**
 * Suite trait for tests that need to compile and run L4 modules.
 * @author ppedemon
 */
trait L4Runner extends BeforeAndAfterAll { this:Suite =>

  val srcModDir = "/testmods.src/"
  val binDir = FileUtils.createTempDir
  val brtRunner = new BrtRunner(brtCp) 

  override def afterAll() {
    FileUtils.delete(binDir)
  }

  def compile(modName:String) {
    val modFile = getAbsolutePath(modName)
    val out = L4C.compile(new FileReader(modFile))
    if (out.isLeft) {
      out.left.get.dumpTo(new PrintWriter(System.err))
      fail("Failed to compile: " + modName)
    } else {
      val className = modName replaceFirst("\\.l4$","")
      L4C.save(binDir.toString, className, out.right.get)
    }
  }

  def run(modName:String, cmd:String):String = {
    compile(modName)
    brtRunner.runBrt(binDir, cmd)
  }
  
  def run(modName:String, cmds:Seq[String]):Seq[String] = {
    compile(modName)
    brtRunner.runBrt(binDir, cmds)
  }

  def check(modName:String, cmd:String, expected:String) {
    assertResult(expected) {
      run(modName,cmd)
    }
  }

  def check(modName:String, specs:Seq[(String,String)]) {
    val (cs,es) = specs.unzip
    assertResult(es) {
      run(modName,cs)
    } 
  }

  private def brtCp:String = {
    val is = getClass().getResourceAsStream("/brt.properties")
    val brtProps = new Properties
    brtProps load is
    brtProps getProperty "cp"
  }

  private def getAbsolutePath(modName:String):String =
    new File(getClass().getResource(srcModDir + modName).toURI).getAbsolutePath
}

