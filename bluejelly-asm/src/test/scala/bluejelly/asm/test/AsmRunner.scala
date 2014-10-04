/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm.test

import java.io.File
import java.util.Properties
import org.scalatest._
import bluejelly.asm.{AsmConfig,Assembler}
import bluejelly.utils.{BrtRunner,FileUtils}
import java.io.FileReader
import java.io.PrintWriter

/**
 * Suite trait for tests that need to compile and run assembler modules.
 * @author ppedemon
 */
trait AsmRunner extends BeforeAndAfterAll { this:Suite =>

  val srcModDir = "/testmods.src/"
  val binDir = FileUtils.createTempDir
  val brtRunner = new BrtRunner(brtCp) 

  override def afterAll() {
    FileUtils.delete(binDir)
  }

  def assemble(modName:String) {
    val modFile = getAbsolutePath(modName)
    val out = Assembler.assemble(new FileReader(modFile), false)
    if (out.isLeft) {
      out.left.get.dumpTo(new PrintWriter(System.err))
      fail("Failed to compile: " + modName)
    } else {
      val className = modName replaceFirst("\\.jas$","")
      Assembler.save(binDir.toString, className, out.right.get)
    }
  }

  def run(modName:String, cmd:String):String = {
    assemble(modName)
    brtRunner.runBrt(binDir, cmd)
  }
  
  def run(modName:String, cmds:Seq[String]):Seq[String] = {
    assemble(modName)
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

