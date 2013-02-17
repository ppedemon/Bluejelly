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

/**
 * Suite trait for tests that need to compile and run assembler modules.
 * @author ppedemon
 */
trait AsmRunner extends BeforeAndAfterAll { this:Suite =>

  val srcModDir = "/testmods.src/"
  val binDir = FileUtils.createTempDir
  val brtRunner = new BrtRunner(brtCp) 
  val config = asmConfig

  override def afterAll() {
    FileUtils.delete(binDir)
  }

  def assemble(modName:String) {
    Assembler.assemble(config, getAbsolutePath(modName))
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
    expectResult(expected) {
      run(modName,cmd)
    }
  }

  def check(modName:String, specs:Seq[(String,String)]) {
    val (cs,es) = specs.unzip
    expectResult(es) {
      run(modName,cs)
    } 
  }

  private def brtCp:String = {
    val is = getClass().getResourceAsStream("/brt.properties")
    val brtProps = new Properties
    brtProps load is
    brtProps getProperty "cp"
  }

  private def asmConfig:AsmConfig = {
    val cfg = new AsmConfig
    cfg.outDir = binDir.toString
    cfg
  }

  private def getAbsolutePath(modName:String):String =
    new File(getClass().getResource(srcModDir + modName).toURI).getAbsolutePath
}

