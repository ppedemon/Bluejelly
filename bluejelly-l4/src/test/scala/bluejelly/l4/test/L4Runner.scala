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

/**
 * Suite trait for tests that need to compile and run L4 modules.
 * @author ppedemon
 */
trait L4Runner extends BeforeAndAfterAll { this:Suite =>

  val srcModDir = "/testmods.src/"
  val binDir = FileUtils.createTempDir
  val brtRunner = new BrtRunner(brtCp) 
  val l4c = createL4C

  override def afterAll() {
    FileUtils.delete(binDir)
  }

  def compile(modName:String) {
    l4c.compile(getAbsolutePath(modName))
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

  private def createL4C:L4C = {
    val cfg = new Config
    cfg.outDir = binDir.toString
    new L4C(cfg)
  }

  private def getAbsolutePath(modName:String):String =
    new File(getClass().getResource(srcModDir + modName).toURI).getAbsolutePath
}

