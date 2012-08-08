/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm.test

import java.io.File

import scala.Array.canBuildFrom

import bluejelly.asm.AsmConfig
import bluejelly.asm.Assembler
import junit.framework.TestCase

/**
 * Very simple testing of the Bluejelly asembler. Ideally, we should
 * run the generated class using the runtime, redirectng stderr and
 * stdout to some file or stream and checking for expected results.
 * 
 * <p>Disclaimer: unfortunately, at the moment I don't have the
 * time to implement tests in the way describe above. Maybe later.
 * For now, just check that we are generating a class when we should.
 * 
 * <p>Assumption: all modules to test are located in the <code>mods</code>
 * folder, and module name is of the form <code>asm.X</code>.
 * 
 * @author ppedemon
 */
class AsmTest extends TestCase {
  
  def testValidModules() {
    assemble(validMods)
    new File("mods/asm").listFiles foreach (_ exists)
  }
  
  def testInvalidModules() {
      assemble(invalidMods)
      invalidMods map classFile forall (f => !(f exists))    
  }
  
  private def modsWhere(p:String => Boolean):Array[String] = {
    val base = new File("mods")
    base.list filter (n => p(n) && (n endsWith ".jas")) map ("mods/" + _)
  }
  
  private def validMods = modsWhere(n => !(n startsWith "Fail_"))
  private def invalidMods = modsWhere(_ startsWith "Fail_")
  
  private def classFile(modName:String) = 
    new File(("mods/asm/" + modName.substring(5)) replaceFirst ("jas$", "class"))
  
  private def assemble(modules:Array[String]) {
    val cfg = new AsmConfig
    cfg.outDir = "mods"
    modules foreach (Assembler.assemble(cfg,_))
  }
  
}
