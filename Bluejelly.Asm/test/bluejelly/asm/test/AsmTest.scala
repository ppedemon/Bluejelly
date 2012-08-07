/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm.test

import org.junit.runner.RunWith
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MustMatchers
import org.scalatest.Spec
import org.scalatest.junit.JUnitRunner
import bluejelly.asm.AsmConfig
import bluejelly.asm.Assembler
import java.io.File
import org.scalatest.FunSpec

/**
 * Custom matchers here.
 */
trait CustomMatchers {

  // Simple &quot;file exists?&quot; matcher.
  class FileExistsMatcher extends Matcher[java.io.File] {
    def apply(f:java.io.File) = {
      val fileOrDir = if (f.isFile) "file" else "directory"
      val failureMessageSuffix = 
        fileOrDir + " named " + f.getName + " did not exist"
      val negatedFailureMessageSuffix = 
        fileOrDir + " named " + f.getName + " existed"
      MatchResult(
        f.exists,
        "The " + failureMessageSuffix,
        "The " + negatedFailureMessageSuffix,
        "the " + failureMessageSuffix,
        "the " + negatedFailureMessageSuffix
      )
    }
  }
  val exist = new FileExistsMatcher
  
}

object CustomMatchers extends CustomMatchers

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
@RunWith(classOf[JUnitRunner])
class AsmTest extends FunSpec with MustMatchers with CustomMatchers { 
   
  describe("The assembler") {
    it("generates class files from a valid set of modules") {
      assemble(validMods)
      new File("mods/asm").listFiles() foreach (_ must exist)
    }
    it("does not generate classes for invalid modules") {
      assemble(invalidMods)
      invalidMods map classFile map (_ must not (exist))
    }
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
