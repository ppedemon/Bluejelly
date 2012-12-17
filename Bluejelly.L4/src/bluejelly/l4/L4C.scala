/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import java.io.File
import java.io.FileOutputStream
import java.io.FileReader
import java.io.PrintWriter
import java.io.StringWriter

import scala.collection.mutable.MutableList

import bluejelly.asm.AsmConfig
import bluejelly.asm.Assembler
import bluejelly.utils.Args
import bluejelly.utils.StrOpt
import bluejelly.utils.UnicodeFilter
import bluejelly.utils.UnitOpt

// Phases of the L4C compiler
private object Phase extends Enumeration {
  type Phase = Value
  val RENAME, OCC, INLINE, COMP, RESOLVE, OPT, FLATTEN = Value
}

// Configuration for an execution of the L4C compiler
private class Config {
  import Phase._
  var help = false
  var version = false
  var stopAt:Phase = null
  var outDir = "."
  var files:MutableList[String] = MutableList()
  def shouldStopAt(phase:Phase) = phase == stopAt
}

/**
 * The L4 compiler.
 * @author ppedemon
 */
object L4C {
  
  import bluejelly.utils._
  import Phase._
  
  private def appName = {
    val n = sys.props("app.name")
    if (n != null) n else "l4c"
  }
  
  private def appVersion = {
    val v = sys.props("app.version")
    if (v != null) v else "1.0"
  }

  private def versionStr = 
    "The l4 (low level lazy language) compiler, v%s" format appVersion
  
  private val cfg = new Config
  private val opts = List(
      ("-v", new UnitOpt(_ => cfg.version = true, "-v Show version information")),
      ("--debug-renamer", new UnitOpt(
          _ => cfg.stopAt = RENAME, 
          "--debug-renamer Send renamer results to stdout and stop")),
      ("--debug-occ", new UnitOpt(
          _ => cfg.stopAt = OCC, 
          "--debug-occ Send occurrence analysis output and stop")),
      ("--debug-inliner", new UnitOpt(
          _ => cfg.stopAt = INLINE, 
          "--debug-inliner Send inliner output to stdout and stop")),
      ("--debug-comp", new UnitOpt(
          _ => cfg.stopAt = COMP, 
          "--debug-comp Send compiler output to stdout and stop")),
      ("--debug-resolver", new UnitOpt(
          _ => cfg.stopAt = RESOLVE, 
          "--debug-resolver Send resolver to stdout output and stop")),
      ("--debug-opt", new UnitOpt(
          _ => cfg.stopAt = OPT, 
          "--debug-resolver Send optimizer output to stdout and stop")),        
      ("-S", new UnitOpt(
          _ => cfg.stopAt = FLATTEN, 
          "-S Create assembler file for the compiler module")),
      ("-d", new StrOpt(
          cfg.outDir = _,
          "-d [dir]~Output directory for compiled code")))      
  private val arg = new Args(
      opts, 
      cfg.files += _, 
      "Usage: " + appName + " [options] files...")

  // Compile a l4 file with the given name
  private def compile(name:String) {
    val r = new UnicodeFilter(new FileReader(name))
    val p = Parser.parseAll(Parser.module, r)
    p match {
      case f@Parser.Failure(_,_) => println(f)
      case Parser.Success(m,_) =>
        // Static analysis
        val result = StaticAnalysis.analyze(m)
        if (!result.isRight) return
        val m0 = Renamer.rename(m)
        if (cfg shouldStopAt RENAME) { ppr(m0); return }          

        // Occurrence analysis
        val m1 = OccAnalysis.analyze(m0)
        if (cfg shouldStopAt OCC) { ppr(m1); return }

        // Inline
        val m2 = Inliner.inline(m1)
        if (cfg shouldStopAt INLINE) { ppr(m2); return }
      
        // Compile
        val m3 = L4Compiler.compile(result.right.get, m2)
        if (cfg shouldStopAt COMP) { println(m3); return }
      
        // Resolve
        val m4 = Resolver.resolve(m3)
        if (cfg shouldStopAt RESOLVE) { println(m4); return }
      
        // Peephole optimization
        val m5 = PeepholeOptimizer.optimize(result.right.get, m4)
        if (cfg shouldStopAt OPT) { println(m5); return }

        // Flatten
        val m6 = Flatten.flatten(m5)
        if (cfg shouldStopAt FLATTEN) { saveAsm(m6); return }
        
        // If we reached this point, emit the class file!
        val asmCfg = new AsmConfig
        asmCfg.outDir = cfg.outDir
        val asm = new Assembler(asmCfg, m6)
        asm assemble
    }
  }
  
  // Save assembler source, used if we are compiling with -S
  private def saveAsm(m:bluejelly.asm.Module) {
    val full = new File(cfg.outDir, m.name.replaceAll("""\.""","/") + ".jas") toString
    val ix = full lastIndexOf '/'
    val path = full take ix
    new File(path).mkdirs()
    val out = new PrintWriter(new FileOutputStream(full))
    m.ppr(out)(0)
    out flush()
    out close()
  }

  // Pretty print a l4 module
  private def ppr(m:Module) {
    val d = PrettyPrinter.ppr(m)
    val w = new StringWriter
    d.format(75, w)
    print(w)    
  }

  // Magic starts here
  def main(argv:Array[String]) {
    if (!arg.parse(argv)) return
    if (arg.helpInvoked) return
    if (cfg.version) {println(versionStr); return}
    if (cfg.files.isEmpty) {
      println(appName + ": no input files")
      println("Use -h or --help for a list of possible options")
      return;
    }
    cfg.files foreach {compile _}
  }  
}
