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
import java.io.IOException
import java.io.PrintWriter
import java.io.Reader
import java.io.StringWriter
import java.util.Properties
import scala.collection.mutable.MutableList
import scala.util.parsing.input.NoPosition
import bluejelly.asm.Assembler
import bluejelly.utils.Args
import bluejelly.utils.Errors
import bluejelly.utils.StrOpt
import bluejelly.utils.UnicodeFilter
import bluejelly.utils.UnitOpt
import bluejelly.utils.Errors

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
class L4C {
  import Phase._
  import L4C.{exit_success,exit_failure}
  
  // Implement compiler pipeline
  private def compilerPipeline[T >: Errors](m:Module):Either[T,Array[Byte]] = {
    val result = StaticAnalysis.analyze(m)
    if (!result.isRight) return Left(result.left.get)
    
    val m0 = Renamer.rename(m)
    val m1 = OccAnalysis.analyze(m0)
    val m2 = Inliner.inline(m1)
    val m3 = L4Compiler.compile(result.right.get, m2)
    val m4 = Resolver.resolve(m3)
    val m5 = PeepholeOptimizer.optimize(result.right.get, m4)
    val m6 = Flatten.flatten(m5)
    val asmOut = Assembler.assemble(m6, false)
    asmOut
  }
  
  // Pass a file thru the compiler pipeline, return success or failure code;
  // this must be invoked only when compiling from the command line.
  private def process(cfg:Config)(name:String):Int = {
    val r = new UnicodeFilter(new FileReader(name))
    val p = Parser.parseAll(Parser.module, r)
    p match {
      case f@Parser.NoSuccess(msg,next) => 
        System.err.println(f)
        exit_failure
      case Parser.Success(m,_) =>
        // Static analysis
        val result = StaticAnalysis.analyze(m)
        if (!result.isRight) {
          result.left.get.dumpTo(new PrintWriter(System.err))
          return exit_failure
        }
        
        // Renamer
        val m0 = Renamer.rename(m)
        if (cfg shouldStopAt RENAME) { ppr(m0); return exit_success }          

        // Occurrence analysis
        val m1 = OccAnalysis.analyze(m0)
        if (cfg shouldStopAt OCC) { ppr(m1); return exit_success }

        // Inline
        val m2 = Inliner.inline(m1)
        if (cfg shouldStopAt INLINE) { ppr(m2); return exit_success }
      
        // Compile
        val m3 = L4Compiler.compile(result.right.get, m2)
        if (cfg shouldStopAt COMP) { println(m3); return exit_success }
      
        // Resolve
        val m4 = Resolver.resolve(m3)
        if (cfg shouldStopAt RESOLVE) { println(m4); return exit_success }
      
        // Peephole optimization
        val m5 = PeepholeOptimizer.optimize(result.right.get, m4)
        if (cfg shouldStopAt OPT) { println(m5); return exit_success }

        // Flatten
        val m6 = Flatten.flatten(m5)
        if (cfg shouldStopAt FLATTEN) { saveAsm(cfg, m6); return exit_success }
        
        // If we reached this point, assemble the module and emit class file
        val out = Assembler.assemble(m6, false)
        if (out.isLeft) {
          out.left.get.dumpTo(new PrintWriter(System.err))
          exit_failure
        } else {
          Assembler.save(cfg.outDir, m6.name, out.right.get)
          exit_success
        }
    }
  }
    
  // Save assembler source, used if we are compiling with -S
  private def saveAsm(cfg:Config, m:bluejelly.asm.Module) {
    val full = new File(cfg.outDir, m.name.replaceAll("""\.""","/") + ".jas").toString
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
}

/**
 * The L4 compiler.
 * @author ppedemon
 */
object L4C {
  
  import bluejelly.utils._
  import Phase._
  
  private val exit_success = 0
  private val exit_failure = 1
  
  private val appName = "l4c"
  
  private def appVersion = {
    try {
      val p = new Properties()
      p.load(classOf[L4C].getResourceAsStream("/l4c-cfg.properties"))
      p.getProperty("l4c.version")
    } catch {
      case e:IOException => "<?>"
    }
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
          "--debug-opt Send optimizer output to stdout and stop")),        
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

  /**
   * Compile a module.
   * @param m  module to compile
   * @return either bag of {@link Errors} or bytecode for the compiled class.
   */
  def compile(m:Module):Either[Errors,Array[Byte]] = {
    val l4c = new L4C
    l4c.compilerPipeline(m)
  }
  
  /**
   * Compile a module whose sources are provided by a {@link Reader}.
   * @param in    {@link Reader} providing module sources
   * @return either bag of {@link Errors} or bytecode for the compiled class.
   */
  def compile(in:Reader):Either[Errors,Array[Byte]] = {
    val r = new UnicodeFilter(in)
    val p = Parser.parseAll(Parser.module, r)
    p match {
      case f@Parser.NoSuccess(msg,next) => 
        val errs = new Errors(true)
        errs.error(next.pos, msg)
        Left(errs)
      case Parser.Success(m,_) =>
        compile(m)
    }
  }
  
  /**
   * Save a compiled module bytecode.
   * @param outDir        base output folder
   * @param moduleName    name of the compiled module
   * @param bytes         bytes of the compiled class we want to save
   */
  def save(outDir:String, moduleName:String, bytes:Array[Byte]) {
    Assembler.save(outDir, moduleName, bytes)
  }
  
  /**
   * Entry point for command line invocation.
   * @argv    command line arguments for the compiler
   */
  def main(argv:Array[String]) {
    if (!arg.parse(argv)) {
      System.exit(exit_failure)
      return
    }
    
    var exit_code = exit_success
    if (arg.helpInvoked)
      ()
    else if (cfg.version) 
      println(versionStr)
    else if (cfg.files.isEmpty) {
      println(appName + ": no input files")
      println("Use -h or --help for a list of possible options")
      exit_code = exit_failure
    } else {
      val l4c = new L4C
      val results = cfg.files map {l4c process(cfg)}
      exit_code = if (results.sum > 0) exit_failure else exit_success
    }
    System.exit(exit_code)
  }  
}
