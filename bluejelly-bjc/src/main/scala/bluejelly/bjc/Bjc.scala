/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc

import bluejelly.bjc.ast.module
import bluejelly.bjc.core._
import bluejelly.bjc.parser.BluejellyParser
import bluejelly.bjc.static._

import bluejelly.utils.CompilerException
import bluejelly.utils.UnicodeFilter

import java.io.{Reader,StringReader,PrintWriter}

/**
 * Enum for the compiler stages.
 * @author ppedemon
 */
object BjcStage extends Enumeration {
  type BjcStage = Value
  var Parser, Reader, All = Value
}

/**
 * This class exposes the Bluejelly compiler API.
 * @author ppedemon
 */
class Bjc(
  fileName:String = "<unnamed>", 
  ifaceLoader:IfaceLoader = ProdLoader) {

  val bjcErrs = new BjcErrors(fileName)
  val loader = new ModuleLoader(ifaceLoader)

  def parse(r:Reader):Option[module.Module] = {
    val in = new UnicodeFilter(r)
    BluejellyParser.phrase(BluejellyParser.program, in) match {
      case BluejellyParser.NoSuccess(msg,in) => 
        bjcErrs.parseError(in.pos, msg);
        None
      case BluejellyParser.Success(m,_) => 
        Some(m)
    }
  }

  def chaseImports(gblEnv:GlobalEnv, mod:module.Module) = {
    val importChaser = new ImportChaser(loader, bjcErrs)
    importChaser.chaseImports(gblEnv, mod)    
  }

  def pipeline(r:Reader) {
    try {
      val mbMod = parse(r); abortIfErrors
      val gblEnv = new GlobalEnv(BjcEnv.withBuiltIns)
      val n_gblEnv = chaseImports(gblEnv, mbMod.get); abortIfErrors 
      println(n_gblEnv)
    } catch {
      case e:CompilerException => dumpErrors
    }
  }

  def hasErrors = bjcErrs.hasErrors 

  def abortIfErrors { 
    if (bjcErrs.hasErrors) bjcErrs.abort
  }

  def dumpErrors {
    bjcErrs.dumpTo(new PrintWriter(System.out))
  }
}

/**
 * Entry point for the Bluejelly compiler.
 * @author ppedemon
 */
object Bjc {
  def main(args:Array[String]) {
    val module = """import Z"""
    val bjc = new Bjc
    bjc.pipeline(new StringReader(module))
  }
}
