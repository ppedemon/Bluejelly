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

import java.io.{File,Reader,FileReader,StringReader,PrintWriter}

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

  def chaseImports(bjcEnv:BjcEnv, mod:module.Module) = {
    val importChaser = new ImportChaser(bjcEnv)
    importChaser.chaseImports(mod)    
  }

  def pipeline(r:Reader) {
    try {
      // Parse
      val mod = bjcTry(parse(r)).get
      
      // Import chasing
      val bjcEnv = new BjcEnv(mod.name, bjcErrs, loader)
      val nameTab = bjcTry(chaseImports(bjcEnv, mod))

      // TODO: add the rest...
      println(nameTab)
    } catch {
      case e:CompilerException => dumpErrors
    }
  }

  def hasErrors = bjcErrs.hasErrors 

  def bjcTry [T](f: =>T):T = {
    val t = f;
    if (hasErrors) bjcErrs.abort
    t
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
    val f = new File(args(0))
    val r = new FileReader(f)
    val bjc = new Bjc(f.getName)
    bjc.pipeline(r)
  }
}
