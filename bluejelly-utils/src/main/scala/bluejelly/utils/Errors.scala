/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.utils

import java.io.PrintWriter

import scala.collection.mutable.MutableList
import scala.util.parsing.input.Position

/**
 * No-arg exception signaling a compiler error.
 * @author ppedemon
 */
case class CompilerException() extends Exception

/**
 * Errors to store in the bag.
 * @author ppedemon
 */
trait ErrorItem
case class Error(val pos:Position, val msg:String) extends ErrorItem {
  override def toString() = "error: %s" format msg
}
case class Warning(val pos:Position, val msg:String) extends ErrorItem {
  override def toString() = "warning: %s" format msg
}

/**
 * General-purpose error bag.
 * @author ppedemon
 */
class Errors(var hasErrors:Boolean = false) {
  val items:MutableList[ErrorItem] = MutableList()
  
  @throws(classOf[CompilerException])
  def abort() { throw new CompilerException }
  
  def warning(pos:Position, msg:String) { items += Warning(pos, msg) }
  def error(pos:Position, msg:String)   { hasErrors = true; items += Error(pos, msg)}
  
  @throws(classOf[CompilerException])
  def fatal(pos:Position, msg:String)   { error(pos, msg); abort() }
  
  def dumpTo(out:PrintWriter) {
    items foreach {out.println(_)}
    out.flush()
  }
}
