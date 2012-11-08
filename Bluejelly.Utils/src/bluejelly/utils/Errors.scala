/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.utils

/**
 * No-arg exception signaling a compiler error.
 */
case class CompilerException() extends Exception

/**
 * General-puspose error bag.
 * @author ppedemon
 */
class Errors(var hasErrors:Boolean = false) {
  @throws(classOf[CompilerException])
  def abort() { throw new CompilerException }
  
  def warning(msg:String) { System.err.println("warning: " + msg) }
  def error(msg:String)   { hasErrors = true; System.err.println("error: " + msg) }
  
  @throws(classOf[CompilerException])
  def fatal(msg:String)   { error(msg); abort() }
}
