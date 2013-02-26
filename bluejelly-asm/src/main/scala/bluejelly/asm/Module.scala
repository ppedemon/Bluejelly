/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm

import java.io.Writer

import scala.collection.mutable.MutableList
import scala.util.parsing.input.Positional

/**
  * A function with a given arity, possibly a matcher 
  * (i.e., performing pattern matching over some value).
  * 
  * @param name      function name
  * @param arity     function arity
  * @param matcher   is this function a matcher?
  * @param block     function body
  */
final class Function(
    val name:String, 
    val arity : Int, 
    val matcher : Boolean, 
    val b:Block) extends PrettyPrintable with Positional {

  // Pretty print a function
  def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x + ".fun")  
    val attrs = MutableList[String]()
    if (arity > 0) attrs += ("arity=%d" format arity)
    if (matcher) attrs += "matcher"
    if (attrs.length > 0) w write attrs.mkString("[",",","]")
    w write (" %s:\n" format name)
    b.ppr(w)(x+2)
    w write ("\n" + " "*x + ".end\n")
  }
}

/**
  * A module.
  * 
  * @param name     module name
  * @param dicts    dictionaries declares in the module 
  * @param funcs    functions declared in the module
  */
final class Module(
    val name:String, 
    val funcs:List[Function]) extends PrettyPrintable {
  
  // Pretty print a module
  def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x + ".module %s:\n" format name)
    for (f <- funcs) { w write '\n'; f.ppr(w)(x) }
    w write ("\n.end\n")
  }
}
