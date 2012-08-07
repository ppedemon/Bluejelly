package bluejelly.asm

import java.io.Writer
import scala.collection.mutable.MutableList

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
    val b:Block) extends PrettyPrintable {

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
  * A dictionary. For all names on ''supers'', ''methods'', 
  * and ''specifics'', order '''is''' important.
  * 
  * @param name         dictionary name
  * @param supers       name of super class dictionaries
  * @param methods      name of functions implementing the dictionary methods
  * @param specifics    name of dictionaries providing instance specific definitions 
  */
final class Dictionary(
    val name:String,
    val supers:List[String], 
    val methods:List[String], 
    val specifics:List[String]) extends PrettyPrintable {
  
  // Pretty print a dictionary
  def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x + ".dict %s:\n" format name)
    for (z <- supers) w write (" "*(x+2) + ".super %s\n" format z)
    for (z <- methods) w write (" "*(x+2) + ".method %s\n" format z)
    for (z <- specifics) w write (" " *(x+2) + ".specific %s\n" format z)
    w write (".end\n")
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
  val dicts:List[Dictionary], 
  val funcs:List[Function]) extends PrettyPrintable {
  
  // Pretty print a module
  def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x + ".module %s:\n" format name)
    for (d <- dicts) { w write '\n'; d.ppr(w)(x) }
    for (f <- funcs) { w write '\n'; f.ppr(w)(x) }
    w write ("\n.end\n")
  }
}
