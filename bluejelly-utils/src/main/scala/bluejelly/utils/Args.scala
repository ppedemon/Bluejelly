/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.utils

import scala.util.control.Exception._
import scala.collection.immutable.SortedMap
import scala.math.max;

/**
 * Exception signaling an error while parsing command line options.
 */
class ArgException(msg:String) extends Exception(msg)

/**
 * Abstract class for command line options. Subclasses 
 * must define the [[consume]] method.
 * 
 * @param doc    documentation string for the option
 * @author ppedemon
 */
abstract class Opt(val doc:String) {
  /**
   * Process this option.
   * 
   * @param optName   option name
   * @param args      rest of command line
   * @return rest of command line, same as args if option takes no arguments
   */
  def consume(optName:String,args:List[String]):List[String]
  
  /**
   * Signal an error while processing this option.
   * @param optName    option name
   * @param msg        error message
   * @throws ArgException with a message based on the [[msg]] parameter
   */
  @throws(classOf[ArgException])
  def error(optName:String,msg:String) = 
    throw new ArgException("`%s' option %s" format (optName,msg))
}

/**
 * Command line option argument taking no arguments.
 */
class UnitOpt(f:Unit => Unit,doc:String) extends Opt(doc) {
  def consume(optName:String,args:List[String]) = { f(); args }
}

/**
 * Command line option taking a string argument
 */
class StrOpt(f:String => Unit,doc:String) extends Opt(doc) {
  def consume(optName:String,args:List[String]) = args match {
    case List() => error(optName, "requires an argument")
    case x::xs  => { f(x); xs }
  }
}

/**
 * Command line option taking an integer argument.
 */
@throws(classOf[ArgException])
class IntOpt(f:Int => Unit,doc:String) extends Opt(doc) {
  def consume(optName:String,args:List[String]) = args match {
    case List() => error(optName, "requires an argument")
    case x::xs  => 
      handling(classOf[NumberFormatException])
        .by(_ => error(optName, ": invalid argument: " + x))({ f(x.toInt);xs })
  }
}

/**
 * Command line option taking a double precision argument.
 */
@throws(classOf[ArgException])
class DoubleOpt(f:Double => Unit,doc:String) extends Opt(doc) {
  def consume(optName:String,args:List[String]) = args match {
    case List() => error(optName, "requires an argument")
    case x::xs  => 
      handling(classOf[NumberFormatException])
        .by(_ => error(optName, ": invalid argument: " + x))({ f(x.toDouble);xs })
  }
}

/**
 * Command line option consuming multiple arguments.
 */
@throws(classOf[ArgException])
class ListOpt(xs:List[Opt],doc:String) extends Opt(doc) {
  def consume(optName:String,args:List[String]) = 
    (args /: xs)((args,x) => x consume (optName,args))
}

/**
 * Command line processor class.
 * @author ppedemon
 * 
 * @param optInfo
 *   list of (option name, Opt instance) pairs.
 *   Invariant: option names must start with an &quot;-&quot;
 * @param anon
 *   defines what to do with non-option arguments (i.e., those not starting
 *   with a dash). Once we see one such argument, the remaining arguments 
 *   are assumed to be non-option, even if they start with a dash.
 * @param usage
 *   string explaining application usage
 * @param helpHook
 *   function to invoke when user asks for help, only if help options
 *   are auto generated. This way, you have a chance to notice that the
 *   user asked for the help option.
 */
class Args(
    optInfo:List[(String,Opt)], 
    anon:String => Unit, 
    usage:String) {
  
  // Flag signaling if help was invoked
  var helpInvoked = false
  
  // Option map, augmented with help options if necessary
  private val opts:Map[String,Opt] = withHelp(SortedMap(optInfo:_*))
  
  // Augment option map with help options (if not already there)
  private def withHelp(opts:Map[String,Opt]):Map[String,Opt] = {
    (opts /: List("-h","--help")) {(m,h) =>
      if (m contains h) m else m + (h -> 
        new UnitOpt(_ => {println(helpMsg);helpInvoked = true}, 
          h + " Print a synopsys of available options"))
    }  
  }
    
  // Nicely formatted msg explaining available options 
  private def docMsg:String = {
    def splitIx(s:String) = max(0, max(s indexOf ' ', s indexOf '~'))
    val pairs = opts.values.map(
        ((p:(String,String)) => (p._1,p._2 substring 1))
        compose ((s:String) => s splitAt (splitIx(s))) 
        compose (_.doc))
    val maxLen = pairs.maxBy(_._1.length)._1.length
    (("",true) /: pairs){case ((s,first),(o,d)) => 
      val line = "  " + o + (" "*(maxLen - o.length)) + "\t" + d
      if (first) (s + line,false) else (s + "\n" + line, false)
    }._1
  }
  
  // Is the given string an option?
  private def isOpt(s:String) = s startsWith "-"
  
  // What to do when we find a option not belonging to the opts map
  private def invalidOpt(optName:String) { 
    throw new ArgException("Invalid option: " + optName)
  }
  
  // Parse command line
  @throws(classOf[ArgException])
  private def parse(
      argv:List[String], 
      inRest:Boolean):Unit = argv match {
    case List() => ()
    case a::as if !inRest && isOpt(a) && (opts contains a) =>
      (parse(opts(a) consume (a,as), false))
    case a::as if !inRest && isOpt(a) => invalidOpt(a)
    case a::as => { anon(a); parse(as, true) }
  }
  
  /**
   * Nicely formatted usage message
   */
  def errMsg(msg:String) = 
    "%s\n%s\nUse -h or --help for a list of possible options" format (msg,usage)
  
  /**
   * Nicely formatted help message
   */
  def helpMsg = "%s\n%s\n" format (usage,docMsg)

  /**
   * Parse command line.
   * @param argv    command line to process
   * @return        whether command line was successfully parsed
   */
  def parse(argv:Array[String]):Boolean = {
    handling(classOf[ArgException]).by({e => 
      System.err.println(errMsg(e getMessage)); 
      false
    }){
      parse(argv.toList, false); 
      true
    }
  }
  
}
