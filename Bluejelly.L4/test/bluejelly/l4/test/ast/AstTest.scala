/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test.ast

import junit.framework.TestCase
import java.io.StringReader
import bluejelly.l4.FunDecl
import bluejelly.l4.Parser
import bluejelly.l4.Module
import java.io.Reader
import java.io.StringWriter
import bluejelly.l4.PrettyPrinter

/**
 * Base class for test of stages transforming an AST.
 * @author ppedemon
 */
abstract class AstTest extends TestCase {

  val utils = new TestUtils
  
  @throws(classOf[IllegalArgumentException])
  protected def parseMod(r:Reader):Module = {
    val result = Parser.parseAll(Parser.module, r)
    result match {
      case f@Parser.Failure(_,_) => throw new IllegalArgumentException(f toString)
      case Parser.Success(m,_) => m
    }
  }
  
  @throws(classOf[IllegalArgumentException])
  protected  def parseMod(s:String):Module = parseMod(new StringReader(s))

  @throws(classOf[IllegalArgumentException])
  protected  def parseFun(r:Reader):FunDecl = {
    val result = Parser.parseAll(Parser.funDecl, r)
    result match {
      case f@Parser.Failure(_,_) => throw new IllegalArgumentException(f toString)
      case Parser.Success(f,_) => f
    }
  }
  
  @throws(classOf[IllegalArgumentException])
  protected  def parseFun(s:String):FunDecl = parseFun(new StringReader(s))
  
  protected def ppr(m:Module) {
    val d = PrettyPrinter.ppr(m)
    val w = new StringWriter
    d.format(75, w)
    print(w)    
  }
}
