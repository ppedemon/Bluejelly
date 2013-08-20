/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc

import scala.util.parsing.input.Reader
import bluejelly.bjc.parser.LayoutScanner

/**
 * Entry point for the Bluejelly compiler.
 * @author ppedemon
 */
object BluejellyCompiler {
  
  import parser.Lexer._
  
  private def scan(s:Reader[Token]):List[Token] = s.first match {
    case EOI() => Nil
    case t@ErrorToken(msg) => List(t)
    case t => t::scan(s.rest)
  }
  
  def main(args:Array[String]) {
    //val scanner = new LayoutScanner("where \n  x=1 \n where \n  y=2")
    val scanner = new LayoutScanner("{ -1 'a' x=1 \n  let \n   x=2 \n   x=3 \n   \"a\\    \\b\" {-wtf?-}\n}")
    val toks = scan(scanner)
    for (t <- toks) println("%s:%s" format (t.pos,t))
  }
}
