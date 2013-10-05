/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc

import java.io.InputStreamReader
import java.io.FileReader

import scala.util.parsing.input.Reader

import bluejelly.bjc.parser.{Scanner,LayoutScanner,BluejellyParser}
import bluejelly.utils.UnicodeFilter

/**
 * Entry point for the Bluejelly compiler.
 * @author ppedemon
 */
object BluejellyCompiler {
  
  import parser.Lexer._
  
  private def scan(s:Reader[Token]):List[Token] = s.first match {
    case EOI() => Nil
    case t@ErrorToken(msg) => t::scan(s.rest)
    case t => t::scan(s.rest)
  }
  
  def main(args:Array[String]) {
    val start = System.currentTimeMillis()    
    val in = new UnicodeFilter(new FileReader("tmp/prog.in"))
    val result = BluejellyParser.phrase(BluejellyParser.program, in)
    printf("Parsing time: %d\n", System.currentTimeMillis()-start)
    
    result match {
      case err@BluejellyParser.NoSuccess(_,_) => println(err)
      case BluejellyParser.Success(m,_) => println(m)
    }
    /*
    val start = System.currentTimeMillis
    val in = new UnicodeFilter(new FileReader("tmp/prog.in"))
    val scanner = new /*Layout*/Scanner(in)
    val toks = scan(scanner)
    for (t <- toks) println("%s:%s:\n%s" format (t.pos,t,t.pos.longString))
    val end = System.currentTimeMillis()
    printf("Lexing time: %d\n", end-start)
    */
    /*
    val scanner = new LayoutScanner(""" --- ---> M.--+ M.--- """)
    val toks = scan(scanner)
    for (t <- toks) println("%s:%s:\n%s" format (t.pos,t,t.pos.longString))
    */
  }
}
