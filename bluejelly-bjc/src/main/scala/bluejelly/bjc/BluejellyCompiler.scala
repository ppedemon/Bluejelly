/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc

import scala.util.parsing.input.CharSequenceReader

/**
 * Entry point for the Bluejelly compiler.
 * @author ppedemon
 */
object BluejellyCompiler {
  
  import Lexer._
  
  private def scan(s:Scanner):List[Token] = s.first match {
    case EOI => Nil
    case t@ErrorToken(msg) => List(t)
    case t => t::scan(s.rest)
  }
  
  def main(args:Array[String]) {
    val scanner = new Scanner("\t-----| foo_F_G |-- {- Nested {-\n\n-} Comments {-{--}-}-} M.+. -> x.F.f.g x.F.g x.F.f.. x.F.. x.F:. \"groovy \\\t\\string\\n\" 0xBAD")
    val toks = scan(scanner)
    for (t <- toks) println("%s:%s" format (t.pos,t))
  }
}
