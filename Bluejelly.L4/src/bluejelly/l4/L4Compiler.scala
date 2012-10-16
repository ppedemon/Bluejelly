/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import bluejelly.utils.UnicodeFilter
import java.io.FileReader
import java.io.StringWriter

/**
 * The L4 compiler.
 * @author ppedemon
 */
object L4Compiler {
  def main(argv:Array[String]) {
    //val r = new UnicodeFilter(new FileReader(argv(0)))
    val p = Parser.parseAll(Parser.module, argv(0))
    p match {
      case f@Parser.Failure(_,_) => println(f)
      case Parser.Success(m,_) => {
        val m1 = Inliner.inline(OccAnalysis.analyze(m))
        val d = PrettyPrinter.ppr(m1)
        val w = new StringWriter
        d.format(75, w)
        print(w)
      }
    }
  }
}
