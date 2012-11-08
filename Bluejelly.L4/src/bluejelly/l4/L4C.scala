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
import scala.util.parsing.input.StreamReader
import java.io.StringReader

/**
 * The L4 compiler.
 * @author ppedemon
 */
object L4C {
  def main(argv:Array[String]) {
    val r = new UnicodeFilter(new FileReader(argv(0)))
    val p = Parser.parseAll(Parser.module, r)
    p match {
      case f@Parser.Failure(_,_) => println(f)
      case Parser.Success(m,_) => {
          // Static analysis
          val result = StaticAnalysis.analyze(m)
          if (result.isRight) return
          
          // Optimize
          val m1 = Inliner.inline(OccAnalysis.analyze(m))

          // Compile
          new L4ToAsm(m1, result.left.get) compile
          val d = PrettyPrinter.ppr(m1)
          val w = new StringWriter
          d.format(75, w)
          print(w)
      }
    }
  }
}
