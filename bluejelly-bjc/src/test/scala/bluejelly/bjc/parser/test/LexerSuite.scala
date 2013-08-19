/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.parser.test

import java.io.InputStreamReader
import scala.util.parsing.input.Reader
import org.scalatest.FunSuite

import bluejelly.utils.UnicodeFilter
import bluejelly.bjc.parser.{Lexer,Scanner,LayoutScanner}


/**
 * Test suite for Bluejelly lexer. We test here the behavior of the
 * lexer, the scanner and the layout sensitive scanner.
 * 
 * @author ppedemon
 */
class LexerSuite extends FunSuite {

  import Lexer._
  
  private def readerFor(fileName:String) = {
    val s = classOf[LexerSuite].getResourceAsStream("/lexer.tests/" + fileName)
    new UnicodeFilter(new InputStreamReader(s))
  }
  
  private def checkLexerOutput(s:Reader[Token], expected:Seq[Token]) {
    s.first match {
      case EOI() =>
        assert(expected.isEmpty, "premature EOI, expected: %s" format expected)
      case t => 
        var e = expected.head
        assert(t == expected.head, "wrong token: %s, expected %s" format (t,e))
        checkLexerOutput(s.rest, expected.tail)
    }
  }
  
  private def testLayoutScanner(fileName:String, expected:Seq[Token]) {
    val s = new LayoutScanner(readerFor(fileName))
    checkLexerOutput(s, expected)
  }

  private def testScanner(fileName:String, expected:Seq[Token]) {
    val s = new Scanner(readerFor(fileName))
    checkLexerOutput(s, expected)
  }

  test("Lexer must lex literals correctly") {
    testScanner("literals.in", Seq(
        IntLit(1),
        IntLit(BigInt("123456789abcdef",16)),
        IntLit(BigInt("123456789abcdef",16)),
        IntLit(0107),
        IntLit(0200)))
  }
}
