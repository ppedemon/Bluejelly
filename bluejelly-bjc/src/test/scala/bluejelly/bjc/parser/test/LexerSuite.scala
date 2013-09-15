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

import bluejelly.bjc.parser.{Lexer,Scanner,LayoutScanner}
import bluejelly.utils.{Name,UnicodeFilter}


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
      case ErrorToken(_) =>
        val e = expected.head
        assert(e.isInstanceOf[ErrorToken], 
            "expected error, but got: %s" format e)
        checkLexerOutput(s.rest, expected.tail)
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

  test("Lexer must ignore comments properly") {
    testScanner("empty.in", Nil)
  }
  
  test("Lexer must lex literals correctly") {
    testScanner("literals.in", Seq(
        IntLit(1),
        IntLit(BigInt("123456789abcdef",16)),
        IntLit(BigInt("123456789abcdef",16)),
        IntLit(0107),
        IntLit(0200),
        FloatLit(1.4e3),
        FloatLit(1.4e3),
        FloatLit(1.4e-3),
        FloatLit(8e2),
        ErrorToken(""),
        FloatLit(0),
        CharLit('a'),
        CharLit('\7'),
        CharLit('\b'),
        CharLit('\f'),
        CharLit('\n'),
        CharLit('\r'),
        CharLit('\t'),
        CharLit('\13'),
        CharLit('\\'),
        CharLit('\''),
        CharLit('"'),
        CharLit('"'),
        CharLit('\127'),
        ErrorToken(""),
        ErrorToken(""),
        StringLit("Lorem Ipsum \\'blah blah blah\\\"''\"\n"),
        StringLit("你好世界"),
        StringLit("你好世界")))
  }
  
  test("Lexer must lex identifiers and symbols correctly") {
    testScanner("idents.in", Seq(
        VarId(Name("a'b")),
        QVarId(Name("M","a")),
        QVarId(Name("bluejelly.lexer.Lexer","lex")),
        ConId(Name("List")),
        QConId(Name("List","List")),
        QConId(Name("bluejelly.List","List")),
        VarSym(Name("-->")),
        QVarSym(Name("M",".+.")),
        QVarSym(Name("bluejelly.lexer.Lexer",".:.")),
        ConSym(Name(":+")),
        QConSym(Name("Complex",":+")),
        QConSym(Name("bluejelly.Complex",":+")),
        VarId(Name("f")),
        VarSym(Name(".")),
        VarId(Name("g")),
        QVarId(Name("F","g")),
        VarId(Name("f")),
        TDotDot(),
        QVarSym(Name("F",".")),
        ConId(Name("F")),
        VarSym(Name("."))
    ))
  }
    
  test("Lexer must handle (reserved) symbols correctly") {
    testScanner("reserved.in", Seq(
      TLParen(), TRParen(), TLBrack(), 
      TRBrack(), TLCurly(), TRCurly(),
      TComma() , TSemi()  , TBack(),
      VarSym(Name("->>")),
      VarSym(Name("...")),
      ConSym(Name(":::")),
      TCoCo(),
      ConSym(Name(":")),
      QConSym(Name("Prelude",":")),
      VarSym(Name("@~")),
      TAt(),
      TTilde(),
      VarSym(Name("=>=")),
      TDArr(),
      VarSym(Name("<=")),
      TEq(),
      VarSym(Name(">"))
    ))
  }
  
  test("Lexer must insert layout tokens correctly") {
    testLayoutScanner("layout.in", Seq(
        VLCurly(),
        TWhere(),
        VLCurly(),
        VarId(Name("x")), TEq(), IntLit(1),
        TSemi(),
        VarId(Name("y")), TEq(), IntLit(2),
        TSemi(),
        TWhere(),
        VLCurly(), VRCurly(),
        VRCurly(),
        TSemi(),
        VarId(Name("a")),
        //
        TSemi(),
        TWhere(), VLCurly(),
        VarId(Name("x")), TEq(), IntLit(1),
        TWhere(), VLCurly(), VarId(Name("y")), TEq(), IntLit(2), VRCurly(),
        VRCurly(),
        //
        TSemi(),
        TDo(),
        TLCurly(),
        VarId(Name("x")), TLArr(), VarId(Name("f")), TSemi(),
        VarId(Name("y")), TLArr(), VarId(Name("g")), TSemi(),
        VarId(Name("return")), VarSym(Name("$")), 
        VarId(Name("x")), VarId(Name("y")),
        TRCurly(),
        VRCurly()
    ))
  }
}
