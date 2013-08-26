/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.parser

import scala.util.parsing.combinator.Parsers

/**
 * Bluejelly parser.
 * @author ppedemon
 */
object BluejellyParser extends Parsers {
  
  import Lexer._
  type Elem = Token
  
  // Rules for parameter-less tokens
  private def module  = elem("`module'", _.isInstanceOf[TModule])
  private def where   = elem("`where'", _.isInstanceOf[TWhere])
  private def vlcurly = elem("`{'", _.isInstanceOf[VLCurly])
  private def vrcurly = elem("`}'", _.isInstanceOf[VRCurly])
  
  // Module Id
  private def modid = elem("module name", _.isInstanceOf[ConId]) ^^ {
    case ConId(id) => id
  }

  // EOI token
  private def eoi = elem("end of input", _.isInstanceOf[EOI])
  
  // Render lexical errors reasonably
  private def lexError = elem("", _.isInstanceOf[ErrorToken]) >> {
    case ErrorToken(s) => Parser{ in => new Error(s,in) {
      override def toString = "[%s] lexical error: %s" format (next.pos,msg)  
    }}
  }

  // A program
  def program = 
    module ~> modid <~ (where ~ vlcurly ~ vrcurly) |
    lexError

  // Parser entry points
  def phrase[T](p:Parser[T], in:String) = 
    super.phrase(p <~ eoi)(new LayoutScanner(in))
  def phrase[T](p:Parser[T], in:java.io.Reader) = 
    super.phrase(p <~ eoi)(new LayoutScanner(in))
}
