/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.parser

import scala.annotation.tailrec
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.{Reader, CharArrayReader, PagedSeqReader}
import Lexer._

/**
 * Emit tokens to be processed by the parser. We do whitespace processing
 * here at the char level, so we can handle nested comments properly.
 * 
 * @author ppedemon
 */
class Scanner(in:Reader[Char]) extends Reader[Token] {
  import CharArrayReader.EofCh
  
  def this(in:String) = this(new CharArrayReader(in.toCharArray()))
  def this(in:java.io.Reader) = this(new PagedSeqReader(PagedSeq.fromReader(in)))

  private val (tok,rest1,rest2) = 
    (skipWhiteStuff(in)) match {
      case (in1,false) => 
        val empty = new CharArrayReader(Array())
        (errorToken(in.pos, "unclosed comment"), empty, empty)
      case (in1,_) => token(in1) match {
        case Lexer.Success(tok, in2) => (tok, in1, in2)
        case ns:Lexer.NoSuccess => 
          (errorToken(ns.next.pos,ns.msg), ns.next, skip(ns.next))
      }
    }

  @tailrec
  private def peek(in:Reader[Char], n:Int, s:String=""):String = 
    if (n == 0 || in.atEnd) s else peek(in.rest, n-1, s + in.first)
  
  @tailrec
  private def skipWhile(in:Reader[Char], p:Char=>Boolean):Reader[Char] = 
    in.first match {
      case EofCh => in
      case c if p(in.first) => skipWhile(in.rest, p)
      case _ => in
    }

  @tailrec
  private def skip(in:Reader[Char], n:Int):Reader[Char] = 
    if (n == 0) in else skip(in.rest, n-1)
  
  private def skipWhitespace(in:Reader[Char]):Reader[Char] = 
    skipWhile(in, _.isWhitespace)

  private def skipLineComment(in:Reader[Char]) = 
    if (peek(in,2) == "--")
      skipWhile(in, _ == '-') match {
        case in1 if in.atEnd => in1
        case in1 if !isSymbol(in1.first) => skipWhile(in1, c => c != '\n' && c != '\r')
        case _ => in 
      }      
    else in
    
  private def skipNestedComments(in:Reader[Char]):(Reader[Char],Boolean) = 
    peek(in,2) match {
      case "{-" => skipInside(skip(in,2),1)
      case _ => (in,true)
    }
    
  @tailrec
  private def skipInside(in:Reader[Char], level:Int):(Reader[Char],Boolean) = 
    peek(in,2) match {
      case "{-" => skipInside(skip(in,2), level+1)
      case "-}" if level == 1 => (skip(in,2),true)
      case "-}" => skipInside(skip(in,2), level-1)
      case _ if in.atEnd => (in,false)
      case _ => skipInside(in.rest, level)
    }

  @tailrec
  private def skipWhiteStuff(in:Reader[Char]):(Reader[Char],Boolean) = {
    val in1 = skipWhitespace(in)
    val in2 = skipLineComment(in1)
    val (in3,ok) = skipNestedComments(in2)
    if (!ok || in3 == in) (in3,ok) else skipWhiteStuff(in3)
  }
  
  private def skip(in:Reader[Char]) = if (in.atEnd) in else in.rest
  
  override def source = in.source
  override def offset = in.offset
  def first = tok
  def rest = new Scanner(rest2)
  def pos = rest1.pos
  def atEnd = in.atEnd || (skipWhiteStuff(in) match {
    case (_,false) => true
    case (in1,_) => in1.atEnd
  })
}
