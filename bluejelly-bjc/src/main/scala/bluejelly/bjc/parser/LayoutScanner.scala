/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.parser

import scala.collection.immutable.Stack
import scala.util.parsing.input.{Reader,Position}
import Lexer._

/**
 * We specify the rules for inserting layout tokens as a function 
 * defining, given the current scanner, the next token to return 
 * and the next scanner.
 * 
 * @autor ppedemon
 */
object LayoutScanner {
  type Behavior = LayoutScanner => (Token,LayoutScanner)
  
  private def init:Behavior = s => first(s) match {
    case t@ErrorToken(_) => 
      error(t)
    case TModule() | TLCurly() =>
      normal()(s)
    case t =>
      val ctx1 = s.ctx.push(t.pos.column)
      (vlcurly(t), new LayoutScanner(s.in, normal(true), ctx1))
  } 
    
  private def normal(noSemi:Boolean = false):Behavior = s => first(s) match {
    case t@ErrorToken(_) => 
      error(t)
    case t if !noSemi && isSame(t, s.ctx) =>
      (semi(t), new LayoutScanner(s.in, normal(true), s.ctx))
    case t if !noSemi && isDedent(t, s.ctx) =>
      (vrcurly(t), new LayoutScanner(s.in, normal(), s.ctx.pop))
    case t if isHotToken(t) =>
      (t, new LayoutScanner(rest(s), maybeNewCtx, s.ctx))
    case t@TLCurly() =>
      (t, new LayoutScanner(rest(s), normal(true), s.ctx.push(0)))
    case t@TRCurly() if inExplicitLayout(s.ctx) =>
      (t, new LayoutScanner(rest(s), normal(), s.ctx.pop))
    case t@TRCurly() =>
      error(t.pos, "unexpected token: `}'")
    case t@EOI() if inImplicitLayout(s.ctx) =>
      (vrcurly(t), new LayoutScanner(rest(s), normal(true), s.ctx.pop))
    case t@EOI() if !s.ctx.isEmpty =>
      error(t.pos, "parser error (possibly bad indentation)")
    case t => 
      (t, new LayoutScanner(rest(s), normal(), s.ctx))
  }
    
  private def maybeNewCtx:Behavior = s => first(s) match {
    case t@ErrorToken(_) =>
      error(t)
    case t@TLCurly() => 
      normal()(s)
    case t if isIndent(t, s.ctx) =>
      val ctx1 = s.ctx.push(t.pos.column)
      (vlcurly(t), new LayoutScanner(s.in, normal(true), ctx1))
    case t => 
      (vlcurly(t), new LayoutScanner(s.in, emptyCtx(t), s.ctx))
  }
    
  private def emptyCtx(t:Token):Behavior = s =>
    (vrcurly(t), new LayoutScanner(s.in, normal(), s.ctx))
      
  // ---------------------------------------------------------------------
  // Helper stuff
  // ---------------------------------------------------------------------

  private def first(s:LayoutScanner) = s.in.first
  private def rest(s:LayoutScanner) = s.in.rest
      
  private def inExplicitLayout(ctx:Stack[Int]) = 
    !ctx.isEmpty && ctx.top == 0
  
  private def inImplicitLayout(ctx:Stack[Int]) = 
    !ctx.isEmpty && ctx.top > 0
    
  private def isHotToken(t:Token) = t match {
    case TWhere()|TLet()|TDo()|TOf() => true
    case _ => false
  }
  
  private def isIndent(t:Token, ctx:Stack[Int]) = 
    (ctx.isEmpty && t != EOI()) || (!ctx.isEmpty && t.pos.column > ctx.top)
  
  private def isSame(t:Token, ctx:Stack[Int]) = 
    t != EOI() && !ctx.isEmpty && t.pos.column == ctx.top
  
  private def isDedent(t:Token, ctx:Stack[Int]) =   
    !ctx.isEmpty && t.pos.column < ctx.top
    
  private def vlcurly(t:Token) = {
    val vlcurly = new VLCurly
    vlcurly.setPos(t.pos)
    vlcurly
  }
   
  private def semi(t:Token) = {
    val semi = new TSemi
    semi.setPos(t.pos)
    semi
  }
  
  private def vrcurly(t:Token) = {
    val vrcurly = new VRCurly
    vrcurly.setPos(t.pos)
    vrcurly
  }
  
  private def error(p:Position, msg:String):(Token,LayoutScanner) =
    error(errorToken(p,msg))
  
  private def error(t:ErrorToken):(Token,LayoutScanner) =
    (t, new LayoutScanner(new Scanner(""), normal(), Stack()))
}

import LayoutScanner.{Behavior,init}

/**
 * Wrapper over a plain Scanner. Layout tokens are inserted here.
 * @author ppedemon
 */
class LayoutScanner(
    val in:Scanner,
    val b:Behavior,
    val ctx:Stack[Int]) extends Reader[Token] {

  def this(in:Scanner) = this(in, init, Stack())
  def this(in:String) = this(new Scanner(in))
  def this(in:java.io.Reader) = this(new Scanner(in))
  
  // This HAS to be lazy, otherwise we would chain
  // LayoutScanner constructor calls... that's bad!
  private lazy val (tok,next) = b(this)
  
  override def source = in.source
  override def offset = in.offset
  def pos = tok.pos
  def atEnd = in.atEnd
  
  def first = tok
  def rest = next
  
  /*
   * So the parse can do the right thing in cases like:
   * 
   *  f x = let z = x+1 in z 
   * 
   * The layout rules implemented by the LayouyScanner object
   * will never insert a VLCurly before the `in' token. So the 
   * parser has to handle the error by trying to pop the current 
   * layout context if possible.
   */
  def inImplicitLayout = LayoutScanner.inImplicitLayout(ctx)
  def popCurrentLayout = new LayoutScanner(in,b,ctx.pop)
}
