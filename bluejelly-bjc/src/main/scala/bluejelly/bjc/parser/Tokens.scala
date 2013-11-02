/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.parser

import scala.math.BigInt
import scala.util.parsing.input.{Position,Positional}

import bluejelly.utils.Name

/**
 * Tokens in the bluejelly top-level language. 
 * @author ppedemon
 */
trait Tokens {
  
  // Auxiliary, for giving decent parse error messages
  def identifier(s:String) = "identifier `%s'" format s
  def operator(s:String) = "operator `%s'" format s
  def kwd(s:String) = "keyword `%s'" format s 
  def badLayout(s:String) = "%s, possibly due to bad layout" format s

  // Abstract tokens (we know the position of every token)
  sealed abstract class Token extends Positional {
    val unexpected:String
  }

  // Identifiers
  case class VarId(val id:Name) extends Token {
    val unexpected = identifier(id.toString)
  }
  case class ConId(val id:Name) extends Token {
    val unexpected = identifier(id.toString)
  }
  case class QVarId(val id:Name) extends Token {
    val unexpected = identifier(id.toString)
  }
  case class QConId(val id:Name) extends Token {
    val unexpected = identifier(id.toString)
  }
  
  // Symbols
  case class VarSym(val sym:Name) extends Token {
    val unexpected = operator(sym.toString)
  }
  case class ConSym(val sym:Name) extends Token {
    val unexpected = operator(sym.toString)
  }
  case class QVarSym(val sym:Name) extends Token {
    val unexpected = operator(sym.toString)
  }
  case class QConSym(val sym:Name) extends Token {
    val unexpected = operator(sym.toString)
  }
  
  // Literals
  case class FloatLit(val x:Double) extends Token {
    val unexpected = "floating point literal"
  }
  case class IntLit(val x:BigInt) extends Token {
    val unexpected = "integer literal"
  }
  case class CharLit(val x:Char) extends Token {
    val unexpected = "character literal"
  }
  case class StringLit(val x:String) extends Token {
    val unexpected = "string literal"
  }
  
  // Keywords
  case class TAs() extends Token { 
    val unexpected = identifier("as") 
  }
  case class TCase() extends Token { 
    val unexpected = kwd("case") 
  }
  case class TClass() extends Token { 
    val unexpected = kwd("class") 
  }
  case class TData() extends Token { 
    val unexpected = kwd("data") 
  }
  case class TDefault() extends Token { 
    val unexpected = kwd("default") 
  }
  case class TDeriving() extends Token { 
    val unexpected = kwd("deriving") 
  }
  case class TDo() extends Token { 
    val unexpected = kwd("do") 
  }
  case class TElse() extends Token { 
    val unexpected = kwd("else") 
  }
  case class TForall() extends Token { 
    val unexpected = kwd("forall") 
  }
  case class THiding() extends Token { 
    val unexpected = identifier("hiding") 
  }
  case class TIf() extends Token { 
    val unexpected = kwd("if") 
  }
  case class TImport() extends Token { 
    val unexpected = kwd("import") 
  }
  case class TIn() extends Token { 
    val unexpected = kwd("in") 
  }
  case class TInfix() extends Token { 
    val unexpected = kwd("infix") 
  }
  case class TInfixl() extends Token { 
    val unexpected = kwd("infixl") 
  }
  case class TInfixr() extends Token { 
    val unexpected = kwd("infixr") 
  }
  case class TInstance() extends Token { 
    val unexpected = kwd("instance") 
  }
  case class TLet() extends Token { 
    val unexpected = kwd("let") 
  }
  case class TMDo() extends Token { 
    val unexpected = kwd("mdo") 
  }
  case class TModule() extends Token { 
    val unexpected = kwd("module") 
  }
  case class TNewtype() extends Token { 
    val unexpected = kwd("newtype") 
  }
  case class TOf() extends Token { 
    val unexpected = kwd("of") 
  }
  case class TPrim() extends Token { 
    val unexpected = kwd("prim") 
  }
  case class TQualified() extends Token { 
    val unexpected = identifier("qualified") 
  }
  case class TThen() extends Token { 
    val unexpected = kwd("then") 
  }
  case class TType() extends Token { 
    val unexpected = kwd("type") 
  }
  case class TWhere() extends Token { 
    val unexpected = kwd("where")
  }
  case class TUnder() extends Token { 
    val unexpected = "`_'" 
  }
  
  // Special
  case class TLParen() extends Token {
    val unexpected = "`('"
  }
  case class TRParen() extends Token {
    val unexpected = "`)'"
  }
  case class TLBrack() extends Token {
    val unexpected = "`['"
  }
  case class TRBrack() extends Token {
    val unexpected = "`]'"
  }
  case class TLCurly() extends Token {
    val unexpected = badLayout("`{'")
  }
  case class TRCurly() extends Token {
    val unexpected = badLayout("`}'")
  }
  case class TComma() extends Token {
    val unexpected = "`,'"
  }
  case class TSemi() extends Token {
    val unexpected = badLayout("`;'")
  }
  case class TBack() extends Token {
    val unexpected = "backquote"
  }

  // Reserved
  case class TDotDot() extends Token {
    val unexpected = "`..'"
  }
  case class TCoCo() extends Token {
    val unexpected = "`::'"
  }
  case class TEq() extends Token {
    val unexpected = "`='"
  }
  case class TLam() extends Token {
    val unexpected = "`\\'"
  }
  case class TBar() extends Token {
    val unexpected = "`|'"
  }
  case class TLArr() extends Token {
    val unexpected = "`<-'"
  }
  case class TRArr() extends Token {
    val unexpected = "`->'"
  }
  case class TImplies() extends Token {
    val unexpected = "`=>'"
  }
  case class TAt() extends Token {
    val unexpected = "`@'"
  }
  case class TTilde() extends Token {
    val unexpected = "`~'"
  }
  case class TMinus() extends Token {
    val unexpected = "`-'"
  }
  
  // Implicit layout
  case class VLCurly() extends Token {
    val unexpected = badLayout("opening brace")
  }
  case class VRCurly() extends Token {
    val unexpected = badLayout("closing brace")
  }
  
  // End-Of-Input token
  case class EOI() extends Token {
    val unexpected = "end of input"
  }
  
  // Error token
  case class ErrorToken(val msg:String) extends Token {
    val unexpected = msg  
  }
  
  def errorToken(p:Position, msg:String) = {
    val t = new ErrorToken(msg)
    t.setPos(p)
    t
  }
}
