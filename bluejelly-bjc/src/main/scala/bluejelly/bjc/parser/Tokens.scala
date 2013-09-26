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
  // Abstract tokens (we know the position of every token)
  sealed abstract class Token extends Positional

  // Identifiers
  case class VarId(val id:Name) extends Token
  case class ConId(val id:Name) extends Token
  case class QVarId(val id:Name) extends Token
  case class QConId(val id:Name) extends Token
  
  // Symbols
  case class VarSym(val sym:Name) extends Token
  case class ConSym(val sym:Name) extends Token
  case class QVarSym(val sym:Name) extends Token
  case class QConSym(val sym:Name) extends Token
  
  // Literals
  case class FloatLit(val x:Double) extends Token
  case class IntLit(val x:BigInt) extends Token
  case class CharLit(val x:Char) extends Token
  case class StringLit(val x:String) extends Token
  
  // Keywords
  case class TAs() extends Token
  case class TCase() extends Token
  case class TClass() extends Token
  case class TData() extends Token
  case class TDefault() extends Token
  case class TDeriving() extends Token
  case class TDo() extends Token
  case class TElse() extends Token
  case class TForall() extends Token
  case class THiding() extends Token
  case class TIf() extends Token
  case class TImport() extends Token
  case class TIn() extends Token
  case class TInfix() extends Token
  case class TInfixl() extends Token
  case class TInfixr() extends Token
  case class TInstance() extends Token
  case class TLet() extends Token
  case class TMDo() extends Token
  case class TModule() extends Token
  case class TOf() extends Token
  case class TPrim() extends Token
  case class TQualified() extends Token
  case class TThen() extends Token
  case class TType() extends Token
  case class TWhere() extends Token
  case class TUnder() extends Token
  
  // Special
  case class TLParen() extends Token
  case class TRParen() extends Token
  case class TLBrack() extends Token
  case class TRBrack() extends Token
  case class TLCurly() extends Token
  case class TRCurly() extends Token
  case class TComma() extends Token
  case class TSemi() extends Token
  case class TBack() extends Token

  // Reserved
  case class TDotDot() extends Token
  case class TCoCo() extends Token
  case class TEq() extends Token
  case class TLam() extends Token
  case class TBar() extends Token
  case class TLArr() extends Token
  case class TRArr() extends Token
  case class TDArr() extends Token
  case class TAt() extends Token
  case class TTilde() extends Token
  case class TMinus() extends Token
  
  // Implicit layout
  case class VLCurly() extends Token
  case class VRCurly() extends Token
  
  // End-Of-Input token
  case class EOI() extends Token
  
  // Error token
  case class ErrorToken(val msg:String) extends Token
  def errorToken(p:Position, msg:String) = {
    val t = new ErrorToken(msg)
    t.setPos(p)
    t
  }
}
