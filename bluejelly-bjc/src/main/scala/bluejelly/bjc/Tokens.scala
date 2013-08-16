/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc

import scala.util.parsing.input.Positional
import bluejelly.utils.Name
import scala.math.BigInt

/**
 * Tokens in the bluejelly top-level language. 
 * @author ppedemon
 */
trait Tokens {
  // Abstract tokens (we know the position of every token)
  abstract class Token extends Positional

  // Identifiers
  case class VarId(val id:Name) extends Token
  case class ConId(val id:Name) extends Token
  
  // Symbols
  case class VarSym(val sym:Name) extends Token
  case class ConSym(val sym:Name) extends Token
  
  // Literals
  case class FloatLit(val x:Double) extends Token
  case class IntLit(val x:BigInt) extends Token
  case class CharLit(val x:Char) extends Token
  case class StringLit(val x:String) extends Token
  
  // Keywords
  case object TCase extends Token
  case object TClass extends Token
  case object TData extends Token
  case object TDefault extends Token
  case object TDeriving extends Token
  case object TDo extends Token
  case object TElse extends Token
  case object TIf extends Token
  case object TImport extends Token
  case object TIn extends Token
  case object TInfix extends Token
  case object TInfixl extends Token
  case object TInfixr extends Token
  case object TInstance extends Token
  case object TLet extends Token
  case object TModule extends Token
  case object TOf extends Token
  case object TPrim extends Token
  case object TThen extends Token
  case object TType extends Token
  case object TWhere extends Token
  case object TUnder extends Token
  
  // Special
  case object TLParen extends Token
  case object TRParen extends Token
  case object TLBrack extends Token
  case object TRBrack extends Token
  case object TLCurly extends Token
  case object TRCurly extends Token
  case object TComma extends Token
  case object TSemi extends Token
  case object TBack extends Token

  // Reserved
  case object TDotDot extends Token
  case object TColon extends Token
  case object TCoCo extends Token
  case object TEq extends Token
  case object TLam extends Token
  case object TBar extends Token
  case object TLArr extends Token
  case object TRArr extends Token
  case object TDArr extends Token
  case object TAt extends Token
  case object TTilde extends Token
  
  // End-Of-Input token
  case object EOI extends Token
  
  // Error token
  case class ErrorToken(val msg:String) extends Token
  def errorToken(msg:String) = new ErrorToken(msg)
}
