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
  import bluejelly.bjc.common.Name._
  import bluejelly.bjc.ast.NameConstants._
  
  type Elem = Token
    
  // ---------------------------------------------------------------------
  // Some code to produce decent error messages
  // ---------------------------------------------------------------------
  
  private def lexError = elem("", _.isInstanceOf[ErrorToken]) >> {
    case ErrorToken(s) => Parser{ in => new Error(s,in) {
      override def toString = "[%s] lexical error: %s" format (next.pos,msg)  
    }}
  }

  override def elem(kind: String, p: Elem => Boolean) = acceptIf(p){
    case ErrorToken(msg) => msg
    case t => "unexpected %s (%s expected)" format (unexpected(t),kind)
  }
  
  def elem(p: Elem => Boolean) = acceptIf(p){
    case ErrorToken(msg) => msg
    case t => "unexpected %s" format unexpected(t)
  }
  
  private def identifier(s:String) = "identifier `%s'" format s
  private def operator(s:String) = "operator `%s'" format s
  private def kwd(s:String) = "keyword `%s'" format s 
  private def badLayout(s:String) = "%s, possibly due to bad layout" format s
  
  def unexpected(t:Token) = t match {
    case VarId(n)     => identifier(n.toString)
    case ConId(n)     => identifier(n.toString)
    case QVarId(n)    => identifier(n.toString)
    case QConId(n)    => identifier(n.toString)
    case TAs()        => identifier("as")
    case TForall()    => identifier("forall")
    case THiding()    => identifier("hiding")
    case TQualified() => identifier("qualified")  

    case VarSym(n)  => operator(n.toString)
    case ConSym(n)  => operator(n.toString)
    case QVarSym(n) => operator(n.toString)
    case QConSym(n) => operator(n.toString)
    
    case FloatLit(x)  => "floating point literal"
    case IntLit(x)    => "integer literal"
    case CharLit(x)   => "character literal"
    case StringLit(x) => "string literal"
    
    case TCase()     => kwd("case")
    case TClass()    => kwd("class")
    case TData()     => kwd("data")
    case TDefault()  => kwd("default")
    case TDeriving() => kwd("deriving")
    case TDo()       => kwd("do")
    case TElse()     => kwd("else")
    case TIf()       => kwd("if")
    case TImport()   => kwd("import")
    case TIn()       => kwd("in")
    case TInfix()    => kwd("infix")
    case TInfixl()   => kwd("infixl")
    case TInfixr()   => kwd("infixr")
    case TInstance() => kwd("instance")
    case TLet()      => kwd("let")
    case TMDo()      => kwd("mdo")
    case TModule()   => kwd("module")
    case TOf()       => kwd("of")
    case TPrim()     => kwd("primitive")
    case TThen()     => kwd("then")
    case TType()     => kwd("type")
    case TWhere()    => kwd("where")
    
    case TUnder()     => "`_'"      
    case TLParen() => "`('"
    case TRParen() => "`('"
    case TLBrack() => "`['"
    case TRBrack() => "`]'"
    case TComma()  => "`,'"
    case TBack()   => "backquote"

    case  TDotDot() => "`..'"
    case  TCoCo()   => "`::'"
    case  TEq()     => "`='"
    case  TLam()    => """`\'"""
    case  TBar()    => "`|'"
    case  TLArr()   => "`<-'"
    case  TRArr()   => "`->'"
    case  TDArr()   => "`=>'"
    case  TAt()     => "`@'"
    case  TTilde()  => "`~'"
    case  TMinus()  => "`-'"

    case TLCurly() => badLayout("`{'")
    case TRCurly() => badLayout("`}'")
    case TSemi()   => badLayout("`;'")
    case VLCurly() => badLayout("opening brace")
    case VRCurly() => badLayout("closing brace")
    case EOI()     => "end of input"
    case ErrorToken(msg) => msg
  }

  // ---------------------------------------------------------------------
  // Token parsers
  // ---------------------------------------------------------------------

  // Names
  private def VARID = elem("identifier",_.isInstanceOf[VarId]) ^^ 
    { case VarId(id) => unqualId(id.name) }
  private def VAROP = elem("operator",_.isInstanceOf[VarSym]) ^^ 
    { case VarSym(op) => unqualOp(op.name) }
  private def QVARID = elem("identifier",_.isInstanceOf[QVarId]) ^^ 
    { case QVarId(id) => qualId(id.qual.get,id.name) }
  private def QVAROP = elem("operator",_.isInstanceOf[QVarSym]) ^^ 
    { case QVarSym(op) => qualOp(op.qual.get,op.name) }
  private def CONID = elem("constructor",_.isInstanceOf[ConId]) ^^
    { case ConId(id) => unqualId(id.name)}
  private def CONOP = elem("constructor",_.isInstanceOf[ConSym]) ^^
    { case ConSym(op) => unqualOp(op.name)}
  private def QCONID = elem("constructor",_.isInstanceOf[QConId]) ^^
    { case QConId(id) => qualId(id.qual.get,id.name)}
  private def QCONOP = elem("constructor",_.isInstanceOf[QConSym]) ^^
    { case QConSym(op) => qualOp(op.qual.get,op.name)}
  
  // Keywords
  private def module = elem(kwd("module"), _.isInstanceOf[TModule])
  private def where  = elem(kwd("where"), _.isInstanceOf[TWhere])
  private def let    = elem(kwd("let"), _.isInstanceOf[TLet])
  private def in     = elem(kwd("in"), _.isInstanceOf[TIn])
    
  // Reserved
  private def minus = elem(_.isInstanceOf[TMinus])
  private def back  = elem(_.isInstanceOf[TBack])
  
  // Special
  private def lpar    = elem(_.isInstanceOf[TLParen])
  private def rpar    = elem(_.isInstanceOf[TRParen])
  private def lcurly  = elem(_.isInstanceOf[TLCurly])
  private def vlcurly = elem(_.isInstanceOf[VLCurly])
  private def rcurly  = elem(_.isInstanceOf[TRCurly])
  private def vrcurly = elem(_.isInstanceOf[VRCurly])
  private def semi    = elem(_.isInstanceOf[TSemi])
    
  // EOI token
  private def eoi = elem("end of input", _.isInstanceOf[EOI])

  // ---------------------------------------------------------------------
  // Variables + Constructors (as identifiers or operators)
  // ---------------------------------------------------------------------
  
  private def varid = 
    ( elem("identifier", _.isInstanceOf[TAs]) ^^^ nmAs
    | elem("identifier", _.isInstanceOf[TForall]) ^^^ nmForall
    | elem("identifier", _.isInstanceOf[THiding]) ^^^ nmHiding
    | elem("identifier", _.isInstanceOf[TQualified]) ^^^ nmQualified
    | VARID)
   
  private def vars = 
    varid | lpar ~> VAROP <~ rpar | lpar ~> minus <~ rpar ^^^ nmMinus
  
  private def qvar = 
    QVARID | lpar ~> QVAROP <~ rpar | vars

  private def varopNoMinus = 
    VAROP | back ~> varid <~ back
  
  private def varop = 
    varopNoMinus | minus ^^^ nmMinus
    
  private def qvaropNoMinus = 
    QVAROP | back ~> QVARID <~ back | varopNoMinus
    
  private def qvarop = 
    qvaropNoMinus | minus ^^^ nmMinus
    
  private def con = 
    CONID  | lpar ~> CONOP  <~ rpar
  
  private def qcon = 
    QCONID | lpar ~> QCONOP <~ rpar | con
  
  private def conop = 
    CONOP  | back ~> CONID  <~ back
  
  private def qconop = 
    QCONOP | back ~> QCONID <~ back | conop
  
  private def qconid = 
    QCONID | CONID  

  private def op = varop | conop
  private def qop = qvarop | qconop
  private def modid = qconid
  
  // A program
  def program = 
    (module ~> modid <~ where) ~ (vlcurly ~> body <~ vrcurly) |
    lexError

  private def body = 
    //(let ~> block <~ in) ~ varid
    (qvar|qvarop)*

  /*  
  private def block = 
    lcurly  ~> cmds <~ rcurly |
    vlcurly ~> cmds <~ close
    
  private def cmds = rep1sep(varid, semi)
  */
  
  private def close = 
    vrcurly |
    Parser { 
      case in:LayoutScanner if in.inImplicitLayout => 
        Success(new VLCurly, in.popCurrentLayout)
      case in => 
        Error("possibly bad layout", in)
    }
  
  // Parser entry points
  def phrase[T](p:Parser[T], in:String) = 
    super.phrase(p <~ eoi)(new LayoutScanner(in))
  def phrase[T](p:Parser[T], in:java.io.Reader) = 
    super.phrase(p <~ eoi)(new LayoutScanner(in))
}
