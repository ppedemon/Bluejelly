/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.parser

import java.math.BigInteger

import scala.util.parsing.input.{Positional}
import scala.util.parsing.combinator.{Parsers}

import bluejelly.utils.Name

/**
 * Basic lexer for the bluejelly top-level language.
 * @author ppedemon
 */
object Lexer extends Parsers with Tokens {
  type Elem = Char
  
  private def oneOf(cs:Char*) = elem("", cs contains _)
  private def chrExcept(cs: Char*) = elem("", c => (cs forall (c !=)))
  private def any = elem("",_ => true)
  
  private def symTypes = Set(
    Character.CONNECTOR_PUNCTUATION,
    Character.DASH_PUNCTUATION,
    Character.END_PUNCTUATION,
    Character.FINAL_QUOTE_PUNCTUATION,
    Character.INITIAL_QUOTE_PUNCTUATION,
    Character.OTHER_PUNCTUATION,
    Character.START_PUNCTUATION,
    Character.CURRENCY_SYMBOL,
    Character.MATH_SYMBOL,
    Character.MODIFIER_SYMBOL,
    Character.OTHER_SYMBOL
  )  

  private def specialSyms = Set('(', ')', '[', ']', '{', '}', ',', ';', '`')
  
  private def ascSyms = Set(
      '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', 
      '=', '>', '?', '@', '\\', '^', '|', '-', '~', ':')
  
  private def keywords:Map[String, Unit => Token] = Map(
      "case"      -> (_ => new TCase),
      "class"     -> (_ => new TClass),
      "data"      -> (_ => new TData),
      "default"   -> (_ => new TDefault),
      "deriving"  -> (_ => new TDeriving),
      "do"        -> (_ => new TDo),
      "else"      -> (_ => new TElse),
      "if"        -> (_ => new TIf),
      "import"    -> (_ => new TImport),
      "in"        -> (_ => new TIn),
      "infix"     -> (_ => new TInfix),
      "infixl"    -> (_ => new TInfixl),
      "infixr"    -> (_ => new TInfixr),
      "instance"  -> (_ => new TInstance),
      "let"       -> (_ => new TLet),
      "mdo"       -> (_ => new TMDo),
      "module"    -> (_ => new TModule),
      "of"        -> (_ => new TOf),
      "primitive" -> (_ => new TPrim),
      "then"      -> (_ => new TThen),
      "type"      -> (_ => new TType),
      "where"     -> (_ => new TWhere),
      "_"         -> (_ => new TUnder)
  )
  
  private def reservedOps:Map[String, Unit => Token] = Map(
      ".." -> (_ => new TDotDot),
      "::" -> (_ => new TCoCo),
      "="  -> (_ => new TEq),
      "\\" -> (_ => new TLam),
      "|"  -> (_ => new TBar),
      "<-" -> (_ => new TLArr),
      "->" -> (_ => new TRArr),
      "=>" -> (_ => new TDArr),
      "@"  -> (_ => new TAt),
      "~"  -> (_ => new TTilde)
  )
  
  def isSymbol(c:Char) = 
    !(specialSyms contains c) && 
      (("_'\"" indexOf c) == -1) &&
        ((ascSyms contains c) || (symTypes contains c.getType.toByte))
           
  // Character types
  private def small = elem("small character", c => c.isLower || c == '_')
  private def large = elem("large character", c => c.isUpper || c.isTitleCase)
  private def digit = elem("digit", _.isDigit)
  private def white = elem("", _.isWhitespace)
  private def sym   = elem("symbol", isSymbol(_))
  private def eoi   = '\032'
     
  // Nicer error messages
  private def printChar(c:Char) = c match {
    case '\n'   => "\\n"
    case '\r'   => "\\r"
    case '\t'   => "\\t"
    case '\f'   => "\\f"
    case '\b'   => "\\b"
    case '\07'  => "\\a"
    case '\013' => "\\v"
    case '\''   => "'"
    case '"'    => "\""
    case '\\'   => "\\"
    case _ if c.isControl || c.isSpaceChar => 
      "\\%s" format (Integer.toString(c,8))
    case _ => "%c" format c
  }  

  override implicit def accept(e: Elem): Parser[Elem] = 
    acceptIf(_ == e)(c => 
      if (c == eoi)
        "unexpected end of input (`%s' expected)" format e
      else
        "`%s' expected but `%s' found" format (e, printChar(c)))
  
  // Auxiliary stuff
  private def varid = 
    small ~ rep(small|large|digit|'\'') ^^ {case c~cs => c +: cs mkString}
  private def conid = 
    large ~ rep(small|large|digit|'\'') ^^ {case c~cs => c +: cs mkString}
  private def varsym = 
    sym ~ rep(sym) ^^ {case c~cs => c +: cs mkString}
  private def consym = 
    ':' ~> rep(sym) ^^ {case cs => ':' +: cs mkString}
  
  private def modid = 
    rep(varid <~ '.') ~ conid ^^ {
      case Nil~c => c
      case vs~c => vs.mkString("",".",".") + c 
    }

  // ---------------------------------------------------------------------
  // Identifiers
  // ---------------------------------------------------------------------
  def ident =
    ((modid <~ '.') ~ varid ^? 
      {case q~id if !(keywords contains id) => QVarId(Name(q,id))}
    |(modid <~ '.') ~ conid ^^ 
      {case q~id => QConId(Name(q,id))}
    |(modid <~ '.') ~ consym ^? 
      {case q~sym if !(reservedOps contains sym) => QConSym(Name(q,sym))} 
    |(modid <~ '.') ~ varsym ^? 
      {case q~sym if !((sym matches "-{2,}") || (reservedOps contains sym)) => 
        QVarSym(Name(q,sym))}
    |varid  ^^ 
      {case "as" => TAs()
       case "forall" => TForall() 
       case "hiding" => THiding()
       case "qualified" => TQualified()
       case id => keywords.getOrElse(id, (_:Unit) => new VarId(Name(id)))()}
    |conid  ^^ 
      {id => ConId(Name(id))}
    |consym ^^ 
      {case sym => reservedOps.getOrElse(sym, (_:Unit) => ConSym(Name(sym)))()}
    |varsym ^^
      {case "-" => TMinus()
       case sym => reservedOps.getOrElse(sym, (_:Unit) => VarSym(Name(sym)))()})

  // ---------------------------------------------------------------------
  // Floating point literals
  // ---------------------------------------------------------------------
  private def fpStr(int:String, dec:String, exp:Option[String]) = 
    int + (if (dec.isEmpty) "" else "." + dec) + exp.getOrElse("")
  
  private def exp:Parser[String] = 
    (oneOf('e','E') ~> opt(oneOf('+','-'))) ~ rep1(digit) ^^ {
      case Some('-')~ds => "e-" + ds.mkString
      case _~ds => "e" + ds.mkString
    }

  private def floatingNum = 
    (rep1(digit) <~ '.') ~ rep1(digit) ~ opt(exp) ^^ 
      {case i~d~e => val s = fpStr(i.mkString, d.mkString, e); (s,s.toDouble)} |
    rep1(digit) ~ exp ^^
      {case i~e => val s = fpStr(i.mkString, "", Some(e)); (s,s.toDouble)}
    
  def floating = 
    floatingNum >> {
      case (_,n) if !n.isInfinity && !n.isNaN => success(FloatLit(n))
      case (s,_) => err("invalid floating point literal: `%s'" format s)
    } 
      
  // ---------------------------------------------------------------------
  // Integer literals (decimal, hexadecimal or octal)
  // ---------------------------------------------------------------------
  private def hexit = elem("hexadecimal digit", 
      c => (c >= 'a' && c <= 'f') || 
           (c >= 'A' && c <= 'F') || 
           (c >= '0' && c <= '9'))    
  
  private def octit = elem("octal digit", c => c >= '0' && c <= '7')           
   
  private def toBigInt(ds:String, radix:Int) = 
    new BigInt(new BigInteger(ds,radix))
  
  def integer =
    ('0' ~ oneOf('x','X')) ~> rep1(hexit) ^^ 
      {c => new IntLit(toBigInt(c.mkString, 16))} |
    ('0' ~ oneOf('o','O')) ~> rep1(octit) ^^ 
      {c => new IntLit(toBigInt(c.mkString,  8))} | 
    rep1(digit) ^^ 
      {c => new IntLit(toBigInt(c.mkString, 10))}
    
  // ---------------------------------------------------------------------
  // String and char literals
  // ---------------------------------------------------------------------
  private def octToChar(cs:Char*) = Integer.parseInt(cs.mkString,8).toChar
        
  private def esc = 
    octit ~ octit ~ octit ^^ {case x~y~z => octToChar(x,y,z)} |
    octit ~ octit ^^ {case x~y => octToChar(x,y)} |
    octit ^^ {octToChar(_)} |
    oneOf('a','b','f','n','r','t','v','\\','\'','"') ^^ {
      case 'a'  => '\07'
      case 'b'  => '\b'
      case 'f'  => '\f'
      case 'n'  => '\n'
      case 'r'  => '\r'
      case 't'  => '\t'
      case 'v'  => '\013'
      case '\\' => '\\'
      case '\'' => '\''
      case '"'  => '"'
    } |
    eoi ~> err("unclosed character literal") | 
    any >> {c => err("invalid escape character: `%c'" format c)}
    
  private def validChr(cs:Char*) = 
    elem("", c => 
      !(cs contains c) && !c.isControl && (c == ' ' || !c.isWhitespace))
 
  private def gap = '\\' ~ rep1(white) ~ '\\' ^^^ ""
    
  def char = 
    '\'' ~> (validChr('\'','\\',eoi)|('\\' ~> esc)) <~ '\'' ^^ {CharLit(_)}
  
  def string = 
    '"' ~> rep(validChr('"','\\',eoi)|gap|('\\' ~> esc)) <~ '"' ^^ 
      {cs => StringLit(cs.mkString)}
    
  // ---------------------------------------------------------------------
  // Special symbols
  // ---------------------------------------------------------------------  
  def special:Parser[Token] = 
    '(' ^^^ new TLParen |
    ')' ^^^ new TRParen |
    '[' ^^^ new TLBrack |
    ']' ^^^ new TRBrack |
    '{' ^^^ new TLCurly |
    '}' ^^^ new TRCurly |
    ',' ^^^ new TComma  |
    ';' ^^^ new TSemi   |
    '`' ^^^ new TBack

    
  // ---------------------------------------------------------------------
  // Everything together
  // ---------------------------------------------------------------------
  def token:Parser[Token] = positioned(
    ident           |
    special         |
    floating        |
    integer         |
    char            |
    string          |
    eoi ^^^ new EOI |
    any >> {c => err("invalid character: `%c'" format c)})
}
