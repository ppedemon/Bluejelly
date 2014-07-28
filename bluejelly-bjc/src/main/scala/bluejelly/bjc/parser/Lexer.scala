/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.parser

import java.math.BigInteger

import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder
import scala.language.implicitConversions
import scala.util.parsing.input.{Positional}
import scala.util.parsing.combinator.{RegexParsers}

import bluejelly.utils.Name

/**
 * Basic lexer for the bluejelly top-level language.
 * @author ppedemon
 */
object Lexer extends RegexParsers with Tokens {
  
  override val whiteSpace = "".r
  private def any = elem("",_ => true)

  private def printChar(c:Char) = c match {
    case '\n'   => "\\n"
    case '\r'   => "\\r"
    case '\t'   => "\\t"
    case '\f'   => "\\f"
    case '\b'   => "\\b"
    case '\07'  => "\\a"
    case '\013' => "\\v"
    case _ if c.isControl || c.isSpaceChar => 
      "\\u%s" format (Integer.toHexString(c))
    case _ => "%c" format c
  }  

  
  // ---------------------------------------------------------------------
  // Basic definitions
  // ---------------------------------------------------------------------
  private val keywords:Map[String, Unit => Token] = Map(
      "case"      -> (_ => TCase()),
      "class"     -> (_ => TClass()),
      "data"      -> (_ => TData()),
      "default"   -> (_ => TDefault()),
      "deriving"  -> (_ => TDeriving()),
      "do"        -> (_ => TDo()),
      "else"      -> (_ => TElse()),
      "forall"    -> (_ => TForall()),
      "if"        -> (_ => TIf()),
      "import"    -> (_ => TImport()),
      "in"        -> (_ => TIn()),
      "infix"     -> (_ => TInfix()),
      "infixl"    -> (_ => TInfixl()),
      "infixr"    -> (_ => TInfixr()),
      "instance"  -> (_ => TInstance()),
      "let"       -> (_ => TLet()),
      "mdo"       -> (_ => TMDo()),
      "module"    -> (_ => TModule()),
      "newtype"   -> (_ => TNewtype()),
      "of"        -> (_ => TOf()),
      "primitive" -> (_ => TPrim()),
      "then"      -> (_ => TThen()),
      "type"      -> (_ => TType()),
      "where"     -> (_ => TWhere()),
      "_"         -> (_ => TUnder())
  )
  
  private val reservedOps:Map[String, Unit => Token] = Map(
      ".." -> (_ => new TDotDot),
      "::" -> (_ => new TCoCo),
      "="  -> (_ => new TEq),
      "\\" -> (_ => new TLam),
      "|"  -> (_ => new TBar),
      "<-" -> (_ => new TLArr),
      "->" -> (_ => new TRArr),
      "=>" -> (_ => new TImplies),
      "@"  -> (_ => new TAt),
      "~"  -> (_ => new TTilde)
  )

  private val small = """[\p{Ll}_]"""
  private val large = """[\p{Lu}\p{Lt}]"""
  private val digit = """\p{Nd}"""
  private val white = """[\p{Zs}\n\r\t\f]"""
  private val sym   = """[\p{S}!#%&/<=>@~:\$\*\+\.\?\^\|\-\\]"""
  
  def isSymbol(c:Char) = 
    (("()[]{},;`" indexOf c) == -1) && 
      (("_'\"" indexOf c) == -1) &&
        (c.toString.matches(sym))

  override implicit def accept(e: Elem): Parser[Elem] = 
    acceptIf(_ == e)(c => 
      if (c == PositionedReader.EofCh)
        "unexpected end of input (`%s' expected)" format e
      else
        "`%s' expected but `%s' found" format (printChar(e), printChar(c)))
    
  private val varid = 
    "%s(%s|%s|%s|')*" format (small,small,large,digit)
  private val conid = 
    "%s(%s|%s|%s|')*" format (large,small,large,digit)
  private val modid = 
    "(%s\\.)*%s" format (varid,conid)
  private val varsym = "%s+" format sym
  private val consym = ":%s" format sym

  // ---------------------------------------------------------------------
  // Identifiers
  // ---------------------------------------------------------------------
  private def ident =
    (("(%s\\.)?(%s|%s|%s|%s)" 
        format (modid,varid,conid,varsym,consym)).r ^? qident
    | varid.r  ^^ {keywords(_)()}
    | varsym.r ^^ {reservedOps(_)()}
    | consym.r ^^ {reservedOps(_)()}
    | conid.r  ^^ {conId(None,_)})

  /*
   * This partial function is the core of the lexer. It tries to
   * lex possibly qualified ids or syms. If it turns out that the
   * last component of the possibly qualified name refers to a 
   * reserved word or operator, we call the function not defined, 
   * so the parser can backtrack.
   * 
   * Performance: haven't profiled, but I'm sure this function
   * accounts for the 80% of the lexer execution time. The lexer
   * goes almost twice as fast now that I'm using regexps instead
   * of combinators, but it's still too slow for my taste. The bad 
   * thing is that I can't think of any obvious hack to improve
   * lexing performance :(
   */
  private def qident = new PartialFunction[String,Token] {
    // When the name obtained from splitting is a reserved word
    // or operator, call the function undefined and allow parser
    // to backtrack. As a special case, we don't allow qualified
    // operators of the form M.-{2,-}. Such operator is a comment,
    // and it could have never been defined in module M.
    def isDefinedAt(s:String) = {
      val (q,n) = split(s)
      !keywords.contains(n) &&
        !reservedOps.contains(n) &&
          (q.isEmpty || !n.matches("-{2,}"))
    }
    // Application: the easy part, depending on the name call 
    // a suitable factory function
    def apply(s:String) = {
      val (q,n) = split(s)
      n match {
        case _ if n.charAt(0).isLower || n.charAt(0) == '_' => varId(q,n)
        case _ if n.charAt(0).isUpper => conId(q,n)
        case _ if n.charAt(0) == ':'  => conSym(q,n)
        case _ => varSym(q,n)
      }
    }
    // Auxiliary: split qualifier and name parts, qualifier 
    // will be None for non-qualified tokens
    private def split(s:String) = {
      val p = ("(%s\\.)?(.+)" format modid).r
      s match {
        case p(q,_,_,_,n) if q != null => (Some(q.dropRight(1)),n)
        case _ => (None,s)
      }
    }
  }  
  
  // Factory stuff, handle a couple of awkward cases here
  @inline
  private def varId(q:Option[String],s:String) = q match {
    case None if s == "as" => TAs()
    case None if s == "hiding" => THiding()
    case None if s == "qualified" => TQualified()
    case None => VarId(Name(s))
    case Some(q) => QVarId(Name(q,s))
  }
  @inline
  private def conId(q:Option[String],s:String) = q match {
    case None => ConId(Name(s))
    case Some(q) => QConId(Name(q,s))
  }
  @inline
  private def varSym(q:Option[String],s:String) = q match {
    case None if s == "-" => TMinus()
    case None => VarSym(Name(s))
    case Some(q) => QVarSym(Name(q,s))
  }
  @inline
  private def conSym(q:Option[String],s:String) = q match {
    case None => ConSym(Name(s))
    case Some(q) => QConSym(Name(q,s))
  }

  // ---------------------------------------------------------------------
  // Floating point literals
  // ---------------------------------------------------------------------  
  private def exp = 
    """[eE][\+-]?(%s)+""" format digit
  private def fp = 
    ( ("""%s+\.%s+(%s)?""" format (digit,digit,exp)).r ^^ (s => (s,s.toDouble))
    | ("""%s+%s""" format (digit,exp)).r ^^ (s => (s,s.toDouble)))
  
  private def floating = 
    fp >> {
      case (_,n) if !n.isInfinity && !n.isNaN => success(FloatLit(n))
      case (s,_) => err("invalid floating point literal: `%s'" format s)
    } 
      
  // ---------------------------------------------------------------------
  // Integer literals (decimal, hexadecimal or octal)
  // ---------------------------------------------------------------------
  private def toBigInt(ds:String, radix:Int) = 
    new BigInt(new BigInteger(ds,radix))
  
  private def integer = 
    ( """0[xX][A-Fa-f0-9]+""".r ^^ (s => IntLit(toBigInt(s.drop(2),16)))
    | """0[oO][0-7]+""".r ^^ (s => IntLit(toBigInt(s.drop(2),8)))
    | ("""(%s)+""" format digit).r ^^ (s => IntLit(toBigInt(s,10))))
  
  // ---------------------------------------------------------------------
  // String and char literals
  // ---------------------------------------------------------------------  
  private def unquote(s:String) = s.drop(1).dropRight(1)
 
  private val escMap = Map(
    'a'  -> '\07', 
    'b'  -> '\b'  , 
    'f'  -> '\f', 
    'n'  -> '\n', 
    'r'  -> '\r',
    't'  -> '\t' , 
    'v'  -> '\013', 
    '"'  -> '"' , 
    '\'' -> '\'', 
    '\\' -> '\\')

  private val esc = """\\[abfnrtv"'\\]|\\[0-7]{1,3}"""
  private val sesc = """\\[abfnrtv"'\\]|\\%s+\\|\\[0-7]{1,3}""" format white
  
  /*
   * Mini parser for a character or string literals, we ignore
   * gaps and turn escape sequences into the real characters.
   */
  @tailrec
  private def unesc(s:String,r:StringBuilder=new StringBuilder):String = {
    @tailrec
    def oct(s:String,n:Int=0):(String,Int) = 
      if (s.isEmpty) (s,n) else
      s.head match {
        case x if x.isDigit => oct(s.tail, (n<<3)|(x.toInt & 0xf))
        case _ => (s,n)
      }
    def gap(s:String) = s.dropWhile(_ != '\\').tail
    def esc(s:String) = (s.tail, escMap(s.head))
    if (s.isEmpty) r.toString else
    s.head match {
      case '\\' => s.tail.head match {
        case c if c.isDigit => 
          val (rest,n) = oct(s.tail)
          unesc(rest, r+n.toChar)
        case c if c.isWhitespace => unesc(gap(s.tail),r)
        case _ => 
          val (rest,c) = esc(s.tail)
          unesc(rest, r+c)
      }
      case c => unesc(s.tail, r+c)
    }
  }
      
  private def char = 
    ( ("""'([^'\p{Cntrl}\\]|%s)'""" format esc).r ^^ 
        (s => CharLit(unesc(unquote(s)).head))
    | "'[^']*".r ~> eoi ~> err("unclosed character literal")
    | "'[^']*'".r ~> err("invalid character literal"))

  private def string = 
    ( (""""([^"\p{Cntrl}\\]|%s)*"""" format sesc).r ^^ 
        (s => StringLit(unesc(unquote(s))))
    | """"[^"]*""".r ~> eoi ~> err("unclosed string literal")
    | """"[^"]*"""".r ~> err("invalid string literal"))

  // ---------------------------------------------------------------------
  // Special symbols
  // ---------------------------------------------------------------------  
  private def special = """[\(\)\[\]\{\},;`]""".r ^^ {
    case "(" => TLParen()
    case ")" => TRParen()
    case "[" => TLBrack()
    case "]" => TRBrack()
    case "{" => TLCurly()
    case "}" => TRCurly()
    case "," => TComma()
    case ";" => TSemi()
    case "`" => TBack()
  }

  // ---------------------------------------------------------------------
  // Everything together
  // ---------------------------------------------------------------------
  private def eoi:Parser[Token] = Parser[Token] { in =>
    if (in.atEnd) Success(EOI(), in)
    else Failure("End of Input expected", in)
  } 

  def token:Parser[Token] = positioned(
    special  |
    ident    |
    floating |
    integer  |
    char     |
    string   |
    eoi      |
    any >> {c => err("invalid character: `%s'" format printChar(c))})
}
