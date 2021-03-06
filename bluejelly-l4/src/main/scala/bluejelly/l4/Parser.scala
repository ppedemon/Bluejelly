/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import bluejelly.utils.Name

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Positional
import scala.util.parsing.input.NoPosition

/**
 * L4 parser.
 * @author ppedemon
 */
object Parser extends JavaTokenParsers {

  // Unquote something of the form "..." or '...'
  def unquote(s:String) = s.substring(1, s.length - 1)

  // Produce a qualified name from a literal string
  def qname(s:String) = {
    val ix = s.lastIndexOf('.')
    Name(s.substring(0,ix),s.substring(ix+1))
  }
  
  // Handle escapes, as defined by the Java language Specification
  def esc = """(\\[btnfr"'\\]|\\[0-3]?[0-7]{1,2})"""
  val escMap = Map(
      'b'  -> 8.toChar, 
      't'  -> '\t', 
      'n'  -> '\n', 
      'f'  -> '\f', 
      'r'  -> '\r', 
      '"'  -> '"' ,
      '\'' -> '\'', 
      '\\' -> '\\')
  @tailrec
  private def unesc(s:String,r:StringBuilder=new StringBuilder):String = {
    @tailrec
    def oct(s:String,n:Int=0):(String,Int) = 
      if (s.isEmpty) (s,n) else
      s.head match {
        case x if x.isDigit => oct(s.tail, (n<<3)|(x.toInt & 0xf))
        case _ => (s,n)
      }
    def esc(s:String) = (s.tail, escMap(s.head))
    if (s.isEmpty) r.toString else
    s.head match {
      case '\\' => s.tail.head match {
        case c if c.isDigit => 
          val (rest,n) = oct(s.tail)
          unesc(rest, r+n.toChar)
        case _ => 
          val (rest,c) = esc(s.tail)
          unesc(rest, r+c)
      }
      case c => unesc(s.tail, r+c)
    }
  }

  // Override whitespace definition to include comments of the form:
  //  # Some text...
  override val whiteSpace = """([ \t\x0B\f\r\n]|#[^\n]*)*""".r
  
  // Possibly qualified identifiers and constructors
  def idRe = """[a-z_\$][\w\$]*"""
  def conRe = """[A-Z][\w\$]*"""
  def qualifier = "(" + idRe + "\\.)*" + conRe
  
  def keywords = 
    Set("and", "data", "extern", "fun", "in", "let", "match", "module", "rec", "with")
  
  def id   = idRe.r ^? { case s if !(keywords contains s) => s }
  def con  = conRe.r
  def qid  = (qualifier + "\\." + idRe).r
  def qcon = (qualifier + "\\." + conRe).r
    
  // Floating point numbers
  override def floatingPointNumber = """-?(\d+(\.\d*)|\d*\.\d+)([eE][+-]?\d+)?""".r
  
  // String literals
  override def stringLiteral = 
    ("\"" + """([^"\p{Cntrl}\\]|""" + esc + ")*" + "\"").r ^^ 
      { s => unesc(unquote(s)) }
  
  // Character literals
  def charLiteral:Parser[Char] = 
    ("""'([^'\p{Cntrl}\\]|""" + esc + ")'").r ^^ 
      { s => unesc(unquote(s)) charAt 0 }

  // Hexadecimal and Octal constants
  def hexNum = "0(x|X)".r ~> """[a-fA-F0-9]+""".r ^^ {Integer.parseInt(_,16)}
  def octNum = "0(o|O)".r ~> """[0-7]+""".r ^^ {Integer.parseInt(_,8)}
  
  // Parse integer and floating point numbers
  def pint:Parser[Int] = hexNum | octNum | (wholeNumber ^^ {_.toInt})  
  def pdbl:Parser[Double] = floatingPointNumber ^^ {_.toDouble}

  // Possibly qualified Vars and ConRefs
  def localVar = id ^^ {s => new Var(Name(s))}
  def variable = qid ^^ {s => new Var(qname(s))} | localVar
  def conRef =
    qcon ^^ {s => new ConRef(qname(s))} |
    con  ^^ {s => new ConRef(Name(s))}

  // Shorthand for positioned combinator
  def $[T <: Positional](p:Parser[T]) = positioned(p)

  // Literals
  def lit:Parser[Lit] = 
    pdbl          ^^ {DblLit(_)} |
    pint          ^^ {IntLit(_)} |
    charLiteral   ^^ {ChrLit(_)} |
    stringLiteral ^^ {StrLit(_)}
  
  // Patterns
  def pat:Parser[Pat] = 
    $( lit ^^ {PLit(_)}
     | id ^^ {s => PVar(new Var(Name(s)))}
     | conRef ~! (id*) ^^ {case c ~ ps => 
         PCon(c, ps map {s => new Var(Name(s))})
       }
     )
      
  def alt:Parser[Alt] = (pat <~ "->") ~ expr ^^ {case p ~ e => new Alt(p,e)} 
  
  def alts:Parser[List[Alt]] = ("|"?) ~> rep1sep(alt, "|")
  
  def decl:Parser[Var~Expr] = 
    ((id ^^ {s => new Var(Name(s))}) <~ "=") ~ expr
  
  def decls:Parser[List[Var~Expr]] = rep1sep(decl, "and")  
  
  def let:Parser[Expr] = 
    ("let" ~> decl <~ "in") ~ expr ^^ {case v ~ e ~ b => Let(v,e,b)}

  def eval:Parser[Expr] = 
    ("let!" ~> decl <~ "in") ~ expr ^^ {case v ~ e ~ b => Eval(v,e,b)}

  def letRec:Parser[Expr] = 
    (("let" ~! "rec") ~> decls <~ "in") ~ expr ^^ 
      {case ds ~ b => LetRec(ds map {case v~e => (v,e)}, b)}
  
  def matchAlts:Parser[Expr] = 
    ("match" ~> variable <~ "with") ~ alts ^^ {case v ~ alts => Match(v,alts)}
  
  def fexp:Parser[Expr] =
    ( variable ~ (aexp+) ^^ {case v ~ args => App(v,args)}
    | conRef ~ (aexp+) ^^ {case v ~ args => ECon(v,args)}
    | ("@" ~> variable) ~! (aexp+) ^^ {case v ~ args => NApp(v,args)}
    | aexp
    )

  def aexp:Parser[Expr] =
    $( lit ^^ {ELit(_)}
     | variable ^^ {App(_,List())}
     | conRef ^^ {ECon(_,List())}
     |"(" ~> expr <~ ")"
     )
  
  def expr:Parser[Expr] =
    $( let 
     | eval 
     | letRec
     | matchAlts
     | fexp
     )

  def dataDecl:Parser[DataDecl] =
    $(("data" ~> conRef) ~! ("{" ~> pint <~ ",")  ~! (pint <~ "}") ^^ 
      {case cref ~ tag ~ arity => DataDecl(cref, new ConDef(tag, arity))})
  
  def extDecl:Parser[List[ExtDecl]] = 
    "extern" ~> rep1sep($(qid ~ ("{" ~> pint <~ "}") ^^ 
        {case v~a => ExtDecl(new Var(qname(v)),a)}), ",")
      
  def funDecl:Parser[FunDecl] =
    $(("fun" ~> localVar) ~ (localVar*) ~ ("=" ~> expr) ^^ 
      {case n ~ args ~ b => FunDecl(n,args,b)})
  
  def topDecl = dataDecl ^^ {List(_)} | funDecl ^^ {List(_)} | extDecl
  
  def module:Parser[Module] =
    ("module" ~> qualifier.r) ~ (topDecl*) ^^ 
      {case name ~ decls => new Module(Name(name), decls flatten)}

}