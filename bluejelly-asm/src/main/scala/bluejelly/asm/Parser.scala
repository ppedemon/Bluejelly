/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm

import scala.util.control.Exception.catching
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Positional
import scala.annotation.tailrec

/**
  * Parse an assembler module.
  * @author ppedemon
  */
object Parser extends JavaTokenParsers {
  
  // Precondition: s is of the form "..."
  def unquote(s:String) = s.substring(1, s.length - 1)

  // Deal with escape sequences, except \\uHHHH which must be handled
  // separately in a preprocessing stage (see UnicodeFilter)
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

  def id = """[a-zA-Z_\$][\w\$]*"""
  def simpleIdent = id.r
  override val whiteSpace = """([ \t\x0B\f\r\n]|#[^\n]*)*""".r
  override def ident = (id + "(\\." + id + ")*").r
  override def floatingPointNumber = """-?(\d+(\.\d*)|\d*\.\d+)([eE][+-]?\d+)?""".r
  
  override def stringLiteral = 
    ("\"" + """([^"\p{Cntrl}\\]|""" + esc + ")*" + "\"").r ^^ 
      { s => unesc(unquote(s)) }
      
  def charLiteral:Parser[Char] = 
    ("""'([^'\p{Cntrl}\\]|""" + esc + ")'").r ^^ 
      { s => unesc(unquote(s)) charAt 0 }

  def hexNum = "0(x|X)".r ~> """[a-fA-F0-9]+""".r ^^ {Integer.parseInt(_,16)}
  def octNum = "0(o|O)".r ~> """[0-7]+""".r ^^ {Integer.parseInt(_,8)}
  
  def pint:Parser[Int] = hexNum | octNum | (wholeNumber ^^ {_.toInt})  
  def pdbl:Parser[Double] = 
    floatingPointNumber ^^ {_.toDouble} | wholeNumber ^^ {_.toDouble}
  
  def intint:((Int,Int) => Instr) => Parser[Instr] = f => 
    pint ~ ("," ~> pint) ^^ {case x ~ y => f(x,y)}
  def intid:((Int,String) => Instr) => Parser[Instr] = f =>
    pint ~ ("," ~> ident) ^^ {case x ~ y => f(x,y)} 
  def idint:((String,Int) => Instr) => Parser[Instr] = f =>
    ident ~ ("," ~> pint) ^^ {case x ~ y => f(x,y)} 

  // Shorthand for positioned combinator
  def $[T <: Positional](p:Parser[T]) = positioned(p)
  
  def table = Map(
    "enter"   -> success(Enter), 
    "ret"     -> success(Return),
    "raise"   -> success(Raise),
    "catch"   -> success(Catch),
    "jmp"     -> (ident ^^ {new Jump(_)}),
    "evalvar" -> intid (new EvalVar(_,_)),
    "retcon"  -> intint (new RetCon(_,_)),
    "retint"  -> (pint ^^ {new RetInt(_)}),
    "retstr"  -> (stringLiteral ^^ {new RetStr(_)}),
    "retdbl"  -> (pdbl ^^ {new RetDbl(_)}),
    "retchr"  -> (charLiteral ^^ {new RetChr(_)}),

    "stack"     -> (pint ^^ {new StackCheck(_)}),
    "dumpstack" -> (stringLiteral ^^ {new DumpStack(_)}),
    "pushvar"   -> (pint ^^ {new PushVar(_)}),
    "pushint"   -> (pint ^^ {new PushInt(_)}),
    "pushdbl"   -> (pdbl ^^ {new PushDbl(_)}),
    "pushstr"   -> (stringLiteral ^^ {new PushStr(_)}),
    "pushchr"   -> (charLiteral ^^ {new PushChr(_)}),
    "pushcode"  -> (ident ^^ {new PushCode(_)}),
    "pushcont"  -> (ident ^^ {new PushCont(_)}),
    "slide"     -> intint (new Slide(_,_)),

    "mkapp"     -> (pint ^^ {new MkApp(_)}),
    "mknapp"    -> (pint ^^ {new MkNapp(_)}),
    "newapp"    -> success(AllocApp),
    "newnapp"   -> success(AllocNapp),
    "packapp"   -> intint (new PackApp(_,_)),
    "packnapp"  -> intint (new PackNapp(_,_)),
   
    "mkcon"   -> intint (new MkTyCon(_,_)),
    "newcon"  -> (pint ^^ {new AllocTyCon(_)}),
    "packcon" -> intint (new PackTyCon(_,_)), 
        
    "matchcon" -> genMatch[Int](pint, new MatchCon(_,_)),
    "matchint" -> genMatch[Int](pint, new MatchInt(_,_)),
    "matchstr" -> genMatch[String](stringLiteral, new MatchStr(_,_)),
    "matchdbl" -> genMatch[Double](pdbl, new MatchDbl(_,_)),
    "matchchr" -> genMatch[Char](charLiteral, new MatchChr(_,_))
  )
  
  // Parse a single simple instruction (this excludes match instructions)
  def instr:Parser[Instr] = {
    def handler(i:String):PartialFunction[Throwable,Parser[Instr]] = {
      case _ => failure("Invalid instruction: " + i)
    }
    $(ident >> {instr => catching (handler(instr)) (table(instr))})
  }

  // Parse an instruction block
  def block:Parser[Block] = (instr+) ^^ {new Block(_)}

  // Parse a case alternative
  def alt[T](p:Parser[T]):Parser[Alt[T]] =
    (".case" ~> p <~ ":") ~ block ^^ {case v ~ b => new Alt(v,b)}
  
  // Parse a sequence of one or more case alternatives
  def manyAlts[T](p:Parser[T]):Parser[List[Alt[T]]] = alt(p)*
  
  // Parse a default alternative
  def deflt:Parser[Block] = ".default" ~ ":" ~> block
  
  // Generic match instruction parser
  def genMatch[T](p:Parser[T],f:(List[Alt[T]],Option[Block]) => Instr):Parser[Instr] = {
    manyAlts(p) ~ (deflt?)  <~ ".end" ^^ {case as ~ d => f(as,d)}
  }
  
  // Parse function arguments: arity and matcher flag
  def funArgs:Parser[(Int,Boolean)] = {
    def arg:Parser[Any] = ("arity" ~ "=" ~> pint) | ("matcher" ^^^ true)
    "[" ~> repsep(arg,",") <~ "]" ^^ { as =>
      var arity = 0
      var matcher = false
      for (a <- as) a match {
        case x:Int => arity = x
        case x:Boolean => matcher = x
      }
      (arity,matcher)
    }
  }
  
  // Parse a function header
  def funHeader:Parser[~[String,(Int,Boolean)]] = 
    simpleIdent ~! (funArgs?) ^^ {case n~a => new ~(n,a.getOrElse((0,false)))} | 
    funArgs ~ simpleIdent ^^ {case a~n => new ~(n,a)}
  
  // Parse a complete function
  def fun:Parser[Function] = 
    $((".fun" ~> funHeader <~ ":") ~ block <~ ".end" ^^
          {case n ~ a ~ b => new Function(n, a._1, a._2, b)})
  
  // Parse a whole module
  def module:Parser[Module] = {
    (".module" ~> ident <~ ":") ~ (fun*) <~ ".end" ^^ { 
      case n ~ funs => new Module(n, funs)
    }
  }
}
