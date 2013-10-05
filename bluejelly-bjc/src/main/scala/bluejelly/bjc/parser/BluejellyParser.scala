/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Positional

import bluejelly.bjc.ast.TySigDecl

/**
 * Bluejelly parser.
 * @author ppedemon
 */
object BluejellyParser extends Parsers {
  
  import Lexer._
  import bluejelly.bjc.common.Name._
  import bluejelly.bjc.ast
  import bluejelly.bjc.ast.types
  import bluejelly.bjc.ast.module._
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

    case  TDotDot()  => "`..'"
    case  TCoCo()    => "`::'"
    case  TEq()      => "`='"
    case  TLam()     => """`\'"""
    case  TBar()     => "`|'"
    case  TLArr()    => "`<-'"
    case  TRArr()    => "`->'"
    case  TImplies() => "`=>'"
    case  TAt()      => "`@'"
    case  TTilde()   => "`~'"
    case  TMinus()   => "`-'"

    case TLCurly() => badLayout("`{'")
    case TRCurly() => badLayout("`}'")
    case TSemi()   => badLayout("`;'")
    case VLCurly() => badLayout("opening brace")
    case VRCurly() => badLayout("closing brace")
    case EOI()     => "end of input"
    case ErrorToken(msg) => msg
  }

  // ---------------------------------------------------------------------
  // Basic parsers
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
  private def as       = elem(kwd("as"), _.isInstanceOf[TAs])
  private def forall   = elem(kwd("forall"), _.isInstanceOf[TForall])
  private def hiding   = elem(kwd("hiding"), _.isInstanceOf[THiding])
  private def `import` = elem(kwd("import"),_.isInstanceOf[TImport])
  private def in       = elem(kwd("in"), _.isInstanceOf[TIn])
  private def let      = elem(kwd("let"), _.isInstanceOf[TLet])
  private def module   = elem(kwd("module"), _.isInstanceOf[TModule])
  private def qual     = elem(kwd("qualified"), _.isInstanceOf[TQualified])
  private def where    = elem(kwd("where"), _.isInstanceOf[TWhere])
    
  private def dot = elem(_ match {
    case t:VarSym => (t.asInstanceOf[VarSym].sym.name == Symbol("."))
    case _ => false
  })
  
  private def bang = elem(_ match {
    case t:VarSym => (t.asInstanceOf[VarSym].sym.name == '!)
    case _ => false
  })
  
  // Reserved
  private def minus   = elem(_.isInstanceOf[TMinus])
  private def back    = elem(_.isInstanceOf[TBack])
  private def dotdot  = elem(_.isInstanceOf[TDotDot])
  private def comma   = elem(_.isInstanceOf[TComma])
  private def coco    = elem(_.isInstanceOf[TCoCo])
  private def implies = elem(_.isInstanceOf[TImplies])
  private def rarr    = elem(_.isInstanceOf[TRArr])
  private def under   = elem(_.isInstanceOf[TUnder])
  
  // Special
  private def lpar    = elem(_.isInstanceOf[TLParen])
  private def rpar    = elem(_.isInstanceOf[TRParen])
  private def lbrack  = elem(_.isInstanceOf[TLBrack])
  private def rbrack  = elem(_.isInstanceOf[TRBrack])
  private def lcurly  = elem(_.isInstanceOf[TLCurly])
  private def vlcurly = elem(_.isInstanceOf[VLCurly])
  private def rcurly  = elem(_.isInstanceOf[TRCurly])
  private def vrcurly = elem(_.isInstanceOf[VRCurly])
  private def semi    = elem(_.isInstanceOf[TSemi])
    
  // EOI token
  private def eoi = elem("end of input", _.isInstanceOf[EOI])

  private def $[T <: Positional](p:Parser[T]) = positioned(p)
  
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
  
  private def modid = ((VARID <~ dot)*) ~ CONID ^^ {
    case qs~m => 
      if (qs.isEmpty) m else qualId(Symbol(qs mkString ("",".","")), m.name)
  }

  private def commas = (comma+) ^^ {_.length}
    
  // ---------------------------------------------------------------------
  // Exports
  // ---------------------------------------------------------------------
  private def expSpec = $(
    ( qconid <~ (lpar ~ dotdot ~ rpar) ^^ {EAll(_)}
    | qconid ~ (lpar ~> enames <~ rpar) ^^ {case e~es => ESome(e,es)}
    | qvar ^^ {EVar(_)}
    | qcon ^^ {EVar(_)}
    | module ~> modid ^^ {EMod(_)}))
  
  private def enames = repsep(qvar|qcon,comma) <~ (comma?)
  
  private def exports = 
    ( lpar ~> (repsep(expSpec,comma) <~ (comma?)) <~ rpar ^^ {ExportSome(_)} 
    | success(ExportAll))

  // ---------------------------------------------------------------------
  // Imports
  // ---------------------------------------------------------------------
  private def inames = repsep(vars|con,comma) <~ (comma?)

  private def impSpec = 
    ( CONID <~ (lpar ~ dotdot ~ rpar) ^^ {IAll(_)}
    | CONID ~ (lpar ~> inames <~ rpar) ^^ {case i~is => ISome(i,is)}
    | CONID ^^ {INone(_)}
    | vars ^^ {IVar(_)})
   
  private def impSpecs = 
    lpar ~> (repsep(impSpec,comma) <~ (comma?)) <~ rpar
     
  private def imports = 
    ((hiding?) ~ impSpecs ^^ {
      case h~is if h.isDefined => HideSome(is)
      case _~is => ImportSome(is)
    }
    | success(ImportAll))
     
  private def impDecl = $(
    `import` ~> ((qual?) ~ modid ~ ((as ~> modid)?) ~ imports) ^^ {
      case q~m~a~i => new ImpDecl(m,q.isDefined,a,i)
    })
     
  private def impDecls = rep1sep(impDecl,semi+) ^^ 
    {_ map (i => (m:Module) => m.addImpDecl(i))}

  // ---------------------------------------------------------------------
  // Types
  // ---------------------------------------------------------------------
  
  private def topType:Parser[types.Type] = 
    $((forall ~> (vars+)) ~ (dot ~> qtype) ^^ {
      case xs~ty => types.PolyType(xs,ty)
    } | qtype)
  
  private def qtype = 
    $(((context <~ implies)?) ~ `type` ^^ {
      case None~ty => ty 
      case Some(preds)~ty => types.QualType(preds,ty)
    })
  
  private def `type` = 
    $(rep(ltype <~ rarr) ~ (btype1|btype2) ^^ {
      case Nil~ty => ty
      case tys~ty => types.Type.mkFun(tys,ty)
    })
  
  private def ltype = bpoly | btype1 | btype2
  private def bpoly = (lpar+) ~> topType <~ (rpar+)
  
  private def context = 
    pred ^^ {List(_)} | 
    lpar ~> repsep(pred,comma) <~ rpar
  
  private def pred = 
    btype2 ^? (types.Type.mkPred, "invalid predicate: %s" format _)  
    
  private def btype1 = 
    $(atype1 ~ (atype*) ^^ {
       case ty~args => types.Type.mkApp(ty, args) 
    })
  
  private def btype2 = 
    $(qconid ~ (atype*) ^^ {
       case n~args => types.Type.mkApp(types.TyCon(n), args)
    })
  
  private def atype = 
    atype1 | $(qconid ^^ {types.TyCon(_)})
  
  private def atype1:Parser[types.Type] = 
    $( varid ^^ {types.TyVar(_)}
     | lpar ~ rarr ~ rpar ^^^ types.ArrowCon
     | lpar ~> commas <~ rpar ^^ {types.TupleCon(_)}
     | lpar ~> repsep(`type`,comma) <~ rpar ^^ {
         case Nil => types.UnitCon
         case List(ty) => ty
         case tys => types.Type.mkApp(types.TupleCon(tys.length), tys)
       }
     | lbrack ~> (`type`?) <~ rbrack ^^ {
         case None => types.ListCon
         case Some(ty) => types.AppType(types.ListCon,ty)
       }
     | under ^^^ types.AnonTyVar())
  
  // ---------------------------------------------------------------------
  // Declarations
  // ---------------------------------------------------------------------
     
  private def topDecl = tysig
  
  private def tysig = (repsep(vars,comma) <~ coco) ~ topType ^^ {
    case vars~ty => new TySigDecl(vars, ty)
  }
  
  // ---------------------------------------------------------------------
  // Blocks, with implicit or explicit layout
  // ---------------------------------------------------------------------

  private def block[T](p:Parser[T]) = 
    vlcurly ~> p <~ close |
    lcurly  ~> p <~ rcurly
  
  private def close = 
    vrcurly |
    Parser { 
      case in:LayoutScanner if in.inImplicitLayout => 
        Success(new VLCurly, in.popCurrentLayout)
      case in => 
        Error("possibly bad layout", in)
    }

  // ---------------------------------------------------------------------
  // Top level module
  // ---------------------------------------------------------------------

  def program = 
    ( module ~> (modid ~ exports ~ (where ~> block(modDecls))) ^^
        { case m~es~body => body.foldRight(new Module(m,es))((f,m) => f(m))}
    | block(modDecls) ^^ 
        { _.foldRight(Module.defaultModule)((f,m) => f(m)) }
    | lexError)
  
  private def modDecls =
    rep(semi) ~> 
      ( (impDecls <~ (semi+)) ~ topDecls ^^ {case is~ds => is++ds}
      | impDecls  <~ (semi*)
      | topDecls) |
    success(Nil)
  
  private def topDecls = rep1sep(topDecl, semi+) <~ (semi*) ^^ 
    (_ map (d => (m:Module) => m.addTopDecl(d)))
  
  // Parser entry points
  def phrase[T](p:Parser[T], in:String) = 
    super.phrase(p <~ eoi)(new LayoutScanner(in))
  def phrase[T](p:Parser[T], in:java.io.Reader) = 
    super.phrase(p <~ eoi)(new LayoutScanner(in))
}
