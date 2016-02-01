/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.parser

import scala.language.postfixOps
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Positional

import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.common.PprUtils.{pprMany}

/**
 * Bluejelly parser.
 * @author ppedemon
 */
object BluejellyParser extends Parsers {
  
  import Lexer._

  import bluejelly.bjc.common.Name._
  import bluejelly.bjc.common.{Name,LocalName,QualName}
  import bluejelly.bjc.common.{Scope,TcScope,IdScope,TvScope,ScopedName}
  
  import bluejelly.bjc.ast
  import bluejelly.bjc.ast.NameConstants._
  import bluejelly.bjc.ast.{ArrowCon,TupleCon,Decl}
  import bluejelly.bjc.ast.dcons._
  import bluejelly.bjc.ast.types._
  import bluejelly.bjc.ast.pat._
  import bluejelly.bjc.ast.exp._  
  import bluejelly.bjc.ast.decls._
  import bluejelly.bjc.ast.module._
  
  import bluejelly.utils.{Name => N}

  type Elem = Token
    
  // ---------------------------------------------------------------------
  // Some code to produce decent error messages
  // ---------------------------------------------------------------------
  
  override def acceptIf(p: Elem => Boolean)(err: Elem => String): Parser[Elem] = Parser { in =>
    if (in.first.isInstanceOf[ErrorToken]) Failure(in.first.asInstanceOf[ErrorToken].msg, in)
    else if (p(in.first)) Success(in.first, in.rest)
    else if (in.atEnd) Failure("end of input", in)
    else Failure(err(in.first), in)
  }

  override def elem(kind: String, p: Elem => Boolean) = acceptIf(p)(t =>  
    "unexpected " + t.unexpected + " (" + kind + " expected)")

  def elem(p: Elem => Boolean) = acceptIf(p)(t =>
    "unexpected " + t.unexpected)
  
  private def lexError = elem("lex error", _.isInstanceOf[ErrorToken]) >> {
    case ErrorToken(s) => Parser{ in => 
      new Error(s,in) {
        override def toString = "[%s] lexical error: %s" format (next.pos,msg)
      }
    }
    case _ => ???
  }

  // ---------------------------------------------------------------------
  // Basic parsers
  // ---------------------------------------------------------------------

  // Names
  private def _VARID = elem("identifier",_.isInstanceOf[VarId]) ^^ { 
    case VarId(id) => id; case _ => ??? 
  }
  private def _VAROP = elem("operator",_.isInstanceOf[VarSym]) ^^ { 
    case VarSym(op) => op; case _ => ??? 
  }
  private def _QVARID = elem("identifier",_.isInstanceOf[QVarId]) ^^ { 
    case QVarId(id) => id; case _ => ??? 
  }
  private def _QVAROP = elem("operator",_.isInstanceOf[QVarSym]) ^^ { 
    case QVarSym(op) => op; case _ => ??? 
  }
  private def _CONID = elem("constructor",_.isInstanceOf[ConId]) ^^ { 
    case ConId(id) => id; case _ => ??? 
  }
  private def _CONOP = elem("constructor",_.isInstanceOf[ConSym]) ^^ { 
    case ConSym(op) => op; case _ => ??? 
  }
  private def _QCONID = elem("constructor",_.isInstanceOf[QConId]) ^^ { 
    case QConId(id) => id; case _ => ??? 
  }
  private def _QCONOP = elem("constructor",_.isInstanceOf[QConSym]) ^^ { 
    case QConSym(op) => op; case _ => ??? 
  }
  
  // Literals
  private def lit = 
    (elem(_.isInstanceOf[IntLit]) ^^ 
      { case IntLit(i) => ast.IntLit(i); case _ => ??? }
    |elem(_.isInstanceOf[FloatLit]) ^^ 
      { case FloatLit(d) => ast.FloatLit(d); case _ => ??? }
    |elem(_.isInstanceOf[CharLit]) ^^ 
      { case CharLit(c) => ast.CharLit(c); case _ => ??? }
    |elem(_.isInstanceOf[StringLit]) ^^ 
      { case StringLit(s) => ast.StringLit(s); case _ => ??? })    
  
  // Keywords
  private def as       = elem(kwd("as"), _.isInstanceOf[TAs])
  private def `case`   = elem(kwd("case"), _.isInstanceOf[TCase])
  private def `class`  = elem(kwd("class"), _.isInstanceOf[TClass])
  private def data     = elem(kwd("data"), _.isInstanceOf[TData])
  private def default  = elem(kwd("default"), _.isInstanceOf[TDefault])
  private def deriving = elem(kwd("deriving"), _.isInstanceOf[TDeriving])
  private def `do`     = elem(kwd("do"), _.isInstanceOf[TDo])
  private def `else`   = elem(kwd("else"), _.isInstanceOf[TElse])
  private def hiding   = elem(kwd("hiding"), _.isInstanceOf[THiding])
  private def `if`     = elem(kwd("if"), _.isInstanceOf[TIf])
  private def `import` = elem(kwd("import"), _.isInstanceOf[TImport])
  private def instance = elem(kwd("instance"), _.isInstanceOf[TInstance])
  private def in       = elem(kwd("in"), _.isInstanceOf[TIn])
  private def infix    = elem(kwd("infix"), _.isInstanceOf[TInfix])
  private def infixl   = elem(kwd("infixl"), _.isInstanceOf[TInfixl])
  private def infixr   = elem(kwd("infixr"), _.isInstanceOf[TInfixr])
  private def let      = elem(kwd("let"), _.isInstanceOf[TLet])
  private def module   = elem(kwd("module"), _.isInstanceOf[TModule])
  private def newtype  = elem(kwd("newtype"), _.isInstanceOf[TNewtype])
  private def of       = elem(kwd("of"), _.isInstanceOf[TOf])
  private def prim     = elem(kwd("primitive"), _.isInstanceOf[TPrim])
  private def qual     = elem(kwd("qualified"), _.isInstanceOf[TQualified])
  private def `then`   = elem(kwd("then"), _.isInstanceOf[TThen])
  private def ty       = elem(kwd("type"), _.isInstanceOf[TType])
  private def where    = elem(kwd("where"), _.isInstanceOf[TWhere])
    
  private def dot = elem(_ match {
    case t:VarSym => (t.asInstanceOf[VarSym].sym.name == Symbol("."))
    case _ => false
  })
  
  private def bang = elem(_ match {
    case t:VarSym => (t.asInstanceOf[VarSym].sym.name == Symbol("!"))
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
  private def from    = elem(_.isInstanceOf[TLArr])
  private def under   = elem(_.isInstanceOf[TUnder])
  private def eq      = elem(_.isInstanceOf[TEq])
  private def bar     = elem(_.isInstanceOf[TBar])
  private def tilde   = elem(_.isInstanceOf[TTilde])
  private def at      = elem(_.isInstanceOf[TAt])
  private def lambda  = elem(_.isInstanceOf[TLam])
  
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
  
  private def eoi = elem("end of input",_.isInstanceOf[EOI])

  private def $[T <: Positional](p:Parser[T]) = positioned(p)
  
  // ---------------------------------------------------------------------
  // Variables + Constructors (as identifiers or operators)
  // ---------------------------------------------------------------------
  
  private def nmMinus = Name.idName(ncMinus)

  private def mkName(n:N, scope:Scope) = {
    if (n.isQual) Name.qualName(n.qual.get, n.name, scope) 
      else Name.localName(n.name, scope)
  }     
  private def idName(n:N) = mkName(n, IdScope)
  private def tcName(n:N) = mkName(n, TcScope)
  private def tvName(n:N) = mkName(n, TvScope)

  private def coerceUnqual:PartialFunction[Name,LocalName] = {
    case x@LocalName(_) => x
  }

  private def varid(scope:Scope = IdScope) = 
    ( elem("identifier", _.isInstanceOf[TAs]) ^^^ localName(ncAs, scope)
    | elem("identifier", _.isInstanceOf[THiding]) ^^^ localName(ncHiding, scope)
    | elem("identifier", _.isInstanceOf[TQualified]) ^^^ localName(ncQualified, scope)
    | _VARID ^^ {mkName(_,scope)})
  
  private def tyvar = varid(TvScope)

  private def `var` = 
    ( varid()
    | lpar ~> _VAROP <~ rpar ^^ {idName(_)}
    | lpar ~> minus <~ rpar ^^^ nmMinus)
  
  private def qvar = 
    (_QVARID ^^ {idName(_)}
    | lpar ~> _QVAROP <~ rpar ^^ {idName(_)}
    | `var` )

  private def varopNoMinus = 
    ( _VAROP ^^ {idName(_)} 
    | back ~> varid() <~ back )
  
  private def varop = 
    varopNoMinus | minus ^^^ nmMinus
    
  private def qvaropNoMinus = 
    ( _QVAROP ^^ {idName(_)} 
    | back ~> _QVARID <~ back ^^ {idName(_)} 
    | varopNoMinus )
    
  private def qvarop = 
    qvaropNoMinus | minus ^^^ nmMinus
    
  private def con = 
    ( _CONID ^^ {idName(_)}
    | lpar ~> _CONOP  <~ rpar ^^ {idName(_)})
  
  private def qcon = 
    ( _QCONID ^^ {idName(_)} 
    | lpar ~> _QCONOP <~ rpar ^^ {idName(_)} 
    | con )
  
  private def conop = 
    ( _CONOP ^^ {idName(_)}
    | back ~> _CONID  <~ back ^^ {idName(_)})
  
  private def qconop = 
    ( _QCONOP ^^ {idName(_)}
    | back ~> _QCONID <~ back ^^ {idName(_)} 
    | conop )
  
  private def qconid(scope:Scope = TcScope) = 
    (_QCONID | _CONID) ^^ {mkName(_,scope)}  

  private def tcon = _CONID ^^ {tcName(_)}

  private def op = varop | conop
  private def qop = qvarop | qconop
  
  private def modid = ((_VARID <~ dot)*) ~ _CONID ^^ {
    case qs~m => if (qs.isEmpty) m.name else 
      Symbol("%s.%s" format (qs mkString ("",".",""), m.name.name)) 
  }

  private def commas = (comma+) ^^ {_.length+1}
  
  private def commaVars = rep1sep(`var`,comma)
  
  private def gcon = 
    ( qcon ^^ {ast.Con(_)}
    | lpar ~ rpar ^^^ ast.UnitCon
    | lbrack ~ rbrack ^^^ ast.ListCon
    | lpar ~> commas <~ rpar ^^ {TupleCon(_)})
  
  // ---------------------------------------------------------------------
  // Exports
  // ---------------------------------------------------------------------
  private def expSpec = $(
    ( qconid() <~ (lpar ~ dotdot ~ rpar) ^^ {EAll(_)}
    | qconid() ~ (lpar ~> enames <~ rpar) ^^ {case e~es => ESome(e,es)}
    | qconid() ^^ {EAbs(_)}
    | qvar ^^ {EVar(_)}
    | module ~> modid ^^ {EMod(_)}))
  
  private def enames = repsep(qvar|qcon,comma) <~ (comma?)
  
  private def exports = 
    ( lpar ~> (repsep(expSpec,comma) <~ (comma?)) <~ rpar ^^ {ExportSome(_)} 
    | success(ExportAll))

  // ---------------------------------------------------------------------
  // Imports
  // ---------------------------------------------------------------------
  private def inames = repsep(`var`|con,comma) <~ (comma?) ^^ {
    _ map coerceUnqual
  }

  private def impSpec = 
    ( tcon <~ (lpar ~ dotdot ~ rpar) ^^ {n => IAll(coerceUnqual(n))}
    | tcon ~ (lpar ~> inames <~ rpar) ^^ {case i~is => ISome(coerceUnqual(i),is)}
    | tcon ^^ {n => INone(coerceUnqual(n))}
    |`var` ^^ {n => IVar(coerceUnqual(n))})
   
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
  
  private def topType:Parser[Type] = qtype
  
  private def qtype = ((context <~ implies)?) ~ `type` ^^ {
    case None~ty => ty 
    case Some(preds)~ty => QualType(preds,ty)
  }
  
  private def `type` = rep(btype <~ rarr) ~ (btype) ^^ {
    case Nil~ty => ty
    case tys~ty => Type.mkFun(tys,ty)
  }
  
  private def btype = btype1 | btype2
  
  private def context = 
    pred ^^ {List(_)} | 
    lpar ~> repsep(pred,comma) <~ rpar
  
  private def pred = 
    btype2 ^? (Type.mkPred, "invalid predicate: %s" format _)  
    
  private def btype1 = atype1 ~ (atype*) ^^ {
    case ty~args => Type.mkApp(ty, args) 
  }
  
  private def btype2 = qconid() ~ (atype*) ^^ {
    case n~args => Type.mkApp(Type.tyCon(n), args)
  }
  
  private def atype = atype1 | qconid() ^^ {Type.tyCon(_)}
  
  private def atype1:Parser[Type] = 
    ( tyvar ^^ {TyVar(_)}
    | lpar ~ rarr ~ rpar ^^^ Type.arrowCon
    | lpar ~> commas <~ rpar ^^ {Type.tupleCon(_)}
    | lpar ~> repsep(`type`,comma) <~ rpar ^^ {
        case Nil => Type.unitCon
        case List(ty) => ty
        case tys => Type.mkApp(Type.tupleCon(tys.length), tys)
      }
    | lbrack ~> (`type`?) <~ rbrack ^^ {
        case None => Type.listCon
        case Some(ty) => AppType(Type.listCon,ty)
      })

  // ---------------------------------------------------------------------
  // Patterns
  // ---------------------------------------------------------------------

  private def pat = $(infixPat)
  
  private def infixPat = pat10 ~ rep(qconop ~ pat10) ^^ {
    case p~xs => xs.foldLeft(p)((p,q) => InfixPat(p,q._1,q._2))
  }
    
  private def pat10 = (minus?) ~ (fpat|apat) ^^ {
    case Some(_)~p => NegPat(p)
    case None~p=> p
  }
  
  private def fpat = gcon ~ rep1(apat) ^^ {
    case con~args => Pat.appPat(ConPat(con), args) 
  }
  
  private def apat:Parser[Pat] = 
    ( (`var` <~ at) ~ pat ^^ {case v~p => AsPat(v,p)}
    | lit ^^ {LitPat(_)}
    | under ^^^ WildPat
    | `var` ^^ {VarPat(_)}
    | tilde ~> apat ^^ {LazyPat(_)} 
    | lbrack ~> rep1sep(pat,comma) <~ rbrack ^^ {ListPat(_)}
    | lpar ~> rep1sep(pat,comma) <~ rpar ^^ {
      case List(p) => ParPat(p) 
      case ps => Pat.tuplePat(ps)
    }
    | qcon ~ (lcurly ~> repsep(recBind,comma) <~ rcurly) ^^ {
      case con~binds => RecPat(con,binds)  
    }
    | gcon ^^ {ConPat(_)})

  private def recBind = (qvar <~ eq) ~ pat ^^ { case v~p => PBind(v,p) }

  // ---------------------------------------------------------------------
  // Expressions
  // ---------------------------------------------------------------------
          
  private def exp:Parser[Exp] = exp0 ~ opt(coco ~> qtype) ^^ {
    case e~None => e
    case e~Some(ty) => TySigExp(e,ty)
  }
  
  private def exp0:Parser[Exp]  = genInfix(exp10a|exp10b) 
  
  private def mneg(p:Parser[Exp]) = (minus?) ~ p ^^ {
    case None~e => e 
    case _~e => NegExp(e)
  }
  private def genInfix(p:Parser[Exp]) = mneg(p) ~ rep(qop ~ mneg(p)) ^^ {
    case e~ops => ops.foldLeft(e)((e,op) => InfixExp(e,op._1,op._2))
  }
  
  private def exp10a = 
    ((`case` ~> exp <~ of) ~ block(alts) ^^ {case e~alts => CaseExp(e,alts)}
    |`do` ~> block(stmts) ^^ {DoExp(_)}
    | (aexp+) ^^ {Exp.appExp(_)})
    
  private def exp10b = 
    ((lambda ~> rep(apat)) ~ (rarr ~> exp) ^^ {case ps~e => LambdaExp(ps,e)}
    |(let ~> decls) ~ (in ~> exp) ^^ {case decls~e => LetExp(decls,e)}
    |(`if` ~> exp) ~ (`then` ~> exp) ~ (`else` ~> exp) ^^ {
      case c~t~e => IfExp(c,t,e)
    })
 
  private def aexp:Parser[Exp] = aexp0 ~ (fbinds*) ^^ {
    case e~Nil => e
    case e~bss => bss.foldLeft(e)(RecUpdExp(_,_)) 
  }
  
  private def aexp0:Parser[Exp] = 
    ( lit ^^ {LitExp(_)}
    | qcon ~ fbinds ^^ {case c~bs => RecConExp(c,bs)}
    | gcon ^^ {ConExp(_)}
    | qvar ^^ {VarExp(_)}
    | lpar ~> rep1sep(exp,comma) <~ rpar ^^ {
      case List(e) => ParExp(e)
      case es => Exp.tupleExp(es)
    }
    | lpar ~> (exp10a ~ qop) <~ rpar ^^ {case e~op => LeftSectExp(e,op)}
    | lpar ~> (qvaropNoMinus ~ exp0) <~ rpar ^^ {case op~e => RightSectExp(op,e)}
    | lpar ~> (qconop ~ exp0) <~ rpar ^^ {case op~e => RightSectExp(op,e)}
    | lbrack ~> listExp <~ rbrack)
  
  // List expressions
  private def listExp = 
    ( (exp <~ bar) ~ rep1sep(stmt,comma) ^^ {case e~qs => ListComp(e,qs)}
    | (exp <~ dotdot) ~ (exp?) ^^ {
      case from~None => EnumFromExp(from)
      case from~Some(to) => EnumFromToExp(from,to)
    }
    | (exp <~ comma) ~ (exp <~ dotdot) ~ (exp?) ^^ {
      case from~th~None => EnumFromThenExp(from,th)
      case from~th~Some(to) => EnumFromThenToExp(from,th,to)
    }
    | rep1sep(exp,comma) ^^ {ListExp(_)})

  // Case alternatives
  private def alts = (semi*) ~> rep1sep(alt,semi+) <~ (semi*)
  private def alt = pat ~ altRhs ~ wherePart ^^ {
    case p~rhs~ds => new Alt(p,rhs,ds)
  }
  private def altRhs = 
    (grdAlt+) ^^ {GrdAltRhs(_)} | rarr ~> exp ^^ {ExpAltRhs(_)}
  private def grdAlt = (bar ~> exp0) ~ (rarr ~> exp) ^^ {
    case g~e => new Guarded(g,e)
  }
  
  // Statements
  private def stmts = (semi*) ~> rep1sep(stmt,semi+) <~ (semi*)
  private def stmt = 
    ((pat <~ from) ~ exp ^^ {case p~e => FromStmt(p,e)}
    |let ~> decls ^^ {LetStmt(_)}
    |exp ^^ {ExpStmt(_)})
  
  // Record bindings
  private def fbinds = lcurly ~> repsep(fbind,comma) <~ rcurly
  private def fbind = (qvar <~ eq) ~ exp ^^ {case v~e => FBind(v,e)}
    
  // ---------------------------------------------------------------------
  // Type constructors, type synonyms, new types
  // ---------------------------------------------------------------------
  
  private def tyLhs = tcon ~ (tyvar*)
  private def mbang = (bang?) ^^ {_.map(_=>true).getOrElse(false)}
  
  private def derivings = 
    deriving ~> qconid() ^^ {List(_)} |
    deriving ~> (lpar ~> repsep(qconid(),comma) <~ rpar)
  
  private def labelGrp = 
    ((commaVars <~ coco) ~ `type` ^^ { 
      case vs~ty => new LabelGroup(vs,ty,false)
    }
    |(commaVars <~ coco) ~ (bang ~> atype) ^^ {
      case vs~ty => new LabelGroup(vs,ty,true)
    })
  
  private def dcon = qconid(IdScope) | lpar ~> _CONOP <~ rpar ^^ {idName(_)}

  private def conopArg = 
    btype ^^ {new DConArg(_,false)} | bang ~> atype ^^ {new DConArg(_,true)}
  
  private def conidArg =
    mbang ~ atype ^^ {case strict~ty => new DConArg(ty,strict)} 
    
  private def constr = 
    (conopArg ~ conop ~ conopArg ^^ {
      case ty1~n~ty2 => new AlgDCon(n,List(ty1,ty2))
    }
    |con ~ (lcurly ~> repsep(labelGrp,comma) <~ rcurly) ^^ {
      case n~grps => new RecDCon(n,grps)
    }
    |dcon ~ (conidArg*) ^^ {
      case n~tys => new AlgDCon(n, tys)
    })
  
  private def constrs = rep1sep(constr,bar)
  
  private def synDecl = $((ty ~> tyLhs) ~ (eq ~> `type`) ^^ {
   case (conid~vs)~ty => new TySynDecl(conid,vs,ty)
  })
  
  private def dataDecl = $(data ~> tyLhs ~ (eq ~> constrs) ~ (derivings?) ^^ {
      case (conid~vs)~dcons~ds => 
        new DataDecl(conid,vs,dcons,ds.getOrElse(Nil))
    })
    
  private def newtyDecl = $(newtype ~> tyLhs ~ (eq ~> constr) ~ (derivings?) ^^ {
      case (conid~vs)~dcon~ds => 
        new NewTyDecl(conid,vs,dcon,ds.getOrElse(Nil))
    })
  
  // ---------------------------------------------------------------------
  // Class and instances
  // ---------------------------------------------------------------------
  
  private def rule = 
    (context <~ implies) ~ pred ^^ { case ctx ~ p => (Some(ctx),p) } |
    pred ^^ {(None,_)}
  
  private def classDecl = (`class` ~> rule) ~ wherePart ^^ {
    case (ctx,p)~ws => ClassDecl(ctx,p,ws)
  }
  
  private def instDecl = (instance ~> rule) ~ wherePart ^^ {
    case (ctx,p)~ws => InstDecl(ctx,p,ws)
  }

  // ---------------------------------------------------------------------
  // Default declarations
  // ---------------------------------------------------------------------

  // Default declarations
  private def defaultDecl = 
    default ~> lpar ~> repsep(`type`,comma) <~ rpar ^^ { DefaultDecl(_) }
  
  // ---------------------------------------------------------------------
  // Declarations
  // ---------------------------------------------------------------------
     
  private def topDecl = $(
      synDecl      |
      dataDecl     |
      newtyDecl    |
      classDecl    |
      instDecl     |
      defaultDecl) | 
      decl         | 
      lexError
      
  // Type signatures
  private def tysig = (commaVars <~ coco) ~ topType ^^ 
    { case vars~ty => new TySigDecl(vars, ty) }
  
  // Fixity declarations
  private def fixity =
    (infix ~> prec ~ rep1sep(op,comma) ^^ {
      case p ~ ops => FixityDecl(NoAssoc,p,ops)
    }
    |infixl ~> prec ~ rep1sep(op,comma) ^^ {
      case p ~ ops => FixityDecl(LeftAssoc,p,ops)
    }
    |infixr ~> prec ~ rep1sep(op,comma) ^^ {
      case p ~ ops => FixityDecl(RightAssoc,p,ops)
    })   
    
  private def prec = elem(_.isInstanceOf[IntLit]) ^? ({ 
    case IntLit(i) if i > 0 && i <= FixityDecl.maxPrec => i.toInt;
    case _ => ???
  },{
    case IntLit(x) => "invalid operator precedence: " + x;
    case _ => ??? 
  }) | success(FixityDecl.defPrec)
  
  // Function and pattern binds
  private def funlhs = funlhs0|funlhs1
  private def funlhs0 = infixPat ~ varop ~ infixPat ^^ {
    case p0 ~ op ~ p1 => (op,List(p0,p1))
  }
  private def base = 
    ( (lpar ~> funlhs0 <~ rpar) ~ apat ^^ { case (f,args)~p1 => (f, args :+ p1) }
    | `var` ~ apat ^^ { case v~p => (v,List(p)) })
  private def funlhs1:Parser[(Name,List[Pat])] = 
    ( base ~ (apat*) ^^ { case (f,args)~ps => (f,args ++ ps) } 
    | (lpar ~> funlhs1 <~ rpar) ~ (apat+) ^^ { 
        case (f,args)~ps => (f,args ++ ps) 
    })
    
  private def rhs = (grdrhs+) ^^ {GrdRhs(_)} | eq ~> exp ^^ {FunRhs(_)}
  private def grdrhs = (bar ~> exp) ~ (eq ~> exp) ^^ {
    case g~e => new Guarded(g,e)
  }

  private def funbind = funlhs ~ rhs ~ wherePart ^^ {
    case (f,args) ~ rhs ~ w => FunBind(f,args,rhs,w)
  }
    
  private def patbind = infixPat ~ rhs ~ wherePart ^^ {
    case p ~ rhs ~ w => PatBind(p,rhs,w)
  }

  private def decl = $(tysig|fixity|funbind|patbind)
  
  private def wherePart:Parser[List[Decl]] = where ~> decls | success(Nil)
  private def decls = block((semi*) ~> repsep(decl,(semi+)) <~ (semi*))
  
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
    (p <~ eoi)(new LayoutScanner(in))
  def phrase[T](p:Parser[T], in:java.io.Reader) = 
    (p <~ eoi)(new LayoutScanner(in))
}
