/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.parser

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

  import bluejelly.bjc.common.Name
  import bluejelly.bjc.common.Name._
  
  import bluejelly.bjc.ast
  import bluejelly.bjc.ast.NameConstants._
  import bluejelly.bjc.ast.{ArrowCon,TupleCon,Decl}
  import bluejelly.bjc.ast.dcons._
  import bluejelly.bjc.ast.types._
  import bluejelly.bjc.ast.pat._
  import bluejelly.bjc.ast.exp._  
  import bluejelly.bjc.ast.decls._
  import bluejelly.bjc.ast.module._
  
  type Elem = Token
    
  // ---------------------------------------------------------------------
  // Some code to produce decent error messages
  // ---------------------------------------------------------------------
  
  private def lexError = elem("", _.isInstanceOf[ErrorToken]) >> {
    case ErrorToken(s) => Parser{ in => new Error(s,in) {
      override def toString = "[%s] lexical error: %s" format (next.pos,msg)  
    }}
  }

  override def elem(kind: String, p: Elem => Boolean) = acceptIf(p)(t =>  
    "unexpected " + t.unexpected + " (" + kind + ") expected")
  
  def elem(p: Elem => Boolean) = acceptIf(p)(t =>
    "unexpected " + t.unexpected)
  
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
  
  // Literals
  private def lit = 
    (elem(_.isInstanceOf[IntLit])    ^^ {case IntLit(i) => ast.IntLit(i)}
    |elem(_.isInstanceOf[FloatLit])  ^^ {case FloatLit(d) => ast.FloatLit(d)}
    |elem(_.isInstanceOf[CharLit])   ^^ {case CharLit(c) => ast.CharLit(c)}
    |elem(_.isInstanceOf[StringLit]) ^^ {case StringLit(s) => ast.StringLit(s)})    
  
  // Keywords
  private def as       = elem(kwd("as"), _.isInstanceOf[TAs])
  private def `case`   = elem(kwd("case"), _.isInstanceOf[TCase])
  private def `class`  = elem(kwd("class"), _.isInstanceOf[TClass])
  private def data     = elem(kwd("data"), _.isInstanceOf[TData])
  private def default  = elem(kwd("default"), _.isInstanceOf[TDefault])
  private def deriving = elem(kwd("deriving"), _.isInstanceOf[TDeriving])
  private def `do`     = elem(kwd("do"), _.isInstanceOf[TDo])
  private def `else`   = elem(kwd("else"), _.isInstanceOf[TElse])
  private def mdo      = elem(kwd("mdo"), _.isInstanceOf[TMDo])
  private def forall   = elem(kwd("forall"), _.isInstanceOf[TForall])
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
  private def then     = elem(kwd("then"), _.isInstanceOf[TThen])
  private def ty       = elem(kwd("type"), _.isInstanceOf[TType])
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

  private def commas = (comma+) ^^ {_.length+1}
  
  private def commaVars = rep1sep(vars,comma)
  
  private def gcon = 
    ( qcon ^^ {ast.Con(_)}
    | lpar ~ rpar ^^^ ast.UnitCon
    | lbrack ~ rbrack ^^^ ast.ListCon
    | lpar ~> commas <~ rpar ^^ {TupleCon(_)})
  
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
  
  private def topType:Parser[Type] = $(polyType | qtype)
  
  private def polyType:Parser[Type] = 
    (forall ~> (vars+)) ~ (dot ~> qtype) ^^ {
      case xs~ty => PolyType(xs,ty)
    }   

  private def qtype = ((context <~ implies)?) ~ `type` ^^ {
    case None~ty => ty 
    case Some(preds)~ty => QualType(preds,ty)
  }
  
  private def `type` = rep(ltype <~ rarr) ~ (btype1|btype2) ^^ {
    case Nil~ty => ty
    case tys~ty => Type.mkFun(tys,ty)
  }
  
  private def ltype = bpoly | btype1 | btype2
  private def bpoly = (lpar+) ~> polyType <~ (rpar+)
  
  private def context = 
    pred ^^ {List(_)} | 
    lpar ~> repsep(pred,comma) <~ rpar
  
  private def pred = 
    btype2 ^? (Type.mkPred, "invalid predicate: %s" format _)  
    
  private def btype1 = atype1 ~ (atype*) ^^ {
    case ty~args => Type.mkApp(ty, args) 
  }
  
  private def btype2 = qconid ~ (atype*) ^^ {
    case n~args => Type.mkApp(Type.tyCon(n), args)
  }
  
  private def atype = atype1 | qconid ^^ {Type.tyCon(_)}
  
  private def atype1:Parser[Type] = 
    ( varid ^^ {TyVar(_)}
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
      }
    | under ^^^ AnonTyVar())

  // ---------------------------------------------------------------------
  // Patterns
  // ---------------------------------------------------------------------

  private def pat = $(
    ( infixPat ~ (coco ~> `type`) ^^ {case p~ty => TySigPat(p,ty)}
    | infixPat ))
  
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
    ( (vars <~ at) ~ pat ^^ {case v~p => AsPat(v,p)}
    | lit ^^ {LitPat(_)}
    | under ^^^ WildPat
    | vars ^^ {VarPat(_)}
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

  private def recBind = 
    ( (qvar <~ eq) ~ pat ^^ { case v~p => AsgPBind(v,p) }
    | vars ^^ {VarPBind(_)})
    
  // ---------------------------------------------------------------------
  // Expressions
  // ---------------------------------------------------------------------
      
  private def exp:Parser[Exp] = 
    ((exp0a <~ coco) ~ qtype ^^ {case e~ty => TySigExp(e,ty)}
    |exp0)
  
  private def exp0:Parser[Exp]  = exp0a|exp0b 
  private def exp0a:Parser[Exp] = genInfix(exp10a)
  private def exp0b:Parser[Exp] = genInfix(exp10b)
  
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
    | mdo ~> block(stmts) ^^ {MDoExp(_)}
    | (aexp+) ^^ {Exp.appExp(_)})
    
  private def exp10b = 
    ((lambda ~> rep(apat)) ~ (rarr ~> exp) ^^ {case ps~e => LambdaExp(ps,e)}
    |(let ~> decls) ~ (in ~> exp) ^^ {case decls~e => LetExp(decls,e)}
    |(`if` ~> exp) ~ (then ~> exp) ~ (`else` ~> exp) ^^ {
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
      case from~then~None => EnumFromThenExp(from,then)
      case from~then~Some(to) => EnumFromThenToExp(from,then,to)
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
  private def fbind = 
    (qvar <~ eq) ~ exp ^^ {case v~e => UpdBind(v,e)} | vars ^^ {VarBind(_)}
    
  // ---------------------------------------------------------------------
  // Type constructors, type synonyms, new types
  // ---------------------------------------------------------------------
  
  private def tyLhs = CONID ~ (varid*)
  private def mbang = (bang?) ^^ {_.map(_=>true).getOrElse(false)}
  
  private def derivings = 
    deriving ~> qconid ^^ {List(_)} |
    deriving ~> (lpar ~> repsep(qconid,comma) <~ rpar)
  
  private def labelGrp = 
    ((commaVars <~ coco) ~ polyType ^^ { 
      case vs~ty => new LabelGroup(vs,ty,false)
    }
    |(commaVars <~ coco) ~ (mbang ~ `type`) ^^ {
      case vs~(strict~ty) => new LabelGroup(vs,ty,strict)
    })
  
  private def conopArg = 
    (bpoly ^^ {new DConArg(_,false)} 
    |mbang ~ (atype+) ^^ {
      case strict~tys => new DConArg(Type.mkApp(tys.head, tys.tail),strict)
    })
  
  private def conidArg = 
    (bpoly ^^ {new DConArg(_,false)} 
    |mbang ~ atype ^^ {case strict~ty => new DConArg(ty,strict)})
    
  private def constr = 
    (conopArg ~ conop ~ conopArg ^^ {
      case ty1~n~ty2 => new AlgDCon(n,List(ty1,ty2))
    }
    |con ~ (lcurly ~> repsep(labelGrp,comma) <~ rcurly) ^^ {
      case n~grps => new RecDCon(n,grps)
    }
    |(qconid|lpar ~> CONOP <~ rpar) ~ (conidArg*) ^^ {
      case n~tys => new AlgDCon(n, tys)
    })
  
  private def qconstr = ((context <~ implies)?) ~ constr ^^ {
    case None~dcon => dcon
    case Some(preds)~ty => new QualDCon(preds,ty)
  }  

  private def pconstr = 
    ((forall ~> (vars+)) ~ (dot ~> qconstr) ^^ {
      case vs~qcon => new PolyDCon(vs,qcon)
    }
    |qconstr)
  
  private def pconstrs = rep1sep(pconstr,bar)
  
  private def ntyLhs = tyLhs ^? ({
    case conid~List(v) => (conid,v)
  },{
    case conid~vs => "invalid newtype constructor: " + new PrettyPrintable {
      def ppr = pprMany(conid::vs)
    }
  })
  
  private def synDecl = $((ty ~> tyLhs) ~ (eq ~> `type`) ^^ {
   case (conid~vs)~ty => new TySynDecl(conid,vs,ty)
  })
  
  private def dataDecl = $(data ~>
    (((context <~ implies)?) ~ tyLhs ~ (eq ~> pconstrs) ~ (derivings?) ^^ {
      case preds~(conid~vs)~dcons~ds => 
        new DataDecl(conid,vs,preds,dcons,ds.getOrElse(Nil))
    }
    |((context <~ implies)?) ~ tyLhs ^^ {
      case preds~(conid~vs) =>
        new DataDecl(conid,vs,preds,Nil,Nil)
    }))
  
  private def newtyDecl = $(newtype ~> 
    (((context <~ implies)?) ~ ntyLhs ~ (eq ~> pconstr) ~ (derivings?) ^^ {
      case preds~p~dcon~ds => 
        new NewTyDecl(p._1,p._2,preds,dcon,ds.getOrElse(Nil))
    }))
  
  // ---------------------------------------------------------------------
  // Class and instances
  // ---------------------------------------------------------------------
  
  private def rule = 
    (context <~ implies) ~ pred ^^ { case ctx ~ p => (Some(ctx),p) } |
    pred ^^ {(None,_)}
  
  private def fundep = 
    (rep(vars) <~ rarr) ~ rep(vars) ^^ { case from ~ to => new FunDep(from,to) }
  
  private def fds = bar ~> rep1sep(fundep,comma) | success(Nil)
  
  private def classDecl = (`class` ~> rule) ~ fds ~ wherePart ^^ {
    case (ctx,p)~fdeps~ws => ClassDecl(ctx,p,fdeps,ws)
  }
  
  private def instDecl = (instance ~> rule) ~ wherePart ^^ {
    case (ctx,p)~ws => InstDecl(ctx,p,ws)
  }

  // ---------------------------------------------------------------------
  // Primitive and default declarations
  // ---------------------------------------------------------------------

  // Default declarations
  private def defaultDecl = 
    default ~> lpar ~> repsep(`type`,comma) <~ rpar ^^ { DefaultDecl(_) }
  
  // Primitive declarations
  private def primDefn = 
    vars ~ elem(_.isInstanceOf[StringLit]) ^^ { case v~StringLit(s) => (v,s) }   
    
  private def primDecl = 
    (prim ~> rep1sep(primDefn,comma)) ~ (coco ~> topType) ^^ {
      case ps~ty => PrimDecl(ps,ty) 
    }

  // ---------------------------------------------------------------------
  // Declarations
  // ---------------------------------------------------------------------
     
  private def topDecl = $(
      synDecl     |
      dataDecl    |
      newtyDecl   |
      classDecl   |
      instDecl    |
      defaultDecl |
      primDecl)   | decl
      
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
    case IntLit(i) if i > 0 && i <= FixityDecl.maxPrec => i.toInt
  },{
    case IntLit(x) => "invalid operator precedence: " + x 
  }) | success(FixityDecl.defPrec)
  
  // Function and pattern binds
  private def funlhs = funlhs0|funlhs1
  private def funlhs0 = infixPat ~ varop ~ infixPat ^^ {
    case p0 ~ op ~ p1 => (op,List(p0,p1))
  }
  private def base = 
    ( (lpar ~> funlhs0 <~ rpar) ~ apat ^^ { case (f,args)~p1 => (f, args :+ p1) }
    | vars ~ apat ^^ { case v~p => (v,List(p)) })
  private def funlhs1:Parser[(Name,List[Pat])] = 
    ( base ~ (apat*) ^^ { case (f,args)~ps => (f,args ++ ps) } 
    | (lpar ~> funlhs1 <~ rpar) ~ (apat+) ^^ { 
        case (f,args)~ps => (f,args ++ ps) 
    })
    
  private def rhs = (grdrhs+) ^^ {GrdRhs(_)} | eq ~> exp ^^ {FunRhs(_)}
  private def grdrhs = (bar ~> exp) ~ (eq ~> exp) ^^ {
    case g~e => new Guarded(g,e)
  }

  private def funbind = funlhs ~ opt(coco ~> `type`) ~ rhs ~ wherePart ^^ {
    case (f,args) ~ ty ~ rhs ~ w => FunBind(f,args,ty,rhs,w)
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
    super.phrase(p <~ eoi)(new LayoutScanner(in))
  def phrase[T](p:Parser[T], in:java.io.Reader) = 
    super.phrase(p <~ eoi)(new LayoutScanner(in))
}
