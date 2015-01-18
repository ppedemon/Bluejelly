/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.iface.test

import bluejelly.bjc.common.{Name,LocalName,QualName}
import bluejelly.bjc.common.{ExportInfo,ExportedTc,ExportedId}
import bluejelly.bjc.common.Fixity
import bluejelly.bjc.common.{TcRef,ConRef,ListRef,UnitRef,ArrowRef,TupleRef}

import bluejelly.bjc.ast.{GCon,Con,ListCon,UnitCon,ArrowCon,TupleCon,Decl}
import bluejelly.bjc.ast.dcons
import bluejelly.bjc.ast.decls
import bluejelly.bjc.ast.module._
import bluejelly.bjc.ast.types

import bluejelly.bjc.iface._
import bluejelly.bjc.parser.BluejellyParser

import java.io.Reader

/**
 * Simple parser for module interfaces. Reuses the Bluejelly parser,
 * but produces a {@link ModIface} instance rather than a {@link Module}.
 * 
 * NOTE 0: this parser is purely for testing purposes. Module interfaces
 * are stored in binary format as static byte arrays. This is simply my
 * cheap way to produce interfaces so I can test the module system. The
 * code here has many limitations that I won't fix (e.g, it generates a 
 * selector function for every label in a record instead of doing so 
 * just for exported labels).
 * 
 * NOTE 1: What was supposed to be a quick testing hack ended up taking a 
 * whole weekend and ~300 lines of code! Not cool.
 * 
 * @author ppedemon
 */
object ModIfaceParser {
  
  // Return a qualified name
  private def qualName(modName:Symbol, n:Name):QualName = n match {
    case LocalName(_) => n.qualify(modName)
    case x@QualName(_,_) => x
  }

  private def mkTcRef(modName:Symbol,c:GCon) = c match {
    case Con(n) => ConRef(qualName(modName,n))
    case ListCon => ListRef
    case UnitCon => UnitRef
    case ArrowCon => ArrowRef
    case TupleCon(n) => TupleRef(n)
  }

  // Kind arbitrarily set to *
  private def mkTv(n:Name):IfaceTyVar = mkTv(n.toSymbol)

  // Kind arbitrarily set to *
  private def mkTv(n:Symbol):IfaceTyVar = 
    new IfaceTyVar(n, IfaceKStar)

  // Transform a syntactic predicate to an interface predicate
  private def mkPred(modName:Symbol)(p:types.Pred):IfacePred = 
    new IfacePred(qualName(modName,p.head), p.tys map convert(modName))
  
  // Transform a pred (C t1 ... tn) into an type constructor application
  private def predToIfaceType(modName:Symbol,p:types.Pred) = {
    val tc = IfaceTcTy(ConRef(qualName(modName,p.head)))
    p.tys.foldLeft[IfaceType](tc)((ty,arg) => IfaceAppTy(ty,convert(modName)(arg)))
  }
  
  // Generate a dictionary name from the given pred
  private def genDictName(p:types.Pred) = {
    val ctor = p.head
    val tcs = p.tys.map(types.Type.unwind(_)._1)
    val tcsUnqual = tcs map {
      case types.TyCon(Con(n)) => types.TyCon(Con(n.unqualify))
      case ty => ty
    }
    Symbol("$f" + ctor.unqualify + tcsUnqual.mkString)
  }
      
  // Collect the type variables of a (syntactic) predicate
  private def predTvs(pred:types.Pred) = 
    pred.tys.foldRight[Set[IfaceTyVar]](Set.empty)((ty,vs) => 
      tyVarsIn(ty) ++ vs)

  private def ifacePredTvs(pred:IfacePred) = 
    pred.tys.foldRight[Set[IfaceTyVar]](Set.empty)((ty,vs) => 
      ifaceTyVarsIn(ty) ++ vs)
      
  private def tyVarsIn(ty:types.Type):Set[IfaceTyVar] = ty match {
    case types.QualType(_,ty) => tyVarsIn(ty)
    case types.AppType(f,x) => tyVarsIn(f) ++ tyVarsIn(x)
    case types.TyCon(_) => Set.empty
    case types.TyVar(n) => Set(mkTv(n))
  }  

  private def ifaceTyVarsIn(ty:IfaceType):Set[IfaceTyVar] = ty match {
    case IfacePolyTy(tvs,ty) => tvs.toSet ++ ifaceTyVarsIn(ty)
    case IfaceQualTy(_,ty) => ifaceTyVarsIn(ty)
    case IfaceAppTy(f,x) => ifaceTyVarsIn(f) ++ ifaceTyVarsIn(x)
    case IfaceTcTy(_) => Set.empty
    case IfaceTvTy(n) => Set(mkTv(n))
  }

  private def predsIn(modName:Symbol,ty:types.Type) = ty match {
    case types.QualType(ps,_) => ps map mkPred(modName)
    case _ => Nil
  }

  // Assuming t is a constructor application, get an IfaceRecSelId
  private def getRecSelId(t:IfaceType) = {
    val (f,_) = IfaceType.unwind(t)
    f match {
      case IfaceTcTy(ConRef(n)) => IfaceRecSelId(n.toSymbol)
      case _ => IfaceVanillaId // Shouldn't be reached
    }
  }  

  private def qualifyPred(modName:Symbol, pred:IfacePred) =
    new IfacePred(pred.n.qualify(modName), pred.tys)

  private def narrowCtx(ctx:List[IfacePred], tys:List[IfaceType]) = {
    val allTvs = tys.foldLeft(Set.empty[IfaceTyVar])(_ ++ ifaceTyVarsIn(_))
    ctx.filter(pred => ifacePredTvs(pred).forall(allTvs.contains(_)))
  }

  // Create an interface type from the given data
  private def mkTy(
      tvs:List[IfaceTyVar], 
      ctx:List[IfacePred], 
      ty:IfaceType) = {
    val qty = if (ctx.isEmpty) ty else IfaceQualTy(ctx,ty)
    if (tvs.isEmpty) qty else IfacePolyTy(tvs,qty)
  }

  // Generate a selector type
  private def mkSelTy(
      tvs:List[IfaceTyVar], 
      ctx:List[IfacePred], 
      ty:IfaceType, 
      t:IfaceType):IfaceType = mkTy(tvs, ctx, IfaceType.mkFun(ty,t)) 

  // Generate ids for class operations with defaults
  private def mkDefIds(
      modName:Symbol,
      defs:List[Name], 
      tvs:List[IfaceTyVar], 
      pred:IfacePred, 
      t:decls.TySigDecl):List[IfaceId] = {
    t.vars.foldRight(List.empty[IfaceId])((n,ids) => defs.contains(n) match {
      case true =>
        val idName = Symbol("$dm" + n)
        val allTvs = tvs.toSet ++ tyVarsIn(t.ty)
        val allPreds = pred::predsIn(modName, t.ty)
        val idTy = mkTy(allTvs.toList, allPreds, convert(modName)(t.ty))
        IfaceId(idName, idTy, IfaceVanillaId) :: ids
      case false => ids
    })
  }
  
  // Create an interface class operation
  private def mkClsOps(
      modName:Symbol,
      defs:List[Name],
      tvs:List[IfaceTyVar],
      ctx:IfacePred,
      ds:List[Decl]):(List[IfaceClsOp],List[IfaceId]) = { 
    val qctx = qualifyPred(modName, ctx)
    val ops = ds.foldRight[List[IfaceClsOp]](Nil)((d,ds) => d match {
      case decls.TySigDecl(ns,ty) =>
        val allTvs = tvs.toSet ++ tyVarsIn(ty)
        val opTy = mkTy(allTvs.toList, List(qctx), convert(modName)(ty))
        ns.map(n => new IfaceClsOp(n.toSymbol, opTy, defs.contains(n))) ++ ds
      case _ => ds
    })
    val ids = ds.foldRight[List[IfaceId]](Nil)((d,ids) => d match {
      case t@decls.TySigDecl(_,_) => mkDefIds(modName, defs, tvs, qctx, t) ++ ids
      case _ => ids
    })
    (ops,ids)
  }
  
  private def mkClsInstArgs(modName:Symbol,pred:types.Pred):TcRef = {
    assert(pred.tys.length == 1)
    val ty = types.Type.unwind(pred.tys.head)._1
    ty match {
      case types.TyCon(gcon) => mkTcRef(modName,gcon)
      case _ => ???
    }
  }
  
  // Convert module imports
  private def convert(is:List[ImpDecl]) = is map {_.modId}
  
  // Convert module exports: we assume that parent names are
  // qualified, but not necessarily children names
  private def convert(modName:Symbol, es:Exports):List[ExportInfo] = es match {
    case ExportSome(es) => es.foldRight[List[ExportInfo]](Nil)((e,es) => 
      e match {
        case EVar(n) => 
          ExportedId(qualName(modName,n))::es
        case EAbs(n) =>
          val qn = qualName(modName,n) 
          ExportedTc(qn,List(qn))::es
        case ESome(n,ns) => 
          val qn = qualName(modName, n)
          val qual = qn.qual
          val q_ns = ns map {n => qualName(qual,n)} 
          val fixed_ns = if (!q_ns.isEmpty && q_ns.head.toSymbol == n.toSymbol) 
            qn::q_ns.tail else q_ns
          ExportedTc(qn,fixed_ns)::es
        case _ => es
      }) 
    case _ => Nil
  }
  
  // Convert fixity declarations
  private def convert(f:decls.FixityDecl):List[(Symbol,Fixity)] = 
    f.ops map {op => (op.toSymbol, new Fixity(f.assoc,f.prec))}

  // Convert type signatures
  private def convert(modName:Symbol)(ty:types.Type):IfaceType = ty match {
    case types.QualType(ctx,ty) => IfaceQualTy(ctx map mkPred(modName), convert(modName)(ty))
    case types.AppType(f,x) => IfaceAppTy(convert(modName)(f),convert(modName)(x))
    case types.TyVar(n) => IfaceTvTy(n.toSymbol)
    case types.TyCon(tc) => IfaceTcTy(mkTcRef(modName,tc))
  }

  // Convert data constructors
  private def convert(
      modName:Symbol,
      tvs:List[IfaceTyVar],
      ctx:List[IfacePred],     // Cumulative contexts found in the data cons
      ty:IfaceType, 
      dcon:dcons.DCon):(IfaceDataCon,List[IfaceId]) = dcon match {
    case dcons.AlgDCon(n,args) => 
      val tys = args map (a => convert(modName)(a.ty))
      val t = IfaceType.mkFun(tys :+ ty)
      (new IfaceDataCon(n.toSymbol, mkTy(tvs, narrowCtx(ctx,tys), t), 
        Nil, args map {_.strict}), Nil)
    case r@dcons.RecDCon(n,lgrps) =>
      val lts = r.flatten
      val labels = lts map (_._1.toSymbol)
      val strict = lts map (_._3)
      val tys = lts map (Function.tupled((_,t,_) => convert(modName)(t)))
      val ids = (labels,tys).zipped.map((l,t) => 
        IfaceId(l, mkSelTy(tvs, narrowCtx(ctx,List(t)), ty, t), getRecSelId(ty)))
      val t = IfaceType.mkFun(tys :+ ty)
      (new IfaceDataCon(n.toSymbol, mkTy(tvs, narrowCtx(ctx,tys), t), labels, strict),ids)
  }

  // Convert a type constructor declaration
  private def convert(modName:Symbol, tc:decls.DataDecl):(IfaceTyCon,List[IfaceId]) = {
    val tvs = tc.vars map mkTv
    val ctx = tc.ctx.map(_ map mkPred(modName)).getOrElse(Nil)
    val tcTy = IfaceType.mkApp(IfaceTcTy(ConRef(qualName(modName,tc.n))), 
        tvs map {tv => IfaceTvTy(tv.name)})
    val (dcons,ids) = tc.rhs.foldRight
      [(List[IfaceDataCon],List[IfaceId])]((Nil,Nil))((dcon,p) => {
        val (d,is) = convert(modName, tvs, ctx, tcTy, dcon)
        (d::p._1, is ++ p._2)
      })
    (IfaceTyCon(tc.n.toSymbol, ctx, tvs, dcons),ids)
  }
  
  // Convert a newtype declaration
  private def convert(modName:Symbol, nt:decls.NewTyDecl):(IfaceTyCon,List[IfaceId]) = {
    val tvs = nt.vars map mkTv
    val ctx = nt.ctx.map(_ map mkPred(modName)).getOrElse(Nil)
    val tcTy = IfaceType.mkApp(IfaceTcTy(ConRef(qualName(modName,nt.n))),
        tvs map {tv => IfaceTvTy(tv.name)})
    val (dcon,ids) = convert(modName, tvs, ctx, tcTy, nt.rhs)
    (IfaceTyCon(nt.n.toSymbol, ctx, tvs, List(dcon)), ids)
  }

  // Convert a type synonym declaration
  private def convert(modName:Symbol, tysyn:decls.TySynDecl):IfaceTySyn =
    IfaceTySyn(tysyn.n.toSymbol, tysyn.vars map mkTv, convert(modName)(tysyn.rhs))
  
  // Convert type signature declarations
  private def convert(modName:Symbol, tysig:decls.TySigDecl):List[IfaceId] = {
    val tvs = tyVarsIn(tysig.ty).toList
    tysig.vars map {v => 
      IfaceId(v.toSymbol, mkTy(tvs, Nil, convert(modName)(tysig.ty)), IfaceVanillaId)
    }
  }
  
  // Convert class declarations
  private def convert(modName:Symbol, cls:decls.ClassDecl):(IfaceCls,List[IfaceId]) = {
    val tvs = predTvs(cls.pred).toList
    val ctx = cls.ctx map (_ map mkPred(modName)) getOrElse Nil
    val pred = mkPred(modName)(cls.pred)
    val defs = cls.ds.foldRight[List[Name]](Nil)((d,ds) => d match {
      case f@decls.FunBind(n,_,_,_,_) => n::ds 
      case _ => ds
    })
    val (ops,ids) = mkClsOps(modName, defs, tvs, pred, cls.ds)
    (IfaceCls(cls.pred.head.toSymbol, tvs, ctx, ops),ids)
  }
  
  // Convert instance declarations
  private def convert(modName:Symbol,inst:decls.InstDecl):(IfaceClsInst,IfaceId) = {
    val tvs = predTvs(inst.pred).toList
    val ctx = inst.ctx map (_ map mkPred(modName)) getOrElse Nil
    val ty = predToIfaceType(modName,inst.pred)
    val dfun = IfaceId(genDictName(inst.pred), mkTy(tvs, ctx, ty), IfaceDFunId)
    val clsInst = new IfaceClsInst(
      qualName(modName,inst.pred.head), 
      mkClsInstArgs(modName,inst.pred), 
      dfun.name)
    (clsInst,dfun)
  }
  
  // Convert an entire module
  private def convert(mod:Module):ModIface = {
    val is = convert(mod.impDecls)
    val es = convert(mod.name, mod.exports)
    val m = new ModIface(mod.name, is, es, Nil, Nil, Nil)
    val m0 = mod.topDecls.foldLeft(m)((m,d) => d match {
      case f@decls.FixityDecl(_,_,_) =>
        val fs = convert(f)
        new ModIface(m.name, m.deps, m.exports, m.fixities ++ fs, 
            m.decls, m.insts)
      case f@decls.TySigDecl(_,_) =>
        val ids = convert(mod.name, f)
        new ModIface(m.name, m.deps, m.exports, m.fixities, 
            m.decls ++ ids, m.insts)
      case t@decls.DataDecl(_,_,_,_,_) =>
        val (tc,ids) = convert(mod.name, t)
        new ModIface(m.name, m.deps, m.exports, m.fixities, 
            m.decls ++ (tc::ids), m.insts)
      case t@decls.NewTyDecl(_,_,_,_,_) =>
        val (tc,ids) = convert(mod.name, t)
        new ModIface(m.name, m.deps, m.exports, m.fixities, 
            m.decls ++ (tc::ids), m.insts)
      case t@decls.TySynDecl(_,_,_) =>
        val tysyn = convert(mod.name, t)
        new ModIface(m.name, m.deps, m.exports, m.fixities, 
            m.decls :+ tysyn, m.insts)
      case c@decls.ClassDecl(_,_,_) =>
        val (cls,ids) = convert(mod.name, c)
        new ModIface(m.name, m.deps, m.exports, m.fixities, 
            m.decls ++ (cls::ids), m.insts)
      case i@decls.InstDecl(_,_,_) =>
        val (inst,dfun) = convert(mod.name, i)
        new ModIface(m.name, m.deps, m.exports, m.fixities, 
            m.decls :+ dfun, m.insts :+ inst)
      case _ => m
    })
    new ModIface(
        m0.name,
        m0.deps,
        m0.exports.sortBy(_.name.toString),
        m0.fixities.sortBy(Function.tupled((f,_) => f.toString)),
        m0.decls.distinct.sortBy(_.name.toString),
        m0.insts.sortBy(_.name.toString))
  }
  
  def parse(in:String):Either[String,ModIface] = {
    val r = BluejellyParser.phrase(BluejellyParser.program, in)
    r match {
      case err@BluejellyParser.NoSuccess(_,_) => Left(err.toString)
      case BluejellyParser.Success(m,_) => Right(convert(m))
    }
  }
  
  def parse(in:Reader):Either[String,ModIface] = {
    val r = BluejellyParser.phrase(BluejellyParser.program, in)
    r match {
      case err@BluejellyParser.NoSuccess(_,_) => Left(err.toString)
      case BluejellyParser.Success(m,_) => Right(convert(m))
    }
  }  
}
