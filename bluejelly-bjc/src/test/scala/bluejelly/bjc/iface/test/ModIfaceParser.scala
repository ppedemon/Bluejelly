/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.iface.test

import bluejelly.bjc.common.Name
import bluejelly.bjc.ast.{GCon,Con,Decl}
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
  
  // Kind arbitrarily set to *
  private def mkTv(n:Name) = 
    new IfaceTyVar(n,IfaceKStar)

  // Transform a syntactic predicate to an interface predicate
  private def mkPred(p:types.Pred):IfacePred = 
    new IfacePred(p.head, p.tys map convert)
  
  // Transform a pred (C t1 ... tn) into an type constructor application
  private def predToIfaceType(p:types.Pred) = {
    val tc = IfaceTcTy(Con(p.head))
    p.tys.foldLeft[IfaceType](tc)((ty,arg) => IfaceAppTy(ty,convert(arg)))
  }
  
  // Generate a dictionary name from the given pred
  private def genDictName(p:types.Pred) = {
    val ctor = p.head
    val tcs = p.tys.map(types.Type.unwind(_)._1)
    Name(Symbol("$f" + ctor + tcs.mkString))
  }
      
  // Collect the type variables of a (syntactic) predicate
  private def predTvs(pred:types.Pred) = 
    pred.tys.foldRight[Set[IfaceTyVar]](Set.empty)((ty,vs) => 
      tyVarsIn(ty) ++ vs)
  
  private def tyVarsIn(ty:types.Type):Set[IfaceTyVar] = ty match {
    case types.QualType(_,ty) => tyVarsIn(ty)
    case types.AppType(f,x) => tyVarsIn(f) ++ tyVarsIn(x)
    case types.TyCon(_) => Set.empty
    case types.TyVar(n) => Set(mkTv(n))
  }  
    
  // Assuming t is a constructor application, get an IfaceRecSelId
  private def getRecSelId(t:IfaceType) = {
    val (f,_) = IfaceType.unwind(t)
    f match {
      case IfaceTcTy(Con(n)) => IfaceRecSelId(n)
      case _ => IfaceVanillaId // Shouldn't be reached
    }
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
      defs:List[Name], 
      tvs:List[IfaceTyVar], 
      pred:IfacePred, 
      t:decls.TySigDecl):List[IfaceId] = {
    t.vars.foldRight[List[IfaceId]](Nil)((n,ids) => defs.contains(n) match {
      case true =>
        val idName = Name(Symbol("$dm" + n))
        val idTy = mkTy(tvs, List(pred), convert(t.ty))
        IfaceId(idName, idTy, IfaceVanillaId) :: ids
      case false => ids
    })
  }
  
  // Create an interface class operation
  private def mkClsOps(
      defs:List[Name],
      tvs:List[IfaceTyVar],
      ctx:IfacePred,
      ds:List[Decl]):(List[IfaceClsOp],List[IfaceId]) = { 
    val ops = ds.foldRight[List[IfaceClsOp]](Nil)((d,ds) => d match {
      case decls.TySigDecl(ns,ty) =>
        val opTy = mkTy(Nil, Nil, convert(ty))
        ns.map(n => new IfaceClsOp(n, opTy, defs.contains(n))) ++ ds
      case _ => ds
    })
    val ids = ds.foldRight[List[IfaceId]](Nil)((d,ids) => d match {
      case t@decls.TySigDecl(_,_) => mkDefIds(defs, tvs, ctx, t) ++ ids
      case _ => ids
    })
    (ops,ids)
  }
  
  private def mkClsInstArgs(pred:types.Pred):List[Option[GCon]] = {
    val tys = pred.tys map (types.Type.unwind(_)._1)
    tys map {
      case types.TyCon(gcon) => Some(gcon)
      case _ => None
    }
  }
  
  // Convert module imports
  private def convert(is:List[ImpDecl]) = is map {_.modId}
  
  // Convert module exports
  private def convert(es:Exports):List[IfaceExport] = es match {
    case ExportSome(es) => es.foldRight[List[IfaceExport]](Nil)((e,es) => 
      e match {
        case EVar(n) => ExportedId(n)::es
        case ESome(n,ns) => ExportedTc(n,ns)::es
        case _ => es
      }) 
    case _ => Nil
  }
  
  // Convert fixity declarations
  private def convert(f:decls.FixityDecl):List[(Name,Fixity)] = 
    f.ops map {(_,new Fixity(f.assoc,f.prec))}

  // Convert type signatures
  private def convert(ty:types.Type):IfaceType = ty match {
    case types.QualType(ctx,ty) => IfaceQualTy(ctx map mkPred, convert(ty))
    case types.AppType(f,x) => IfaceAppTy(convert(f),convert(x))
    case types.TyVar(n) => IfaceTvTy(n)
    case types.TyCon(tc) => IfaceTcTy(tc)
  }

  // Convert data constructors
  private def convert(
      tvs:List[IfaceTyVar],
      ctx:List[IfacePred],     // Cumulative contexts found in the data cons
      ty:IfaceType, 
      dcon:dcons.DCon):(IfaceDataCon,List[IfaceId]) = dcon match {
    case dcons.AlgDCon(n,args) => 
      val t = IfaceType.mkFun((args map {arg => convert(arg.ty)}) :+ ty)
      (new IfaceDataCon(n, mkTy(tvs, ctx, t), Nil, args map {_.strict}), Nil)
    case r@dcons.RecDCon(n,lgrps) =>
      val lts = r.flatten
      val labels = lts map (_._1)
      val strict = lts map (_._3)
      val tys = lts map (Function.tupled((_,t,_) => convert(t)))
      val ids = (labels,tys).zipped.map((l,t) => 
        IfaceId(l,mkSelTy(tvs, ctx, ty, t), getRecSelId(ty)))
      val t = IfaceType.mkFun(tys :+ ty)
      (new IfaceDataCon(n, mkTy(tvs, ctx, t), labels, strict),ids)
  }

  // Convert a type constructor declaration
  private def convert(tc:decls.DataDecl):(IfaceTyCon,List[IfaceId]) = {
    val tvs = tc.vars map mkTv
    val ctx = tc.ctx.map(_ map mkPred).getOrElse(Nil)
    val tcTy = IfaceType.mkApp(IfaceTcTy(Con(tc.n)), 
        tvs map {tv => IfaceTvTy(tv.name)})
    val (dcons,ids) = tc.rhs.foldRight
      [(List[IfaceDataCon],List[IfaceId])]((Nil,Nil))((dcon,p) => {
        val (d,is) = convert(tvs, ctx, tcTy, dcon)
        (d::p._1, is ++ p._2)
      })
    (IfaceTyCon(tc.n, ctx, tvs, dcons),ids)
  }
  
  // Convert a newtype declaration
  private def convert(nt:decls.NewTyDecl):(IfaceTyCon,List[IfaceId]) = {
    val tvs = nt.vars map mkTv
    val ctx = nt.ctx.map(_ map mkPred).getOrElse(Nil)
    val tcTy = IfaceType.mkApp(IfaceTcTy(Con(nt.n)),
        tvs map {tv => IfaceTvTy(tv.name)})
    val (dcon,ids) = convert(tvs, ctx, tcTy, nt.rhs)
    (IfaceTyCon(nt.n, ctx, tvs, List(dcon)),ids)
  }
  
  // Convert a type synonym declaration
  private def convert(tysyn:decls.TySynDecl):IfaceTySyn =
    IfaceTySyn(tysyn.n, tysyn.vars map mkTv, convert(tysyn.rhs))
  
  // Convert type signature declarations
  private def convert(tysig:decls.TySigDecl):List[IfaceId] = {
    val tvs = tyVarsIn(tysig.ty).toList
    tysig.vars map {v => IfaceId(v, mkTy(tvs, Nil, convert(tysig.ty)), IfaceVanillaId)}
  }
  
  // Convert class declarations
  private def convert(cls:decls.ClassDecl):(IfaceCls,List[IfaceId]) = {
    val tvs = predTvs(cls.pred).toList
    val ctx = cls.ctx map (_ map mkPred) getOrElse Nil
    val pred = mkPred(cls.pred)
    val defs = cls.ds.foldRight[List[Name]](Nil)((d,ds) => d match {
      case f@decls.FunBind(n,_,_,_,_) => n::ds 
      case _ => ds
    })
    val (ops,ids) = mkClsOps(defs, tvs, pred, cls.ds)
    (IfaceCls(cls.pred.head, tvs, ctx, pred, ops),ids)
  }
  
  // Convert instance declarations
  private def convert(inst:decls.InstDecl):(IfaceClsInst,IfaceId) = {
    val tvs = predTvs(inst.pred).toList
    val ctx = inst.ctx map (_ map mkPred) getOrElse Nil
    val ty = predToIfaceType(inst.pred)
    val dfun = IfaceId(genDictName(inst.pred), mkTy(tvs, ctx, ty), IfaceDFunId)
    val clsInst = new IfaceClsInst(inst.pred.head, mkClsInstArgs(inst.pred), dfun.name)
    (clsInst,dfun)
  }
  
  // Convert an entire module
  private def convert(mod:Module):ModIface = {
    val is = convert(mod.impDecls)
    val es = convert(mod.exports)
    val m = new ModIface(mod.name, is, es, Nil, Nil, Nil)
    val m0 = mod.topDecls.foldLeft(m)((m,d) => d match {
      case f@decls.FixityDecl(_,_,_) =>
        val fs = convert(f)
        new ModIface(m.name, m.deps, m.exports, m.fixities ++ fs, 
            m.decls, m.insts)
      case f@decls.TySigDecl(_,_) =>
        val ids = convert(f)
        new ModIface(m.name, m.deps, m.exports, m.fixities, 
            m.decls ++ ids, m.insts)
      case t@decls.DataDecl(_,_,_,_,_) =>
        val (tc,ids) = convert(t)
        new ModIface(m.name, m.deps, m.exports, m.fixities, 
            m.decls ++ (tc::ids), m.insts)
      case t@decls.NewTyDecl(_,_,_,_,_) =>
        val (tc,ids) = convert(t)
        new ModIface(m.name, m.deps, m.exports, m.fixities, 
            m.decls ++ (tc::ids), m.insts)
      case t@decls.TySynDecl(_,_,_) =>
        val tysyn = convert(t)
        new ModIface(m.name, m.deps, m.exports, m.fixities, 
            m.decls :+ tysyn, m.insts)
      case c@decls.ClassDecl(_,_,_) =>
        val (cls,ids) = convert(c)
        new ModIface(m.name, m.deps, m.exports, m.fixities, 
            m.decls ++ (cls::ids), m.insts)
      case i@decls.InstDecl(_,_,_) =>
        val (inst,dfun) = convert(i)
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
