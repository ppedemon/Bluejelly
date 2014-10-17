/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.Con
import bluejelly.bjc.common.Name
import bluejelly.bjc.iface._

import java.io.IOException

case class LoaderException(msg:String) extends Exception(msg)

/**
 * Abstract class for objects knowing how to transform a module name into
 * a ModIface. The idea is to allow for loader mocks building ModIfaces 
 * from  a .hi file, hence making possible to test the module system.
 *
 * @author ppedemon 
 */
abstract class IfaceLoader {
  @throws[LoaderException]
  def load(modName:Name):ModIface
}

/**
 * `Producton-ready' IfaceLoader: this is the interface loader that
 * will be used in production code: it solves modules using the class
 * path, and delegates on ModIfaceIO for the extracting the interface
 * from a class bytecode.
 *
 * @author ppedemon 
 */
object ProdLoader extends IfaceLoader {
  def load(modName:Name) = try {
    val iface = ModIfaceIO.load(modName.toString)
    if (iface.name != modName) 
      throw LoaderException(s"interface is for module named `$iface.name')") 
    iface
  } catch {
    case e:LoaderException => 
      throw e
    case e:IOException => throw new LoaderException(
      s"interface not found")
    case _:Exception => throw new LoaderException(
      s"class doesn't look like a Bluejelly interface)")
  }
}

/**
 * ModuleLoader: recursively load a module and its dependencies.
 * Module declarations are stored in a given [[BjcEnv]] instance.
 *
 * @author ppedemon
 */
class ModuleLoader(val loader:IfaceLoader = ProdLoader) {

  def load(env:BjcEnv, modName:Name):BjcEnv = 
    if (env.hasModDefn(modName)) env else {
      val iface = loader.load(modName)
      val modDefn = translate(iface)
      iface.deps.foldLeft(env.addModDefn(modDefn))(load)
    }

  private def translate(iface:ModIface) = {
    // Sanity check: exports must be qualified
    if (!iface.exports.forall(_ match {
      case ExportedId(n) => n.isQual
      case ExportedTc(n,ns) => n.isQual && ns.forall(_.isQual) 
    })) throw new LoaderException(s"export list is invalid")
    
    // Create ids with details = VanillaId, adjust later
    val ids = iface.decls.foldLeft(Map.empty[Name,Id])((m,d) => d match {
      case IfaceId(n,ty,_) => m + (n -> Id(n, VanillaId, translateTy(ty)))
      case _ => m
    })

    // TyDecls (type synonyms, Type constructors, classes)
    // As a side effect, add data constructors as well
    val modIds = ids.values.toList
    val m0 = new ModDefn(iface.name, iface.exports, iface.fixities, modIds)
    val m1 = iface.decls.foldLeft(m0)((modDefn,d) => d match {
      case IfaceId(_,_,_) => 
        modDefn
      case IfaceTySyn(name,tvs,ty) => 
        modDefn.addTySyn(TySyn(name, tvs map tyVar, translateTy(ty)))
      case IfaceTyCon(name,ctx,tvs,dcons) =>
        val ds = dcons map (d => translateDCon(d,ids))
        val tycon = TyCon(name, ctx map tyPred, tvs map tyVar, ds)
        for (d <- ds) {
          for (id <- d.fields) id.details = RecSelId(tycon) 
          d.tycon = tycon
        }
        modDefn.addTyCon(tycon)
      case IfaceCls(name,tvs,ctx,ops) =>
        assert (tvs.size == 1)
        val clsOps = ops map translateClsOp
        val cls = Cls(name, tyVar(tvs.head), ctx map tyPred, clsOps)
        for (op <- clsOps) {
          op.cls = cls
          op.id.details = ClsOpId(cls)
        }
        modDefn.addClass(cls)
    })

    // Instances
    iface.insts.foldLeft(m1)(_ addInst _)
  }

  // Translate a dcons, the tycon field must be adjusted later
  private def translateDCon(dcon:IfaceDataCon, ids:Map[Name,Id]) = {
    val fields = dcon.fields map (n => ids(n))
    new DataCon(dcon.name, translateTy(dcon.ty), dcon.stricts, fields)
  }

  // Translate a class op, the class field must be adjusted later
  private def translateClsOp(clsOp:IfaceClsOp) = {
    val id = Id(clsOp.name, VanillaId, translateTy(clsOp.ty))
    new ClsOp(id, null, clsOp.isDefault)
  }

  // Sanity check: names tycons must be fully qualified
  private def translateTy(ty:IfaceType):Type = ty match {
    case IfacePolyTy(tvs, ty) => PolyTy(tvs map tyVar, translateTy(ty))
    case IfaceQualTy(ctx, ty) => QualTy(ctx map tyPred, translateTy(ty))
    case IfaceAppTy(fun, arg) => AppTy(translateTy(fun), translateTy(arg))
    case IfaceTcTy(gcon) => gcon match {
      case Con(n) if !n.isQual => throw new LoaderException(s"found non-qualified tycon: $n")
      case _ => TcTy(gcon) 
    } 
    case IfaceTvTy(tv) => TvTy(tv) 
  }

  private def translateKind(k:IfaceKind):Kind = k match {
    case IfaceKStar => KStar
    case IfaceKFun(x,y) => KFun(translateKind(x), translateKind(y))
  }

  private def tyVar(tv:IfaceTyVar) = 
    new TyVar(tv.name, translateKind(tv.kind))

  private def tyPred(pred:IfacePred) = {
    if (!pred.n.isQual) throw new LoaderException(s"found non-qualified predicate: %pred.n")
    new TyPred(pred.n, pred.tys map translateTy)
  }
}
