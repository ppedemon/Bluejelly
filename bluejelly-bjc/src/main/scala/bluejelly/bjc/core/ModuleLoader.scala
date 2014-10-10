/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

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
      throw LoaderException(s"Invalid module: `$modName' (it declares `$iface.name')") 
    iface
  } catch {
    case e:LoaderException => 
      throw e
    case e:IOException => throw new LoaderException(
      s"Module not found: `$modName'")
    case _:Exception => throw new LoaderException(
      s"Error loading: `$modName' (class doesn't look like a Bluejelly module)")
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
    // Fixities
    val fixities = iface.fixities.foldLeft(Map.empty[Name,Fixity])((m,nf) => 
      m + (nf._1 -> nf._2))
    
    // Create ids with details = VanillaId, adjust later
    val ids = iface.decls.foldLeft(Map.empty[Name,Id])((m,d) => d match {
      case IfaceId(n,ty,_) => m + (n -> Id(n, VanillaId, translateTy(ty)))
      case _ => m
    })

    // TyDecls (type synonyms, Type constructors, classes)
    // As a side effect, add data constructors as well
    val m0 = new ModDefn(iface.name, iface.exports, fixities, ids)
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
      case IfaceCls(name,tvs,ctx,pred,ops) =>
        assert (tvs.size == 1)
        val clsOps = ops map translateClsOp
        val cls = Cls(name, tyVar(tvs.head), ctx map tyPred, tyPred(pred), clsOps)
        for (op <- clsOps) {
          op.cls = cls
          op.id.details = ClsOpId(cls)
        }
        modDefn.addClass(cls)
    })

    // Instances
    iface.insts.foldLeft(m1)((modDefn,inst) => {
      assert (inst.tys.size == 1)
      val cls = modDefn.getCls(inst.name)
      val gcon = inst.tys.head.get
      val i = new Inst(cls, gcon, ids(inst.dfunId))
      ids(inst.dfunId).details = DFunId(i)
      modDefn.addInst(i)
    })
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

  private def translateTy(ty:IfaceType):Type = ty match {
    case IfacePolyTy(tvs, ty) => PolyTy(tvs map tyVar, translateTy(ty))
    case IfaceQualTy(ctx, ty) => QualTy(ctx map tyPred, translateTy(ty))
    case IfaceAppTy(fun, arg) => AppTy(translateTy(fun), translateTy(arg))
    case IfaceTcTy(gcon) => TcTy(gcon)
    case IfaceTvTy(tv) => TvTy(tv) 
  }

  private def translateKind(k:IfaceKind):Kind = k match {
    case IfaceKStar => KStar
    case IfaceKFun(x,y) => KFun(translateKind(x), translateKind(y))
  }

  private def tyVar(tv:IfaceTyVar) = 
    new TyVar(tv.name, translateKind(tv.kind))

  private def tyPred(pred:IfacePred) = 
    new TyPred(pred.n, pred.tys map translateTy)
}
