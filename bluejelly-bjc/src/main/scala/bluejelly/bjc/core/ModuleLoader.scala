/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.Con
import bluejelly.bjc.common.Fixity
import bluejelly.bjc.common.{Name,ScopedName}
import bluejelly.bjc.common.ScopedName._
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
  def load(modName:Symbol):ModIface
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
  def load(modName:Symbol) = try {
    val iface = ModIfaceIO.load(modName.name)
    if (iface.name != modName) 
      throw LoaderException(s"interface is for module named `${iface.name.name}'") 
    iface
  } catch {
    case e:LoaderException => 
      throw e
    case e:IOException => 
      throw new LoaderException(s"interface not found: `${modName.name}'")
    case _:Exception => throw new LoaderException(
      s"class for module `${modName.name}' doesn't look like a Bluejelly interface)")
  }
}

/**
 * ModuleLoader: recursively load a module and its dependencies.
 * Module declarations are stored in a given [[BjcEnv]] instance.
 *
 * @author ppedemon
 */
class ModuleLoader(val loader:IfaceLoader = ProdLoader) {

  def load(env:BjcEnv, modName:Symbol):BjcEnv = 
    if (env.hasModDefn(modName)) env else {
      val iface = loader.load(modName)
      val modDefn = translate(iface)
      iface.deps.foldLeft(env addModDefn modDefn)(load)
    }

  private def translate(iface:ModIface) = {    
    // Create fixity table
    val fixities = iface.fixities.foldLeft(Map.empty[ScopedName,Fixity])(
      (map,p) => map + (idName(p._1) -> p._2)
    ) 

    // Create ids with details = VanillaId, adjust later
    val ids = iface.decls.foldLeft(Map.empty[Symbol,Id])((m,d) => d match {
      case IfaceId(n,ty,_) => m + (n -> Id(idName(n), VanillaId, translateTy(ty)))
      case _ => m
    })

    // TyDecls (type synonyms, Type constructors, classes)
    // As a side effect, add data constructors as well
    val m0 = new ModDefn(iface.name, iface.exports, fixities)
    val m1 = iface.decls.foldLeft(m0)((modDefn,d) => d match {
      case IfaceId(n,_,_) => 
        modDefn.addId(ids(n))
      case IfaceTySyn(name,tvs,ty) => 
        modDefn.addTySyn(TySyn(tcName(name), tvs map tyVar, translateTy(ty)))
      case IfaceTyCon(name,ctx,tvs,dcons) =>
        val ds = (dcons.foldRight((List.empty[DataCon],dcons.size-1)){
          case (d,(ds,tag)) => (translateDCon(d,tag,ids)::ds,tag-1)
        })._1
        val tycon = TyCon(tcName(name), ctx map tyPred, tvs map tyVar, ds)
        for (d <- ds) {
          for (id <- d.fields) id.details = RecSelId(tycon) 
          d.tycon = tycon
        }
        modDefn.addTyCon(tycon)
      case IfaceCls(name,tvs,ctx,ops) =>
        assert (tvs.size == 1)
        val clsOps = ops map translateClsOp
        val cls = Cls(tcName(name), tyVar(tvs.head), ctx map tyPred, clsOps)
        for (op <- clsOps) {
          op.cls = cls
          op.id.details = ClsOpId(cls)
        }
        modDefn.addClass(cls)
    })

    // Instances
    iface.insts.foldLeft(m1)((modDefn,i) => { 
      val id = ids(i.dfunId)
      val inst = new Inst(i.name, i.con, id)
      id.details = DFunId(inst)
      modDefn.addInst(inst)
    })
  }

  // Translate a dcons, the tycon field must be adjusted later
  private def translateDCon(dcon:IfaceDataCon, tag:Int, ids:Map[Symbol,Id]) = {
    val fields = dcon.fields map (n => ids(n))
    new DataCon(idName(dcon.name), translateTy(dcon.ty), dcon.stricts, fields, tag)
  }

  // Translate a class op, the class field must be adjusted later
  private def translateClsOp(clsOp:IfaceClsOp) = {
    val id = Id(idName(clsOp.name), VanillaId, translateTy(clsOp.ty))
    new ClsOp(id, null, clsOp.isDefault)
  }

  // Sanity check: names tycons must be fully qualified
  private def translateTy(ty:IfaceType):Type = ty match {
    case IfacePolyTy(tvs, ty) => PolyTy(tvs map tyVar, translateTy(ty))
    case IfaceQualTy(ctx, ty) => QualTy(ctx map tyPred, translateTy(ty))
    case IfaceAppTy(fun, arg) => AppTy(translateTy(fun), translateTy(arg))
    case IfaceTcTy(tcref) => TcTy(tcref)
    case IfaceTvTy(tv) => TvTy(tvName(tv)) 
  }

  private def translateKind(k:IfaceKind):Kind = k match {
    case IfaceKStar => KStar
    case IfaceKFun(x,y) => KFun(translateKind(x), translateKind(y))
  }

  private def tyVar(tv:IfaceTyVar) = 
    new TyVar(tvName(tv.name), translateKind(tv.kind))

  private def tyPred(pred:IfacePred) = 
    new TyPred(pred.n, pred.tys map translateTy)
}
