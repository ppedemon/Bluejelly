package bluejelly.bjc

import scalaz._
import scalaz.Kleisli._
import syntax.monad._
import syntax.traverse._
import std.list._

import ast.SourceSpan

// -----------------------------------------------------------------------
// Scope for a skolem variable
// -----------------------------------------------------------------------
case class SkolemScope(val skolemScope:Int) {
  def runSkolemScope = skolemScope
}

// -----------------------------------------------------------------------
// Type constraints
// -----------------------------------------------------------------------
case class Constraint(val className:Qualified[ProperName[ClassName.type]], val args:Seq[Type]) {
  def onArgs(f:Type => Type) = Constraint(className, args.map(f))
  def onArgsM[M[_]:Monad](f:Type => M[Type]) = 
    args.toList.traverse(f) >>= {tys => implicitly[Monad[M]].point(Constraint(className, tys.toSeq))}
}

// -----------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------
trait Type

case object TypeWildcard extends Type
case class TUnknown(val n:Int) extends Type
case class TypeVar(val name:String) extends Type
case class TypeConstructor(val name:Qualified[ProperName[TypeName.type]]) extends Type
case class TypeApp(val ty0:Type, val ty1:Type) extends Type
case class ForAll(val v:String, val ty:Type, val scope:Option[SkolemScope]) extends Type
case class ConstrainedType(val constraints:Seq[Constraint], val ty:Type) extends Type
case class Skolem(val name:String, val n:Int, val scope:SkolemScope, val posn:Option[SourceSpan]) extends Type
case class KindedType(val ty:Type, val kind:Kind) extends Type

// Auxiliary, only for pretty printing purposes
case class PrettyPrintFunction(val ty0:Type, ty1:Type) extends Type
case class PrettyPrintForAll(val vs:Seq[String], val ty:Type) extends Type

object Types {
  def isMonoType(ty:Type) = ty match {
    case ForAll(_,_,_) => false
    case _ => true
  }

  def mkForAll(vs:Seq[String], ty:Type) = vs.foldLeft(ty)((ty,v) => ForAll(v, ty, None))

  def replaceTypeVars(v:String, r:Type, ty:Type) = replaceAllTypeVars(Seq((v,r)), ty)

  def replaceAllTypeVars(m:Seq[(String,Type)], ty:Type) = {
    def newName:(String, Set[String], Int) => String = (origName, inUse, attempt) => {
      val n = s"${origName}${attempt}"
      if (inUse(n)) newName(origName, inUse, attempt + 1) else n
    }
    def go:(Set[String],Map[String,Type]) => Type => Type = (bound,m) => ty => ty match {
      case t@TypeVar(v) => m.getOrElse(v, t)
      case TypeApp(ty0, ty1) => TypeApp(go(bound, m)(ty0), go(bound, m)(ty1))
      case t@ForAll(v, ty, scope) if m.contains(v) => go(bound, m - v)(t)
      case ForAll(v, ty, scope) =>
        val used = m.values.flatMap(usedTypeVariables(_)).toSet
        if (used(v)) {
          val v1 = newName(v, used ++ bound ++ m.keySet, 0)
          val ty1 = go(bound, Map(v -> TypeVar(v1)))(ty)
          ForAll(v1, go(bound + v1, m)(ty1), scope)
        } else ForAll(v, go(bound + v, m)(ty), scope)
      case ConstrainedType(constraints, ty) => ConstrainedType(constraints map (_.onArgs(go(bound,m))), go(bound, m)(ty))
      case KindedType(ty, kind) => KindedType(go(bound, m)(ty), kind)
      case ty => ty
    }
    go(Set(),m.toMap)(ty)
  }

  def usedTypeVariables(ty:Type):Seq[String] = everythingOnTypes((_:Seq[String]) ++ (_:Seq[String])){
    case TypeVar(v) => Seq(v)
    case _ => Seq()
  }(ty).distinct

  def freeTypeVariables(ty:Type):Seq[String] = {
    def go:(Set[String], Type) => Seq[String] = (bound, ty) => ty match {
      case TypeVar(v) if !bound(v) => Seq(v)
      case TypeApp(ty0, ty1) => go(bound, ty0) ++ go(bound, ty1)
      case ForAll(v, ty, _) => go(bound + v, ty)
      case ConstrainedType(constraints, ty) => constraints.flatMap(_.args flatMap go.curried(bound)) ++ go(bound, ty)
      case KindedType(ty, kind) => go(bound, ty)
      case _ => Seq()
    }
    go(Set(), ty).distinct
  }

  def quantify(ty:Type):Type = freeTypeVariables(ty).foldRight(ty)(ForAll(_, _, None))

  def moveQuantifiersToFront(ty:Type):Type = {
    def go:(Seq[(String,Option[SkolemScope])], Seq[Constraint]) => Type => Type = (qs,cs) => ty => ty match {
      case ForAll(v, ty, scope) => go((v,scope) +: qs,cs)(ty)
      case ConstrainedType(constraints, ty) => go(qs, cs ++ constraints)(ty)
      case ty =>
        val cty = if (cs.isEmpty) ty else ConstrainedType(cs, ty)
        qs.foldLeft(cty){case (ty,(v,scope)) => ForAll(v, ty, scope)}
    }
    go (Seq(),Seq())(ty)
  }

  def constainsWilcards(ty:Type):Boolean = everythingOnTypes((_:Boolean) || (_:Boolean)){
    case TypeWildcard => true
    case _ => false
  }(ty)

  // ---------------------------------------------------------------------
  // Traversals
  // ---------------------------------------------------------------------
  def everywhereOnType(f:Type => Type)(ty:Type):Type = {
    def go:Type => Type = {
      case TypeApp(ty0, ty1) => f(TypeApp(go(ty0), go(ty1)))
      case ForAll(v, ty, scope) => f(ForAll(v, go(ty), scope))
      case ConstrainedType(constraints, ty) => f(ConstrainedType(constraints.map(_.onArgs(go)), go(ty)))
      case KindedType(ty, kind) => f(KindedType(go(ty), kind))
      case PrettyPrintFunction(ty0, ty1) => f(PrettyPrintFunction(go(ty0), go(ty1)))
      case PrettyPrintForAll(vs, ty) => f(PrettyPrintForAll(vs, go(ty)))
      case ty => f(ty)
    }
    go(ty)
  }

  def everywhereOnTypesTopDown(f:Type => Type)(ty:Type):Type = {
    def go:Type => Type = {
      case TypeApp(ty0, ty1) => TypeApp(go(f(ty0)),go(f(ty1)))
      case ForAll(v, ty, scope) => ForAll(v, go(f(ty)), scope)
      case ConstrainedType(constraints, ty) => ConstrainedType(constraints.map(_.onArgs(go compose f)), go(f(ty)))
      case KindedType(ty, kind) => KindedType(go(f(ty)), kind)
      case PrettyPrintFunction(ty0, ty1) => PrettyPrintFunction(go(f(ty0)), go(f(ty1)))
      case PrettyPrintForAll(vs, ty) => PrettyPrintForAll(vs, go(f(ty)))
      case ty => f(ty)
    }
    go(f(ty))
  }

  def everywhereOnTypesM[M[_]:Monad](f:Type => M[Type])(ty:Type):M[Type] = {
    def go:Type => M[Type] = {
      case TypeApp(ty0, ty1) => (go(ty0) |@| go(ty1))(TypeApp.apply _) >>= f
      case ForAll(v, ty, scope) => (go(ty) |@| implicitly[Monad[M]].pure(scope))(ForAll.apply(v, _, _)) >>= f
      case ConstrainedType(constraints, ty) => (constraints.toList.traverse(_.onArgsM(go)) |@| go(ty))(ConstrainedType.apply _) >>= f
      case KindedType(ty, kind) => (go(ty) |@| implicitly[Monad[M]].pure(kind))(KindedType.apply _) >>= f
      case PrettyPrintFunction(ty0, ty1) => (go(ty0) |@| go(ty1))(PrettyPrintFunction.apply _) >>= f
      case PrettyPrintForAll(vs, ty) => (implicitly[Monad[M]].pure(vs) |@| go(ty))(PrettyPrintForAll.apply _) >>= f
      case ty => f(ty)
    }
    go(ty)
  }

  def everywhereOnTypesTopDownM[M[_]:Monad](f:Type => M[Type])(ty:Type):M[Type] = {
    def go:Type => M[Type] = {
      case TypeApp(ty0, ty1) => ((f(ty0) >>= go) |@| (f(ty1) >>= go))(TypeApp.apply _)
      case ForAll(v, ty, scope) => ((f(ty) >>= go) |@| implicitly[Monad[M]].pure(scope))(ForAll.apply(v, _, _))
      case ConstrainedType(constraints, ty) => (constraints.toList.traverse(_.onArgsM(kleisli(go) <==< f)) |@| (f(ty) >>= go))(ConstrainedType.apply _)
      case KindedType(ty, kind) => ((f(ty) >>= go) |@| implicitly[Monad[M]].pure(kind))(KindedType.apply _)
      case PrettyPrintFunction(ty0, ty1) => ((f(ty0) >>= go) |@| (f(ty1) >>= go))(PrettyPrintFunction.apply _)
      case PrettyPrintForAll(vs, ty) => (implicitly[Monad[M]].pure(vs) |@| (f(ty) >>= go))(PrettyPrintForAll.apply _)
      case ty => f(ty)
    }
    (kleisli(go) <==< f).apply(ty)
  }

  def everythingOnTypes[A](f:(A,A) => A)(g:Type => A)(ty:Type):A = {
    def go:Type => A = {
      case t@TypeApp(ty0, ty1) => f(f(g(t), go(ty0)), go(ty1))
      case t@ForAll(_, ty, _) => f(g(t), go(ty))
      case t@ConstrainedType(constraints, ty) => f((constraints flatMap (_.args) map go).foldLeft(g(t))(f), go(ty))
      case t@KindedType(ty, _) => f(g(t), go(ty))
      case t@PrettyPrintFunction(ty0, ty1) => f(f(g(t), go(ty0)), go(ty1))
      case t@PrettyPrintForAll(_, ty) => f(g(t), go(ty))
      case ty => g(ty)
    }
    go(ty)
  }

  // Da fuck!?
  def everythingWithContextOnTypes[S,R](s0:S, r0:R, f:(R,R) => R, g:(S,Type) => (S,R))(ty:Type):R = {
    def go1:(S,Type) => R = { case (s,ty) =>
      val (s1,r) = g(s, ty)
      f(r, go(s1, ty))
    }
    def go:(S,Type) => R = (s,ty) => ty match {
      case TypeApp(ty0, ty1) => f(go1(s, ty0), go1(s, ty1))
      case ForAll(_, ty, _) => go1(s, ty)
      case ConstrainedType(constraints, ty) => f((constraints flatMap (_.args) map go1.curried(s)).foldLeft(r0)(f), go1(s, ty))
      case KindedType(ty, _) => go1(s, ty)
      case PrettyPrintFunction(ty0, ty1) => f(go1(s, ty0), go1(s, ty1))
      case PrettyPrintForAll(_, ty) => go1(s, ty)
      case ty => r0
    }
    go1(s0, ty)
  }
}
