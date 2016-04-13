package bluejelly.bjc

import scalaz._
import syntax.monad._

import monad.MonadSupply

// -----------------------------------------------------------------------
// Identifiers
// -----------------------------------------------------------------------

trait Ident {
  def runIdent:String
  def showIdent = runIdent
}
case class Id(val name:String) extends Ident {
  def runIdent = name
}
case class Op(val name:String) extends Ident {
  def runIdent = name
  override def showIdent = s"($name)"
}
case class Gen(val name:Option[String], val value:Int) extends Ident {
  def runIdent = "$%s%d" format (name.getOrElse(""), value)
}

// -----------------------------------------------------------------------
// Proper names
// -----------------------------------------------------------------------

trait ProperNameType
case object TypeName extends ProperNameType
case object ConstructorName extends ProperNameType
case object ClassName extends ProperNameType
case object Namespace extends ProperNameType

case class ProperName[T <: ProperNameType](val properName:String) {
  def runProperName = properName
}

// -----------------------------------------------------------------------
// Module names
// -----------------------------------------------------------------------

case class ModuleName(val path:Seq[String], val name:ProperName[Namespace.type]) {
  def runModuleName = (path :+ name.runProperName).mkString(".")
}

// -----------------------------------------------------------------------
// Qualified names
// -----------------------------------------------------------------------

case class Qualified[T](val qualifier:Option[ModuleName], val thing:T) {
  def showQualified(f:T => String) = qualifier match {
    case Some(m) => "%s.%s" format (m.runModuleName, f(thing))
    case None => f(thing)
  }

  def qualify(m:ModuleName) = (qualifier.getOrElse(m), thing)
  def disqualify = thing
  def isUnqualified = qualifier.isEmpty
  def isQualified = !isUnqualified
  def isQualifiedWith(m:ModuleName) = qualifier.fold(false)(_ == m)
}

// -----------------------------------------------------------------------
// Object exposing the whole module interface
// -----------------------------------------------------------------------

object Names {
  def freshIdent[F[_]](s:String)(implicit F:MonadSupply[F]):F[Ident] = 
    ((Gen(Some(s), _:Int)):LiftV[F,Int,Ident]).lift.apply(F.fresh)

  def freshIdentAnon[F[_]](implicit F:MonadSupply[F]):F[Ident] =
    ((Gen(None, _:Int)):LiftV[F,Int,Ident]).lift.apply(F.fresh)    

  def coerceProperName[A <: ProperNameType, B <: ProperNameType](a:ProperName[A]):ProperName[B] = 
    ProperName[B](a.runProperName)

  def toModuleName(s:String) = {
    val toks = s.split("""\.""").reverse
    ModuleName(toks.tail.reverse.toSeq, ProperName[Namespace.type](toks.head))
  }
  
  def mkQualified[T](m:ModuleName, thing:T) = Qualified(Some(m),thing)
}

/*
 * TODO: Remove this!
 */
object test {
  import Id._
  import monad.SupplyT._
  import monad.MonadSupply._
  import Names._
  
  val names = (for {
    n0 <- freshIdent[Supply]("x")
    n1 <- freshIdentAnon[Supply]
  } yield List(n0,n1)).eval(0).asInstanceOf[List[Ident]]
}
