package bluejelly.bjc.ast

import scalaz._, Id._, syntax.functor._

import bluejelly.bjc.{Environment,DataDeclType,NameKind}
import bluejelly.bjc.Kind
import bluejelly.bjc.{Constraint,Type}
import bluejelly.bjc.{Ident,ProperName,ClassName,ConstructorName,TypeName,ModuleName,Qualified}

// -----------------------------------------------------------------------
// References to declarations in import/export lists
// -----------------------------------------------------------------------
trait DeclarationRef
case class TypeRef(val name:ProperName[TypeName.type], val ctors:Option[Seq[ProperName[ConstructorName.type]]]) extends DeclarationRef
case class TypeOpRef(val ident:Ident) extends DeclarationRef
case class ValueRef(val ident:Ident) extends DeclarationRef
case class TypeClassRef(val name:ProperName[ClassName.type]) extends DeclarationRef
case class TypeInstanceRef(val ident:Ident)
case class ModuleRef(val moduleName:ModuleName) extends DeclarationRef
case class ProperRef(val name:String) extends DeclarationRef
case class PositionedDeclarationRef(val pos:SourceSpan, val decl:DeclarationRef) extends DeclarationRef

class DeclarationRefEqual extends Equal[DeclarationRef] {
  override def equalIsNatural = false

  override def equal(dr0:DeclarationRef, dr1:DeclarationRef):Boolean = (dr0,dr1) match {
    case (PositionedDeclarationRef(_, r0), r1) => equal(r0,r1)
    case (r0, PositionedDeclarationRef(_, r1)) => equal(r0,r1)
    case _ => dr0 == dr1 
  }
}

object DeclarationRef {
  implicit val declarationRefEqual = new DeclarationRefEqual

  def isModuleRef(d:DeclarationRef):Boolean = d match {
    case PositionedDeclarationRef(_,r) => isModuleRef(r)
    case ModuleRef(_) => true
    case _ => false
  }

  def findDuplicateRefs(refs:Seq[DeclarationRef]):(Seq[DeclarationRef], Seq[ProperName[ConstructorName.type]]) = {
    def stripPosInfo:DeclarationRef => DeclarationRef = {
      case PositionedDeclarationRef(_, r) => stripPosInfo(r)
      case r => r
    }
    def simplifyTypeRef:DeclarationRef => DeclarationRef = {
      case TypeRef(name, _) => TypeRef(name, None)
      case r => r
    }
    val positionLess = refs map stripPosInfo
    val simplified = positionLess map simplifyTypeRef
    val dupRefs = (simplified diff simplified.distinct).distinct
    val dupCtors = (positionLess flatMap {
      case TypeRef(_, Some(ctors)) => 
        val dups = ctors diff ctors.distinct
        if (dups.isEmpty) None else Some(dups)
      case _ => None
    }).flatten
    (dupRefs, dupCtors)
  }
}

// -----------------------------------------------------------------------
// Import types
// -----------------------------------------------------------------------
trait ImportDeclarationType
case object Implicit extends ImportDeclarationType
case class Explicit(val refs:Seq[DeclarationRef]) extends ImportDeclarationType
case class Hiding(val refs:Seq[DeclarationRef]) extends ImportDeclarationType

object ImportDeclarationType {
  def isImplicit(i:ImportDeclarationType):Boolean = i match {
    case Implicit => true
    case _ => false
  }

  def isExplicit(i:ImportDeclarationType):Boolean = i match {
    case Explicit(_) => true
    case _ => false
  }
}

// -----------------------------------------------------------------------
// Expressions
// -----------------------------------------------------------------------
trait Expr

case class Lit(val value:Literal[Expr]) extends Expr
case class UnaryMinus(val expr:Expr) extends Expr
case class BinaryNoParens(val left:Expr, val op:Expr, val right:Expr) extends Expr
case class Parens(val expr:Expr) extends Expr
case class OperatorSection(val op:Expr, val operand:Expr \/ Expr) extends Expr
case class Abs(val args:Ident \/ Binder, val body:Expr) extends Expr
case class App(val fun:Expr, val arg:Binder) extends Expr
case class Var(val ident:Qualified[Ident]) extends Expr
case class Cond(val cond:Expr, val thenExpr:Expr, val elseExpr:Expr) extends Expr
case class Constructor(val name:Qualified[ProperName[ConstructorName.type]]) extends Expr
case class Case(val expr:Expr, val alts:Seq[CaseAlternative]) extends Expr
case class TypedValue(val check:Boolean, val expr:Expr, val ty:Type) extends Expr
case class Let(val decls:Seq[Declaration], body:Expr) extends Expr
case class Do(val elems:Seq[DoNotationElem]) extends Expr
case class Hole(val msg:String) extends Expr
case class PositionedValue(val pos:SourceSpan, val expr:Expr) extends Expr

// Wrapper for an expression constructing a dictionary for a given class.
// Expr must be of the form: App(App(...App(Tuple(n),arg_1)...),arg_n)
case class TypeClassDictionaryConstructorApp(val className:Qualified[ProperName[ClassName.type]], val expr:Expr) extends Expr

// Type class dictionary accessor, to be solved during desugaring
case class TypeClassDictionaryAccessor(val name:Qualified[ProperName[ClassName.type]], val ident:Ident) extends Expr

// Placeholder to be solved into a type class dictionary. Holds a constraint describing 
// the intended class name and arguments, and all the dictionaries in scope.
case class TypeClassDictionary(val self:Constraint, val typeClassDictionaries:Environment.DictsInScope) extends Expr

// Placeholder for a super class dictionary to be solved into a type class dictionary. 
case class SuperClassDictionary(val name:Qualified[ProperName[ClassName.type]], val args:Seq[Type]) extends Expr

case class CaseAlternative(val binders:Seq[Binder], val body:Seq[(Expr,Expr)] \/ Expr)

trait DoNotationElem
case class DoNotationExpr(val expr:Expr) extends DoNotationElem
case class DoNotationBind(val binder:Binder, val expr:Expr) extends DoNotationElem
case class DoNotationLet(val decls:Seq[Declaration]) extends DoNotationElem
case class PositionedDoNotationElem(val pos:SourceSpan, val elem:DoNotationElem) extends DoNotationElem

// -----------------------------------------------------------------------
// Declarations
// -----------------------------------------------------------------------
trait Declaration

case class ImportDeclaration(
  val moduleName:ModuleName, 
  val importType:ImportDeclarationType, 
  val alias:Option[ModuleName]) extends Declaration

case class DataDeclaration(
  val dataDeclType:DataDeclType, 
  val name:ProperName[TypeName.type], 
  val vars:Seq[(String,Option[Kind])], 
  val dcons:Seq[(ProperName[ConstructorName.type], Seq[Type])]) extends Declaration

case class TypeSynonymDeclaration(
  val name:ProperName[TypeName.type], 
  val vars:Seq[(String, Option[Kind])], 
  val ty:Type) extends Declaration

case class ValueDeclaration(
  val ident:Ident, 
  val nameKind:NameKind, 
  val args:Seq[Binder], 
  val body:Seq[(Expr,Expr)] \/ Expr) extends Declaration

case class TypeClassDeclaration(
  val name:ProperName[ClassName.type], 
  val vars:Seq[(String, Option[Kind])], 
  val constratints:Seq[Constraint],
  val members:Seq[Declaration]) extends Declaration

case class TypeInstanceDeclaration(
  val ident:Ident, 
  val constraints:Seq[Constraint], 
  val className:Qualified[ProperName[ClassName.type]], 
  val args:Seq[Type], 
  val body:TypeInstanceBody) extends Declaration

case class FixityDeclaration(
  val fixity:Fixity, 
  val op:String, 
  val alias:Option[Qualified[FixityAlias]]) extends Declaration

case class TypeDeclaration(val ident:Ident, val ty:Type) extends Declaration

case class DataBindingGroupDeclaration(val dataDecls:Seq[Declaration]) extends Declaration
case class BindingGroupDeclaration(val decls:Seq[(Ident, NameKind, Expr)]) extends Declaration

case class PositionedDeclaration(val pos:SourceSpan, val decl:Declaration) extends Declaration

trait FixityAlias
case class AliasValue(val ident:Ident) extends FixityAlias
case class AliasConstructor(val name:ProperName[ConstructorName.type]) extends FixityAlias
case class AliasType(val name:ProperName[TypeName.type]) extends FixityAlias

trait TypeInstanceBody
case object DerivedInstance extends TypeInstanceBody
case class ExplicitInstance(val members:Seq[Declaration]) extends TypeInstanceBody

object Declaration {
  def foldFixityAlias[A](f:Ident => A)
      (g:ProperName[ConstructorName.type] => A)
      (h:ProperName[TypeName.type] => A)(alias:FixityAlias):A = alias match {
    case AliasValue(ident) => f(ident)
    case AliasConstructor(name) => g(name)
    case AliasType(name) => h(name)
  }

  def getValueAlias(alias:FixityAlias):Option[Ident \/ ProperName[ConstructorName.type]] = alias match {
    case AliasValue(ident) => Some(-\/(ident))
    case AliasConstructor(name) => Some(\/-(name))
    case _ => None
  }

  def getTypeAlias(alias:FixityAlias):Option[ProperName[TypeName.type]] = alias match {
    case AliasType(name) => Some(name)
    case _ => None
  }

  def traverseTypeInstanceBody[F[_]]
    (f:Seq[Declaration] => F[Seq[Declaration]])
    (t:TypeInstanceBody)
    (implicit F:Applicative[F]):F[TypeInstanceBody] = t match {
      case DerivedInstance => F.pure(t)
      case ExplicitInstance(members) => f(members) map (ExplicitInstance.apply _)
    }

  def mapTypeInstanceBody(f:Seq[Declaration] => Seq[Declaration])(t:TypeInstanceBody):TypeInstanceBody = 
    traverseTypeInstanceBody[Id](f)(t)

  def isValueDeclaration(d:Declaration):Boolean = d match {
    case ValueDeclaration(_,_,_,_) => true
    case PositionedDeclaration(_, d) => isValueDeclaration(d)
    case _ => false
  }

  def isDataDeclaration(d:Declaration):Boolean = d match {
    case DataDeclaration(_,_,_,_) => true
    case TypeSynonymDeclaration(_,_,_) => true
    case PositionedDeclaration(_, d) => isDataDeclaration(d)
    case _ => false
  }

  def isImportDeclaration(d:Declaration):Boolean = d match {
    case ImportDeclaration(_,_,_) => true
    case PositionedDeclaration(_, d) => isImportDeclaration(d)
    case _ => false
  }

  def isFixityDeclaration(d:Declaration):Boolean = d match {
    case FixityDeclaration(_,_,_) => true
    case PositionedDeclaration(_, d) => isFixityDeclaration(d)
    case _ => false
  }

  def isTypeInstanceDeclaration(d:Declaration):Boolean = d match {
    case TypeInstanceDeclaration(_,_,_,_,_) => true
    case PositionedDeclaration(_, d) => isTypeInstanceDeclaration(d)
    case _ => false
  }

  def isTypeClassDeclaration(d:Declaration):Boolean = d match {
    case TypeClassDeclaration(_,_,_,_) => true
    case PositionedDeclaration(_, d) => isTypeClassDeclaration(d)
    case _ => false
  }

  def flattenDataDecls(ds:Seq[Declaration]):Seq[Declaration] = {
    def flattenOne:Declaration => Seq[Declaration] = {
      case DataBindingGroupDeclaration(ds) => ds flatMap flattenOne
      case d => Seq(d)
    }
    ds flatMap flattenOne
  }
}

// -----------------------------------------------------------------------
// Finally, top level modules
// -----------------------------------------------------------------------
case class Module(
  val pos:SourceSpan, 
  val name:ModuleName, 
  val decls:Seq[Declaration], 
  val exports:Option[Seq[DeclarationRef]]) {

  def moduleName = name

  def addDefaultImport(mn:ModuleName):Module = {
    def matchesImport:ModuleName => Declaration => Boolean = mn => {
      case ImportDeclaration(moduleName,_,_) if moduleName == mn => true
      case PositionedDeclaration(_,d) => matchesImport(mn)(d)
      case _ => false
    }
    if (mn == moduleName || decls.exists(matchesImport(mn))) this else 
      this.copy(decls = ImportDeclaration(mn, Implicit, None) +: decls)
  }
}
