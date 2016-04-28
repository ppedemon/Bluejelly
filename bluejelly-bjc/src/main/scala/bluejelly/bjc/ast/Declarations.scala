package bluejelly.bjc.ast

import scalaz._

import bluejelly.bjc.{Environment,DataDeclType,NameKind}
import bluejelly.bjc.Kind
import bluejelly.bjc.{Constraint,Type}
import bluejelly.bjc.{Ident,ProperName,ClassName,ConstructorName,TypeName,ModuleName,Qualified}

// -----------------------------------------------------------------------
// References to declarations in import/export lists
// -----------------------------------------------------------------------
trait DeclarationRef
case class TypeRef(name:ProperName[TypeName.type], ctors:Option[Seq[ProperName[ConstructorName.type]]]) extends DeclarationRef
case class TypeOpRef(ident:Ident) extends DeclarationRef
case class ValueRef(ident:Ident) extends DeclarationRef
case class TypeClassRef(name:ProperName[ClassName.type]) extends DeclarationRef
case class TypeInstanceRef(ident:Ident)
case class ModuleRef(moduleName:ModuleName) extends DeclarationRef
case class ProperRef(name:String) extends DeclarationRef
case class PositionedDeclarationRef(pos:SourceSpan, decl:DeclarationRef) extends DeclarationRef

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
case class Explicit(refs:Seq[DeclarationRef]) extends ImportDeclarationType
case class Hiding(refs:Seq[DeclarationRef]) extends ImportDeclarationType

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

case class Lit(value:Literal[Expr]) extends Expr
case class UnaryMinus(expr:Expr) extends Expr
case class BinaryNoParens(left:Expr, op:Expr, right:Expr) extends Expr
case class Parens(expr:Expr) extends Expr
case class OperatorSection(op:Expr, operand:Expr \/ Expr) extends Expr
case class Abs(args:Ident \/ Binder, body:Expr) extends Expr
case class App(fun:Expr, arg:Binder) extends Expr
case class Var(ident:Qualified[Ident]) extends Expr
case class Cond(cond:Expr, thenExpr:Expr, elseExpr:Expr) extends Expr
case class Constructor(name:Qualified[ProperName[ConstructorName.type]]) extends Expr
case class Case(expr:Expr, alts:Seq[CaseAlternative]) extends Expr
case class TypedValue(check:Boolean, expr:Expr, ty:Type) extends Expr
case class Let(decls:Seq[Declaration], body:Expr) extends Expr
case class Do(elems:Seq[DoNotationElem]) extends Expr
case class Hole(msg:String) extends Expr
case class PositionedValue(pos:SourceSpan, expr:Expr) extends Expr

// Wrapper for an expression constructing a dictionary for a given class.
// Expr must be of the form: App(App(...App(Tuple(n),arg_1)...),arg_n)
case class TypeClassDictionaryConstructorApp(className:Qualified[ProperName[ClassName.type]], expr:Expr) extends Expr

// Type class dictionary accessor, to be solved during desugaring
case class TypeClassDictionaryAccessor(name:Qualified[ProperName[ClassName.type]], ident:Ident) extends Expr

// Placeholder to be solved into a type class dictionary. Holds a constraint describing 
// the intended class name and arguments, and all the dictionaries in scope.
case class TypeClassDictionary(self:Constraint, typeClassDictionaries:Environment.DictsInScope) extends Expr

// Placeholder for a super class dictionary to be solved into a type class dictionary. 
case class SuperClassDictionary(name:Qualified[ProperName[ClassName.type]], args:Seq[Type]) extends Expr

// -----------------------------------------------------------------------
// Auxiliary
// -----------------------------------------------------------------------
case class CaseAlternative(binders:Seq[Binder], body:Seq[(Expr,Expr)] \/ Expr)

trait DoNotationElem
case class DoNotationExpr(expr:Expr) extends DoNotationElem
case class DoNotationBind(binder:Binder, expr:Expr) extends DoNotationElem
case class DoNotationLet(decls:Seq[Declaration]) extends DoNotationElem
case class PositionedDoNotationElem(pos:SourceSpan, elem:DoNotationElem) extends DoNotationElem

// -----------------------------------------------------------------------
// Declarations
// -----------------------------------------------------------------------
trait Declaration

case class ImportDecl(
  moduleName:ModuleName, 
  importType:ImportDeclarationType, 
  alias:Option[ModuleName]) extends Declaration

case class DataDecl(
  dataDeclType:DataDeclType, 
  name:ProperName[TypeName.type], 
  vars:Seq[(String,Option[Kind])], 
  dcons:Seq[(ProperName[ConstructorName.type], Seq[Type])]) extends Declaration

case class TypeSynonymDecl(
  name:ProperName[TypeName.type], 
  vars:Seq[(String, Option[Kind])], ty:Type) extends Declaration

case class ValueDecl(
  ident:Ident, 
  nameKind:NameKind, 
  args:Seq[Binder], 
  body:Seq[(Expr,Expr)] \/ Expr) extends Declaration

case class TypeClassDecl(
  name:ProperName[ClassName.type], 
  vars:Seq[(String, Option[Kind])], 
  constratints:Seq[Constraint],
  members:Seq[Declaration]) extends Declaration

case class TypeInstanceDecl(
  ident:Ident, 
  constraints:Seq[Constraint], 
  className:Qualified[ProperName[ClassName.type]], 
  args:Seq[Type], 
  body:TypeInstanceBody) extends Declaration

case class FixityDecl(fixity:Fixity, op:String, alias:Option[Qualified[FixityAlias]]) extends Declaration

case class TypeDecl(ident:Ident, ty:Type) extends Declaration

case class DataBindingGroupDecl(dataDecls:Seq[Declaration]) extends Declaration
case class BindingGroupDecl(decls:Seq[(Ident, NameKind, Expr)]) extends Declaration

case class PositionedDecl(pos:SourceSpan, decl:Declaration) extends Declaration

// -----------------------------------------------------------------------
// Auxiliary stuff for declarations
// -----------------------------------------------------------------------
trait FixityAlias
case class AliasValue(ident:Ident) extends FixityAlias
case class AliasConstructor(name:ProperName[ConstructorName.type]) extends FixityAlias
case class AliasType(name:ProperName[TypeName.type]) extends FixityAlias

trait TypeInstanceBody
case object DerivedInstance extends TypeInstanceBody
case class ExplicitInstance(members:Seq[Declaration]) extends TypeInstanceBody

// -----------------------------------------------------------------------
// Finally, top level modules
// -----------------------------------------------------------------------
case class Module(
  pos:SourceSpan, 
  name:ModuleName, 
  decls:Seq[Declaration], 
  exports:Option[Seq[DeclarationRef]])
