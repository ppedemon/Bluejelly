package bluejelly.bjc

import bluejelly.bjc.{Constants => C}, Types._

// -----------------------------------------------------------------------
// Auxiliary definitions
// -----------------------------------------------------------------------
trait NameVisibility
case object Undefined extends NameVisibility
case object Defined extends NameVisibility

trait NameKind
case object Private extends NameKind
case object Public extends NameKind
case object External extends NameKind

trait TypeKind
case object TypeSynonym extends TypeKind
case object ExternData extends TypeKind
case object LocalTypeVariable extends TypeKind
case object ScopedTypeVar
case class DataType(
  vars:Seq[(String, Option[Kind])], 
  datacons:Seq[(ProperName[ConstructorName.type], Seq[Type])])

trait DataDeclType
case object Data extends DataDeclType
case object NewType extends DataDeclType

// -----------------------------------------------------------------------
// Environment
// -----------------------------------------------------------------------
case class NameProps(val ty:Type, val nameKind:NameKind, val nameVisibility:NameVisibility)
case class TypeConProps(val kind:Kind, val typeKind:TypeKind)
case class DataConProps(val dataDeclType:DataDeclType, val tyconName:ProperName[TypeName.type], val ty:Type, val vars:Seq[Ident])
case class TypeSynProps(val vars:Seq[(String, Option[Kind])], val ty:Type)
case class TypeClassProps(val vars:Seq[(String, Option[Kind])], val members:Seq[(Ident,Type)], val constraints:Seq[Constraint])

class Environment(
  val names:Map[(ModuleName,Ident),NameProps],
  val types:Map[Qualified[ProperName[TypeName.type]], TypeConProps],
  val dataConstructors:Map[Qualified[ProperName[ConstructorName.type]], DataConProps],
  val typeSynonyms: Map[Qualified[ProperName[TypeName.type]], TypeSynProps],
  val typeClassDictionaries:Environment.DictsInScope,
  val typeClasses:Map[Qualified[ProperName[ClassName.type]], TypeClassProps]) {

  def lookupConstructor(ctor:Qualified[ProperName[ConstructorName.type]]):DataConProps = 
    dataConstructors.get(ctor) getOrElse Panic(s"Data constructor not found: ${ctor.showQualified(_.runProperName)}")

  def lookupValue(ident:Qualified[Ident]):Option[NameProps] = ident match {
    case Qualified(Some(m),ident) => names.get((m,ident))
    case _ => None
  }

  def isNewtypeConstructor(ctor:Qualified[ProperName[ConstructorName.type]]):Boolean = lookupConstructor(ctor) match {
    case DataConProps(NewType, _, _, _) => true
    case DataConProps(Data, _, _, _) => false
  }
}

object Environment {
  type DictsInScope = Map[Option[ModuleName], Map[Qualified[ProperName[ClassName.type]], Map[Qualified[Ident], TypeClassDictionaryInScope]]]

  def primName[T <: ProperNameType](s:String):Qualified[ProperName[T]] = 
    Qualified(Some(Names.toModuleName(C.prim)), ProperName[T](s))

  def primTy(s:String):Type = TypeConstructor(primName(s))

  val tyFunction = primTy("Function")
  val tyString = primTy("String")
  val tyChar = primTy("Char")
  val tyDouble = primTy("Double")
  val tyInt = primTy("Int")
  val tyBool = primTy("Bool")
  val tyList = primTy("List")

  def isTypeOrApp(intended:Type, ty:Type):Boolean = ty match {
    case TypeApp(ty,_) => ty == intended
    case ty => ty == intended
  }

  def isTuple(ty:Type):Boolean = ty match {
    case TypeApp(ty,_) => isTuple(ty)
    case TupleConstructor(_) => true
    case _ => false
  }

  def tupleConProps(tupleCon:TupleConstructor) =
    TypeConProps(kind = Seq.fill(tupleCon.arity)(KStar).reduceRight[Kind](KFun(_,_)), typeKind = ExternData)

  def isFunction(ty:Type) = isTypeOrApp(tyFunction, ty)
  def isList(ty:Type) = isTypeOrApp(tyList, ty)

  def function(from:Type, to:Type) = TypeApp(TypeApp(tyFunction, from), to)

  def primTypes:Map[Qualified[ProperName[TypeName.type]], TypeConProps] = Map(
    primName("Function") -> TypeConProps(KFun(KStar, KFun(KStar, KStar)), ExternData),
    primName("List") -> TypeConProps(KFun(KStar, KStar), ExternData),
    primName("String") -> TypeConProps(KStar, ExternData),
    primName("Char") -> TypeConProps(KStar, ExternData),
    primName("Double") -> TypeConProps(KStar, ExternData),
    primName("Int") -> TypeConProps(KStar, ExternData),
    primName("Bool") -> TypeConProps(KStar, ExternData)  
  )

  def primDataCons:Map[Qualified[ProperName[ConstructorName.type]], DataConProps] = {
    def listOf(a:String) = TypeApp(tyList, TypeVar(a))
    val listName = ProperName[TypeName.type]("List")
    val tyNil = mkForAll(Seq("a"), listOf("a"))
    val tyCons = mkForAll(Seq("a"), function(TypeVar("a"), function(listOf("a"), listOf("a"))))
    Map(
      primName("Nil") -> DataConProps(Data, listName, tyNil, Seq()),
      primName("Cons") -> DataConProps(Data, listName, tyCons, Seq(Id("a")))
    )
  }

  def initEnvironment = new Environment(Map(), primTypes, primDataCons, Map(), Map(), Map())
}
