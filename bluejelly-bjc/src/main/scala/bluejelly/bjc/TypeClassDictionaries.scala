package bluejelly.bjc

case class TypeClassDictionaryInScope(
  val name:Qualified[Ident], 
  val path:Seq[(Qualified[ProperName[ClassName.type]],Int)],
  val className:Qualified[ProperName[ClassName.type]],
  val instanceTypes:Seq[Type],
  val dependencies:Option[Seq[Constraint]]
)

trait DictionaryValue
case class LocalDictionary(val ident:Qualified[Ident]) extends DictionaryValue
case class GlobalDictionaryValue(val ident:Qualified[Ident]) extends DictionaryValue
case class DepedentDictionaryValue(
  val ident:Qualified[Ident], 
  val deps:Seq[DictionaryValue]) extends DictionaryValue
case class SubclassdictionaryValue(
  val dict:DictionaryValue, 
  val superclassName:Qualified[ProperName[ClassName.type]],
  val index:Int) extends DictionaryValue
