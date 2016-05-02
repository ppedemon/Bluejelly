package bluejelly.bjc.ast

import scalaz._
import std.function._
import syntax.arrow._

import bluejelly.bjc.Types._
import bluejelly.bjc.Names._
import bluejelly.bjc.{Id,ProperName,TypeName,ConstructorName,ClassName,ModuleName,Qualified}
import bluejelly.bjc.{Type,Constraint,TypeConstructor,ConstrainedType}

object Exports {
  private def isDConExported
    (typeName:ProperName[TypeName.type], declRefs:Option[Seq[DeclarationRef]])
    (dcon:ProperName[ConstructorName.type]):Boolean = declRefs match {
    case None => true
    case Some(rs) => 
      def exports:DeclarationRef => Boolean = {
        case PositionedDeclarationRef(_,r) => exports(r)
        case TypeRef(name, None) => true
        case TypeRef(name, Some(constructors)) => constructors contains dcon
      }
      rs exists exports
  }

  def isExported(declRefs:Option[Seq[DeclarationRef]])(d:Declaration):Boolean = (declRefs,d) match {
    case (None,_) | (_, TypeInstanceDeclaration(_,_,_,_,_)) => true
    case (_, PositionedDeclaration(_,d)) => isExported(declRefs)(d)
    case (Some(rs), _) => 
      def matches:Declaration => DeclarationRef => Boolean = d => dr => (d,dr) match {
        case (TypeDeclaration(ident0,_), ValueRef(ident1)) => ident0 == ident1
        case (ValueDeclaration(ident0,_,_,_), ValueRef(ident1)) => ident0 == ident1
        case (DataDeclaration(_,name0,_,_), TypeRef(name1,_)) => name0 == name1
        case (TypeSynonymDeclaration(name0,_,_), TypeRef(name1,_)) => name0 == name1
        case (TypeClassDeclaration(name0,_,_,_), TypeClassRef(name1)) => name0 == name1

        case (DataDeclaration(_,name,_,_), ProperRef(s)) => name.runProperName == s
        case (TypeClassDeclaration(name,_,_,_), ProperRef(s)) => name.runProperName == s

        case (FixityDeclaration(_,op,Some(Qualified(_,AliasValue(_)))), ValueRef(ident)) => op == ident.runIdent
        case (FixityDeclaration(_,op,Some(Qualified(_,AliasConstructor(_)))), ValueRef(ident)) => op == ident.runIdent
        case (FixityDeclaration(_,op,Some(Qualified(_,AliasType(_)))), TypeOpRef(ident)) => op == ident.runIdent

        case (PositionedDeclaration(_,d),_) => matches(d)(dr)
        case (_, PositionedDeclarationRef(_,dr)) => matches(d)(dr)

        case _ => false
      }
      rs exists matches(d)
  }

  private type ClassOrTypeName = Qualified[ProperName[ClassName.type]] \/ Qualified[ProperName[TypeName.type]]

  private def typeInstanceConstituents(d:Declaration):Seq[ClassOrTypeName] = d match {
    case TypeInstanceDeclaration(_, constraints, className, args, _) => 
      def fromConstraint:Constraint => Seq[ClassOrTypeName] = c => -\/(c.className) +: c.args.flatMap(fromType)
      def fromType:Type => Seq[ClassOrTypeName] = everythingOnTypes((_:Seq[ClassOrTypeName]) ++ (_:Seq[ClassOrTypeName]))(go)
      def go:Type => Seq[ClassOrTypeName] = {
        case TypeConstructor(name) => Seq(\/-(name))
        case ConstrainedType(constraints, _) => constraints flatMap fromConstraint
        case _ => Seq()
      }
      -\/(className) +: (constraints.flatMap(fromConstraint) ++ args.flatMap(fromType))

    case PositionedDeclaration(_,d) => typeInstanceConstituents(d)
    case _ => Seq()
  }

  private def filterInstances(mn:ModuleName, drefs:Option[Seq[DeclarationRef]])(decls:Seq[Declaration]):Seq[Declaration] = {
    def checkQual[A]:Qualified[A] => Boolean = q => q.isQualified && !q.isQualifiedWith(mn)
    def typeName:PartialFunction[DeclarationRef,ProperName[TypeName.type]] = {
      case TypeRef(name,_) => name
      case PositionedDeclarationRef(_,r) => typeName(r)
    }
    def typeClassName:PartialFunction[DeclarationRef,ProperName[ClassName.type]] = {
      case TypeClassRef(name) => name
      case PositionedDeclarationRef(_,r) => typeClassName(r)
    }
    def visibleOutside:Seq[ProperName[ClassName.type] \/ ProperName[TypeName.type]] => ClassOrTypeName => Boolean = 
      refs => q => q.fold(checkQual, checkQual) || refs.contains(q fold (n => -\/(n.disqualify), n => \/-(n.disqualify)))

    drefs match {
      case None => decls
      case Some(refs) => 
        val filteredRefs = refs.collect(typeClassName).map(-\/(_)) ++ refs.collect(typeName).map(\/-(_))
        decls.filter(typeInstanceConstituents(_).forall(visibleOutside(filteredRefs)))
    }
  }

  private def filterDataConstructors(drefs:Option[Seq[DeclarationRef]])(d:Declaration):Declaration = d match {
    case DataDeclaration(dtype, name, args, dcons) =>
      val dcons1 = dcons filter (((_:(ProperName[ConstructorName.type], Seq[Type]))._1) >>> isDConExported(name, drefs))
      DataDeclaration(dtype, name, args, dcons1)
    case PositionedDeclaration(pos,d) => PositionedDeclaration(pos, filterDataConstructors(drefs)(d))
    case _ => d 
  }

  def exportedDeclarations(m:Module):Seq[Declaration] = {
    def go:Seq[Declaration] => Seq[Declaration] = Declaration.flattenDataDecls _ >>> 
      (_.filter(isExported(m.exports))) >>> 
      (_.map(filterDataConstructors(m.exports))) >>> 
      filterInstances(m.moduleName, m.exports)
    go(m.decls)
  }
}
