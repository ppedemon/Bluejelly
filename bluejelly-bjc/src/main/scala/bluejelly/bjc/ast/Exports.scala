package bluejelly.bjc.ast

import scalaz._

import bluejelly.bjc.Types._
import bluejelly.bjc.Names._
import bluejelly.bjc.{ProperName,TypeName,ConstructorName,ClassName,Qualified,Id}
import bluejelly.bjc.{Type,Constraint,TypeConstructor,ConstrainedType}

object Exports {
  def isDConExported
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

  type ClassOrTypeName = Qualified[ProperName[ClassName.type]] \/ Qualified[ProperName[TypeName.type]]

  def typeInstanceConstituents(d:Declaration):Seq[ClassOrTypeName] = d match {
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
}
