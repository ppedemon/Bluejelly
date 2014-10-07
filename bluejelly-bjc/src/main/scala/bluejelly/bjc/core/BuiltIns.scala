/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.{GCon,UnitCon,TupleCon,ArrowCon,ListCon,Con}
import bluejelly.bjc.common.Name

import Type._

/**
 * Information about built-in stuff.
 * @author ppedemon
 */
object BuiltIns {

  private val nmInt = Name('Int)
  private val nmBigInt = Name('BigInt)
  private val nmDouble = Name('Double)
  private val nmChar = Name('Char)
  private val nmString = Name('String)
  private val nmArrow = Name('->)
  private val nmUnit = Name(Symbol("()"))
  private val nmList = Name(Symbol("[]"))
  private val nmCons = Name(':)

  val builtInNames = Set(
    nmInt,
    nmBigInt,
    nmDouble,
    nmChar,
    nmString,
    nmUnit,
    nmArrow,
    nmList
  )

  val builtInTyCons = Map(
    nmInt -> primTyCon(nmInt),
    nmBigInt -> primTyCon(nmBigInt),
    nmDouble -> primTyCon(nmDouble),
    nmChar -> primTyCon(nmChar),
    nmString -> primTyCon(nmString),
    nmUnit -> primTyCon(nmUnit),
    nmArrow -> arrowTyCon,
    nmList -> listTyCon
  )

  def isBuiltIn(con:GCon) = con match {
    case UnitCon | ArrowCon | ListCon | TupleCon(_) => true
    case Con(n) if builtInNames(n) => true
    case _ => false
  }

  def builtInsEnv =
    builtInTyCons.values.foldLeft(new BjcEnv)(_ addTyCon _)

  private def primTyCon(name:Name) =
    new TyCon(name, false, Nil, Nil, Nil)

  private def arrowTyCon = 
    new TyCon(nmArrow, false, Nil, List(tyVar('a),tyVar('b)), Nil) 

  private def listTyCon = {
    val nilType = PolyTy(List(tyVar('a)), AppTy(TcTy(ListCon),tvTy('a)))

    val consType = PolyTy(List(tyVar('a)), mkFun(List(
        TvTy(Name('a)), 
        AppTy(TcTy(ListCon),tvTy('a)), 
        AppTy(TcTy(ListCon),tvTy('a)))))

    val nil = new DataCon(nmList, nilType, Nil, Nil)
    val cons = new DataCon(nmCons, consType, List(false,false), Nil)
    val list = new TyCon(nmList, false, Nil, List(tyVar('a)), List(nil,cons))
    nil.tycon = list
    cons.tycon = list
    list
  }
}
