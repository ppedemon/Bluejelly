/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.common

import scala.collection.mutable.Map

/**
 * Provide fresh names throughout the compilation pipeline.
 * @author ppedemon
 */
/*
object NameSupply {
  val ns:Map[SupplyNS,Int] = Map()
  
  def freshName[T <: SupplyNS](t:T) = {
    val k = ns.getOrElseUpdate(t, 0)
    ns(t) = k+1
    Name(Symbol(k.toString))
  }
  
  def freshTyVar = freshName(TyVarNS)
}
*/