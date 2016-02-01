/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core

import bluejelly.bjc.ast.module

import bluejelly.bjc.common.Name
import bluejelly.bjc.common.PrettyPrintable
import bluejelly.bjc.common.PprUtils._

import bluejelly.utils.Errors
import bluejelly.utils.Document
import bluejelly.utils.Document._

import scala.util.parsing.input.Position

import java.io.StringWriter

/**
 * Error bag for compiler errors.
 * @author ppedemon
 */
 class BjcErrors(val mod:String) extends Errors {

  private def ppr(d:Document):String = {
    val s = new StringWriter
    d.format(75, s)
    s.flush()
    s.toString
  }
  
  //private def q(t:Any):Document = text("`%s'" format t.toString)

  private def q[T <% PrettyPrintable](t:T) = 
    group("`" :: t.ppr :: text("'")) 
  
  private def loc(pos:Position, d:Document) = 
    gnest(group(mod :: ":" :: pprPos(pos) :: text(":") :/: d)) 

  private def locError(pos:Position, d:Document) {
    error(null, ppr(loc(pos,d)))
  }

  private def locWarning(pos:Position, d:Document) {
    warning(null, ppr(loc(pos,d)))
  }  

  // ---------------------------------------------------------------------
  // Parse error
  // ---------------------------------------------------------------------

  def parseError(pos:Position, msg:String) = locError(pos,text(msg))

  // ---------------------------------------------------------------------
  // Import chasing errors
  // ---------------------------------------------------------------------

  def implicitIfaceLoadError(modName:Symbol, msg:String) {
    val d = gnest(
      gnest("Failed to load interface for" :/: 
        group(q(modName) :: text(":"))) :/: text(msg))
    error(null, ppr(d))
  }

  def ifaceLoadError(imp:module.ImpDecl, msg:String) {
    val d = gnest(
      gnest("Failed to load interface for" :/: 
        group(q(imp.modId) :: text(":"))) :/: text(msg))
    locError(imp.pos, d)
  }

  def ifaceImportError(imp:module.ImpDecl, ispec:module.ISpec) {
    val d = gnest("module" :/: q(imp.modId) 
      :/: "does not export" :/: q(ispec))
    locError(imp.pos, d)
  }
}
 