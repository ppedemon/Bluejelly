/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.common

import scala.text.DocNil
import scala.text.Document
import scala.text.Document.{_}
import java.io.StringWriter
import java.io.Writer

/*
 * Convenience constants for pretty-printing.
 */
private object PrettyPrintable {
  val defaultWidth = 75
}

/**
 * Trait for pretty-printable things. Users are expected to implement
 * <code>ppr</code>, while the <code>toString</code> method defaults
 * to the string representation of <code>ppr</code>.
 * 
 * @author ppedemon
 *
 */
trait PrettyPrintable {
  def ppr:Document
  def output(width:Int, writer:Writer) {
    ppr.format(width, writer)
    writer.flush
  }
  override def toString = {
    val w = new StringWriter
    output(PrettyPrintable.defaultWidth, w)
    w.toString
  }
}

/**
 * Pretty printing utilities.
 * @author ppedemon
 */
object PprUtils {
  
  def nl = text("\n")
  
  def quoted(x:Any) = text("`%s'" format x)

  def gnest(d:Document) = group(nest(2,d))
  
  def cat(d0:Document, d1:Document):Document = d0 match {
    case DocNil => d1
    case _ => d1 match {
      case DocNil => d0
      case _ => d0 :/: d1
    }
  }

  def cat(ds:List[Document]):Document = 
    ds.foldLeft[Document](empty)(cat(_,_))
    
  def between(l:String, d:Document, r:String) = 
    l :: d :: text(r)
      
  def pprMany(xs:List[PrettyPrintable]) = 
    group(xs.foldRight[Document](empty)((x,d) => cat(x.ppr,d)))
    
  def pprMany(xs:List[PrettyPrintable], sep:String):Document = group(
    xs match {
      case Nil => empty
      case List(x) => x.ppr
      case x::xs => cat(x.ppr :: text(sep), pprMany(xs,sep))
    })
    
  def pprList(xs:List[PrettyPrintable])  = between("[",pprMany(xs,","),"]")
  def pprTuple(xs:List[PrettyPrintable]) = between("(",pprMany(xs,","),")")
  
  def vppr(xs:List[PrettyPrintable]) = 
    xs.foldRight[Document](empty) {
      case (x,DocNil) => x.ppr
      case (x,d) => x.ppr :/: d
    }
}
