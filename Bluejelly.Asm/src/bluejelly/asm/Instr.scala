/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm

import java.io.Writer
import java.io.StringWriter

/**
  * Something pretty-printable.
  * @author ppedemon
  */
trait PrettyPrintable {
  implicit val x:Int = 0
  def ppr(w:Writer)(implicit x:Int)
  override def toString = {
    val sw = new StringWriter
    ppr(sw)
    sw toString
  }
}

/**
  * Base class for an assembler instruction. 
  */
abstract class Instr extends PrettyPrintable {
  def ppr(w:Writer)(implicit x:Int) = Instr.ppr(this,w,x)
}

// -----------------------------------------------------------------------
// Control instructions
// -----------------------------------------------------------------------
case object Enter extends Instr
case object Return extends Instr

case object Raise extends Instr
case object Catch extends Instr

case class Jump(funId:String) extends Instr
case class EvalVar(off:Int, funId:String) extends Instr

case class RetCon(tag:Int, n:Int) extends Instr
case class RetInt(i:Int) extends Instr
case class RetStr(s:String) extends Instr
case class RetDbl(d:Double) extends Instr
case class RetChr(c:Char) extends Instr

// -----------------------------------------------------------------------
// Push instructions
// -----------------------------------------------------------------------

case class StackCheck(n:Int) extends Instr
case class DumpStack(s:String) extends Instr
case class PushVar(offset:Int) extends Instr
case class PushInt(i:Int) extends Instr
case class PushStr(s:String) extends Instr
case class PushDbl(d:Double) extends Instr
case class PushChr(c:Char) extends Instr
case class PushCode(funId:String) extends Instr
case class PushCont(funId:String) extends Instr
case class Slide(off:Int, n:Int) extends Instr

// -----------------------------------------------------------------------
// Handling application nodes
// -----------------------------------------------------------------------
case class MkApp(n:Int) extends Instr
case class MkNapp(n:Int) extends Instr
case object AllocApp extends Instr
case object AllocNapp extends Instr
case class PackApp(off:Int, n:Int) extends Instr
case class PackNapp(off:Int, n:Int) extends Instr

// -----------------------------------------------------------------------
// Handling tycons
// -----------------------------------------------------------------------
case class MkTyCon(tag:Int, n:Int) extends Instr
case class AllocTyCon(tag:Int) extends Instr
case class PackTyCon(off:Int, n:Int) extends Instr

// -----------------------------------------------------------------------
// Pattern matching
// -----------------------------------------------------------------------

class Block(val is:List[Instr]) extends PrettyPrintable {
  def ppr(w:Writer)(implicit x:Int) {
    var first = true
    for (i <- is) { 
      if (first) first = false; else w write '\n'
      i.ppr(w)(x)
    }
  }
}
object Block {
  def apply(is:List[Instr]) = new Block(is)
  def apply(is:Instr*) = new Block(is.toList)
}

class Alt[T](val v:T, val b:Block) extends PrettyPrintable {
  def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x)
    w write (".case %s:\n" format v)
    b.ppr(w)(x+2)
  }
}

case class MatchCon(alts:List[Alt[Int]], deflt:Option[Block]) extends Instr
case class MatchInt(alts:List[Alt[Int]], deflt:Option[Block]) extends Instr
case class MatchStr(alts:List[Alt[String]], deflt:Option[Block]) extends Instr
case class MatchDbl(alts:List[Alt[Double]], deflt:Option[Block]) extends Instr
case class MatchChr(alts:List[Alt[Char]], deflt:Option[Block]) extends Instr

// -----------------------------------------------------------------------
// Pretty printing
// -----------------------------------------------------------------------

/**
  * Pretty printer object. 
  */
object Instr {
  private def pref(n:Int, w:Writer, s:String) = w write (" "*n + s)

  private 
  def ppr[T](x:Int, w:Writer, name:String, alts:List[Alt[T]], d:Option[Block]) {
    w write (" "*x)
    w write (name + '\n')
    var first = true
    for (alt <- alts) {
      if (first) first = false; else w write '\n' 
      alt.ppr(w)(x)
    }
    d match {
      case None => ()
      case Some(b) => {
        if (!first) w write '\n'
        w write (" "*x + ".default:\n"); b.ppr(w)(x+2)
      }
    }
    w write ("\n" + " "*x + ".end")
  }
  
  def ppr(i:Instr, w:Writer, x:Int):Unit = i match {
    case Enter               => pref(x, w, "enter")
    case Return              => pref(x, w, "ret")
    case Raise               => pref(x, w, "raise")
    case Catch               => pref(x, w, "catch")
    case Jump(funId)         => pref(x, w, "jmp " + funId)
    case EvalVar(off, funId) => pref(x, w, "evalvar %d,%s" format (off, funId))
    case RetCon(tag, n)      => pref(x, w, "retcon %d,%d" format (tag, n))
    case RetInt(i)           => pref(x, w, "retint " + i)
    case RetStr(s)           => pref(x, w, "retstr " + s)
    case RetDbl(d)           => pref(x, w, "retdbl " + d)
    case RetChr(c)           => pref(x, w, "retchr " + c)
    
    case StackCheck(n)   => pref(x, w, "stack " + n)
    case DumpStack(s)    => pref(x, w, "dumpstack \"%s\"" format s)
    case PushVar(off)    => pref(x, w, "pushvar " + off)
    case PushInt(i)      => pref(x, w, "pushint " + i)
    case PushStr(s)      => pref(x, w, "pushstr \"%s\"" format s)
    case PushDbl(d)      => pref(x, w, "pushdbl " + d)
    case PushChr(c)      => pref(x, w, "pushchr '%c'" + c)
    case PushCode(funId) => pref(x, w, "pushcode " + funId)
    case PushCont(funId) => pref(x, w, "pushcont " + funId)
    case Slide(off, n)   => pref(x, w, "slide %d,%d" format (off, n))
    
    case MkApp(n)         => pref(x, w, "mkapp " + n)
    case MkNapp(n)        => pref(x, w, "mknapp " + n)
    case AllocApp         => pref(x, w, "newapp")
    case AllocNapp        => pref(x, w, "newnapp")
    case PackApp(off, n)  => pref(x, w, "packapp %d,%d" format (off, n))
    case PackNapp(off, n) => pref(x, w, "packnapp %d,%d" format (off, n))
    
    case MkTyCon(tag, n)    => pref(x, w, "mkcon %d,%d" format (tag, n))
    case AllocTyCon(tag)    => pref(x, w, "newcon " + tag)
    case PackTyCon(off, n)  => pref(x, w, "packcon %d,%d" format (off, n))
    
    case MatchCon(as,d) => ppr(x, w, "matchcon", as, d)
    case MatchInt(as,d) => ppr(x, w, "matchint", as, d)
    case MatchStr(as,d) => ppr(x, w, "matchstr", as, d)
    case MatchDbl(as,d) => ppr(x, w, "matchdbl", as, d)
    case MatchChr(as,d) => ppr(x, w, "matchchr", as, d)
  }
}
