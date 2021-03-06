/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4

import bluejelly.asm.Instr
import bluejelly.asm.Block
import java.io.Writer

/**
 * Push a symbolic [Var] onto the stack. This will be solved
 * into a [PushVar] instructions using the stack offset.
 */
case class PushSym(val v:Var) extends Instr {
  override def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x + "pushsym " + v)
  }
}

/**
 * Create an application node for a symbolic [Var]. The var will be
 * solved to an offset, and we will replace this with a [PackApp].
 */
case class PackAppSym(val v:Var, n:Int) extends Instr {
  override def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x + ("packappsym %s,%d" format (v,n)))
  }  
}

/**
 * Create an non-updatable application node for a symbolic [Var]. 
 * The var will be solved to an offset, and we will replace this 
 * with a [PackNapp].
 */
case class PackNappSym(val v:Var, n:Int) extends Instr {
  override def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x + ("packnappsym %s,%d" format (v,n)))
  }  
}

/**
 * Create an constructor node for a symbolic [Var]. The var will be
 * solved to an offset, and we will replace this with a [PackTyCon].
 */
case class PackTyConSym(val v:Var, n:Int) extends Instr {
  override def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x + ("packtyconsym %s,%d" format (v,n)))
  }  
}

/**
 * Pseudo-instruction for a function parameter. This allows to
 * compute the stack offset for the parameter, thus replacing
 * [PushSym] with [PushVar] instructions
 */
case class Param(val v:Var) extends Instr {
  override def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x + ".param " + v)
  }  
}

/**
 * Pseudo-instruction for local bindings introduced by let!, let or 
 * let rec. This allows to compute the stack offset for the local 
 * binding, thus replacing [PushSym]s with [PushVar] instructions.
 */
case class Local(val v:Var) extends Instr {
  override def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x + ".local " + v)
  }
}

/**
 * Block to evaluate. Parameter name is continuation name, and
 * matcher flags indicated whether the continuation is a matcher.
 */
case class Reduce(val name:String, val matcher:Boolean, val b:Block) extends Instr {
  override def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x + ".reduce ")
    w write name
    if (matcher) w write "[matcher]:\n" else w write ":\n"
    b.ppr(w)(x+2)
    w write ("\n" + " "*x + ".end")
  }
}
object Reduce{
  def apply(name:String, matcher:Boolean, is:List[Instr]) = 
    new Reduce(name,matcher,new Block(is))
  def apply(name:String, matcher:Boolean, is:Instr*) = 
    new Reduce(name,matcher,new Block(is.toList))
}

/**
 * A block producing a value on the stack. This allows to compute
 * the number of stack positions to slide when leaving the block.
 */
case class Atom(val b:Block) extends Instr {
  override def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x)
    w write ".atom:\n"
    b.ppr(w)(x+2)
    w write ("\n" + " "*x + ".end")
  }
}
object Atom {
  def apply(is:List[Instr]) = new Atom(new Block(is))
  def apply(is:Instr*) = new Atom(new Block(is.toList))
}

/**
 * A block holding instructions for initializing bindings in a 
 * let rec. This allows to compute the number of stack positions 
 * to slide when leaving the block.
 */
case class Init(val b:Block) extends Instr {
  override def ppr(w:Writer)(implicit x:Int) {
    w write (" "*x)
    w write (".init:\n")
    b.ppr(w)(x+2)
    w write ("\n" + " "*x + ".end")
  }
}
object Init {
  def apply(is:List[Instr]) = new Init(new Block(is))
  def apply(is:Instr*) = new Init(new Block(is.toList))
}
