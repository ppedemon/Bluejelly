/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Type

/**
  * Help generate bytecode instructions.
  * @author ppedemon
  */
object ByteCodeHelper {
  
  import org.objectweb.asm.ClassWriter._
  import org.objectweb.asm.Opcodes._

  val ctx  = "bluejelly/runtime/ExecutionContext"
  val dict = "bluejelly/runtime/Dictionary"
  val node = "bluejelly/runtime/nodes/Node"
  val int  = "bluejelly/runtime/nodes/Int"
  val char = "bluejelly/runtime/nodes/Char"
  val dbl  = "bluejelly/runtime/nodes/Double"
  val str  = "bluejelly/runtime/nodes/Str"
  
  private val iconsts = Map(
   -1 -> ICONST_M1, 
    0 -> ICONST_0, 
    1 -> ICONST_1, 
    2 -> ICONST_2, 
    3 -> ICONST_3,
    4 -> ICONST_4,
    5 -> ICONST_5)  

  // Transform a class name into a descriptor
  def toDesc(name:String) = 'L' + name + ';'
    
  // Push stack from the ExecutionContext (variable #1)
  def getStack(v:MethodVisitor) {
    v.visitVarInsn(ALOAD, 1)
    v.visitFieldInsn(GETFIELD, ctx, "s", '[' + toDesc(node))
  }
  
  // Push stack pointer from the ExecutionContext (variable #1)
  def getStackPointer(v:MethodVisitor) {
    v.visitVarInsn(ALOAD, 1)
    v.visitFieldInsn(GETFIELD, ctx, "sp", "I")
  }
  
  // Efficiently push an integer value onto the JVM stack
  def pushIntConst(v:MethodVisitor, n:Int) {
    if (iconsts.contains(n)) {
      v.visitInsn(iconsts(n))
    } else if (n >= Byte.MinValue && n <= Byte.MaxValue) {
      v.visitIntInsn(BIPUSH, n)
    } else if (n >= Short.MinValue && n <= Short.MaxValue) {
      v.visitIntInsn(SIPUSH, n)
    } else {
      v.visitLdcInsn(n)
    }
  }

  // Invoke a method with signature "()V" on an ExecutionContext instance
  def invokeVoidCtxMethod(method:String)(v:MethodVisitor) {
      v.visitVarInsn(ALOAD, 1)
      v.visitMethodInsn(INVOKEVIRTUAL, ctx, method, "()V")
  }
  
  // Invoke a method with signature "(I)V" on an ExecutionContext instance
  def invokeIntCtxMethod(method:String, n:Int)(v:MethodVisitor) {
    v.visitVarInsn(ALOAD, 1)
    pushIntConst(v, n)
    v.visitMethodInsn(INVOKEVIRTUAL, ctx, method, "(I)V")
  }
  
  // Invoke a method with signature "(II)V" on an ExecutionContext instance
  def invokeIntIntCtxMethod(method:String, n:Int, m:Int)(v:MethodVisitor) {
    v.visitVarInsn(ALOAD, 1)
    pushIntConst(v, n)
    pushIntConst(v, m)
    v.visitMethodInsn(INVOKEVIRTUAL, ctx, method, "(II)V")
  }
    
  // Invoke a method with signature "(Ljava/lang/String;)V" 
  // on an ExecutionContext instance
  def invokeStrCtxMethod(method:String, arg:String)(v:MethodVisitor) {
    v.visitVarInsn(ALOAD, 1)
    v.visitLdcInsn(arg)
    v.visitMethodInsn(INVOKEVIRTUAL, ctx, method, "(Ljava/lang/String;)V")
  }

  // Invoke a method with signature "(ILjava/lang/String;)V" 
  // on an ExecutionContext instance
  def invokeIntStrCtxMethod(method:String, n:Int, s:String)(v:MethodVisitor) {
    v.visitVarInsn(ALOAD, 1)
    pushIntConst(v, n)
    v.visitLdcInsn(s)
    v.visitMethodInsn(INVOKEVIRTUAL, ctx, method, "(ILjava/lang/String;)V")
  }
  
  // Mark the code emitted by f as the final instruction of a function,
  // by adding a RETURN instruction after it.
  def mkFinal(f:MethodVisitor => Unit)(v:MethodVisitor) {
    f(v)
    v.visitInsn(RETURN)
  }
  
  // Generate JVM code for a [pushvar n] instruction
  def pushVar(v:MethodVisitor, n:Int) {
    getStack(v)
    getStackPointer(v)
    pushIntConst(v, 1)
    v.visitInsn(IADD)
    getStack(v)
    getStackPointer(v)
    
    // Only substract from sp is n is not zero
    if (n != 0) {
      pushIntConst(v, n)
      v.visitInsn(ISUB)
    }
    
    v.visitInsn(AALOAD)
    v.visitInsn(AASTORE)
    v.visitVarInsn(ALOAD, 1)
    v.visitInsn(DUP)
    v.visitFieldInsn(GETFIELD, ctx, "sp", "I")
    pushIntConst(v, 1)
    v.visitInsn(IADD)
    v.visitFieldInsn(PUTFIELD, ctx, "sp", "I")
  }
 
  // Push some basic node onto the stack
  def pushBasic(v:MethodVisitor, f: MethodVisitor => Unit) {
    getStack(v)
    v.visitVarInsn(ALOAD, 1)
    v.visitInsn(DUP)
    v.visitFieldInsn(GETFIELD, ctx, "sp", "I")
    pushIntConst(v, 1)
    v.visitInsn(IADD)
    v.visitInsn(DUP_X1)
    v.visitFieldInsn(PUTFIELD, ctx, "sp", "I")
    f(v)
    v.visitInsn(AASTORE)
  }

  def createIntNode(n:Int)(v:MethodVisitor) {
    pushIntConst(v, n)
    v.visitMethodInsn(INVOKESTATIC, int, "mkInt", "(I)" + toDesc(int))
  }

  def createCharNode(c:Char)(v:MethodVisitor) {
    pushIntConst(v, c.toInt)
    v.visitMethodInsn(INVOKESTATIC, char, "mkChr", "(C)" + toDesc(char))
  }
  
  def createDblNode(d:Double)(v:MethodVisitor) {
    v.visitTypeInsn(NEW, dbl)
    v.visitInsn(DUP)
    v.visitLdcInsn(d)
    v.visitMethodInsn(INVOKESPECIAL, dbl, "<init>", "(D)V")
  }

  def createStrNode(s:String)(v:MethodVisitor) {
    v.visitTypeInsn(NEW, str)
    v.visitInsn(DUP)
    v.visitLdcInsn(s)
    v.visitMethodInsn(INVOKESPECIAL, str, "<init>", "(Ljava/lang/String;)V")
  }

  def pushInt(v:MethodVisitor, n:Int) {
    pushBasic(v, createIntNode(n))
  }

  def pushChar(v:MethodVisitor, c:Char) {
    pushBasic(v, createCharNode(c))
  }

  def pushDbl(v:MethodVisitor, d:Double) {
    pushBasic(v, createDblNode(d))
  }
  
  def pushStr(v:MethodVisitor, s:String) {
    pushBasic(v, createStrNode(s))
  }
  
  def pushCode(v:MethodVisitor, fun:String) {
    val desc = "(Ljava/lang/String;)Lbluejelly/runtime/nodes/Executable;"
    pushBasic(v, { v => 
      v.visitVarInsn(ALOAD, 1)
      v.visitLdcInsn(fun)
      v.visitMethodInsn(INVOKEVIRTUAL, ctx, "getFun", desc)
    })
  }
  
  def pushDict(v:MethodVisitor, dictId:String) {
    val desc = "(Ljava/lang/String;)Lbluejelly/runtime/nodes/Dict;"
    pushBasic(v, { v => 
      v.visitVarInsn(ALOAD, 1)
      v.visitLdcInsn(dictId)
      v.visitMethodInsn(INVOKEVIRTUAL, ctx, "getDict", desc)
    })
  }
  
}