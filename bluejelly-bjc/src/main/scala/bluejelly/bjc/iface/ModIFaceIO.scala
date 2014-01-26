/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.iface

import org.objectweb.asm.ClassAdapter
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.MethodAdapter
import org.objectweb.asm.Opcodes._

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import java.io.InputStream

/**
 * The object deals with saving and retrieving interfaces.
 * @author ppedemon
 */
object ModIFaceIO {

  def save(iface:ModIface, module:Array[Byte]):Array[Byte] = {
    val r = new ClassReader(module)
    val w = new ClassWriter(r, 0)
    val a = new IfaceWriter(w, iface)
    r.accept(a, 0)
    w.toByteArray()
  }
  
  def save(iface:ModIface, in:InputStream):Array[Byte] = {
    val r = new ClassReader(in)
    val w = new ClassWriter(r, 0)
    val a = new IfaceWriter(w, iface)
    r.accept(a, 0)
    w.toByteArray()
  }
}

/*
 * Write an interface as a static byte array to the class
 * proxied by the given ClassWriter. Like this:
 * 
 *   public static final byte[] $IFACE;
 *   static { $IFACE = {....} }
 */
private class IfaceWriter(
    cw:ClassWriter, 
    iface:ModIface) extends ClassAdapter(cw) {

  private val ICONSTS = Map(
    0 -> ICONST_0,
    1 -> ICONST_1,
    2 -> ICONST_2,
    3 -> ICONST_3,
    4 -> ICONST_4,
    5 -> ICONST_5
  )

  private val clinit = "<clinit>";
  private val fName = "$IFACE";
    
  private val bytes = ifaceBytes(iface) 
  private var hasStaticBlock = false
  private var modName:String = null

  // Generate initialization code in a <clinit> method
  private class StaticBlockAdapter(mv:MethodVisitor) extends MethodAdapter(mv) {    
    override def visitCode() {
      super.visitCode()
      pushInt(mv, bytes.length)
      visitIntInsn(NEWARRAY, T_BYTE)
      
      var ix = 0
      for (b <- bytes) {
        visitInsn(DUP)
        pushInt(mv, ix)
        pushInt(mv, b)
        visitInsn(BASTORE)
        ix += 1
      }
      
      visitFieldInsn(PUTSTATIC, modName, fName, "[B")
    }

    override def visitMaxs(maxStack:Int, maxLocals:Int) {
      super.visitMaxs(scala.math.max(maxStack,4), maxLocals)
    }
  }
  
  // Create $IFACE field
  override def visit(
      version:Int, 
      access:Int, 
      name:String, 
      signature:String, 
      superName:String, 
      interfaces:Array[String]) {
    modName = name
    super.visit(version, access, name, signature, superName, interfaces)    
    super.visitField(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, 
      fName, "[B", null, null).visitEnd()
  }
  
  override def visitMethod(
      access:Int, 
      name:String, 
      desc:String, 
      signature:String, 
      exceptions:Array[String]):MethodVisitor = {
    val mv = super.visitMethod(access, name, desc, signature, exceptions)
    if (name.equals(clinit) && !hasStaticBlock) {
      hasStaticBlock = true
      new StaticBlockAdapter(mv)
    } else {
      mv
    }
  }
  
  override def visitEnd() {
    if (!hasStaticBlock) {
      val mv = new StaticBlockAdapter(
          cv.visitMethod(ACC_STATIC, clinit, "()V", null, null))
      mv.visitCode()
      mv.visitInsn(RETURN)
      mv.visitMaxs(4, 0)
      mv.visitEnd()
    }
    super.visitEnd()
  }
  
  private def ifaceBytes(iface:ModIface) = {
    val out = new ByteArrayOutputStream
    iface.serialize(new DataOutputStream(out))
    out.toByteArray()
  }
  
  private def pushInt(mv:MethodVisitor, x:Int) = x match {
    case _ if ICONSTS.contains(x) => 
      mv.visitInsn(ICONSTS(x))
    case _ if x >= Byte.MinValue && x <= Byte.MaxValue => 
      mv.visitIntInsn(BIPUSH, x)
    case _ if x >= Short.MinValue && x <= Short.MaxValue =>
      mv.visitIntInsn(SIPUSH, x)
    case _ => 
      mv.visitLdcInsn(x)
  }
}
