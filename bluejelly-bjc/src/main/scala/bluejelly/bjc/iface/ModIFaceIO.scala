/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.iface

import org.objectweb.asm.{ClassAdapter,ClassReader,ClassWriter}
import org.objectweb.asm.{Attribute,ByteVector,Label}
import org.objectweb.asm.commons.EmptyVisitor

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.io.{DataInputStream,DataOutputStream,InputStream}

/**
 * The object deals with saving and retrieving interfaces.
 * @author ppedemon
 */
object ModIFaceIO {

  def save(iface:ModIface, module:Array[Byte]):Array[Byte] =
    save(iface, new ClassReader(module))
  
  def save(iface:ModIface, in:InputStream):Array[Byte] =
    save(iface, new ClassReader(in))
  
  private def save(iface:ModIface, r:ClassReader) = {
    val w = new ClassWriter(r, 0)
    val a = new IfaceWriter(w, iface)
    r.accept(a, 0)
    w.toByteArray()    
  }
  
  def load(in:InputStream):ModIface = load(new ClassReader(in))
  
  def load(modName:String):ModIface = load(new ClassReader(modName))
  
  private def load(r:ClassReader) = {
    val ir = new IfaceReader
    r.accept(ir, Array[Attribute](new IfaceAttr), 
        ClassReader.SKIP_CODE|ClassReader.SKIP_DEBUG|ClassReader.SKIP_FRAMES)
    ModIface.load(new DataInputStream(new ByteArrayInputStream(ir.bytes)))    
  }
}

/*
 * Custom module interface attribute.
 */
private object IfaceAttr { def JELLY_IFACE = "jelly.iface" }

private class IfaceAttr(val bytes:Array[Byte]) 
  extends Attribute(IfaceAttr.JELLY_IFACE) {
  
  def this() = this(Array.empty)
  
  override def isUnknown = false
  
  override def read(
      cr:ClassReader,
      off:Int,
      len:Int,
      buf:Array[Char],
      codeOff:Int,
      labels:Array[Label]) = {
    val b = new Array[Byte](len)
    Array.copy(cr.b, off, b, 0, len)
    new IfaceAttr(b)
  }
  
  override def write(
      cw:ClassWriter, 
      code:Array[Byte], 
      len:Int, 
      maxStack:Int, 
      maxLocals:Int) = new ByteVector().putByteArray(bytes, 0, bytes.length)
}

// Add an interface to a class proxied by a class writer as a custom attribute
private class IfaceWriter(
    cw:ClassWriter, 
    iface:ModIface) extends ClassAdapter(cw) {

  private val bytes = ifaceBytes(iface) 
  
  override def visitEnd() {
    val attr = new IfaceAttr(ifaceBytes(iface))
    super.visitAttribute(attr)
    super.visitEnd()
  }
  
  private def ifaceBytes(iface:ModIface) = {
    val out = new ByteArrayOutputStream
    iface.serialize(new DataOutputStream(out))
    out.toByteArray()
  }
}

// Read custom interface attribute from a class
private class IfaceReader extends EmptyVisitor {
  var bytes:Array[Byte] = null
  override def visitAttribute(attr:Attribute) = attr match {
    case a:IfaceAttr => bytes = a.bytes
  }
}
