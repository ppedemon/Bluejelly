/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.common

import scala.annotation.tailrec

import java.io.DataInputStream
import java.io.DataOutputStream

/**
 * Trait for things that can be serialized to a data stream.
 * @author ppedemon
 */
trait Serializable {
  def serialize(out:DataOutputStream)
}

/**
 * Trait for things that can be loaded from a data stream.
 * @author ppedemon
 */
trait Loadable[T] {
  def load(in:DataInputStream):T
}

/**
 * Serialization and loading functionality for some basic types.
 * @author ppedemon
 */
object Binary {
  
  // Pimp boolean type
  class SerializableBoolean(x:Boolean) extends Serializable {
    def serialize(out:DataOutputStream) = out.writeBoolean(x)
  }
  
  implicit def booleanToSerializableBoolean(x:Boolean) =
    new SerializableBoolean(x)
  
  def loadBoolean(in:DataInputStream):Boolean = in.readBoolean()

  // Pimp tuples of serializable stuff
  class SerializableTuple[U <% Serializable, V <% Serializable](x:(U,V))
      extends Serializable {
    def serialize(out:DataOutputStream) { 
      val (u,v) = x
      u.serialize(out)
      v.serialize(out)
    }
  }
  
  implicit def tupleToSerializableTuple
    [U <% Serializable, V <% Serializable](x:(U,V)) = 
      new SerializableTuple[U,V](x)

  def loadTuple[U,V](f:DataInputStream => U, g:DataInputStream => V)
    (in:DataInputStream) = (f(in),g(in))

  // Pimp lists of serializable stuff
  class SerializableList[T <% Serializable](xs:List[T]) {
    def serialize(out:DataOutputStream) {
      out.writeInt(xs.length)
      for (x <- xs) x.serialize(out)      
    }
  }  
    
  implicit def listToSerializableList[T <% Serializable](xs:List[T]) = 
    new SerializableList(xs)
  
  def loadList[T](f:DataInputStream => T,in:DataInputStream):List[T] = {
    @tailrec def read(n:Int,xs:List[T]):List[T] = 
      if (n == 0) xs.reverse else read(n-1,f(in)::xs)
    read(in.readInt,Nil)
  }  
}
