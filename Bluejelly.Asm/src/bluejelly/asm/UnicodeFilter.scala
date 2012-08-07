/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.asm

import java.io.FilterReader
import java.io.Reader
import java.io.IOException

/**
 * Unicode filter: preprocessor decoding unicode sequences into the 
 * actual unicode character, as mandated by the JLS, Sect. 3.3.
 * 
 * @author ppedemon
 */
class UnicodeFilter(in:Reader) extends FilterReader(in) {
  var ns   = 0
  var c0   = -1
  var c1   = -1
  var eof  = false
  var even = false

  @throws(classOf[IOException])
  override def read():Int = {
    next
    if (c0 == -1) return -1
    
    if (c0 == '\\') {
      if (c1 == '\\') {
        even = !even
        return c0
      } else if (c1 == 'u') {
        if (even) {
          even = false
          return c0
        } else {
          return unicodeSeq
        }
      } else {
        even = false
        return c0
      }
    } else {
      return c0
    }
  }
    
  // Is this reader ready to provide more input?
  @throws(classOf[IOException])
  override def ready:Boolean = !eof && in.ready()

  // Read at most [len] chars into buffer [cbuf], starting at offset [off]
  @throws(classOf[IOException])
  override def read(cbuf:Array[Char], off:Int, len:Int):Int = {
    if (!this.ready()) return -1;
    for (i <- off until (off + len)) {
      val c = read()
      if (c == -1) return i - off
      cbuf(i) = c.toChar
    }
    return len - off;
  }

  // Grab next character from underlying reader
  @throws(classOf[IOException])
  private def next {
    c0 = if (c0 == -1 && c1 == -1) in.read() else c1
    c1 = in.read()
    eof = c1 == -1
  }

  // Lex a unicode sequence: \\{u}+HHHH
  @throws(classOf[IOException])
  private def unicodeSeq:Int = {
    var c = 0
    do next while (c0 == 'u')
    for (i <- 0 until 4) {
	  val v = hexValue(c0)
	  if (v != -1) {
	    c = (c << 4) + v
	  } else {
	    val msg = 
	      if (c0 == -1) "EOF in unicode escape"
	      else "Invalid unicode escape: \\u" + c.toHexString + c0.toChar
	    throw new IOException(msg)
	  }
      if (i < 3) next
    }
    c
  }

  // Get hex value from the given digit
  private def hexValue(d:Int):Int =
    if (d >= '0' && d <= '9') d & 0xf
    else if ((d >= 'a' && d <= 'f') || (d >= 'A' && d <= 'F')) (d & 0x5f) - 'A' + 10
    else -1

}