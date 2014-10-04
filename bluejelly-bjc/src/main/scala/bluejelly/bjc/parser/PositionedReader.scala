/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.parser

import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.{Reader,Position,CharArrayReader,PagedSeqReader}

/**
 * A position expressed as a (row,col) pair. Waaay more efficient
 * than {@link OffsetPosition}, that iterates the source from the
 * beginning to the current position looking for end-of-line marks.
 * 
 * @author ppedemon
 */
class ExplicitPosition(
    source:CharSequence,  // The whole string so far
    rowOffset:Int,
    row:Int, 
    col:Int) extends Position {
  
  def line = row
  def column = col
  
  def lineContents = {
    var end = rowOffset
    while (end < source.length && source.charAt(end) != '\n') end = end + 1
    source.subSequence(rowOffset, end).toString
  }
  
  override def toString = line+"."+column
  override def longString = lineContents + '\n' + (" "*(col-1)) + '^'
  
  override def <(that:Position) = this.line < that.line ||
    (this.line == that.line && this.column < that.column)
}

/**
 * Object encapsulating useful constants.
 * @author ppedemon
 */
object PositionedReader {
  final val TabSize = 8
  final val EofCh = '\u001a'
}

/**
 * A reader that computes positions efficiently. In addition, it 
 * subscribes to what Haskell considers to be a newline character
 * or the width of the tab character (as describe in Section 10.3
 * of the Haskell report).
 *
 * @author ppedemon
 */
class PositionedReader(
    in:Reader[Char], 
    override val offset:Int, 
    rowOffset:Int,
    row:Int,
    col:Int) extends Reader[Char] {

    import PositionedReader._
  
    // Some convenience constructors
    def this(in:Reader[Char]) = this(in, 0, 0, 1, 1)
    def this(in:String) = this(new CharArrayReader(in.toCharArray()))
    def this(in:java.io.Reader) = 
      this(new PagedSeqReader(PagedSeq.fromReader(in)))
  
    override lazy val source = in.source
    
    def first =
      if (in.atEnd) EofCh
      else in.first match {
        case '\r' if !in.rest.atEnd && in.rest.first == '\n' => '\n'
        case c@_ => c
      }
    
    def rest = 
      if (in.atEnd) this
      else in.first match {
        case '\r' if !in.rest.atEnd && in.rest.first == '\n' => 
          new PositionedReader(in.rest.rest, offset+2, offset+2, row+1,1)
        case '\r' | '\n' | '\f' => 
          new PositionedReader(in.rest, offset+1, offset+1, row+1,1)
        case '\t' =>
          val nextCol = col + TabSize - (col-1)%TabSize
          new PositionedReader(in.rest, offset+1, rowOffset, row, nextCol)
        case _ => 
          new PositionedReader(in.rest, offset+1, rowOffset, row, col+1)
      }
    
    def pos = new ExplicitPosition(source, rowOffset, row, col)
    def atEnd = in.atEnd
}
