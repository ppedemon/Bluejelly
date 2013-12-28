package bluejelly.bjc.iface.test

import bluejelly.bjc.TestResourceReader

import java.io.File
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

/**
 * Test module system.
 * @author ppedemon
 */
class IfaceTest extends FunSuite with TestResourceReader with BeforeAndAfter {
  
  private val base = new File("/iface.tests")
  
  before {
     val in = readerFor(new File(base,"Simple.hi"))
     val iface = ModIfaceParser.parse(in)
     iface match {
       case Left(err) => fail(err)
       case Right(iface) => println(iface)
     }
   }
  
  test("Module interface must be parsed properly") {
    // Nothing to do for the moment
  }
}