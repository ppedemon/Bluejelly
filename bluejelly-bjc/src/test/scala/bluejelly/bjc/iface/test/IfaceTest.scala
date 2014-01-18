package bluejelly.bjc.iface.test

import java.io.{File,FileInputStream,FileOutputStream}
import java.io.{DataInputStream,DataOutputStream}

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

import bluejelly.bjc.iface.ModIface
import bluejelly.bjc.TestResourceReader

/**
 * Test module system.
 * @author ppedemon
 */
class IfaceTest extends FunSuite with TestResourceReader with BeforeAndAfter {
  
  private val base = new File("/iface.tests")
  
  /*
  before {
     val in = readerFor(new File(base,"Simple.hi"))
     val iface = ModIfaceParser.parse(in)
     iface match {
       case Left(err) => fail(err)
       case Right(iface) => println(iface)
     }
   }
   */
  
  test("Module interface must be serialized and loaded properly") {
    val iface = parseIface("Simple.hi")
    val file = File.createTempFile("iface-", ".hi")
    
    try {
      iface.serialize(new DataOutputStream(new FileOutputStream(file)))
      val in = new DataInputStream(new FileInputStream(file))
      val loadedIface = ModIface.load(in)
      assert(iface.toString == loadedIface.toString)
      //println(loadedIface)
    } finally {
      file.delete
    }
  }

  private def parseIface(ifaceName:String) = {
     val in = readerFor(new File(base,ifaceName))
     val iface = ModIfaceParser.parse(in)
     iface match {
       case Left(err) => fail(err)
       case Right(iface) => iface
     }    
  }
  
}
