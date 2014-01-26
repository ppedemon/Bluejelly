package bluejelly.bjc.iface.test

import java.io.{File,FileInputStream,FileOutputStream}
import java.io.{DataInputStream,DataOutputStream}
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import bluejelly.bjc.iface.ModIface
import bluejelly.bjc.TestResourceReader
import bluejelly.bjc.iface.ModIFaceIO

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

  // For now no errors will do, later we will load from the saved class
  test("Module interface must be saved properly to a class") {
    val inFile = new File(base,"ZipFib.class")
    val in = getClass().getResourceAsStream(inFile.getPath())
    val iface = parseIface("Simple.hi")
    val bytes = ModIFaceIO.save(iface, in)
    val outFile = File.createTempFile("ZipFib-", ".class")
    val out = new FileOutputStream(outFile)
    out.write(bytes)
    out.close
    outFile.delete
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
