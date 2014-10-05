package bluejelly.bjc.iface.test

import bluejelly.bjc.iface.{ModIface,ModIfaceIO}
import bluejelly.bjc.TestResourceReader

import java.io.{File,FileInputStream,FileOutputStream}
import java.io.{DataInputStream,DataOutputStream}

import scala.io.Source
import org.scalatest.FunSuite

/**
 * Test module system.
 * @author ppedemon
 */
class IfaceTest extends FunSuite with TestResourceReader {
  
  private val base = new File("/iface.tests")
  
  test("Module interfaces must be serialized and loaded properly") {
    val iface = parseIface("Simple.hi")
    val file = File.createTempFile("iface-", ".hi")
    
    try {
      iface.serialize(new DataOutputStream(new FileOutputStream(file)))
      val in = new DataInputStream(new FileInputStream(file))
      val loadedIface = ModIface.load(in)
      assert(iface.toString == loadedIface.toString)
    } finally {
      file.delete
    }
  }

  test("Module interfaces must be properly saved and read from a class") {
    val inFile = new File(base,"ZipFib.class")
    val is = getClass().getResourceAsStream(inFile.getPath())
    val iface = parseIface("Simple.hi")
    val bytes = ModIfaceIO.save(iface, is)
    val outFile = File.createTempFile("ZipFib-", ".class")
    
    val out = new FileOutputStream(outFile)
    out.write(bytes)
    out.close
    
    val in = new FileInputStream(outFile)
    val loadedIface = ModIfaceIO.load(in)
    assert(iface.toString == loadedIface.toString)
    println(loadedIface)
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
