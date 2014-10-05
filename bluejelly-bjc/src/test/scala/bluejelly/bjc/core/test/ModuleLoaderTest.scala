/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.core.test

import bluejelly.bjc.TestResourceReader
import bluejelly.bjc.common.Name
import bluejelly.bjc.core.{LoaderException,IfaceLoader,ModuleLoader,BjcEnv}
import bluejelly.bjc.iface.test.ModIfaceParser

import java.io.File

import org.scalatest.FunSuite

/**
 * Test module system.
 * @author ppedemon
 */
class ModuleLoaderTest extends FunSuite with TestResourceReader {
  
  private val base = new File("/iface.tests")
  
  /**
   * Mock interface loader: load a ModIface from a text
   * file that is parsed using the Bluejelly parser.
   */
  private object TestLoader extends IfaceLoader {
    def load(modName:Name) = try {
      val normName = modName.toString.replaceAll("\\.","/") + ".hi"
      val in = readerFor(new File(base,normName))
      val iface = ModIfaceParser.parse(in)
      iface match {
         case Left(err) => 
           throw new LoaderException(s"Error loading module: $modName")
         case Right(iface) => 
           if (iface.name != modName) 
             throw LoaderException(s"Invalid module: $modName") 
           iface
      }
    } catch {
      case e:LoaderException => 
        throw e
      case e:NullPointerException => 
        throw LoaderException(s"Module not found: $modName")
    }
  }

  test("ModuleLoader must load properly module interfaces in test mode") {
    val loader = new ModuleLoader(TestLoader)
    val env = loader.load(new BjcEnv,Name(Symbol("Simple")))
    assert(env.loadedMods.size == 2)
  }
}
