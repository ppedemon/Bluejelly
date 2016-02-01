/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc.static.test

import bluejelly.bjc.Bjc
import bluejelly.bjc.TestResourceReader
import bluejelly.bjc.common.Name.{idName,tcName}
import bluejelly.bjc.core._
import bluejelly.bjc.static._

import bluejelly.bjc.core.test.TestLoader
import bluejelly.bjc.iface.test.ModIfaceParser

import java.io.{File,StringReader,PrintWriter}

import org.scalatest.FunSuite

/**
 * Test import chasing.
 * @author ppedemon
 */
class ImportTest extends FunSuite with TestResourceReader {

  private val base = new File("/import.tests")
  
  private def doImport(
      modTxt:String, 
      f:(Bjc,NameTable) => Unit, 
      shouldFail:Boolean = false) = {
    val unnamed = "<unnamed>"
    val bjc = new Bjc(unnamed, new TestLoader(base))
    val result = bjc.parse(new StringReader(modTxt))
    result match {
      case None => 
        bjc.dumpErrors
        fail("Test failed")
      case Some(mod) =>
        val bjcEnv = BjcEnv(mod.name, bjc.bjcErrs, bjc.loader)
        val nameTab = bjc.chaseImports(bjcEnv, mod)
        if (bjc.hasErrors) {
          bjc.dumpErrors
          if (shouldFail) fail("Test failed")
        } else {
          f(bjc, nameTab)
        }
    }
  }

  // ---------------------------------------------------------------------
  // `Normal' imports
  // ---------------------------------------------------------------------

  test("Import chaser must handle vanilla imports") {
    doImport("import A", (_,nameTab) => {
      assert(nameTab.hasName(tcName('D)))
      assert(nameTab.hasName(idName('C)))
      assert(nameTab.hasName(idName('D)))
      assert(nameTab.hasName(idName('op1)))
      assert(nameTab.hasName(idName('op2)))
      assert(!nameTab.hasName(tcName('Arrow)))
      assert(nameTab.hasName(idName('arr)))
      assert(nameTab.hasName(tcName('Color)))
      assert(nameTab.hasName(idName('Red)))
      assert(nameTab.hasName(idName('Black)))
    })
  }

  test("Import chaser must handle moot imports") {
    doImport("import A()", (_,nameTab) => {
      assert(!nameTab.hasName(tcName('D)))
      assert(!nameTab.hasName(idName('C)))
      assert(!nameTab.hasName(idName('D)))
      assert(!nameTab.hasName(idName('op1)))
      assert(!nameTab.hasName(idName('op2)))
      assert(!nameTab.hasName(tcName('Arrow)))
      assert(!nameTab.hasName(idName('arr)))
      assert(!nameTab.hasName(tcName('Color)))
      assert(!nameTab.hasName(idName('Red)))
      assert(!nameTab.hasName(idName('Black)))
    })
  }

  test("Importing non-existing modules must fail") {
    doImport("import X", (bjc,_) => assert(bjc.hasErrors))
  }

  test("Importing non-existing stuff must fail") {
    doImport("import A(X)", (bjc,_) => assert(bjc.hasErrors))
  }

  test("Imports of a whole type constructor must read it and its children") {
    doImport("import A(D(..))", (_,nameTab) => {
      assert(nameTab.hasName(tcName('D)))
      assert(nameTab.hasName(idName('D)))
      assert(nameTab.hasName(idName('C)))
    })    
  }

  test("Imports of a type constructor must read the it without its children") {
    doImport("import A(C)", (_,nameTab) => {
      assert(nameTab.hasName(tcName('C)))
      assert(!nameTab.hasName(idName('op1)))
      assert(!nameTab.hasName(idName('op2)))
    })
  }

  test("Partial imports of a type constructor must read the precise components") {
    doImport("import A(D(C,D),C(op1))", (_,nameTab) => {
      assert(nameTab.hasName(tcName('C)))
      assert(nameTab.hasName(tcName('D)))
      assert(nameTab.hasName(idName('C)))
      assert(nameTab.hasName(idName('D)))
      assert(nameTab.hasName(idName('op1)))
      assert(!nameTab.hasName(idName('op2)))
    })
  }

  test("Partial imports of non-existing components must fail") {
    doImport("import A(D(D,C,Z),C(op1))", (bjc,_) => assert(bjc.hasErrors))
  }

  test("Importing subordinate ids must work if they are exported as children") {
    doImport("import A(op1,op2)", (_,nameTab) => {
      assert(nameTab.hasName(idName('op1)))
      assert(nameTab.hasName(idName('op2)))
      assert(!nameTab.hasName(tcName('C)))
      assert(!nameTab.hasName(idName('C)))
    })
  }

  test("Importing subordinate ids must work when exported as top level entities") {
    doImport("import A(arr,id)", (_,nameTab) => {
      assert(nameTab.hasName(idName('arr)))
      assert(nameTab.hasName(idName('id)))
      assert(!nameTab.hasName(tcName('Arrow)))
    })  
  }

  test("Importing entities not fully exported must work") {
    doImport("import A(Point)", (_,nameTab) => {
      assert(nameTab.hasName(tcName('Point)))
      assert(!nameTab.hasName(idName('P)))
    })
  }

  test("Fully importing entities not fully exported must work") {
    doImport("import A(Point(..))", (_,nameTab) => {
      assert(nameTab.hasName(tcName('Point)))
      assert(!nameTab.hasName(idName('P)))
    })
  }

  test("Importing entities not fully exported using empty children lists must work") {
    doImport("import A(Point())", (_,nameTab) => {
      assert(nameTab.hasName(tcName('Point)))
      assert(!nameTab.hasName(idName('P)))
    })
  }

  test("Importing non-visible children must fail") {
    doImport("import A(Point(P))", (bjc,_) => assert(bjc.hasErrors))
  }

  // ---------------------------------------------------------------------
  // Hiding imports
  // ---------------------------------------------------------------------

  test("Hiding nothing must import everything") {
    doImport("import A hiding()", (_,nameTab) => {
      assert(nameTab.hasName(tcName('D)))
      assert(nameTab.hasName(idName('C)))
      assert(nameTab.hasName(idName('D)))
      assert(nameTab.hasName(idName('op1)))
      assert(nameTab.hasName(idName('op2)))
      assert(!nameTab.hasName(tcName('Arrow)))
      assert(nameTab.hasName(idName('arr)))
      assert(nameTab.hasName(tcName('Color)))
      assert(nameTab.hasName(idName('Red)))
      assert(nameTab.hasName(idName('Black)))
    })
  }

  test("Hiding whole type constructors and their children must work correctly") {
    doImport("import A hiding(D(..))", (_,nameTab) => {
      assert(!nameTab.hasName(tcName('D)))
      assert(!nameTab.hasName(idName('C)))
      assert(!nameTab.hasName(idName('D)))
      assert(nameTab.hasName(tcName('C)))
      assert(nameTab.hasName(idName('op1)))
      assert(nameTab.hasName(idName('op2)))
      assert(nameTab.hasName(idName('arr)))
      assert(nameTab.hasName(tcName('Color)))
      assert(nameTab.hasName(idName('Red)))
      assert(nameTab.hasName(idName('Black)))
      assert(nameTab.hasName(idName('id)))
    })
  }

  test("Hiding whole type constructors and individual data cons must work correctly") {
    doImport("import A hiding(C,C(..))", (_,nameTab) => {
      assert(nameTab.hasName(tcName('D)))
      assert(!nameTab.hasName(idName('C)))
      assert(nameTab.hasName(idName('D)))
      assert(!nameTab.hasName(tcName('C)))
      assert(!nameTab.hasName(idName('op1)))
      assert(!nameTab.hasName(idName('op2)))
    })
  }

  test("Hiding whole type constructors by enumerating children must remove the whole export") {
    doImport("import A hiding(D(D,C))", (_,nameTab) => {
      assert(!nameTab.hasName(tcName('D)))
      assert(!nameTab.hasName(idName('C)))
      assert(!nameTab.hasName(idName('D)))
      assert(nameTab.hasName(tcName('C)))
      assert(nameTab.hasName(idName('op1)))
      assert(nameTab.hasName(idName('op2)))
    })
  }

  test("Hiding part of constructors by enumerating some children must work") {
    doImport("import A hiding(C(op1))", (_,nameTab) => {
      assert(!nameTab.hasName(tcName('C)))
      assert(!nameTab.hasName(idName('op1)))
      assert(nameTab.hasName(idName('op2)))
    })
  }

  test("Hiding part of constructors and individual data cons must work") {
    doImport("import A hiding(C(op1),C,D)", (_,nameTab) => {
      assert(!nameTab.hasName(tcName('C)))
      assert(!nameTab.hasName(idName('C)))
      assert(!nameTab.hasName(idName('op1)))
      assert(nameTab.hasName(idName('op2)))
      assert(!nameTab.hasName(tcName('D)))
      assert(!nameTab.hasName(idName('D)))
    })
  }

  test("Hiding individual type constructors and data cons must work") {
    doImport("import A hiding(C,C,D)", (_,nameTab) => {
      assert(!nameTab.hasName(tcName('C)))
      assert(!nameTab.hasName(idName('C)))
      assert(!nameTab.hasName(tcName('D)))
      assert(!nameTab.hasName(idName('D)))
      assert(nameTab.hasName(idName('op1)))
      assert(nameTab.hasName(idName('op2)))
    })
  }

  test("Vanilla id hiding must remove the corresponding ids") {
    doImport("import A hiding(id)", (_,nameTab) => {
      assert(!nameTab.hasName(idName('id)))
      assert(nameTab.hasName(tcName('C)))
      assert(nameTab.hasName(idName('C)))
      assert(nameTab.hasName(tcName('D)))
      assert(nameTab.hasName(idName('D)))
      assert(nameTab.hasName(idName('op1)))
      assert(nameTab.hasName(idName('op2)))
    })
  }

  test("Hiding children ids must work") {
    doImport("import A hiding(op1,op2,Red)", (_,nameTab) => {
      assert(nameTab.hasName(idName('id)))
      assert(nameTab.hasName(tcName('C)))
      assert(nameTab.hasName(idName('C)))
      assert(nameTab.hasName(tcName('D)))
      assert(nameTab.hasName(idName('D)))
      assert(!nameTab.hasName(idName('op1)))
      assert(!nameTab.hasName(idName('op2)))
      assert(!nameTab.hasName(idName('Red)))
    })
  }

  test("Hiding children ids exported as standalone entities must work") {
    doImport("import A hiding(arr)", (_,nameTab) => {
      assert(!nameTab.hasName(idName('arr)))
      assert(nameTab.hasName(idName('id)))
      assert(nameTab.hasName(tcName('C)))
      assert(nameTab.hasName(idName('C)))
      assert(nameTab.hasName(tcName('D)))
      assert(nameTab.hasName(idName('D)))
      assert(nameTab.hasName(idName('op1)))
      assert(nameTab.hasName(idName('op2)))
    })
  }

  test("Hiding stuff at random must work") {
    doImport("import A hiding(arr,C,D,op1,Red,Black,Color)", (_,nameTab) => {
      assert(!nameTab.hasName(idName('arr)))
      assert(nameTab.hasName(idName('id)))
      assert(!nameTab.hasName(tcName('C)))
      assert(!nameTab.hasName(idName('C)))
      assert(!nameTab.hasName(tcName('D)))
      assert(!nameTab.hasName(idName('D)))
      assert(!nameTab.hasName(idName('op1)))
      assert(nameTab.hasName(idName('op2)))
      assert(!nameTab.hasName(tcName('Color)))
      assert(!nameTab.hasName(idName('Red)))
      assert(!nameTab.hasName(idName('Black)))
    })
  }

  test("Hiding stuff and then adding it must cancel each other") {
    doImport("import A hiding(arr,C,D); import qualified A as Z(C(..),D(D))", (_,nameTab) => {
      assert(!nameTab.hasName(idName('arr)))
      assert(nameTab.hasName(idName('id)))
      assert(nameTab.hasName(tcName('C)))
      assert(!nameTab.hasName(idName('C)))
      assert(nameTab.hasName(tcName('D)))
      assert(nameTab.hasName(idName('D)))
      assert(nameTab.hasName(idName('op1)))
      assert(nameTab.hasName(idName('op2)))
    })
  }
}
