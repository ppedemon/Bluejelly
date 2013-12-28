package bluejelly.bjc.parser.test

import java.io.File
import org.scalatest.FunSuite
import bluejelly.bjc.parser.BluejellyParser
import bluejelly.bjc.TestResourceReader

class ParserSuite extends FunSuite with TestResourceReader {

  private val base = new File("/parser.tests")
  
  private def testParser(fileName:String, name:String, verbose:Boolean = false) {
    val in = readerFor(new File(base, fileName))
    val start = System.currentTimeMillis()
    val result = BluejellyParser.phrase(BluejellyParser.program, in)
    val end = System.currentTimeMillis()
    result match {
      case err@BluejellyParser.NoSuccess(_,_) => 
        fail(err.toString)
      case BluejellyParser.Success(m,_) => 
        assert(m.name.toString == name)
        printf("Parsing time for module `%s': %.3f secs.\n", 
            name, (end-start)/1000f)
        if (verbose) println(m)
    }
  }

  test("Parser must handle primitive declarations") {
    testParser("Prims.hs", "Main")
  }

  test("Parser must handle dead simple Haskell code") {
    testParser("Simple.hs", "Main")
  }

  test("Parser must handle real Haskell code") {
    testParser("Pie.hs", "Main")
  }

  test("Parser must parse the Unix.hs demo module") {
    testParser("Unix.hs", "Unix")
  }
    
  test("Parser must handle (almost) real GHC List module") {
    testParser("List.hs", "container.List")
  }
  
  test("Parser must handle (almost) real GHC Maybe module") {
    testParser("Maybe.hs", "container.Maybe")
  }

  test("Parser must handle (almost) real GHC Monad module") {
    testParser("Monad.hs", "control.Monad")
  }
  
  test("Parser must handle Graph.hs (graph data type and algorithms)") {
    testParser("Graph.hs", "container.Graph")
  }

}
