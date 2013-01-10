/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test.comp
import org.junit.Test

/**
 * Test our binary tree test functions.
 * @author ppedemon
 */
class BTreeTest extends L4CompilerTest {

  override def setUp() {
    compile("List.l4")
  }
  
  @Test def testInOrder() {
    val xs = 0 to 9 mkString ("[",",","]")
    check("BTree.l4", Seq(
      (":BTree.testInOrderR", xs),
      (":BTree.testInOrderR", xs)
    ))
  }
  
  @Test def testDepth() {
    check("BTree.l4", "BTree.testDepth", "5")
  }
  
  @Test def testTorture() {
    check("BTree.l4", ":BTree.torture", 
        0 +: ((2 to 10) ++ List(4,0,1,7,6,3,5,2,9,8)) mkString("[",",","]"))
  }
}
