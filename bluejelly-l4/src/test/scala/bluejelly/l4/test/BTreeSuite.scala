/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test

import org.scalatest._

/**
 * Test our binary tree test functions.
 * @author ppedemon
 */
class BTreeSuite extends FunSuite with BeforeAndAfterEach with L4Runner {

  override def beforeEach() {
    compile("List.l4")
  }
  
  test("in order traversal of a BST must list elements in ascending order") {
    val xs = 0 to 9 mkString ("[",",","]")
    check("BTree.l4", Seq(
      (":BTree.testInOrderR", xs),
      (":BTree.testInOrderR", xs)
    ))
  }
  
  test("depth of test BST must be 5") {
    check("BTree.l4", "BTree.testDepth", "5")
  }
  
  test("contrived code on BST must compile correctly") {
    check("BTree.l4", ":BTree.torture", 
        0 +: ((2 to 10) ++ List(4,0,1,7,6,3,5,2,9,8)) mkString("[",",","]"))
  }
}
