/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test

import bluejelly.l4.Renamer

import org.scalatest._

/**
 * Test renamer stage. We want to check that, for every AST a,
 * renamer(a) is isomorphic to a.
 * 
 * @author ppedemon
 */
class RenamerSuite extends FunSuite with AstTest {

  private def doTest(in:String,expected:String) {
    val mi = parseMod(in)
    val me = parseMod(expected)
    val mo = Renamer.rename(mi)
    assert(utils.isoMod(me,mo))        
  }
  
  test("the renamer must reduce to the identity function if no variable shadowing") {
    val s = "module M fun f x = let y = x in y"
    doTest(s,s)
  }
  
  test("the renamer must rename shadowing let bindings") {
    val p = "module M fun f x = let x = x in x"
    val q = "module M fun f x = let y = x in y"
    doTest(p,q)
  }
  
  test("the renamer must rename shadowing nested let bindings") {
    val p = "module M fun f x = let x = let x = e in x in x"
    val q = "module M fun f x = let y = let z = e in z in y"
    doTest(p,q)
  }
  
  test("the renamer must rename shadowing recursive let bindings") {
    val p = "module M fun f x y = let rec x = y x and y = x y in X.f x y"
    val q = "module M fun f x y = let rec u = v u and v = u v in X.f u v"
    doTest(p,q)
  }
  
  test("the renamer must rename shadowing variables in patterns") {
    val p = "module M data C{0,2} fun f x y = match x with C x y -> 1"
    val q = "module M data C{0,2} fun f x y = match x with C u v -> 1"
    doTest(p,q)
  }
  
  test("the renamer must rename shadowing let bindings inside matchers") {
    val p = "module M data C{0,2} fun f x y = match x with C x z -> let z = x in z"
    val q = "module M data C{0,2} fun f x y = match x with C u z -> let v = u in v"
    doTest(p,q)
  }

  test("the renamer must rename shadowing strict bindings inside matchers") {
    val p = "module M data C{0,2} fun f x y = match x with C x y -> let! y = x in y"
    val q = "module M data C{0,2} fun f x y = match x with C u v -> let! w = u in w"
    doTest(p,q)
  }

  test("the renamer must rename shadowing default patterns") {
    val p = "module M data C{0,2} fun f x y = match x with C x y -> let! y = x in y | y -> y"
    val q = "module M data C{0,2} fun f x y = match x with C u v -> let! w = u in w | h -> h"
    doTest(p,q)
  }
  
  test("the renamer must rename shadowing let bindings inside complex applications") {
    val p = "module M data C{0,2} fun f x = let x = C x x in C x (f x)"
    val q = "module M data C{0,2} fun f x = let y = C x x in C y (f y)"
  }
}

