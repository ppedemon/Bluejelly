/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test

import bluejelly.l4.Inliner
import bluejelly.l4.OccAnalysis
import bluejelly.l4.Renamer
import org.junit.Test

/**
 * Test the inliner (and tacitly, the occurrence analysis stage).
 * @author ppedemon
 */
class InlinerTest extends AstTest {

  private def doTest(in:String,expected:String) {
    val mi = parseMod(in)
    val me = parseMod(expected)
    val mo = Inliner.inline(OccAnalysis.analyze(Renamer.rename(mi)))
    assert(utils.isoMod(me,mo))
  }

  @Test def testCon() {
    val p = "module M data C{0,2} fun f x = let v = M.f x in let x = 1 in C v x"
    val q = "module M data C{0,2} fun f x = C (M.f x) 1"
    doTest(p,q)
  }
  
  @Test def testApp() {
    val p = "module M data C{0,2} fun f x = let v = M.f x in let x = 1 in M.g v x"
    val q = "module M data C{0,2} fun f x = M.g (M.f x) 1"
    doTest(p,q)
  }

  @Test def testNApp() {
    val p = "module M data C{0,2} fun f x = let v = M.f x in let x = 1 in @M.g v x"
    val q = "module M data C{0,2} fun f x = @M.g (M.f x) 1"
    doTest(p,q)
  }

  @Test def testNested() {
    val p = "module M fun f x = let v = let z = 1 in M.f z in M.f v"
    val q = "module M data C{0,2} fun f x = M.f (M.f 1)"
    doTest(p,q)
  }

  @Test def testDeadCode() {
    val p = "module M fun f x = let v = M.f x in x"
    val q = "module M data C{0,2} fun f x = x"
    doTest(p,q)
  }
  
  @Test def testMany() {
    val p = "module M data C{0,0} fun f = let v = 1 in let u = C in M.f v u v u"
    val q = "module M data C{0,0} fun f = M.f 1 C 1 C"
    doTest(p,q)
  }
  
  @Test def testInlineLetExpr() {
    val p = "module M fun f x = let v = let y = 1 in M.f x y in M.f v v"
    val q = "module M fun f x = let v = M.f x 1 in M.f v v"
    doTest(p,q)
  }

  @Test def testInlineLetBody() {
    val p = "module M fun f x = let v = let y = M.g 1 in M.f x y y in let z = 2 in M.f v v z"
    val q = "module M fun f x = let v = let y = M.g 1 in M.f x y y in M.f v v 2"
    doTest(p,q)
  }

  @Test def testInlineLet() {
    val p = "module M fun f x = let v = let y = M.g 1 in M.f x y y in let z = 2 in M.f v z"
    val q = "module M fun f x = M.f (let y = M.g 1 in M.f x y y) 2"
    doTest(p,q)
  }
  
  @Test def testTrivialEval() {
    val p = "module M data C{0,0} fun f = let! c = C in M.f c c (M.g c)"
    val q = "module M data C{0,0} fun f = M.f C C (M.g C)"
    doTest(p,q)
  }
  
  @Test def testEvalFirstUse() {
    val p = "module M fun f = let! x = let z = 1 in M.g z in M.f x"
    val q = "module M fun f = M.f (let! x = M.g 1 in x)"
    doTest(p,q)
  }
  
  @Test def testRec() {
    val p = "module M fun f x = let x = M.f x in let rec g = M.g f x and h = g 1 in f h"
    val q = "module M fun f x = let rec g = M.g f (M.f x) and h = g 1 in f h"
    doTest(p,q)
  }

  @Test def testMatch1() {
    val p = "module M fun f y = let! x = 1 in match x with x -> y"
    val q = "module M fun f y = let! x = 1 in match z with z -> y"
    doTest(p,q)
  }

  @Test def testMatch2() {
    val p = "module M fun f = let x = 1 in let u = 0 in match x with x -> M.f u u x"
    val q = "module M fun f = let! x = 1 in match x with z -> M.f 0 0 z"
    doTest(p,q)
  }
  
  @Test def testMatch3() {
    val p = "module M fun f = let x = 1 in let u = M.f 3 in match x with x -> M.f u u x"
    val q = "module M fun f = let u = M.f 3 in let! x = 1 in match x with z -> M.f u u z"
    doTest(p,q)
  }
  
  @Test def testExpandApp() {
    val p = "module M fun f x = let g = M.f x in match x with x -> g x"
    val q = "module M fun f x = match x with y -> M.f x y"
    doTest(p,q)
  }
  
  @Test def testExpandNApp() {
    val p = "module M fun f x = let g = @M.f x in match x with x -> g x"
    val q = "module M fun f x = match x with y -> M.f x y"
    doTest(p,q)
  }
  
  @Test def testAppEval() {
    val p = "module M fun f x y u v w = let x = let! x = M.f x y in x in x u v w"
    val q = "module M fun f x y u v w = let! z = M.f x y in z u v w"
    doTest(p,q)
  }

  @Test def testAppLet() {
    val p = "module M fun f = let g = let x = M.f 1 2 in M.h x x 3 in g 4 5"
    val q = "module M fun f = let g = let x = M.f 1 2 in M.h x x 3 in g 4 5"
    doTest(p,q)
  }
  
  // -------------------------------------------------------------------------
  // These used to be in InlineTest.l4
  // -------------------------------------------------------------------------
  
  @Test def testInlineCons() {
    val p = "module M data C{0,3} fun f = let x = 1 in let y = 2 in let z = 3 in C x y z"
    val q = "module M data C{0,3} fun f = C 1 2 3"
    doTest(p,q)
  }
  
  @Test def testInlineApp() {
    val p = "module M " +
    		"fun f x y = let a = @M.f x y in " +
    		"let z = bluejelly.Int.add x y in " +
    		"let w = Cons 1 Nil in @a z w"    		
    val q = "module M " +
            "fun f x y = @M.f x y (bluejelly.Int.add x y) (Cons 1 Nil)"          
    doTest(p,q)
  }
  
  @Test def testInlineLetFun() {
    val p = "module M " +
    		"fun f x y = let f = let! g = bluejelly.Int.add x (bluejelly.Int.mul y 2) in g in " +
    		"let w = bluejelly.Int.add x y in " +
    		"let z = Cons 1 Nil in f w z"
    val q = "module M " +
    		"fun f x y = let! g = bluejelly.Int.add x (bluejelly.Int.mul y 2) in " +
    		"g (bluejelly.Int.add x y) (Cons 1 Nil)"
    doTest(p,q)
  }
  
  @Test def testStrictInline() {
    val p = "module M " +
            "fun h x y = let f = let! g = e x y in bluejelly.Int.add g 1 in " +
            "let w = bluejelly.Int.add x y in " +
            "let z = Cons 1 Nil in f w z"
    val q = "module M " +
            "fun h x y = bluejelly.Int.add (let! g = e x y in g) 1 " +
            "(bluejelly.Int.add x y) (Cons 1 Nil)"
    doTest(p,q)
  }
}
