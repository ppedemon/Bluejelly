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

import org.scalatest._

/**
 * Test the inliner (and tacitly, the occurrence analysis stage).
 * @author ppedemon
 */
class InlinerSuite extends FunSuite with AstTest {

  private def doTest(in:String,expected:String) {
    val mi = parseMod(in)
    val me = parseMod(expected)
    val mo = Inliner.inline(OccAnalysis.analyze(Renamer.rename(mi)))
    //ppr(mo)
    assert(utils.isoMod(me,mo))
  }

  test("the inliner must inline constructor application arguments") {
    val p = "module M data C{0,2} fun f x = let v = M.f x in let x = 1 in C v x"
    val q = "module M data C{0,2} fun f x = C (M.f x) 1"
    doTest(p,q)
  }
  
  test("the inliner must inline application arguments") {
    val p = "module M data C{0,2} fun f x = let v = M.f x in let x = 1 in M.g v x"
    val q = "module M data C{0,2} fun f x = M.g (M.f x) 1"
    doTest(p,q)
  }

  test("the inliner must inline partial application arguments") {
    val p = "module M data C{0,2} fun f x = let v = M.f x in let x = 1 in @M.g v x"
    val q = "module M data C{0,2} fun f x = @M.g (M.f x) 1"
    doTest(p,q)
  }

  test("the inliner must inline inside nested lets") {
    val p = "module M fun f x = let v = let z = 1 in M.f z in M.f v"
    val q = "module M data C{0,2} fun f x = M.f (M.f 1)"
    doTest(p,q)
  }

  test("the inliner must remove dead let's") {
    val p = "module M fun f x = let v = M.f x in x"
    val q = "module M data C{0,2} fun f x = x"
    doTest(p,q)
  }
  
  test("the inliner must inline trivial expressions with multiple occurrences") {
    val p = "module M data C{0,0} fun f = let v = 1 in let u = C in M.f v u v u"
    val q = "module M data C{0,0} fun f = M.f 1 C 1 C"
    doTest(p,q)
  }
  
  test("the inliner must inline applications inside let's") {
    val p = "module M fun f x = let v = let y = 1 in M.f x y in M.f v v"
    val q = "module M fun f x = let v = M.f x 1 in M.f v v"
    doTest(p,q)
  }

  test("the inliner must inline applications in a let body") {
    val p = "module M fun f x = let v = let y = M.g 1 in M.f x y y in let z = 2 in M.f v v z"
    val q = "module M fun f x = let v = let y = M.g 1 in M.f x y y in M.f v v 2"
    doTest(p,q)
  }

  test("the inliner must inline let's if they are trivial or occur once") {
    val p = "module M fun f x = let v = let y = M.g 1 in M.f x y y in let z = 2 in M.f v z"
    val q = "module M fun f x = M.f (let y = M.g 1 in M.f x y y) 2"
    doTest(p,q)
  }
  
  test("the inliner must inline eval's if they are trivial") {
    val p = "module M data C{0,0} fun f = let! c = C in M.f c c (M.g c)"
    val q = "module M data C{0,0} fun f = M.f C C (M.g C)"
    doTest(p,q)
  }
  
  test("the inliner must inline an eval if it's the first strict binding used in the eval body") {
    val p = "module M fun f = let! x = let z = 1 in M.g z in M.f x"
    val q = "module M fun f = M.f (let! x = M.g 1 in x)"
    doTest(p,q)
  }
  
  test("the inliner must work inside let rec bodies") {
    val p = "module M fun f x = let x = M.f x in let rec g = M.g f x and h = g 1 in f h"
    val q = "module M fun f x = let rec g = M.g f (M.f x) and h = g 1 in f h"
    doTest(p,q)
  }

  test("the inliner must not inline trivial evals in matchers") {
    val p = "module M fun f y = let! x = 1 in match x with x -> y"
    val q = "module M fun f y = let! x = 1 in match z with z -> y"
    doTest(p,q)
  }

  test("the inliner must inline trivial let vars in matchers") {
    val p = "module M fun f = let x = 1 in let u = 0 in match x with x -> M.f u u x"
    val q = "module M fun f = let! x = 1 in match x with z -> M.f 0 0 z"
    doTest(p,q)
  }
  
  test("the inliner must transform lets on matched vals into evals") {
    val p = "module M fun f = let x = 1 in let u = M.f 3 in match x with x -> M.f u u x"
    val q = "module M fun f = let u = M.f 3 in let! x = 1 in match x with z -> M.f u u z"
    doTest(p,q)
  }
  
  test("the inliner must expand vars aliasing partial applications") {
    val p = "module M fun f x = let g = M.f x in match x with x -> g x"
    val q = "module M fun f x = match x with y -> M.f x y"
    doTest(p,q)
  }
  
  test("the inliner must expand vars aliasing non-updatable applications") {
    val p = "module M fun f x = let g = @M.f x in match x with x -> g x"
    val q = "module M fun f x = match x with y -> M.f x y"
    doTest(p,q)
  }

  test("the inliner must keep non trivial let's aliasing functions") {
    val p = "module M fun f = let g = let x = M.f 1 2 in M.h x x 3 in g 4 5"
    val q = "module M fun f = let g = let x = M.f 1 2 in M.h x x 3 in g 4 5"
    doTest(p,q)
  }
  
  test("the inliner must inline bindings aliasing evals reducing to functions") {
    val p = "module M " +
    		"fun f x y = let f = let! g = bluejelly.Int.add x (bluejelly.Int.mul y 2) in g in " +
    		"let w = bluejelly.Int.add x y in " +
    		"let z = Cons 1 Nil in f w z"
    val q = "module M " +
    		"fun f x y = let! g = bluejelly.Int.add x (bluejelly.Int.mul y 2) in " +
    		"g (bluejelly.Int.add x y) (Cons 1 Nil)"
    doTest(p,q)
  }
  
  test("the inliner must inline strict arguments in function aliases") {
    val p = "module M " +
            "fun h x y = let f = let! g = e x y in bluejelly.Int.add g 1 in " +
            "let w = bluejelly.Int.add x y in " +
            "let z = Cons 1 Nil in f w z"
    val q = "module M " +
            "fun h x y = bluejelly.Int.add (let! g = e x y in g) 1 " +
            "(bluejelly.Int.add x y) (Cons 1 Nil)"
    doTest(p,q)
  }

  test("the inliner must keep evals when another strict binding is used first") {
    val p = "module M " +
            "fun h x y = let f = let! g = e x y in let! z = X.f 1 in bluejelly.Int.add z z g 1 in " +
            "let w = bluejelly.Int.add x y in " +
            "let z = Cons 1 Nil in f w z"
    val q = "module M " +
            "fun h x y = let f = let! g = e x y in let! z = X.f 1 in bluejelly.Int.add z z g 1 in " +
            "f (bluejelly.Int.add x y) (Cons 1 Nil)"
    doTest(p,q)
  }
  
  test("the inliner keep evals occurring multiple times or used after another strict binding") {
    val p = "module M fun noInline x y = " +
    		"  let! f = " +
    		"    let! k = id const in " +
    		"    let! n = neg 1 in " +
    		"     third n n k 1 " +
    		"  in " +
    		"  let v = bluejelly.Int.add x y in" +
    		"  let w = Cons 1 Nil in " +
    		"  f v w"
    val q = "module M fun noInline x y = " +
            "  let! f = " +
            "    let! k = id const in " +
            "    let! n = neg 1 in " +
            "    third n n k 1 " +
            "  in " +
            "  f (bluejelly.Int.add x y) (Cons 1 Nil)"
    doTest(p,q)
  }
  
  test("the inliner must handle correctly renamed bindings") {
    val p = "module M fun f x = " +
    		"  let x = Cons 1 (Cons 1 Nil) in " +
    		"  let! z = X.h (bluejelly.Int.add 1 2) in " +
    		"  match z with | Nil -> x | x -> Cons x Nil"
    val q = "module M fun f x = " +
            "  let! z = X.h (bluejelly.Int.add 1 2) in " +
            "  match z with | Nil -> Cons 1 (Cons 1 Nil) | y -> Cons y Nil"
    doTest(p,q)
  }
}

