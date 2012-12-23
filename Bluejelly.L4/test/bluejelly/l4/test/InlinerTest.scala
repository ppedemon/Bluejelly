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

  def testCon() {
    val p = "module M data C{0,2} fun f x = let v = M.f x in let x = 1 in C v x"
    val q = "module M data C{0,2} fun f x = C (M.f x) 1"
    doTest(p,q)
  }
  
  def testApp() {
    val p = "module M data C{0,2} fun f x = let v = M.f x in let x = 1 in M.g v x"
    val q = "module M data C{0,2} fun f x = M.g (M.f x) 1"
    doTest(p,q)
  }

  def testNApp() {
    val p = "module M data C{0,2} fun f x = let v = M.f x in let x = 1 in @M.g v x"
    val q = "module M data C{0,2} fun f x = @M.g (M.f x) 1"
    doTest(p,q)
  }

  def testNested() {
    val p = "module M fun f x = let v = let z = 1 in M.f z in M.f v"
    val q = "module M data C{0,2} fun f x = M.f (M.f 1)"
    doTest(p,q)
  }

  def testDeadCode() {
    val p = "module M fun f x = let v = M.f x in x"
    val q = "module M data C{0,2} fun f x = x"
    doTest(p,q)
  }
  
  def testMany() {
    val p = "module M data C{0,0} fun f = let v = 1 in let u = C in M.f v u v u"
    val q = "module M data C{0,0} fun f = M.f 1 C 1 C"
    doTest(p,q)
  }
  
  def testInlineLetExpr() {
    val p = "module M fun f x = let v = let y = 1 in M.f x y in M.f v v"
    val q = "module M fun f x = let v = M.f x 1 in M.f v v"
    doTest(p,q)
  }

  def testInlineLetBody() {
    val p = "module M fun f x = let v = let y = M.g 1 in M.f x y y in let z = 2 in M.f v v z"
    val q = "module M fun f x = let v = let y = M.g 1 in M.f x y y in M.f v v 2"
    doTest(p,q)
  }

  def testInlineLet() {
    val p = "module M fun f x = let v = let y = M.g 1 in M.f x y y in let z = 2 in M.f v z"
    val q = "module M fun f x = M.f (let y = M.g 1 in M.f x y y) 2"
    doTest(p,q)
  }
  
  def testTrivialEval() {
    val p = "module M data C{0,0} fun f = let! c = C in M.f c c (M.g c)"
    val q = "module M data C{0,0} fun f = M.f C C (M.g C)"
    doTest(p,q)
  }
  
  def testEvalFirstUse() {
    val p = "module M data C{0,0} fun f = let! x = let z = 1 in M.g z in M.f x"
    val q = "module M data C{0,0} fun f = M.f (let! x = M.g 1 in x)"
    doTest(p,q)    
  }
}
