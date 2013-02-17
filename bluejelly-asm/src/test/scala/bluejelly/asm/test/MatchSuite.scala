/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.asm.test

import org.scalatest._

/**
 * Test match instructions. Lots of variants to test here.
 * @author ppedemon
 */
class MatchSuite extends FunSuite with AsmRunner {

  test("constructor matchers must work, whether we return in registers or not") {
    check("MatchCon.jas", Seq(
      ("MatchCon.msimple","0"),
      ("MatchCon.msimpleReg","1")
    ))
  }
  
  test("matching complex constructors must work independently of the return convention") {
    check("MatchCon.jas", Seq(
      ("MatchCon.mcomplex","3"),
      ("MatchCon.mcomplexReg","3"),
      ("MatchCon.mcomplexDef","-1"),
      ("MatchCon.mcomplexDefReg","-1")
    ))
  }
  
  test("nested matchers must work, even though this is contrived code") {
    check("MatchCon.jas", Seq(
      ("MatchCon.mnested","30"),
      ("MatchCon.mnested1","1"),
      ("MatchCon.mnestedReg","30"),
      ("MatchCon.mnested1Reg","1")
    ))
  }
  
  test("matching on floating point must work (up to precision loss)") {
    check("MatchDbl.jas", Seq(
      ("MatchDbl.matchDbl", "Other"),
      ("MatchDbl.matchPi","PI")
    ))
  }
  
  test("matching on integer constants must work in all their variants") {
    check("MatchInt.jas", Seq(
      ("MatchInt.matchInt1","42"),
      ("MatchInt.matchInt2","42"),
      ("MatchInt.matchInt3","42"),
      ("MatchInt.matchInt4","42"),
      ("MatchInt.matchInt5","-1"),
      ("MatchInt.matchInt6","-1"),
      ("MatchInt.matchInt7","3"),
      ("MatchInt.matchInt8","3"),
      ("MatchInt.matchInt9","-1"),
      ("MatchInt.matchInt10","-1"),
      ("MatchInt.matchInt11","1"),
      ("MatchInt.matchInt12","1")
    ))
  }
  
  test("sparse matches (generating a JVM lookupswitch instructions) must work") {
    check("MatchLookup.jas", Seq(
      ("MatchLookup.match0","2"),
      ("MatchLookup.match50","1"),
      ("MatchLookup.match99","3"),
      ("MatchLookup.matchDef","-1")
    ))
  }
  
  test("matching on strings must work") {
    check("MatchStr.jas", Seq(
      ("MatchStr.match","wrong!"),
      ("MatchStr.matchFailure","wrong option!"),
      ("MatchStr.matchDef","\"\u594f\nHello\ncrazy\nworld\""),
      ("MatchStr.matchHail","b"),
      ("MatchStr.matchc","\u58ff")
    ))
  }
}

