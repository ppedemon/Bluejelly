/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.asm.test
import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

/**
 * Bluejelly assembler test suite.
 * @author ppedemon
 */
@RunWith(classOf[Suite])
@SuiteClasses(Array(
  classOf[BasicTest],
  classOf[MatchTest],
  classOf[PolyTest],
  classOf[ExtrasTest]))
class AsmTestSuite {}
