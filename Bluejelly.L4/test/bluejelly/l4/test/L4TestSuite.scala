/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

import bluejelly.l4.test.ast.AstTestSuite
import bluejelly.l4.test.comp.L4CompilerTestSuite

/**
 * Suite comprising all L4 tests.
 * @author ppedemon
 */
@RunWith(classOf[Suite])
@SuiteClasses(Array(classOf[AstTestSuite],classOf[L4CompilerTestSuite]))
class L4TestSuite {}
