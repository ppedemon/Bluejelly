/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test.comp

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

/**
 * Run all tests for the L4C compiler.
 * @author ppedemon
 */
@RunWith(classOf[Suite])
@SuiteClasses(Array(classOf[BasicTest]))
class L4CompilerTestSuite {}