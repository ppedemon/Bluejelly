/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test.ast

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite._

/**
 * Ast test suite.
 * @author ppedemon
 */
@RunWith(classOf[Suite])
@SuiteClasses(Array(classOf[RenamerTest],classOf[InlinerTest]))
class AstTestSuite {}