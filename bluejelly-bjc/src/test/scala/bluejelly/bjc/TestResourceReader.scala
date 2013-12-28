/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc

import bluejelly.utils.UnicodeFilter
import java.io.InputStreamReader
import java.io.File

/**
 * Simple trait factoring out functionaliy for reading test resources.
 * @author ppedemon
 *
 */
trait TestResourceReader {
  def readerFor(file:File) = {
    val s = getClass().getResourceAsStream(file.getPath())
    new UnicodeFilter(new InputStreamReader(s))
  }
}
