/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.utils

import scala.util.matching.Regex

/**
 * Strings supporting interpolation, as implemented here:
 * 
 *   http://dcsobral.blogspot.com.ar/2010/01/string-interpolation-in-scala-with.html
 *   
 * @author ppedemon
 */
object InterpolableString {
  implicit def strToInterpolableStr(s:String) = new {
    def fill(m:Map[String,String]):String = """\$\{([^}]+)\}""".r.replaceAllIn(s, 
        (_:Regex.Match) match {
          case Regex.Groups(v) => m.getOrElse(v, "")
        })
    def fill(ps:Pair[String,String]*):String = {
      return fill(Map(ps:_*))
    }
  }
}
