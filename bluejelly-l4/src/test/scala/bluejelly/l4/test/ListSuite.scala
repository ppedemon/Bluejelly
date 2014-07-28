/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.l4.test

import org.scalatest._
import scala.collection.mutable.MutableList

/**
 * Test list functions.
 * @author ppedemon
 */
class ListSuite extends FunSuite with L4Runner {

  // For testing Primes.primesList
  private def sieve(max:Int):Array[Int] = {
    val a = (2 to max map {(true,_)}).toArray
    for (i <- 0 until a.length)
      if (a(i)._1)
        for (j <- i + a(i)._2 until a.length by a(i)._2)
          a(j) = (false,a(i)._2)
    a filter {_._1} map (_._2)
  }

  // For testing Fib.fibList
  private def fib(n:Int):MutableList[BigInt] = {
    val xs = MutableList[BigInt](0,1)
    for (i <- 2 until n) xs += xs(i-2) + xs(i-1)
    xs
  }

  test("L4 must compile correctly a long list of primes using trial division") {
    check("List.l4", ":List.testPrims", sieve(550) take 100 mkString("[",",","]"))
  }
  
  test("L4 must compile correctly a long list of Fibonacci numbers") {
    check("List.l4", ":List.testFib", fib(200) mkString("[",",","]"))
  }

  test("L4 must compile correctly the take function on lists") {
    check("List.l4", ":List.testTake", 1 to 10 mkString ("[",",","]"))
  }

  test("L4 must compile correctly the map function on lists") {
    check("List.l4", ":List.testMap", 1 to 100 map {_+1} mkString ("[",",","]"))
  }

  test("L4 must compile correctly the filter function on lists") {
    check("List.l4", ":List.testFilter", 
        1 to 100 map {_+1} filter {_%2==0} mkString ("[",",","]"))
  }
  
  test("L4 must compile correctly the append function on lists") {
    check("List.l4", ":List.testAppend", (1 to 10) ++ (1 to 20) mkString ("[",",","]"))
  }
  
  test("L4 must compile correctly the sum function on lists using folds") {
    check("List.l4", Seq(("List.testSuml","55"),("List.testSumr","55")))
  }

  test("L4 must compile correctly the reverse function on lists using fold and flip") {
    check("List.l4", ":List.testReverse", (1 to 20).reverse mkString ("[",",","]"))
  }
}
