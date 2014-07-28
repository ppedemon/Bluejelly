/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.asm.test

import scala.collection.mutable.MutableList
import scala.math.BigInt

import org.scalatest._

/**
 * Extras suite: test "real-world" code in modules:
 *   Nest.jas, List.jas, Primes.jas and Fib.jas
 *   
 * @author ppedemon
 */
class ExtrasSuite extends FunSuite with AsmRunner {

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
  
  test("the assembler must generate correct code for nested evaluations") {
    check("Nest.jas", "Nest.main", 7.toString)
  }
  
  test("it must handle slightlier complex cases, such as list functions") {
    check("List.jas", Seq(
      (":List.testEnumFromInt", 1 to 10 mkString ("[",",","]")),
      (":List.filterTest", 1 to 10 filter {_ % 2 == 0} mkString ("[",",","]"))
    ))
  }
    
  test("it must generate correct code for prime number generation") {
    val ps = sieve(250) take 50 mkString ("[",",","]")
    check("Primes.jas", ":Primes.primesList", ps)
  }
  
  test("it must generate correct code for Fibonacci number generation") {
    val xs = fib(100) mkString ("[",",","]")
    check("Fib.jas", ":Fib.fibList", xs)
  }
}

