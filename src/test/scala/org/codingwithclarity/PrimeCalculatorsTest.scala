package org.codingwithclarity

import org.scalatest.{FlatSpec, ShouldMatchers}


class PrimeCalculatorsTest extends FlatSpec with ShouldMatchers {

  val primesUntil100 = List(3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)

  it should "be able to calculate the first few primes" in {

    PrimeCalculators.primeStream().takeWhile(_ < 100).toList should be(primesUntil100)
    PrimeCalculators.primesNonRecursiveStream(100) should be(primesUntil100)
    PrimeCalculators.primesIterative(100) should be(primesUntil100)
  }

  it should "calculate the performance of a stream version Vs. an iterative version" in {

    println("Timing iterative prime calculator for primes up to 1 million:")
    val iterativeAvg = (1 to 50).map(_ => timed(PrimeCalculators.primesIterative(1000000))).sum / 50
    println("Iterative took " + iterativeAvg + " ms on average over 50 runs.")

    println("-----------")

    println("Timing stream prime calculator for primes up to 1 million:")
    val streamAvg = (1 to 50).map(_ => timed(PrimeCalculators.primesNonRecursiveStream(1000000))).sum / 50
    println("Streams took " + streamAvg + " ms on average over 50 runs.")
  }

  private def timed(function: => Unit): Long = {
    val s = System.currentTimeMillis()
    function
    val e = System.currentTimeMillis()
    e - s
  }

}
