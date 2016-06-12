package org.codingwithclarity

import scala.collection.mutable

object PrimeCalculators {

  def primeStream(stream: Stream[Int] = Stream.from(3, 2)): Stream[Int] = {
    stream.head #:: primeStream(stream.tail.filter(_ % stream.head != 0))
  }

  def primesNonRecursiveStream(end: Int): List[Int] = {
    val odds = Stream.from(3, 2).takeWhile(_ <= Math.sqrt(end).toInt)
    val composites = odds.flatMap(i => Stream.from(i * i, 2 * i).takeWhile(_ <= end))
    Stream.from(3, 2).takeWhile(_ < end).diff(composites).toList
  }

  def primesIterative(end: Int): List[Int] = {
    val primeIndices = mutable.ArrayBuffer.fill((end + 1) / 2)(1)

    val intSqrt = Math.sqrt(end).toInt
    for (i <- 3 to end by 2 if i <= intSqrt) {
      for (nonPrime <- i * i to end by 2 * i) {
        primeIndices.update(nonPrime / 2, 0)
      }
    }

    (for (i <- primeIndices.indices if primeIndices(i) == 1) yield 2 * i + 1).tail.toList
  }
}
