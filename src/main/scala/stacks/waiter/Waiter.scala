package stacks.waiter

import java.io.{File, FileInputStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object Solution {

  val allPrimes = 2 #:: Stream.from(3, 2).filter(isPrime)

  def isPrime(n: Int): Boolean =
    allPrimes.takeWhile(p => p * p <= n).forall(n % _ != 0)

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/stacks.waiter/in0.txt")))

    val sc = new java.util.Scanner(System.in)
    val Array(n, q) = sc.nextLine().split(" ").map(_.toInt)
    var aCurrent = mutable.Stack[Int](sc.nextLine().split(" ").map(_.toInt).reverse: _*)

    val primes = allPrimes.take(q).toArray

    var aNext = new mutable.Stack[Int]()
    val rgb = Array.ofDim[mutable.Stack[Int]](primes.length)

    while (aCurrent.nonEmpty) {
      val a = aCurrent.pop()
      (0 until q).find { i => a % primes(i) == 0 } match {
        case Some(i) => {
          if (rgb(i) == null)
            rgb(i) = new mutable.Stack[Int]()
          rgb(i).push(a)
        }
        case None => {
          aNext.push(a)
        }
      }
    }

    for (i <- 0 until rgb.length) {
      if(rgb(i) != null) {
        val z = if(i % 2 == 0) rgb(i) else rgb(i).reverse
        while (z.nonEmpty)
          println(z.pop())
      }
    }

    if (rgb.length % 2 == 0)
      aNext = aNext.reverse

    while (aNext.nonEmpty)
      println(aNext.pop())
  }

}

