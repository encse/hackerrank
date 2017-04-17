package stacks.largestrectangle

import java.io.{File, FileInputStream}

import scala.collection.mutable


object Solution {

  def solve(as: Array[Int]): Int = {

    var s = mutable.Stack[(Int, Int)]()
    s.push((as(0), 0))

    var i = 1

    var maxR = 0
    while (i < as.length) {
      val a = as(i)

      if (a > s.top._1) {
        s.push((a, i))
      } else {
        var rolledBack = 0
        while (s.nonEmpty && a <= s.top._1) {
          val (h, j) = s.pop()
          rolledBack = i - j
          val le = rolledBack
          val r = h * le
          maxR = math.max(r, maxR)
        }

        s.push((a, i-rolledBack))
      }
      i += 1
    }

    var rolledBack = 0
    while (s.nonEmpty) {
      val (h, j) = s.pop()
      rolledBack = i - j
      val le = rolledBack
      val r = h * le
      maxR = math.max(r, maxR)
    }

    maxR
  }

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in0.txt")))
    val t1 = System.currentTimeMillis()

    val sc = new java.util.Scanner(System.in)
    var c = sc.nextInt()
    sc.nextLine()
    var a = new Array[Int](c)
    for (a_i <- 0 until c) {
      a(a_i) = sc.nextInt()
    }

    println(solve(a))
    println((System.currentTimeMillis() - t1) / 1000.0)
  }
}
