package stacks.andorxor

import java.io.{File, FileInputStream}

import scala.collection.mutable


object Solution {
  def solve(as: Array[Int]): Int = {
    var dMax = 0
    var bs = Array.ofDim[Int](as.length)
    bs(0) = -1
    var i = 1

    while (i < as.length) {
      val a = as(i)

      if (a > as(i - 1)) {
        dMax = math.max(dMax, as(i - 1) ^ a)
        bs(i) = i - 1
      } else {
        dMax = math.max(dMax, as(i - 1) ^ a)
        var j = bs(i - 1)
        bs(i) = -1
        while (j != -1) {
          if (as(j) < a) {
            dMax = math.max(dMax, as(j) ^ a)
            bs(i) = j
            j = -1
          } else {
            j = bs(j)
          }
        }

      }

      i += 1
    }
    dMax
  }

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in16.txt")))
    val sc = new java.util.Scanner(System.in)
    var c = sc.nextInt()
    var a = new Array[Int](c)
    for (a_i <- 0 until c) {
      a(a_i) = sc.nextInt()
    }

    println(solve(a))
  }
}
