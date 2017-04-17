package stacks.poisonous_plants

import java.io.{File, FileInputStream}

import scala.collection.mutable


object Solution {

  def solve(as: Array[Int]): Int = {
    val diesOnDay = Array.fill[Int](as.length)(-1)

    var i = 1
    var minHeight = Int.MaxValue
    var res = 0

    while (i < as.length) {
      minHeight = math.min(minHeight, as(i - 1))

      if (minHeight < as(i)) {
        var j = i - 1
        var dies = 0

        while (as(j) >= as(i)) {
          dies = math.max(dies, diesOnDay(j))
          j -= 1
        }

        dies += 1
        diesOnDay(i) = dies
        if (dies > res)
          res = dies
      }

      i += 1
    }

    res
  }

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in23.txt")))
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
