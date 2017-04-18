
import java.io.{File, FileInputStream}

import scala.collection.mutable


object Solution {

  def solve(i: Int):Int = {
    val seen = mutable.Set[Int]()
    val queue = mutable.Queue[(Int, Int)]()

    queue.enqueue((i, 0))

    def step(): Int = {
      val (e, d) = queue.dequeue()
      if (e == 0) {
        d
      } else {
        def add(j: Int): Unit = {
          if (!seen.contains(j)) {
            queue.enqueue((j, d + 1))
            seen.add(j)
          }
        }

        add(e - 1)
        var j = 2
        while (j * j <= e) {
          if (e % j == 0) {
            add(e / j)
          }
          j += 1
        }
        step()
      }
    }
    step()
  }

  def main(args: Array[String]) {
   // System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in0.txt")))
    val sc = new java.util.Scanner(System.in)

    val n = sc.nextInt()
    val as = Array.ofDim[(Int,Int)](n)
    for (i <- 0 until n) {
      as(i) = (sc.nextInt(), sc.nextInt())
    }

    var c = 0
    var i = 0
    var t = 0
    var iStart = 0
    while (c != n) {
      val (f, dNext) = as(i)

      if(t+f-dNext < 0){
          t=0
        c = 0
        iStart = i+1
        i+=1
      }
      else {
        t = t+f - dNext
        i+=1
        c += 1
      }
      i = i % n
    }
    println(iStart)

  }
}
