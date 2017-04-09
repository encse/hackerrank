package downtozeroii

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
    System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in0.txt")))
    val sc = new java.util.Scanner(System.in)

    val q = sc.nextInt()
    for (i <- 1 to q) {
      println(solve(sc.nextInt()))
    }
  }
}
