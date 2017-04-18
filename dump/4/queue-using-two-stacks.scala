
import java.io.{File, FileInputStream}

import scala.collection.mutable
object Solution {


  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)

    val n = sc.nextInt()

    val q = mutable.Queue[Int]()
    for {i <- 1 to n} {
      sc.nextInt() match {
        case 1 => q.enqueue(sc.nextInt())
        case 2 => q.dequeue()
        case 3 => println(q.head)
      }
    }
  }
}
