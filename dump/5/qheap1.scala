
import java.io.{File, FileInputStream}

import scala.collection.mutable


object Solution {


  def main(args: Array[String]) {
   // System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in1.txt")))
    val sc = new java.util.Scanner(System.in)

    val q = sc.nextInt()
    val heap = mutable.SortedSet[Int]()
    for (i <- 1 to q) {
      sc.nextInt() match {
        case 1 =>
          val v = sc.nextInt()
          heap.add(v)
        case 2 =>
          val v = sc.nextInt()
          heap.remove(v)
        case 3 =>
          println(heap.head)
      }
    }
  }

}
