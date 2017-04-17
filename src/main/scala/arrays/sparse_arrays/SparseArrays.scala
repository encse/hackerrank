package arrays.sparse_arrays
import scala.collection.mutable

object Solution {


  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val N = sc.nextInt()

    val m = new mutable.HashMap[String, Int]()

    val seq = Array.ofDim[Int](N)
    for (i <- 0 until N) {
      val st = sc.next()
      if (m.contains(st))
        m(st) = m(st) + 1
      else
        m(st) = 1
    }
    val K = sc.nextInt()
    for (i <- 0 until K) {
      val st = sc.next()
      if (m.contains(st))
        println(m(st))
      else
        println(0)
    }
  }
}


