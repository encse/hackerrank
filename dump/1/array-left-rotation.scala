import scala.collection.mutable.ArrayBuffer

object Solution {


  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val N = sc.nextInt()
    val R = sc.nextInt()


    val seq = Array.ofDim[Int](N)
    for (i <- 0 until N) {
      seq(i) = sc.nextInt()
    }

    println((seq.takeRight(N - R) ++ seq.take(R)).mkString(" "))
  }
}


