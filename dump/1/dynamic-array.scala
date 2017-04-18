import scala.collection.mutable.ArrayBuffer

object Solution {


  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in)
    val N = sc.nextInt()
    val Q = sc.nextInt()

    val seq = Array.ofDim[ArrayBuffer[Int]](N)
    for(i<- 0 until N){
      seq(i) = new ArrayBuffer[Int]()
    }

    var lastAns = 0
    for (i <- 0 until Q) {
      val (a, x, y) = (sc.nextInt(), sc.nextInt(), sc.nextInt())
      if(a == 1){
        val idx = (x ^ lastAns) % N
        seq(idx).append(y)
      }
      else {
        val idx = (x ^ lastAns) % N
        lastAns = seq(idx)(y % seq(idx).length)
        println(lastAns)
      }
    }
   
  }
}


