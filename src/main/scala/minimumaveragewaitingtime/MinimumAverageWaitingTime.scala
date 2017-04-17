package minimumaveragewaitingtime

import java.io.{File, FileInputStream}
import scala.collection.mutable

object Solution {


  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in1.txt")))
    val sc = new java.util.Scanner(System.in)

    val n = sc.nextInt()
    val pqT = mutable.PriorityQueue[(Long, Long)]()(Ordering.by { case (t, l) => -t })
    val pqX = mutable.PriorityQueue[(Long, Long)]()(Ordering.by { case (t, l) => -l })

    for (i <- 1 to n) {
      pqT.enqueue((sc.nextLong(), sc.nextLong()))
    }


    var wait = 0L
    var tNow = 0L
    while (pqT.nonEmpty || pqX.nonEmpty) {

      if (pqX.isEmpty){
        val (ti, li) = pqT.dequeue()
        pqX.enqueue((ti,li))
        tNow = ti
      }

      val (ti, li) = pqX.dequeue()
      if (tNow < ti) {
        tNow = ti
      }
      wait += li + tNow - ti

      tNow += li
      while(pqT.nonEmpty && pqT.head._1 <= tNow){
        val (ti, li) = pqT.dequeue()
        pqX.enqueue((ti,li))
      }

    }

    println(wait / n)
  }

}


