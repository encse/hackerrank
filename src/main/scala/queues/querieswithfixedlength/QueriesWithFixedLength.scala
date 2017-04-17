package queues.querieswithfixedlength

import java.io.{File, FileInputStream}

import scala.collection.{mutable}


object Solution {

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in3.txt")))
    val sc = new java.util.Scanner(System.in)

    val n = sc.nextInt()
    val cQuestino = sc.nextInt()
    val as = Array.ofDim[Int](n)
    for (i <- 0 until n) {
      as(i) = sc.nextInt()
    }

    for (iq <- 0 until cQuestino) {
      val q = mutable.Queue[Int]()
      val d = sc.nextInt()
      var i = 0
      while (i < d) {
        q.enqueue(as(i))
        i += 1
      }

      var maxCur = q.max
      var min = maxCur
      while (i < n) {
        val aOld = q.dequeue()
        val aNew = as(i)
        q.enqueue(aNew)

        if(aNew > maxCur) {
          maxCur = aNew
        } else if(aOld == maxCur){
          maxCur = q.max
        }

        min = math.min(min, maxCur)
        i+=1
      }
      println(min)
    }
    println("done")
  }
}
