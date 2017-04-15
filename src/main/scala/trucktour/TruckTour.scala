package trucktour

import java.io.{File, FileInputStream}

import scala.collection.mutable


object Solution {

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in1.txt")))
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
