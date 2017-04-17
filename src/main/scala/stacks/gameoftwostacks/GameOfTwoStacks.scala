package stacks.gameoftwostacks

import java.io.{File, FileInputStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object Solution {

  def find(lo_ :Int, hi_ :Int, dgfGoal:Int=>Boolean):Option[Int] = {
    if(dgfGoal(lo_)){
      Some(lo_)
    } else {
      var fAny = false
      var hi = hi_
      var lo = lo_
      while (hi - lo > 1) {
        val m = (lo + hi) / 2
        if (!dgfGoal(m))
          lo = m
        else {
          hi = m
          fAny = true
        }
      }
      if (fAny || dgfGoal(hi)) Some(hi) else None
    }
  }

  def sumUp(v:Array[Long]): Unit = {
    var s = 0L
    for (i <- v.indices) {
      s += v(i)
      v(i) = s
    }
  }
  def solve(maxSum: Int, a: Array[Long], b: Array[Long]): Long = {
    val (vLonger, vShorter) = if (a.length > b.length) (a, b) else (b, a)
    sumUp(vLonger)
    sumUp(vShorter)

    var maxD = 0
    var itemsShorter = 0
    while(itemsShorter <= vShorter.length){
      val sum = if(itemsShorter == 0) 0 else vShorter(itemsShorter-1)
      if (sum <= maxSum) {
        val itemsLonger = find(0, vLonger.length - 1, iLonger => {
          sum + vLonger(iLonger) > maxSum
        }) match {
          case Some(idx) => idx
          case None => vLonger.length
        }

        maxD = math.max(maxD, itemsShorter + itemsLonger)
      }
      itemsShorter += 1
    }
    maxD
  }

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in.txt")))
    val sc = new java.util.Scanner(System.in)
    var g = sc.nextInt()
    var a0 = 0
    while (a0 < g) {
      var n = sc.nextInt()
      var m = sc.nextInt()
      var x = sc.nextInt()
      var a = new Array[Long](n)
      for (a_i <- 0 until n) {
        a(a_i) = sc.nextInt()
      }
      var b = new Array[Long](m)
      for (b_i <- 0 until m) {
        b(b_i) = sc.nextInt()
      }
      println(solve(x, a, b))
      a0 += 1
    }

  }
}
