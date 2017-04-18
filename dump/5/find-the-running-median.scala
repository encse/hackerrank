
import java.io.{File, FileInputStream}
import scala.collection.mutable

class MinMaxMedianHeap {
  private var _m: Int = -1
  private var size = 0
  private var ltHeap = mutable.PriorityQueue[Int]()
  private var geHeap = mutable.PriorityQueue[Int]()(Ordering.by(i => -i))

  def median: Double = {
    if (size % 2 == 1) {
      _m
    } else {
      (_m + geHeap.head) / 2.0
    }
  }

  def add(item: Int): Unit = {
    size += 1
    if(size == 1 ){
      _m = item
    }
    else if (item < _m) {
      ltHeap.enqueue(item)
      while (ltHeap.size != geHeap.size && ltHeap.size +1 != geHeap.size) {
        geHeap.enqueue(_m)
        _m = ltHeap.dequeue()
      }
    } else {
      geHeap.enqueue(item)
      while (ltHeap.size != geHeap.size && ltHeap.size +1 != geHeap.size) {
        ltHeap.enqueue(_m)
        _m = geHeap.dequeue()
      }
    }
  }

}

object Solution {


  def main(args: Array[String]) {
   // System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in0.txt")))
    val sc = new java.util.Scanner(System.in)

    val n = sc.nextInt()
    val mmmHeap = new MinMaxMedianHeap()
    for (i <- 1 to n) {
      mmmHeap.add(sc.nextInt())
      println(mmmHeap.median)
    }
  }

}