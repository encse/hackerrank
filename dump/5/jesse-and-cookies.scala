
import java.io.{File, FileInputStream}
import java.util


object Solution {


  def insert(heap: util.SortedMap[Int,Int], item:Int): Unit = {
    if (heap.containsKey(item)) {
      heap.put(item, heap.get(item) + 1)
    } else {
      heap.put(item, 1)
    }
  }

  def pop(heap: util.SortedMap[Int,Int]): Int = {
    val (item1) = heap.firstKey()
    if(heap.get(item1) == 1)
      heap.remove(item1)
    else
      heap.put(item1, heap.get(item1)-1)
    item1
  }

  def main(args: Array[String]) {
    //System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in0.txt")))
    val sc = new java.util.Scanner(System.in)

    val n = sc.nextInt()
    val k = sc.nextInt()
    val heap = new util.TreeMap[Int,Int]()
    var size = 0
    for (i <- 1 to n) {
      insert(heap, sc.nextInt())
      size += 1
    }

    var res = 0
    while (size >= 2 && heap.firstKey() < k){
      val item1 = pop(heap)
      val item2 = pop(heap)

      insert(heap, item1 + 2*item2)
      res +=1
      size -= 1
    }
    if(size >= 1 && heap.firstKey() < k)
      println(-1)
    else
      println(res)
  }

}

