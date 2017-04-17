package disjointset.merging_communities

import java.io.{File, FileInputStream}

import scala.collection.mutable

class Community {
  var size: Int = 1
  var parent: Community = null

  def root: Community = {
    if (parent == null)
      this
    else {
      val res = parent.root
      this.parent = res
      res
    }
  }

  def mergeWith(c: Community): Unit = {
    val r1 = root
    val r2 = c.root
    if (r1 != r2) {
      r1.size += r2.size
      r2.parent = r1
    }
  }
}

object Solution {


  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName.replace('.','/')}/in0.txt")))
    val sc = new java.util.Scanner(System.in)

    val n = sc.nextInt()
    val q = sc.nextInt()
    val communities = Array.fill[Community](n+1)(new Community)

    sc.nextLine()
    for (i <- 1 to q) {
      var line = sc.nextLine().split(" ")
      if (line(0) == "Q"){
        println(communities(line(1).toInt).root.size)
      } else {
        val c1 = communities(line(1).toInt)
        val c2 = communities(line(2).toInt)
        c1.mergeWith(c2)
      }
    }
  }

}


