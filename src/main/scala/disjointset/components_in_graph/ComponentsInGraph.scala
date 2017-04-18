package disjointset.components_in_graph

import java.io.{File, FileInputStream}

class Node {
  var size: Int = 1
  var parent: Node = _

  def root: Node = {
    if (parent == null)
      this
    else {
      val res = parent.root
      this.parent = res
      res
    }
  }

  def mergeWith(c: Node): Unit = {
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
    System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName.replace('.', '/')}/in0.txt")))
    val sc = new java.util.Scanner(System.in)

    val n = sc.nextInt()
    val nodes = Array.fill[Node](2 * n + 1)(new Node)

    for (i <- 1 to n) {
      val inodeFrom = sc.nextInt()
      val inodeTo = sc.nextInt()

      nodes(inodeFrom).mergeWith(nodes(inodeTo))
    }

    var min = Int.MaxValue
    var max = Int.MinValue

    for (i <- 1 to n) {
      val s = nodes(i).root.size
      if (s > 1) {
        min = math.min(min, s)
        max = math.max(max, s)
      }
    }
    println(s"$min $max")
  }

}


