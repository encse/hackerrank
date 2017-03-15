package balancedforest

import java.io.{File, FileInputStream, FileOutputStream, PrintStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Node(inode:Int, c:Int, edges:ListBuffer[Edge] = new ListBuffer){
  def addEdge(edge: Edge) = {
    edges.append(edge)
  }
  def degree = edges.length
}

case class Edge(inodeA:Int, inodeB:Int, var cut:(Int, Int) = null){
}

case class SplitMap(nodes: IndexedSeq[Node]) {


  private val sumC = nodes.map(node => node.c).sum

  private def otherNode(node: Node, edge: Edge) = {
    if (edge.inodeA == node.inode) nodes(edge.inodeB) else nodes(edge.inodeA)
  }

  private def otherCut(node: Node, edge: Edge) = {
    val (cutA, cutB) = edge.cut
    if (edge.inodeA == node.inode) cutB else cutA
  }

  var e = 0
  while(e < nodes.length - 1){
    for (node <- nodes if node.edges.count(edge => edge.cut == null) == 1) {
      var w = node.c
      for {
        edge <- node.edges if edge.cut != null
      } w += otherCut(node, edge)

      for (edge <- node.edges if edge.cut == null) {
        e+=1
        if (edge.inodeA == node.inode) {
          edge.cut = (w, sumC - w)
        } else {
          edge.cut = (sumC - w, w)
        }
      }
    }
  }

}

object Solution {
  private def otherNode(nodes: IndexedSeq[Node], node: Node, edge: Edge): Node = {
    if (edge.inodeA == node.inode) nodes(edge.inodeB) else nodes(edge.inodeA)
  }

  def reachable(nodes: IndexedSeq[Node], inode1: Int, inode2: Int, inode3: Int, inode4: Int): Boolean = {
    val seen = mutable.Set[Int](inode1, inode4)
    val q = new mutable.Queue[Int]
    q.enqueue(inode2)
    var ok = false
    while (q.nonEmpty && !ok) {
      val inode = q.dequeue()
      if (!seen(inode)) {
        seen.add(inode)
        if (inode == inode3) {
          ok = true
        } else {
          for (edge <- nodes(inode).edges) {
            q.enqueue(otherNode(nodes, nodes(inode), edge).inode)
          }
        }
      }
    }

    ok
  }

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/balancedforest/in1.txt")))

    val sc = new java.util.Scanner(System.in)
    val q = sc.nextInt()
    for (i <- 1 to q) {
      val n = sc.nextInt()
      sc.nextLine()
      val nodes: IndexedSeq[Node] = for ((st, idx) <- sc.nextLine().split(" ").zipWithIndex) yield Node(idx, st.toInt)
      val edges: IndexedSeq[Edge] = for (iedge <- 1 until n) yield Edge(sc.nextInt() - 1, sc.nextInt() - 1)

      for (edge <- edges) {
        nodes(edge.inodeA).addEdge(edge)
        nodes(edge.inodeB).addEdge(edge)
      }

      val s = SplitMap(nodes)
      //println(edges)
      var i = 0
      var min = Int.MaxValue
      while (i < edges.length) {
        var j = 0
        val edge12 = edges(i)
        val cutA = Math.min(edge12.cut._1, edge12.cut._2)
        val cutB = Math.max(edge12.cut._1, edge12.cut._2)
        val (inode1, inode2) = if (edge12.cut._1 < edge12.cut._2) (edge12.inodeA, edge12.inodeB) else (edge12.inodeB, edge12.inodeA)

        while (j < edges.length) {
          if(i == j && cutA == cutB){
            min = Math.min(min, cutA)
          } else if (i != j) {
            val edge34 = edges(j)

            val k1 = cutA
            var k2 = -1
            var k3 = -1

            if (reachable(nodes, inode1, inode2, edge34.inodeA, edge34.inodeB)) {
              k2 = edge34.cut._1 - cutA
              k3 = edge34.cut._2
            }
            else if(reachable(nodes, inode1, inode2, edge34.inodeB, edge34.inodeA)) {
              k2 = edge34.cut._1
              k3 = edge34.cut._2 - cutA
            }

            if (k2 != -1 && k3 != -1) {
              val ks = Array(k1, k2, k3).sorted
              if(ks(1) == ks(2)){
                min = Math.min(min, ks(1) - ks(0))
              }
            }

          }
          j += 1
        }
        i += 1
      }
      if (min == Int.MaxValue)
        println(-1)
      else
        println(min)
    }
  }

}
