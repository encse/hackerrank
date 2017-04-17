package trees.balanced_forest

import java.io.{File, FileInputStream, FileOutputStream, PrintStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Node(inode:Int, c:Long, edges:ListBuffer[Edge] = new ListBuffer, var edgeOut:Edge = null){
  def addEdge(edge: Edge) = {
    edges.append(edge)
  }
  def degree = edges.length
}

case class Edge(iedge:Int, inodeA:Int, inodeB:Int, var cutA:Long = -1, var cutB:Long = -1, var inodeOut:Int = -1) {
  def reverse(): Edge = Edge(iedge, inodeB, inodeA, cutB, cutA, inodeOut)

  override def equals(o: Any): Boolean = o match {
    case that: Edge => that.iedge == iedge
    case _ => false
  }

  override def hashCode: Int = iedge.hashCode

  def hasCut = cutA >= 0

}

case class SplitMap(nodes: IndexedSeq[Node]) {

  var qMap = mutable.Map[Long, mutable.Set[Int]]()
  def lookup(cutA: Long):Iterable[Int] = {

    qMap.getOrElse(cutA, Nil)

  }

  private val sumC = nodes.map(node => node.c).sum

  private def otherNode(node: Node, edge: Edge) = {
    if (edge.inodeA == node.inode) nodes(edge.inodeB) else nodes(edge.inodeA)
  }

  private def otherCut(node: Node, edge: Edge) = {
    if (edge.inodeA == node.inode) edge.cutB else edge.cutA
  }

  val allEdges = (for(node <- nodes; edge <- node.edges) yield edge).toSet

  var e = 0
  while(e < nodes.length - 1){
    for (node <- nodes if node.edges.count(edge => !edge.hasCut) == 1) {
      var w = node.c
      for {
        edge <- node.edges if edge.hasCut
      } w += otherCut(node, edge)

      for (edge <- node.edges if !edge.hasCut) {
        node.edgeOut = edge
        edge.inodeOut = if(edge.inodeA == node.inode) edge.inodeB else edge.inodeA
        e+=1
        if (edge.inodeA == node.inode) {
          edge.cutA = w
          edge.cutB = sumC - w
        } else {
          edge.cutA = sumC - w
          edge.cutB = w
        }
      }
    }
  }

  for(edge <- allEdges){
      val (cutA, cutB) = (edge.cutA, edge.cutB)
      val d = Math.abs(cutA - cutB)

      if (!qMap.contains(cutA))
        qMap(cutA) = mutable.Set[Int]()

      if (!qMap.contains(cutB))
        qMap(cutB) = mutable.Set[Int]()

//      if (!qMap.contains(d))
//        qMap(d) = mutable.Set[Int]()

      qMap(cutA).add(edge.iedge)
      qMap(cutB).add(edge.iedge)
   //   qMap(d).add(edge.iedge)
  }
}

object Solution {
  private def otherNode(nodes: IndexedSeq[Node], node: Node, edge: Edge): Node = {
    if (edge.inodeA == node.inode) nodes(edge.inodeB) else nodes(edge.inodeA)
  }

  def orient(nodes: IndexedSeq[Node], edge1: Edge, edge2: Edge): (Edge, Edge) = {
    def next(edge: Edge): (Edge, Edge) = {
      val inodeNext = edge.inodeOut
      val nodeNext = nodes(inodeNext)
      val edgeNext = nodeNext.edgeOut

      if (edgeNext == null) {
        (edge, edge)
      } else if (edgeNext.inodeA == edge.inodeB) {
        (edge, edgeNext)
      } else if (edgeNext.inodeB == edge.inodeB) {
        (edge, edgeNext.reverse())
      } else if (edgeNext.inodeA == edge.inodeA) {
        (edge.reverse(), edgeNext)
      } else if (edgeNext.inodeB == edge.inodeA) {
        (edge.reverse(), edgeNext.reverse())
      } else
        ???
    }

    if (edge1 == edge2) {
      (edge1, edge2.reverse())
    } else if (edge1.inodeA == edge2.inodeA) {
      (edge1.reverse(), edge2)
    } else if (edge1.inodeA == edge2.inodeB) {
      (edge1.reverse(), edge2.reverse())
    } else if (edge1.inodeB == edge2.inodeA) {
      (edge1, edge2)
    } else if (edge1.inodeB == edge2.inodeB) {
      (edge1, edge2.reverse())
    } else {
      val (edge1M, edgeNext1) = next(edge1)
      val (edge2M, edgeNext2) = next(edge2)

      val (oriented1, oriented2) = orient(nodes, edgeNext1, edgeNext2)
      val res1 = if (oriented1.inodeA == edgeNext1.inodeA) edge1M else edge1M.reverse()
      val res2 = if (oriented2.inodeA == edgeNext2.inodeA) edge2M else edge2M.reverse()
      (res1, res2)
    }

  }


  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/trees.balancedforest/in6.txt")))

    val sc = new java.util.Scanner(System.in)
    val q = sc.nextInt()
    val t1 = System.currentTimeMillis()
    for (i <- 1 to q) {
      val n = sc.nextInt()
      sc.nextLine()
      val nodes: IndexedSeq[Node] = for ((st, idx) <- sc.nextLine().split(" ").zipWithIndex) yield Node(idx, st.toInt)
      val edges: IndexedSeq[Edge] = for (iedge <- 1 until n) yield Edge(iedge - 1, sc.nextInt() - 1, sc.nextInt() - 1)

      for (edge <- edges) {
        nodes(edge.inodeA).addEdge(edge)
        nodes(edge.inodeB).addEdge(edge)
      }

      val s = SplitMap(nodes)
      var i = 0
      var min = Long.MaxValue

      while (i < edges.length) {
        var j = 0
        val edge12 = if (edges(i).cutA > edges(i).cutB) edges(i).reverse() else edges(i)
        if (edge12.cutA == edge12.cutB) {
          min = Math.min(min, edge12.cutA)
        } else if (2 * edge12.cutA <= edge12.cutB && edge12.cutB % 2 == 0) {

          // akkor ot bovitjuk, es a cutB-t kettevagjuk
          for (j <- s.lookup(edge12.cutB / 2)) {

            if (i != j) {
              val edge34 = edges(j)

              val (orientedEdge12, orientedEdge34) = orient(nodes, edge12, edge34)
              //ne forduljon meg
              if (orientedEdge12.inodeA == edge12.inodeA) {
                val k1 = edge12.cutA
                val k2 = orientedEdge34.cutA - edge12.cutA
                val k3 = orientedEdge34.cutB

                val ks = Array(k1, k2, k3).sorted
                if (ks(1) == ks(2)) {
                  min = Math.min(min, ks(1) - ks(0))
                }

              }

            }
          }

        } else if (2 * edge12.cutA > edge12.cutB) {
          // ot beken hagyjuk, es maradekot vagjuk kette egy cutA meretu es egy cutB - cutA meretu darabra
          for (j <- s.lookup(edge12.cutA)) {

            if (i != j) {
              val edge34 = edges(j)

              val (orientedEdge12, orientedEdge34) = orient(nodes, edge12, edge34)
              //ne forduljon meg
              if (orientedEdge12.inodeA == edge12.inodeA) {
                val k1 = edge12.cutA
                val k2 = edge12.cutA
                val k3 = edge12.cutB - edge12.cutA

                val ks = Array(k1, k2, k3).sorted
                if (ks(1) == ks(2)) {
                  min = Math.min(min, ks(1) - ks(0))
                }

              }

            }
          }
        }

        i += 1
      }
      if (min == Long.MaxValue)
        println(-1)
      else
        println(min)
    }

    val t2 = System.currentTimeMillis()
    //println((t2-t1)/1000.0)
  }

}

