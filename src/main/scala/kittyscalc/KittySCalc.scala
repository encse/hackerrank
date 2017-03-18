package kittyscalc

import java.io.{File, FileInputStream, FileOutputStream, PrintStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Node(inode:Int, var u:Long, var uWithDist:Long, var res:Long=0, edges:ListBuffer[Edge] = new ListBuffer){
  def addEdge(edge: Edge) = {
    edges.append(edge)
  }
  def degree = edges.length
}

case class Edge(iedge:Int, inodeA:Int, inodeB:Int, var inodeOut:Int = -1) {
  def reverse(): Edge = Edge(iedge, inodeB, inodeA, inodeOut)

  override def equals(o: Any): Boolean = o match {
    case that: Edge => that.iedge == iedge
    case _ => false
  }

  override def hashCode: Int = iedge.hashCode
}


object Solution {
  private def otherNode(nodes: IndexedSeq[Node], node: Node, edge: Edge): Node = {
    if (edge.inodeA == node.inode) nodes(edge.inodeB) else nodes(edge.inodeA)
  }


  def solve(nodes: IndexedSeq[Node], k:Set[Int]):Long = {
    var e = 0
    var res = 0L
    for (node <- nodes) {
      node.u = if (k.contains(node.inode)) node.inode else 0L
      node.uWithDist = 0
      node.res = 0

      for(edge <- node.edges){
        edge.inodeOut = -1
      }
    }


    var restINodes = nodes.map(node => node.inode).toSet
    while (restINodes.nonEmpty) {
      var restINodesNew: Set[Int] = restINodes
      for (inode <- restINodesNew; node = nodes(inode) if node.edges.count(edge => edge.inodeOut == -1) <= 1) {
        //egy uj level
        require(restINodes.contains(node.inode))

        val v = node.u
        node.u = node.u
        node.uWithDist = 0
        node.res = 0

        for (edge <- node.edges if edge.inodeOut != -1) {
          val o = otherNode(nodes, node, edge)
          node.u += o.u
          node.uWithDist += o.uWithDist + o.u
          node.res += 1 * v * o.u  + o.uWithDist * v + o.res //(v * (o.uWithDist + o.u)) + o.res
        }

        for {
          edgeI <- node.edges if edgeI.inodeOut != -1
          edgeJ <- node.edges if edgeJ.inodeOut != -1
          if edgeI.iedge < edgeJ.iedge
        } {
          val oI = otherNode(nodes, node, edgeI)
          val oJ = otherNode(nodes, node, edgeJ)
          node.res +=  2 * oJ.u * oI.u  + oI.uWithDist * oJ.u + oJ.uWithDist * oI.u
        }

       //println(node)
        for (edge <- node.edges if edge.inodeOut == -1) {
          edge.inodeOut = otherNode(nodes, node, edge).inode
        }
        node.u %= 1000000007
        node.uWithDist %= 1000000007
        node.res %= 1000000007

        res = node.res

        restINodesNew -= node.inode
      }
      restINodes = restINodesNew
    }
    res
  }

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/kittyscalc/in.txt")))

    val sc = new java.util.Scanner(System.in)
    val (n,q) = (sc.nextInt(), sc.nextInt())
    val nodes: IndexedSeq[Node] = (0 to n).map(i => Node(i, i, 0))
    val edges: IndexedSeq[Edge] = (1 to n-1).map(iedge => Edge(iedge, sc.nextInt(), sc.nextInt()))

    for(edge <- edges){
      nodes(edge.inodeA).edges.append(edge)
      nodes(edge.inodeB).edges.append(edge)
    }

    val t1 = System.currentTimeMillis()
    for (i <- 1 to q) {
      val c = sc.nextInt()
      sc.nextLine()
      val k = sc.nextLine().split(" ").map(st => st.toInt).toSet

      println(solve(nodes, k))
    }

    val t2 = System.currentTimeMillis()
    //println((t2-t1)/1000.0)
  }

}

