package kittyscalc

import java.io.{File, FileInputStream, FileOutputStream, PrintStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Data(var u:Long, var uWithDist:Long, var res:Long=0, var version:Int){

}


class Node(val inode:Int, var inodeParent:Int = -1) {
  var data:Data = null
  var edges = mutable.Set[Edge]()
  var notSeenEdges = mutable.Set[Edge]()

  def addEdge(edge: Edge) = {
    notSeenEdges.add(edge)
    edges.add(edge)
  }

  private def otherNode(nodes: IndexedSeq[Node], node: Node, edge: Edge): Node = {
    if (edge.inodeA == node.inode) nodes(edge.inodeB) else nodes(edge.inodeA)
  }

  def seenEdge(nodes:IndexedSeq[Node], edge:Edge): Unit ={
    notSeenEdges.remove(edge)

    otherNode(nodes, this, edge).notSeenEdges.remove(edge)
  }

  private var distanceFromRoot = -1

  def getDistanceFromRoot(nodes: IndexedSeq[Node]): Int = {

    if (distanceFromRoot == -1) {
      if (inodeParent == -1) {
        distanceFromRoot = 0
      } else {
        distanceFromRoot = nodes(inodeParent).getDistanceFromRoot(nodes) + 1
      }
    }

    distanceFromRoot
  }
}

class Edge(val iedge:Int, val inodeA:Int, val inodeB:Int) {
  var inodeOut:Int = -1

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

  def buildParentGraph(nodes: IndexedSeq[Node]): Unit = {

    var restINodes: Iterable[Node] = nodes

    var restINodesNew = mutable.Set[Node]()
    while (restINodes.nonEmpty) {
      for (node <- restINodes) {
        if (node.notSeenEdges.size == 1) {
          val edge = node.notSeenEdges.head
          node.inodeParent = otherNode(nodes, node, edge).inode
          node.seenEdge(nodes, edge)
          restINodesNew += otherNode(nodes, node, edge)
          restINodesNew -= node
        } else if (node.notSeenEdges.isEmpty) {
          restINodesNew -= node
        }

      }
      restINodes = restINodesNew
    }
  }

  def cacheGet(node:Node, query:Int): Data ={
    if (node.data == null || node.data.version != query) null else node.data
  }

  def solve(nodes: IndexedSeq[Node], ks:Set[Node], query:Int):Long = {
    var e = 0
    var res = 0L
    var ksSorted = ks.toList.sortBy(node => -node.getDistanceFromRoot(nodes))
    var restNodes = mutable.Set[Node]()

    while (ksSorted.length + restNodes.size >= 1) {

      var ksCurrent: Iterable[Node] = Nil
      if (ksSorted.nonEmpty && (restNodes.isEmpty || ksSorted.head.getDistanceFromRoot(nodes) == restNodes.head.getDistanceFromRoot(nodes))) {
        val d = ksSorted.head.getDistanceFromRoot(nodes)
        val (ksCurrentT, ksSortedT) = ksSorted.span(node => node.getDistanceFromRoot(nodes) == d)
        ksCurrent = ksCurrentT
        ksSorted = ksSortedT
      }

      var restINodesNew = mutable.Set[Node]()
      for{
        set <- List(ksCurrent, restNodes)
        node <- set
      } {
        if (node.data == null || node.data.version != query) {
          node.data = new Data(if (ks.contains(node)) node.inode else 0, 0, 0, query)

          val data = node.data
          val v = data.u

          for {edge <- node.edges
               o = otherNode(nodes, node, edge)
               if o.inode != node.inodeParent} {

            val dataO = cacheGet(o, query)
            if (dataO != null) {
              data.u += dataO.u
              data.uWithDist += dataO.uWithDist + dataO.u
              data.res += 1 * v * dataO.u + dataO.uWithDist * v + dataO.res
            }
          }

          for {
            edgeI <- node.edges
            oI = otherNode(nodes, node, edgeI)
            if oI.inode != node.inodeParent

            edgeJ <- node.edges
            oJ = otherNode(nodes, node, edgeJ)
            if oJ.inode != node.inodeParent

            if edgeI.iedge < edgeJ.iedge
          } {

            val dataI = cacheGet(oI, query)
            val dataJ = cacheGet(oJ, query)

            if (dataI != null && dataJ != null) {
              data.res += 2 * dataJ.u * dataI.u + dataI.uWithDist * dataJ.u + dataJ.uWithDist * dataI.u
            }
          }

          if (node.inodeParent != -1) {
            restINodesNew += nodes(node.inodeParent)
          }

          data.u %= 1000000007
          data.uWithDist %= 1000000007
          data.res %= 1000000007

          // if(ksCurrent.contains(inode)) {
          res = data.res
          //}
        }
      }

      restNodes = restINodesNew
    }

    res
  }


  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/kittyscalc/in8.txt")))

    val sc = new java.util.Scanner(System.in)
    val (n,q) = (sc.nextInt(), sc.nextInt())
    val nodes: IndexedSeq[Node] = (0 to n).map(i => new Node(i))
    val edges: IndexedSeq[Edge] = (1 to n-1).map(iedge => new Edge(iedge, sc.nextInt(), sc.nextInt()))

    for(edge <- edges){
      nodes(edge.inodeA).addEdge(edge)
      nodes(edge.inodeB).addEdge(edge)
    }

    val t1 = System.currentTimeMillis()
    buildParentGraph(nodes)
    println((System.currentTimeMillis()-t1)/1000.0)
    for (i <- 1 to q) {
      val c = sc.nextInt()
      sc.nextLine()
      val k = sc.nextLine().split(" ").map(st => st.toInt).toSet

      println(solve(nodes, k.map(k => nodes(k)), i))
    }

    val t2 = System.currentTimeMillis()
    println((System.currentTimeMillis()-t1)/1000.0)
  }

}

