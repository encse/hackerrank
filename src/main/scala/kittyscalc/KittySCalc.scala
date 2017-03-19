package kittyscalc

import java.io.{File, FileInputStream, FileOutputStream, PrintStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Data(var u:Long, var uWithDist:Long, var res:Long=0, var version:Int){

}


class Node(val inode:Int, var nodeParent:Node = null) {
  var data:Data = null
  var edges = mutable.Set[Edge]()
  var notSeenEdges = mutable.Set[Edge]()

  def addEdge(edge: Edge) = {
    notSeenEdges.add(edge)
    edges.add(edge)
  }

  private def otherNode(nodes: IndexedSeq[Node], node: Node, edge: Edge): Node = {
    if (edge.nodeA == node) edge.nodeB else edge.nodeA
  }

  def seenEdge(nodes:IndexedSeq[Node], edge:Edge): Unit ={
    notSeenEdges.remove(edge)

    otherNode(nodes, this, edge).notSeenEdges.remove(edge)
  }

  private var distanceFromRoot = -1

  def getDistanceFromRoot(): Int = {

    if (distanceFromRoot == -1) {
      if (nodeParent == null) {
        distanceFromRoot = 0
      } else {
        distanceFromRoot = nodeParent.getDistanceFromRoot() + 1
      }
    }

    distanceFromRoot
  }
}

class Edge(val iedge:Int, val nodeA:Node, val nodeB:Node) {
  var nodeOut:Node = null

  def otherNode(node:Node) =
    if(node == nodeA) nodeB else nodeA
}


object Solution {

  def buildParentGraph(nodes: IndexedSeq[Node]): Unit = {

    var restINodes: Iterable[Node] = nodes

    var restINodesNew = mutable.Set[Node]()
    while (restINodes.nonEmpty) {
      for (node <- restINodes) {
        if (node.notSeenEdges.size == 1) {
          val edge = node.notSeenEdges.head
          node.nodeParent = edge.otherNode(node)
          node.seenEdge(nodes, edge)
          restINodesNew += node.nodeParent
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

  def solve(ks:Set[Node], query:Int):Long = {
    var e = 0
    var res = 0L
    var ksSorted = ks.toList.sortBy(node => -node.getDistanceFromRoot())
    var restNodes = mutable.Set[Node]()

    while (ksSorted.length + restNodes.size >= 1) {

      var ksCurrent: Iterable[Node] = Nil
      if (ksSorted.nonEmpty && (restNodes.isEmpty || ksSorted.head.getDistanceFromRoot() == restNodes.head.getDistanceFromRoot())) {
        val d = ksSorted.head.getDistanceFromRoot()
        val (ksCurrentT, ksSortedT) = ksSorted.span(node => node.getDistanceFromRoot() == d)
        ksCurrent = ksCurrentT
        ksSorted = ksSortedT
      }

      var restNodesNew = mutable.Set[Node]()
      for {
        set <- List(ksCurrent, restNodes)
        node <- set
      } {
        if (node.data == null || node.data.version != query) {
          node.data = new Data(if (ks.contains(node)) node.inode else 0, 0, 0, query)

          val data = node.data
          val v = data.u

          for (edge <- node.edges) {
            val o = edge.otherNode(node)
            val dataO = cacheGet(o, query)
            if (dataO != null) {
              data.u += dataO.u
              data.uWithDist += dataO.uWithDist + dataO.u
              data.res += 1 * v * dataO.u + dataO.uWithDist * v + dataO.res
            }
          }

          for {
            edgeI <- node.edges
            edgeJ <- node.edges
            if edgeI.iedge < edgeJ.iedge
          } {
            val oI = edgeI.otherNode(node)
            val oJ = edgeJ.otherNode(node)

            val dataI = cacheGet(oI, query)
            val dataJ = cacheGet(oJ, query)

            if (dataI != null && dataJ != null) {
              data.res += 2 * dataJ.u * dataI.u + dataI.uWithDist * dataJ.u + dataJ.uWithDist * dataI.u
            }
          }

          if (node.nodeParent != null) {
            restNodesNew += node.nodeParent
          }

          data.u %= 1000000007
          data.uWithDist %= 1000000007
          data.res %= 1000000007

          res = data.res
        }
      }

      restNodes = restNodesNew
    }

    res
  }


  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/kittyscalc/in8.txt")))

    val sc = new java.util.Scanner(System.in)
    val (n,q) = (sc.nextInt(), sc.nextInt())
    val nodes: IndexedSeq[Node] = (0 to n).map(i => new Node(i))
    for(iedge <- 1 to n-1) {
      val edge = new Edge(iedge, nodes(sc.nextInt()), nodes(sc.nextInt()))
      edge.nodeA.addEdge(edge)
      edge.nodeB.addEdge(edge)
    }

    val t1 = System.currentTimeMillis()
    buildParentGraph(nodes)
    println((System.currentTimeMillis()-t1)/1000.0)
    for (i <- 1 to q) {
      val c = sc.nextInt()
      sc.nextLine()
      val k = sc.nextLine().split(" ").map(st => st.toInt).toSet

      println(solve(k.map(k => nodes(k)), i))
    }

    val t2 = System.currentTimeMillis()
    println((System.currentTimeMillis()-t1)/1000.0)
  }

}

