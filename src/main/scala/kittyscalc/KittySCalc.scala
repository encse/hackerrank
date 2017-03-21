package kittyscalc

import java.io.{File, FileInputStream, FileOutputStream, PrintStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Data(var u:Long, var uWithDist:Long, var res:Long=0, var version:Int){

}


class Node(val inode:Int, var nodeParent:Node = null) {
  var data:Data = null
  var edges = mutable.ListBuffer[Edge]()
  var notSeenEdges = mutable.Set[Edge]()

  def addEdge(edge: Edge) = {
    notSeenEdges.add(edge)
    edges.append(edge)
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

    val nodesCurrent = mutable.Queue(nodes.filter(node => node.notSeenEdges.size == 1): _*)

    while (nodesCurrent.nonEmpty) {
      val node = nodesCurrent.dequeue()
      if (node.notSeenEdges.size == 1) {
        val edge = node.notSeenEdges.head
        node.nodeParent = edge.otherNode(node)
        node.seenEdge(nodes, edge)
        if (node.nodeParent.notSeenEdges.size == 1)
          nodesCurrent.enqueue(node.nodeParent)
      } else {
        //require(node.notSeenEdges.size == 0)
      }
    }
  }


  def dataGet(node: Node, query: Int): Data = {
    if (node.data == null || node.data.version != query) null else node.data
  }

  def solve(ksList: Seq[Node], query: Int): Long = {
    var res = 0L
    val ks = ksList.toSet
    val ksSorted = mutable.PriorityQueue(ksList: _*)(Ordering.by(node => node.getDistanceFromRoot()))

    while (ksSorted.size > 1) {
      val node = ksSorted.dequeue()
      if (dataGet(node, query) == null) {
        node.data = new Data(if (ks.contains(node)) node.inode else 0, 0, 0, query)

        val data = node.data
        val v = data.u

        var i = 0
        while (i < node.edges.length) {
          val edgeI = node.edges(i)
          val oI = edgeI.otherNode(node)
          val dataI = dataGet(oI, query)
          if (dataI != null) {
            data.u += dataI.u
            data.uWithDist += dataI.uWithDist + dataI.u
            data.res += 1 * v * dataI.u + dataI.uWithDist * v + dataI.res

            var j = i + 1
            while (j < node.edges.length) {
              val edgeJ = node.edges(j)
              val oJ = edgeJ.otherNode(node)
              val dataJ = dataGet(oJ, query)

              if (dataJ != null) {
                data.res += 2 * dataJ.u * dataI.u + dataI.uWithDist * dataJ.u + dataJ.uWithDist * dataI.u
              }

              j += 1
            }

          }
          i += 1
        }

        if (node.nodeParent != null) {
          ksSorted.enqueue(node.nodeParent)
        }

        data.u %= 1000000007
        data.uWithDist %= 1000000007
        data.res %= 1000000007

        res = data.res
      }
    }

    res
  }


  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/kittyscalc/in20.txt")))
    val t1 = System.currentTimeMillis()

    val sc = new java.util.Scanner(System.in)
    val (n, q) = (sc.nextInt(), sc.nextInt())
    val nodes: IndexedSeq[Node] = (0 to n).map(i => new Node(i))
    var iedge = 1
    while (iedge <= n - 1) {
      val edge = new Edge(iedge, nodes(sc.nextInt()), nodes(sc.nextInt()))
      edge.nodeA.addEdge(edge)
      edge.nodeB.addEdge(edge)
      iedge += 1
    }

    println((System.currentTimeMillis() - t1) / 1000.0)
    buildParentGraph(nodes)
    println((System.currentTimeMillis() - t1) / 1000.0)
    var i = 1
    while (i <= q) {
      val c = sc.nextInt()
      sc.nextLine()
      val k = sc.nextLine().split(" ").map(st => nodes(st.toInt))

      println(solve(k, i))
      i += 1
    }

    println((System.currentTimeMillis() - t1) / 1000.0)
  }

}

