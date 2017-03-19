//package kittyscalc

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

    def qqq(restNodesNew: mutable.Set[Node], set:Iterable[Node]) = {
      set.foreach{ node =>
        if (node.data == null || node.data.version != query) {
          node.data = new Data(if (ks.contains(node)) node.inode else 0, 0, 0, query)

          val data = node.data
          val v = data.u

          var i = 0
          while(i<node.edges.length){
            val edge = node.edges(i)
            val o = edge.otherNode(node)
            val dataO = cacheGet(o, query)
            if (dataO != null) {
              data.u += dataO.u
              data.uWithDist += dataO.uWithDist + dataO.u
              data.res += 1 * v * dataO.u + dataO.uWithDist * v + dataO.res
            }
            i+=1
          }

          i = 0
          while(i<node.edges.length) {
            val edgeI = node.edges(i)
            var j = i+1
            val oI = edgeI.otherNode(node)
            val dataI = cacheGet(oI, query)
            if(dataI != null) {
              while (j < node.edges.length) {
                val edgeJ = node.edges(j)

                val oJ = edgeJ.otherNode(node)

                val dataJ = cacheGet(oJ, query)

                if (dataJ != null) {
                  data.res += 2 * dataJ.u * dataI.u + dataI.uWithDist * dataJ.u + dataJ.uWithDist * dataI.u
                }

                j += 1
              }
            }
            i +=1
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
    }

    while (ksSorted.nonEmpty || restNodes.nonEmpty) {

      var ksCurrent: Iterable[Node] = Nil
      if (ksSorted.nonEmpty && (restNodes.isEmpty || ksSorted.head.getDistanceFromRoot() == restNodes.head.getDistanceFromRoot())) {
        val d = ksSorted.head.getDistanceFromRoot()
        val (ksCurrentT, ksSortedT) = ksSorted.span(node => node.getDistanceFromRoot() == d)
        ksCurrent = ksCurrentT
        ksSorted = ksSortedT
      }

      val restNodesNew = mutable.Set[Node]()
      qqq(restNodesNew, ksCurrent)
      qqq(restNodesNew, restNodes)

      restNodes = restNodesNew
    }

    res
  }


  def main(args: Array[String]) {
    //System.setIn(new FileInputStream(new File(s"src/main/scala/kittyscalc/in10.txt")))

    val sc = new java.util.Scanner(System.in)
    val (n,q) = (sc.nextInt(), sc.nextInt())
    val nodes: IndexedSeq[Node] = (0 to n).map(i => new Node(i))
    var iedge =1
    while(iedge <= n-1){
      val edge = new Edge(iedge, nodes(sc.nextInt()), nodes(sc.nextInt()))
      edge.nodeA.addEdge(edge)
      edge.nodeB.addEdge(edge)
      iedge+=1
    }

    // val t1 = System.currentTimeMillis()
    buildParentGraph(nodes)
    //println((System.currentTimeMillis()-t1)/1000.0)
    var i =1
    while(i<=q) {
      val c = sc.nextInt()
      sc.nextLine()
      val k = sc.nextLine().split(" ").map(st => st.toInt).toSet

      println(solve(k.map(k => nodes(k)), i))
      i+=1
    }

    //val t2 = System.currentTimeMillis()
    //println((System.currentTimeMillis()-t1)/1000.0)
  }

}

