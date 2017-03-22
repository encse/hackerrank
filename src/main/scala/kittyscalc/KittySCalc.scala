package kittyscalc

import java.io.{File, FileInputStream}

import scala.collection.mutable

class Data(var u:Long, var uWithDist:Long, var res:Long=0, var version:Int){

}


class Node(val inode:Int, var nodeParent:Node = null) {
  var data:Data = null
  private var edgesI: mutable.ListBuffer[Edge] = mutable.ListBuffer[Edge]()
  var notSeenEdges: mutable.Set[Edge] = mutable.Set[Edge]()

  lazy val edges = edgesI.toArray

  def addEdge(edge: Edge): Unit = {
    notSeenEdges.add(edge)
    edgesI.append(edge)
  }

  def seenEdge(edge: Edge): Unit ={
    notSeenEdges.remove(edge)
    edge.otherNode(this).notSeenEdges.remove(edge)
  }

  private var distanceFromRoot = -1
  def getDistanceFromRoot(): Int = {

    if (distanceFromRoot == -1) {
      val stack = mutable.Stack[Node]()
      var node = this

      while(node.distanceFromRoot == -1){
        if (node.nodeParent == null){
          node.distanceFromRoot = 0
        }
        else {
          stack.push(node)
          node = node.nodeParent
        }
      }

      var d = node.distanceFromRoot
      while(stack.nonEmpty){
        val node = stack.pop()
        node.distanceFromRoot = d+1
        d+=1
      }
    }

    distanceFromRoot
  }
}

class Edge(val iedge:Int, val nodeA:Node, val nodeB:Node) {

  def otherNode(node:Node): Node =
    if(node == nodeA) nodeB else nodeA
}


object Solution {

  def buildParentGraph(nodes: Seq[Node]): Unit = {

    val nodesCurrent = mutable.Queue[Node]()
    for (node <- nodes if node.notSeenEdges.size == 1)
      nodesCurrent.enqueue(node)

    while (nodesCurrent.nonEmpty) {
      val node = nodesCurrent.dequeue()
      if (node.notSeenEdges.size == 1) {
        val edge = node.notSeenEdges.head
        node.nodeParent = edge.otherNode(node)
        node.seenEdge(edge)
        if (node.nodeParent.notSeenEdges.size == 1)
          nodesCurrent.enqueue(node.nodeParent)
      }
    }
  }


  def dataGet(node: Node, query: Int): Data = {
    if (node.data == null || node.data.version != query) null else node.data
  }

  def solve(ksList: Seq[Node], query: Int): Long = {
    val mod = 1000000007
    var res = 0L
    val ks = ksList.toSet
    val ksSorted = mutable.PriorityQueue(ksList: _*)(Ordering.by(node => node.getDistanceFromRoot()))

    while (ksSorted.size > 1) {
      val node = ksSorted.dequeue()

      if (dataGet(node, query) == null) {
        val data = new Data(if (ks.contains(node)) node.inode else 0, 0, 0, query)
        val v = data.u

        node.data = data

        var uSum = 0L
        var uWithDistSum = 0L
        var resSum = 0L
        var j = 0
        while (j < node.edges.length) {
          val edgeJ = node.edges(j)
          val oJ = edgeJ.otherNode(node)
          val dataJ = dataGet(oJ, query)

          if (dataJ != null) {
            uSum += dataJ.u
            uWithDistSum += dataJ.uWithDist
            resSum += dataJ.res
          }
          j += 1
        }

        var i = 0
        var neighbours = 0L
        while (i < node.edges.length) {
          val edgeI = node.edges(i)
          val oI = edgeI.otherNode(node)
          val dataI = dataGet(oI, query)

          if (dataI != null) {

            neighbours +=
              2 * dataI.u * (uSum - dataI.u) +
                dataI.uWithDist * (uSum - dataI.u) +
                dataI.u * (uWithDistSum - dataI.uWithDist)
          }

          i += 1
        }

        data.u = (data.u + uSum) % mod
        data.uWithDist = (data.uWithDist + uSum + uWithDistSum) % mod
        data.res = (data.res + v * (uSum + uWithDistSum) + resSum + (neighbours >>> 1)) % mod

        if (node.nodeParent != null && dataGet(node.nodeParent, query) == null) {
          ksSorted.enqueue(node.nodeParent)
        }

        res = data.res
      }
    }

    res
  }


  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/kittyscalc/in10.txt")))
    val t1 = System.currentTimeMillis()

    val st = readLine().split(" ").map(stI => stI.toInt)
    val n = st(0)
    val q = st(1)
    val nodes = Array.fill[Node](n + 1)(null)
    var inode = 0
    while (inode <= n) {
      nodes(inode) = new Node(inode)
      inode += 1
    }
    var iedge = 1
    while (iedge <= n - 1) {
      val st = readLine()
      var ich = st.indexOf(" ")
      val inodeA = st.substring(0, ich).toInt
      val inodeB = st.substring(ich + 1).toInt

      val edge = new Edge(iedge, nodes(inodeA), nodes(inodeB))
      edge.nodeA.addEdge(edge)
      edge.nodeB.addEdge(edge)
      iedge += 1
    }

    println((System.currentTimeMillis() - t1) / 1000.0)
    buildParentGraph(nodes)
    println((System.currentTimeMillis() - t1) / 1000.0)
    var i = 1
    while (i <= q) {
      val c = readLine().toInt
      val k = readLine().split(" ").map(st => nodes(st.toInt))

      println(solve(k, i))
      i += 1
    }

    println((System.currentTimeMillis() - t1) / 1000.0)
  }

}

