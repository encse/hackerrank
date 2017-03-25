package kittyscalc

import java.io.{File, FileInputStream}

import scala.collection.mutable

class Data(var u:Long, var uWithDist:Long, var res:Long=0, var version:Int){

}
class Data2(var nodeCount:Int){
  var version:Int = -1
  private var nodes = Array.ofDim[Node](nodeCount)
  def apply(i:Int):Node = nodes(i)

  var length = 0
  def add(node:Node) = {
    nodes(length) = node
    length +=1
  }
  def clear(): Unit ={
    length = 0
  }

}

class Node(val inode:Int, var nodeParent:Node = null) {


  var inK = -1
  var data: Data = new Data(0, 0, 0, -1)
  var data2: Data2 = null

  var edgeCount = 0
  var notSeenEdges: mutable.Set[Edge] = mutable.Set[Edge]()

  def addEdge(edge: Edge): Unit = {
    notSeenEdges.add(edge)
    edgeCount+=1
  }

  def seenEdge(edge: Edge): Unit = {
    notSeenEdges.remove(edge)
    edge.otherNode(this).notSeenEdges.remove(edge)
  }

  private var distanceFromRoot = -1

  def getDistanceFromRoot(): Int = {

    if (distanceFromRoot == -1) {
      val stack = mutable.Stack[Node]()
      var node = this

      while (node.distanceFromRoot == -1) {
        if (node.nodeParent == null) {
          node.distanceFromRoot = 0
        }
        else {
          stack.push(node)
          node = node.nodeParent
        }
      }

      var d = node.distanceFromRoot
      while (stack.nonEmpty) {
        val node = stack.pop()
        node.distanceFromRoot = d + 1
        d += 1
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
  val mod = 1000000007

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


  def data2Get(node: Node, query: Int): Data2 = {
    if (node.data2.version != query) null else node.data2
  }

  def dist(nodeA:Node, nodeB:Node) : Long = {
    var d = 0L
    var nodeAT = nodeA
    var dA = nodeAT.getDistanceFromRoot()
    var nodeBT = nodeB
    var dB = nodeBT.getDistanceFromRoot()
    while (nodeAT != nodeBT && dA > 0 && dB > 0) {
      if (dA > dB){
        nodeAT = nodeAT.nodeParent
        d+=1
        dA -=1
      } else{
        nodeBT = nodeBT.nodeParent
        d+=1
        dB -=1
      }
    }

    if(nodeAT == nodeBT)
      d
    else
      d + dA + dB
  }
  def bf(ksList: Array[Node]): Long ={
    var res = 0L
    var i=0
    while(i<ksList.length){
      var nodeA = ksList(i)
      var j=i+1
      while(j<ksList.length) {
        var nodeB = ksList(j)
        var d = dist(nodeA, nodeB)
        res = (res + d * nodeA.inode * nodeB.inode) % mod
        j+=1
      }
      i+=1
    }
    res
  }
  def solve(pq: PQueue, ksList: Array[Node], query: Int): Long = {

    var res = 0L

    pq.init(ksList)

    var qqq = 0
    while (qqq < ksList.length) {
      val node = ksList(qqq)
      node.inK = query
      qqq += 1
    }

    while (pq.hasMore()) {
      val node = pq.dequeue()
      val data = node.data
      data.u = if (node.inK == query) node.inode else 0
      data.res = 0
      data.uWithDist = 0
      data.version = query
      val v = data.u

      node.data = data

      var uSum = 0L
      var uWithDistSum = 0L
      var resSum = 0L
      val data2 = data2Get(node, query)

      if (data2 != null) {
        var j = 0
        while (j < data2.length) {
          val dataJ = data2(j).data
          uSum += dataJ.u
          uWithDistSum += dataJ.uWithDist
          resSum += dataJ.res
          j += 1
        }

        var neighbours = 0L
        var i = 0
        while (i < data2.length) {
          val dataI = data2(i).data

          neighbours +=
            2 * dataI.u * (uSum - dataI.u) +
              dataI.uWithDist * (uSum - dataI.u) +
              dataI.u * (uWithDistSum - dataI.uWithDist)
          i += 1
        }

        data.u = (data.u + uSum) % mod
        data.uWithDist = (data.uWithDist + uSum + uWithDistSum) % mod
        data.res = (data.res + v * (uSum + uWithDistSum) + resSum + (neighbours >>> 1)) % mod
      }

      if (node.nodeParent != null) {

        if (node.nodeParent.data.version != query) {
          node.nodeParent.data.version = query
          if (node.nodeParent.inK != query) {
            pq.enqueue(node.nodeParent)
          }
        }

        if (node.nodeParent.data2.version != query) {
          node.nodeParent.data2.version = query
          node.nodeParent.data2.clear()
        }
        node.nodeParent.data2.add(node)

      }

      res = data.res
    }

    res
  }

  class PQueue(nodeCount:Int) {
    private var iitems: Array[Node] = _
    private var jitems: Array[Node] = Array.ofDim[Node](nodeCount)
    private var iitemsFirst:Int = _
    private var iitemsNext:Int = _ //iitems.length
    private var jitemsFirst:Int = _
    private var jitemsNext:Int = _

    def init(iitems: Array[Node]): Unit = {
      this.iitems = iitems
      scala.util.Sorting.quickSort(iitems)(Ordering.by(node => -node.getDistanceFromRoot()))

      iitemsFirst = 0
      iitemsNext = iitems.length
      jitemsFirst = 0
      jitemsNext = 0

    }

    def hasMore(): Boolean = iitemsNext - iitemsFirst > 0 || jitemsNext - jitemsFirst > 0

    def enqueue(item: Node): Unit = {
      jitems(jitemsNext) = item
      jitemsNext += 1

    }

    def dequeue(): Node = {
      val ks1NonEmpty = iitemsNext - iitemsFirst > 0
      val ks2NonEmpty = jitemsNext - jitemsFirst > 0
      if (ks1NonEmpty && ks2NonEmpty) {
        if (iitems(iitemsFirst).getDistanceFromRoot() > jitems(jitemsFirst).getDistanceFromRoot()) {
          iitemsFirst += 1
          iitems(iitemsFirst - 1)
        } else {
          jitemsFirst += 1
          jitems(jitemsFirst - 1)
        }
      } else if (ks1NonEmpty) {
        iitemsFirst += 1
        iitems(iitemsFirst - 1)
      } else {
        jitemsFirst += 1
        jitems(jitemsFirst - 1)
      }
    }
  }

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/kittyscalc/in20.txt")))
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

    inode = 0
    while (inode <= n) {
      nodes(inode).data2 = new Data2(nodes(inode).edgeCount)
      inode += 1
    }

    println((System.currentTimeMillis() - t1) / 1000.0)
    buildParentGraph(nodes)
    println((System.currentTimeMillis() - t1) / 1000.0)
    val pq = new PQueue(nodes.length)

    var i = 1
    var sb = new mutable.StringBuilder()
    while (i <= q) {
      val c = readLine().toInt
      val k = readLine().split(" ").map(st => nodes(st.toInt))
      val ress = solve(pq, k, i)
      sb.append(ress)
      sb.append("\n")
      i += 1
    }

    println(sb)
    println((System.currentTimeMillis() - t1) / 1000.0)
  }

}

