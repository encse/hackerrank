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
  val modulo = 1000000007
  def mod(l:Long) = if(l<modulo) l else l % modulo
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

  def solve(pq: PQueue, ksList: Array[Node], query: Int): Long = {

    var res = 0L

    pq.init(ksList)

    var qqq = 0
    while (qqq < ksList.length) {
      val node = ksList(qqq)
      node.inK = query
      qqq += 1
    }

    while (pq.nonEmpty) {
      val node = pq.dequeue()
      val data = node.data
      val data2 = if (node.data2.version == query) node.data2 else null


      data.u = if (node.inK == query) node.inode else 0
      data.res = 0
      data.uWithDist = 0

      if (data2 != null) {

        if (data.u == 0 && data2.length == 1) {
          val dataJ = data2(0).data
          data.u = dataJ.u
          data.res = dataJ.res
          data.uWithDist = mod(dataJ.u + dataJ.uWithDist)
        } else {

          var uSum = 0L
          var uWithDistSum = 0L
          var resSum = 0L

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
          if (data2.length > 1) {
            while (i < data2.length) {
              val dataI = data2(i).data

              neighbours +=
                ((dataI.u << 1) + dataI.uWithDist) * (uSum - dataI.u) +
                  dataI.u * (uWithDistSum - dataI.uWithDist)
              i += 1
            }
          }

          data.res = mod(data.res + data.u * (uSum + uWithDistSum) + resSum + (neighbours >>> 1))
          data.u = mod(data.u + uSum)
          data.uWithDist = mod(data.uWithDist + uSum + uWithDistSum)
        }
      }

      if (node.nodeParent != null && pq.nonEmpty) {
        if (node.nodeParent.inK != query) {
          if (node.nodeParent.data.version != query) {
            node.nodeParent.data.version = query
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
    private var iitemsNext:Int = _
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

    def nonEmpty: Boolean = iitemsNext - iitemsFirst > 0 || jitemsNext - jitemsFirst > 0

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

  def readInt() = {
    val st = readLine()
    var d = 0

    var ich = 0
    var ik = 0
    while(ich < st.length){
      val ch = st(ich)
      d = 10 * d + ch - '0'
      ich+=1
    }
    d
  }

  def readInts(c:Int) = {
    val st = readLine()

    var d = 0

    val res = Array.ofDim[Int](c)

    var ich = 0
    var ik = 0
    while(ich < st.length){
      val ch = st(ich)
      if(ch == ' ') {
        res(ik) = d
        ik += 1
        d = 0
      } else {
        d = 10 * d + ch - '0'
      }
      ich+=1

    }
    res(ik) = d
    res
  }

  def readNodes(nodes:Array[Node]) = {
    val st = readLine()

    var ich = 0
    var ck = 1
    var d = 0

    while(ich < st.length){
      val ch = st(ich)
      if(ch == ' ') {
        ck+=1
      }
      ich+=1

    }
    val res = Array.ofDim[Node](ck)


    ich = 0
    var ik = 0
    while(ich < st.length){
      val ch = st(ich)
      if(ch == ' ') {
        res(ik) = nodes(d)
        ik += 1
        d = 0
      } else {
        d = 10 * d + ch - '0'
      }
      ich+=1

    }
    res(ik) = nodes(d)
    res
  }

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/kittyscalc/in17.txt")))
    val t1 = System.currentTimeMillis()

    val st = readLine().split(" ").map(stI => stI.toInt)
    val n = st(0)
    val q = st(1)
    val nodes = Array.ofDim[Node](n + 1)
    var inode = 0
    while (inode <= n) {
      nodes(inode) = new Node(inode)
      inode += 1
    }
    var iedge = 1
    while (iedge <= n - 1) {
      val inodes = readInts(2)
      val edge = new Edge(iedge, nodes(inodes(0)), nodes(inodes(1)))
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
      val c = readInt()
      val k = readNodes(nodes)
      val ress = solve(pq, k, i)
      sb.append(ress)
      sb.append("\n")
      i += 1
    }

    println(sb)
    println((System.currentTimeMillis() - t1) / 1000.0)
  }

}

