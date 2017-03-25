package kittyscalc

import java.io.{File, FileInputStream}

import scala.collection.mutable

class Data(var u:Long, var uWithDist:Long, var res:Long=0, var version:Int){

}
class Data2(var nodes: mutable.ArrayBuffer[Node], var version:Int){

}

class Node(val inode:Int, var nodeParent:Node = null) {

  var inK = -1
  var data: Data = null
  var data2: Data2 = new Data2(new mutable.ArrayBuffer[Node](), -1)

  var notSeenEdges: mutable.Set[Edge] = mutable.Set[Edge]()

  def addEdge(edge: Edge): Unit = {
    notSeenEdges.add(edge)
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


  def dataGet(node: Node, query: Int): Data = {
    if (node.data == null || node.data.version != query) null else node.data
  }
  def data2Get(node: Node, query: Int): Data2 = {
    if (node.data2 == null || node.data2.version != query) null else node.data2
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

    var kor = 0
    var kor2 = 0
    while (pq.hasMore()) {
      kor += 1
      val node = pq.dequeue()
      if (dataGet(node, query) == null) {
        kor2 += 1
        val data = new Data(if (node.inK == query) node.inode else 0, 0, 0, query)
        val v = data.u

        node.data = data

        var uSum = 0L
        var uWithDistSum = 0L
        var resSum = 0L
        val data2 = data2Get(node, query)

        if (data2 != null) {
          val nodesArray = data2.nodes.toArray
          var j = 0
          while (j < nodesArray.length) {
            val dataJ = dataGet(nodesArray(j), query)
            uSum += dataJ.u
            uWithDistSum += dataJ.uWithDist
            resSum += dataJ.res
            j+=1
          }

          var neighbours = 0L
          var i = 0
          while (i < nodesArray.length) {
            val dataI = dataGet(nodesArray(i), query)

            neighbours +=
              2 * dataI.u * (uSum - dataI.u) +
                dataI.uWithDist * (uSum - dataI.u) +
                dataI.u * (uWithDistSum - dataI.uWithDist)
            i+=1
          }

          data.u = (data.u + uSum) % mod
          data.uWithDist = (data.uWithDist + uSum + uWithDistSum) % mod
          data.res = (data.res + v * (uSum + uWithDistSum) + resSum + (neighbours >>> 1)) % mod
        }

        if (node.nodeParent != null) {
          if (dataGet(node.nodeParent, query) == null) {
            pq.enqueue(node.nodeParent)

          }
          if (node.nodeParent.data2.version != query) {
            node.nodeParent.data2.version = query
            node.nodeParent.data2.nodes.clear()
          }
          node.nodeParent.data2.nodes.append(node)
        }

        res = data.res
      }
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

    def hasMore(): Boolean = jitemsNext - jitemsFirst + iitemsNext - iitemsFirst > 1

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

  def check(kPrev:Array[Node], k: Array[Node]) = {
    scala.util.Sorting.quickSort(kPrev)(Ordering.by(node => node.inode))
    scala.util.Sorting.quickSort(k)(Ordering.by(node => node.inode))

    var iP= 0
    var i = 0
    var added = 0
    var removed = 0
    var common = 0
    while(iP < kPrev.length || i<k.length) {
      if (iP < kPrev.length && i < k.length) {
        if(kPrev(iP).inode < k(i).inode){
          removed+=1
          iP +=1
        } else if(k(i).inode < kPrev(iP).inode){
          added +=1
          i +=1
        } else {
          iP+=1
          i+=1
          common+=1
        }

      } else if (iP < kPrev.length) {
        removed+=1
        iP +=1
      }
      else {
        added +=1
        i +=1
      }
    }
    //println(s"length: ${k.length}, added: $added, removed: $removed, common: ${100*common/k.length}")
  }

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/kittyscalc/in17.txt")))
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
    println(nodes.map(node => node.getDistanceFromRoot()).max)
    println(q)
    println((System.currentTimeMillis() - t1) / 1000.0)
    val pq = new PQueue(nodes.length)

    var kprev:Array[Node] = null
    var i = 1
    while (i <= q) {
      val c = readLine().toInt
      val k = readLine().split(" ").map(st => nodes(st.toInt))
      val ress = solve(pq, k, i)
      println(ress)
      i += 1
      kprev = k
    }

    println((System.currentTimeMillis() - t1) / 1000.0)
  }

}

