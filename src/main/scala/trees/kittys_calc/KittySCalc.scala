package trees.kittys_calc

import java.io.{File, FileInputStream}

import scala.collection.mutable


class Data2(var nodeCount:Int){


}

class Node(val inode:Int, var nodeParent:Node = null) {
  var inK = -1
  var u:Long = 0
  var uWithDist:Long = 0
  var res:Long=0
  var query:Int = -1
  var distanceFromRoot = -1
  var siblingsVersion:Int = -1
  var siblings:Array[Node] = _
  var siblingCount = 0
  var edgeCount = 0
  var notSeenEdges: mutable.Set[Edge] = mutable.Set[Edge]()

  def addSibling(node:Node) = {
    siblings(siblingCount) = node
    siblingCount +=1
  }
  def clearSiblings(): Unit ={
    siblingCount = 0
  }

  def addEdge(edge: Edge): Unit = {
    notSeenEdges.add(edge)
    edgeCount+=1
  }

  def seenEdge(edge: Edge): Unit = {
    notSeenEdges.remove(edge)
    edge.otherNode(this).notSeenEdges.remove(edge)
  }
}

class Edge(val iedge:Int, val nodeA:Node, val nodeB:Node) {

  def otherNode(node:Node): Node =
    if(node == nodeA) nodeB else nodeA
}


object Solution {
  val modulo = 1000000007
  def mod(l:Long) = if(l<modulo) l else l % modulo
  def precompute(nodes: Seq[Node]): Unit = {

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

    for (node <- nodes) {
      computeDistanceFromRoot(node)
      node.siblings = Array.ofDim[Node](node.edgeCount)
    }
  }

  def computeDistanceFromRoot(nodeQ:Node): Unit = {

    if (nodeQ.distanceFromRoot == -1) {
      val stack = mutable.Stack[Node]()
      var node = nodeQ

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
      val data = node
      val data2 = if (node.siblingsVersion == query) node else null


      data.u = if (node.inK == query) node.inode else 0
      data.res = 0
      data.uWithDist = 0

      if (data2 != null) {

        if (data.u == 0 && data2.siblingCount == 1) {
          val dataJ = data2.siblings(0)
          data.u = dataJ.u
          data.res = dataJ.res
          data.uWithDist = mod(dataJ.u + dataJ.uWithDist)
        } else {

          var uSum = 0L
          var uWithDistSum = 0L
          var resSum = 0L

          var j = 0
          while (j < data2.siblingCount) {
            val dataJ = data2.siblings(j)
            uSum += dataJ.u
            uWithDistSum += dataJ.uWithDist
            resSum += dataJ.res
            j += 1
          }

          var neighbours = 0L
          var i = 0
          if (data2.siblingCount > 1) {
            while (i < data2.siblingCount) {
              val dataI = data2.siblings(i)

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
          if (node.nodeParent.query != query) {
            node.nodeParent.query = query
            pq.enqueue(node.nodeParent)
          }
        }

        if (node.nodeParent.siblingsVersion != query) {
          node.nodeParent.siblingsVersion = query
          node.nodeParent.clearSiblings()
        }
        node.nodeParent.addSibling(node)
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
      scala.util.Sorting.quickSort(iitems)(Ordering.by(node => -node.distanceFromRoot))

      iitemsFirst = 0
      iitemsNext = iitems.length
      jitemsFirst = 0
      jitemsNext = 0

    }

    def nonEmpty: Boolean = iitemsNext > iitemsFirst || jitemsNext > jitemsFirst

    def enqueue(item: Node): Unit = {
      jitems(jitemsNext) = item
      jitemsNext += 1

    }

    def dequeue(): Node = {
      val ks1NonEmpty = iitemsNext > iitemsFirst
      val ks2NonEmpty = jitemsNext > jitemsFirst
      if (ks1NonEmpty && ks2NonEmpty) {
        if (iitems(iitemsFirst).distanceFromRoot > jitems(jitemsFirst).distanceFromRoot) {
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
    System.setIn(new FileInputStream(new File(s"src/main/scala/trees.kittyscalc/in17.txt")))
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



    println((System.currentTimeMillis() - t1) / 1000.0)
    precompute(nodes)
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

