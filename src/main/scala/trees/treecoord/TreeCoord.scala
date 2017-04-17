package trees.treecoord

import java.io.{File, FileInputStream}

import scala.collection.mutable

class NodePair(val node1:Node, val node2:Node) {
  var res = 0
  var longestPathBelow = 0
  var inK = false

  var children = mutable.Set[NodePair]()

  override def toString: String = {
    s"(${node1.inode},${node2.inode})"
  }
}

object NodePair {
  val repo = mutable.Map[(Node, Node), NodePair]()

  def create(node1: Node, node2: Node):NodePair = {
    val key = (node1, node2)
    val r = repo.get(key)
    if (r.isDefined) {
      r.get
    } else {
      repo(key) = new NodePair(node1, node2)
      repo(key)
    }
  }


}



class Node(val inode:Int, var nodeParent:Node = null) {
  var distanceFromRoot: Int = -1
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
}

case class Edge(iedge:Int, nodeA:Node, nodeB:Node) {

  def otherNode(node:Node): Node =
    if(node == nodeA) nodeB else nodeA
}

class PQueue(nodeCount:Int) {
  private var iitems: Array[NodePair] = _
  private var jitems: Array[NodePair] = Array.ofDim[NodePair](nodeCount)
  private var iitemsFirst:Int = _
  private var iitemsNext:Int = _
  private var jitemsFirst:Int = _
  private var jitemsNext:Int = _

  def furtherFromRoot(nodePairA:NodePair, nodePairB:NodePair):Boolean =
    nodePairA.node1.distanceFromRoot > nodePairB.node1.distanceFromRoot ||
      nodePairA.node1.distanceFromRoot == nodePairB.node1.distanceFromRoot && nodePairA.node2.distanceFromRoot > nodePairB.node2.distanceFromRoot

  def init(iitems: Array[NodePair]): Unit = {
    this.iitems = iitems
    scala.util.Sorting.quickSort(iitems)(Ordering.fromLessThan(furtherFromRoot))

    iitemsFirst = 0
    iitemsNext = iitems.length
    jitemsFirst = 0
    jitemsNext = 0

  }

  def nonEmpty: Boolean = iitemsNext > iitemsFirst || jitemsNext > jitemsFirst

  def enqueue(item: NodePair): Unit = {
    jitems(jitemsNext) = item
    jitemsNext += 1

  }

  def dequeue(): NodePair = {
    val ks1NonEmpty = iitemsNext > iitemsFirst
    val ks2NonEmpty = jitemsNext > jitemsFirst
    if (ks1NonEmpty && ks2NonEmpty) {
      if (furtherFromRoot(iitems(iitemsFirst), jitems(jitemsFirst))) {
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


object Solution {

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
    }
  }

  def computeDistanceFromRoot(nodeQ: Node): Unit = {

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


  def readInts(): (Int, Int) = {
    val st = readLine()

    var d = 0

    val res = Array.ofDim[Int](2)

    var ich = 0
    var ik = 0
    while (ich < st.length) {
      val ch = st(ich)
      if (ch == ' ') {
        res(ik) = d
        ik += 1
        d = 0
      } else {
        d = 10 * d + ch - '0'
      }
      ich += 1

    }
    res(ik) = d
    (res(0), res(1))
  }

  def solve(ksList: Array[NodePair]): Long = {
    var last: NodePair = null

    val pq = new PQueue(100000)
    pq.init(ksList)


    var res = 0
    while (pq.nonEmpty) {
      val current = pq.dequeue()
      last = current
      val node1 = current.node1
      val node2 = current.node2
      val parent =
        if (node2.nodeParent != null) NodePair.create(node1, node2.nodeParent)
        else if (node1.nodeParent != null) NodePair.create(node1.nodeParent, node2)
        else null

      var first = true
      for (child <- current.children) {
        if (first) {
          current.res = child.res
        }
        else {
          current.res = math.max(current.res, child.res)
        }

        if (current.inK) {
          current.res = math.max(current.res, child.longestPathBelow + 1)
        }

        if (!first) {
          current.res = math.max(current.res, child.longestPathBelow + 1 + current.longestPathBelow)
        }

        if (first) {
          current.longestPathBelow = child.longestPathBelow + 1
        } else {
          current.longestPathBelow = math.max(child.longestPathBelow + 1, current.longestPathBelow)
        }


        first = false
      }

      res = current.res

      if (parent != null) {
        println(current + "->" + parent)
        parent.children.add(current)
        pq.enqueue(parent)
      }
    }

    res
  }

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/trees.treecoord/in1.txt")))
    val t1 = System.currentTimeMillis()

    val (cnode, cquery) = readInts()
    val nodes = Array.ofDim[Node](cnode + 1)
    var inode = 0
    while (inode <= cnode) {
      nodes(inode) = new Node(inode)
      inode += 1
    }

    var iedge = 1
    while (iedge <= cnode - 1) {
      val (inodeA, inodeB) = readInts()
      val edge = Edge(iedge, nodes(inodeA), nodes(inodeB))
      edge.nodeA.addEdge(edge)
      edge.nodeB.addEdge(edge)
      iedge += 1
    }

    println((System.currentTimeMillis() - t1) / 1000.0)
    precompute(nodes)
    println((System.currentTimeMillis() - t1) / 1000.0)

    var k = Array.ofDim[NodePair](cquery)
    var i = 0
    while (i < cquery) {
      val (inodeA, inodeB) = readInts()
      k(i) = NodePair.create(nodes(inodeA), nodes(inodeB))
      k(i).inK = true
      i+=1
    }

    println(solve(k))
    println((System.currentTimeMillis() - t1) / 1000.0)
  }

}

