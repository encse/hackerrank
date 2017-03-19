package kittyscalc

import java.io.{File, FileInputStream, FileOutputStream, PrintStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Data(var u:Long, var uWithDist:Long, var res:Long=0){

}


case class Node(inode:Int, edges:ListBuffer[Edge] = new ListBuffer, var inodeParent:Int = -1) {
  def addEdge(edge: Edge) = {
    edges.append(edge)
  }

  def degree = edges.length

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
  var seen:Boolean = false

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

  def buildParentGraph(nodes: IndexedSeq[Node]): Unit ={

    var restINodes:Iterable[Node] = nodes

    while (restINodes.nonEmpty) {
      var restINodesNew = mutable.Set[Node]()
      for (node <- restINodes) {
        val edgesNotSeen = node.edges.filter(edge => !edge.seen)

        if(edgesNotSeen.length == 1){
          val edge = edgesNotSeen.head
          node.inodeParent = otherNode(nodes, node, edge).inode
          edge.seen = true
        } else if(edgesNotSeen.isEmpty) {

        }
        else {
          restINodesNew += node
        }

      }
      restINodes = restINodesNew
    }
  }

  def solve(nodes: IndexedSeq[Node], ks:Set[Int]):Long = {
    var e = 0
    var res = 0L
    var ksSorted = ks.toList.sortBy(k => -nodes(k).getDistanceFromRoot(nodes))
    var restINodes = mutable.Set[Int]()

    var cache = mutable.Map[Int, Data]()

    while (ksSorted.length + restINodes.size >= 1) {
      var ksCurrent = mutable.Set[Int]()
      if (ksSorted.nonEmpty && (restINodes.isEmpty || nodes(ksSorted.head).getDistanceFromRoot(nodes) == nodes(restINodes.head).getDistanceFromRoot(nodes))) {
        val d = nodes(ksSorted.head).getDistanceFromRoot(nodes)
        val (ksCurrentT, ksSortedT) = ksSorted.span(k => nodes(k).getDistanceFromRoot(nodes) == d)
        ksCurrent = mutable.Set[Int](ksCurrentT: _*)
        ksSorted = ksSortedT
      }

      var restINodesNew = mutable.Set[Int]()
      for (inode <- ksCurrent.union(restINodes)) {
        if (!cache.contains(inode)) {
          cache(inode) = new Data(if (ks.contains(inode)) inode else 0, 0, 0)
        }
        val data = cache(inode)
        val v = data.u
        val node = nodes(inode)

        for {edge <- node.edges
             o = otherNode(nodes, node, edge)
             if o.inode != node.inodeParent} {

          for(dataO <- cache.get(o.inode)) {
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
          for{
            dataI <- cache.get(oI.inode)
            dataJ <- cache.get(oJ.inode)
          }{
            data.res +=  2 * dataJ.u * dataI.u  + dataI.uWithDist * dataJ.u + dataJ.uWithDist * dataI.u
          }
        }

        if (node.inodeParent != -1) {
          restINodesNew += node.inodeParent
        }

        data.u %= 1000000007
        data.uWithDist %= 1000000007
        data.res %= 1000000007

       // if(ksCurrent.contains(inode)) {
          res = data.res
        //}

      }

      restINodes = restINodesNew
    }

    res
  }


  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/kittyscalc/in20.txt")))

    val sc = new java.util.Scanner(System.in)
    val (n,q) = (sc.nextInt(), sc.nextInt())
    val nodes: IndexedSeq[Node] = (0 to n).map(i => Node(i))
    val edges: IndexedSeq[Edge] = (1 to n-1).map(iedge => new Edge(iedge, sc.nextInt(), sc.nextInt()))

    for(edge <- edges){
      nodes(edge.inodeA).edges.append(edge)
      nodes(edge.inodeB).edges.append(edge)
    }

    val t1 = System.currentTimeMillis()
    buildParentGraph(nodes)
    println((System.currentTimeMillis()-t1)/1000.0)
    for (i <- 1 to q) {
      val c = sc.nextInt()
      sc.nextLine()
      val k = sc.nextLine().split(" ").map(st => st.toInt).toSet

      println(solve(nodes, k))
    }

    val t2 = System.currentTimeMillis()
    println((System.currentTimeMillis()-t1)/1000.0)
  }

}

