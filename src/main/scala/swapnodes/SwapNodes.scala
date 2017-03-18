package swapnodes

import java.io.{File, FileInputStream}


case class Node(inode:Int, var left:Int = -1, var right:Int = -1) {

}


object Solution {


  def inorder(nodes: Array[Node], inode: Int):Unit = {
    if(inode != -1){
      inorder(nodes, nodes(inode).left)
      print(inode + " ")
      inorder(nodes, nodes(inode).right)
    }

  }

  def swapRecursive(nodes: Array[Node], inode: Int, depth:Int, k:Int):Unit = {
    if (inode != -1) {

      if(depth % k == 0) {
        val c = nodes(inode).left
        nodes(inode).left = nodes(inode).right
        nodes(inode).right = c
      }
      swapRecursive(nodes, nodes(inode).left, depth +1, k)
      swapRecursive(nodes, nodes(inode).right, depth +1, k)
    }
  }
  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/balancedforest/in6.txt")))
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val nodes = (0 to n).map(inode => Node(inode)).toArray
    for(inode <- 1 to n){
      val (left, right) = (sc.nextInt(), sc.nextInt())
      nodes(inode).left = left
      nodes(inode).right = right
    }
    val t = sc.nextInt()
    for(i <- 1 to t){
      val k = sc.nextInt()
      swapRecursive(nodes, 1, 1, k)


      inorder(nodes, 1)
      println("")

    }

  }

}

