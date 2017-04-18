

import java.io.{File, FileInputStream}

class Node {
  var size: Int = 1
  var parent: Node = _

  def root: Node = {
    if (parent == null)
      this
    else {
      val res = parent.root
      this.parent = res
      res
    }
  }

  def mergeWith(c: Node): Unit = {
    val r1 = root
    val r2 = c.root
    if (r1 != r2) {
      r1.size += r2.size
      r2.parent = r1
    }
  }
}

object Solution {


  val mod = BigInt(1000000007L) * 6

  def S(n: Iterable[BigInt]): BigInt = n.sum.mod(mod)

  def S2(n: Iterable[BigInt]): BigInt = n.map(i => (i * i).mod(mod)).sum.mod(mod)

  def S3(n: Iterable[BigInt]): BigInt = n.map(i => (i * i * i).mod(mod)).sum.mod(mod)

  def H(n: Iterable[BigInt]): BigInt = (S(n) * S(n) * S(n).mod(mod) - 3 * S(n) * S2(n).mod(mod) + 2 * S3(n).mod(mod)).mod(mod)


  def main(args: Array[String]) {
   // System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName.replace('.', '/')}/in5.txt")))
    val sc = new java.util.Scanner(System.in)

    val n = sc.nextInt()
    val nodes = Array.fill[Node](n + 1)(new Node)

    for (i <- 1 until n) {
      val inodeFrom = sc.nextInt()
      val inodeTo = sc.nextInt()
      val color = sc.next("[a-z]")

      if (color == "b") {
        nodes(inodeFrom).mergeWith(nodes(inodeTo))
      }
    }

    val sizes = nodes.tail.map(node => node.root).toSet[Node].toSeq.map(x => BigInt(x.size))

    println(H(sizes)/6)
  }

}


