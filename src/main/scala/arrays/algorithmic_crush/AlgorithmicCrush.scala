package arrays

import java.io.{File, FileInputStream}




case class Range(var from: Int, var to: Int, var v: Long, var left: Range = null, var right: Range = null) {

  def leaf: Boolean = left == null

  def length: Int = to - from + 1

  require(length > 0)

  override def toString() = {
    def indent(st:String): String ={
      val lines = st.split("\n").zipWithIndex.map{
        case (stT,0) => "--" + stT
        case (stT, _) => "    " + stT
      }
      lines.mkString("\n")
    }

    if (leaf)
      s"[$from, ${to}, $length, $v]"
    else {
      s"""[$from, ${to}, $length]:
         |${indent(left.toString())}
         |${indent(right.toString())}
         |""".stripMargin
    }
  }

  def split(): Unit = {
    if (to != from) {
      val l = length / 2
      // println(s"length:$length, newLength: $l")

      left = Range(from, from + l - 1, 0)
      right = Range(from + l, to, 0)
    }
  }

  def update(a: Int, b: Int, s: Int): Unit = {
    if (a == from && b == to) {
      v += s
    } else if (leaf) {
      split()
      update(a, b, s)
    } else {
      if (b <= left.to) {
        left.update(a, b, s)
      } else if (a >= right.from) {
        right.update(a, b, s)
      } else {
        left.update(a, left.to, s)
        right.update(right.from, b, s)
      }
    }

  }

  def max: Long = {
    if (leaf)
      v
    else
      Math.max(v + left.max, v + right.max)
  }
}
