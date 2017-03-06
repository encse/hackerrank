package qtt
import java.io.{File, FileInputStream}

import scala.collection.mutable.ListBuffer

case class MyInt(v:BigInt) {
  def <(that: MyInt): Boolean = v < that.v

  def block(blockLength: BigInt): BigInt = {
    (v - 1) / blockLength
  }
}

case class Level(level:Int) {
  lazy val next:Level = Level(level+1)
  lazy val prev:Level = Level(level-1)
  lazy val blockLength:BigInt =
    level match {
      case 0 => 1
      case 1 => 10
      case _ => prev.blockLength * prev.blockLength
    }
}

case class Range(level:Level, block:BigInt) {
  lazy val from:MyInt = MyInt(level.blockLength * block + 1)
  lazy val to:MyInt = MyInt(level.blockLength * (block + 1))

  def subRangeContaining(i:MyInt):Range = {
    val subLevel = level.prev
    val block =  i.block(subLevel.blockLength)
    Range(subLevel, block)
  }
}

object Solution {

  def solveI(l: MyInt, r: MyInt, range:Range, res:ListBuffer[(Level, BigInt)]): Unit = {
    if (l == range.from && r == range.to) {
      res.append((range.level, 1))
    } else {
      val subRangeL = range.subRangeContaining(l)
      val subRangeR = range.subRangeContaining(r)

      if (subRangeL == subRangeR) {
        solveI(l, r, subRangeL, res)
      } else {

        var wholeSubrangeCount = subRangeR.block - subRangeL.block + 1

        if (subRangeL.from < l) {
          wholeSubrangeCount -= 1
        }

        if (r < subRangeR.to) {
          wholeSubrangeCount -= 1
        }

        if (subRangeL.from < l) {
          solveI(l, subRangeL.to, subRangeL, res)
        }

        res.append((subRangeL.level, wholeSubrangeCount))

        if (r < subRangeR.to) {
          solveI(subRangeR.from, r, subRangeR, res)
        }
      }
    }
  }

  def solve(left: MyInt, right: MyInt): List[(Level, BigInt)] = {
    var level = Level(0)
    while (MyInt(level.blockLength) < right) {
      level = level.next
    }
    val res = new ListBuffer[(Level, BigInt)]
    solveI(left, right, Range(level, 0), res)
    res.toList
  }

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/qtt/in.txt")))

    val sc = new java.util.Scanner(System.in)
    val L = sc.nextBigInteger()
    val Q = sc.nextBigInteger()
    val r = solve(MyInt(L), MyInt(Q))
    println(r.length)
    for ((a, b) <- r) {
      println(s"${a.level} $b")
    }
  }

}
