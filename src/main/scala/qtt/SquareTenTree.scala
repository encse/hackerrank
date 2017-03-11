package qtt
import java.io.{File, FileInputStream}

import scala.collection.mutable.ListBuffer

case class MyInt(private val st:String) {
  def <(that: MyInt): Boolean = st.length < that.st.length || (st.length == that.st.length && st < that.st)

  def block(blockLength: BlockLength): BigInt = {

    val chars = st.toCharArray
    var i = chars.length - 1
    var ok = false
    while (!ok) {
      if (chars(i) != '0') {
        chars(i) = (chars(i) - 1).toChar
        ok = true
      }
      else {
        chars(i) = '9'
      }
      i -= 1
    }
    val cut = blockLength.v.toString().length() - 1
    if(chars.length < cut)
      0
    else
      BigInt(chars.take(chars.length - cut.toInt).mkString(""))


  }
}
case class BlockLength(length:BigInt){
  def *(b:BlockLength):BlockLength = {
    BlockLength(length * b.length)
  }
  def v():BigInt = {
    length
  }
}
object MyInt {
  def from(blockLength:BlockLength, block:BigInt):MyInt = {
    MyInt((blockLength.v * block + 1).toString)
  }
  def to(blockLength:BlockLength, block:BigInt): MyInt ={
    MyInt((blockLength.v * (block+1)).toString)
  }
}
case class Level(level:Int) {
  lazy val next:Level = Level(level+1)
  lazy val prev:Level = Level(level-1)
  lazy val blockLength:BlockLength =
    level match {
      case 0 => BlockLength(1)
      case 1 => BlockLength(10)
      case _ =>  prev.blockLength * prev.blockLength
    }
}

case class Range(level:Level, block:BigInt) {
  lazy val from:MyInt = MyInt.from(level.blockLength, block)
  lazy val to:MyInt = MyInt.to(level.blockLength, block)

  def subRangeContaining(i:MyInt):Range = {
    val subLevel = level.prev
    val block = i.block(subLevel.blockLength)
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
    while (MyInt(level.blockLength.v().toString()) < right) {
      level = level.next
    }
    val res = new ListBuffer[(Level, BigInt)]
    solveI(left, right, Range(level, 0), res)
    res.toList
  }

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/qtt/in.txt")))

    val sc = new java.util.Scanner(System.in)
    val L = sc.next
    val Q = sc.next
    val r = solve(MyInt(L), MyInt(Q))
    println(r.length)
    for ((a, b) <- r) {
      println(s"${a.level} $b")
    }
  }

}
