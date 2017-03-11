package qtt
import java.io.{File, FileInputStream, FileOutputStream, PrintStream}


import scala.collection.mutable.ListBuffer

case class MyInt(var digits:Array[Int]) {

  MyInt.qqq +=1
  digits =
    if (digits.length == 1 || digits(0) != 0)
     digits
    else
     digits.dropWhile(d => d == 0)

  override def toString: String = digits.map(d => (d + '0').toChar).mkString("")

  def shift(logLength: Int):MyInt = {
    if (logLength == 0) {
      this
    } else {

      val res = Array.fill[Int](digits.length + logLength)(0)
      digits.copyToArray(res)
      MyInt(res)
    }
  }

  def ==(that:MyInt):Boolean = {
    digits.sameElements(that.digits)
  }

  def <(that: MyInt): Boolean = {
    if (digits.length < that.digits.length)
      true
    else if(digits.length > that.digits.length)
      false
    else {
      var i = 0
      var eq = true
      while (eq && i < digits.length) {
        if (digits(i) == that.digits(i)) {
          i += 1
        } else {
          eq = false
        }
      }
      !eq && digits(i) < that.digits(i)
    }
  }

  def dec(): MyInt = {
    this - MyInt.One
  }

  def inc(): MyInt = {
    val res = digits.clone()

    var i = res.length - 1
    var d = 1
    while (d != 0 && i >= 0) {
      res(i) += d
      if (res(i) > 9) {
        res(i) = res(i) % 10
        d = 1
      } else {
        d = 0
      }
      i -= 1
    }

    if (d == 1) {
      val q = Array.fill[Int](res.length + 1)(0)
      q(0) = 1
      MyInt(q)
    }
    else {
      MyInt(res)
    }
  }

  def -(other: MyInt): MyInt = {
    if (other == this) {
      MyInt.Zero
    } else {
      val res = digits.clone()
      var i = res.length - 1
      var j = other.digits.length - 1

      var d = 0
      while (d != 0 || j >= 0) {
        val z = if (j < 0) 0 else other.digits(j)
        val q = res(i) - z - d
        if (q < 0) {
          res(i) = 10 + q
          d = 1
        }
        else {
          res(i) = q
          d = 0
        }
        j -= 1
        i -= 1
      }
      MyInt(res)
    }
  }

  def block(blockLength: BlockLength): MyInt = {

    val res = dec()
    if (blockLength.logLength == 0)
      res
    else if (res.digits.length <= blockLength.logLength)
      MyInt.Zero
    else {
      MyInt(res.digits.take(res.digits.length - blockLength.logLength))
    }

  }
}

case class BlockLength(logLength:Int) {}

object MyInt {

  var qqq = 0

  def fromInt(i:Int):MyInt = {
    MyInt(i.toString.map(x => x - '0').toArray)
  }

  def from(blockLength: BlockLength, block: MyInt): MyInt = {
    block.shift(blockLength.logLength).inc()
  }

  def to(blockLength: BlockLength, block: MyInt): MyInt = {
    block.inc().shift(blockLength.logLength)
  }

  val One = MyInt(Array(1))
  val Zero = MyInt(Array(0))

}

case class Level(level:Int) {
  lazy val next: Level = Level(level + 1)
  lazy val prev: Level = Level(level - 1)
  lazy val blockLength: BlockLength =
    level match {
      case 0 => BlockLength(0)
      case _ => BlockLength(Math.pow(2, level - 1).toInt)
    }
}

case class Range(level:Level, block:MyInt) {
  lazy val from: MyInt = MyInt.from(level.blockLength, block)
  lazy val to: MyInt = MyInt.to(level.blockLength, block)

  def subRangeContaining(i: MyInt): Range = {
    val subLevel = level.prev
    val block = i.block(subLevel.blockLength)
    Range(subLevel, block)
  }
}

object Solution {

  def solveI(l: MyInt, r: MyInt, range: Range, res: ListBuffer[(Level, MyInt)]): Unit = {
    if (l == range.from && r == range.to) {
      res.append((range.level,  MyInt.One))
    } else {
      var subRangeL = range.subRangeContaining(l)
      var subRangeR = range.subRangeContaining(r)

      if (subRangeL == subRangeR) {
        solveI(l, r, subRangeL, res)
      } else {

        if (subRangeL.from < l) {
          solveI(l, subRangeL.to, subRangeL, res)
        }
        var wholeSubrangeCount = subRangeR.block - subRangeL.block
        if (subRangeL.from == l && subRangeR.to == r) {
          wholeSubrangeCount = wholeSubrangeCount.inc()
        } else if (subRangeL.from < l && r < subRangeR.to) {
          wholeSubrangeCount = wholeSubrangeCount.dec()
        }

        res.append((subRangeR.level, wholeSubrangeCount))

        if (r < subRangeR.to) {
          solveI(subRangeR.from, r, subRangeR, res)
        }
      }
    }
  }

  def solve(left: MyInt, right: MyInt): List[(Level, MyInt)] = {
    var level = Level(0)
    while (level.blockLength.logLength < right.digits.length) {
      level = level.next
    }
    val res = new ListBuffer[(Level, MyInt)]
    solveI(left, right, Range(level, MyInt.Zero), res)
    res.toList
  }

  def main(args: Array[String]) {
    //test()
    System.setIn(new FileInputStream(new File(s"src/main/scala/qtt/in50.txt")))
   // System.setOut(new PrintStream(new FileOutputStream(new File(s"src/main/scala/qtt/out50.out"))))

    val L = readLine().map(x => x - '0').toArray
    val Q = readLine().map(x => x - '0').toArray

    val t0 = System.nanoTime()
    val r = solve(MyInt(L), MyInt(Q))
    println(r.length)

    for ((a, b) <- r) {
      println(s"${a.level} $b")
    }

    val t1 = System.nanoTime()
    //println("Elapsed time: " + (t1 - t0) / (1000000000.0)+ "s")

  }

}
