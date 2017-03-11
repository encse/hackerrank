package qtt
import java.io.{File, FileInputStream}
import scala.collection.mutable.ListBuffer

case class MyInt(private val digitsI:Array[Int]) {
  val digits:Array[Int] = if(digitsI.length == 1 || digitsI(0) != 0)  digitsI else digitsI.dropWhile(d => d == 0)
  override def toString: String = digits.map(d => (d + '0').toChar).mkString("")

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
      var lt = false
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
    this - MyInt(Array(1))
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
      MyInt(Array(1) ++ res)
    }
    else {
      MyInt(res)
    }
  }

  def -(other: MyInt): MyInt = {
    if (other == this) {
      MyInt(Array(0))
    } else {
      //require(other < this)
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
    if (res.digits.length <= blockLength.logLength)
      MyInt(Array(0))
    else {
      MyInt(res.digits.take(res.digits.length - blockLength.logLength))
    }

  }
}

case class BlockLength(logLength:Int) {}

object MyInt {

  def fromInt(i:Int):MyInt = {
    MyInt(i.toString.map(x => x - '0').toArray)
  }

  def from(blockLength: BlockLength, block: MyInt): MyInt = {
    val digits = block.digits ++ Array.fill(blockLength.logLength)(0)
    MyInt(digits).inc()
  }

  def to(blockLength: BlockLength, block: MyInt): MyInt = {
    val digits = block.inc().digits ++ Array.fill(blockLength.logLength)(0)
    MyInt(digits)
  }
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
      res.append((range.level, MyInt(Array(1))))
    } else {
      val subRangeL = range.subRangeContaining(l)
      val subRangeR = range.subRangeContaining(r)

      if (subRangeL == subRangeR) {
        solveI(l, r, subRangeL, res)
      } else {

        var wholeSubrangeCount = (subRangeR.block - subRangeL.block).inc()

        if (subRangeL.from < l) {
          wholeSubrangeCount = wholeSubrangeCount.dec()
        }

        if (r < subRangeR.to) {
          wholeSubrangeCount = wholeSubrangeCount.dec()
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

  def solve(left: MyInt, right: MyInt): List[(Level, MyInt)] = {
    var level = Level(0)
    while (level.blockLength.logLength < right.digits.length) {
      level = level.next
    }
    val res = new ListBuffer[(Level, MyInt)]
    solveI(left, right, Range(level, MyInt(Array(0))), res)
    res.toList
  }

  def test(): Unit ={

    val lim = 1000
    for(i <- 0 to lim) {
      for (j <- i to lim) {
        require((MyInt.fromInt(j) - MyInt.fromInt(i)) ==  MyInt.fromInt(j-i), s"$j - $i != ${MyInt.fromInt(j) - MyInt.fromInt(i)}")
      }
    }
    for(i <- 0 to lim) {
      if(i!=0)
        require(MyInt.fromInt(i).dec() ==  MyInt.fromInt(i - 1), s"$i -1 != ${MyInt.fromInt(i).dec()}")
      require(MyInt.fromInt(i).inc() ==  MyInt.fromInt(i + 1), s"$i +1 != ${MyInt.fromInt(i).inc()}")
    }

    for(i <- 0 to lim) {
      for (j <- 0 to lim) {
        require( (MyInt.fromInt(i) < MyInt.fromInt(j)) ==  (i < j), s"$i < $j != ${MyInt.fromInt(i) < MyInt.fromInt(j)}")
        require( (MyInt.fromInt(i) == MyInt.fromInt(j)) ==  (i == j), s"$i == $j != ${MyInt.fromInt(i) == MyInt.fromInt(j)}")
      }
    }

  }
  def main(args: Array[String]) {
    //test()
    System.setIn(new FileInputStream(new File(s"src/main/scala/qtt/in1.txt")))

    val L = readLine().map(x => x - '0').toArray
    val Q = readLine().map(x => x - '0').toArray
    val r = solve(MyInt(L), MyInt(Q))
    println(r.length)
    for ((a, b) <- r) {
      println(s"${a.level} $b")
    }
  }

}
