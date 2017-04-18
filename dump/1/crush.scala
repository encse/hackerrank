object Solution {

    def main(args: Array[String]) {
        
        val sc = new java.util.Scanner(System.in)
        val L = sc.nextInt()
        val Q = sc.nextInt()
        val f = newRange(L)
        for(i<-0 until Q){
            val (a, b, s) = (sc.nextInt(), sc.nextInt(), sc.nextInt())
            f.update(a, b, s)
        }
       
        println(f.max)
    }
        
     def newRange(l:Int):Range= {
  var q = 1
  while (q < l)
    q *= 2
  Range(1, q, 0)
}   
}


case class Range(var from:Int, var to:Int, var v:Long, var left:Range = null, var right:Range= null) {
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
    } else {
      throw new UnsupportedOperationException("coki")
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

