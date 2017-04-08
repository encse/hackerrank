package castleonthegrid

import java.io.{File, FileInputStream}

import scala.collection.mutable

case class Grid(cells: Array[Array[Char]]){
  def free(irow:Int,icol:Int): Boolean = cells(irow)(icol) == '.'

}

object Solution {


  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/${getClass.getPackage.getName}/in0.txt")))
    val sc = new java.util.Scanner(System.in)

    val n = sc.nextInt()
    sc.nextLine()
    val cells = Array.ofDim[Array[Char]](n)
    for(i <- 0 until n) {
      cells(i) = sc.nextLine().toCharArray
    }
    val grid = Grid(cells)
    val (irowStart, icolStart, irowEnd, icolEnd) = (sc.nextInt(), sc.nextInt(), sc.nextInt(), sc.nextInt())
    val q = mutable.Queue[(Int,Int,Int)]()
    val seen = mutable.Set[(Int,Int)]()
    seen.add((irowStart, icolStart))
    q.enqueue((irowStart, icolStart, 0))
    var found = false
    while (q.nonEmpty && !found) {
      val (irow,icol,dist) = q.dequeue()
      if ((irow, icol) == (irowEnd, icolEnd)){
        found = true
        println(dist)
      } else {

        def add(irow: Int, icol: Int): Unit = {
          if (!seen.contains(irow, icol)) {
            q.enqueue((irow, icol, dist + 1))
            seen.add((irow, icol))
          }
        }

        var icolT = icol - 1
        while (icolT >= 0 && grid.free(irow, icolT)) {
          add(irow, icolT)
          icolT -= 1
        }

        icolT = icol + 1
        while (icolT < n && grid.free(irow, icolT)) {
          add(irow, icolT)
          icolT += 1
        }

        var irowT = irow - 1
        while (irowT >= 0 && grid.free(irowT, icol)) {
          add(irowT, icol)
          irowT -= 1
        }

        irowT = irow + 1
        while (irowT < n && grid.free(irowT, icol)) {
          add(irowT, icol)
          irowT += 1
        }
      }
    }
  }
}
