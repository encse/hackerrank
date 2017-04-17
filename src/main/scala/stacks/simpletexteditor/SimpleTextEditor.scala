package stacks.simpletexteditor

import java.io.{File, FileInputStream}

import scala.collection.mutable


object Solution {

  def main(args: Array[String]) {
    System.setIn(new FileInputStream(new File(s"src/main/scala/stacks.simpletexteditor/in0.txt")))

    val sc = new java.util.Scanner(System.in)
    val Q = sc.nextInt()
    sc.nextLine()

    var text = ""
    var undo = new mutable.Stack[()=>Unit]()
    for(i <- 0 until Q){
      val items = sc.nextLine().split(" ")
      if(items(0) == "1"){
        text += items(1)
        undo.push( () => text = text.substring(0, text.length() - items(1).length()))
      } else if(items(0) == "2"){
        val ich = text.length() - items(1).toInt
        val deleted = text.substring(ich)
        text = text.substring(0, ich)
        undo.push( () => text += deleted)
      } else if(items(0) == "3"){
        println(text(items(1).toInt-1))
      } else {
        undo.pop()()
      }
    }

  }

}

