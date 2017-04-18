object Solution {

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var arr = Array.ofDim[Int](6,6);
        for(arr_i <- 0 to 5) {
           for(arr_j <- 0 to 5){
              arr(arr_i)(arr_j) = sc.nextInt();
           }
        }
     
        var m = -100000
        for(i<-1 to 4){
          for(j<-1 to 4){
            m = Math.max(m, hgSum(arr, i, j))
          }
        }
        println(m)
  }

  def hgSum(arr:Array[Array[Int]], i:Int, j:Int): Int ={
    var sum = 0
    for(iT <- -1 to 1){
      for(jT<- -1 to 1){
        if(iT != 0 || jT == 0)
          sum += arr(i+iT)(j+jT)
      }
    }
    sum
  }
}