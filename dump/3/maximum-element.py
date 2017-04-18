Array.prototype.max = function() {
  return Math.max.apply(null, this);
};

function processData(input) {
   var iline = 0
   var lines = input.split("\n")
   function readline(){
       return lines[iline++];       
   }
   
   var N = readline()
   var stack = []
   var m = -1
   for (var i =0; i< N;i++){
       var q = readline().split(" ")
       if(q[0] == 1){
           stack.push(q[1])
           m = Math.max(m, q[1])
       }else if(q[0] == 2){
           var e = stack.pop()
           if(e == m){
               m = stack.max()
           }
       } else {
           console.log(m)
       }
   }
} 

process.stdin.resume();
process.stdin.setEncoding("ascii");
_input = "";
process.stdin.on("data", function (input) {
    _input += input;
});

process.stdin.on("end", function () {
   processData(_input);
});
