process.stdin.resume();
process.stdin.setEncoding('ascii');

var input_stdin = "";
var input_stdin_array = "";
var input_currentline = 0;

process.stdin.on('data', function (data) {
    input_stdin += data;
});

process.stdin.on('end', function () {
    input_stdin_array = input_stdin.split("\n");
    main();    
});

function readLine() {
    return input_stdin_array[input_currentline++];
}

/////////////// ignore above this line ////////////////////
function check(st){
    var stack = []
    for(var ich=0;ich<st.length;ich++){
        var ch = st[ich];
        if(ch == "(" || ch == "[" || ch == "{"){
            stack.push(ch);
        } else{
            var top = stack[stack.length-1];
            if(ch == "}" && top != "{") return "NO";
            if(ch == ")" && top != "(") return "NO";
            if(ch == "]" && top != "[") return "NO";
            stack.pop();
        }
    }
    return stack.length == 0 ? "YES" : "NO";
}
function main() {
    var t = parseInt(readLine());
    for(var a0 = 0; a0 < t; a0++){
        var st = readLine();
        console.log(check(st))

    }

}
