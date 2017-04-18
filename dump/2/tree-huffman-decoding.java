/*  
   class Node
      public  int frequency; // the frequency of this tree
       public  char data;
       public  Node left, right;
    
*/ 

void decode(String st, Node root)
{
    String res = "";
    Node n = root;
    for(int i=0;i<st.length();i++){
        if(st.charAt(i) == '0'){
            n = n.left;
        } else {
            n = n.right;
        }
        if(n.left == null && n.right == null){
            res += n.data;
            n=root;
        }
    }

    System.out.println(res);
}
