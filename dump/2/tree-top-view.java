
/*
   class Node 
       int data;
       Node left;
       Node right;
*/
ArrayList top_viewI(Node root, boolean left)
{
    ArrayList res = new ArrayList();
    if(root != null){
        if (left)
            res.addAll(top_viewI(root.left, left));
        res.add(root.data);
        if(!left)
            res.addAll(top_viewI(root.right, left));
    }
    return res;
        
}
void top_view(Node root)
{
    String res = "";
    ArrayList items = top_viewI(root, true);
    items.addAll(top_viewI(root.right, false));
    for(int i=0;i<items.size();i++){
        if(i != 0)
            res += " ";
        res += items.get(i).toString();
    }
    System.out.println(res);
}
