package levelordertraversal;

import scala.Console;

import java.util.Queue;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * Created by encse on 3/18/17.
 */
class Node{
    int data;
    Node left;
    Node right;
}
public class Solution {

    void LevelOrder(Node root)
    {

        java.util.Queue<Node> nodes = new java.util.concurrent.LinkedBlockingQueue<Node>();
        nodes.add(root);
        while(nodes.size() > 0){
            Node node = nodes.remove();
            System.out.print(node.data + " ");
            if(node.left != null)
                nodes.add(node.left);
            if(node.right != null)
                nodes.add(node.right);
        }

    }

}
