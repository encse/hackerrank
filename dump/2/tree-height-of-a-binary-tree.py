# Enter your code here. Read input from STDIN. Print output to STDOUT
'''
class Node:
      def __init__(self,info): 
          self.info = info  
          self.left = None  
          self.right = None 
           

       // this is a node of the tree , which contains info as data, left , right
'''
def heightI(root):
    if(root == None):
        return 0
    return 1+max(heightI(root.left), heightI(root.right))
def height(root):
    return heightI(root)-1