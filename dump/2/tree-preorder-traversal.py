import sys

"""
Node is defined as
self.left (the left child of the node)
self.right (the right child of the node)
self.data (the value of the node)
"""
def preOrderI(root):
   
    res = str(root.data)
    if root.left != None:
        res += " " + preOrderI(root.left)
    if root.right != None:
        res += " " + preOrderI(root.right)
    return res

def preOrder(root):
    print preOrderI(root)