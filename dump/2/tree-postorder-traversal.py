"""
Node is defined as
self.left (the left child of the node)
self.right (the right child of the node)
self.data (the value of the node)
"""
def traverse(root):
    res =  []
    if root.left != None:
        res += traverse(root.left)
    if root.right != None:
        res += traverse(root.right)
    res += [str(root.data)]
    return res

def postOrder(root):
    print " ".join(traverse(root))